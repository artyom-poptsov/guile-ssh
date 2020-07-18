;;; tunnel.scm -- SSH tunnels

;; Copyright (C) 2015, 2016, 2017, 2018, 2019, 2020 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This file is a part of Guile-SSH.
;;
;; Guile-SSH is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; Guile-SSH is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Guile-SSH.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; High-level API built upon the basic port forwarding facilities for managing
;; port forwards.


;;; Code:

(define-module (ssh tunnel)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:use-module (ssh session)
  #:use-module (ssh channel)
  #:export (make-tunnel
            tunnel?
            tunnel-reverse?
            tunnel-session
            tunnel-bind-address
            tunnel-port
            tunnel-host
            tunnel-host-port
            start-forward
            call-with-ssh-forward

            ;; Helper procedures
            make-tunnel-channel
            tunnel-open-forward-channel))


;;; Tunnel type

(define-record-type <tunnel>
  (%make-tunnel session timeout bind-address port
                host host-port reverse?)
  tunnel?
  (session      tunnel-session)          ; session
  (timeout      tunnel-timeout)          ; number
  (bind-address tunnel-bind-address)     ; string
  (port         tunnel-port)             ; number
  (host         tunnel-host)             ; string
  (host-port    tunnel-host-port)        ; number
  (reverse?     tunnel-reverse?))        ; boolean

(set-record-type-printer!
 <tunnel>
 (lambda (tunnel port)
   "Print information about a TUNNEL to a PORT."
   (let ((tunnel-address (number->string (object-address tunnel) 16)))
     (if (tunnel-reverse? tunnel)
         (format port "#<tunnel ~a:~a <- ~a:~a ~a>"
                 (tunnel-host tunnel)
                 (tunnel-host-port tunnel)
                 (if (tunnel-bind-address tunnel)
                     (tunnel-bind-address tunnel)
                     "*")
                 (tunnel-port tunnel)
                 tunnel-address)
         (format port "#<tunnel ~a:~a -> ~a:~a ~a>"
                 (tunnel-bind-address tunnel)
                 (tunnel-port tunnel)
                 (tunnel-host tunnel)
                 (tunnel-host-port tunnel)
                 tunnel-address)))))

(define (make-tunnel-channel tunnel)
  (let ((channel (make-channel (tunnel-session tunnel))))
    (unless channel
      (error "Could not make a channel" tunnel))
    channel))

(define (tunnel-open-forward-channel tunnel)
  "Open a new forward channel for a TUNNEL.  Return the newly created open
channel, or throw an error if a channel could not be opened."
  (let ((channel (make-tunnel-channel tunnel)))
    (case (channel-open-forward channel
                                #:source-host (tunnel-bind-address tunnel)
                                #:local-port  (tunnel-port tunnel)
                                #:remote-host (tunnel-host tunnel)
                                #:remote-port (tunnel-host-port tunnel))
      ((ok)
       channel)
      (else =>
            (lambda (res) (error "Could not open forward channel" tunnel res))))))

(define (tunnel-listen-forward tunnel)
  "Return value is undefined."
  (receive (result port)
      (channel-listen-forward (tunnel-session tunnel)
                              #:address (tunnel-bind-address tunnel)
                              #:port    (tunnel-port tunnel))
    ;; TODO: Handle port
    (or (eq? result 'ok)
        (error "Could not open forward channel" tunnel result))))


;;; Procedures

(define* (make-tunnel session
                      #:key (bind-address "127.0.0.1") port
                      host (host-port port)
                      (timeout 1000)
                      (reverse? #f))
  "Make a new SSH tunnel using SESSION.  The procedure returns a new <tunnel>
object.

In case of direct port forwarding (when REVERSE? is set to #f), a BIND-ADDRESS
is a host from which the connections are originated, and a PORT is a port on
which the tunnel will be listening to the incoming connections.  A HOST and a
HOST-PORT is a host and port to which the connections are forwarded.

Setting REVERSE? to #t changes the direction of the tunnel and a reverse port
forwarding tunnel will be created.  In this case a server allocates a socket
to listen to PORT on the remote side, and whenever a connection is made to
this port, the connection is forwarded over the secure channel, and a
connection is made to HOST and HOST-PORT from the local machine.  HOST can be
set to #f to tell the server to listen on all addresses and known protocol
families.  Setting a PORT to 0 tells the server to bind the first unprivileged
port.

The procedure does not binds ports nor transfers data to the port (in case of
reverse port forwarding), you should start port forwarding by means of the
procedures that operate on a <tunnel> object -- e.g.  'start-forward' or
'call-with-ssh-forward'."
  (let ((timeout (if (and timeout (> timeout 0))
                     timeout
                     1)))
    (%make-tunnel session timeout
                  bind-address port
                  host host-port
                  reverse?)))


(define-syntax-rule (p1->p2? p1 p2)
  "Return #t if P1 and P2 are open ports and P1 has data that can be read, #f
otherwise."
  (and (not (port-closed? p1))
       (not (port-closed? p2))
       (char-ready? p1)))

(define-syntax cond-io
  (syntax-rules (else <- -> =>)
    ((_ (p1 -> p2 => proc) ...)
     (cond ((p1->p2? p1 p2) (proc p1 p2)) ...))
    ((_ (p1 <- p2 => proc) ...)
     (cond ((p1->p2? p2 p1) (proc p1 p2)) ...))
    ((_ (p1 -> p2 => proc) ... (else exp ...))
     (cond ((p1->p2? p1 p2) (proc p1 p2)) ... (else exp ...)))
    ((_ (p1 <- p2 => proc) ... (else exp ...))
     (cond ((p1->p2? p2 p1) (proc p1 p2)) ... (else exp ...)))))


(define (transfer port-1 port-2)
  "Transfer data from a PORT-1 to a PORT-2.  Close both ports if reading from
the PORT-1 returns EOF."
  (let ((data (get-bytevector-some port-1)))
    (if (not (eof-object? data))
        (put-bytevector port-2 data)
        (begin
          (close port-1)
          (close port-2)))))

(define (tunnel-timeout/s+us tunnel)
  "Get a TUNNEL timeout as two values: timeout in seconds and microseconds."
  (let ((timeout (tunnel-timeout tunnel)))
    (values (and timeout (quotient  timeout 1000000))
            (and timeout (remainder timeout 1000000)))))

(define (main-loop tunnel sock idle-proc)
  "Start the main loop of a TUNNEL.  Accept connections on SOCK, transfer data
between SOCK and the remote side.  Call IDLE-PROC as

  (idle-proc client-socket channel)

when no data is available."
  (let-values (((timeout-s timeout-us) (tunnel-timeout/s+us tunnel)))

    (define (select-client client)
      (select (list client) '() '() timeout-s timeout-us))

    (while (connected? (tunnel-session tunnel))
      (catch #t
        (lambda ()
          (let* ((channel           (tunnel-open-forward-channel tunnel))
                 (client-connection (accept sock))
                 (client            (car client-connection)))
            (while (channel-open? channel)
              (cond-io
               (client -> channel => transfer)
               (channel -> client => transfer)
               (else
                (let ((selected (select-client client)))
                  (when (null? (car selected))
                    (idle-proc client channel))))))))
        (const #t)))))


(define (main-loop/reverse tunnel idle-proc)

  (define (tunnel-connect tunnel sock)
    "Make a connection for a reverse TUNNEL.  The return value is
unspecified."
    (connect sock AF_INET
             (inet-pton AF_INET (tunnel-host tunnel))
             (tunnel-host-port tunnel)))

  (let ((timeout (tunnel-timeout tunnel)))
    (while (connected? (tunnel-session tunnel))
      (receive (channel port)
          (channel-accept-forward (tunnel-session tunnel) 1000)
        (when channel
          (let ((sock (socket PF_INET SOCK_STREAM 0)))
            (tunnel-connect tunnel sock)
            (while (channel-open? channel)
              (cond-io
               (channel -> sock => transfer)
               (sock -> channel => transfer)
               (else
                ;; XXX: Very hacky.  We should use something like 'select'
                ;; here.
                (when (channel-open? channel)
                  (usleep timeout)
                  (idle-proc sock channel)))))))))))


(define* (start-forward tunnel #:optional (idle-proc (const #f)))
  "Start port forwarding for a TUNNEL.  Call IDLE-PROC as

  (idle-proc client-socket channel)

when no data is available to forward.  If no IDLE-PROC is specified then a
procedure that always returns #f is used instead."
  (if (tunnel-reverse? tunnel)
      (begin
        (tunnel-listen-forward tunnel)
        (main-loop/reverse tunnel idle-proc))
      (let ((sock (socket PF_INET SOCK_STREAM 0)))
        (bind sock AF_INET (inet-pton AF_INET (tunnel-bind-address tunnel))
              (tunnel-port tunnel))
        (listen sock 10)
        (main-loop tunnel sock idle-proc)
        (close sock))))

(define (call-with-ssh-forward tunnel proc)
  "Call a procedure PROC as (proc sock) where SOCK is a socket that forwards
all the received data to a remote side through a TUNNEL, and vice versa.
Return the result the PROC call."
  (let ((sock   (socket PF_INET SOCK_STREAM 0))
        (thread (call-with-new-thread
                 (lambda ()
                   (start-forward tunnel)))))

    (while #t
      (catch #t
        (lambda ()
          (connect sock AF_INET (inet-pton AF_INET (tunnel-bind-address tunnel))
                   (tunnel-port tunnel))
          (break))
        (lambda args
          (sleep 1))))

    (dynamic-wind
      (const #f)
      (lambda ()
        (proc sock))
      (lambda ()
        (close-port sock)
        (cancel-thread thread)))))

;;; tunnel.scm ends here.
