;;; tunnel.scm -- SSH tunnels

;; Copyright (C) 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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


;;; Code:

(define-module (ssh tunnel)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (ssh session)
  #:use-module (ssh channel)
  #:export (make-tunnel
            tunnel?
            tunnel-reverse?
            tunnel-session
            tunnel-source-host
            tunnel-local-port
            tunnel-remote-host
            tunnel-remote-port
            start-forward
            call-with-ssh-forward))


;;; Tunnel type

(define-immutable-record-type <tunnel>
  (%make-tunnel session timeout source-host local-port
                remote-host remote-port reverse?)
  tunnel?
  (session     tunnel-session)          ; session
  (timeout     tunnel-timeout)          ; number
  (source-host tunnel-source-host)      ; string
  (local-port  tunnel-local-port)       ; number
  (remote-host tunnel-remote-host)      ; string
  (remote-port tunnel-remote-port)      ; number
  (reverse?    tunnel-reverse?))        ; boolean

(set-record-type-printer!
 <tunnel>
 (lambda (tunnel port)
   "Print information about a TUNNEL to a PORT."
   (format port "#<tunnel ~a:~a ~a ~a:~a ~a>"
           (tunnel-source-host tunnel)
           (tunnel-local-port  tunnel)
           (if (tunnel-reverse? tunnel) "<-" "->")
           (if (tunnel-remote-host tunnel)
               (tunnel-remote-host tunnel)
               "*")
           (tunnel-remote-port tunnel)
           (number->string (object-address tunnel) 16))))

(define (make-tunnel-channel tunnel)
  (let ((channel (make-channel (tunnel-session tunnel))))
    (or channel
        (error "Could not make a channel" tunnel))
    channel))

(define (tunnel-open-forward-channel tunnel)
  "Open a new forward channel for a TUNNEL.  Return the newly created open
channel, or throw an error if a channel could not be opened."
  (let ((channel (make-tunnel-channel tunnel)))
    (case (channel-open-forward channel
                                #:source-host (tunnel-source-host tunnel)
                                #:local-port  (tunnel-local-port  tunnel)
                                #:remote-host (tunnel-remote-host tunnel)
                                #:remote-port (tunnel-remote-port tunnel))
      ((ok)
       channel)
      (else =>
            (lambda (res) (error "Could not open forward channel" tunnel res))))))

(define (tunnel-listen-forward tunnel)
  "Return value is undefined."
  (receive (result port)
      (channel-listen-forward (tunnel-session tunnel)
                              #:address (tunnel-remote-host tunnel)
                              #:port    (tunnel-remote-port tunnel))
    ;; TODO: Handle port
    (or (eq? result 'ok)
        (error "Could not open forward channel" tunnel result))))


;;; Procedures

(define* (make-tunnel session
                      #:key (source-host "127.0.0.1") local-port
                      remote-host (remote-port local-port)
                      (timeout 1000)
                      (reverse? #f))
  "Make a new SSH tunnel using SESSION.  The procedure returns a new <tunnel>
object.

In case of direct port forwarding (when REVERSE? is set to #f), a SOURCE-HOST
is a host from which the connections are originated, and a LOCAL-PORT is a
port on which the tunnel will be listening to the incoming connections.  A
REMOTE-HOST and a REMOTE-PORT is a host and port to which the connections are
forwarded.

Setting REVERSE? to #t changes the direction of the tunnel, so a reverse port
forwarding tunnel will be created.  In this case a SOURCE-HOST and a
LOCAL-PORT specifies the host and the port to which remote connections should
be forwarded.  A server binds REMOTE-HOST and REMOTE-PORT and begins listening
for inbound connections.  REMOTE-HOST can be set to #f to tell the server to
listen on all addresses and known protocol families.  Setting a PORT to 0
tells the server to bind the first unprivileged port.

The procedure does not binds a LOCAL-PORT nor transfers data to the port (in
case of reverse port forwarding), you should start port forwarding by means of
the procedures that operate on a <tunnel> object -- e.g.  'start-forward' or
'call-with-ssh-forward'."
  (let ((timeout (if (and timeout (> timeout 0))
                     timeout
                     1)))
    (%make-tunnel session timeout
                  source-host local-port
                  remote-host remote-port
                  reverse?)))


(define-syntax-rule (p1->p2? p1 p2)
  "Return #t if P1 and P2 are open ports and P1 has data that can be read, #f
otherwise."
  (and (not (or (port-closed? p1) (port-closed? p2)))
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

(define (main-loop tunnel sock idle-proc)
  "Start the main loop of a TUNNEL.  Accept connections on SOCK, transfer data
between SOCK and the remote side.  Call IDLE-PROC as

  (idle-proc client-socket channel)

when no data is available."
  (let* ((timeout    (tunnel-timeout tunnel))
         (timeout-s  (and timeout (quotient  timeout 1000000)))
         (timeout-us (and timeout (remainder timeout 1000000))))
    (while (connected? (tunnel-session tunnel))
      (let* ((channel           (tunnel-open-forward-channel tunnel))
             (client-connection (accept sock))
             (client            (car client-connection)))

          (while (channel-open? channel)
            (cond-io
             (client -> channel => transfer)
             (channel -> client => transfer)
             (else
              (let ((selected (select (list client) '() '()
                                      timeout-s timeout-us)))
                (and (null? (car selected))
                     (idle-proc client channel))))))))))

(define (main-loop/reverse tunnel idle-proc)
  (let* ((timeout    (tunnel-timeout tunnel))
         (timeout-s  (and timeout (quotient  timeout 1000000)))
         (timeout-us (and timeout (remainder timeout 1000000))))
    (while (connected? (tunnel-session tunnel))
      (receive (channel port)
          (channel-accept-forward (tunnel-session tunnel) 1000)
        (and channel
             (let* ((sock (socket PF_INET SOCK_STREAM 0)))
               (connect sock AF_INET
                        (inet-aton (tunnel-source-host tunnel))
                        (tunnel-local-port tunnel))
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
        (setsockopt sock SOL_SOCKET SO_REUSEADDR 1) ; DEBUG
        (bind sock AF_INET (inet-pton AF_INET (tunnel-source-host tunnel))
              (tunnel-local-port tunnel))
        (listen sock 10)
        (main-loop tunnel sock idle-proc)
        (close sock))))

(define (call-with-ssh-forward tunnel proc)
  "Call a procedure PROC as (proc sock) where SOCK is a socket that forwards
all the received data to a remote side through a TUNNEL, and vice versa."
  (let ((sock   (socket PF_INET SOCK_STREAM 0))
        (thread (call-with-new-thread
                 (lambda ()
                   (start-forward tunnel)))))

    (connect sock AF_INET (inet-pton AF_INET (tunnel-source-host tunnel))
             (tunnel-local-port tunnel))

    (proc sock)

    (close-port sock)
    (cancel-thread thread)))

;;; tunnel.scm ends here.
