;;; tunnel.scm -- Guile-SSH tunnel tests.

;; Copyright (C) 2015, 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 rdelim)
             (ice-9 receive)
             ;; Helper procedures.
             (tests common)
             ;; Guile-SSH
             (ssh auth)
             (ssh channel)
             (ssh log)
             (ssh session)
             (ssh server)
             (ssh tunnel))

(test-begin-with-log "tunnel")

;;;

(define %test-string "hello scheme world")

(define (call-with-connected-session/tunnel proc)
  (call-with-connected-session
   (lambda (session)
     (authenticate-server session)
     (userauth-none! session)
     (proc session))))

(define (call-with-forward-channel session proc)
  (let ((channel (make-channel session)))
    (dynamic-wind
      (const #f)
      (lambda ()
        (case (channel-open-forward channel
                                    #:source-host "localhost"
                                    #:local-port  (get-unused-port)
                                    #:remote-host "localhost"
                                    #:remote-port (1+ (get-unused-port)))
          ((ok)
           (proc channel))
          (else => (cut error "Could not open forward" <>))))
      (lambda () (close channel)))))


(test-equal-with-log "port forwarding, direct"
  %test-string
  (run-client-test
   ;; server
   (lambda (server)
     (start-server/dt-test server
                           (lambda (channel)
                             (write-line (read-line channel) channel))))
   ;; client
   (lambda ()
     (call-with-connected-session/tunnel
      (lambda (session)
        (call-with-forward-channel session
          (lambda (channel)                                   
            (write-line %test-string channel)
            (poll channel read-line))))))))

;; Create a tunnel, check the result.
(test-assert-with-log "make-tunnel"
  (run-client-test
   ;; server
   (lambda (server)
     (start-server/dt-test server
                           (lambda (channel)
                             (write-line (read-line channel) channel))))
   (lambda ()
     (call-with-connected-session/tunnel
      (lambda (session)
        (let* ((local-port (get-unused-port))
               (remote-host "www.example.org")
               (tunnel  (make-tunnel session
                                     #:port  local-port
                                     #:host remote-host)))
          (and (eq?      (tunnel-session tunnel)      session)
               (string=? (tunnel-bind-address tunnel) "127.0.0.1")
               (eq?      (tunnel-port tunnel)         local-port)
               (eq?      (tunnel-host-port tunnel)    local-port)
               (eq?      (tunnel-host tunnel)         remote-host)
               (eq?      (tunnel-reverse? tunnel)     #f))))))))


;; Client calls 'call-with-ssh-forward' with a procedure which sends a string
;; to a server; server echoes the string back.  Client checks if the sent
;; string and the result of 'call-with-ssh-forward' matches.
;;
;; Note that the main part of the test is done in "call/pf" process, only
;; comparison of the original string and the call result is done in the main
;; process of the test case.  The reason for this is srfi-64 tests go bananas
;; when a thread is spawn in a test: the thread shares memory with the parent,
;; and it inherits the test environment, which in turn leads to errors.
;;
;; XXX: This test case contains operations that potentially can block it
;; forever.
;;
;; Here's a schematic representation of the test case:
;;
;; test
;;  |
;;  o                                      Fork.
;;  |___________________________________
;;  o                                   \  Fork.
;;  |______________                      |
;;  |              \                     |
;;  |               |                    |
;;  |               |                    |
;;  |            call/pf               server
;;  |               |                    |
;;  |               o                    | 'call-with-ssh-forward'
;;  |               |______________      |
;;  |               |              \     |
;;  |               | "hello world" :    |
;;  |               |-------------->:    |
;;  |               |               o    | Re-send the message
;;  |               |               :--->|   to the server.
;;  |               |               :    o Echoing back.
;;  |               |               :<---|
;;  |               | "hello world" o    | Re-send the message
;;  |               |<--------------:    |   to the caller.
;;  |               |               o    | Stop the thread.
;;  |               o                    | Bind/listen a socket.
;;  | "hello world" |                    |
;;  |<--------------|                    |
;;  o               |                    | Check the result.
;;  |               |                    |
;;
(test-equal-with-log "call-with-ssh-forward"
  %test-string
  (run-client-test/separate-process
   ;; Server
   (lambda (server)
     (start-server/dt-test server
                           (lambda (channel)
                             (write-line (read-line channel) channel))))
   ;; Client (call/pf)
   (lambda ()
     (set-log-userdata! (string-append (get-log-userdata) " (call/pf)"))
     (call-with-connected-session/tunnel
      (lambda (session)
        (let* ((local-port  (get-unused-port))
               (remote-host "www.example.org")
               (tunnel      (make-tunnel session
                                         #:port local-port
                                         #:host remote-host)))
          (call-with-ssh-forward tunnel
                                 (lambda (sock)
                                   (write-line %test-string sock)
                                   (poll sock read-line)))))))
   ;; Handle the result.
   (lambda (result)
     result)))


(test-assert-with-log "channel-{listen,cancel}-forward"
  (run-client-test
   ;; Server
   (lambda (server)
     (start-server/dist-test server))
   ;; Client
   (lambda ()
     (call-with-connected-session/tunnel
      (lambda (session)
        (let ((portnum (get-unused-port)))
          (and
           (receive (result pnum)
               (channel-listen-forward session
                                       #:address "localhost"
                                       #:port    portnum)
             (and (equal? result 'ok)
                  (= pnum portnum)))
           (eq? (channel-cancel-forward session "localhost" portnum) 'ok))))))))

(test-end "tunnel")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; tunnel.scm ends here.
