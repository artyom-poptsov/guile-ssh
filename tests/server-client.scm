;;; client-server.scm -- Guile-SSH server is SUT.

;; Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This file is a part of libguile-ssh.
;;
;; libguile-ssh is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; libguile-ssh is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with libguile-ssh.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (srfi srfi-64)
             (ice-9 threads)
             (ssh server)
             (ssh session)
             (ssh auth)
             (ssh message))

(test-begin "server-client")


;;; Global symbols

(define addr   "127.0.0.1")
(define port   12345)
(define topdir (getenv "abs_top_srcdir"))
(define rsakey (format #f "~a/tests/rsakey" topdir))
(define log    (test-runner-aux-value (test-runner-current)))
(define client-thread #f)

;;; Helper procedures and macros

(define (make-session-for-test)
  "Make a session with predefined parameters for a test."

  ;; FIXME: This hack is aimed to give every client its own unique
  ;; port to listen to.  Servers will pick up new port number
  ;; automatically through global `port' symbol as well.
  (set! port (1+ port))

  (make-session
   #:host    addr
   #:port    port
   #:timeout 10        ;seconds
   #:user    "bob"
   #:log-verbosity 'nolog))

(define (make-server-for-test)
  "Make a server with predefined parameters for a test."
  (make-server
   #:bindaddr addr
   #:bindport port
   #:rsakey   rsakey
   #:log-verbosity 'nolog))

(define (clnmsg message)
  "Print a server MESSAGE to the test log."
  (format log "    client: ~a~%" message))

(define-macro (spawn-client-thread . body)
  `(set! client-thread
      (make-thread
       (lambda ()
         ,@body))))

(define (cancel-client-thread)
  (cancel-thread client-thread))


;;; Testing of basic procedures

(spawn-client-thread
 (let ((session (make-session-for-test)))
   (while (not (connected? session))
     (sleep 1)
     (catch #t
       (lambda () (connect! session))
       (lambda (key . args) #f)))
   (authenticate-server session)
   (let ((res (connected? session)))
     (disconnect! session)
     res)))

(test-assert "accept, key exchange"
  (let ((server (make-server-for-test)))
    (server-listen server)
    (let ((s (server-accept server)))
      (server-handle-key-exchange s)
      s)))

(cancel-client-thread)


(spawn-client-thread
 (let ((session (make-session-for-test)))
   (while (not (connected? session))
     (sleep 1)
     (catch #t
       (lambda () (connect! session))
       (lambda (key . args) #f)))
   (clnmsg "connected")
   (authenticate-server session)
   (clnmsg "server authenticated")
   (userauth-none! session)
   (clnmsg "client authenticated")
   (disconnect! session)))

(test-assert "server-message-get"
  (let ((server (make-server-for-test)))
    (server-listen server)
    (let ((session (server-accept server)))
      (server-handle-key-exchange session)
      (let ((msg (server-message-get session)))
        (message-auth-set-methods! msg '(none))
        (message-reply-success msg)
        (disconnect! session)
        ;; We consider the test passed if no exceptions were thrown.
        #t))))

(cancel-client-thread)


(spawn-client-thread
 (let ((session (make-session-for-test)))
   (while (not (connected? session))
     (sleep 1)
     (catch #t
       (lambda () (connect! session))
       (lambda (key . args) #f)))
   (clnmsg "connected")
   (authenticate-server session)
   (clnmsg "server authenticated")
   (userauth-none! session)
   (clnmsg "client authenticated")
   (disconnect! session)))

(test-assert "message-get-type"
  (let ((server (make-server-for-test)))
    (server-listen server)
    (let ((session (server-accept server)))
      (server-handle-key-exchange session)
      (let ((msg (server-message-get session)))
        (let ((msg-type (message-get-type msg))
              (expected-type '(request-service)))
          (message-auth-set-methods! msg '(none))
          (message-reply-success msg)
          (disconnect! session)
          (equal? msg-type expected-type))))))

(cancel-client-thread)


(test-end "server-client")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; server-client.scm ends here.
