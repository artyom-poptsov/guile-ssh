;;; client-server.scm -- Guile-SSH server is SUT.

;; Copyright (C) 2014, 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(use-modules (srfi srfi-64)
             (ice-9 threads)
             (ssh server)
             (ssh session)
             (ssh auth)
             (ssh message)
             (ssh log))

(test-begin "server-client")


;;; Global symbols

(define %addr   "127.0.0.1")
(define *port*   12500)
(define %topdir (getenv "abs_top_srcdir"))


;;; Load helper procedures

(add-to-load-path (getenv "abs_top_srcdir"))
(use-modules (tests common))


;;; Logging

(define %libssh-log-file "server-client-libssh.log")
(define %error-log-file  "server-client-errors.log")

(setup-libssh-logging! %libssh-log-file)
(setup-error-logging! %error-log-file)

;;; Helper procedures and macros

(define (make-session-for-test)
  "Make a session with predefined parameters for a test."
  (make-session
   #:host    %addr
   #:port    *port*
   #:timeout 10        ;seconds
   #:user    "bob"
   #:knownhosts %knownhosts
   #:log-verbosity 'nolog))

(define (make-server-for-test)
  "Make a server with predefined parameters for a test."
  (make-server
   #:bindaddr %addr
   #:bindport *port*
   #:rsakey   %rsakey
   #:dsakey   %dsakey
   #:log-verbosity 'rare))

(define clnmsg
  (let ((log (test-runner-aux-value (test-runner-current))))
    (lambda (message)
      "Print a server MESSAGE to the test log."
      (format log "    client: ~a~%" message))))


;; Pass the test case NAME as the userdata to the libssh log
(define-syntax test-assert-with-log
  (syntax-rules ()
    ((_ name body ...)
     (test-assert name
       (begin
         (set-log-userdata! name)

         ;; Every test uses its own port to avoid conflicts
         (set! *port* (1+ *port*))

         body ...)))))


(define (run-server-test client-proc server-proc)
  "Run a CLIENT-PROC in newly created process.  A session is passed to a
CLIENT-PROC as an argument.  SERVER-PROC is called with a server as an
argument.  The procedure returns a result of SERVER-PROC call."
  (let ((server  (make-server-for-test))
        (session (make-session-for-test))
        (pid     (primitive-fork)))
    (if (zero? pid)
        ;; server
        (dynamic-wind
          (const #f)
          (lambda ()
            (client-proc session))
          (lambda ()
            (primitive-exit 1)))
        ;; client
        (server-proc server))))


;;; Testing of basic procedures

(test-assert-with-log "accept, key exchange"
  (run-server-test

   ;; client
   (lambda (session)
     (sleep 1)
     (connect! session)
     (authenticate-server session)
     (primitive-exit))

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((s (server-accept server)))
       (catch #t
         (lambda ()
           (server-handle-key-exchange s))
         (lambda (key . args)
           (display args)
           (newline)))
       s))))


(test-assert-with-log "server-message-get"
  (run-server-test

   ;; client
   (lambda (session)
     (sleep 1)
     (connect! session)
     (clnmsg "connected")
     (authenticate-server session)
     (clnmsg "server authenticated")
     (userauth-none! session)
     (clnmsg "client authenticated")
     (primitive-exit))

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (let ((msg (server-message-get session)))
         (message-auth-set-methods! msg '(none))
         (message-reply-success msg)
         (message? msg))))))


(test-assert-with-log "message-get-type"
  (run-server-test

   ;; client
   (lambda (session)
     (sleep 1)
     (connect! session)
     (clnmsg "connected")
     (authenticate-server session)
     (clnmsg "server authenticated")
     (userauth-none! session)
     (clnmsg "client authenticated")
     (primitive-exit))

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (let ((msg (server-message-get session)))
         (let ((msg-type (message-get-type msg))
               (expected-type '(request-service)))
           (message-auth-set-methods! msg '(none))
           (message-reply-success msg)
           (disconnect! session)
           (equal? msg-type expected-type)))))))


(test-assert-with-log "message-get-session"
  (run-server-test

   ;; client
   (lambda (session)
     (sleep 1)
     (connect! session)
     (clnmsg "connected")
     (authenticate-server session)
     (clnmsg "server authenticated")
     (userauth-none! session)
     (clnmsg "client authenticated")
     (primitive-exit))

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (let* ((msg (server-message-get session))
              (x   (message-get-session msg)))
         (message-auth-set-methods! msg '(none))
         (message-reply-success msg)
         (disconnect! x)
         (equal? x session))))))


(test-end "server-client")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; server-client.scm ends here.
