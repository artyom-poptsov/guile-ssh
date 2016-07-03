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

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (ice-9 threads)
             (ssh server)
             (ssh session)
             (ssh auth)
             (ssh message)
             (ssh log)
             (tests common))

(test-begin-with-log "server-client")

;;; Helper procedures and macros

(define clnmsg
  (let ((log (test-runner-aux-value (test-runner-current))))
    (lambda (message)
      "Print a server MESSAGE to the test log."
      (format log "    client: ~a~%" message))))


;;; Testing of basic procedures

(test-assert-with-log "accept, key exchange"
  (run-server-test

   ;; client
   (lambda (session)
     (sleep 1)
     (connect! session)
     (authenticate-server session))

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
     (clnmsg "client authenticated"))

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
     (clnmsg "client authenticated"))

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
     (clnmsg "client authenticated"))

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
