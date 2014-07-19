;;; client-server.scm -- Guile-SSH server is SUT.

;; Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
(define %rsakey (format #f "~a/tests/rsakey" %topdir))
(define %dsakey (format #f "~a/tests/dsakey" %topdir))
(define %knownhosts (format #f "~a/tests/knownhosts" %topdir))

(define %libssh-log-file "server-client-libssh.log")
(define %error-log-file  "server-client-errors.log")

(set-current-error-port (open-output-file %error-log-file))


;;; Logging callback

(define libssh-log-printer
  (let ((p (open-output-file %libssh-log-file)))
    (lambda (priority function message userdata)
      (format p "[~a, \"~a\", ~a]: ~a~%"
              (strftime "%Y-%m-%dT%H:%M:%S%z" (localtime (current-time)))
              userdata
              priority
              message))))

(set-logging-callback! libssh-log-printer)

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
      (format %log "    client: ~a~%" message))))


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


;;; Testing of basic procedures

(test-assert-with-log "accept, key exchange"
  (let ((server (make-server-for-test))
        (session (make-session-for-test))
        (pid     (primitive-fork)))

    (if (not (= 0 pid))

        ;; server
        (begin
          (server-listen server)
          (let ((s (server-accept server)))
            (catch #t
              (lambda ()
                (server-handle-key-exchange s))
              (lambda (key . args)
                (display args)
                (newline)))
            s))

        ;; client
        (begin
          (sleep 1)
          (connect! session)
          (authenticate-server session)
          (primitive-exit)))))


(test-assert-with-log "server-message-get"
  (let ((session (make-session-for-test))
        (pid     (primitive-fork)))

    (if (not (= 0 pid))

        ;; server
        (let ((server (make-server-for-test)))
          (server-listen server)
          (let ((session (server-accept server)))
            (server-handle-key-exchange session)
            (let ((msg (server-message-get session)))
              (message-auth-set-methods! msg '(none))
              (message-reply-success msg)
              (message? msg))))

        ;; client
        (begin
          (sleep 1)
          (connect! session)
          (clnmsg "connected")
          (authenticate-server session)
          (clnmsg "server authenticated")
          (userauth-none! session)
          (clnmsg "client authenticated")
          (primitive-exit)))))


(test-assert-with-log "message-get-type"
  (let ((session (make-session-for-test))
        (pid     (primitive-fork)))

    (if (not (= 0 pid))

        ;; server
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
                (equal? msg-type expected-type)))))

        ;; client
        (begin
          (sleep 1)
          (connect! session)
          (clnmsg "connected")
          (authenticate-server session)
          (clnmsg "server authenticated")
          (userauth-none! session)
          (clnmsg "client authenticated")
          (primitive-exit)))))


(test-end "server-client")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; server-client.scm ends here.
