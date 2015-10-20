;;; common.scm -- Heper procedures and macros for tests.

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

(define-module (tests common)
  #:use-module (srfi srfi-64)
  #:use-module (ssh session)
  #:use-module (ssh server)
  #:use-module (ssh log)
  #:export (;; Variables
            %topdir
            %knownhosts
            %addr
            rsakey                      ;TODO: Rename
            %rsakey
            %dsakey

            ;; Procedures
            test-assert-with-log
            make-session-loop
            make-session-for-test
            make-server-for-test
            make-libssh-log-printer
            setup-libssh-logging!
            setup-error-logging!
            run-client-test
            run-server-test))


(define %topdir (getenv "abs_top_srcdir"))
(define %addr   "127.0.0.1")
(define *port*  12400)
(define rsakey (format #f "~a/tests/rsakey" %topdir))
(define %rsakey (format #f "~a/tests/rsakey" %topdir))
(define %dsakey (format #f "~a/tests/dsakey" %topdir))
(define %knownhosts (format #f "~a/tests/knownhosts"
                            (getenv "abs_top_builddir")))


;; Pass the test case NAME as the userdata to the libssh log
(define-syntax test-assert-with-log
  (syntax-rules ()
    ((_ name body ...)
     (test-assert name
       (begin
         (set-log-userdata! name)
         body ...)))))

(define-macro (make-session-loop session . body)
  `(let session-loop ((msg (server-message-get ,session)))
     (and msg (begin ,@body))
     (and (connected? session)
          (session-loop (server-message-get ,session)))))

(define (make-session-for-test)
  "Make a session with predefined parameters for a test."
  (make-session
   #:host    %addr
   #:port    *port*
   #:timeout 10        ;seconds
   #:user    "bob"
   #:knownhosts %knownhosts
   #:log-verbosity 'rare))

(define (make-server-for-test)
  "Make a server with predefined parameters for a test."

  ;; FIXME: This hack is aimed to give every server its own unique
  ;; port to listen to.  Clients will pick up new port number
  ;; automatically through global `port' symbol as well.
  (set! *port* (1+ *port*))

  (make-server
   #:bindaddr %addr
   #:bindport *port*
   #:rsakey   %rsakey
   #:log-verbosity 'rare))


;;; Tests

(define (run-client-test server-proc client-proc)
  "Run a SERVER-PROC in newly created process.  The server passed to a
SERVER-PROC as an argument.  CLIENT-PROC is expected to be a thunk that should
be executed in the parent process.  The procedure returns a result of
CLIENT-PROC call."
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))
    (if (zero? pid)
        ;; server
        (dynamic-wind
          (const #f)
          (lambda ()
            (set-log-userdata! (string-append (get-log-userdata) " (server)"))
            (server-set! server 'log-verbosity 'rare)
            (server-proc server)
            (primitive-exit 0))
          (lambda ()
            (primitive-exit 1)))
        ;; client
        (client-proc))))

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


;;; Logging

(define (make-libssh-log-printer log-file)
  "Make a libssh log printer with output to a LOG-FILE.  Return the log
printer."
  (let ((p (open-output-file log-file)))
    (lambda (priority function message userdata)
      (format p "[~a, \"~a\", ~a]: ~a~%"
              (strftime "%Y-%m-%dT%H:%M:%S%z" (localtime (current-time)))
              userdata
              priority
              message))))

(define (setup-libssh-logging! log-file)
  "Setup libssh logging for a test suite with output to a LOG-FILE."
  (let ((log-printer (make-libssh-log-printer log-file)))
    (set-logging-callback! log-printer)))

(define (setup-error-logging! log-file)
  "Setup error logging for a test suite with output to a LOG-FILE."
  (set-current-error-port (open-output-file log-file)))

;;; common.scm ends here
