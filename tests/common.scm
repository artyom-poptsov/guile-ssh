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
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (ssh session)
  #:use-module (ssh server)
  #:use-module (ssh log)
  #:use-module (ssh message)
  #:export (;; Variables
            %topdir
            %knownhosts
            %addr
            %rsakey
            %rsakey-pub
            %dsakey
            %dsakey-pub
            %ecdsakey
            %ecdsakey-pub

            ;; Procedures
            get-unused-port
            test-assert-with-log
            make-session-loop
            make-session-for-test
            make-server-for-test
            make-libssh-log-printer
            start-server/dt-test
            start-server/dist-test
            setup-libssh-logging!
            setup-error-logging!
            setup-test-suite-logging!
            run-client-test
            run-client-test/separate-process
            run-server-test))


(define %topdir (getenv "abs_top_srcdir"))
(define %addr   "127.0.0.1")
(define *port*  12400)


;; Keys
(define %rsakey       (format #f "~a/tests/keys/rsakey"       %topdir))
(define %rsakey-pub   (format #f "~a/tests/keys/rsakey.pub"   %topdir))
(define %dsakey       (format #f "~a/tests/keys/dsakey"       %topdir))
(define %dsakey-pub   (format #f "~a/tests/keys/dsakey.pub"   %topdir))
(define %ecdsakey     (format #f "~a/tests/keys/ecdsakey"     %topdir))
(define %ecdsakey-pub (format #f "~a/tests/keys/ecdsakey.pub" %topdir))


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
   #:dsakey   %dsakey
   #:log-verbosity 'rare))


;;; Port helpers.

(define (port-in-use? port-number)
  "Return #t if a port with a PORT-NUMBER isn't used, #f otherwise."
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (catch #t
      (lambda ()
        (bind sock AF_INET INADDR_LOOPBACK port-number)
        (close sock)
        #f)
      (lambda args
        (close sock)
        #t))))

(define get-unused-port
  (let ((port-num 12345))
    (lambda ()
      "Get an unused port number."
      (let loop ((num port-num))
        (if (port-in-use? num)
            (loop (1+ num))
            (begin
              (set! port-num num)
              num))))))


;;; Test Servers

(define (start-server/dt-test server rwproc)
  (server-listen server)
  (let ((session (server-accept server))
        (channel #f))
    (server-handle-key-exchange session)
    (make-session-loop session
      (if (not (eof-object? msg))
          (let ((msg-type (message-get-type msg)))
            (case (car msg-type)
              ((request-channel-open)
               (set! channel (message-channel-request-open-reply-accept msg))
               (let poll ((ready? #f))
                 (if ready?
                     (rwproc channel)
                     (poll (char-ready? channel)))))
              ((request-channel)
               (message-reply-success msg))
              (else
               (message-reply-success msg)))))))
  (primitive-exit))

(define (start-server/dist-test server rwproc)
  (server-listen server)
  (let ((session (server-accept server))
        (channel #f))

    (server-handle-key-exchange session)

    (let* ((proc (lambda (session message user-data)
                   (let ((type (message-get-type message))
                         (req  (message-get-req  message)))
                     (format (current-error-port) "global req: type: ~a~%"
                             type)
                     (case (cadr type)
                       ((global-request-tcpip-forward)
                        (let ((pnum (global-req:port req)))
                          (format (current-error-port) "global req: port: ~a~%"
                                  pnum)
                          (message-reply-success message
                                                 pnum)))
                       ((global-request-cancel-tcpip-forward)
                        (message-reply-success message 1))))))
           (callbacks `((user-data               . #f)
                        (global-request-callback . ,proc))))
      (session-set! session 'callbacks callbacks))

    (make-session-loop session
      (unless (eof-object? msg)
        (message-reply-success msg))))
  (primitive-exit))


;;; Tests

(define (format-log/scm level proc-name message . args)
  "Format a log MESSAGE, append \"[SCM]\" to a PROC-NAME."
  (apply format-log level (string-append "[SCM] " proc-name)  message args))

(define (multifork . procs)
  "Execute each procedure from PROCS list in a separate process.  The last
procedure from PROCS is executed in the main process; return the result of the
main procedure."
  (format-log/scm 'nolog "multifork" "procs 1: ~a~%" procs)
  (let* ((len      (length procs))
         (mainproc (car (list-tail procs (- len 1))))
         (procs    (list-head procs (- len 1)))
         (pids     (map (lambda (proc)
                          (let ((pid (primitive-fork)))
                            (when (zero? pid)
                              (proc)
                              (primitive-exit 0))
                            pid))
                        procs)))
    (format-log/scm 'nolog "multifork" "procs 2: ~a~%" procs)
    (format-log/scm 'nolog "multifork" "mainproc: ~a~%" mainproc)
    (format-log/scm 'nolog "multifork" "PIDs: ~a~%" pids)
    (dynamic-wind
      (const #f)
      mainproc
      (lambda ()
        (format-log/scm 'nolog "multifork" "killing spawned processes ...")
        (for-each (lambda (pid) kill pid SIGTERM) pids)))))


(define (run-client-test server-proc client-proc)
  "Run a SERVER-PROC in newly created process.  The server passed to a
SERVER-PROC as an argument.  CLIENT-PROC is expected to be a thunk that should
be executed in the parent process.  The procedure returns a result of
CLIENT-PROC call."
  (let ((server (make-server-for-test)))
    (multifork
     ;; server
     (lambda ()
       (dynamic-wind
         (const #f)
         (lambda ()
           (set-log-userdata! (string-append (get-log-userdata) " (server)"))
           (server-set! server 'log-verbosity 'rare)
           (server-proc server)
           (primitive-exit 0))
         (lambda ()
           (primitive-exit 1))))
     ;; client
     client-proc)))

;; Run a client test in a separate process; only a PRED procedure is running
;; in the main test process:
;;
;; test
;;  |
;;  o                                  Fork.
;;  |_______________________________
;;  o                               \  Fork.
;;  |______________                  |
;;  |              \                 |
;;  |               |                |
;;  |               |                |
;;  |               |                |
;;  |          CLIENT-PROC      SERVER-PROC
;;  |               |                |
;;  |               o                | Bind/listen a socket.
;;  | "hello world" |                |
;;  |<--------------|                |
;;  o               |                | Check the result
;;  |               |                | with a predicate PRED.
;;
;; XXX: This procedure contains operations that potentially can block it
;; forever.
;;
(define (run-client-test/separate-process server-proc client-proc pred)
  "Run a SERVER-PROC and CLIENT-PROC as separate processes.  Check the result
returned by a CLIENT-PROC with a predicate PRED."
  (let ((server (make-server-for-test))
        (sock-path (tmpnam)))
    (multifork
     ;; Server procedure
     (lambda ()
       (server-proc server))
     ;; Client procedure
     (lambda ()
       (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
         (bind sock AF_UNIX sock-path)
         (listen sock 1)
         (let ((result (client-proc))
               (client (car (accept sock))))
           (write-line result client)
           (sleep 10)
           (close client))))
     ;; Main procedure
     (lambda ()
       (let ((sock (socket PF_UNIX SOCK_STREAM 0)))

         ;; XXX: This operation can potentially block the process forever.
         (while (not (file-exists? sock-path)))

         (format (test-runner-aux-value (test-runner-current))
                 "    client: sock-path: ~a~%" sock-path)

         (connect sock AF_UNIX sock-path)

         ;; XXX: This too.
         (while (not (char-ready? sock)))

         (let ((result (read-line sock)))
           (close sock)
           (pred result)))))))


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

(define (setup-test-suite-logging! test-name)
  "Setup error logging for a TEST-SUITE."
  (let ((libssh-log-file (string-append test-name "-libssh.log"))
        (errors-log-file (string-append test-name "-errors.log")))
    (setup-libssh-logging! libssh-log-file)
    (setup-error-logging! errors-log-file)))

;;; common.scm ends here
