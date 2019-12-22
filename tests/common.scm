;;; common.scm -- Heper procedures and macros for tests.

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

(define-module (tests common)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 threads)
  #:use-module (ssh session)
  #:use-module (ssh channel)
  #:use-module (ssh server)
  #:use-module (ssh auth)
  #:use-module (ssh log)
  #:use-module (ssh message)
  #:export (;; Variables
            %topdir
            %topbuilddir
            %knownhosts
            %config
            %addr
            %rsakey
            %rsakey-pub
            %dsakey
            %dsakey-pub
            %ecdsakey
            %ecdsakey-pub

            ;; Procedures
            get-unused-port
            set-port!
            test-begin-with-log
            test-assert-with-log
            test-error-with-log
            test-error-with-log/=
            test-equal-with-log
            start-session-loop
            make-session-for-test
            make-server-for-test
            make-libssh-log-printer
            call-with-connected-session
            call-with-connected-session/shell
            start-server-loop
            start-server/dt-test
            start-server/dist-test
            start-server/exec
            run-client-test
            run-client-test/separate-process
            run-server-test
            setup-test-suite-logging!
            format-log/scm
            poll))


(define %topdir (getenv "abs_top_srcdir"))
(define %topbuilddir (getenv "abs_top_builddir"))
(define %addr   "127.0.0.1")
(define *port*  12400)


;; Keys
(define %rsakey       (format #f "~a/tests/keys/rsakey"       %topdir))
(define %rsakey-pub   (format #f "~a/tests/keys/rsakey.pub"   %topdir))
(define %dsakey       (format #f "~a/tests/keys/dsakey"       %topdir))
(define %dsakey-pub   (format #f "~a/tests/keys/dsakey.pub"   %topdir))
(define %ecdsakey     (format #f "~a/tests/keys/ecdsakey"     %topdir))
(define %ecdsakey-pub (format #f "~a/tests/keys/ecdsakey.pub" %topdir))


(define (format-log/scm level proc-name message . args)
  "Format a log MESSAGE, append \"[SCM]\" to a PROC-NAME."
  (apply format-log level (string-append "[SCM] " proc-name)  message args))


(define %knownhosts (format #f "~a/tests/knownhosts"
                            (getenv "abs_top_builddir")))

(define %config (format #f "~a/tests/config" %topdir))


;; Pass the test case NAME as the userdata to the libssh log
(define-syntax test-assert-with-log
  (syntax-rules ()
    ((_ name body ...)
     (test-assert name
       (begin
         (set-log-userdata! name)
         body ...)))))

;; Ensure that the specific ERROR is raised during the test, check the error
;; with HANDLER.
(define-syntax test-error-with-log/handler
  (syntax-rules ()
    ((_ name expr handler)
     (test-assert-with-log name
       (catch #t
         (lambda () expr #f)
         handler)))))

;; Ensure that the specific ERROR is raised during the test and the error is
;; raised with the specified MESSAGE.
(define-syntax-rule (test-error-with-log/= name error expected-message expr)
  (test-error-with-log/handler name expr
                               (lambda (key . args)
                                 (if (equal? key error)
                                     (let* ((message (cadr args))
                                            (result  (string=? message
                                                               expected-message)))
                                       (unless result
                                         (format-log/scm 'nolog name
                                                         (string-append
                                                          "Messages do not match: "
                                                          "expected \"~a\", got \"~a\"")
                                                         result expected-message))
                                       result)
                                     (begin
                                       (format-log/scm 'nolog name
                                                       (string-append
                                                        "Errors do not match: "
                                                        "expected '~a', got '~a' (args: ~a)")
                                                       error key args)
                                       #f)))))

;; Ensure that the specific ERROR is raised during the test.
(define-syntax test-error-with-log
  (syntax-rules ()
    ((_ name error expr)
     (test-error-with-log/handler name expr
                                  (lambda (key . args)
                                    (let ((result (equal? key error)))
                                      (unless result
                                        (format-log/scm 'nolog name
                                                        (string-append
                                                         "Errors do not match: "
                                                         "expected ~a, got ~a (args: ~a)")
                                                        error key args))
                                      result))))
    ((_ name expr)
     (test-error-with-log/handler name expr (const #t)))))

(define-syntax-rule (test-equal-with-log name expected expr)
  (test-assert-with-log name
    (equal? expr expected)))


(define (start-session-loop session body)
  (let session-loop ((msg         (server-message-get session))
                     (msg-counter 0))
    (format-log/scm 'nolog "start-session-loop"
                    "message: ~a" msg)
    (when msg
      (format-log/scm 'nolog "start-session-loop"
                      "message being handled: ~a" msg)
      (body msg)
      (format-log/scm 'nolog "start-session-loop"
                      "message handled: ~a" msg))
    (if (connected? session)
        (begin
          (format-log/scm 'nolog "start-session-loop"
                          "message counter: ~a" msg-counter)
          (session-loop (server-message-get session) (1+ msg-counter)))
        (format-log/scm 'nolog "start-session-loop"
                        "disconnected; message counter: ~a" msg-counter))))

(define (make-session-for-test)
  "Make a session with predefined parameters for a test."
  (format-log/scm 'nolog "make-session-for-test"
                  "host: ~a; port: ~a" %addr *port*)
  (make-session
   #:host    %addr
   #:port    *port*
   #:timeout 10        ;seconds
   #:user    "bob"
   #:knownhosts %knownhosts
   #:log-verbosity 'functions))

(define mtx (make-mutex 'allow-external-unlock))
(define (make-server-for-test)
  "Make a server with predefined parameters for a test."
  (lock-mutex mtx)
  (dynamic-wind
    (const #f)
    (lambda ()
      ;; FIXME: This hack is aimed to give every server its own unique
      ;; port to listen to.  Clients will pick up new port number
      ;; automatically through global `port' symbol as well.
      (set! *port* (get-unused-port))

      (format-log/scm 'nolog "make-server-for-test"
                      "bindaddr: ~a; bindport: ~a" %addr *port*)

      (let ((s (make-server
                #:bindaddr %addr
                #:bindport *port*
                #:rsakey   %rsakey
                #:dsakey   %dsakey
                #:log-verbosity 'functions)))
        (format-log/scm 'nolog "make-server-for-test"
                        "***** bindaddr: ~a; bindport: ~a" %addr *port*)
        (server-listen s)
        s))
    (lambda ()
      (unlock-mutex mtx))))

(define (call-with-connected-session proc)
  "Call the one-argument procedure PROC with a freshly created and connected
SSH session object, return the result of the procedure call.  The session is
disconnected when the PROC is finished."
  (let ((session (make-session-for-test)))
    (format-log/scm 'nolog
                    "call-with-connected-session"
                    "session: ~a" session)
    (dynamic-wind
      (lambda ()
        (format-log/scm 'nolog
                        "call-with-connected-session"
                        "~a" "connecting...")
        (let ((result (connect! session)))
          (format-log/scm 'nolog
                          "call-with-connected-session"
                          "result: ~a" result)
          (unless (equal? result 'ok)
            (format-log/scm 'nolog "call-with-connected-session"
                            "ERROR: Could not connect to a server: ~a" result)
            (error "Could not connect to a server" session result))))
      (lambda () (proc session))
      (lambda () (disconnect! session)))))

(define (call-with-connected-session/shell proc)
  "Make a session for a shell test."
  (call-with-connected-session
   (lambda (session)
     (format-log/scm 'nolog "call-with-connected-session/shell"
                     "session: ~a" session)
     (let ((auth-result (authenticate-server session)))
       (format-log/scm 'nolog "call-with-connected-session/shell"
                       "server auth result: ~a" auth-result))
     (let ((auth-result (userauth-none! session)))
       (format-log/scm 'nolog "call-with-connected-session/shell"
                       "client auth result: ~a" auth-result))
     (proc session))))


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
  (let ((port-num (+ 10000 (random 2345 (random-state-from-platform))))
        (mtx      (make-mutex 'allow-external-unlock)))
    (lambda ()
      "Get an unused port number."
      (lock-mutex mtx)
      (let loop ((num port-num))
        (if (port-in-use? num)
            (loop (1+ num))
            (begin
              (format-log/scm 'nolog "get-unused-port"
                              "port choosen: ~a" num)
              (set! port-num num)
              (unlock-mutex mtx)
              num))))))

(define (set-port! port)
  (set! *port* port))


;;;

(define (poll port proc)
  "Poll a PORT, call a PROC when data is available."
  (format-log/scm 'nolog "poll" "Polling ...")
  (let p ((ready? #f))
  (format-log/scm 'nolog "poll" "ready? ~a ..." ready?)
    (if ready?
        (proc port)
        (p (char-ready? port)))))


;;; Test Servers

(define (start-server-loop server proc)
  "Start a SERVER loop, call PROC on incoming sessions."

  (define (state:init)
    (format-log/scm 'nolog "start-server-loop [state:init]"
                    "server: ~a" server)
    (catch #t
      (lambda ()
        (server-listen server)
        (state:accept))
      (lambda (key args)
        (format-log/scm 'nolog "start-server-loop"
                        "ERROR: ~a: ~a" key args)
        (state:init))))

  (define (state:accept)
    (format-log/scm 'nolog "start-server-loop [state:accept]"
                    "server: ~a" server)
    (let ((session (server-accept server)))
      (format-log/scm 'nolog "start-server-loop [state:accept]"
                      "session: ~a" session)
      (server-handle-key-exchange session)
      (format-log/scm 'nolog "start-server-loop [state:accept]"
                      "~a" "key exchange handled")
      (proc session)
      (state:accept)))

  (state:init))


(define (start-server/dt-test server rwproc)
  (start-server-loop server
    (lambda (session)
      (start-session-loop session
        (lambda (msg)
          (case (car (message-get-type msg))
            ((request-channel-open)
             (let ((channel (message-channel-request-open-reply-accept msg)))
               (poll channel rwproc)))
            (else
             (message-reply-success msg))))))))


(define %guile-version-string "\
GNU Guile 2.2.3
Copyright (C) 1995-2017 Free Software Foundation, Inc.

Guile comes with ABSOLUTELY NO WARRANTY; for details type `,show w'.
This program is free software, and you are welcome to redistribute it
under certain conditions; type `,show c' for details.

Enter `,help' for help.
scheme@(guile-user)> ")

(define (start-server/exec server body)
  "Start SERVER for a command execution test."
  (define *channel* #f)

  (define (state:message-handle-exec message session)
      (format-log/scm 'nolog "start-server/exec" "state:message-handle-exec: message: ~A" message)
    (let ((command (exec-req:cmd (message-get-req message))))
      (format-log/scm 'nolog "start-server/exec" "command: ~A" command)
      (cond
       ((string=? command "ping")
        (message-reply-success message)
        (channel-request-send-exit-status *channel* 0)
        (write-line "pong" *channel*)
        (channel-send-eof *channel*))
       ((string=? command "uname") ; For exit status testing
        (body session message *channel*))
       ((string=? command "exit status") ; For exit status testing
        (message-reply-success message)
        (channel-request-send-exit-status *channel* 0))
       ((string-match "echo '.*" command) ; fallback commands
        (message-reply-success message)
        (channel-request-send-exit-status *channel* 0)
        (channel-send-eof *channel*))
       ((string=? command "cat /proc/loadavg")
        (message-reply-success message)
        (write-line "0.01 0.05 0.10 4/1927 242011" *channel*)
        (channel-request-send-exit-status *channel* 0)
        (channel-send-eof *channel*))
       ((string=? command "which guile > /dev/null && guile --version")
        (write-line %guile-version-string *channel*)
        (message-reply-success message)
        (channel-request-send-exit-status *channel* 0)
        (channel-send-eof *channel*))
       ((string=? command "guile -q")
        (message-reply-success message)
        (display %guile-version-string *channel*)
        (body session message *channel*))
       (else
        (message-reply-success message)
        (write-line command *channel*)
        (channel-request-send-exit-status *channel* 0)
        (channel-send-eof *channel*)))))

  (define (state:handle-tcpip-forward message)
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
      (session-set! (message-get-session message) 'callbacks callbacks)
      (message-reply-success message 1)))

  (define (state:process-message message)
    (format-log/scm 'nolog "start-server/exec"
                    "message: ~a" message)
    (let ((message-type (message-get-type message)))
      (format-log/scm 'nolog "start-server/exec"
                      "message-type: ~a" message-type)
      (case (car message-type)
        ((request-channel-open)
         (set! *channel* (message-channel-request-open-reply-accept message))
         (case (cadr message-type)
           ((channel-direct-tcpip)
            (write-line (read-line *channel*) *channel*))))
        ((request-channel)
         (state:message-handle-request-channel message message-type))
        ((request-global)
         (state:handle-tcpip-forward message))
        (else
         (message-reply-success message)))))

  (define (state:message-handle-request-channel message message-type)
    (let ((subtype (cadr message-type)))
      (format-log/scm 'nolog "start-server/exec"
                      "message-subtype: ~a" subtype)
      (case subtype
        ((channel-request-exec)
         (state:message-handle-exec message (message-get-session message)))
        (else
         (message-reply-success message)))))

  (define (state:init)
    (start-server-loop server
      (lambda (session)
        (format-log/scm 'nolog "start-server/exec"
                        "session: ~a" session)
        (start-session-loop session state:process-message))))

  (state:init))

(define (start-server/dist-test server)
  (server-listen server)
  (let ((session (server-accept server)))

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

    (start-session-loop session
                        (lambda (msg type)
                          (message-reply-success msg)))))


;;; Tests

(define (multifork . procs)
  "Execute each procedure from PROCS list in a separate process.  The last
procedure from PROCS is executed in the main process; return the result of the
main procedure."
  (define (signal-handler args)
    (primitive-exit 0))
  (format-log/scm 'nolog "multifork" "procs 1: ~a~%" procs)
  (let* ((len      (length procs))
         (mainproc (car (list-tail procs (- len 1))))
         (procs    (list-head procs (- len 1)))
         (pids     (map (lambda (proc)
                          (let ((pid (primitive-fork)))
                            (if (zero? pid)
                                (dynamic-wind
                                  (lambda ()
                                    (test-runner-reset (test-runner-current))
                                    (set! test-log-to-file #f)
                                    (sigaction SIGTERM signal-handler)
                                    (format-log/scm 'nolog "multifork"
                                                    "Running proc ...~%"))
                                  proc
                                  (lambda ()
                                    ;; This handler makes sure the child
                                    ;; process exits when PROC exit, be it a
                                    ;; non-local exit or a normal return.
                                    (format-log/scm 'nolog "multifork"
                                                    "Exiting ...~%")
                                    (primitive-exit 0)))
                                (begin
                                  (format-log/scm 'nolog "multifork" "PID: ~a~%" pid)
                                  pid))))
                        procs)))
    (format-log/scm 'nolog "multifork" "procs 2: ~a~%" procs)
    (format-log/scm 'nolog "multifork" "mainproc: ~a~%" mainproc)
    (format-log/scm 'nolog "multifork" "PIDs: ~a~%" pids)
    (dynamic-wind
      (const #f)
      (lambda () (mainproc pids))
      (lambda ()
        (format-log/scm 'nolog "multifork" "killing spawned processes ~a ...~%" pids)
        (catch #t
          (lambda () (for-each (cut kill <> SIGTERM) pids))
          (lambda args
            (format-log/scm 'nolog "multifork"
                            "ERROR: Could not kill process ~a: ~a~%" pids args)))
        (format-log/scm 'nolog "multifork" "waiting for processes status ~a ...~%" pids)
        (catch #t
          (lambda () (for-each waitpid pids))
          (lambda args
            (format-log/scm 'nolog "multifork"
                            "ERROR: Could not wait for PIDS ~a: ~a" pids args)))))))


;;   "Run a SERVER-PROC in newly created process.  The server passed to a
;; SERVER-PROC as an argument.  CLIENT-PROC is expected to be a thunk that should
;; be executed in the parent process.  The procedure returns a result of
;; CLIENT-PROC call."
(define-syntax-rule (run-client-test server-proc client-proc)
  (let ((test-suite-name (car (test-runner-group-stack (test-runner-current))))
        (test-name (test-runner-test-name (test-runner-current))))
    (format-log/scm 'nolog "run-client-test" "Making a server ...")
    (let ((port (get-unused-port)))
      (set-port! port)
      (format-log/scm 'nolog "run-client-test" "Spawning processes ...")
      (multifork
       ;; server
       (lambda ()
         (execle "/usr/bin/guile"
                 (environ)
                 "/usr/bin/guile"
                 "-L" (format #f "~a/" %topdir)
                 "-L" (format #f "~a/modules/" %topdir)
                 "-e" "main"
                 "-s" (format #f "~a/tests/common/test-server.scm" %topdir)
                 test-suite-name
                 test-name
                 (number->string port)
                 (format #f "~a" (quote server-proc)))
         (format-log/scm 'nolog "run-client-test" "Could not spawn process!")
         (exit 1))
       ;; client
       (lambda (pids)
         (format-log/scm 'nolog "run-client-test"
                         "PIDs: ~a" pids)
         (format-log/scm 'nolog "run-client-test"
                         "***** group stack: ~a"
                         (test-runner-group-stack
                          (test-runner-current)))
         ;; Wait for synchronization.
         (let ((run-file (string-append test-name ".run")))
           (while (not (file-exists? run-file))
             (usleep 10)
                     (format-log/scm 'nolog "run-client-test"
                                     "***** pid: ~a"
                                     (car pids))
             (let ((res (waitpid (car pids) WNOHANG)))
                     (format-log/scm 'nolog "run-client-test"
                                     "***** waitpid: ~a"
                                     res)
               (if (> (car res) 0)
                   (begin
                     (format-log/scm 'nolog "run-client-test"
                                     "Child process finished: ~a"
                                     (car pids))
                     (exit 1)))))

           (delete-file run-file))

         (client-proc))))))

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
    (format-log/scm 'nolog
                    "run-client-test/separate-process"
                    "socket path: ~a" sock-path)
    (multifork
     ;; Server procedure
     (lambda ()
       (server-proc server))
     ;; Client procedure
     (lambda ()
       (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
         (bind sock AF_UNIX sock-path)
         (listen sock 1)
         (while #t
           (let ((result (client-proc))
                 (client (car (accept sock))))
             (format-log/scm 'nolog
                             "run-client-test/separate-process"
                             "client: result: ~a" result)
             (write-line result client)
             (sleep 10)
             (close client)))))
     ;; Main procedure
     (lambda (pids)
       (let ((sock (socket PF_UNIX SOCK_STREAM 0)))

         ;; XXX: This operation can potentially block the process forever.
         (while (not (file-exists? sock-path)))

         (format (test-runner-aux-value (test-runner-current))
                 "    client: sock-path: ~a~%" sock-path)

         (connect sock AF_UNIX sock-path)

         ;; XXX: This too.
         (poll sock
               (lambda (sock)
                 (let ((result (read-line sock)))
                   (format-log/scm 'nolog
                                   "run-client-test/separate-process"
                                   "main: result: ~a" result)
                   (close sock)
                   (delete-file sock-path)
                   (pred result)))))))))


(define (run-server-test client-proc server-proc)
  "Run a CLIENT-PROC in newly created process.  A session is passed to a
CLIENT-PROC as an argument.  SERVER-PROC is called with a server as an
argument.  The procedure returns a result of SERVER-PROC call."
  (let ((server  (make-server-for-test))
        (session (make-session-for-test)))
    (multifork
     ;; server
     (lambda ()
       (dynamic-wind
         (const #f)
         (lambda ()
           (client-proc session))
         (lambda ()
           (primitive-exit 1))))
     ;; client
     (lambda (pids)
       (server-proc server)))))


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

(define (test-begin-with-log test-name)
  (set-log-verbosity! 'functions)
  (setup-test-suite-logging! test-name)
  (test-begin test-name))


;;; common.scm ends here
