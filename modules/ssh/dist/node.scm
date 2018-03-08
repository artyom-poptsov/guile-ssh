;;; node.scm -- Distributed computing node

;; Copyright (C) 2015, 2016, 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
;; along with Guile-SSH.  If not, see
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This module describes the distributed node object.  This object holds an
;; SSH tunnel to a remote host and remote REPL port, thus it can be used to
;; execute jobs (see (ssh dist job)).
;;
;; The module provides the following procedures:
;;   node?
;;   node-session
;;   node-repl-port
;;   make-node
;;   node-eval
;;   node-open-rrepl
;;   node-guile-version
;;   node-run-server
;;   node-stop-server
;;   node-server-running?
;;   node-loadavg
;;   with-ssh
;;
;;   rrepl-eval
;;   rrepl-skip-to-prompt
;;
;; There are two specific exceptions that the module procedures can throw:
;;   node-error
;;   node-repl-error
;;
;; See the Info documentation for detailed description of these exceptions and
;; aforementioned procedures.


;;; Code:

(define-module (ssh dist node)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ssh session)
  #:use-module (ssh session)
  #:use-module (ssh channel)
  #:use-module (ssh popen)
  #:use-module (ssh tunnel)
  #:use-module (ssh log)
  #:use-module (ssh shell)
  #:export (node?
            node-session
            node-tunnel
            node-repl-port
            make-node
            node-eval
            node-eval-1
            node-guile-version
            node-run-server
            node-stop-server
            node-server-running?
            node-loadavg
            with-ssh

            node-open-rrepl
            rrepl-eval
            rrepl-skip-to-prompt
            rrepl-get-result))

(define %guile-default-repl-port 37146)


(define (eof-or-null? str)
  "Return #t if a STR is an EOF object or an empty string, #f otherwise."
  (or (eof-object? str) (string-null? str)))


;;; Error reporting

(define (node-error . args)
  "Raise a node error."
  (apply throw (cons 'node-error args)))

(define (node-repl-error . args)
  "Raise a REPL error."
  (apply throw (cons 'node-repl-error args)))


;;; Node type

(define-record-type <node>
  (%make-node tunnel repl-port start-repl-server? stop-repl-server?)
  node?
  (tunnel node-tunnel)                          ; <tunnel>
  (repl-port node-repl-port)                    ; number
  (start-repl-server? node-start-repl-server?)  ; boolean
  (stop-repl-server?  node-stop-repl-server?))  ; boolean

(define (node-session node)
  "Get node session."
  (tunnel-session (node-tunnel node)))

(set-record-type-printer!
 <node>
 (lambda (node port)
   (let ((s (node-session node)))
     (format port "#<node ~a@~a:~a/~a ~a>"
             (session-get s 'user)
             (session-get s 'host)
             (session-get s 'port)
             (node-repl-port node)
             (number->string (object-address node) 16)))))


(define* (make-node session #:optional (repl-port 37146)
                    #:key (start-repl-server? #t)
                    (stop-repl-server? #f))
  "Make a new distributed computing node.  If START-REPL-SERVER? is set to
#t (which is by default) then start a REPL server on a remote host
automatically in case when it is not started yet.  If STOP-REPL-SERVER? is set
to #t then a REPL server will be stopped as soon as an evaluation is done."
  (let ((tunnel (make-tunnel session
                             #:port 0          ;Won't be used
                             #:host "localhost"
                             #:host-port repl-port)))
    (%make-node tunnel repl-port start-repl-server? stop-repl-server?)))


;;; Remote REPL (RREPL)

(define (read-string str)
  "Read a string STR."
  (call-with-input-string str read))

(define (rrepl-skip-to-prompt repl-channel)
  "Read from REPL-CHANNEL until REPL is observed.  Throw 'node-error' on an
error."
  (let loop ((line (read-line repl-channel)))
    (when (eof-object? line)
      (node-error "Could not locate REPL prompt" repl-channel))
    (or (string=? "Enter `,help' for help." line)
        (loop (read-line repl-channel)))))


;; Regexp for parsing a result of evaluation of an expression that returns a
;; value.
(define %repl-result-regexp
  (make-regexp "^(.*)@(.*)> \\$([0-9]+) = (.*)"))

;; Regexp for parsing a result of evaluation of an expression that returns
;; multiple values.
(define %repl-result-2-regexp
  (make-regexp "^\\$([0-9]+) = (.*)"))

;; Regexp for parsing a result of evaluation of an expression which return
;; value is unspecified.
(define %repl-undefined-result-regexp
  (make-regexp "^(.*)@(.*)> "))

;; Regexp for parsing an evaluation error.
(define %repl-error-regexp
  (make-regexp "^(.*)@(.*)> ERROR: .*"))

(define %repl-error-regexp-2
  (make-regexp "^ERROR: .*"))

;; Regexp for parsing "unbound variable" errors.
(define %repl-error-unbound-variable
  (make-regexp "socket:[0-9]+:[0-9]+: \
In procedure module-lookup: Unbound variable: .*"))


(define (rrepl-get-result repl-channel)
  "Get result of evaluation form REPL-CHANNEL, return four values: an
evaluation result, a number of the evaluation, a module name and a language
name.  Throw 'node-repl-error' on an error."

  (define (raise-repl-error result . rest)
    "Raise an REPL error with a RESULT of evaluation."
    (node-repl-error "Evaluation failed" result rest))

  (define (parse-result matches lines)
    (if (null? lines)
        (reverse matches)
        (let ((line (car lines)))
          (if (or (eof-or-null? line)
                  (regexp-exec %repl-undefined-result-regexp line))
              (reverse matches)
              (parse-result (cons (regexp-exec %repl-result-2-regexp line) matches)
                            (cdr lines))))))

  (define (read-result match rest)
    (let* ((matches (parse-result (list match) rest))
           (len     (length matches)))
      (catch #t
        (lambda ()
          (if (= len 1)
              (let ((m (car matches)))
                (values (read-string (match:substring m 4))
                        (string->number (match:substring m 3))))
              (let ((rv (make-vector len))
                    (nv (make-vector len)))
                ;; The 1st match also contains a module name and language name,
                ;; but we want only the evaluation result and the result number.
                (let ((m (car matches)))
                  (vector-set! rv 0 (read-string (match:substring m 4)))
                  (vector-set! nv 0 (string->number (match:substring m 3))))

                (do ((i 1 (1+ i)))
                    ((= i len))
                  (let ((m (list-ref matches i)))
                    (vector-set! rv i (read-string (match:substring m 2)))
                    (vector-set! nv i (string->number (match:substring m 1)))))
                (values rv nv))))
        (lambda (key . message)
          (case key
            ((read-error)
             (raise-repl-error (format #f "Reader error: ~a: ~a: ~a"
                                       (car message)
                                       (apply format #f
                                              (cadr message)
                                              (cddr message))
                                       (string-join
                                        (map (lambda (match)
                                               (match:substring match 0))
                                             matches)))))
            (else
             (raise-repl-error message
                               (map (lambda (match) (match:substring match 0))
                                    matches))))))))

  (define (error? line)
    "Does a LINE contain an REPL error message?"
    (or (regexp-exec %repl-error-regexp line)
        (regexp-exec %repl-error-regexp-2 line)
        (regexp-exec %repl-error-unbound-variable line)))

  (define (error-message? result)
    "Does a RESULT of evaluation contains a REPL error message?"
    (find error? result))

  (define (handle-response result)
    (cond
     ((error-message? result)
      (raise-repl-error (string-join result "\n")))
     ((regexp-exec %repl-result-regexp (car result)) =>
      (lambda (match)
        (receive (result eval-num)
            (read-result match (cdr result))
          (values
           result                                ; Result
           eval-num                              ; # of evaluation
           (match:substring match 2)             ; Module
           (match:substring match 1)))))         ; Language
     ((regexp-exec %repl-undefined-result-regexp (car result)) =>
      (lambda (match)
        (values
         *unspecified*                ; Result
         *unspecified*                ; # of evaluation
         (match:substring match 2)    ; Module
         (match:substring match 1)))) ; Language
     (else
      (raise-repl-error (string-join result "\n")))))

  (define (read-response result)
    (let ((line (read-line repl-channel)))
      (if (eof-or-null? line)
          (handle-response (reverse result))
          (read-response (cons line result)))))

  (read-response '()))


(define (rrepl-eval rrepl-channel quoted-exp)
  "Evaluate QUOTED-EXP using RREPL-CHANNEL, return four values: an evaluation
result, a number of the evaluation, a module name and a language name.  Throw
'node-repl-error' on an error."
  (write quoted-exp rrepl-channel)
  (newline rrepl-channel)
  (write-line '(newline) rrepl-channel)
  (write-line '(exit) rrepl-channel)
  (rrepl-get-result rrepl-channel))


;;;

(define (procps-available? node)
  "Check if procps is available on a NODE."
  (command-available? (node-session node) "pgrep"))

(define (issue-procps-warning function node missing-procps-tool)
  "Issue procps warning due to a MISSING-PROCPS-TOOL on a NODE to the libssh
log."
  (format-log 'rare
              function
              (string-append
               "WARNING: '~a' from procps is not available on the node"
               " ~a; falling back to the Guile-SSH '~a' implementation")
              missing-procps-tool node missing-procps-tool))


(define (node-server-running? node)
  "Check if a RREPL is running on a NODE, return #t if it is running and
listens on an expected port, return #f otherwise."
  (define (guile-up-and-running?)
    (let ((rp (tunnel-open-forward-channel (node-tunnel node))))
      (and (channel-open? rp)
           (let ((line (read-line rp)))
             (close rp)
             (and (not (eof-object? line))
                  (string-match "^GNU Guile .*" line))))))

  (let* ((pgrep? (procps-available? node))
         (pgrep  (if pgrep? pgrep fallback-pgrep)))
    (unless pgrep?
      (issue-procps-warning "node-server-running?" node "pgrep"))
    (receive (result rc)
            (pgrep (node-session node)
                   (format #f "guile --listen=~a"
                           (node-repl-port node))
                   #:full? #t)
      (or (and (zero? rc)
               (guile-up-and-running?))
          ;; Check the default port.
          (and (= (node-repl-port node) %guile-default-repl-port)
               (receive (result rc)
                       (pgrep (node-session node)
                              "guile --listen"
                              #:full? #t)
                 (and (zero? rc)
                      (guile-up-and-running?))))))))


(define %guile-listen-command "nohup guile --listen=~a 0<&- &>/dev/null")

(define (node-run-server node)
  "Run a RREPL server on a NODE.  Throw 'node-error' with the current node and
the Guile return code from a server on an error."
  (let* ((cmd     (format #f %guile-listen-command (node-repl-port node)))
         (channel (open-remote-input-pipe (node-session node) cmd))
         (rc      (channel-get-exit-status channel)))
    (format-log 'functions "[scm] node-run-server"
                "commmand: '~a'; return code: ~a" cmd rc)
    (when (not (zero? rc))
      (node-error "node-run-server: Could not run a RREPL server"
                  node rc))
    (while (not (node-server-running? node))
      (usleep 100))))

(define (node-stop-server node)
  "Stop a RREPL server on a NODE."
  (format-log 'functions "[scm] node-stop-server"
              "trying to SIGTERM the RREPL server on ~a ..." node)
  (let* ((pkill? (procps-available? node))
         (pkill (if pkill? pkill fallback-pkill)))
    (unless pkill?
      (issue-procps-warning "node-stop-server" node "pkill"))
    (pkill (node-session node)
           (format #f "guile --listen=~a" (node-repl-port node))
           #:full? #t)
    (while (node-server-running? node)
      (format-log 'functions "[scm] node-stop-server"
                  "trying to SIGKILL the RREPL server on ~a ..."
                  node)
      (pkill (node-session node)
             (format #f "guile --listen=~a" (node-repl-port node))
             #:signal 'SIGKILL
             #:full? #t)
      (sleep 1))))


(define (node-open-rrepl node)
  "Open a RREPL.  Return a new RREPL channel."
  (and (node-start-repl-server? node)
       (not (node-server-running? node))
       (node-run-server node))
  (tunnel-open-forward-channel (node-tunnel node)))


(define (node-eval node quoted-exp)
  "Evaluate QUOTED-EXP on the node and return the evaluated result."
  (let ((repl-channel (node-open-rrepl node)))
    (rrepl-skip-to-prompt repl-channel)
    (dynamic-wind
      (const #t)
      (lambda ()
        (rrepl-eval repl-channel quoted-exp))
      (lambda ()
        (when (node-stop-repl-server? node)
          (node-stop-server node))

        ;; Close REPL-CHANNEL right away to prevent finalization from
        ;; happening in another thread at the wrong time (see
        ;; <https://bugs.gnu.org/26976>.)
        (close-port repl-channel)))))

(define (node-eval-1 node quoted-exp)
  "Evaluate QUOTED-EXP on the node and return the evaluated result.  The
procedure returns the 1st evaluated value if multiple values were returned."
  (receive (result eval-num)
      (node-eval node quoted-exp)
    (if (vector? eval-num)
        (vector-ref result 0)
        result)))


(define (node-guile-version node)
  "Get Guile version installed on a NODE, return the version string.  Return
#f if Guile is not installed."
  (receive (result rc)
      (rexec (node-session node) "which guile > /dev/null && guile --version")
    (and (zero? rc)
         (car result))))


(define-syntax-rule (with-ssh node exp ...)
  "Evaluate expressions on a remote REPL using a NODE, return four values: an
evaluation result, a number of the evaluation, a module name and a language
name.  Throw 'node-error' or 'node-repl-error' on an error."
  (node-eval node (quote (begin exp ...))))

(define (node-loadavg node)
  "Get average load of a NODE.  Return an alist of five elements as described in
proc(5) man page."
  (with-ssh node
    (use-modules (ice-9 rdelim))
    (define (list-element->number l n)
      (string->number (list-ref l n)))
    (let* ((p   (open-input-file "/proc/loadavg"))
           (raw (read-line p)))
      (close p)
      (let ((raw-list (string-split raw #\space)))
        `((one                 . ,(list-element->number raw-list 0))
          (five                . ,(list-element->number raw-list 1))
          (fifteen             . ,(list-element->number raw-list 2))
          (scheduling-entities . ,(map string->number
                                       (string-split (list-ref raw-list 3)
                                                     #\/)))
          (last-pid            . ,(list-element->number raw-list 4)))))))

;;; node.scm ends here
