;;; node.scm -- Distributed computing node

;; Copyright (C) 2015, 2016, 2017, 2018 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
;;   node-rrepl-port
;;   make-node
;;   node-eval
;;   node-open-rrepl
;;   node-guile-version
;;   node-loadavg
;;   with-ssh
;;
;;   rrepl-eval
;;   rrepl-skip-to-prompt
;;   rrepl-get-result
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
            node-rrepl-port
            make-node
            node-eval
            node-eval-1
            node-guile-version
            node-loadavg
            with-ssh

            rrepl-eval
            rrepl-skip-to-prompt
            rrepl-get-result))


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
  (%make-node session rrepl-port guile-version)
  node?
  (session node-session)                                       ; <session>
  (rrepl-port node-rrepl-port node-rrepl-port-set!)            ; <port>
  (guile-version node-guile-version node-guile-version-set!))  ; <string>

(set-record-type-printer!
 <node>
 (lambda (node port)
   (let ((s (node-session node)))
     (format port "#<node ~a@~a:~a/~a (~a) ~a>"
             (session-get s 'user)
             (session-get s 'host)
             (session-get s 'port)
             (node-guile-version node)
             (if (port-closed? (node-rrepl-port node))
                 "stopped"
                 "running")
             (number->string (object-address node) 16)))))


;;;  Helper procedures.

(define (rrepl-skip-to-prompt repl-channel)
  "Read from REPL-CHANNEL until REPL is observed.  Throw 'node-error' on an
error."
  (let loop ((line (read-line repl-channel)))
    (when (eof-object? line)
      (node-error "Could not locate RREPL prompt" repl-channel))
    (unless (string=? "Enter `,help' for help." line)
      (loop (read-line repl-channel)))))

(define (session-open-rrepl session)
  "Open a stateless RREPL.  Return a new RREPL channel."
  (open-remote-pipe* session OPEN_BOTH "guile" "-q"))

(define (make-rrepl session)
  (let ((rrepl-port (session-open-rrepl session)))
    (let ((guile-version (read-line rrepl-port)))
      (when (eof-object? guile-version)
        (node-repl-error "Could not locate GNU Guile on the node." session))
      (rrepl-skip-to-prompt rrepl-port)
      (values rrepl-port guile-version))))


;;;

(define* (make-node session)
  "Make a new distributed computing node."
  (receive (rrepl-port guile-version)
      (make-rrepl session)
    (%make-node session rrepl-port guile-version)))

(define (node-stop-rrepl! node)
  "Stop a RREPL on a NODE."
  (close-port (node-rrepl-port node)))

(define (node-start-rrepl! node)
  "Start a new RREPL on a NODE."
  (receive (rrepl-port guile-version)
      (make-rrepl session)
    (node-rrepl-port-set! node rrepl-port)
    (node-guile-version-set! node guile-version)))


;;; Remote REPL (RREPL)

(define (read-string str)
  "Read a string STR."
  (call-with-input-string str read))


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
  (rrepl-get-result rrepl-channel))


;;; Evaluation procedures.

(define (node-eval node quoted-exp)
  "Evaluate QUOTED-EXP on the node and return the evaluated result."
  (rrepl-eval (node-rrepl-port node) quoted-exp))

(define (node-eval-1 node quoted-exp)
  "Evaluate QUOTED-EXP on the node and return the evaluated result.  The
procedure returns the 1st evaluated value if multiple values were returned."
  (receive (result eval-num)
      (node-eval node quoted-exp)
    (if (vector? eval-num)
        (vector-ref result 0)
        result)))


;;; Useful macros and procedures.

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
