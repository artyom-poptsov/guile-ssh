;;; node.scm -- Distributed computing node

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
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ssh session)
  #:use-module (ssh session)
  #:use-module (ssh channel)
  #:use-module (ssh tunnel)
  #:export (node?
            node-session
            node-repl-port
            make-node
            node-eval
            node-eval-1

            node-open-rrepl
            rrepl-eval
            rrepl-skip-to-prompt
            rrepl-get-result))


;;; Error reporting

(define (node-error . args)
  "Raise a node error."
  (apply throw (cons 'node-error args)))

(define (node-repl-error . args)
  "Raise a REPL error."
  (apply throw (cons 'node-repl-error args)))


;;; Node type

(define-record-type <node>
  (%make-node tunnel repl-port)
  node?
  (tunnel node-tunnel)
  (repl-port node-repl-port))

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


(define* (make-node session #:optional (repl-port 37146))
  "Make a new distributed computing node."
  (let ((tunnel (make-tunnel session
                             #:port 0          ;Won't be used
                             #:host "localhost"
                             #:host-port repl-port)))
    (%make-node tunnel repl-port)))


;;; Remote REPL (RREPL)

(define (node-open-rrepl node)
  "Open a RREPL.  Return a new RREPL channel."
  (tunnel-open-forward-channel (node-tunnel node)))

(define (rrepl-skip-to-prompt repl-channel)
  "Read from REPL-CHANNEL until REPL is observed.  Throw 'node-error' on an
error."
  (let loop ((line (read-line repl-channel)))

    (and (eof-object? line)
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

(define (rrepl-get-result repl-channel)
  "Get result of evaluation form REPL-CHANNEL, return four values: an
evaluation result, a number of the evaluation, a module name and a language
name.  Throw 'node-repl-error' on an error."

  (define (raise-repl-error result)
    (let loop ((line   (read-line repl-channel))
               (result result))
      (if (or (eof-object? line) (string-null? line))
          (node-repl-error "Evaluation failed" result)
          (loop (read-line repl-channel)
                (string-append result "\n" line)))))

  (define (read-result match)
    (let ((matches
           (let loop ((line    (read-line repl-channel))
                      (matches (list match)))
             (if (or (eof-object? line) (string-null? line)
                     (regexp-exec %repl-undefined-result-regexp line))
                 (reverse matches)
                 (loop (read-line repl-channel)
                       (cons (regexp-exec %repl-result-2-regexp line) matches))))))
      (let ((len (length matches)))
        (if (= len 1)
            (let ((m (car matches)))
              (values (call-with-input-string (match:substring m 4)
                                              read)
                      (string->number (match:substring m 3))))
            (let ((rv (make-vector len))
                  (nv (make-vector len)))
              (vector-set! rv 0
                           (call-with-input-string (match:substring (car matches)
                                                                    4)
                                                   read))
              (vector-set! nv 0
                           (string->number (match:substring (car matches) 3)))
              (do ((i 1 (1+ i)))
                  ((= i len))
                (vector-set! rv i
                             (call-with-input-string
                              (match:substring (list-ref matches i)
                                               2)
                              read))
                (vector-set! nv i
                             (string->number (match:substring (list-ref matches
                                                                        i)
                                                              1))))
              (values rv nv))))))

  (let ((result (read-line repl-channel)))
    (if (string-null? result)
        (rrepl-get-result repl-channel)
        (begin
          (cond
           ((regexp-exec %repl-result-regexp result) =>
            (lambda (match)
              (receive (result eval-num)
                  (read-result match)
                (values
                 result                                ; Result
                 eval-num                              ; # of evaluation
                 (match:substring match 2)             ; Module
                 (match:substring match 1)))))         ; Language
           ((regexp-exec %repl-error-regexp result) =>
            (lambda (match) (raise-repl-error result)))
           ((regexp-exec %repl-undefined-result-regexp result) =>
            (lambda (match)
              (values
               *unspecified*                ; Result
               *unspecified*                ; # of evaluation
               (match:substring match 2)    ; Module
               (match:substring match 1)))) ; Language
           (else
            (raise-repl-error result)))))))

(define (rrepl-eval rrepl-channel quoted-exp)
  "Evaluate QUOTED-EXP using RREPL-CHANNEL, return four values: an evaluation
result, a number of the evaluation, a module name and a language name.  Throw
'node-repl-error' on an error."
  (write quoted-exp rrepl-channel)
  (newline rrepl-channel)
  (write-line '(newline) rrepl-channel)
  (rrepl-get-result rrepl-channel))


;;;

(define (node-eval node quoted-exp)
  "Evaluate QUOTED-EXP on the node and return the evaluated result."
  (let ((repl-channel (node-open-rrepl node)))
    (rrepl-skip-to-prompt repl-channel)
    (rrepl-eval repl-channel quoted-exp)))

(define (node-eval-1 node quoted-exp)
  "Evaluate QUOTED-EXP on the node and return the evaluated result.  The
procedure returns the 1st evaluated value if multiple values were returned."
  (receive (result eval-num)
      (node-eval node quoted-exp)
    (if (vector? eval-num)
        (vector-ref result 0)
        result)))

;;; node.scm ends here
