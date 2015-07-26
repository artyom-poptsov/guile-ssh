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

            node-open-rrepl))


;;; Error reporting

(define (node-error . args)
  "Raise a node error."
  (apply throw (cons 'node-error args)))

(define (node-repl-error . args)
  "Raise a REPL error."
  (apply throw (cons 'node-repl-error args)))

;;;


(define-immutable-record-type <node>
  (%make-node tunnel repl-port)
  node?
  (tunnel node-tunnel)
  (repl-port node-repl-port))

(define (node-session node)
  "Get node session."
  (tunnel-session (node-tunnel node)))

(define (node-open-rrepl node)
  "Open a RREPL.  Return a new RREPL channel."
  (tunnel-open-forward-channel (node-tunnel node)))

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

(define (rrepl-skip-to-prompt repl-channel)
  "Read from REPL-CHANNEL until REPL is observed.  Throw 'node-error' on an
error."
  (let loop ((line (read-line repl-channel)))

    (and (eof-object? line)
         (node-error "Could not locate REPL prompt" repl-channel))

    (or (string=? "Enter `,help' for help." line)
        (loop (read-line repl-channel)))))


(define %repl-result-regexp
  (make-regexp "scheme@\\(guile-user\\)> \\$([0-9]+) = (.*)"))

(define (rrepl-get-result repl-channel)
  "Get result of evaluation form REPL-CHANNEL, return two values: the number
of evaluation and the evaluation result.  Throw 'node-repl-error' on an
error."
  (let* ((result (read-line repl-channel))
         (match  (regexp-exec %repl-result-regexp result)))
    (or match
        (let loop ((line   (read-line repl-channel))
                   (result result))
          (if (or (eof-object? line) (string-null? line))
              (node-repl-error "Evaluation failed" result)
              (loop (read-line repl-channel) (string-append result "\n" line)))))
    (values (match:substring match 1) (match:substring match 2))))

(define (rrepl-eval rrepl-channel quoted-exp)
  "Evaluate QUOTED-EXP using RREPL-CHANNEL, return the result of evaluation."
  (write-line quoted-exp rrepl-channel)
  (rrepl-get-result rrepl-channel))

(define (node-eval node quoted-exp)
  "Evaluate QUOTED-EXP on the node and return the evaluated result."
  (let ((repl-channel (node-open-rrepl node)))
    (rrepl-skip-to-prompt repl-channel)
    (receive (num val)
        (rrepl-eval repl-channel quoted-exp)
      (call-with-input-string val
        read))))

;;; node.scm ends here
