;;; dist.scm -- Spirit of disrtibuted computing for Scheme.

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
;; along with Guile-SSH.  If not, see
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This module contains distributed forms of some useful procedures such as
;; 'map'.
;;
;; The module exports:
;;   distribute
;;   dist-map
;;   rrepl
;;   make-node
;;   node?
;;   node-session
;;   node-repl-port
;;
;; See the Info documentation for the detailed description of these
;; procedures.


;;; Code:

(define-module (ssh dist)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ssh session)
  #:use-module (ssh dist node)
  #:use-module (ssh dist job)
  #:re-export (node? node-session node-repl-port make-node)
  #:export (distribute dist-map rrepl))


(define (flatten-1 lst)
  "Flatten a list LST one level down.  Return a flattened list."
  (fold-right append '() lst))

(define (warning fmt . args)
  (apply format (current-error-port) (string-append "WARNING: " fmt) args))

(define (execute-job nodes job)
  "Execute a JOB, handle errors."
  (catch 'node-error
    (lambda ()
      (catch 'node-repl-error
        (lambda ()
          (hand-out-job job))
        (lambda args
          (format (current-error-port)
                  "ERROR: In ~a:~%~a:~%~a~%"
                  job (cadr args) (caddr args))
          (error "Could not execute a job" job))))
    (lambda args
      (warning "Could not execute a job ~a~%" job)
      (let ((nodes (delete (job-node job) nodes)))
        (and (null? nodes)
             (error "Could not execute a job" job))
        (warning "Passing a job ~a to a node ~a ...~%" job (car nodes))
        (execute-job nodes (set-job-node job (car nodes)))))))


(define-syntax-rule (distribute nodes expr ...)
  "Evaluate each EXPR in parallel, using distributed computation.  Split the
job to nearly equal parts and hand out each of resulting sub-jobs to NODES.
Return the results of N expressions as a set of N multiple values."
  (let ((jobs (assign-eval nodes (list (quote expr) ...))))
    (apply values (flatten-1 (n-par-map (length jobs) (cut execute-job nodes <>)
                                        jobs)))))

(define-syntax-rule (dist-map nodes proc lst)
  "Do list mapping using distributed computation.  The job is splitted to
nearly equal parts and hand out resulting jobs to NODES.  Return the result of
computation."
  (let ((jobs (assign-map nodes lst (quote proc))))
    (flatten-1 (n-par-map (length jobs) (cut execute-job nodes <>) jobs))))


(define (rrepl node)
  "Start a remote REPL (RREPL) session using NODE.  Enter ',rq' to disconnect
from the RREPL."
  (let* ((s            (node-session node))
         (user         (session-get s 'user))
         (host         (session-get s 'host))
         (port         (session-get s 'port))
         (repl-port    (node-repl-port node))
         (repl-channel (node-open-rrepl node))
         (rrepl-id     (format #f "~a@~a:~a/~a" user host port repl-port)))
    (rrepl-skip-to-prompt repl-channel)
    (receive (result result-num module language)
        (rrepl-eval repl-channel '#t)
      (format #t "~a ~a@~a> "
              rrepl-id language module))
    (while #t
      (let ((exp (read)))
        (format #t "~%DEBUG: expression: ~a~%" exp)
        (cond
         ((equal? exp '(unquote rq))
          (break))
         ((equal? (car exp) 'unquote)
          (let ((args (read-line)))
            (receive (result result-num module language)
                (rrepl-eval repl-channel
                            (format #f ",~a ~a"
                                    (cadr exp) args))
              (format #t "$~a = ~a~%~a ~a@~a> "
                      result-num result
                      rrepl-id language module))))
         (else
          (receive (result result-num module language)
              (rrepl-eval repl-channel exp)
            (format #t "$~a = ~a~%~a ~a@~a> "
                    result-num result
                    rrepl-id language module))))))))

;;; dist.scm ends here


