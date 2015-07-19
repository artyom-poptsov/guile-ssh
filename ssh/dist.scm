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
;;   make-node
;;   node?
;;   node-session
;;   node-repl-port
;;
;; See the Info documentation for the detailed description of these
;; procedures.


;;; Code:

(define-module (ssh dist)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ssh dist node)
  #:use-module (ssh dist job)
  #:re-export (node? node-session node-repl-port make-node)
  #:export (distribute dist-map))


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
    (flatten-1 (n-par-map (length nodes) (cut execute-job nodes <>) jobs))))

;;; dist.scm ends here


