;;; job.scm -- Distributed jobs.

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



;;; Code:

(define-module (ssh dist job)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (ssh dist node)
  #:export (make-job
            job?
            job-type
            job-node set-job-node
            job-data
            job-proc

            assign-eval
            assign-jobs
            hand-out-job

            ;; Helper procedures
            job->sexp
            split))


(define-immutable-record-type <job>
  (make-job type node data proc)
  job?
  (type job-type)
  (node job-node set-job-node)
  (data job-data)
  (proc job-proc))

(set-record-type-printer!
 <job>
 (lambda (job port)
   (format port "#<job ~a ~a ~a>"
           (job-type job)
           (job-node job)
           (number->string (object-address job) 16))))

(define (job->sexp job)
  "Convert a JOB to a analogous symbolic expression."
  (case (job-type job)
    ((map)
     `(map ,(job-proc job) ',(job-data job)))
    ((eval)
     `(parallel ,@(job-proc job)))
    (else
     (error "Unknown job type" job))))


(define (split lst count)
  "Split a list LST into COUNT chunks.  Return a list of chunks."
  (receive (chunk-size-q chunk-size-r)
      (round/ (length lst) count)
    (let loop ((l   lst)
               (n   count)
               (res '()))
      (if (> n 0)
          (if (> (length l) 1)
              (loop (if (< chunk-size-q (length l))
                        (list-tail l chunk-size-q)
                        l)
                    (1- n)
                    (append res
                            (list (list-head l
                                             (if (and (= n 1)
                                                      (not (= chunk-size-r 0)))
                                                 (+ chunk-size-q chunk-size-r)
                                                 chunk-size-q)))))
              (append res (list l)))
          res))))

(define (assign-eval nodes expressions)
  (map (cut make-job 'eval <> #f <>)
       (list-head nodes (length expressions))
       (split expressions (length nodes))))

(define (assign-jobs nodes lst proc)
  "Split the work to nearly equal parts according to length of NODES list and
assign each part of work to a node.  Return list of assigned jobs."
  (map (cut make-job 'map <> <> proc)
       (list-head nodes (length expressions))
       (split lst (length nodes))))

(define (hand-out-job job)
  "Hand out JOB to the assigned node and return the result of computation."
  (case (job-type job)
    ((map)
     (node-eval (job-node job)
                `(,(job-type job) ,(job-proc job) (quote ,(job-data job)))))
    ((eval)
     (map (lambda (expr)
            (node-eval (job-node job) expr))
          (job-proc job)))))

;;; job.scm ends here.
