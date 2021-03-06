;;; job.scm -- Distributed jobs.

;; Copyright (C) 2015-2020 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module describes a job object that holds information on a distributed
;; computing job.  This information includes the job type, the node that is
;; assigned for execution of the job (see (ssh dist node)), job data and
;; procedure(s) to process the data.
;;
;; The module exports the following procedures:
;;   make-job
;;   job?
;;   job-type
;;   job-node
;;   set-job-node
;;   job-data
;;   job-proc
;;   assign-eval
;;   assign-map
;;   hand-out-job
;;   job->sexp
;;   split
;;
;; See the Info documentation for detailed description of these procedures.


;;; Code:

(define-module (ssh dist job)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (ssh dist node)
  #:use-module (ssh log)
  #:export (make-job
            job?
            job-type
            job-node set-job-node
            job-data
            job-proc

            assign-eval
            assign-map
            hand-out-job

            ;; Helper procedures
            job->sexp
            split))


(define-record-type <job>
  (make-job type node data proc)
  job?
  (type job-type)
  (node job-node)
  (data job-data)
  (proc job-proc))

(define (set-job-node job node)
  "A functional setter that returns a new copy of JOB with the node field
changed to a NODE."
  (make-job (job-type job) node (job-data job) (job-proc job)))

(set-record-type-printer!
 <job>
 (lambda (job port)
   (format port "#<job ~a ~a ~a>"
           (job-type job)
           (job-node job)
           (number->string (object-address job) 16))))

(define (job->sexp job)
  "Convert a JOB to an equivalent symbolic expression."
  (case (job-type job)
    ((map)
     `(map ,(job-proc job) ',(job-data job)))
    ((eval)
     `(map primitive-eval ',(job-proc job)))
    (else
     (error "Unknown job type" job))))


(define (split lst count)
  "Split a list LST into COUNT chunks.  Return a list of chunks."
  (define (append-list lst . items)
    "Append ITEMS list to LST."
    (append lst items))
  (define (list-rest lst lst-len k)
    "Skip the first K elements of LST, return the list with the rest of the
LST elements.  If K is lesser than LST-LEN then return all the elements of
LST."
    (if (< k lst-len)
        (list-tail lst k)
        lst))
  (receive (chunk-size-q chunk-size-r)
      (round/ (length lst) count)
    (let loop ((l   lst)
               (n   count)
               (res '()))
      (let ((l-len (length l)))
        (if (> n 0)
            (if (> l-len 1)
                (loop (list-rest l l-len chunk-size-q)
                      (1- n)
                      (append-list res
                                   (if (> n 1)
                                       (list-head l chunk-size-q)
                                       l)))
                (append-list res l))
            res)))))

(define (assign-eval nodes expressions)
  "Split an EXPRESSIONS list to nearly equal parts according to the length of
a NODES list and assign each evaluation job to a node.  Return a list of
assigned jobs."
  (map (cut make-job 'eval <> #f <>)
       nodes
       (split expressions (length nodes))))

(define (assign-map nodes lst proc)
  "Split the 'map' work to nearly equal parts according to the length of NODES
list and assign each part of work to a node.  Return a list of assigned jobs."
  (map (cut make-job 'map <> <> proc)
       nodes
       (split lst (length nodes))))

(define (hand-out-job job)
  "Hand out JOB to the assigned node and return the result of computation."
  (format-log 'functions "hand-out-job" "node: ~a; type: ~a; proc: ~a; data: ~a"
              (job-node job) (job-type job) (job-proc job) (job-data job))
  (case (job-type job)
    ((map)
     (node-eval-1 (job-node job)
                  `(,(job-type job) ,(job-proc job) (quote ,(job-data job)))))
    ((eval)
     (map (lambda (expr)
            (node-eval-1 (job-node job) expr))
          (job-proc job)))
    (else
     (error "Unknown job type" job))))

;;; job.scm ends here.
