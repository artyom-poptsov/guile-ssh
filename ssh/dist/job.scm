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
  #:export (%make-job
            job?
            job-type
            job-node
            job-data
            job-proc

            %assign-jobs
            %hand-out-job))


(define-immutable-record-type <job>
  (%make-job type node data proc)
  job?
  (type job-type)
  (node job-node)
  (data job-data)
  (proc job-proc))

(set-record-type-printer!
 <job>
 (lambda (job port)
   (format port "#<job ~a ~a ~a>"
           (job-type job)
           (job-node job)
           (number->string (object-address job) 16))))


(define (%assign-jobs nodes lst proc)
  "Split the work to nearly equal parts according to length of NODES list and
assign each part of work to a node.  Return list of assigned jobs."
  (map (cut %make-job 'map <> <> proc)
       nodes
       (%split lst (length nodes))))

(define (%hand-out-job job)
  "Hand out JOB to the assigned node and return the result of computation."
  (node-eval (job-node job)
             `(,(job-type job) ,(job-proc job) (quote ,(job-data job)))))

(define (%split lst count)
  "Split a list LST into COUNT chunks.  Return a list of chunks."
  (receive (chunk-size-q chunk-size-r)
      (round/ (length lst) count)
    (let loop ((l   lst)
               (n   count)
               (res '()))
      (if (> n 0)
          (if (> (length l) 1)
              (loop (list-tail l chunk-size-q)
                    (1- n)
                    (append res
                            (list (list-head l
                                             (if (and (= n 1)
                                                      (> chunk-size-r 0))
                                                 (+ chunk-size-q chunk-size-r)
                                                 chunk-size-q)))))
              (loop l (1- n) (append res (list l))))
          res))))

;;; job.scm ends here.
