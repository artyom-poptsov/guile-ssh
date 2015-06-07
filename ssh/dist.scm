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

;; This module contains disrtibuted forms of some useful procedures such as
;; `map'.


;;; Code:

(define-module (ssh dist)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (ssh dist node)
  #:use-module (ssh dist job)
  #:re-export (node? node-session node-repl-port make-node node-eval
                     %node-open-repl-channel)
  #:export (dist-map))


(define (%flatten-1 lst)
  "Flatten a list LST one level down.  Return a flattened list."
  (fold-right append '() lst))


(define-syntax-rule (dist-map nodes proc lst)
  "Do list mapping using distributed computation.  The job is splitted to
nearly equal parts and hand out resulting jobs to NODES.  Return the result of
computation."
    (let ((jobs (assign-jobs nodes lst (quote proc))))
      (%flatten-1 (n-par-map (length nodes) hand-out-job jobs))))

;;; dist.scm ends here


