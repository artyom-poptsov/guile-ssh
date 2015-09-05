;;; dist.scm -- Testing of the distributed forms

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
;; along with Guile-SSH.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (srfi srfi-64)
             (ssh  session)
             (ssh  key)
             (ssh  auth)
             (ssh  dist)
             (ssh  dist job))

(test-begin "dist")

;;; Load helper procedures

(define topdir (getenv "abs_top_srcdir"))
(load (format #f "~a/tests/common.scm" topdir))

;;;


(test-assert "make-node"
  (let* ((s (make-session-for-test))
         (n (make-node s)))
    (and n
         (eq? (node-repl-port n) 37146)
         (eq? (node-session n)   s))))

(test-assert "split"
  (and (equal? (split '(a b c d e f g) 3) '((a b) (c d) (e f g)))
       (equal? (split '(a) 2) '((a)))))

(test-assert "make-job"
  (let* ((s (make-session-for-test))
         (n (make-node s))
         (data '(1 2 3))
         (proc '(lambda (n) (1+ n)))
         (j (make-job 'map n data proc)))
    (and (eq? (job-type j) 'map)
         (eq? (job-node j) n)
         (eq? (job-data j) data)
         (eq? (job-proc j) proc))))


(test-end "dist")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; dist.scm ends here.
