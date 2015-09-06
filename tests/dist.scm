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
             (ice-9 receive)
             (ssh  session)
             (ssh  key)
             (ssh  auth)
             (ssh  dist)
             (ssh  dist job)
             (ssh  dist node))

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

(test-assert "set-job-node"
  (let* ((s    (make-session-for-test))
         (n1   (make-node s))
         (n2   (make-node s))
         (data '())
         (proc '(lambda (n) (1+ n)))
         (j1   (make-job 'map n1 data proc))
         (j2   (set-job-node j1 n2)))
    (and (not (eq? j1 j2))
         (eq? (job-type j1) (job-type j2))
         (eq? (job-node j1) n1)
         (eq? (job-node j2) n2)
         (eq? (job-data j1) (job-data j2))
         (eq? (job-proc j1) (job-proc j2)))))


;;; Testing of 'rrepl-get-result'.
;; These test cases are intended to test various inputs for 'rrepl-get-result'
;; procedure.

(test-assert "rrepl-get-result"
  (receive (result eval-num module-name lang)
      (call-with-input-string "scheme@(guile-user)> $0 = test"
                              rrepl-get-result)
    (and (eq?      result      'test)
         (=        eval-num    0)
         (string=? module-name "(guile-user)")
         (string=? lang        "scheme"))))

(test-assert "rrepl-get-result, unspecified"
  (receive (result eval-num module-name lang)
      (call-with-input-string "scheme@(guile-user)> "
                              rrepl-get-result)
    (and (eq?      result      *unspecified*)
         (eq?      eval-num    *unspecified*)
         (string=? module-name "(guile-user)")
         (string=? lang        "scheme"))))

(test-assert "rrepl-get-result, error"
  (catch 'node-repl-error
         (lambda ()
           (call-with-input-string "scheme@(guile-user)> ERROR: error."
                                   rrepl-get-result)
           #f)
         (lambda (key . args)
           (string=? (cadr args) "scheme@(guile-user)> ERROR: error."))))

(test-assert "rrepl-get-result, elisp"
  (receive (result eval-num module-name lang)
      (call-with-input-string "elisp@(guile-user)> $0 = #nil"
                              rrepl-get-result)
    (and (eq?      result      '#nil)
         (=        eval-num    0)
         (string=? module-name "(guile-user)")
         (string=? lang        "elisp"))))

(test-assert "rrepl-get-result, multiple values"
  (receive (result eval-num module-name lang)
      (call-with-input-string "scheme@(guile-user)> $0 = v1\n$1 = v2"
                              rrepl-get-result)
    (and (vector? eval-num)
         (vector? result)
         (eq?      (vector-ref result 0)   'v1)
         (eq?      (vector-ref result 1)   'v2)
         (=        (vector-ref eval-num 0) 0)
         (=        (vector-ref eval-num 1) 1)
         (string=? module-name "(guile-user)")
         (string=? lang        "scheme"))))

;;;


(test-end "dist")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; dist.scm ends here.
