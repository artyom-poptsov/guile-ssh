;;; sssh-ssshd.scm -- Communication between sssh and ssshd.

;; Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This file is a part of libguile-ssh.
;;
;; libguile-ssh is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; libguile-ssh is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with libguile-ssh.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (srfi srfi-64)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex))

(test-begin "sssh-ssshd")


;;;

(define *topdir* (getenv "abs_top_srcdir"))
(define *rsakey* (format #f "~a/tests/rsakey" *topdir*))
(define *test-cmd* "uname --all")

(define *ssshd-cmd* (format #f "~a/examples/ssshd.scm --detach -r ~a"
                            *topdir* *rsakey*))
(define *sssh-cmd*  (format #f "~a/examples/sssh.scm -i ~a -p 12345 127.0.0.1 '~a'"
                            *topdir* *rsakey* *test-cmd*))

(define pid #f)


;;; Tests

(test-assert "ssshd, start"
  (let ((p (open-input-pipe *ssshd-cmd*)))
    (let r ((l (read-line p)))
      (if (not (eof-object? l))
          (let ((m (string-match "PID: ([0-9]+)" l)))
            (if (regexp-match? m)
                (set! pid (string->number (match:substring m 1)))
                (r (read-line p))))))
    pid))

(define output (read-line (open-input-pipe *test-cmd*)))

(test-assert "sssh, exec"
  (let ((p (open-input-pipe *sssh-cmd*))
        (res #f))
    (let r ((l (read-line p)))
      (if (not (eof-object? l))
          (if (string=? output l)
              (set! res #t)
              (r (read-line p)))))
    res))


;;; Stop the ssshd server

(if pid
    (kill pid SIGTERM))


(test-end "sssh-ssshd")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; sssh-ssshd.scm ends here.
