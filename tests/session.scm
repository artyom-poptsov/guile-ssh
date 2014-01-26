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
             (ssh session))

(test-begin "session")

(test-assert "%make-session"
  (%make-session))

(test-assert "equal?, check that a session equals to itself"
  (let ((session (%make-session)))
    (equal? session session)))

(test-assert "equal?, check that two sessions aren't equal"
  (let ((session1 (%make-session))
        (session2 (%make-session)))
    (not (equal? session1 session2))))

(test-assert "session-set!, valid values"
  (let ((session (%make-session))
        (options '((host         "localhost")
                   (port         22)
                   (bindaddr     "127.0.0.1")
                   (user         "Random J. User")
                   (timeout      15)    ;seconds
                   (timeout-usec 15000) ;milliseconds
                   (ssh1         #f #t)
                   (ssh2         #f #t)
                   (log-verbosity 0 1 2 3 4 0)
                   (compression   "yes" "no")
                   (compression-level 1 2 3 4 5 6 7 8 9)))
        (res #t))
    (for-each
     (lambda (opt)
       (for-each
        (lambda (val)
          (session-set! session (car opt) (cadr opt)))
        (cdr opt)))
     options)
    res))

(test-assert "session-set!, invalid values"
  (let ((session (%make-session))
        (options '((host              12345 #t)
                   (port              "string" -22)
                   (bindaddr          12345 -12345)
                   (user              12345 -12345)
                   (timeout           "string" -15)
                   (timeout-usec      "string" -15000)
                   (ssh1              12345 "string")
                   (ssh2              12345 "string")
                   (log-verbosity     "string" -1 5)
                   (compression       12345)
                   (compression-level -1 0 10)))
        (res #t))
    (for-each
     (lambda (opt)
       (for-each
        (lambda (val)
          (catch #t
            (lambda ()
              (session-set! session (car opt) val)
              (let* ((r (test-runner-current))
                     (l (test-runner-aux-value r)))
                (format l "  opt: ~a, val: ~a -- passed mistakenly~%"
                        (car opt) val)
                (set! res #f)))
            (lambda (key . args)
              #t)))
        (cdr opt)))
     options)
    res))

(test-assert "make-session"
  (make-session #:host "localhost"
                #:port 22
                #:user "Random J. User"))

(test-assert "blocking-flush!"
  (let ((session (%make-session))
        (timeout 15))
    (eq? (blocking-flush! session timeout) 'ok)))

(test-assert "get-protocol-version"
  (let ((session (%make-session)))
    (get-protocol-version session)))

(test-assert "connected?, check that we are not connected"
  (let ((session (%make-session)))
    (not (connected? session))))

(test-end "session")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
