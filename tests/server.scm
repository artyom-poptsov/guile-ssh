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
             (ssh server))

(test-begin "server")

(test-assert "%make-server"
  (%make-server))

(test-assert "server?"
  (let ((server (%make-server))
        (x      "I'm not a server"))
    (and (server? server)
         (not (server? x)))))

(test-assert "comparison of servers"
  (let ((s1 (%make-server))
        (s2 (%make-server)))
    (and (equal? s1 s1)
         (not (equal? s1 s2)))))

(test-assert "server-set!, valid values"
  (let* ((server  (%make-server))
         (topdir  (getenv "abs_top_srcdir"))
         (options `((bindaddr      "127.0.0.1")
                    (bindport      22)
                    ;; (hostkey       ,(format #f "~a/tests/rsakey" topdir))
                    (rsakey        ,(format #f "~a/tests/rsakey" topdir))
                    (dsakey        ,(format #f "~a/tests/dsakey" topdir))
                    (banner        "string")
                    (log-verbosity 0 1 2 3 4 0)
                    (blocking-mode #f #t)))
         (log (test-runner-aux-value (test-runner-current)))
         (res #t))

    (for-each
     (lambda (opt)
       (for-each
        (lambda (val)
          (catch #t
            (lambda ()
              (server-set! server (car opt) val))
            (lambda (key . args)
              (set! res #f)
              (format log "  opt: ~a, val: ~a, error: ~a~%"
                      (car opt)
                      val
                      args))))
        (cdr opt)))
     options)
    res))

(test-end "server")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
