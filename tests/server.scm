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

(test-assert "server-set!, invalid values"
  (let ((server  (%make-server))
        (options '((bindaddr       "I'm not a IP address" 42)
                   (bindport       "I'm not a port" -42)
                   (rsakey         "I'm not a RSA key" 42)
                   (dsakey         "I'm not a DSA key" 42)
                   (banner         12345)
                   (log-verbosity  -1 5)
                   (blocking-mode  42 "string")))
        (log (test-runner-aux-value (test-runner-current)))
        (res #t))

    (for-each
     (lambda (opt)
       (for-each
        (lambda (val)
          (catch #t
            (lambda ()
              (server-set! server (car opt) val)
              (format log "  opt: ~a, val: ~a -- passed mistakenly~%"
                      (car opt) val)
              (set! res #f))
            (lambda (key . args)
              #t)))
        (cdr opt)))
     options)
    res))

(test-assert "make-server"
  (let ((topdir  (getenv "abs_top_srcdir")))
    (make-server #:bindaddr      "127.0.0.1"
                 #:bindport      123456
                 #:rsakey        (format #f "~a/tests/rsakey" topdir)
                 #:dsakey        (format #f "~a/tests/dsakey" topdir)
                 #:banner        "banner"
                 #:log-verbosity 1
                 #:blocking-mode #f)))

(test-assert "server-listen"
  (let* ((topdir  (getenv "abs_top_srcdir"))
         (server  (make-server #:bindaddr "127.0.0.1"
                               #:bindport 123456
                               #:rsakey   (format #f "~a/tests/rsakey" topdir)
                               #:log-verbosity 1)))
    (server-listen server)
    #t))

(test-end "server")

(exit (= (test-runner-fail-count (test-runner-current)) 0))
