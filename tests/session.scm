;;; session.scm -- Testing of session procedures without a connection.

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
;; along with Guile-SSH.  If not, see <http://www.gnu.org/licenses/>.

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (ssh session)
             ;; Helper procedures
             (tests common))

(test-begin-with-log "session")

;;;

(test-assert "%make-session"
  (%make-session))

(test-assert "session?"
  (let ((session (%make-session))
        (x       "string"))
    (and (session? session)
         (not (session? x)))))

(test-assert "comparison of sessions"
  (let ((s1 (%make-session))
        (s2 (%make-session)))
    (and (equal? s1 s1)
         (not (equal? s1 s2)))))

(test-assert "session-set!, valid values"
  (let ((session (%make-session))
        (options `((host         "localhost")
                   (port         22)
                   (bindaddr     "127.0.0.1")
                   (user         "Random J. User")
                   (timeout      15)    ;seconds
                   (timeout-usec 15000) ;milliseconds
                   (ssh1         #f #t)
                   (ssh2         #f #t)
                   (log-verbosity nolog rare protocol packet functions
                                  nolog)
                   (compression   "yes" "no")
                   (compression-level 1 2 3 4 5 6 7 8 9)
                   (callbacks     ((user-data . "hello")
                                   (global-request-callback . ,(const #f))))))
        (res #t))
    (for-each
     (lambda (opt)
       (for-each
        (lambda (val)
          (session-set! session (car opt) val))
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
                   (log-verbosity     "string" -1 0 1 2 3 4 5)
                   (compression       12345)
                   (compression-level -1 0 10)
                   (callbacks         "not a list"
                                      ((global-request-callback . #f)))))
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
            (const #t)))
        (cdr opt)))
     options)
    res))

(test-assert "session-get"
  (let* ((host         "example.com")
         (port         12345)
         (user         "alice")
         (proxycommand "test")
         (callbacks    '((user-data . "test")))
         (session      (make-session #:host         host
                                     #:port         port
                                     #:user         user
                                     #:identity     %rsakey
                                     #:proxycommand proxycommand
                                     #:callbacks    callbacks)))
    (and (string=? (session-get session 'host)         host)
         (=        (session-get session 'port)         port)
         (string=? (session-get session 'user)         user)
         (string=? (session-get session 'identity)     %rsakey)
         (string=? (session-get session 'proxycommand) proxycommand)
         (equal?   (session-get session 'callbacks)    callbacks)
         ;; Make sure that default callbacks value is '#f'.
         (equal?   (session-get (%make-session) 'callbacks) #f))))

(test-assert "session-parse-config!"
  (let ((session (make-session #:host "example")))
    (session-parse-config! session %config)
    (format (current-error-port) "session: ~a~%" session)
    (and (string=? (session-get session 'host) "example.org")
         (string=? (session-get session 'user) "alice")
         (=        (session-get session 'port) 2222))))

(test-assert "make-session"
  (make-session #:host "localhost"
                #:port 22
                #:user "Random J. User"))

(test-equal-with-log "blocking-flush!"
  'ok
  (blocking-flush! (%make-session) 15))

(test-assert "connected?, check that we are not connected"
  (let ((session (%make-session)))
    (not (connected? session))))

(test-end "session")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; session.scm ends here.
