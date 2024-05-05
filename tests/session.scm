;;; session.scm -- Testing of session procedures without a connection.

;; Copyright (C) 2014-2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (ice-9 regex)
             (ssh session)
             (ssh log)
             (ssh version)
             ;; Helper procedures
             (tests common))

(define %libssh-minor-version
  (string->number (cadr (string-split (get-libssh-version) #\.))))

(set-log-verbosity! 'functions)

(test-begin-with-log "session")

;;;

(test-assert "%make-session"
  (%make-session))

(test-assert-with-log "%make-session, gc test"
  (let ((max-sessions 1000))
    (do ((idx 1 (+ idx 1)))
        ((> idx max-sessions))
      (when (zero? (euclidean-remainder idx 100))
        (format-log/scm 'nolog "" (format #f "~d / ~d sessions created ..."
                                          idx max-sessions)))
      (%make-session))
    #t))

(test-assert "session?"
  (let ((session (%make-session))
        (x       "string"))
    (and (session? session)
         (not (session? x)))))



(test-assert "display, undefined values"
  (let* ((session (%make-session))
         (output  (with-output-to-string
                    (lambda ()
                      (display session)))))
    (if (string-match "#<session #<undefined>@#<undefined>:22 \\(disconnected\\) [0-9a-z]+>"
                      output)
        output
        (error "Wrong output" output))))

(test-assert "display, defined values"
  (let* ((session (make-session #:host "example.org"
                                #:user "alice"))
         (output  (with-output-to-string
                    (lambda ()
                      (display session)))))
    (if (string-match "#<session alice@example.org:22 \\(disconnected\\) [0-9a-z]+>"
                      output)
        output
        (error "Wrong output" output))))



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
                   (compression-level 1 2 3 4 5 6 7 8 9)
                   (compression   "yes" "no")
                   (callbacks     ((user-data . "hello")
                                   (global-request-callback . ,(const #f))))))
        (res #t))

    (if (>= %libssh-minor-version 8)
        (set! options (cons  '(nodelay #f #t) options)))

    (for-each
     (lambda (opt)
       (for-each
        (lambda (val)
          (session-set! session (car opt) val))
        (cdr opt)))
     options)
    res))

(unless (>= %libssh-minor-version 10)
  (test-skip "session-set!, rsa-min-size"))
(test-assert "session-set!, rsa-min-size"
  (let ((session (%make-session)))
    (session-set! session 'rsa-min-size 1024)))

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

    (if (>= %libssh-minor-version 8)
        (set! options (cons '(nodelay 12345 "string") options)))

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

(test-error "session-set!, invalid option type"
  'wrong-type-arg
  (let ((session (%make-session)))
    (session-set! session "non-valid type" "value")))

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

(test-error "session-get, non-session object"
  'wrong-type-arg
  (session-get "non-session object" 'test))

(test-error "session-get, invalid option"
  'guile-ssh-error
  (let ((session (%make-session)))
    (session-get session 'wrong-option)))

(test-assert "session-parse-config!"
  (let ((session (make-session #:host "example")))
    (session-parse-config! session %config)
    (format (current-error-port) "session: ~a~%" session)
    (format (current-error-port) "host: ~a~%" (session-get session 'host))
    (format (current-error-port) "user: ~a~%" (session-get session 'user))
    (format (current-error-port) "port: ~a~%" (session-get session 'port))
    (and (string=? (session-get session 'host) "example.org")
         (string=? (session-get session 'user) "alice")
         (=        (session-get session 'port) 2222))))

(test-error "session-parse-config!, non-session object"
  'wrong-type-arg
  (session-parse-config! "non-session object" "wrong-value"))

(test-error "session-parse-config!, wrong config file"
  'guile-ssh-error
  (session-parse-config! (%make-session) "wrong-value"))

(test-assert "make-session"
  (make-session #:host "localhost"
                #:port 22
                #:user "Random J. User"))

(test-assert "make-session, '#:config' and '#:host' is specified"
  (make-session #:host   "localhost"
                #:config %config))

(test-assert "make-session, '#:config' as a boolean value"
  (make-session #:host   "localhost"
                #:config #f))

(test-error "make-session, only '#:config' is specified"
  'guile-ssh-error
  (make-session #:config %config))

(test-equal "make-session: keywords must overwrite config options"
  22
  (let ((s (make-session #:host   "example"
                         #:port   22
                         ;; Configuration sets port to 2222
                         #:config %config)))
    (session-get s 'port)))

(test-equal-with-log "blocking-flush!"
  'ok
  (blocking-flush! (%make-session) 15))

(test-error "connected?, non-session object"
  'wrong-type-arg
  (connected? "non-session object"))

(test-assert "connected?, check that we are not connected"
  (let ((session (%make-session)))
    (not (connected? session))))

(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "session")

(exit (= 0 exit-status))

;;; session.scm ends here.
