;;; server.scm -- Testing of server procedures without a client.

;; Copyright (C) 2014, 2015, 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (ssh server)
             (ssh version)
             ;; Helper procedures
             (tests common))

(define %libssh-minor-version
  (string->number (cadr (string-split (get-libssh-version) #\.))))

(test-begin-with-log "server")

;;;

(test-assert "%make-server"
  (%make-server))

(test-assert-with-log "server?"
  (let ((server (%make-server))
        (x      "I'm not a server"))
    (and (server? server)
         (not (server? x)))))

(test-assert-with-log "comparison of servers"
  (let ((s1 (%make-server))
        (s2 (%make-server)))
    (and (equal? s1 s1)
         (not (equal? s1 s2)))))

(test-assert-with-log "server-set!, valid values"
  (let* ((server  (%make-server))
         (topdir  (getenv "abs_top_srcdir"))
         (options `((bindaddr      "127.0.0.1")
                    (bindport      22)
                    ,(if (= %libssh-minor-version 7)
                         (list 'hostkey %rsakey %dsakey)
                         '(hostkey "ssh-rsa" "ssh-dss"))
                    (rsakey        ,%rsakey)
                    (dsakey        ,%dsakey)
                    (banner        "string")
                    (log-verbosity nolog rare protocol packet functions)
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

(test-assert-with-log "server-set!, invalid values"
  (let ((server  (%make-server))
        (options '(;; Errors with wrong IP address format will be
                   ;; caught on `server-listen' call, so that's the
                   ;; reason that we don't check `bindaddr' with
                   ;; garbage strings here.
                   (bindaddr       #f 42)
                   ;; The same situation with rsa/dsa keys -- errors
                   ;; will be caught on `server-accept' call.
                   (rsakey         #f 42)
                   (dsakey         #f 42)

                   (bindport       "I'm not a port" -42)
                   (hostkey        "invalid value" 1 'invalid-value)
                   (banner         12345)
                   (log-verbosity  -1 0 1 2 3 4 5)
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

(test-assert-with-log "make-server"
  (let ((topdir  (getenv "abs_top_srcdir")))
    (make-server #:bindaddr      "127.0.0.1"
                 #:bindport      123456
                 #:rsakey        %rsakey
                 #:dsakey        %dsakey
                 #:banner        "banner"
                 #:log-verbosity 'nolog
                 #:blocking-mode #f)))

(test-assert-with-log "server-get"
  (let* ((topdir        (getenv "abs_top_srcdir"))
         (bindaddr      "127.0.0.1")
         (bindport      123456)
         (banner        "banner")
         (log-verbosity 'nolog)
         (blocking-mode #f)
         (server (make-server #:bindaddr      bindaddr
                              #:bindport      bindport
                              #:rsakey        %rsakey
                              #:dsakey        %dsakey
                              #:banner        banner
                              #:log-verbosity log-verbosity
                              #:blocking-mode blocking-mode)))
    (and (eq? (server-get server 'bindaddr)      bindaddr)
         (eq? (server-get server 'bindport)      bindport)
         (eq? (server-get server 'rsakey)        %rsakey)
         (eq? (server-get server 'dsakey)        %dsakey)
         (eq? (server-get server 'banner)        banner)
         (eq? (server-get server 'log-verbosity) log-verbosity)
         (eq? (server-get server 'blocking-mode) blocking-mode))))

(test-assert-with-log "server-listen"
  (let* ((topdir  (getenv "abs_top_srcdir"))
         (server  (make-server #:bindaddr "127.0.0.1"
                               #:bindport 123456
                               #:rsakey   %rsakey
                               #:log-verbosity 'nolog)))
    (server-listen server)
    #t))

(test-end "server")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; server.scm ends here.

