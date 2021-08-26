;;; shell.scm -- Remote shell tests.

;; Copyright (C) 2016, 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (ice-9 rdelim)
             (ice-9 receive)
             (srfi srfi-4)
             (ssh session)
             (ssh channel)
             (ssh auth)
             (ssh key)
             (ssh log)
             (ssh shell)
             (ssh popen)
             (ssh message)
             (tests common))

(set-log-verbosity! 'functions)

(test-begin-with-log "shell")


;;;

;; Client executes "uname", server replies with success code 0.
(test-assert-with-log "rexec"
  (run-client-test
   ;; Server
   (lambda (server)
     (start-server/exec server (const #t)))
   ;; Client
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (format-log/scm 'nolog "rexec" "session: ~a" session)
        (receive (result exit-code)
            (rexec session "ping")
          (list result exit-code)))))))

(test-assert-with-log "which"
  (run-client-test
   (lambda (server)
     (start-server/exec server (const #t)))
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (receive (result exit-code)
            (which session "uname")
          (and (zero? exit-code)
               (string=? (car result) "which 'uname'"))))))))

(test-assert-with-log "pgrep"
  (run-client-test
   (lambda (server)
     (start-server/exec server (const #t)))
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (receive (result exit-code)
            (pgrep session "guile --listen=37146" #:full? #t)
          (and (zero? exit-code)
               (string=? "pgrep -f 'guile --listen=37146'"
                         (car result)))))))))

(test-assert-with-log "command-available?"
  (run-client-test
   (lambda (server)
     (start-server/exec server (const #t)))
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (command-available? session "guile"))))))

(test-assert-with-log "pgrep, gulile"
  (run-client-test
   (lambda (server)
     (start-server/exec server (const #t)))
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (receive (result exit-code)
            (pgrep session "guile" #:use-guile? #t)
          (and (zero? exit-code)
               result)))))))

(test-assert-with-log "loadavg"
  (run-client-test
   (lambda (server)
     (start-server/exec server (const #t)))
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (equal? (loadavg session)
                '("0.01" "0.05" "0.10" "4/1927" "242011")))))))


;;;
(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "shell")

(exit (= 0 exit-status))

;;; shell.scm ends here.
