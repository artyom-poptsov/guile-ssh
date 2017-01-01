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
             (ice-9 receive)
             (srfi srfi-4)
             (ssh session)
             (ssh auth)
             (ssh key)
             (ssh log)
             (ssh shell)
             (ssh popen)
             (tests common))

(test-begin-with-log "shell")

(define (call-with-connected-session/shell proc)
  "Make a session for a shell test."
  (call-with-connected-session
   (lambda (session)
     (format-log/scm 'nolog "call-with-connected-session/shell"
                     "session: ~a" session)
     (authenticate-server session)
     (userauth-none! session)
     (proc session))))

;;;

;; Client executes "uname", server replies with success code 0.
(test-assert-with-log "rexec"
  (run-client-test
   start-server/exec
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (receive (result exit-code)
            (rexec session "uname")
          (list result exit-code)))))))

(test-assert-with-log "which"
  (run-client-test
   start-server/exec
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (receive (result exit-code)
            (which session "uname")
          (and (zero? exit-code)
               (string=? (car result) "which 'uname'"))))))))

(test-assert-with-log "command-available?"
  (run-client-test
   start-server/exec
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (command-available? session "guile"))))))

(test-assert-with-log "fallback-pgrep"
  (run-client-test
   start-server/exec
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (receive (result exit-code)
            (fallback-pgrep session "guile")
          (and (zero? exit-code)
               result)))))))

(test-assert-with-log "loadavg"
  (run-client-test
   start-server/exec
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (equal? (loadavg session)
                '("0.01" "0.05" "0.10" "4/1927" "242011")))))))


;;;

(test-end "shell")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; shell.scm ends here.
