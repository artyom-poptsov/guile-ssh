;;; popen.scm -- Remote pipes testing.

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

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (ice-9 rdelim)
             (tests common)
             (ssh channel)
             (ssh session)
             (ssh auth)
             (ssh popen))

(test-begin "popen")


;;; Logging

(setup-test-suite-logging! "popen")

;;; Helper procedures.

(define (make-session/channel-test)
  "Make a session for a channel test."
  (let ((session (make-session-for-test)))
    (sleep 1)
    (connect! session)
    (authenticate-server session)
    (userauth-none! session)
    session))

(define (response=? channel string)
  "Read a line from a CHANNEL, check if the line is equal to a STRING."
  (string=? (read-line channel) string))

(define (input-only? port)
  (and (input-port? port)
       (not (output-port? port))))

(define (output-only? port)
  (and (output-port? port)
       (not (input-port? port))))

;;;

(test-assert-with-log "open-remote-pipe, OPEN_READ"
  (run-client-test
   start-server/exec
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (open-remote-pipe session "ping" OPEN_READ)))
       (and (input-only? channel)
            (poll channel (lambda args (response=? channel "pong"))))))))

(test-assert-with-log "open-remote-pipe, OPEN_PTY_READ"
  (run-client-test
   start-server/exec
   (lambda ()
     (let* ((session       (make-session/channel-test))
            (OPEN_PTY_READ (string-append OPEN_PTY OPEN_READ))
            (channel       (open-remote-pipe session "ping" OPEN_PTY_READ)))
       (and (input-only? channel)
            (poll channel (lambda args (response=? channel "pong"))))))))

(test-assert-with-log "open-remote-pipe, OPEN_BOTH"
  (run-client-test
   start-server/exec
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (open-remote-pipe session "ping" OPEN_BOTH)))
       (and (input-port? channel)
            (output-port? channel)
            (poll channel (lambda args (response=? channel "pong"))))))))

(test-assert-with-log "open-remote-pipe*"
  (run-client-test
   start-server/exec
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (open-remote-pipe* session OPEN_READ "ping")))
       (and (input-only? channel)
            (poll channel (lambda args (response=? channel "pong"))))))))

(test-assert-with-log "open-remote-input-pipe"
  (run-client-test
   start-server/exec
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (open-remote-input-pipe session "ping")))
       (and (input-only? channel)
            (poll channel (lambda args (response=? channel "pong"))))))))

(test-assert-with-log "open-remote-output-pipe"
  (run-client-test
   start-server/exec
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (open-remote-output-pipe session "ping")))
       (output-only? channel)))))

(test-end "popen")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; popen.scm ends here.
