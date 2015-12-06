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

;;;

(define (make-session/channel-test)
  "Make a session for a channel test."
  (let ((session (make-session-for-test)))
    (sleep 1)
    (connect! session)
    (authenticate-server session)
    (userauth-none! session)
    session))

;;;

(test-assert-with-log "open-remote-pipe"
  (run-client-test
   start-server/exec
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (open-remote-pipe session "ping" OPEN_READ)))
       (and (input-port? channel)
            (not (output-port? channel))
            (let poll ((ready? #f))
              (if ready?
                  (string=? (read-line channel) "pong")
                  (poll (char-ready? channel)))))))))

(test-assert-with-log "open-remote-pipe*"
  (run-client-test
   start-server/exec
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (open-remote-pipe* session OPEN_READ "ping")))
       (and (input-port? channel)
            (not (output-port? channel))
            (let poll ((ready? #f))
              (if ready?
                  (string=? (read-line channel) "pong")
                  (poll (char-ready? channel)))))))))

(test-assert-with-log "open-remote-input-pipe"
  (run-client-test
   start-server/exec
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (open-remote-input-pipe session "ping")))
       (and (input-port? channel)
            (not (output-port? channel))
            (let poll ((ready? #f))
              (if ready?
                  (string=? (read-line channel) "pong")
                  (poll (char-ready? channel)))))))))

(test-assert-with-log "open-remote-output-pipe"
  (run-client-test
   start-server/exec
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (open-remote-output-pipe session "ping")))
       (and (output-port? channel)
            (not (input-port? channel)))))))

(test-end "popen")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; popen.scm ends here.
