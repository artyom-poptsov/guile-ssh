;;; log.scm -- Testing of the logging callback

;; Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(use-modules (srfi srfi-64)
             (ssh  log))

(test-begin "client-server")

(test-assert "current-logging-callback"
  (eq? (current-logging-callback) %default-log-printer))


(define (custom-logging-callback priority function message useradata)
  (display "Hello Scheme World!"))

(test-assert "set-logging-callback!, custom"
  (begin
    (set-logging-callback! custom-logging-callback)
    (eq? (current-logging-callback) custom-logging-callback)))

(test-assert "set-logging-callback!, default (libssh)"
  (begin
    (set-logging-callback! %default-libssh-log-printer)
    (eq? (current-logging-callback) %default-libssh-log-printer)))

(test-assert "set-logging-callback!, default"
  (begin
    (set-logging-callback! %default-log-printer)
    (eq? (current-logging-callback) %default-log-printer)))


(test-end "client-server")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; log.scm ends here
