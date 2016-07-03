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

(test-begin "log")

(test-equal "current-logging-callback"
  %default-log-printer
  (current-logging-callback))


(define (custom-logging-callback priority function message useradata)
  (display "Hello Scheme World!"))

(test-equal "set-logging-callback!, custom"
  custom-logging-callback
  (begin
    (set-logging-callback! custom-logging-callback)
    (current-logging-callback)))

(test-equal "set-logging-callback!, default (libssh)"
  %default-libssh-log-printer
  (begin
    (set-logging-callback! %default-libssh-log-printer)
    (current-logging-callback)))

(test-equal "set-logging-callback!, default"
  %default-log-printer
  (begin
    (set-logging-callback! %default-log-printer)
    (current-logging-callback)))


(test-assert "set-log-verbosity!"
  (begin
    (set-log-verbosity! 'functions)
    (catch #t
      (lambda ()
        (set-log-verbosity! 'wrong-verbosity)
        #f)
      (lambda (key . args)
        #t))))

(test-equal "get-log-verbosity"
  'functions
  (get-log-verbosity))


(test-end "log")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; log.scm ends here
