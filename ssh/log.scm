;;; log.scm -- Guile-SSH logging procedures

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

(define-module (ssh log)
  #:export (%default-log-printer
            %default-libssh-log-printer
            current-logging-callback
            set-logging-callback!
            set-log-userdata!
            get-log-userdata
            format-log))


(define (%default-log-printer priority function message userdata)
  "Default REPL-orented log printer for use with interactive Guile sessions
which comments out log messages with \";;; \""
  (display ";;; " (current-error-port))
  (%default-libssh-log-printer priority function message userdata))

(define (format-log priority procedure-name format-string . args)
  "Write a formatted message to the libssh log with the given PRIORITY.

Priority is expected to be one of the following symbols: 'nolog, 'rare,
'protocol, 'packet, 'functions

Return value is undefined."
  (let ((message (apply format #f format-string args)))
    (%write-log priority procedure-name message)))

(load-extension "libguile-ssh" "init_log_func")

;; Set the default log printer.
(set-logging-callback! %default-log-printer)

;;; log.scm ends here
