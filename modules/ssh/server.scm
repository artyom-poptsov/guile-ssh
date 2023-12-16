;;; server.scm -- SSH server API.

;; Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
;; along with Guile-SSH.  If not, see
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This module contains API that is used for SSH server.
;;
;; These methods are exported:
;;
;;   %make-server
;;   make-server
;;   server-accept
;;   server-set!
;;   server-get
;;   server-listen!
;;   server-handle-key-exchange
;;   server-message-get


;;; Code:

(define-module (ssh server)
  #:use-module (ice-9 optargs)
  #:use-module (ssh log)
  #:export (server
            server?
	    %make-server
            make-server
            server-accept
            server-set!
            server-get
            server-listen
            server-handle-key-exchange
            server-message-get))

;; Set a SSH option if it is specified by the user
(define-macro (server-set-if-specified! option)
  `(if ,option (server-set! server (quote ,option) ,option)))

(define* (make-server #:key bindaddr bindport hostkey dsakey rsakey banner
                      log-verbosity blocking-mode)
  "Make a new SSH server with the specified configuration.\n
Return a new SSH server."
  (let ((server (%make-server)))
    (server-set-if-specified! bindaddr)
    (server-set-if-specified! bindport)
    (server-set-if-specified! hostkey)
    (server-set-if-specified! dsakey)
    (server-set-if-specified! rsakey)
    (server-set-if-specified! banner)
    (server-set-if-specified! log-verbosity)
    (server-set-if-specified! blocking-mode)
    server))

(define* (server-accept server #:key (callbacks #f))
  (%server-accept server callbacks))

(unless (getenv "GUILE_SSH_CROSS_COMPILING")
  (load-extension "libguile-ssh" "init_server"))

;;; server.scm ends here
