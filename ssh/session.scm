;;; session.scm -- SSH session management.

;; Copyright (C) 2013, 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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


;;; Commentary:

;; This module contains API that is used for SSH session management.
;;
;; These methods are exported:
;;
;;   session?
;;   %make-session
;;   make-session
;;   blocking-flush!
;;   session-set!
;;   session-get
;;   get-protocol-version
;;   connect!
;;   disconnect!
;;   connected?
;;   authenticate-server
;;   get-public-key-hash
;;   write-known-host!
;;   get-error


;;; Code:

(define-module (ssh session)
  #:use-module (ice-9 optargs)
  #:export (session
            session?
            %make-session
            make-session
	    blocking-flush!
            session-set!
            session-get
            get-protocol-version
            connect!
            disconnect!
            connected?
            authenticate-server
            get-server-public-key
            write-known-host!
            get-error))

;; Set a SSH option if it is specified by the user
(define-macro (session-set-if-specified! option)
  `(if ,option (session-set! session (quote ,option) ,option)))

;; This procedure is more convenient than primitive `%make-session',
;; but on other hand it should be a bit slower because of additional
;; checks.  I think we can put up with this. -avp
(define* (make-session #:key host port user ssh-dir identity add-identity
                       knownhosts timeout timeout-usec ssh1 ssh2 log-verbosity
                       ciphers-c-s ciphers-s-c compression-c-s compression-s-c
                       proxycommand stricthostkeycheck compression
                       compression-level)
  "Make a new SSH session with specified configuration.\n
Return a new SSH session."
  (let ((session (%make-session)))
    (session-set-if-specified! host)
    (session-set-if-specified! port)
    (session-set-if-specified! user)
    (session-set-if-specified! ssh-dir)
    (session-set-if-specified! identity)
    (session-set-if-specified! add-identity)
    (session-set-if-specified! knownhosts)
    (session-set-if-specified! timeout)
    (session-set-if-specified! timeout-usec)
    (session-set-if-specified! ssh1)
    (session-set-if-specified! ssh2)
    (session-set-if-specified! log-verbosity)
    (session-set-if-specified! ciphers-c-s)
    (session-set-if-specified! ciphers-s-c)
    (session-set-if-specified! compression-c-s)
    (session-set-if-specified! compression-s-c)
    (session-set-if-specified! proxycommand)
    (session-set-if-specified! stricthostkeycheck)
    (session-set-if-specified! compression)
    (session-set-if-specified! compression-level)
    session))

(load-extension "libguile-ssh" "init_session")

;;; session.scm ends here
