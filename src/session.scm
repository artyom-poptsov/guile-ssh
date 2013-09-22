;;; session.scm -- SSH session management.

;; Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This file is a part of libguile-ssh.
;;
;; libguile-ssh is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; libguile-ssh is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with libguile-ssh.  If not, see
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This module contains API that is used for SSH session management.
;;
;; These methods are exported:
;;
;;   make-session
;;   blocking-flush!
;;   session-set!
;;   get-version
;;   connect!
;;   disconnect!
;;   connected?
;;   authenticate-server
;;   get-public-key-hash
;;   write-known-host))
;;   get-error


;;; Code:

(define-module (ssh session)
  #:export (session
	    make-session
	    blocking-flush!
	    session-set!
	    get-protocol-version
	    connect!
	    disconnect!
	    connected?
	    authenticate-server
	    get-public-key-hash
	    write-known-host!
            get-error))

(load-extension "libguile-ssh" "init_session")

;;; session.scm ends here
