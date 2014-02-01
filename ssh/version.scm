;;; version.scm -- Get information about versions.

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

;; This module provides functions that is used for getting information
;; about current versions.
;;
;; These methods are exported:
;;
;;   get-libssh-version
;;   get-library-version
;;
;; `get-libssh-version' returns libssh version as a string in the
;; follwing format:
;;
;;   <version> ::= <major> "." <minor> "." <micro>
;;
;; For example, "0.5.2".
;;
;; `get-library-version' returns version of the libguile-ssh library
;; as a string.


;;; Code:

(define-module (ssh version)
  #:export (get-libssh-version
            get-library-version))

(load-extension "libguile-ssh" "init_version")

;;; session.scm ends here
