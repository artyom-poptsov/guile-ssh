;;; version.scm -- Get information about versions.

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
;; along with Guile-SSH.  If not, see <http://www.gnu.org/licenses/>.


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
;; `get-library-version' returns version of the Guile-SSH library
;; as a string.


;;; Code:

(define-module (ssh version)
  #:use-module (ssh log)
  #:export (get-libssh-version
            get-library-version
            get-crypto-library
            zlib-support?
            dsa-support?
            ;; Low-level procedures
            %get-libssh-version))

(unless (getenv "GUILE_SSH_CROSS_COMPILING")
  (load-extension "libguile-ssh" "init_version"))

(define (get-libssh-version)
  "Get version of the libssh."
  (car (string-split (%get-libssh-version) #\/)))

(define (get-crypto-library)
  "Get cryptographic library name with which libssh was compiled.  Possible
values are: 'openssl, 'gnutls"
  (string->symbol (cadr (string-split (%get-libssh-version) #\/))))

(define (zlib-support?)
  "Return #t if libssh was compiled wit zlib support, #f otherwise."
  (let ((version (string-split (%get-libssh-version) #\/)))
    (and (not (null? (cddr version)))
         (string=? "zlib" (caddr version)))))

;;; session.scm ends here
