;;; key.scm -- SSH keys management.

;; Copyright (C) 2013-2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;; Copyright (C) 2026 Nicolas Graves <ngraves@ngraves.fr>
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

;; This module contains API that is used for SSH key management.
;;
;; These methods are exported:
;;
;;   key?
;;   public-key?
;;   private-key?
;;   make-keypair
;;   get-key-type
;;   public-key->string
;;   string->pubilc-key
;;   public-key-from-file
;;   private-key->public-key
;;   private-key-from-file
;;   private-key-to-file
;;   get-public-key-hash
;;   get-public-key-fingerprint
;;   bytevector->hex-string
;;   sign
;;   verify


;;; Code:

(define-module (ssh key)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (ssh log)
  #:export (key
            key?
            public-key?
            private-key?
            make-keypair
            get-key-type
            public-key->string
            string->public-key
            public-key-from-file
            private-key->public-key
            private-key-from-file
            private-key-to-file
            get-public-key-fingerprint
            get-public-key-hash
            bytevector->hex-string
            sign
            verify))

(define (bytevector->hex-string bv)
  "Convert bytevector BV to a colon separated hex string."
  (string-join (map (lambda (e) (format #f "~2,'0x" e))
                    (bytevector->u8-list bv))
               ":"))

(define* (private-key-from-file path
                                #:key
                                (auth-callback #f)
                                (user-data #f))
  (%private-key-from-file path auth-callback user-data))

(define* (get-public-key-fingerprint key #:optional (hash 'sha256))
  (%gssh-get-public-key-fingerprint key hash))

(define* (sign data key
               #:key
               (namespace "file")
               (hash 'sha512))
  "Sign DATA using a private KEY with specified NAMESPACE and HASH.

DATA can be a binary bytevector or a string.
HASH should be 'sha256 or 'sha512.
Return the armored signature string on success, #f on error."
  (match data
    ((? string?)
     (%gssh-sign (string->utf8 data) key namespace hash))
    ((? bytevector?)
     (%gssh-sign data key namespace hash))
    (_
     (format (current-error-port) "sign: DATA must be a string or bytevector")
     #f)))

(define* (verify data signature
                 #:key (namespace "file"))
  "Verify a SIGNATURE for DATA with NAMESPACE.

DATA can be a binary bytevector or a string.
Return the signing key on success, #f on error."
  (match data
    ((? string?)
     (%gssh-verify (string->utf8 data) signature namespace))
    ((? bytevector?)
     (%gssh-sign data key namespace hash))
    (_
     (format (current-error-port) "verify: DATA must be a string or bytevector")
     #f)))

(unless (getenv "GUILE_SSH_CROSS_COMPILING")
  (load-extension "libguile-ssh" "init_key"))

;;; key.scm ends here.
