;;; key.scm -- SSH keys management.

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
;;   bytevector->hex-string


;;; Code:

(define-module (ssh key)
  #:use-module (ice-9 format)
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
            get-public-key-hash
            bytevector->hex-string))

(define (bytevector->hex-string bv)
  "Convert bytevector BV to a colon separated hex string."
  (string-join (map (lambda (e) (format #f "~2,'0x" e))
                    (bytevector->u8-list bv))
               ":"))

(load-extension "libguile-ssh" "init_key")

;;; key.scm ends here.
