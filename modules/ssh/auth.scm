;;; auth.scm -- API for SSH user authentication.

;; Copyright (C) 2013-2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains API that is used for SSH user authentication.
;;
;; These methods are exported:
;;
;;   userauth-public-key!
;;   userauth-public-key/auto!
;;   userauth-public-key/try
;;   userauth-agent!
;;   userauth-password!
;;   userauth-gssapi!
;;   userauth-none!
;;   userauth-get-list


;;; Code:

(define-module (ssh auth)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ssh log)
  #:use-module (ssh session)
  #:export (userauth-public-key!
            userauth-public-key/auto!
            userauth-public-key/try
            userauth-agent!
            userauth-password!
            userauth-gssapi!
            userauth-none!
            userauth-get-list))

(unless (getenv "GUILE_SSH_CROSS_COMPILING")
  (load-extension "libguile-ssh" "init_auth_func"))

;;; auth.scm ends here.
