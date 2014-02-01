;;; auth.scm -- API for SSH user authentication.

;; Copyright (C) 2013, 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains API that is used for SSH user authentication.
;; 
;; These methods are exported:
;;
;;   userauth-pubkey!
;;   userauth-autopubkey!
;;   userauth-password!
;;   userauth-none!


;;; Code:

(define-module (ssh auth)
  #:use-module (ssh session)
  #:export (userauth-pubkey!
            userauth-autopubkey!
            userauth-password!
            userauth-none!
            userauth-get-list))

(load-extension "libguile-ssh" "init_auth_func")

;;; auth.scm ends here.
