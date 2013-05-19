;;; key.scm -- SSH keys management.

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

;; This module contains API that is used for SSH key management.
;; 
;; These methods are exported:
;;
;;   ssh:key
;;   ssh:key?
;;   ssh:public-key?
;;   ssh:private-key?
;;   ssh:public-key->string
;;   ssh:public-key-from-file
;;   ssh:private-key->public-key
;;   ssh:private-key-from-file


;;; Code:

(define-module (ssh key)
  #:export (ssh:key
            ssh:key?
            ssh:public-key?
            ssh:private-key?
            ssh:public-key->string
            ssh:public-key-from-file
            ssh:private-key->public-key
            ssh:private-key-from-file))

(load-extension "libguile-ssh" "init_key")

;;; key.scm ends here.
