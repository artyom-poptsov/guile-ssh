;;; message.scm -- Procedures for working with SSH messages.

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


;;; Code:

(define-module (ssh message)
  #:export (message
            message-reply-default
            message-get-type
            message-auth-reply-success
            message-auth-reply-public-key-success
            message-auth-get-user
            message-auth-get-password
            message-auth-set-methods!
            message-channel-request-open-reply-accept
            message-channel-request-reply-success))

(load-extension "libguile-ssh" "init_message")

;;; message.scm ends here
