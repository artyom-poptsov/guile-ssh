;;; channel.scm -- API for SSH channel manipulation.

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
;; along with Guile-SSH.  If not, see
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This module contains API that is used for working with SSH
;; channels.
;;
;; These procedures are exported:
;;
;;   channel?
;;   make-channel
;;   channel-open-session
;;   channel-request-env
;;   channel-request-exec
;;   channel-request-pty
;;   channel-request-shell
;;   channel-set-pty-size!
;;   channel-set-stream!
;;   channel-get-stream
;;   channel-open?
;;   channel-eof?


;;; Code:

(define-module (ssh channel)
  #:use-module (ssh log)
  #:use-module (ssh session)
  #:export (channel
            channel?
            make-channel
            channel-open-session
            channel-request-env
            channel-request-exec
            channel-request-pty
            channel-request-shell
            channel-set-pty-size!
            channel-set-stream!
            channel-get-stream
            channel-get-session
            channel-open?
            channel-eof?))

(load-extension "libguile-ssh" "init_channel")

;;; channel.scm ends here.
