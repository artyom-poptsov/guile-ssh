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
;;   channel-open-forward
;;   channel-open-reverse-forward
;;   channel-cancel-forward
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
            channel-open-forward
            channel-open-forward/reverse
            channel-cancel-forward
            channel-request-send-exit-status
            channel-set-pty-size!
            channel-set-stream!
            channel-get-stream
            channel-get-session
            channel-get-exit-status
            channel-open?
            channel-eof?))


(define* (channel-open-forward channel
                               #:key (source-host "localhost") local-port
                               remote-host (remote-port local-port))
  "Open a TCP/IP forwarding channel.  Connect to a REMOTE-HOST and REMOTE-PORT,
and use SOURCE-HOST and LOCAL-PORT as origination of connections.

If the SOURCE-HOST is not set, then \"localhost\" is used.  If REMOTE-PORT is
not set, then it will be set to LOCAL-PORT value.

Please note that the procedure does not bind the LOCAL-PORT and does not
automatically forward the content of a socket to the channel."
  (%channel-open-forward channel
                         remote-host remote-port
                         source-host local-port))

(define* (channel-open-forward/reverse channel
                                       #:key (source-host "localhost") local-port
                                       remote-host (remote-port local-port))
  ;; TODO: Write more detailed docstring.
  "Open a TCP/IP reverse forwarding channel."
  (%channel-open-forward/reverse channel
                                 remote-host remote-port
                                 source-host local-port))
                               

(load-extension "libguile-ssh" "init_channel")

;;; channel.scm ends here.
