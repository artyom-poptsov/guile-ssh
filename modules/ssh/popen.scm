;;; popen.scm -- Remote popen emulation.

;; Copyright (C) 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module provides implementation of "remote popen".  That is, you may
;; create either input, output or bidirectional pipes to remote process with
;; the procedures exported by the module.
;;
;; These procedures are exported:
;;
;;   open-remote-pipe
;;   open-remote-pipe*
;;   open-remote-input-pipe
;;   open-remote-input-pipe*
;;   open-remote-output-pipe
;;   open-remote-output-pipe*

;;; Code:

(define-module (ssh popen)
  #:use-module (ssh channel)
  #:export (open-remote-pipe
            open-remote-pipe*
            open-remote-input-pipe
            open-remote-input-pipe*
            open-remote-output-pipe
            open-remote-output-pipe*))

(define (open-remote-pipe session command mode)
  "Execute a COMMAND on the remote host using a SESSION with a pipe to it.
Returns newly created channel port with the specified MODE."
  (let ((channel (make-channel session mode)))
    (unless channel
      (throw 'guile-ssh-error "Could not create a channel" session command mode))
    (channel-open-session channel)
    (channel-request-exec channel command)
    channel))

(define (open-remote-pipe* session mode prog . args)
  "Execute a PROG with optional ARGS on the remote host using a SESSION with a
pipe to it.  Returns newly created channel port with the specified MODE."
  (open-remote-pipe session (string-join (cons prog args)) mode))


(define (open-remote-input-pipe session command)
  "Execute a COMMAND on the remote host using a SESSION with an input pipe to it.
Returns newly created input channel port."
  (open-remote-pipe session command OPEN_READ))

(define (open-remote-input-pipe* session prog . args)
  "Execute a PROG with optional ARGS on the remote host using a SESSION with
an input pipe to it.  Returns newly created input channel port."
  (open-remote-pipe session (string-join (cons prog args)) OPEN_READ))


(define (open-remote-output-pipe session command)
  "Execute a COMMAND on the remote host using a SESSION with an input pipe to it.
Returns newly created input channel port."
  (open-remote-pipe session command OPEN_WRITE))

(define (open-remote-output-pipe* session prog . args)
  "Execute a PROG with optional ARGS on the remote host using a SESSION with
an output pipe to it.  Returns newly created output channel port."
  (open-remote-pipe session (string-join (cons prog args)) OPEN_WRITE))

;;; popen.scm ends here.
