;;; popen.scm -- Remote popen emulation.

;; Copyright (C) 2015-2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
;;
;; Variables exported:
;;
;;   OPEN_PTY
;;
;; See the Info documentation for the detailed description of these
;; procedures.


;;; Code:

(define-module (ssh popen)
  #:use-module (ssh channel)
  #:export (open-remote-pipe
            open-remote-pipe*
            open-remote-input-pipe
            open-remote-input-pipe*
            open-remote-output-pipe
            open-remote-output-pipe*
            OPEN_PTY))

(define OPEN_PTY "t")


;; This procedure is taken from GNU Guile 3.0.0.
;;
;; Original comment:
;;
;; string-replace-substring By A. Wingo in
;; https://lists.gnu.org/archive/html/guile-devel/2014-03/msg00058.html
;; also in string-replace-substring guix:guix/utils.scm.

(define (string-replace-substring str substring replacement)
  "Return a new string where every instance of @var{substring} in string
   @var{str} has been replaced by @var{replacement}. For example:

   @lisp
   (string-replace-substring \"a ring of strings\" \"ring\" \"rut\")
   @result{} \"a rut of struts\"
   @end lisp
   "
  (let ((sublen (string-length substring)))
    (with-output-to-string
      (lambda ()
        (let lp ((start 0))
          (cond
           ((string-contains str substring start)
            => (lambda (end)
                 (display (substring/shared str start end))
                 (display replacement)
                 (lp (+ end sublen))))
           (else
            (display (substring/shared str start)))))))))



(define (shell-quote s)
  "Quote string S for sh-compatible shells."
  (string-append "'" (string-replace-substring s "'" "'\\''") "'"))

(define (open-remote-pipe session command mode)
  "Execute a COMMAND on the remote host using a SESSION with a pipe to it.
Returns newly created channel port with the specified MODE."
  (let ((channel (make-channel session mode)))
    (unless channel
      (throw 'guile-ssh-error "Could not create a channel" session command mode))
    (channel-open-session channel)
    (when (string-contains mode OPEN_PTY)
      (channel-request-pty channel))
    (channel-request-exec channel command)
    channel))

(define (open-remote-pipe* session mode prog . args)
  "Execute a PROG with optional ARGS on the remote host using a SESSION with a
pipe to it.  Returns newly created channel port with the specified MODE."
  (open-remote-pipe session
                    (string-join (cons (shell-quote prog)
                                       (map shell-quote args)))
                    mode))


(define (open-remote-input-pipe session command)
  "Execute a COMMAND on the remote host using a SESSION with an input pipe to it.
Returns newly created input channel port."
  (open-remote-pipe session command OPEN_READ))

(define (open-remote-input-pipe* session prog . args)
  "Execute a PROG with optional ARGS on the remote host using a SESSION with
an input pipe to it.  Returns newly created input channel port."
  (open-remote-pipe session
                    (string-join (cons (shell-quote prog)
                                       (map shell-quote args)))
                    OPEN_READ))


(define (open-remote-output-pipe session command)
  "Execute a COMMAND on the remote host using a SESSION with an input pipe to it.
Returns newly created input channel port."
  (open-remote-pipe session command OPEN_WRITE))

(define (open-remote-output-pipe* session prog . args)
  "Execute a PROG with optional ARGS on the remote host using a SESSION with
an output pipe to it.  Returns newly created output channel port."
  (open-remote-pipe session
                    (string-join (cons (shell-quote prog)
                                       (map shell-quote args)))
                    OPEN_WRITE))

;;; popen.scm ends here.
