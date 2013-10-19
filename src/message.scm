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
  #:use-module (ssh key)
  #:export (message
            message-reply-default
            message-get-type
            message-get-req

            message-auth-reply-success
            message-auth-reply-public-key-success
            message-auth-set-methods!
            auth-req:user auth-req:password auth-req:pubkey

            message-channel-request-open-reply-accept
            message-channel-request-reply-success

            pty-req:term pty-req:width pty-req:height pty-req:pxwidth
            pty-req:pxheight

            env-req:name env-req:value

            global-req:addr global-req:port))


(define (auth-req:user req)     (vector-ref req 0))
(define (auth-req:password req) (vector-ref req 1))
(define (auth-req:pubkey req)   (vector-ref req 2))

(define (pty-req:term req)     (vector-ref req 0))
(define (pty-req:width req)    (vector-ref req 1))
(define (pty-req:height req)   (vector-ref req 2))
(define (pty-req:pxwidth req)  (vector-ref req 3))
(define (pty-req:pxheight req) (vector-ref req 4))

(define (env-req:name req)  (vector-ref req 0))
(define (env-req:value req) (vector-ref req 1))

(define (exec-req:cmd req) (vector-ref req 0))

(define (global-req:addr req) (vector-ref req 0))
(define (global-req:port req) (vector-ref req 1))


(load-extension "libguile-ssh" "init_message")

;;; message.scm ends here
