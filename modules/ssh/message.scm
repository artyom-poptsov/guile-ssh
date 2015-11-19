;;; message.scm -- Procedures for working with SSH messages.

;; Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains message parsing utilites for Guile-SSH
;; servers.
;;
;; Messages can be fetched from the client by calling
;; `server-message-get' procedure.  The server can get content of the
;; requests by calling `message-get-req' procedure with a message
;; passed as an arguement.
;;
;; `message-get-req' returns the content of a request as a vector
;; which can be parsed by related procedures such as `auth:req:user'
;; and friends.

;;; Code:

(define-module (ssh message)
  #:use-module (ssh log)
  #:use-module (ssh key)
  #:export (message
            message?
            message-reply-default
            message-reply-success
            message-get-type
            message-get-req
            message-get-session

            message-service-reply-success
            service-req:service

            channel-open-req:orig channel-open-req:orig-port
            channel-open-req:dest channel-open-req:dest-port

            message-auth-reply-success
            message-auth-reply-public-key-ok
            message-auth-set-methods!
            auth-req:user auth-req:password auth-req:pubkey
            auth-req:pubkey-state

            message-channel-request-reply-success
            message-channel-request-open-reply-accept

            message-global-request-reply-success

            pty-req:term pty-req:width pty-req:height pty-req:pxwidth
            pty-req:pxheight

            exec-req:cmd

            env-req:name env-req:value

            global-req:addr global-req:port))


(define (service-req:service req) (vector-ref req 0))

(define (channel-open-req:orig      req) (vector-ref req 0))
(define (channel-open-req:orig-port req) (vector-ref req 1))
(define (channel-open-req:dest      req) (vector-ref req 2))
(define (channel-open-req:dest-port req) (vector-ref req 3))

(define (auth-req:user         req) (vector-ref req 0))
(define (auth-req:password     req) (vector-ref req 1))
(define (auth-req:pubkey       req) (vector-ref req 2))
(define (auth-req:pubkey-state req) (vector-ref req 3))

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


(define (message-reply-success msg . args)
  "Reply 'success' to the message MSG.  This procedure is a convenient
wrapper for other '*-reply-success' procedures.  The right procedure
to use will be selected depending on a type of the message MSG."
  (let ((msg-type (message-get-type msg)))
    (case (car msg-type)

      ((request-auth)
       (cond
        ((= (length args) 0)
         (message-auth-reply-success msg #f))
        ((= (length args) 1)
         (if (and (symbol? (car args)) (eq? (car args) 'partial))
             (message-auth-reply-success msg #t)
             (error
              (string-append "message-reply-success: "
                             "Wrong argument.  Expected: 'partial")
              (car args))))
        (else
         (error "message-reply-success: Wrong number of arguments."
                args))))

      ((request-service)
       (message-service-reply-success msg))

      ((request-channel-open)
       (message-channel-request-reply-success msg))

      ((request-channel)
       (message-channel-request-reply-success msg))

      ((request-global)
       (message-global-request-reply-success msg (car args))))))


(load-extension "libguile-ssh" "init_message")

;;; message.scm ends here
