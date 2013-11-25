#!/usr/bin/guile \
--debug -e main -s
!#

;;; ssshd.scm -- Scheme Secure Shell Daemon.

;; Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Scheme Secure Shell Daemon (SSHD) -- an ssh daemon written in GNU
;; Guile that uses Guile-SSH API.


;;; Code:

(use-modules (ice-9 rdelim)
             (ice-9 popen)
             (ssh server)
             (ssh message)
             (ssh session)
             (ssh channel)
             (ssh key)
             (ssh auth))                ; userauth-*


;;; Variables and constants

(define *default-bindport*      12345)
(define *default-log-verbosity* 0)
(define *default-rsakey*        (string-append (getenv "HOME")
                                               "/.ssh/id_rsa"))

(define debug? #t)


;;; Helper procedures


(define (format-debug . args)
  (if debug?
      (format args)))

(define (poll channel)
  "Poll a channel CHANNEL for data.  Return available data size."
  (let f ((count #f))
    (if (or (not count) (zero? count))
        (f (channel-poll channel #f))
        count)))

(define (read-all port)
  "Read all lines from the PORT."
  (let r ((res "")
          (str (read-line port 'concat)))
    (if (not (eof-object? str))
        (r (string-append res str) (read-line port 'concat))
        res)))


;;; Handlers

(define (handle-channel channel)
  (let* ((count (poll channel))
         (data  (channel-read channel count #f)))
    (format #t "  data: ~a~%" data)
    (channel-read channel 10 #f)))

(define (handle-request-exec msg channel)
  "Handle a non-interactive SSH session"
  (let ((cmd (exec-req:cmd (message-get-req msg))))
    (format #t "  cmd: ~a~%" cmd)
    (let* ((port (open-input-pipe cmd))
           (res  (read-all port)))
      (channel-write channel (string-length res) res))))

(define (handle-req-auth session msg msg-type)
  (let ((subtype (cadr msg-type)))

    (format #t "  subtype: ~a~%" subtype)

    ;; Allowed authentication methods
    (message-auth-set-methods! msg '(public-key password))

    (case subtype
      ((auth-method-publickey)
       (let* ((req          (message-get-req msg))
              (user         (auth-req:user req))
              (pubkey       (auth-req:pubkey req))
              (pubkey-state (auth-req:pubkey-state req)))
         (format #t
                 (string-append "  User ~a wants to authenticate with a public key (~a)~%"
                                "  Public key state: ~a~%")
                 user (get-key-type pubkey) pubkey-state)

         (case pubkey-state
           ((none)
            (message-auth-reply-public-key-ok msg))

           ((valid)
            (message-reply-success msg))

           (else
            (format #t "  Bad public key state: ~a~%" pubkey-state)
            (message-reply-default msg)))))

      ((auth-method-password)
       (let* ((req (message-get-req msg))
              (user (auth-req:user     req))
              (pswd (auth-req:password req)))

         (newline)
         (display req)
         (newline)

         (format #t "  User ~a wants to authenticate with a password~%" user)

         ;; Check the password for the user.
         (case (userauth-password! session user pswd)
           ((success)
            (message-reply-success msg #f))

           ((partial)
            (message-reply-success msg #t))

           (else
            (message-reply-default msg)))))

      ;; To enable authentication through the "none" method, we have
      ;; to call `message-auth-reply-success' procedure.
      ;;
      ;; The "none" method is disabled according to recommendations of
      ;; RFC4251.  Here we return the list of available authentication
      ;; methods back to the client.
      ((auth-method-none)
       (message-reply-default msg))

      (else
       (message-reply-default msg)))))

(define (handle-req-channel-open msg msg-type)
  (let ((subtype (cadr msg-type)))
    (format #t "  subtype: ~a~%" subtype)
    (case subtype
      ((channel-session)
       (message-channel-request-open-reply-accept msg))
      (else
       (message-reply-default msg)
       #f))))

(define (shell-loop channel)
  (let* ((cnt (channel-poll channel #f))
         (cmd (channel-read channel cnt #f)))
    (format #t "  ~a~%" cmd)
    (let* ((port (open-input-pipe cmd))
           (res  (read-all port)))
      (channel-write channel (string-length res) res)))
  (if (channel-open? channel)
      (shell-loop channel)))

(define (handle-req-channel msg msg-type channel)
  (let ((subtype (cadr msg-type)))

    (format #t "  subtype: ~a~%" subtype)

    (case subtype

      ((channel-request-exec)
       (handle-request-exec msg channel)
       (message-reply-success msg))

      ((channel-request-pty)
       (let ((pty-req (message-get-req msg)))
         (format #t
                 (string-append "  pty requested:~%"
                                "    Term:   ~a~%"
                                "    Width:  ~a~%"
                                "    Height: ~a~%")
                 (pty-req:term pty-req)
                 (pty-req:width pty-req)
                 (pty-req:height pty-req))
         (message-reply-success msg)))

      ((channel-request-env)
       (let* ((env-req (message-get-req msg))
              (name    (env-req:name env-req))
              (value   (env-req:value env-req)))
         (format #t
                 (string-append "  env requested:~%"
                                "    name:  ~a~%"
                                "    value: ~a~%")
                 name value)
         (setenv name value)
         (message-reply-success msg)))

      (else
       (message-reply-success msg)))))


;;; Entry point of the program.

(define (main . args)
  (display "---------- ssshd ----------\n")
  (let ((server  (make-server #:bindport      *default-bindport*
                              #:rsakey        *default-rsakey*
                              #:log-verbosity *default-log-verbosity*
                              #:banner        "Scheme Secure Shell Daemon"))
        (channel #f))

    ;; Start listen to incoming connections.
    (server-listen server)

    ;; Accept new connections from clients.  Every connection is
    ;; handled in its own SSH session.
    (let main-loop ((session (server-accept server)))

      (server-handle-key-exchange session)

      ;; Handle messages from the connected SSH client.
      (let session-loop ((msg (server-message-get session)))
        (display "Message received.\n")

        (if (not msg)
            (error (get-error session)))

        (let ((msg-type (message-get-type msg)))

          (format #t "Message type: ~a~%" msg-type)

          ;; Check the type of the message
          (case (car msg-type)

            ((request-service)
             (let ((srv-req (message-get-req msg)))
               (format #t "  Service requested: ~a~%"
                       (service-req:service srv-req))
               (message-reply-success msg)))

            ((request-auth)
             (handle-req-auth session msg msg-type))

            ((request-channel-open)
             (set! channel (handle-req-channel-open msg msg-type)))

            ((request-channel)
             (handle-req-channel msg msg-type channel)
             ;; FIXME: We currently support only one exec request per
             ;; a session.
             (if (eq? (cadr msg-type) 'channel-request-exec)
                 (disconnect! session)))

            (else
             (display "Send the default reply.\n")
             (message-reply-default msg))))

        (display "Message is handled.\n")
        (if (connected? session)
            (session-loop (server-message-get session))))

      (display "Disconnect the current session.\n")
      (disconnect! session)

      (display "Waiting for the next connection...\n")
      (main-loop (server-accept server)))))

;;; ssshd.scm ends here
