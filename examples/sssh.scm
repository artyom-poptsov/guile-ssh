#!/usr/bin/guile \
--debug -e main
!#

;;; sssh.scm -- Scheme Secure Shell.

;; Copyright (C) 2013, 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This program is aimed to demonstrate some features of Guile-SSH
;; library.  See https://github.com/artyom-poptsov/libguile-ssh


;;; Code:

(use-modules (ice-9 getopt-long)
             (ice-9 rdelim)
             (ssh channel)
             (ssh session)
             (ssh auth)
             (ssh key)
             (ssh version))


;;; Variables and constants

(define *program-name* "sssh")
(define *default-identity-file*
  (string-append (getenv "HOME") "/.ssh/id_rsa"))

(define debug? #f)


;; Command line options
(define *option-spec*
  '((user          (single-char #\u) (value #t))
    (port          (single-char #\p) (value #t))
    (identity-file (single-char #\i) (value #t))
    (help          (single-char #\h) (value #f))
    (version       (single-char #\v) (value #f))
    (debug         (single-char #\d) (value #f))
    (ssh-debug                       (value #f))))


;;; Helper procedures

(define (handle-error session)
  "Handle a SSH error."
  (display (get-error session))
  (newline)
  (exit 1))

(define (print-debug msg)
  "Print debug information"
  (if debug?
      (display msg)))

(define (format-debug fmt . args)
  "Format a debug message."
  (if debug?
      (format #t fmt args)))

(define (read-all port)
  "Read all lines from the PORT."
  (let r ((res "")
          (str (read-line port 'concat)))
    (if (not (eof-object? str))
        (r (string-append res str) (read-line port 'concat))
        res)))

;;; Printing of various information


(define (print-version-and-exit)
  "Print information about versions."
  (format #t "libssh version:       ~a~%" (get-libssh-version))
  (format #t "libguile-ssh version: ~a~%" (get-library-version))
  (exit))


(define (print-help-and-exit)
  "Print information about program usage."
  (display
   (string-append
    *program-name* " -- Scheme Secure Shell
Copyright (C) Artyom Poptsov <poptsov.artyom@gmail.com>
Licensed under GNU GPLv3+

Usage: " *program-name* " [ -upidv ] <host> <command>

Options:
  --user=<user>, -u <user>                User name
  --port=<port-number>, -p <port-number>  Port number
  --identity-file=<file>, -i <file>       Path to private key
  --debug, -d                             Debug mode
  --ssh-debug                             Debug libssh
  --version, -v                           Print version
"))
  (exit))


;;; Entry point of the program

(define (main args)

  (if (null? (cdr args))
      (print-help-and-exit))

  (let* ((options           (getopt-long args *option-spec*))
         (user              (option-ref options 'user (getenv "USER")))
         (port              (string->number (option-ref options 'port "22")))
         (identity-file     (option-ref options 'identity-file
                                        *default-identity-file*))
         (debug-needed?     (option-ref options 'debug #f))
         (ssh-debug-needed? (option-ref options 'ssh-debug #f))
         (help-needed?      (option-ref options 'help #f))
         (version-needed?   (option-ref options 'version #f))
         (args              (option-ref options '() #f)))

    (set! debug? debug-needed?)

    (if help-needed?
        (print-help-and-exit))

    (if version-needed?
        (print-version-and-exit))

    (if (or (null? args) (null? (cdr args)))
        (print-help-and-exit))

    (let ((host (car args))
          (cmd  (cadr args)))

      (print-debug "1. make-session (ssh_new)\n")
      (let ((session (make-session #:user user
                                   #:host host
                                   #:port port
                                   #:identity identity-file
                                   #:log-verbosity (if ssh-debug-needed?
                                                       'functions
                                                       'nolog))))

        (print-debug "3. connect! (ssh_connect_x)\n")
        (connect! session)

        (format-debug "   Available authentication methods: ~a~%"
                      (userauth-get-list session))

        (print-debug "4. authenticate-server (ssh_is_server_known)\n")
        (case (authenticate-server session)
          ((ok)        (print-debug "   ok\n"))
          ((not-known) (display "   The server is unknown.  Please check MD5.\n")))

        (format-debug "   MD5 hash: ~a~%" (get-public-key-hash session))

        (print-debug "5. userauth-autopubkey!\n")
        (let ((res (userauth-autopubkey! session)))
          (if (eqv? res 'error)
              (handle-error session)))

        (print-debug "6. make-channel (ssh_channel_new)\n")
        (let ((channel (make-channel session)))

          (format-debug "   channel: ~a~%" channel)

          (if (not channel)
              (handle-error session))

          (print-debug "7. channel-open-session (ssh_channel_open_session)\n")
          (catch #t
            (lambda () (channel-open-session channel))
            (lambda (key . args)
              (display args)
              (newline)))

          (format-debug "   channel: ~a~%" channel)

          (print-debug "8. channel-request-exec (ssh_channel_request_exec)\n")
          (channel-request-exec channel cmd)

          (print-debug "9. channel-poll (ssh_channel_poll)\n")
          (let poll ((ready? #f))
            (if ready?
                (begin
                  (print-debug "10. channel-read (ssh_channel_read)\n")
                  (display (read-all channel))
                  (newline))
                (poll (char-ready? channel))))
          (close channel)
          (disconnect! session))))))

;;; sssh.scm ends here
