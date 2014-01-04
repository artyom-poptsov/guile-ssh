#!/usr/bin/guile \
--debug -e main
!#

;;; client.scm -- Echo client example.

;; Copyright (C) 2013-2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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


;;; Code:

(use-modules (ice-9 getopt-long)
             (ice-9 rdelim)
             (ssh channel)
             (ssh session)
             (ssh auth)
             (ssh key))

(define *program-name* "echo-client")
(define *default-identity-file*
  (string-append (getenv "HOME") "/.ssh/id_rsa"))


;; Command line options
(define *option-spec*
  '((user          (single-char #\u) (value #t))
    (port          (single-char #\p) (value #t))
    (identity-file (single-char #\i) (value #t))
    (help          (single-char #\h) (value #f))))


(define (print-help)
  "Print information about program usage."
  (display
   (string-append
    *program-name* " -- Echo client example.\n"
    "Copyright (C) Artyom Poptsov <poptsov.artyom@gmail.com>\n"
    "Licensed under GNU GPLv3+\n"
    "\n"
    "Usage: " *program-name* " [ -upidv ] <host> <string>\n"
    "\n"
    "Options:\n"
    "  --user=<user>, -u <user>                User name\n"
    "  --port=<port-number>, -p <port-number>  Port number\n"
    "  --identity-file=<file>, -i <file>       Path to private key\n")))


(define (handle-error session)
  "Handle a SSH error."
  (display (get-error session))
  (newline)
  (exit 1))

(define (get-prvkey session identity-file)
  (let ((prvkey (private-key-from-file session identity-file)))
    (if (not prvkey)
        (handle-error session))
    prvkey))

(define (get-pubkey session prvkey)
  (let ((pubkey (private-key->public-key prvkey)))
    (if (not pubkey)
        (handle-error session))
    pubkey))


(define (main args)
  "Entry point of the program."
  (if (null? (cdr args))
      (begin
        (print-help)
        (exit 0)))

  (let* ((options           (getopt-long args *option-spec*))
         (user              (option-ref options 'user (getenv "USER")))
         (port              (string->number (option-ref options 'port "22")))
         (identity-file     (option-ref options 'identity-file
                                        *default-identity-file*))
         (help-needed?      (option-ref options 'help #f))
         (args              (option-ref options '() #f)))



    (if help-needed?
        (begin
          (print-help)
          (exit 0)))

    (if (or (null? args) (null? (cdr args)))
        (begin
          (print-help)
          (exit 0)))

    (let* ((host (car args))
           (str  (cadr args))
           (session (make-session #:user user
                                  #:host host
                                  #:port port
                                  #:log-verbosity 0))) ;Be quiet

      (connect! session)
      (case (authenticate-server session)
        ((not-known) (display "   The server is unknown.  Please check MD5.\n")))

      (let* ((private-key (get-prvkey session identity-file))
             (public-key  (get-pubkey session private-key)))

        (if (eqv? (userauth-pubkey! session #f public-key private-key) 'error)
            (handle-error session))

        (let ((channel     (make-channel session)))

          (if (not channel)
              (handle-error session))

          (channel-open-session channel)

          (display str channel)

          (let poll ((count #f))
            (if (or (not count) (zero? count))
                (poll (channel-poll channel #f))
                (begin
                  (display (read-line channel))
                  (newline))))

          (close channel)
          (display channel))))))

;;; echo.scm ends here.
