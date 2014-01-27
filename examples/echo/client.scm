#!/usr/bin/guile \
--debug -e main
!#

;;; client.scm -- Echo client example.

;; Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; Echo client example.
;;
;; Usage: client.scm [ options ] <host> <string>
;;
;; Options:
;;   --user=<user>, -u <user>                User name
;;   --port=<port-number>, -p <port-number>  Port number
;;   --identity-file=<file>, -i <file>       Path to private key
;;
;; Examples:
;;   $ ./client.scm -i ~/.ssh/id_rsa -p 12345 127.0.0.1 "`date`"


;;; Code:

(use-modules (ice-9 getopt-long)
             (ice-9 rdelim)
             (ssh channel)
             (ssh session)
             (ssh auth)
             (ssh key))

(define *program-name* "client.scm")
(define *default-identity-file* (format #f "~a/.ssh/id_rsa" (getenv "HOME")))
(define *default-user* (getenv "USER"))
(define *default-port* "22")


;; Command line options
(define *option-spec*
  '((user          (single-char #\u) (value #t))
    (port          (single-char #\p) (value #t))
    (identity-file (single-char #\i) (value #t))
    (help          (single-char #\h) (value #f))))


(define (print-help)
  "Print information about program usage."
  (display (string-append "\
" *program-name* " -- Echo client example.

Copyright (C) Artyom V. Poptsov <poptsov.artyom@gmail.com>
Licensed under GNU GPLv3+

Usage: " *program-name* " [ options ] <host> <string>

Options:
  --user=<user>, -u <user>                User name
  --port=<port-number>, -p <port-number>  Port number
  --identity-file=<file>, -i <file>       Path to private key
")))

(define (handle-error session)
  "Handle a SSH error."
  (display (get-error session))
  (newline)
  (exit 1))

(define (get-prvkey session identity-file)
  "Get a private SSH key.  Handle possible errors."
  (let ((prvkey (private-key-from-file session identity-file)))
    (if (not prvkey)
        (handle-error session))
    prvkey))

(define (get-pubkey session prvkey)
  "Get a public SSH key from private key PRVKEY.  Handle possible
errors."
  (let ((pubkey (private-key->public-key prvkey)))
    (if (not pubkey)
        (handle-error session))
    pubkey))

(define (read-all port)
  "Read all lines from the PORT."
  (let r ((res (read-line port 'concat))
          (str ""))
    (if (not (eof-object? str))
        (r (string-append res str) (read-line port 'concat))
        res)))


(define (main args)
  "Entry point of the program."
  (if (null? (cdr args))
      (begin
        (print-help)
        (exit 0)))

  (let* ((options       (getopt-long args *option-spec*))
         (user          (option-ref options 'user *default-user*))
         (port          (option-ref options 'port *default-port*))
         (identity-file (option-ref options 'identity-file
                                    *default-identity-file*))
         (help-needed?  (option-ref options 'help #f))
         (args          (option-ref options '() #f)))

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
                                  #:port (string->number port)
                                  #:log-verbosity 'nolog))) ;Be quiet

      (connect! session)

      (case (authenticate-server session)
        ((not-known)
         (display "The server is unknown.  Please check MD5 sum:\n")
         (format #t "  ~a~%" (get-public-key-hash session))))

      (let* ((private-key (get-prvkey session identity-file))
             (public-key  (get-pubkey session private-key)))

        (if (eqv? (userauth-pubkey! session #f public-key private-key) 'error)
            (handle-error session))

        (let ((channel (make-channel session)))

          (if (not channel)
              (handle-error session))

          (channel-open-session channel)

          (write-line str channel)

          (let poll ((ready? #f))
            (if ready?
                (format #t "Response from server: ~a~%" (read-all channel))
                (poll (char-ready? channel))))

          (close channel))))))

;;; echo.scm ends here.
