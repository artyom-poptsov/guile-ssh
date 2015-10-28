;;; sftp.scm -- Procedures for working with SFTP.

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

;;; Code:

(define-module (ssh sftp)
  #:export (sftp-session?
            make-sftp-session
            sftp-init
            sftp-get-session
            sftp-get-error
            sftp-mkdir
            sftp-rmdir
            sftp-mv
            sftp-symlink
            sftp-readlink
            sftp-chmod
            sftp-unlink

            ;; Low-level SFTP session procedures
            %make-sftp-session
            %sftp-init

            ;; File ports
            sftp-open-file
            sftp-file?))


;;; Low-level SFTP session procedures.

(define (%make-sftp-session session)
  "Make a new SFTP session using a SSH SESSION without initialization of the
session with a server.  Throw 'guile-ssh-error' exception on an error."
  (%gssh-make-sftp-session session))

(define (%sftp-init sftp-session)
  "Initialize a SFTP-SESSION with the server.  Throw 'guile-ssh-error'
exception on an error, return value is undefined."
  (%gssh-sftp-init sftp-session))


;;; Main SFTP session API.

(define (make-sftp-session session)
  "Make a new SFTP session using a SSH SESSION, initialize the session with a
server.  Return initialized SFTP session or throw 'guile-ssh-error' exception
on an error"
  (let ((sftp-session (%gssh-make-sftp-session session)))
    (%gssh-sftp-init sftp-session)
    sftp-session))

(define (sftp-session? x)
  "Return #t if X is a SFTP session, #f otherwise."
  (%gssh-sftp-session? x))


(define (sftp-get-session sftp-session)
  "Get the parent SSH session for a SFTP-SESSION."
  (%gssh-sftp-get-session sftp-session))

(define (sftp-get-error sftp-session)
  "Get the last SFTP error from a SFTP-SESSION.  Return the error name as a symbol,
or throw 'guile-ssh-error' on if an error occured in the procedure itself."
  (%gssh-sftp-get-error sftp-session))


(define* (sftp-mkdir sftp-session dirname #:optional (mode #o777))
  "Create a directory DIRNAME using a SFTP-SESSION with a MODE.  If the MODE
is omitted, the current umask value is used."
  (%gssh-sftp-mkdir sftp-session dirname mode))

(define (sftp-rmdir sftp-session dirname)
  "Remove a directory DIRNAME.  Throw 'guile-ssh-error' on an error.  Return
value is undefined."
  (%gssh-sftp-rmdir sftp-session dirname))

(define (sftp-mv sftp-session source dest)
  "Move or rename a file SOURCE into a DEST.  Throw 'guile-ssh-error' on an
error.  Return value is undefined."
  (%gssh-sftp-mv sftp-session source dest))


(define (sftp-symlink sftp-session target dest)
  "Create a symbolic link to a TARGET in a DEST.  Throw 'guile-ssh-error' on an
error.  Return value is undefined."
  (%gssh-sftp-symlink sftp-session target dest))

(define (sftp-readlink sftp-session path)
  "Read the value of a symbolic link pointed by a PATH.  Return the value or
'#f' on an error."
  (%gssh-sftp-readlink sftp-session path))


(define* (sftp-chmod sftp-session filename #:optional (mode #o777))
  "Change permissions of a FILENAME.  Permissions are set to 'mode & ~umask'.
If MODE is not set then #o777 is used.  Throw 'guile-ssh-error' on an error.
Return value is undefined."
  (%gssh-sftp-chmod sftp-session filename mode))

(define (sftp-unlink sftp-session filename)
  "Unlink (delete) a FILENAME.  Throw 'guile-ssh-error' on an error.  Return
value is undefined."
  (%gssh-sftp-unlink sftp-session filename))


;;; SFTP file API.

(define* (sftp-open-file sftp-session filename flags #:optional (mode #o666))
  "Open a FILENAME, return an open file port.  Throw 'guile-ssh-error' on an
error."
  (%gssh-sftp-open-file sftp-session filename flags mode))

(define (sftp-file? x)
  "Return #t if X is an SFTP file port, #f otherwise."
  (%gssh-sftp-file? x))


;;; Load libraries.

(load-extension "libguile-ssh" "init_sftp_session")
(load-extension "libguile-ssh" "init_sftp_file")

;;; sftp-session.scm ends here.

