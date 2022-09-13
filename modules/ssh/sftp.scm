;;; sftp.scm -- Procedures for working with SFTP.

;; Copyright (C) 2015-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains SFTP API procedures.
;;
;; The module exports:
;;   sftp-session?
;;   make-sftp-session
;;   sftp-get-session
;;   sftp-get-error
;;   sftp-mkdir
;;   sftp-rmdir
;;   sftp-mv
;;   sftp-symlink
;;   sftp-readlink
;;   sftp-chmod
;;   sftp-unlink
;;   sftp-open
;;   sftp-file?
;;   call-with-remote-input-file
;;   call-with-remote-output-file
;;   with-input-from-remote-file
;;   with-output-to-remote-file
;;
;; These exported procedures are low-level ones and should not be used without
;; a good reason:
;;   %make-sftp-session
;;   %sftp-init
;;
;; See the Info documentation for the detailed description of these
;; procedures.

;;; Code:

(define-module (ssh sftp)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 streams)
  #:export (sftp-session?
            make-sftp-session
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
            sftp-open
            sftp-file?

            ;; Directories
            sftp-dir-open
            sftp-dir-open-stream
            sftp-dir-close
            sftp-dir-eof?
            sftp-dir-read

            ;; High-level operations on remote files
            call-with-remote-input-file
            call-with-remote-output-file
            with-input-from-remote-file
            with-output-to-remote-file))


;;; Low-level SFTP session procedures.

(define (%make-sftp-session ssh-session)
  "Make a new SFTP session using an SSH-SESSION without initialization of the
session with a server.  Throw 'guile-ssh-error' exception on an error."
  (%gssh-make-sftp-session ssh-session))

(define (%sftp-init sftp-session)
  "Initialize a SFTP-SESSION with the server.  Throw 'guile-ssh-error'
exception on an error, return value is undefined."
  (%gssh-sftp-init sftp-session))


;;; Main SFTP session API.

(define (make-sftp-session ssh-session)
  "Make a new SFTP session using an SSH-SESSION, initialize the session with a
server.  Return initialized SFTP session or throw 'guile-ssh-error' exception
on an error"
  (let ((sftp-session (%gssh-make-sftp-session ssh-session)))
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
or throw 'guile-ssh-error' on if an error occurred in the procedure itself."
  (%gssh-sftp-get-error sftp-session))


(define* (sftp-mkdir sftp-session dirname #:optional (mode #o777))
  "Create a directory DIRNAME using a SFTP-SESSION with permissions specified
by a MODE.  The permissions of the created file are (MODE & ~umask).  If the
MODE is omitted, #o777 is used."
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


(define* (sftp-chmod sftp-session filename mode)
  "Change permissions of a FILENAME.  Permissions are set to 'MODE & ~umask'.
Throw 'guile-ssh-error' on an error.  Return value is undefined."
  (%gssh-sftp-chmod sftp-session filename mode))

(define (sftp-unlink sftp-session filename)
  "Unlink (delete) a FILENAME.  Throw 'guile-ssh-error' on an error.  Return
value is undefined."
  (%gssh-sftp-unlink sftp-session filename))


;;; SFTP file API.

(define* (sftp-open sftp-session filename flags #:optional (mode #o666))
  "Open a FILENAME with permissions specified by MODE, return an open file
port.  Permissions are set to 'MODE & ~umask'; the default MODE is #o666.
Throw 'guile-ssh-error' on an error."
  (%gssh-sftp-open sftp-session filename flags mode))

(define (sftp-file? x)
  "Return #t if X is an SFTP file port, #f otherwise."
  (%gssh-sftp-file? x))


;;; High-Level operations on remote files.
;; Those procedures are partly based on GNU Guile's 'r4rs.scm'; the goal is to
;; provide a convenient API similar to Guile I/O API.

(define (with-input-from-port port thunk)
  (let ((swaports (lambda () (set! port (set-current-input-port port)))))
    (dynamic-wind swaports thunk swaports)))

(define (with-output-to-port port thunk)
  (let ((swaports (lambda () (set! port (set-current-output-port port)))))
    (dynamic-wind swaports thunk swaports)))


(define (call-with-remote-input-file sftp-session filename proc)
  "Call a PROC with a remote file port opened for input using an SFTP-SESSION.
PROC should be a procedure of one argument, FILENAME should be a string naming
a file.  The behaviour is unspecified if a file already exists.

The procedure calls PROC with one argument: the port obtained by opening the
named remote file for input.

If the procedure returns, then the port is closed automatically and the values
yielded by the procedure are returned.  If the procedure does not return, then
the port will not be closed automatically unless it is possible to prove that
the port will never again be used for a read or write operation."
  (let ((input-file (sftp-open sftp-session filename O_RDONLY)))
    (call-with-values
        (lambda () (proc input-file))
      (lambda vals
        (close-port input-file)
        (apply values vals)))))

(define (call-with-remote-output-file sftp-session filename proc)
  "Call a PROC with a remote file port opened for output using an
SFTP-SESSION.  PROC should be a procedure of one argument, FILENAME should be
a string naming a file.  The behaviour is unspecified if a file already
exists.

The procedure calls PROC with one argument: the port obtained by opening the
named remote file for output.

If the procedure returns, then the port is closed automatically and the values
yielded by the procedure are returned.  If the procedure does not return, then
the port will not be closed automatically unless it is possible to prove that
the port will never again be used for a read or write operation."
  (let ((output-file-port (sftp-open sftp-session filename
                                     (logior O_WRONLY O_CREAT))))
    (call-with-values
        (lambda () (proc output-file-port))
      (lambda vals
        (close-port output-file-port)
        (apply values vals)))))


(define (with-input-from-remote-file sftp-session filename thunk)
  "THUNK must be a procedure of no arguments, and FILENAME must be a string
naming a file.  The file must already exist. The file is opened for input, an
input port connected to it is made the default value returned by
'current-input-port', and the THUNK is called with no arguments.  When the
THUNK returns, the port is closed and the previous default is restored.
Returns the values yielded by THUNK.  If an escape procedure is used to escape
from the continuation of these procedures, their behavior is implementation
dependent."
  (call-with-remote-input-file sftp-session filename
    (lambda (p) (with-input-from-port p thunk))))

(define (with-output-to-remote-file sftp-session filename thunk)
  "THUNK must be a procedure of no arguments, and FILENAME must be a string
naming a file.  The effect is unspecified if the file already exists.  The
file is opened for output, an output port connected to it is made the default
value returned by 'current-output-port', and the THUNK is called with no
arguments.  When the THUNK returns, the port is closed and the previous
default is restored.  Returns the values yielded by THUNK.  If an escape
procedure is used to escape from the continuation of these procedures, their
behavior is implementation dependent."
  (call-with-remote-output-file sftp-session filename
    (lambda (p) (with-output-to-port p thunk))))

(define (sftp-dir-open-stream sftp-session directory)
  "Open an SFTP directory.  Return a ICE-9 stream of directory attributes."
  (let ((dir (sftp-dir-open directory)))
    (make-stream (lambda (state)
                   (if (sftp-dir-eof? dir)
                       #f
                       (cons state (sftp-dir-read dir))))
                 (sftp-dir-read dir))))


;;; Load libraries.

(unless (getenv "GUILE_SSH_CROSS_COMPILING")
  (load-extension "libguile-ssh" "init_sftp_session")
  (load-extension "libguile-ssh" "init_sftp_file")
  (load-extension "libguile-ssh" "init_sftp_dir"))

;;; sftp-session.scm ends here.

