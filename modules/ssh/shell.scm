;;; shell.scm -- Remote shell.

;; Copyright (C) 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
;; along with Guile-SSH.  If not, see
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Remote shell.
;;
;; The module provides the following procedures:
;;   rexec
;;   which
;;   pgrep
;;   fallback-pgrep
;;
;; See the Info documentation for detailed description of these exceptions and
;; aforementioned procedures.


;;; Code:

(define-module (ssh shell)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (ssh channel)
  #:use-module (ssh popen)
  #:export (rexec which pgrep fallback-pgrep command-available?))


;;;

;; TODO: Move to some other file do prevent duplicating of the procedure in
;; (ssh dist node).
(define (eof-or-null? str)
  "Return #t if a STR is an EOF object or an empty string, #f otherwise."
  (or (eof-object? str) (string-null? str)))


;;;

(define (rexec session cmd)
  "Execute a command CMD on the remote side.  Return two values: list of
output lines returned by CMD and its exit code."
  (let ((channel (open-remote-input-pipe session cmd)))
    (values (let loop ((line   (read-line channel))
                       (result '()))
              (if (eof-or-null? line)
                  (reverse result)
                  (loop (read-line channel)
                        (cons line result))))
            (channel-get-exit-status channel))))


;;;

(define (which session program-name)
  "Check if a PROGRAM-NAME is available on a remote side.  Return two values:
a check result and a return code."
  (rexec session (format #f "which '~a'" program-name)))


(define* (pgrep session pattern #:key (full? #f))
  "Check if a process with a PATTERN cmdline is available on a NODE.
Return two values: a check result and a return code."
  (rexec session (format #f "pgrep ~a '~a'"
                         (if full? "--full" "")
                         pattern)))

(define (fallback-pgrep session pattern)
  "Guile-SSH implementation of 'pgrep' that uses pure bash and '/proc'
filesystem.  Check if a process with a PATTERN cmdline is available on a NODE.
Return two values: a check result and a return code."
  (define (make-command ptrn)
    (format #f "\
echo '
for p in $(ls /proc); do
  if [[ \"$p\" =~~ ^[0-9]+ ]]; then
    name=$(cat \"/proc/$p/status\" 2>/dev/null | head -1);
    if [[ \"$name\" =~~ Name:.*guile ]]; then
      cmdline=$(cat \"/proc/$p/cmdline\");
      if [[ \"$cmdline\" =~~ ~a ]]; then
        exit 0;
      fi;
    fi;
  fi;
done;
exit 1;
' | bash
" ptrn))

  (let ((ptrn (string-append (regexp-substitute/global #f " " pattern
                                                       'pre "?" 'post)
                             ".*")))
    (rexec session (make-command ptrn))))

(define (command-available? session command)
  "check if COMMAND is available on a remote side."
  (receive (result rc)
      (which session command)
    (zero? rc)))

;;; shell.scm ends here.
