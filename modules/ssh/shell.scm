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
;;   pkill
;;   fallback-pgrep
;;   fallback-pkill
;;   command-available?
;;
;; See the Info documentation for detailed description of these exceptions and
;; aforementioned procedures.


;;; Code:

(define-module (ssh shell)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (ssh channel)
  #:use-module (ssh popen)
  #:use-module (ssh log)
  #:export (rexec which pgrep pkill fallback-pgrep command-available?
                  fallback-pkill loadavg))


;;;

(define (rexec session cmd)
  "Execute a command CMD on the remote side.  Return two values: list of
output lines returned by CMD and its exit code."
  (let* ((channel (open-remote-input-pipe session cmd))
         (result  (let loop ((line   (read-line channel))
                             (result '()))
                    (if (eof-object? line)
                        (reverse result)
                        (loop (read-line channel)
                              (cons line result)))))
         (exit-status (channel-get-exit-status channel)))
    (close channel)
    (values result exit-status)))


;;;

(define (which session program-name)
  "Check if a PROGRAM-NAME is available on a remote side.  Return two values:
a check result and a return code."
  (rexec session (format #f "which '~a'" program-name)))


;; We should use short '-f' option for pgrep and pkill instead of the long
;; version of it ('--full') because the long version was added on september
;; 2011 (according to the commit log of procps-ng [1]) and some systems may
;; not support it due to older procps.
;;
;; [1] https://gitlab.com/procps-ng/procps/commit/4581ac2240d3c2108c6a65ba2d19599195b512bc

(define* (pgrep session pattern #:key (full? #f))
  "Check if a process with a PATTERN cmdline is available on a machine
represented by a SESSION.  Return two values: a check result and a return
code."
  (rexec session (format #f "pgrep ~a '~a'"
                         (if full? "-f" "")
                         pattern)))

(define* (pkill session pattern #:key (full? #f)
                (signal 'SIGTERM))
  "Send a SIGNAL to a process which name matches to PATTERN on a remote
machine represented by a SESSION.  Return two values: a pkill result and a
return code."
  (rexec session (format #f "pkill ~a --signal ~a '~a'"
                         (if full? "-f" "")
                         signal
                         pattern)))

(define* (fallback-pgrep session pattern #:key (full? #f))
  "Guile-SSH implementation of 'pgrep' that uses pure bash and '/proc'
filesystem.  Check if a process with a PATTERN cmdline is available on a NODE.
Note that FULL? option is not used at the time (the procedure always perform
full search.)  Return two values: a check result and a return code."
  (define (make-command ptrn)
    (format #f "\
echo '
for p in $(ls /proc); do
  if [[ \"$p\" =~~ ^[0-9]+ ]]; then
    name=$(cat \"/proc/$p/status\" 2>/dev/null | head -1);
    if [[ \"$name\" =~~ Name:.*guile ]]; then
      cmdline=$(cat \"/proc/$p/cmdline\");
      if [[ \"$cmdline\" =~~ ~a ]]; then
        echo $p
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

(define* (fallback-pkill session pattern #:key (full? #f)
                         (signal 'SIGTERM))
  "Guile-SSH implementation of 'pkill' that uses pure bash, '/proc' filsystem
and Guile itself to kill a process.  Note that this procedure won't work if
Guile is missing on a target machine.

Send a SIGNAL to a process which name matches to PATTERN on a remote machine
represented by a SESSION.  Return two values: a pkill result and a return
code."
  (let-values (((pids exit-status) (fallback-pgrep session pattern)))
    (format-log 'functions "[scm] fallback-pkill"
                "pids: ~a (pgrep exit status: ~a)"
                (car pids) exit-status)
    (let ((cmd (format "guile -c '(kill ~a ~a)'" (car pids) signal)))
      (format-log 'functions "[scm] fallback-pkill"
                "going to use this kill command: ~a" cmd)
      (rexec session cmd))))

(define (command-available? session command)
  "check if COMMAND is available on a remote side."
  (receive (result rc)
      (which session command)
    (zero? rc)))

(define (loadavg session)
  "Get average load of a host using a SESSION."
  (receive (result exit-status)
      (rexec session "cat /proc/loadavg")
    (unless (zero? exit-status)
      (throw 'guile-ssh-error "Could not get average load for a host" session))
    (string-split (car result) #\space)))

;;; shell.scm ends here.
