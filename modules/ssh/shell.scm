;;; shell.scm -- Remote shell.

;; Copyright (C) 2016, 2017, 2020 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
;;   command-available?
;;   loadavg
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
  #:use-module (ice-9 ftw)
  #:use-module (srfi  srfi-1)
  #:use-module (ssh channel)
  #:use-module (ssh popen)
  #:use-module (ssh dist node)
  #:use-module (ssh log)
  #:export (rexec
            which
            pgrep
            pkill
            command-available?
            guile-version
            loadavg))



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



(define (which session program-name)
  "Check if a PROGRAM-NAME is available on a remote side.  Return two values:
a check result and a return code."
  (rexec session (format #f "which '~a'" program-name)))


(define* (%guile-pgrep session pattern #:key (full? #f))
  (node-eval
   (make-node session)
   `(begin
      (use-modules (ice-9 ftw)
                   (ice-9 rdelim)
                   (ice-9 format)
                   (ice-9 regex)
                   (srfi  srfi-1))
      (let ((procs (scandir "/proc" (lambda (e) (string-match "^[0-9]+" e)))))
        (fold (lambda (entry prev)
                (let* ((cmdline-file (format #f "/proc/~a/status" entry))
                       (cmdline-port (open-input-file cmdline-file)))
                  (if cmdline-port
                      (let ((cmdline (read-line cmdline-port)))
                        (cond
                         ((eof-object? cmdline)
                          prev)
                         ((and ,full? (string=? ,pattern cmdline))
                          (cons entry prev))
                         ((string-match ,pattern cmdline)
                          (cons entry prev))
                         (else
                          prev)))
                      prev)))
              '()
               procs)))))

(define* (%guile-pkill session pattern
                       #:key
                       (full? #f)
                       (signal SIGTERM))
  "Guile-SSH implementation of 'pkill' that uses Guile on the remote side.
Note that this procedure won't work if Guile is missing on a target machine.

Send a SIGNAL to a process which name matches to PATTERN on a remote machine
represented by a SESSION.  Return two values: a pkill result and a return
code."
  (let ((pids (%guile-pgrep session pattern #:full? full?)))
    (format-log 'functions
                 "[SCM] %guile-pkill"
                 "pids: ~a"
                 pids)
    (node-eval
     (make-node session)
     `(begin
        (for-each (lambda (pid) (kill pid ,signal))
                  (quote ,(map string->number pids)))
        (quote ,pids)))))



;; We should use short '-f' option for pgrep and pkill instead of the long
;; version of it ('--full') because the long version was added on september
;; 2011 (according to the commit log of procps-ng [1]) and some systems may
;; not support it due to older procps.
;;
;; [1] https://gitlab.com/procps-ng/procps/commit/4581ac2240d3c2108c6a65ba2d19599195b512bc

(define* (pgrep session pattern
                #:key
                (full?      #f)
                (use-guile? #f))
  "Check if a process with a PATTERN cmdline is available on a machine
represented by a SESSION.  Return two values: a check result and a return
code."
  (if use-guile?
      (receive (result eval-number module-name lang-name)
          (%guile-pgrep session pattern #:full? full?)
        (values result 0))
      (rexec session (format #f "pgrep ~a '~a'"
                             (if full? "-f" "")
                             pattern))))

(define* (pkill session pattern #:key
                (full?      #f)
                (signal     SIGTERM)
                (use-guile? #f))
  "Send a SIGNAL to a process which name matches to PATTERN on a remote
machine represented by a SESSION.  Return two values: a pkill result and a
return code."
  (if use-guile?
      (receive (result eval-number module-name lang-name)
          (%guile-pkill session pattern #:full? full? #:signal signal)
        (values result 0))
      (rexec session (format #f "pkill ~a --signal ~a '~a'"
                             (if full? "-f" "")
                             signal
                             pattern))))



(define (command-available? session command)
  "check if COMMAND is available on a remote side."
  (receive (result rc)
      (which session command)
    (zero? rc)))

(define (procps-available? session)
  "Check if procps is available on a NODE."
  (command-available? session "pgrep"))

(define (guile-version session)
  "Get Guile version installed on a remote side, return the version string.
Return #f if Guile is not installed."
  (receive (result rc)
      (rexec session "which guile > /dev/null && guile --version")
    (and (zero? rc)
         (car result))))

(define (loadavg session)
  "Get average load of a host using a SESSION."
  (receive (result exit-status)
      (rexec session "cat /proc/loadavg")
    (unless (zero? exit-status)
      (throw 'guile-ssh-error "Could not get average load for a host" session))
    (string-split (car result) #\space)))

;;; shell.scm ends here.
