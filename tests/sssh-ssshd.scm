;;; sssh-ssshd.scm -- Communication between sssh and ssshd.

;; Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This file is a part of libguile-ssh.
;;
;; libguile-ssh is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; libguile-ssh is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with libguile-ssh.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (srfi srfi-64)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex))

(test-begin "sssh-ssshd")


;;;

(define *topdir* (getenv "abs_top_srcdir"))
(define *rsakey* (format #f "~a/tests/rsakey" *topdir*))
(define *dsakey* (format #f "~a/tests/dsakey" *topdir*))
(define *test-cmd* "uname --all")

(define *srv-address* INADDR_LOOPBACK)
(define *srv-port*    12600)
(define *srv-pid-file* "ssshd.pid")

(define *ssshd-cmd*
  (string-append
   *topdir* "/examples/ssshd.scm --detach"
   " --pid-file=" *srv-pid-file*
   " --port=" (number->string *srv-port*)
   " --rsakey=" *rsakey*
   " --dsakey=" *dsakey*))

(define *sssh-cmd*
  (string-append
   *topdir* "/examples/sssh.scm"
   " --identity-file=" *rsakey*
   " --port=" (number->string *srv-port*)
   " " (inet-ntoa *srv-address*)
   " '" *test-cmd* "'"))

(setenv "GUILE_LOAD_PATH" *topdir*)

;; We must unset `SSH_AUTH_SOCK' to prevent sssh from asking SSH agent
;; (if it is present) for keys.
(unsetenv "SSH_AUTH_SOCK")

(define ssshd-pid #f)


;;; Tests

(test-assert "ssshd, start"
  (let ((*max-tries* 10))
    (system *ssshd-cmd*)
    (let wait-pid-file ((exists?    #f)
                        (sleep-time 1)  ;s
                        (try        1))
      (if exists?
          (let* ((p   (open-input-file *srv-pid-file*))
                 (pid (read-line p)))
            (set! ssshd-pid (string->number pid)))
          (if (<= try *max-tries*)
              (begin
                (sleep sleep-time)
                (wait-pid-file (file-exists? *srv-pid-file*)
                               (1+ sleep-time)
                               (1+ try)))
              (format #t "Couldn't read a PID file ~a in ~a tries.~%"
                      *srv-pid-file* try))))
    ssshd-pid))

(define output (read-line (open-input-pipe *test-cmd*)))

(test-assert "sssh, exec"
  (let ((p (open-input-pipe *sssh-cmd*))
        (res #f))
    (let r ((l (read-line p)))
      (if (not (eof-object? l))
          (if (string=? output l)
              (set! res #t)
              (r (read-line p)))))
    res))


;;; Cleanup

(if ssshd-pid
    (kill ssshd-pid SIGTERM))

(if (file-exists? *srv-pid-file*)
    (delete-file *srv-pid-file*))


(test-end "sssh-ssshd")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; sssh-ssshd.scm ends here.
