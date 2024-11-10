;;; sssh-ssshd.scm -- Communication between sssh and ssshd.

;; Copyright (C) 2014, 2015, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex)
             ;; Helper procedures
             (tests common)
             (ssh version))

(test-begin-with-log "sssh-ssshd")


;;;

(define *test-cmd* "uname -a")

(define *srv-address* INADDR_LOOPBACK)
(define *srv-port*    12600)
(define *srv-pid-file* "ssshd.pid")

(define *ssshd-cmd*
  (string-append
   %topbuilddir "/examples/ssshd.scm --detach"
   " --pid-file=" *srv-pid-file*
   " --port=" (number->string *srv-port*)
   " --rsakey=" %rsakey
   " --dsakey=" %dsakey))

(define %libssh-version
  (map string->number (string-split (get-libssh-version) #\.)))

(define *sssh-cmd*
  (string-append
   %topbuilddir "/examples/sssh.scm"
   ;; XXX: We cannot use ECDSA keys in libssh versions prior 0.8.3 because of
   ;; the bug that was fixed only in 0.8.3.
   " --identity-file=" (if (or (= (cadr %libssh-version) 7)
                               (and (= (cadr %libssh-version) 8)
                                    (< (caddr %libssh-version) 3)))
                           %dsakey
                           %ecdsakey)
   " --port=" (number->string *srv-port*)
   " --known-hosts-file=''"
   " " (inet-ntop AF_INET *srv-address*)
   " '" *test-cmd* "'"))

(setenv "GUILE_LOAD_PATH" (string-append %topdir "/modules"))

;; We must unset `SSH_AUTH_SOCK' to prevent sssh from asking SSH agent
;; (if it is present) for keys.
(unsetenv "SSH_AUTH_SOCK")

(define ssshd-pid #f)

(define (cleanup pid)
  (when pid
    (catch #t
      (lambda ()
        (kill pid SIGTERM))
      (const #t))
    (catch #t
      (lambda ()
        (waitpid pid))
      (const #t)))
  (and (file-exists? *srv-pid-file*)
       (delete-file *srv-pid-file*)))

(define (wait-pid-file max-tries pid-file)
  (let loop ((exists?    #f)
             (sleep-time 1)  ; s
             (try        1))
    (if exists?
        (let* ((p   (open-input-file pid-file))
               (pid (read-line p)))
          (string->number pid))
        (if (<= try max-tries)
            (begin
              (sleep sleep-time)
              (loop (file-exists? pid-file)
                    (1+ sleep-time)
                    (1+ try)))
            (begin
              (format #t "Couldn't read a PID file ~a in ~a tries.~%"
                      pid-file try)
              #f)))))


;;; Tests

(test-assert-with-log "ssshd, start"
  (let ((max-tries 10))
    (system *ssshd-cmd*)
    (let ((pid (wait-pid-file max-tries *srv-pid-file*)))
      (cleanup pid)
      pid)))

(test-assert-with-log "sssh, exec"
  (let ((max-tries 10))
    (format (current-error-port) "test  command: ~A~%" *test-cmd*)
    (format (current-error-port) "ssshd command: ~A~%" *ssshd-cmd*)
    (format (current-error-port) "sssh  command: ~A~%" *sssh-cmd*)
    (system *ssshd-cmd*)
    (let* ((pid    (wait-pid-file max-tries *srv-pid-file*))
           (output (read-line (open-input-pipe *test-cmd*)))
           (p      (open-input-pipe *sssh-cmd*))
           (result (let ((line (read-line p)))
                     (format (current-error-port) "      line: ~a~%" line)
                     (and (not (eof-object? line))
                          (string=? line output)))))
      (format (current-error-port) "output: ~a~%" output)
      (format (current-error-port) "result: ~a~%" result)
      (cleanup pid)
      result)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "sssh-ssshd")

(exit (= 0 exit-status))

;;; sssh-ssshd.scm ends here.
