;;; auth.scm -- API for SSH user authentication.

;; Copyright (C) 2013-2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains API that is used for SSH user authentication.
;;
;; These methods are exported:
;;
;;   userauth-public-key!
;;   userauth-public-key/auto!
;;   userauth-public-key/try
;;   userauth-agent!
;;   userauth-password!
;;   userauth-gssapi!
;;   userauth-none!
;;   userauth-get-list
;;   openssh-agent-start
;;   openssh-agent-info
;;   openssh-agent-setenv


;;; Code:

(define-module (ssh auth)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ssh log)
  #:use-module (ssh session)
  #:export (userauth-public-key!
            userauth-public-key/auto!
            userauth-public-key/try
            userauth-agent!
            userauth-password!
            userauth-gssapi!
            userauth-none!
            userauth-get-list
            openssh-agent-start
            openssh-agent-info
            openssh-agent-setenv))

;;;

(define %ssh-auth-sock-regexp
  (make-regexp "SSH_AUTH_SOCK=(.*); export SSH_AUTH_SOCK;"))

(define %ssh-agent-pid-regexp
  (make-regexp "SSH_AGENT_PID=(.*); export SSH_AGENT_PID;"))

(define (openssh-agent-start)
  "Start an OpenSSH agent.  Return a list with SSH agent information."
  (let ((p (open-input-pipe "ssh-agent -s")))
    (let ((ssh-auth-sock-data (read-line p))
          (ssh-agent-pid-data (read-line p)))
      `((SSH_AUTH_SOCK
         . ,(let ((match (regexp-exec %ssh-auth-sock-regexp
                                      ssh-auth-sock-data)))
              (match:substring match 1)))
        (SSH_AGENT_PID
         . ,(let ((match (regexp-exec %ssh-agent-pid-regexp
                                      ssh-agent-pid-data)))
              (match:substring match 1)))))))


;;;

(define %ssh-agent-dir-regexp
  (make-regexp "ssh-[0-9A-Za-z]+$"))

(define* (openssh-agent-info #:optional (user (getenv "USER")))
  "Get OpenSSH agent information for a given USER as a list."

  (define (owned-by-user? file-name uid)
    (= (stat:uid (stat file-name)) uid))

  (define (user->uid user)
    (passwd:uid (getpwnam user)))

  (define (readdir-3rd dir-name)
    (let ((stream (opendir dir-name)))
      (readdir stream)
      (readdir stream)
      (let ((file (readdir stream)))
        (closedir stream)
        file)))

  (define (agent-socket->pid agent-socket)
    (cdr (string-split agent-socket #\.)))

  (let ((dir (opendir "/tmp"))
        (uid (user->uid user)))
    (let loop ((entry (readdir dir))
               (info  '()))
      (if (eof-object? entry)
          info
          (let ((file-name (string-append "/tmp/" entry)))
            (if (and (regexp-exec %ssh-agent-dir-regexp entry)
                     (owned-by-user? file-name uid))
                (let ((agent-socket (readdir-3rd file-name)))
                  (loop (readdir dir)
                        (cons `(,(string-append file-name "/" agent-socket)
                                . ,(agent-socket->pid agent-socket))
                              info)))
                (loop (readdir dir) info)))))))


;;;

(define (openssh-agent-setenv)
  "Setup openssh agent environment variables for the current user."
  (setenv "SSH_AUTH_SOCK" (caar (openssh-agent-info))))


;;;

(unless (getenv "GUILE_SSH_CROSS_COMPILING")
  (load-extension "libguile-ssh" "init_auth_func"))

;;; auth.scm ends here.
