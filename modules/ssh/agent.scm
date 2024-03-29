;;; agent.scm -- Interaction with SSH agents.

;; Copyright (C) 2020-2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; The module contains procedures for interaction with SSH agent instances.
;;
;; The module provides the following procedures:
;;   ssh-agent-sock-get
;;   ssh-agent-sock-set!
;;   ssh-agent-start
;;   ssh-agent-stop
;;   ssh-agent-info
;;
;; See the Info documentation for detailed description of these exceptions and
;; aforementioned procedures.


;;; Code:

(define-module (ssh agent)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:export (ssh-agent-sock-get
            ssh-agent-sock-set!
            ssh-agent-start
            ssh-agent-stop
            ssh-agent-info))



(define %ssh-agent-sock-env   "SSH_AUTH_SOCK")
(define %ssh-agent-dir-regexp (make-regexp "^ssh-[A-Za-z0-9]{12}"))
(define %ssh-agent-pid-file-regexp (make-regexp "agent.[0-9]+"))
(define %ssh-auth-sock-regexp
  (make-regexp "SSH_AUTH_SOCK=(.*); export SSH_AUTH_SOCK;"))

(define %ssh-agent-pid-regexp
  (make-regexp "SSH_AGENT_PID=(.*); export SSH_AGENT_PID;"))



(define (ssh-agent-start)
  "Start an OpenSSH agent.  Return a list with SSH agent information."
  (let ((p (open-input-pipe "ssh-agent -s")))
    (let ((ssh-auth-sock-data (read-line p))
          (ssh-agent-pid-data (read-line p)))

      (when (or (eof-object? ssh-auth-sock-data)
                (eof-object? ssh-agent-pid-data))
        (error "Could not start a SSH agent"))

      (close p)

      (let ((sockm (regexp-exec %ssh-auth-sock-regexp  ssh-auth-sock-data))
            (pidm   (regexp-exec %ssh-agent-pid-regexp ssh-agent-pid-data)))

        (unless (and sockm pidm)
          (error "Could not parse SSH agent response"
                 ssh-auth-sock-data
                 ssh-agent-pid-data))

      `((SSH_AUTH_SOCK . ,(match:substring sockm 1))
        (SSH_AGENT_PID . ,(match:substring pidm 1)))))))

(define (ssh-agent-stop)
  "Kill the current agent (given by the 'SSH_AGENT_PID' environment
variable)."
  (system "ssh-agent -k"))


(define* (ssh-agent-info #:key
                         (user (getenv "USER"))
                         (path (or (getenv "TMPDIR") "/tmp")))
  "Get OpenSSH agent information for a given USER as a list."

  (define (owned-by-user? file-name uid)
    (= (stat:uid (stat file-name)) uid))

  (define (user->uid user)
    (passwd:uid (getpwnam user)))

  (define (get-agent-socket-file dir-name)
    (let ((stream (opendir dir-name)))
      (let loop ((file-name (readdir stream)))
        (if (regexp-exec %ssh-agent-pid-file-regexp file-name)
            (begin
              (closedir stream)
              file-name)
            (loop (readdir stream))))))

  (define (agent-socket->pid agent-socket)
    (cadr (string-split agent-socket #\.)))

  (let ((dir (opendir path))
        (uid (user->uid user)))
    (let loop ((entry (readdir dir))
               (info  '()))
      (if (eof-object? entry)
          (begin
            (closedir dir)
            info)
          (let ((file-name (string-append path "/" entry)))
            (if (and (regexp-exec %ssh-agent-dir-regexp entry)
                     (owned-by-user? file-name uid))
                (let* ((agent-socket (get-agent-socket-file file-name))
                       (auth-sock    (string-append file-name "/" agent-socket))
                       (agent-pid    (agent-socket->pid agent-socket)))
                  (loop (readdir dir)
                        (append info
                                (list
                                 (list (cons 'SSH_AUTH_SOCK auth-sock)
                                       (cons 'SSH_AGENT_PID agent-pid))))))
                (loop (readdir dir) info)))))))

(define (ssh-agent-sock-get)
  "Get the 'SSH_AGENT_SOCK' environment variable value."
  (getenv %ssh-agent-sock-env))

(define (ssh-agent-sock-set! sock-file)
  "Set the value of 'SSH_AGENT_SOCK' environment variable."
  (setenv %ssh-agent-sock-env sock-file))

;;; agent.scm ends here.
