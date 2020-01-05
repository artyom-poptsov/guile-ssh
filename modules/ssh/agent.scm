;;; agent.scm -- Interaction with SSH agents.

;; Copyright (C) 2020 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
;;   ssh-agent-find-socks
;;
;; See the Info documentation for detailed description of these exceptions and
;; aforementioned procedures.


;;; Code:

(define-module (ssh agent)
  #:use-module (ice-9 regex)
  #:export (ssh-agent-sock-get
            ssh-agent-sock-set!
            ssh-agent-find-socks))

(define %ssh-agent-sock-env   "SSH_AUTH_SOCK")
(define %ssh-agent-dir-regexp "^ssh-[A-Za-z0-9]{12}")

(define (ssh-agent-sock-get)
  "Get the 'SSH_AGENT_SOCK' environment variable value."
  (getenv %ssh-agent-sock-env))

(define (ssh-agent-sock-set! sock-file)
  "Set the value of 'SSH_AGENT_SOCK' environment variable."
  (setenv %ssh-agent-sock-env sock-file))

(define* (ssh-agent-find-socks #:key
                               (search-dir     (getenv "TMPDIR"))
                               (subdir-pattern %ssh-agent-dir-regexp))
  (define (current-user-uid? obj)
    (let ((st (stat obj)))
      (= (getuid) (stat:uid st))))

  (define (append-sock path)
    (string-append path
                   "/"
                   (let ((dir (opendir path)))
                     (readdir dir)                    ; skip "."
                     (readdir dir)                    ; skip ".."
                     (readdir dir))))

  (let* ((search-dir (or search-dir "/tmp"))
         (dir        (opendir search-dir)))
    (let loop ((result '())
               (entry  (readdir dir)))
      (if (eof-object? entry)
          result
          (if (string-match subdir-pattern entry)
              (let ((path (string-append search-dir "/" entry)))
                (loop (if (current-user-uid? path)
                          (cons (append-sock path) result)
                          result)
                      (readdir dir)))
              (loop result (readdir dir)))))))

;;; agent.scm ends here.
