;;; common.scm -- Heper procedures and macros for tests.

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

(use-modules (srfi srfi-64)
             (ssh session)
             (ssh server)
             (ssh log))


(define %addr   "127.0.0.1")
(define *port*  12400)
(define rsakey (format #f "~a/tests/rsakey" topdir))
(define %knownhosts (format #f "~a/tests/knownhosts"
                            (getenv "abs_top_builddir")))


;; Pass the test case NAME as the userdata to the libssh log
(define-syntax test-assert-with-log
  (syntax-rules ()
    ((_ name body ...)
     (test-assert name
       (begin
         (set-log-userdata! name)
         body ...)))))

(define-macro (make-session-loop session . body)
  `(let session-loop ((msg (server-message-get ,session)))
     (and msg (begin ,@body))
     (and (connected? session)
          (session-loop (server-message-get ,session)))))

(define (make-session-for-test)
  "Make a session with predefined parameters for a test."
  (make-session
   #:host    %addr
   #:port    *port*
   #:timeout 10        ;seconds
   #:user    "bob"
   #:knownhosts %knownhosts
   #:log-verbosity 'rare))

(define (make-server-for-test)
  "Make a server with predefined parameters for a test."

  ;; FIXME: This hack is aimed to give every server its own unique
  ;; port to listen to.  Clients will pick up new port number
  ;; automatically through global `port' symbol as well.
  (set! *port* (1+ *port*))

  (make-server
   #:bindaddr %addr
   #:bindport *port*
   #:rsakey   rsakey
   #:log-verbosity 'nolog))

;;; common.scm ends here
