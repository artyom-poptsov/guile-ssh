;;; client-server.scm -- Guile-SSH client is SUT.

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
             (ice-9 threads)
             (ssh server)
             (ssh session))

(test-begin "client-server")


;;; Global symbols

(define addr   "127.0.0.1")
(define port   12345)
(define topdir (getenv "abs_top_srcdir"))
(define rsakey (format #f "~a/tests/rsakey" topdir))
(define log    (test-runner-aux-value (test-runner-current)))

(define (make-session-for-test)
  "Make a session with predefined parameters for a test."
  (make-session
   #:host    addr
   #:port    port
   #:timeout 10        ;seconds
   #:user    "bob"
   #:log-verbosity 'nolog))

(define (srvmsg message)
  "Print a server MESSAGE to the test log."
  (format log "    server: ~a~%" message))


;;; Create a Guile-SSH server

(define pid (primitive-fork))

(if (zero? pid)
    (let ((server (make-server
                   #:bindaddr addr
                   #:bindport port
                   #:rsakey   rsakey
                   #:log-verbosity 'nolog)))
      (srvmsg "created")
      (server-listen server)
      (srvmsg "listening")
      (while #t
        (let ((s (server-accept server)))
          (srvmsg "client accepted")
          (server-handle-key-exchange s)
          (srvmsg "key exchange handled")
          (sleep 1)
          (session? s)))))


;;; Test Cases

(test-assert "connect!, disconnect!"
  (let ((session (make-session-for-test)))
    (connect! session)
    (authenticate-server session)
    (let ((res (connected? session)))
      (disconnect! session)
      res)))

(test-assert "get-protocol-version"
  (let ((session (make-session-for-test)))
    (connect! session)
    (authenticate-server session)
    (let ((res (get-protocol-version session)))
      (disconnect! session)
      res)))

(test-assert "get-public-key-hash"
  (let ((session (make-session-for-test)))
    (connect! session)
    (authenticate-server session)
    (let ((res (get-public-key-hash session)))
      (disconnect! session)
      res)))

;; Stop the server
(kill pid SIGINT)

(test-end "client-server")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; client-server.scm ends here.
