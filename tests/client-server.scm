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
             (ice-9 rdelim)
             (ssh server)
             (ssh session)
             (ssh auth)
             (ssh message)
             (ssh key)
             (ssh channel))

(test-begin "client-server")


;;; Global symbols

(define addr   "127.0.0.1")
(define port   12345)
(define topdir (getenv "abs_top_srcdir"))
(define rsakey (format #f "~a/tests/rsakey" topdir))
(define log    (test-runner-aux-value (test-runner-current)))
(define server-thread #f)


;;; Helper procedures and macros

(define (make-session-for-test)
  "Make a session with predefined parameters for a test."
  (make-session
   #:host    addr
   #:port    port
   #:timeout 10        ;seconds
   #:user    "bob"
   #:log-verbosity 'nolog))

(define (make-server-for-test)
  "Make a server with predefined parameters for a test."

  ;; FIXME: This hack is aimed to give every server its own unique
  ;; port to listen to.  Clients will pick up new port number
  ;; automatically through global `port' symbol as well.
  (set! port (1+ port))

  (make-server
   #:bindaddr addr
   #:bindport port
   #:rsakey   rsakey
   #:log-verbosity 'nolog))

(define (srvmsg message)
  "Print a server MESSAGE to the test log."
  (format log "    server: ~a~%" message))

(define-macro (spawn-server-thread . body)
  `(set! server-thread
      (make-thread
       (lambda ()
         ,@body))))

(define (cancel-server-thread)
  (cancel-thread server-thread))


;;; Testing of basic procedures.


(spawn-server-thread
 (let ((server (make-server-for-test)))
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

(cancel-server-thread)


;; Server replies "default" with the list of allowed authentication
;; methods.  Client receives the list.
(spawn-server-thread
 (let ((server (make-server-for-test)))
   (server-listen server)
   (while #t
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)
       (let session-loop ((msg (server-message-get s)))
         (message-auth-set-methods! msg '(password public-key))
         (message-reply-default msg)
         (session-loop (server-message-get s)))))))

(test-assert "userauth-get-list"
  (let ((session (make-session-for-test)))
    (connect! session)
    (authenticate-server session)
    (let ((res (userauth-get-list session)))
      (equal? res '(password public-key)))))

(cancel-server-thread)


;;; Authentication

;; Server replies with "success", client receives 'success.
(spawn-server-thread
 (let ((server (make-server-for-test)))
   (server-listen server)
   (while #t
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)
       (let session-loop ((msg (server-message-get s)))
         (message-auth-set-methods! msg '(none))
         (message-reply-success msg)
         (session-loop (server-message-get s)))))))

(test-assert "userauth-none!, success"
  (let ((session (make-session-for-test)))
    (connect! session)
    (authenticate-server session)
    (let ((res (userauth-none! session)))
      (disconnect! session)
      (eq? res 'success))))

(cancel-server-thread)


;; Server replies with "default", client receives 'denied.
(spawn-server-thread
 (let ((server (make-server-for-test)))
   (server-listen server)
   (while #t
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)
       (let session-loop ((msg (server-message-get s)))
         (message-auth-set-methods! msg '(public-key))
         (message-reply-default msg)
         (session-loop (server-message-get s)))))))

(test-assert "userauth-none!, denied"
  (let ((session (make-session-for-test)))
    (sleep 2)
    (connect! session)
    (authenticate-server session)
    (let ((res (userauth-none! session)))
      (disconnect! session)
      (eq? res 'denied))))

(cancel-server-thread)


;; Server replies with "partial success", client receives 'partial.
(spawn-server-thread
 (let ((server (make-server-for-test)))
   (server-listen server)
   (while #t
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)
       (let session-loop ((msg (server-message-get s)))
         (message-auth-set-methods! msg '(none))
         (message-reply-success msg 'partial)
         (session-loop (server-message-get s)))))))

(test-assert "userauth-none!, partial"
  (let ((session (make-session-for-test)))
    (connect! session)
    (authenticate-server session)
    (let ((res (userauth-none! session)))
      (disconnect! session)
      (eq? res 'partial))))

(cancel-server-thread)


(spawn-server-thread
 (let ((server (make-server-for-test)))
   (server-listen server)
   (while #t
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)
       (let session-loop ((msg (server-message-get s)))
         (message-auth-set-methods! msg '(password))
         (message-reply-success msg)
         (session-loop (server-message-get s)))))))

(test-assert "userauth-password!, success"
  (let ((session (make-session-for-test)))
    (connect! session)
    (authenticate-server session)
    (let ((res (userauth-password! session "alice" "password")))
      (disconnect! session)
      (eq? res 'success))))

(cancel-server-thread)


(spawn-server-thread
 (let ((server (make-server-for-test)))
   (server-listen server)
   (while #t
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)
       (let session-loop ((msg (server-message-get s)))
         (message-auth-set-methods! msg '(password))
         (message-reply-default msg)
         (session-loop (server-message-get s)))))))

(test-assert "userauth-password!, denied"
  (let ((session (make-session-for-test)))
    (connect! session)
    (authenticate-server session)
    (let ((res (userauth-password! session "alice" "password")))
      (disconnect! session)
      (eq? res 'denied))))

(cancel-server-thread)


(spawn-server-thread
 (let ((server (make-server-for-test)))
   (server-listen server)
   (while #t
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)
       (let session-loop ((msg (server-message-get s)))
         (message-auth-set-methods! msg '(password))
         (message-reply-success msg 'partial)
         (session-loop (server-message-get s)))))))

(test-assert "userauth-password!, partial"
  (let ((session (make-session-for-test)))
    (connect! session)
    (authenticate-server session)
    (let ((res (userauth-password! session "alice" "password")))
      (disconnect! session)
      (eq? res 'partial))))

(cancel-server-thread)


(spawn-server-thread
 (let ((server (make-server-for-test)))
   (server-listen server)
   (while #t
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)
       (let session-loop ((msg (server-message-get s)))
         (message-reply-success msg)
         (session-loop (server-message-get s)))))))

(test-assert "userauth-pubkey!, success"
  (let ((session (make-session-for-test)))
    (connect! session)
    (authenticate-server session)
    (let* ((prvkey (private-key-from-file session rsakey))
           (pubkey (private-key->public-key prvkey)))
      (let ((res (userauth-pubkey! session #f pubkey prvkey)))
        (disconnect! session)
        (eq? res 'success)))))

(cancel-server-thread)


;;; Channel test

(spawn-server-thread
 (let ((server (make-server-for-test)))
   (server-listen server)
   (while #t
     (let ((s (server-accept server))
           (channel #f))
       (server-handle-key-exchange s)
       (let session-loop ((msg (server-message-get s)))
         (let ((msg-type (message-get-type msg)))
           (srvmsg msg-type)
           (case (car msg-type)
             ((request-channel-open)
              (set! channel (message-channel-request-open-reply-accept msg)))
             ((request-channel)
              (if (equal? (cadr msg-type) 'channel-request-exec)
                  (write-line "pong" channel))
              (message-reply-success msg))
             (else
               (message-reply-success msg))))
         (session-loop (server-message-get s)))))))

(define session
  (let ((session (make-session-for-test)))
    (connect! session)
    (authenticate-server session)
    (userauth-none! session)
    session))

(define channel #f)

(test-assert "make-channel"
  (begin
    (set! channel (make-channel session))
    channel))

(test-assert "channel-open-session"
  (begin
    (channel-open-session channel)
    (not (port-closed? channel))))

(test-assert "channel-request-exec"
  (begin
    (channel-request-exec channel "hello")
    (let ((res (read-line channel)))
      (and res
           (string=? "pong" res)))))

(disconnect! session)
(set! session #f)

(cancel-server-thread)


(test-end "client-server")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; client-server.scm ends here.
