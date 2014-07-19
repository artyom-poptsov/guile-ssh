;;; client-server.scm -- Guile-SSH client is SUT.

;; Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (ice-9 threads)
             (ice-9 rdelim)
             (rnrs bytevectors)
             (ssh server)
             (ssh session)
             (ssh auth)
             (ssh message)
             (ssh key)
             (ssh channel)
             (ssh log)
             (srfi srfi-4))

(test-begin "client-server")


;;; Global symbols

(define addr   "127.0.0.1")
(define port   12400)
(define topdir (getenv "abs_top_srcdir"))
(define rsakey (format #f "~a/tests/rsakey" topdir))
(define %knownhosts (format #f "~a/tests/knownhosts" topdir))
(define log    (test-runner-aux-value (test-runner-current)))
(define *server-thread* #f)

(define %libssh-log-file "client-server-libssh.log")
(define %error-log-file  "client-server-errors.log")

(set-current-error-port (open-output-file %error-log-file))


;;; Logging callback

(define libssh-log-printer
  (let ((p (open-output-file %libssh-log-file)))
    (lambda (priority function message userdata)
      (format p "[~a, \"~a\", ~a]: ~a~%"
              (strftime "%Y-%m-%dT%H:%M:%S%z" (localtime (current-time)))
              userdata
              priority
              message))))

(set-logging-callback! libssh-log-printer)


;;; Helper procedures and macros

(define (make-session-for-test)
  "Make a session with predefined parameters for a test."
  (make-session
   #:host    addr
   #:port    port
   #:timeout 10        ;seconds
   #:user    "bob"
   #:knownhosts %knownhosts
   #:log-verbosity 'rare))

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

(define-macro (make-session-loop session . body)
  `(let session-loop ((msg (server-message-get ,session)))
     (and msg (begin ,@body))
     (and (connected? session)
          (session-loop (server-message-get ,session)))))


;; Pass the test case NAME as the userdata to the libssh log
(define-syntax test-assert-with-log
  (syntax-rules ()
    ((_ name body ...)
     (test-assert name
       (begin
         (set-log-userdata! name)
         body ...)))))


;;; Testing of basic procedures.

(test-assert-with-log "connect!, disconnect!"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))
        
        ;; client
        (let ((session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (let ((res (connected? session)))
            (disconnect! session)
            res))

        ;; server
        (begin
          (server-listen server)
          (let ((s (server-accept server)))
            (server-handle-key-exchange s)
            (primitive-exit))))))

(test-assert-with-log "get-protocol-version"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (let ((res (get-protocol-version session)))
            (disconnect! session)
            (eq? 2 res)))

        ;; server
        (begin
          (server-listen server)
          (let ((s (server-accept server)))
            (server-handle-key-exchange s)
            (primitive-exit))))))

(test-assert-with-log "authenticate-server, not-known"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (let ((res (authenticate-server session)))
            (disconnect! session)
            (eq? res 'not-known)))

        ;; server
        (begin
          (server-listen server)
          (let ((s (server-accept server)))
            (server-handle-key-exchange s)
            (primitive-exit))))))

(test-assert-with-log "authenticate-server, ok"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (write-known-host! session)
          (let ((res (authenticate-server session)))
            (disconnect! session)
            (delete-file %knownhosts)
            (eq? res 'ok)))

        ;; server
        (begin
          (server-listen server)
          (let ((s (server-accept server)))
            (server-handle-key-exchange s)
            (primitive-exit))))))

(test-assert-with-log "get-public-key-hash"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((hash-md5-bv   #vu8(15 142 110 203 162 228 250 211 20 212 26 217 118 57 217 66))
              (hash-md5-str  "0f:8e:6e:cb:a2:e4:fa:d3:14:d4:1a:d9:76:39:d9:42")
              (hash-sha1-bv  #vu8(20 65 56 155 119 45 84 163 50 26 59 92 215 159 139 5 229 174 84 80))
              (hash-sha1-str "14:41:38:9b:77:2d:54:a3:32:1a:3b:5c:d7:9f:8b:05:e5:ae:54:50")
              (session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (authenticate-server session)
          (let* ((pubkey   (get-server-public-key session))
                 (md5-res  (get-public-key-hash pubkey 'md5))
                 (sha1-res (get-public-key-hash pubkey 'sha1)))
            (disconnect! session)
            (and (bytevector=? md5-res hash-md5-bv)
                 (string=? (bytevector->hex-string md5-res) hash-md5-str)
                 (bytevector=? sha1-res hash-sha1-bv)
                 (string=? (bytevector->hex-string sha1-res) hash-sha1-str))))

        ;; server
        (begin
          (server-listen server)
          (let ((s (server-accept server)))
            (server-handle-key-exchange s)
            (primitive-exit))))))


;;; Authentication


;; Server replies with "success", client receives 'success.
(test-assert-with-log "userauth-none!, success"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (authenticate-server session)
          (let ((res (userauth-none! session)))
            (disconnect! session)
            (eq? res 'success)))

        ;; server
        (begin
          (server-listen server)
          (let ((session (server-accept server)))
            (server-handle-key-exchange session)
            (make-session-loop session
                               (message-auth-set-methods! msg '(none))
                               (message-reply-success msg)))
          (primitive-exit)))))


;; Server replies with "default", client receives 'denied.
(test-assert-with-log "userauth-none!, denied"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (authenticate-server session)
          (let ((res (userauth-none! session)))
            (disconnect! session)
            (eq? res 'denied)))

        ;; server
        (begin
          (server-listen server)
          (let ((session (server-accept server)))
            (server-handle-key-exchange session)
            (make-session-loop session
                               (message-auth-set-methods! msg '(public-key))
                               (message-reply-default msg)))
          (primitive-exit)))))


;; Server replies with "partial success", client receives 'partial.
(test-assert-with-log "userauth-none!, partial"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (authenticate-server session)
          (let ((res (userauth-none! session)))
            (disconnect! session)
            (eq? res 'partial)))

        ;; server
        (begin
          (server-listen server)
          (let ((session (server-accept server)))
            (server-handle-key-exchange session)
            (make-session-loop session
                               (message-auth-set-methods! msg '(none))
                               (message-reply-success msg 'partial)))
          (primitive-exit)))))


(test-assert-with-log "userauth-password!, success"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (authenticate-server session)
          (let ((res (userauth-password! session "password")))
            (disconnect! session)
            (eq? res 'success)))

        ;; server
        (begin
          (server-listen server)
          (let ((session (server-accept server)))
            (server-handle-key-exchange session)
            (make-session-loop session
                               (message-auth-set-methods! msg '(password))
                               (message-reply-success msg)))
          (primitive-exit)))))


(test-assert-with-log "userauth-password!, denied"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (authenticate-server session)
          (let ((res (userauth-password! session "password")))
            (disconnect! session)
            (eq? res 'denied)))

        ;; server
        (begin
          (server-listen server)
          (let ((session (server-accept server)))
            (server-handle-key-exchange session)
            (make-session-loop session
                               (message-auth-set-methods! msg '(password))
                               (message-reply-default msg)))
          (primitive-exit)))))


(test-assert-with-log "userauth-password!, partial"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (authenticate-server session)
          (let ((res (userauth-password! session "password")))
            (disconnect! session)
            (eq? res 'partial)))

        ;; server
        (begin
          (server-listen server)
          (let ((session (server-accept server)))
            (server-handle-key-exchange session)
            (make-session-loop session
                               (message-auth-set-methods! msg '(password))
                               (message-reply-success msg 'partial)))
          (primitive-exit)))))


(test-assert-with-log "userauth-public-key!, success"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (authenticate-server session)
          (let* ((prvkey (private-key-from-file rsakey)))
            (let ((res (userauth-public-key! session prvkey)))
              (disconnect! session)
              (eq? res 'success))))

        ;; server
        (begin
          (server-listen server)
          (let ((session (server-accept server)))
            (server-handle-key-exchange session)
            (make-session-loop session
                               (message-reply-success msg)))
          (primitive-exit)))))


;; Server replies "default" with the list of allowed authentication
;; methods.  Client receives the list.
(test-assert-with-log "userauth-get-list"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((session (make-session-for-test)))
          (sleep 1)
          (connect! session)
          (authenticate-server session)
          (userauth-none! session)
          (let ((res (userauth-get-list session)))
            (equal? res '(password public-key))))

        ;; server
        (begin
          (server-listen server)
          (let ((session (server-accept server)))
            (server-handle-key-exchange session)
            (make-session-loop session
                               (message-auth-set-methods! msg '(password public-key))
                               (message-reply-default msg)))
          (primitive-exit)))))



;;; Channel test

;; make, open, exec

(define (start-server/channel-test server)
  "Start SERVER for a channel test."
  (server-listen server)
  (let ((session (server-accept server))
        (channel #f))
    (server-handle-key-exchange session)
    (make-session-loop session
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
           (message-reply-success msg)))))
    (primitive-exit)))

(define (make-session/channel-test)
  "Make a session for a channel test."
  (let ((session (make-session-for-test)))
    (sleep 1)
    (connect! session)
    (authenticate-server session)
    (userauth-none! session)
    session))

(test-assert "make-channel"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let ((session (make-session/channel-test)))
          (make-channel session))

        ;; server
        (start-server/channel-test server))))

(test-assert-with-log "channel-open-session"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let* ((session (make-session/channel-test))
               (channel (make-channel session)))
          (channel-open-session channel)
          (not (port-closed? channel)))

        ;; server
        (start-server/channel-test server))))

;; Client sends "ping" as a command to execute, server replies with "pong"
(test-assert-with-log "channel-request-exec"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let* ((session (make-session/channel-test))
               (channel (make-channel session)))
          (channel-open-session channel)
          (channel-request-exec channel "ping")
          (let ((res (read-line channel)))
            (and res
                 (string=? "pong" res))))

        (start-server/channel-test server))))


;; data transferring
;; FIXME: Probably these TCs can be implemented more elegantly.

(define (start-server/dt-test server rwproc)
  (server-listen server)
  (let ((session (server-accept server))
        (channel #f))
    (server-handle-key-exchange session)
    (make-session-loop session
      (if (not (eof-object? msg))
          (let ((msg-type (message-get-type msg)))
            (case (car msg-type)
              ((request-channel-open)
               (set! channel (message-channel-request-open-reply-accept msg))
               (let poll ((ready? #f))
                 (if ready?
                     (rwproc channel))
                 (poll (char-ready? channel))))
              ((request-channel)
               (message-reply-success msg))
              (else
               (message-reply-success msg)))))))
  (primitive-exit))

(define (make-channel/dt-test session)
  (let ((c (make-channel session)))
    (channel-open-session c)
    c))


(test-assert-with-log "data transferring, string"
  (let ((server (make-server-for-test))
        (pid    (primitive-fork)))

    (if (not (= 0 pid))

        ;; client
        (let* ((session (make-session/channel-test))
               (channel (make-channel/dt-test session))
               (str "Hello Scheme World!"))
          (write-line str channel)
          (let poll ((ready? #f))
            (if ready?
                (let ((res (read-line channel)))
                  (disconnect! session)
                  (equal? res str))
                (poll (char-ready? channel)))))

        ;; server
        (start-server/dt-test server
                              (lambda (channel)
                                (let ((str (read-line channel)))
                                  (write-line str channel)))))))


(test-assert-with-log "data transferring, bytevector"
  (let ((server    (make-server-for-test))
        (pid       (primitive-fork))
        (vect-size 10)
        (vect-fill 10))

    (if (not (= 0 pid))

        ;; client
        (let* ((session (make-session/channel-test))
               (channel (make-channel/dt-test session))
               (vect    (make-u8vector vect-size vect-fill)))
          (uniform-array-write vect channel)
          (let poll ((ready? #f))
            (if ready?
                (let ((res (make-u8vector vect-size 0)))
                  (uniform-array-read! res channel)
                  (equal? res vect))
                (poll (char-ready? channel)))))

        (start-server/dt-test server
                              (lambda (channel)
                                (let ((v (make-u8vector vect-size 0)))
                                  (uniform-array-read! v channel)
                                  (uniform-array-write v channel)))))))


(test-end "client-server")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; client-server.scm ends here.
