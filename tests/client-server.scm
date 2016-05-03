;;; client-server.scm -- Guile-SSH client is SUT.

;; Copyright (C) 2014, 2015, 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (srfi srfi-26)
             (ice-9 threads)
             (ice-9 rdelim)
             (rnrs bytevectors)
             (rnrs io ports)
             (ssh server)
             (ssh session)
             (ssh auth)
             (ssh message)
             (ssh key)
             (ssh channel)
             (ssh log)
             (ssh tunnel)
             (srfi srfi-4))

(test-begin "client-server")


;;; Global symbols

(define topdir (getenv "abs_top_srcdir"))

(define log    (test-runner-aux-value (test-runner-current)))
(define *server-thread* #f)

;;; Load helper procedures

(add-to-load-path (getenv "abs_top_srcdir"))
(use-modules (tests common))


;;; Logging

(setup-test-suite-logging! "client-server")


;;; Helper procedures and macros

(define (srvmsg message)
  "Print a server MESSAGE to the test log."
  (format log "    server: ~a~%" message))


;;; Testing of basic procedures.

;; Helper procedures.

(define (simple-server-proc server)
  "start a SERVER that accepts a connection and handles a key exchange."
  (let ((s (server-accept server)))
    (server-handle-key-exchange s)))


;; Tests.

(test-assert-with-log "connect!, disconnect!"
  (run-client-test

   ;; server
   simple-server-proc

   ;; client
   (lambda ()
     (let ((session (make-session-for-test)))
       (sleep 1)
       (connect! session)
       (let ((res (connected? session)))
         (disconnect! session)
         res)))))

(test-assert-with-log "get-protocol-version"
  (run-client-test

   ;; server
   simple-server-proc

   ;; client
   (lambda ()
     (let ((session (make-session-for-test)))
       (sleep 1)
       (connect! session)
       (let ((res (get-protocol-version session)))
         (disconnect! session)
         (eq? 2 res))))))

(test-assert-with-log "authenticate-server, not-known"
  (run-client-test

   ;; server
   simple-server-proc

   ;; client
   (lambda ()
     (let ((session (make-session-for-test)))
       (sleep 1)
       (connect! session)
       (let ((res (authenticate-server session)))
         (disconnect! session)
         (eq? res 'not-known))))))

(test-assert-with-log "authenticate-server, ok"
  (run-client-test

   ;; server
   simple-server-proc

   ;; client
   (lambda ()
     (let ((session (make-session-for-test)))
       (sleep 1)
       (connect! session)
       (write-known-host! session)
       (let ((res (authenticate-server session)))
         (disconnect! session)
         (delete-file %knownhosts)
         (eq? res 'ok))))))

(test-assert-with-log "get-public-key-hash"
  (run-client-test

   ;; server
   simple-server-proc

   ;; client
   (lambda ()
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
              (string=? (bytevector->hex-string sha1-res) hash-sha1-str)))))))


;;; Authentication


;; Server replies with "success", client receives 'success.
(test-assert-with-log "userauth-none!, success"
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (make-session-loop session
                          (message-auth-set-methods! msg '(none))
                          (message-reply-success msg))))

   ;; client
   (lambda ()
     (let ((session (make-session-for-test)))
       (sleep 1)
       (connect! session)
       (authenticate-server session)
       (let ((res (userauth-none! session)))
         (disconnect! session)
         (eq? res 'success))))))


;; Server replies with "default", client receives 'denied.
(test-assert-with-log "userauth-none!, denied"
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (make-session-loop session
                          (message-auth-set-methods! msg '(public-key))
                          (message-reply-default msg))))

   ;; client
   (lambda ()
     (let ((session (make-session-for-test)))
       (sleep 1)
       (connect! session)
       (authenticate-server session)
       (let ((res (userauth-none! session)))
         (disconnect! session)
         (eq? res 'denied))))))


;; Server replies with "partial success", client receives 'partial.
(test-assert-with-log "userauth-none!, partial"
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (make-session-loop session
                          (message-auth-set-methods! msg '(none))
                          (message-reply-success msg 'partial))))

   ;; client
   (lambda ()
     (let ((session (make-session-for-test)))
       (sleep 1)
       (connect! session)
       (authenticate-server session)
       (let ((res (userauth-none! session)))
         (disconnect! session)
         (eq? res 'partial))))))


(test-assert-with-log "userauth-password!, success"
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (make-session-loop session
                          (message-auth-set-methods! msg '(password))
                          (message-reply-success msg))))

   ;; client
   (lambda ()
     (let ((session (make-session-for-test)))
       (sleep 1)
       (connect! session)
       (authenticate-server session)
       (let ((res (userauth-password! session "password")))
         (disconnect! session)
         (eq? res 'success))))))


(test-assert-with-log "userauth-password!, denied"
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (make-session-loop session
                          (message-auth-set-methods! msg '(password))
                          (message-reply-default msg))))

   ;; client
   (lambda ()
     (let ((session (make-session-for-test)))
       (sleep 1)
       (connect! session)
       (authenticate-server session)
       (let ((res (userauth-password! session "password")))
         (disconnect! session)
         (eq? res 'denied))))))


(test-assert-with-log "userauth-password!, partial"
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (make-session-loop session
                          (message-auth-set-methods! msg '(password))
                          (message-reply-success msg 'partial))))

   ;; client
   (lambda ()
     (let ((session (make-session-for-test)))
       (sleep 1)
       (connect! session)
       (authenticate-server session)
       (let ((res (userauth-password! session "password")))
         (disconnect! session)
         (eq? res 'partial))))))


(test-assert-with-log "userauth-public-key!, success"
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (make-session-loop session
                          (message-reply-success msg))))

   ;; client
   (lambda ()
     (let ((session (make-session-for-test)))
       (sleep 1)
       (connect! session)
       (authenticate-server session)
       (let* ((prvkey (private-key-from-file %rsakey)))
         (let ((res (userauth-public-key! session prvkey)))
           (disconnect! session)
           (eq? res 'success)))))))


;; Server replies "default" with the list of allowed authentication
;; methods.  Client receives the list.
(test-assert-with-log "userauth-get-list"
  (run-client-test

   ;; server
   (lambda (server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (make-session-loop session
                          (message-auth-set-methods! msg '(password public-key))
                          (message-reply-default msg))))

   ;; client
   (lambda ()
     (let ((session (make-session-for-test)))
       (sleep 1)
       (connect! session)
       (authenticate-server session)
       (userauth-none! session)
       (let ((res (userauth-get-list session)))
         (equal? res '(password public-key)))))))


;;; Channel test

;; make, open, exec

(define (start-server/channel-test server)
  "Start SERVER for a channel test."
  (start-server-loop server
    (let ((channel #f))
      (lambda (msg)
        (let ((msg-type (message-get-type msg)))
          (srvmsg msg-type)
          (case (car msg-type)
            ((request-channel-open)
             (set! channel (message-channel-request-open-reply-accept msg)))
            ((request-channel)
             (if (equal? (cadr msg-type) 'channel-request-exec)
                 (let ((cmd (exec-req:cmd (message-get-req msg))))
                   (cond
                    ((string=? cmd "ping")
                     (write-line "pong" channel)
                     (message-reply-success msg))
                    ((string=? cmd "uname") ; For exit status testing
                     (message-reply-success msg)
                     (channel-request-send-exit-status channel 0))))
                 (message-reply-success msg)))
            (else
             (message-reply-success msg))))))))

(define (make-session/channel-test)
  "Make a session for a channel test."
  (define max-tries 30)
  (let loop ((session (make-session-for-test))
             (count   max-tries))
    (if (not (eq? (connect! session) 'ok))
        (begin
          (format-log/scm 'nolog
                          "make-session/channel-test"
                          "Unable to connect in ~d tries: ~a~%"
                          (- max-tries count)
                          session)
          (disconnect! session)
          (set! session #f)
          (sleep 1)
          (if (zero? count)
              (format-log/scm 'nolog
                              "make-session/channel-test"
                              "~a"
                              "Giving up ...")
              (loop (make-session-for-test)
                    (1- count))))
        (begin
          (authenticate-server session)
          (userauth-none! session)
          session))))

(test-assert "make-channel"
  (run-client-test

   ;; server
   (lambda (server)
     (start-server/channel-test server))

   ;; client
   (lambda ()
     (let ((session (make-session/channel-test)))
       (make-channel session)))))

(test-assert-with-log "channel-get-session"
  (run-client-test

   ;; server
   (lambda (server)
     (start-server/channel-test server))

   ;; client
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (make-channel session)))
       (eq? session (channel-get-session channel))))))

(test-assert-with-log "channel-open-session"
  (run-client-test

   ;; server
   (lambda (server)
     (start-server/channel-test server))

   ;; client
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (make-channel session)))
       (channel-open-session channel)
       (not (port-closed? channel))))))

;; Client sends "ping" as a command to execute, server replies with "pong"
(test-assert-with-log "channel-request-exec"
  (run-client-test

   ;; server
   (lambda (server)
     (start-server/channel-test server))

   ;; client
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (make-channel session)))
       (channel-open-session channel)
       (channel-request-exec channel "ping")
       (let ((res (read-line channel)))
         (and res
              (string=? "pong" res)))))))

;; Client sends "uname" as a command to execute, server returns exit status 0.
(test-assert-with-log "channel-request-exec, exit status"
  (run-client-test

   ;; server
   (lambda (server)
     (start-server/channel-test server))

   ;; client
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (make-channel session)))
       (channel-open-session channel)
       (channel-request-exec channel "uname")
       (= (channel-get-exit-status channel) 0)))))


;; data transferring
;; FIXME: Probably these TCs can be implemented more elegantly.

(define (make-channel/dt-test session)
  (let ((c (make-channel session)))
    (channel-open-session c)
    c))


(test-assert-with-log "data transferring, string"
  (run-client-test

   ;; server
   (lambda (server)
     (start-server/dt-test server
                           (lambda (channel)
                             (let ((str (read-line channel)))
                               (write-line str channel)))))

   ;; client
   (lambda ()
     (let* ((session (make-session/channel-test))
            (channel (make-channel/dt-test session))
            (str "Hello Scheme World!"))
       (write-line str channel)
       (poll channel
             (lambda args
               (let ((res (read-line channel)))
                 (disconnect! session)
                 (equal? res str))))))))


(test-assert-with-log "data transferring, bytevector"
  (let ((vect-size 10)
        (vect-fill 10))

    (run-client-test

     ;; server
     (lambda (server)
       (start-server/dt-test server
                             (lambda (channel)
                               (let ((v (get-bytevector-n channel vect-size)))
                                 (put-bytevector channel v)))))

     ;; client
     (lambda ()
       (let* ((session (make-session/channel-test))
              (channel (make-channel/dt-test session))
              (vect    (make-bytevector vect-size vect-fill)))
         (put-bytevector channel vect)
         (poll channel
               (lambda args
                 (let ((res (get-bytevector-n channel vect-size)))
                   (equal? res vect)))))))))


;;;

(test-end "client-server")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; client-server.scm ends here.
