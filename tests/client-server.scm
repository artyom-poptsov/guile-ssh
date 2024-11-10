;;; client-server.scm -- Guile-SSH client is SUT.

;; Copyright (C) 2014, 2015, 2016, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (srfi srfi-26)
             (ice-9 threads)
             (ice-9 rdelim)
             (ice-9 regex)
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
             (ssh version)
             (srfi srfi-4)
             (tests common))

(test-begin-with-log "client-server")


;;; Global symbols

(define topdir (getenv "abs_top_srcdir"))

(define log    (test-runner-aux-value (test-runner-current)))


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
   (lambda (server)
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)))
   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (and (connected? session)
             (begin
               (disconnect! session)
               (not (connected? session)))))))))

(test-equal-with-log "get-protocol-version"
  2
  (run-client-test
   ;; server
   (lambda (server)
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)))
   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (let ((version (get-protocol-version session)))
          (disconnect! session)
          version))))))

(test-assert-with-log "authenticate-server, not-known"
  'not-known
  (run-client-test
   ;; server
   (lambda (server)
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)))
   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
       (authenticate-server session))))))

(test-equal-with-log "authenticate-server, ok"
  'ok
  (run-client-test
   ;; server
   (lambda (server)
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)))
   ;; client
   (lambda ()
     (let ((res (call-with-connected-session
                 (lambda (session)
                   (write-known-host! session)
                   (authenticate-server session)))))
       (delete-file %knownhosts)
       res))))

(test-assert-with-log "get-public-key-hash"
  (run-client-test

   ;; server
   (lambda (server)
     (let ((s (server-accept server)))
       (server-handle-key-exchange s)))

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


;;;
;;; Authentication
;;;


;;; 'userauth-none!'

;; The procedure called with a wrong object as a parameter which leads to an
;; exception.
(test-error-with-log "userauth-none!, session: non-session object" 'wrong-type-arg
  (userauth-none! "Not a session."))

;; Client tries to authenticate using a non-connected session which leads to
;; an exception.
(test-error-with-log "userauth-none!, session: non-connected session" 'wrong-type-arg
  (userauth-none! (make-session-for-test)))


;; Server replies with "success", client receives 'success.
(test-equal-with-log "userauth-none!, session: connected session"
  'success
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (start-session-loop session
                           (lambda (msg)
                             (message-auth-set-methods! msg '(none))
                             (message-reply-success msg)))))

   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (authenticate-server session)
        (userauth-none! session))))))


;; Server replies with "default", client receives 'denied.
(test-equal-with-log "userauth-none!, denied"
  'denied
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (start-session-loop session
                           (lambda (msg)
                             (message-auth-set-methods! msg '(public-key))
                             (message-reply-default msg)))))

   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (authenticate-server session)
        (userauth-none! session))))))


;; Server replies with "partial success", client receives 'partial.
(test-equal-with-log "userauth-none!, partial"
  'partial
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (start-session-loop session
                           (lambda (msg)
                             (message-auth-set-methods! msg '(none))
                             (message-reply-success msg 'partial)))))

   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (authenticate-server session)
        (userauth-none! session))))))


;;; 'userauth-password!'

;; The procedure called with a wrong object as a parameter which leads to an
;; exception.
(test-error-with-log "userauth-password!, session: non-session object"
  'wrong-type-arg
  (userauth-password! "Not a session." "Password"))

;; Client tries to authenticate using a non-connected session which leads to
;; an exception.
(test-error-with-log "userauth-password!, session: non-connected session"
  'wrong-type-arg
  (userauth-password! (make-session-for-test) "Password"))

;; User tries to authenticate using a non-string object as a password. the
;; procedure raises an error.
(test-error-with-log "userauth-password!, password: non-string object"
  'wrong-type-arg
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (start-session-loop session
                           (lambda (msg)
                             (message-auth-set-methods! msg '(password))
                             (message-reply-success msg)))))

   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (userauth-password! session 123))))))


(test-equal-with-log "userauth-password!, success"
  'success
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (start-session-loop session
                           (lambda (msg)
                             (message-auth-set-methods! msg '(password))
                             (message-reply-success msg)))))

   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (authenticate-server session)
        (userauth-password! session "password"))))))


(test-equal-with-log "userauth-password!, denied"
  'denied
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (start-session-loop session
                           (lambda (msg)
                             (message-auth-set-methods! msg '(password))
                             (message-reply-default msg)))))

   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (authenticate-server session)
        (userauth-password! session "password"))))))


(test-equal-with-log "userauth-password!, partial"
  'partial
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (start-session-loop session
                           (lambda (msg)
                             (message-auth-set-methods! msg '(password))
                             (message-reply-success msg 'partial)))))

   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (authenticate-server session)
        (userauth-password! session "password"))))))


;;; 'userauth-public-key!'

;; The procedure called with a wrong object as a parameter which leads to an
;; exception.
(test-error-with-log "userauth-public-key!, wrong parameter" 'wrong-type-arg
  (userauth-public-key! "Not a session." (private-key-from-file %rsakey)))

;; Client tries to authenticate using a non-connected session which leads to
;; an exception.
(test-error-with-log "userauth-public-key!, non-connected session"
  'wrong-type-arg
  (userauth-public-key! (make-session-for-test)
                        (private-key-from-file %rsakey)))

;; Client tries to use a non-key object for authentication, the procedure
;; raises an exception.
(test-error-with-log "userauth-public-key!, private-key: non-key object"
  'wrong-type-arg
  (run-client-test
   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (start-session-loop session
                           (lambda (msg)
                             (message-reply-success msg)))))
   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (userauth-public-key! session "Non-key object."))))))

;; Client tries to use a public key for authentication, the procedure raises
;; an exception.
(test-error-with-log "userauth-public-key!, private-key: public key"
  'wrong-type-arg
  (run-client-test
   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (start-session-loop session
                           (lambda (msg)
                             (message-reply-success msg)))))
   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (userauth-public-key! session (public-key-from-file %rsakey-pub)))))))


(let* ((version (get-libssh-version))
       (version (map string->number (string-split version #\.))))
  (when (and (zero? (car version))
             (or (= (cadr version) 7)
                 (and (= (cadr version) 8)
                      (< (caddr version) 3))))
    ;; XXX: This test fails because of the problems with ECDSA public key
    ;; authentication in libssh versions prior 0.8.3.
    (test-skip "userauth-public-key!, success")))

(test-equal-with-log "userauth-public-key!, success"
  'success
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (start-session-loop session
                           (lambda (msg)
                             (message-reply-success msg)))))

   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (authenticate-server session)
        (let ((prvkey (private-key-from-file %ecdsakey)))
          (userauth-public-key! session prvkey)))))))

(test-equal-with-log "userauth-public-key!, success (RSA)"
  'success
  (run-client-test

   ;; server
   (lambda (server)
     (server-listen server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (start-session-loop session
                           (lambda (msg)
                             (message-reply-success msg)))))

   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (authenticate-server session)
        (let ((prvkey (private-key-from-file %rsakey)))
          (userauth-public-key! session prvkey)))))))


;;; 'userauth-public-key/auto!'

;; The procedure called with a wrong object as a parameter which leads to an
;; exception.
(test-error-with-log "userauth-public-key/auto!, session: non-session object"
  'wrong-type-arg
  (userauth-public-key/auto! "Not a session."))

;; Client tries to authenticate using a non-connected session which leads to
;; an exception.
(test-error-with-log "userauth-public-key/auto!, session: non-connected session"
  'wrong-type-arg
  (userauth-public-key/auto! (make-session-for-test)))


;;; 'userauth-gssapi!'

;; The procedure called with a wrong object as a parameter which leads to an
;; exception.
(test-error-with-log "userauth-gssapi!, wrong parameter" 'wrong-type-arg
  (userauth-gssapi! "Not a session."))

;; Client tries to authenticate using a non-connected session which leads to
;; an exception.
(test-error-with-log "userauth-gssapi!, not connected" 'wrong-type-arg
  (userauth-gssapi! (make-session-for-test)))


;;;


;; The procedure called with a wrong object as a parameter which leads to an
;; exception.
(test-error-with-log "userauth-get-list, wrong parameter" 'wrong-type-arg
  (userauth-get-list "Not a session."))

(test-error-with-log "userauth-get-list, non-connected" 'wrong-type-arg
  (userauth-get-list (make-session-for-test)))


;; Server replies "default" with the list of allowed authentication
;; methods.  Client receives the list.
(test-equal-with-log "userauth-get-list"
  '(password public-key)
  (run-client-test

   ;; server
   (lambda (server)
     (let ((session (server-accept server)))
       (server-handle-key-exchange session)
       (start-session-loop session
                           (lambda (msg)
                             (message-auth-set-methods! msg '(password public-key))
                             (message-reply-default msg)))))

   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (authenticate-server session)
        (userauth-none! session)
        (userauth-get-list session))))))


;;; Channel test

;; make, open, exec

;; TODO: Fix the bug: the procedure cannot be used to test errors.
(define (call-with-connected-session/channel-test proc)
  (define max-tries 30)
  (define (loop count)
    (catch #t
      (lambda ()
        (call-with-connected-session
         (lambda (session)
           (format-log/scm 'nolog
                           "call-with-connected-session/channel-test"
                           "connected in ~d tries: ~a" count session)
           (let ((result (authenticate-server session)))
             (format-log/scm 'nolog
                             "call-with-connected-session/channel-test"
                             "server authentication result: ~a" result)
             (when (equal? result 'error)
               (error "Could not authenticate server" session result)))
           (let ((result (userauth-none! session)))
             (format-log/scm 'nolog
                             "call-with-connected-session/channel-test"
                             "client authentication result: ~a" result))
             ;; (unless (equal? result 'ok)
             ;;   (error "Could not authenticate client" session result)))
           (proc session))))
      (lambda args
        (format-log/scm 'nolog
                        "make-session/channel-test"
                        "Unable to connect in ~d tries~%"
                        count)
        (sleep 1)
        (if (= count max-tries)
            (format-log/scm 'nolog
                            "make-session/channel-test"
                            "~a"
                            "Giving up ...")
            (loop (1+ count))))))
  (loop 1))


(test-assert-with-log "make-channel"
  (run-client-test

   ;; server
   (lambda (server)
     (start-server/exec server (const #t)))

   ;; client
   (lambda ()
     (call-with-connected-session/channel-test
      make-channel))))

(test-assert-with-log "channel-get-session"
  (run-client-test

   ;; server
   (lambda (server)
     (start-server/exec server (const #t)))

   ;; client
   (lambda ()
     (call-with-connected-session/channel-test
      (lambda (session)
        (let ((channel (make-channel session)))
          (eq? session (channel-get-session channel))))))))

(test-assert-with-log "channel-open-session"
  (run-client-test

   ;; server
   (lambda (server)
     (start-server/exec server (const #t)))

   ;; client
   (lambda ()
     (call-with-connected-session/channel-test
      (lambda (session)
        (format-log/scm 'nolog "channel-open-session [client]"
                        "session: ~a" session)
        (let ((channel (make-channel session)))
          (format-log/scm 'nolog "channel-open-session [client]"
                          "channel: ~a" channel)
          (channel-open-session channel)
          (format-log/scm 'nolog "channel-open-session [client]"
                          "channel 2: ~a" channel)
          (not (port-closed? channel))))))))

;; Client sends "ping" as a command to execute, server replies with "pong"
(test-assert-with-log "channel-request-exec"
  (run-client-test

   ;; server
   (lambda (server)
     (start-server/exec server (const #t)))

   ;; client
   (lambda ()
     (call-with-connected-session/channel-test
      (lambda (session)
        (let ((channel (make-channel session)))
          (channel-open-session channel)
          (channel-request-exec channel "ping")
          (let ((res (read-line channel)))
            (and res
                 (string=? "pong" res)))))))))

;; Client sends "uname" as a command to execute, server returns exit status 0.
(test-assert-with-log "channel-request-exec, exit status"
  0
  (run-client-test

   ;; server
   (lambda (server)
     (start-server/exec server (const #t)))

   ;; client
   (lambda ()
     (call-with-connected-session/channel-test
      (lambda (session)
        (let ((channel (make-channel session)))
          (channel-open-session channel)
          (channel-request-exec channel "exit status")
          (channel-get-exit-status channel)))))))

(test-assert-with-log "channel-request-exec, printing a freed channel"
  (run-client-test

   ;; server
   (lambda (server)
     (start-server/exec server (const #t)))

   ;; client
   (lambda ()
     (call-with-connected-session/channel-test
      (lambda (session)
        (let ((channel (make-channel session)))
          (format-log/scm 'nolog "channel-request-exec, printing a freed channel"
                          "channel 0: ~a" channel)
          (channel-open-session channel)
          (format-log/scm 'nolog "channel-request-exec, printing a freed channel"
                          "channel 1: ~a" channel)
          (channel-request-exec channel "exit status")
          (format-log/scm 'nolog "channel-request-exec, printing a freed channel"
                          "channel 2: ~a" channel)
          (close channel)
          (format-log/scm 'nolog "channel-request-exec, printing a freed channel"
                          "channel: ~a" channel)
          (string-match "#<unknown channel \\(freed\\) [0-9a-f]+>"
                        (object->string channel))))))))

(test-error-with-log "channel-get-exit-status, freed channel"
  'wrong-type-arg
  (run-client-test
   ;; server
   (lambda (server)
     (start-server/exec server (const #t)))
   ;; client
   (lambda ()
     (call-with-connected-session
      (lambda (session)
        (authenticate-server session)
        (userauth-none! session)
        (let ((channel (make-channel session)))
          (channel-open-session channel)
          (channel-request-exec channel "exit status")
          (close channel)
          (channel-get-exit-status channel)))))))


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
     (call-with-connected-session/channel-test
      (lambda (session)
        (let ((channel (make-channel/dt-test session))
              (str "Hello Scheme World!"))
          (write-line str channel)
          (poll channel
                (lambda args
                  (let ((res (read-line channel)))
                    (disconnect! session)
                    (equal? res str))))))))))


(test-assert-with-log "data transferring, bytevector"
  (run-client-test

   ;; server
   (lambda (server)
     (use-modules (rnrs bytevectors)
                  (rnrs io ports))
     (start-server/dt-test server
                           (lambda (channel)
                             (let ((v (get-bytevector-n channel 10)))
                               (put-bytevector channel v)))))

   ;; client
   (lambda ()
     (call-with-connected-session/channel-test
      (lambda (session)
        (let* ((vect-size 10)
               (channel   (make-channel/dt-test session))
               (vect      (make-bytevector vect-size 42)))
          (format-log/scm 'nolog
                          "data transferring, bytevector"
                          "vect: ~a" vect)
          (put-bytevector channel vect)
          (poll channel
                (lambda args
                  (let ((res (get-bytevector-n channel vect-size)))
                    (format-log/scm 'nolog
                                    "data transferring, bytevector"
                                    "res: ~a" res)
                    (equal? res vect))))))))))

;; This test checks if the client side is able to handle unexpected close of
;; the channel from the remote side.
;;
;; When a client tries to read from a channel that is abruptly closed by the
;; remote side (e.g. when a remote side is crashed) the reading procedure must
;; return EOF without issuing an error or a segmentation fault.
(test-assert-with-log "data transferring, remote side abruptly closed"
  (run-client-test

   ;; server
   (lambda (server)
     (use-modules (rnrs bytevectors)
                  (rnrs io ports))
     (start-server/dt-test server
                           (lambda (channel)
                             (let ((v (get-bytevector-n channel 10)))
                               (put-bytevector channel v)
                               (kill (getpid) SIGKILL)))))

   ;; client
   (lambda ()
     (call-with-connected-session/channel-test
      (lambda (session)
        (let* ((vect-size 10)
               (channel   (make-channel/dt-test session))
               (vect      (make-bytevector vect-size 42)))
          (put-bytevector channel vect)
          (let loop ((data   '())
                     (result (get-bytevector-n channel (/ vect-size 2))))
            (if (eof-object? result)
                data
                (catch #t
                  (lambda ()
                    (loop (cons result data)
                          (get-bytevector-n channel (/ vect-size 2))))
                  (lambda err
                    (format-log/scm 'nolog
                                    "data transferring, remote side abruptly closed"
                                    "err: ~a" err)
                    #f))))))))))


;;;
;;; Channels
;;;

;; Client opens a channel to a server, sends data and then sends EOF on the
;; channel.  Server reads data and sends it back.  Client checks if the
;; channel is closed for output, and reads the data.
(test-assert-with-log "channel-send-eof"
  (run-client-test
   (lambda (server)
     (start-server/dt-test server
                           (lambda (channel)
                             (let ((str (read-line channel)))
                               (write-line str channel)))))
   (lambda ()
     (call-with-connected-session/channel-test
      (lambda (session)
        (let ((channel (make-channel/dt-test session))
              (str     "Hello Scheme World!"))
          (write-line str channel)
          (channel-send-eof channel)
          (and (input-port? channel)
               (not (output-port? channel))
               (string=? (read-line channel) str))))))))


;;;

(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "client-server")

(exit (= 0 exit-status))

;;; client-server.scm ends here.
