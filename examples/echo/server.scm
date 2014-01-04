#!/usr/bin/guile \
--debug -e main -s
!#

(use-modules (ice-9 rdelim)
             (ice-9 popen)
             (ssh server)
             (ssh message)
             (ssh session)
             (ssh channel)
             (ssh key)
             (ssh auth))                ; userauth-*

(define *default-bindport*      12345)
(define *default-log-verbosity* 0)
(define *default-rsakey*        (string-append (getenv "HOME")
                                               "/.ssh/id_rsa"))

(define (handle-req-auth session msg msg-type)
  (let ((subtype (cadr msg-type)))

    (format #t "  subtype: ~a~%" subtype)

    ;; Allowed authentication methods
    (message-auth-set-methods! msg '(public-key))

    (case subtype
      ((auth-method-publickey)
       (let* ((req          (message-get-req msg))
              (user         (auth-req:user req))
              (pubkey       (auth-req:pubkey req))
              (pubkey-state (auth-req:pubkey-state req)))
         (format #t
                 (string-append "  User ~a wants to authenticate with a public key (~a)~%"
                                "  Public key state: ~a~%")
                 user (get-key-type pubkey) pubkey-state)

         (case pubkey-state
           ((none)
            (message-auth-reply-public-key-ok msg))

           ((valid)
            (message-reply-success msg))

           (else
            (format #t "  Bad public key state: ~a~%" pubkey-state)
            (message-reply-default msg)))))

      (else
       (message-reply-default msg)))))

(define (handle-req-channel-open msg msg-type)
  (let ((subtype (cadr msg-type)))
    (format #t "  subtype: ~a~%" subtype)
    (case subtype
      ((channel-session)
       (message-channel-request-open-reply-accept msg))
      (else
       (message-reply-default msg)
       #f))))

(define (handle-req-channel msg msg-type channel)
  (let ((subtype (cadr msg-type)))

    (format #t "  subtype: ~a~%" subtype)

    (case subtype

      ((channel-request-env)
       (let* ((env-req (message-get-req msg))
              (name    (env-req:name env-req))
              (value   (env-req:value env-req)))
         (format #t
                 (string-append "  env requested:~%"
                                "    name:  ~a~%"
                                "    value: ~a~%")
                 name value)
         (setenv name value)
         (message-reply-success msg)))

      (else
       (message-reply-success msg)))))

(define (main args)
  (let ((server (make-server #:bindport      *default-bindport*
                             #:rsakey        *default-rsakey*
                             #:log-verbosity *default-log-verbosity*
                             #:banner        "Scheme Secure Shell Daemon"))
        (channel #f))

    (format #t (string-append
                "Using private key ~a~%"
                "Listening on port ~a~%")
            *default-rsakey*
            *default-bindport*)

    ;; Start listen to incoming connections.
    (server-listen server)

    ;; Accept new connections from clients.  Every connection is
    ;; handled in its own SSH session.
    (let main-loop ((session (server-accept server)))
      (display "Client accepted.\n")
      (server-handle-key-exchange session)
      ;; Handle messages from the connected SSH client.
      (let session-loop ((msg (server-message-get session)))
        (if msg
            (let ((msg-type (message-get-type msg)))
              (format #t "Message: ~a~%" msg-type)
              ;; Check the type of the message
              (case (car msg-type)
                ((request-service)
                 (let ((srv-req (message-get-req msg)))
                   (format #t "  Service requested: ~a~%"
                           (service-req:service srv-req))
                   (message-reply-success msg)))

                ((request-auth)
                 (handle-req-auth session msg msg-type))

                ((request-channel-open)
                 (set! channel (handle-req-channel-open msg msg-type))
                 (let poll ((count #f))
                   (if (or (not count) (zero? count))
                       (poll (channel-poll channel #f))
                       (let ((str (read-line channel)))
                         (format #t "Received message: ~a~%" str)
                         (display "Echoing back...\n")
                         (display str channel))))
                 (close channel))

                ((request-channel)
                 (handle-req-channel msg msg-type channel))

                (else
                 (display "Reply default\n")
                 (message-reply-default msg)))))
        (if (connected? session)
            (session-loop (server-message-get session))))
      (disconnect! session)
      (main-loop (server-accept server)))))

;;; server.scm ends here.
