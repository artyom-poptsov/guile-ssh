;;; node.scm -- Distributed computing node

(define-module (ssh dist node)
  #:use-module (ice-9 rdelim)
  #:use-module (ssh session)
  #:use-module (ssh server)
  #:use-module (ssh message)
  #:use-module (ssh session)
  #:use-module (ssh channel)
  #:use-module (ssh key)
  #:use-module (ssh dist)
  #:export (node?
            make-node
            run-node))

(define <node>
  (make-vtable "pw"
               (lambda (struct port)
                 (let* ((server   (struct-ref struct 0))
                        (bindaddr (server-get server 'bindaddr))
                        (bindport (server-get server 'bindport)))
                   (format port "#<node ~a:~a>"
                           (if bindaddr
                               bindaddr
                               "")
                           bindport)))))

(define (node? x)
  "Check if X is a <node> instance."
  (and (struct? x)
       (eq? (struct-vtable x) <node>)))

(define* (make-node #:key (white-list '())
                          (black-list '())
                          (bindaddr   #f)
                          (bindport   2223)
                          (rsakey     #f)
                          (dsakey     #f)
                          (log-verbosity 'nolog))
  (let ((server (make-server #:bindaddr      bindaddr
                             #:bindport      bindport
                             #:rsakey        rsakey
                             #:dsakey        dsakey
                             #:log-verbosity log-verbosity)))
    (make-struct/no-tail <node> server)))

(define (accept-and-catch server)
  "Accept new client connection to a SERVER.  Catch errors, return #f on
error."
  (catch 'guile-ssh-error
    (lambda ()
      (server-accept server))
    (lambda (key . args)
      (format #t "~a: ~a~%" key args)
      #f)))

(define (read-all port)
  "Read all lines from the PORT."
  (let r ((res (read-line port 'concat))
          (str ""))
    (if (and (not (eof-object? str)) (char-ready? port))
        (r (string-append res str) (read-line port 'concat))
        res)))

(define (handle-req-channel-open msg msg-type)
  (let ((subtype (cadr msg-type)))
    (format #t "  subtype: ~a~%" subtype)
    (case subtype
      ((channel-session)
       (message-channel-request-open-reply-accept msg))
      (else
       (message-reply-default msg)
       #f))))

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

(define (run-session-loop session)
  (display "Session loop\n")
  (while #t
    (let ((msg (server-message-get session)))
      (format #t "Message: ~a~%" msg)
      (and msg
           (let ((msg-type (message-get-type msg)))
             (format #t "Message: ~a~%" msg-type)
             (case (car msg-type)
               ((request-service)
                (message-reply-success msg))
               ((request-auth)
                (handle-req-auth session msg msg-type))
               ((request-channel-open)
                (let* ((channel (handle-req-channel-open msg msg-type)))
                  (%handle-job channel)
                  (close channel)
                  (message-reply-success msg)
                  (break)))
               (else
                (message-reply-default msg))))))))

(define (run-node node)
  (let ((server (struct-ref node 0)))
    (server-listen server)
    (while #t
      (let ((session (accept-and-catch server)))
        (format #t "Session: ~a~%" session)
        (or session
            (begin
              (sleep 1)
              (continue)))
        (server-handle-key-exchange session)
        (run-session-loop session)
        (disconnect! session)))))

;;; node.scm ends here
