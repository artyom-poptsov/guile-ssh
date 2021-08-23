(use-modules (ice-9 rdelim)
             (tests common)
             (ssh auth)
             (ssh channel)
             (ssh dist)
             (ssh key)
             (ssh message)
             (ssh popen)
             (ssh server)
             (ssh session)
             (ssh tunnel)
             (ssh log)
             (ssh server))

(define (main args)
  (let ((test-suite-name (list-ref args 1))
        (test-name       (list-ref args 2)))

    (define (log message)
      (let ((port (open-file (string-append test-suite-name "/log.txt") "a+")))
        (format port
                "~s: ~a~%"
                (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time)))
                message)
        (close port)))

    (unless (file-exists? test-suite-name)
      (mkdir test-suite-name))

    (log args)

    (set-log-userdata! test-name)
    (setup-test-suite-logging! (string-append test-suite-name "/" test-name))
    (let* ((port      (string->number (list-ref args 3)))
           (handler   (eval-string (list-ref args 4)))
           (s         (make-server
                       #:bindaddr %addr
                       #:bindport port
                       #:rsakey   %rsakey
                       #:dsakey   %dsakey
                       #:log-verbosity 'functions)))
      (server-listen s)
      (let ((p (open-output-file (format #f
                                         "~a/~a.run"
                                         test-suite-name
                                         test-name))))
        (format p "~a~%" (getpid))
        (close p))
      (usleep 100)
      (handler s))))
