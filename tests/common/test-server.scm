(use-modules (ice-9 rdelim)
             (tests common)
             (ssh channel)
             (ssh session)
             (ssh auth)
             (ssh log)
             (ssh server)
             (ssh popen))

(define (main args)
  (let ((f (open-output-file "/tmp/b.txt")))
    (display args f)
    (newline f)
    (close f))
  (let ((test-name (list-ref args 1)))
    (setup-test-suite-logging! test-name))
  (let* ((port      (string->number (list-ref args 2)))
         (handler   (eval-string (list-ref args 3)))
         (s         (make-server
                     #:bindaddr %addr
                     #:bindport port
                     #:rsakey   %rsakey
                     #:dsakey   %dsakey
                     #:log-verbosity 'functions)))
    (server-listen s)
    (handler s)))

