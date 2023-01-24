;;; channel-leak.scm -- Check if libssh channels are not freed corretly.
;;
;; This test checks if the channel are properly freed; otherwise the OpenSSH
;; server will report "no more sessions" error when the maximum number of
;; sessions per a TCP connection (as specified by "MaxSessions" option) is
;; exhausted.
;;
;; Reported by Andrew Tropin <andrew@trop.in> in
;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=58290>
;;
;; The test for reproducing the problem was provided by Ludovic Courtès
;; <ludo@gnu.org>.  This file contains its code with slight changes.

(use-modules (ssh session)
             (ssh popen)
             (ssh auth)
             (ssh log)
             (rnrs io ports))

(define session
  (make-session #:host "localhost"))

(define (main args)
  (session-parse-config! session)
  (connect! session)
  (userauth-public-key/auto! session)
  (set-log-verbosity! 'functions)

  (let loop ((i 0))
    (format (current-error-port) "-- ~a --~%" i)
    (let ((pipe (open-remote-pipe session "date" "r")))
      (pk 'x (get-string-all pipe))
      (close-port pipe)
      (loop (+ 1 i)))))

;;; channel-leak.scm ends here.
