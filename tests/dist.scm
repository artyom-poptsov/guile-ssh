;;; dist.scm -- Testing of the distributed forms

;; Copyright (C) 2015, 2016, 2017, 2018, 2019, 2020 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (ice-9 receive)
             (ice-9 rdelim)
             (ssh  session)
             (ssh  key)
             (ssh  auth)
             (ssh  message)
             (ssh  server)
             (ssh  log)
             (ssh  dist)
             (ssh  dist job)
             (ssh  dist node)
             (tests common))

(set-log-verbosity! 'functions)


(test-begin-with-log "dist")

;;;


(test-assert-with-log "make-node"
  (run-client-test
   ;; Server
   (lambda (server)
     (start-server/exec server (const #t)))
   ;; Client
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (let ((n (make-node session)))
          (and n (eq? (node-session n) session))))))))


(test-equal "split, 1"
  '((a b) (c d) (e f g))
  (split '(a b c d e f g) 3))

(test-equal "split, 2"
  '((a))
  (split '(a) 2))


(test-assert-with-log "make-job"
  (run-client-test
   ;; Server
   (lambda (server)
     (start-server/exec server (const #f)))
   ;; Client
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (let* ((node (make-node session))
               (data '(1 2 3))
               (proc '(lambda (n) (1+ n)))
               (j (make-job 'map node data proc)))
          (and (eq? (job-type j) 'map)
               (eq? (job-node j) node)
               (eq? (job-data j) data)
               (eq? (job-proc j) proc))))))))


(test-assert-with-log "set-job-node"
  (run-client-test
   ;; Server
   (lambda (server)
     (start-server/exec server (const #t)))
   ;; Client
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (let* ((node (make-node session))
               (data '())
               (proc '(lambda (n) (1+ n)))
               (j1   (make-job 'map #f data proc))
               (j2   (set-job-node j1 node)))
          (and (not (eq? j1 j2))
               (eq? (job-type j1) (job-type j2))
               (eq? (job-node j1) #f)
               (eq? (job-node j2) node)
               (eq? (job-data j1) (job-data j2))
               (eq? (job-proc j1) (job-proc j2)))))))))

(test-error-with-log "hand-out-job, invalid type"
  (run-client-test
   ;; server
   (lambda (server)
     (start-server/exec server (const #t)))
   ;; client
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (let ((n (make-node session)))
          (hand-out-job (make-job 'invalid-job n '() (const #t)))))))))


(test-assert-with-log "assign-eval"
  (run-client-test
   ;; server
   (lambda (server)
     (start-server/exec server (const #t)))
   ;; client
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (let* ((nodes (make-list 2 (make-node session)))
               (exprs (make-list 10 '(lambda (x) (1+ x))))
               (jobs  (assign-eval nodes exprs)))
          (and (eq? (length jobs) 2)
               (eq? (job-type (car jobs)) 'eval)
               (eq? (length (job-proc (car jobs))) 5))))))))


;;; Testing of 'rrepl-get-result'.
;; These test cases are intended to test various inputs for 'rrepl-get-result'
;; procedure.

(test-assert "rrepl-get-result"
  (receive (result eval-num module-name lang)
      (call-with-input-string "scheme@(guile-user)> $0 = test"
                              rrepl-get-result)
    (and (eq?      result      'test)
         (=        eval-num    0)
         (string=? module-name "(guile-user)")
         (string=? lang        "scheme"))))

(test-assert "rrepl-get-result, unspecified"
  (receive (result eval-num module-name lang)
      (call-with-input-string "scheme@(guile-user)> "
                              rrepl-get-result)
    (and (eq?      result      *unspecified*)
         (eq?      eval-num    *unspecified*)
         (string=? module-name "(guile-user)")
         (string=? lang        "scheme"))))

(test-error-with-log/= "rrepl-get-result, error"
  'node-repl-error "scheme@(guile-user)> ERROR: error."
  (call-with-input-string "scheme@(guile-user)> ERROR: error."
                          rrepl-get-result))

;; See <https://github.com/artyom-poptsov/guile-ssh/issues/3>.
(test-error-with-log/= "rrepl-get-result, compilation error"
  'node-repl-error "scheme@(guile-user)> While compiling expression:\nERROR: no code for module (module-that-doesnt-exist)"
  (call-with-input-string
   (string-append "scheme@(guile-user)> While compiling expression:\n"
                  "ERROR: no code for module (module-that-doesnt-exist)")
   rrepl-get-result))

(test-error-with-log/= "rrepl-get-result, unbound variable error"
  'node-repl-error "scheme@(guile-user)> ;;; socket:9:7: warning: \
possibly unbound variable `e'\nsocket:9:7: In procedure #<procedure \
1a44920 at socket:9:7 ()>:\nsocket:9:7: In procedure module-lookup: \
Unbound variable: e"
  (call-with-input-string
   (string-append (string-append
                   "scheme@(guile-user)> ;;; socket:9:7: warning: "
                   "possibly unbound variable `e'\nsocket:9:7: "
                   "In procedure #<procedure 1a44920 at socket:9:7 ()>:\n"
                   "socket:9:7: In procedure module-lookup: Unbound variable: e"))
   rrepl-get-result))

;; Here we have to use regexps to match the error message because of
;; differences between Guile 3.0.7 and older versions.
;;
;; See <https://github.com/artyom-poptsov/guile-ssh/issues/28>
(test-error-with-log/match "rrepl-get-result, unknown # object error"
  'node-repl-error "Reader error: .+: #<unknown port>:1:3: \
Unknown # object: \\(.+\\): scheme@\\(guile-user\\)> \
\\$4 = #<session #<undefined>@#<undefined>:22 \\(disconnected\\) 453fff>"
  (call-with-input-string
   (string-append  "scheme@(guile-user)> $4 = "
                   "#<session #<undefined>@#<undefined>:22 (disconnected) 453fff>")
   rrepl-get-result))

(test-assert "rrepl-get-result, elisp"
  (receive (result eval-num module-name lang)
      (call-with-input-string "elisp@(guile-user)> $0 = #nil"
                              rrepl-get-result)
    (and (eq?      result      '#nil)
         (=        eval-num    0)
         (string=? module-name "(guile-user)")
         (string=? lang        "elisp"))))

(test-assert "rrepl-get-result, multiple values"
  (receive (result eval-num module-name lang)
      (call-with-input-string "scheme@(guile-user)> $0 = v1\n$1 = v2"
                              rrepl-get-result)
    (and (vector? eval-num)
         (vector? result)
         (eq?      (vector-ref result 0)   'v1)
         (eq?      (vector-ref result 1)   'v2)
         (=        (vector-ref eval-num 0) 0)
         (=        (vector-ref eval-num 1) 1)
         (string=? module-name "(guile-user)")
         (string=? lang        "scheme"))))


(test-assert "rrepl-skip-to-prompt, valid input"
  (begin
    (call-with-input-string "Enter `,help' for help."
      (lambda (port)
        (rrepl-skip-to-prompt port)))
    #t))

(test-error-with-log "rrepl-skip-to-prompt, invalid input" 'node-error
  (call-with-input-string "invalid input"
                          (lambda (port)
                            (rrepl-skip-to-prompt port))))

(test-assert-with-log "node-guile-version, valid response"
  (run-client-test
   ;; Server
   (lambda (server)
     (start-server/exec server (const #t)))
   ;; Client
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (format-log/scm 'nolog "client" "session: ~a" session)
        (let ((n (make-node session)))
          (string=? (node-guile-version n)
                    "GNU Guile 2.2.3")))))))


;;; Distributed forms.

;; The client uses distributed form 'with-ssh' to evaluate (+ 21 21).  The
;; server pretends to be a RREPL server and returns the evaluation "result",
;; 42.
(test-equal-with-log "with-ssh"
  42
  (run-client-test
   ;; server
   (lambda (server)
     (start-server/exec server (lambda (session message channel)
                                 (let ((line (read-line channel)))
                                   (format-log/scm 'nolog "with-ssh"
                                                   "client request: ~A"
                                                   line)
                                   (write-line "$1 = 42\n" channel)))))
   ;; client
   (lambda ()
     (call-with-connected-session/shell
      (lambda (session)
        (format-log/scm 'nolog "client" "session: ~a" session)
        (let ((n (make-node session)))
          (with-ssh n
            (+ 21 21))))))))

;;;


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "dist")

(exit (= 0 exit-status))

;;; dist.scm ends here.
