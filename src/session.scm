;;; session.scm -- 

;; Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;; 
;; 


;;; Commentary:

;; 
;; 
;; These methods are exported:
;; 
;;   


;;; Code:

(define-module (ssh session)
  #:export (ssh:session
            ssh:make-session
            ssh:blocking-flush!
            ssh:session-set!
            ssh:get-version
            ssh:connect!
            ssh:disconnect!
            ssh:connected?))

(load-extension "libguile-ssh" "init_session_type")

;;; session.scm ends here
