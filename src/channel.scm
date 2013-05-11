;;; channel.scm -- 

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

(define-module (ssh channel)
  #:use-module (ssh session)
  #:export (ssh:channel
            ssh:make-channel
            ssh:close-channel!
            ssh:channel-request-exec
            ssh:channel-poll
            ssh:channel-read
            ssh:channel-open?
            ssh:channel-eof?))

(load-extension "libguile-ssh" "init_channel_type")

;;; channel.scm ends here.
