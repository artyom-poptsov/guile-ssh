;;; dist.scm -- Spirit of disrtibuted computing for Scheme.

;; Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
;; along with Guile-SSH.  If not, see
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This module contains disrtibuted forms of some useful procedures such as
;; `map'.


;;; Code:

(define-module (ssh dist)
  #:use-module (ssh   channel)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 rdelim)
  #:export (dist-map

            ;; Low-level procedures
            %send-message %recv-message
            %dist-job %handle-job))

(define %delimiter "\0")


(define (%send-message message channel)
  "Send MESSAGE to CHANNEL."
  (write message channel)
  (write-char #\nul channel))

(define (%recv-message channel)
  "Receive server response from the CHANNEL."
  (read-delimited %delimiter channel))


(define (%dist-job channel quoted-proc lst)
  "Distribute computation job to a job server over the CHANNEL."
  (%send-message (list quoted-proc lst) channel))

(define (%handle-job channel)
  "Receive and handle distributed job from CHANNEL."
  (let* ((data (read (open-input-string (%recv-message channel))))
         (proc (primitive-eval (car data))))
    (format #t "proc: ~a; list: ~a~%" proc (cadr data))
    (%send-message (map proc (cadr data)) channel)))


(define-syntax dist-map
  (syntax-rules ()
    "Distributed version of the `map' high-order procedure."
    ((_ session proc arg)
     (let ((channel     (make-channel session))
           (split-point (round (/ (length arg) 2))))

       (channel-open-session channel)

       (%dist-job channel (quote proc) (list-tail arg split-point))

       (let ((res     (map proc (list-head arg split-point)))
             (job-res (read (open-input-string (%recv-message channel)))))
         (append res job-res))))))

;;; dist.scm ends here


