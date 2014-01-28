;; Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This file is a part of libguile-ssh.
;;
;; libguile-ssh is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; libguile-ssh is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with libguile-ssh.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (srfi srfi-64)
             (ice-9 threads)
             (ssh server)
             (ssh session))

(test-begin "client-server")

(test-assert "connect"
  (let* ((addr      "127.0.0.1")
         (port      12345)
         (topdir    (getenv "abs_top_srcdir"))
         (rsakey    (format #f "~a/tests/rsakey" topdir))
         (tries-max 10)
         (log       (test-runner-aux-value (test-runner-current)))
         (mtx       (make-mutex))
         (server-thread (lambda (srv)
                          (let ((s (server-accept srv)))
                            (lock-mutex mtx)
                            (session? s)
                            (unlock-mutex mtx)))))

    (let ((session (make-session
                    #:host    addr
                    #:port    port
                    #:timeout 10        ;seconds
                    #:user    "Random J. User"
                    #:log-verbosity 'nolog))
          (server  (make-server
                    #:bindaddr addr
                    #:bindport port
                    #:rsakey   rsakey
                    #:log-verbosity 'nolog)))

      (server-listen server)

      (lock-mutex mtx)

      (make-thread server-thread server)

      (do ((try 1 (1+ try)))
          ((or (> try tries-max)
               (connected? session)))
        (sleep 1)
        (catch #t
          (lambda ()
            (connect! session)
            (format log "  connected on ~a try~%" try))
          (lambda (key . args)
            #f)))
      (let ((res (connected? session)))
        (unlock-mutex mtx)
        res))))

(test-end "client-server")

(exit (= (test-runner-fail-count (test-runner-current)) 0))


