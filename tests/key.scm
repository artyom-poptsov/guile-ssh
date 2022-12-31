;;; key.scm -- Testing of Guile-SSH keys

;; Copyright (C) 2014-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (ssh key)
             (ssh version)
             (tests common))

;;;


;; ECDSA doesn't work if libssh 0.6.3 was compiled GCrypt
(define %openssl? (eq? (get-crypto-library) 'openssl))
(define-syntax-rule (when-openssl test)
  (or (not %openssl?)
      test))

(define-syntax-rule (unless-openssl expr)
  (or %openssl?
      expr))

(define-syntax-rule (unless-dsa-supported expr)
  (unless (dsa-support?)
    expr))

(test-begin-with-log "key")

(test-assert-with-log "public-key-from-file: RSA"
  (public-key-from-file %rsakey-pub))

(unless-dsa-supported
 (test-skip "public-key-from-file: DSA"))
(test-assert-with-log "public-key-from-file: DSA"
  (public-key-from-file %dsakey-pub))

(unless-openssl
 (test-skip "public-key-from-file: ECDSA"))
(test-assert-with-log "public-key-from-file: ECDSA"
  (public-key-from-file %ecdsakey-pub))

(test-assert "private-key-from-file: RSA"
  (private-key-from-file %rsakey))

(unless-dsa-supported
 (test-skip "private-key-from-file: DSA"))
(test-assert "private-key-from-file: DSA"
  (private-key-from-file %dsakey))

(unless-openssl
 (test-skip "private-key-from-file: ECDSA"))
(test-assert "private-key-from-file: ECDSA"
  (private-key-from-file %ecdsakey))

(define *rsa-pub-key*   (public-key-from-file %rsakey-pub))
(define *dsa-pub-key*   (public-key-from-file %dsakey-pub))
(define *ecdsa-pub-key* (when-openssl
                         (public-key-from-file %ecdsakey-pub)))

(test-equal "key?: not a key"
  #f
  (key? "not a key"))

(test-assert "key?: RSA"
  (key? (private-key-from-file %rsakey)))

(unless-dsa-supported
 (test-skip "key?: DSA"))
(test-assert "key?: DSA"
  (key? (private-key-from-file %dsakey)))

(unless-openssl
 (test-skip "key?: ECDSA"))
(test-assert "key?: ECDSA"
  (key? (private-key-from-file %ecdsakey)))

(test-assert "key?: RSA (public)"
  (key? *rsa-pub-key*))

(unless-dsa-supported
 (test-skip "key?: DSA (public)"))
(test-assert "key?: DSA (public)"
  (key? *dsa-pub-key*))

(unless-openssl
 (test-skip "key?: ECDSA (public)"))
(test-assert "key?: ECDSA (public)"
  (key? *ecdsa-pub-key*))

(test-assert "private-key?: RSA"
  (private-key? (private-key-from-file %rsakey)))

(test-equal "private-key?: RSA (public)"
  #f
  (private-key? *rsa-pub-key*))

(test-equal "private-key?: not a key"
  #f
  (private-key? "not a key"))

(test-assert "public-key?: RSA (public)"
  (public-key? *rsa-pub-key*))

(test-assert "public-key?: RSA"
  (public-key? (private-key-from-file %rsakey)))

(test-equal "public-key?: not a key"
  #f
  (public-key? "not a key"))

(test-assert-with-log "private-key->public-key: RSA"
  (private-key->public-key (private-key-from-file %rsakey)))

(unless-dsa-supported
 (test-skip "private-key->public-key: DSA"))
(test-assert-with-log "private-key->public-key: DSA"
  (private-key->public-key (private-key-from-file %dsakey)))

(unless-openssl
 (test-skip "private-key->public-key: ECDSA"))
(test-assert-with-log "private-key->public-key: ECDSA"
  (private-key->public-key (private-key-from-file %ecdsakey)))


(test-assert-with-log "get-key-type: RSA"
  (equal? (eq? 'rsa (get-key-type (private-key-from-file %rsakey)))))

(unless-dsa-supported
 (test-skip "get-key-type: DSA"))
(test-assert-with-log "get-key-type: DSA"
  (equal? (eq? 'rsa (get-key-type (private-key-from-file %dsakey)))))

(unless-openssl
 (test-skip "get-key-type: ECDSA"))
(test-assert-with-log "get-key-type: ECDSA"
  (let ((key (private-key-from-file %ecdsakey)))
    (or (eq? 'ecdsa-p256 (get-key-type key))
        ;; For libssh versions prior to 0.9
        (eq? 'ecdsa (get-key-type key)))))


(unless-openssl
 (test-skip "private-key-to-file"))
(test-assert-with-log "private-key-to-file"
  (let ((file-name "./tmp-rsa-key"))
    (private-key-to-file (private-key-from-file %rsakey) file-name)
    (let ((key (private-key-from-file file-name)))
      (delete-file file-name)
      (and (key? key)
           (private-key? key)))))


;;; Converting between strings and keys

(define %rsakey-pub-string
  "AAAAB3NzaC1yc2EAAAADAQABAAABAQC+8H9j5Yt3xeqaAxXAtSbBsW0JsJegngwfLveHA0ev3ndEKruylR6CZgf6OxshTwUeBaqn7jJMf+6RRQPTcxihgtZAfdyKdPGWDtmePBnG64+uGEaP8N3KvCzlANKf5tmxS8brJlQhxKL8t+3IE8w3QmCMnCGKWprsL/ygPA9koWauUqqKvOQbZXdUEfLvZfnsE1laRyK4dwLiiM2vyGZM/2yePLP4xYu/uYdPFaukxt3DMcgrEy9zuVcU8wbkJMKM57sambvituzMVVqRdeMX9exZv32qcXlpChl4XjFClQ0lqOb8S8CNTPXm3zQ2ZJrQtUHiD54RYhlXD7X0TO6v")
(define %dsakey-pub-string
  "AAAAB3NzaC1kc3MAAACBAOpnJ64w3Qo3HkCCODTPpLqPUrDLg0bxWdoae2tsXFwhBthIlCV8N0hTzOj1Qrgnx/WiuDk5qXSKOHisyqVBv8sGLOUTBy0Fdz1SobZ9+WGu5+5EiJm78MZcgtHXHu1GPuImANifbSaDJpIGKItq0V5WhpLXyQC7o0Vt70sGQboVAAAAFQDeu+6APBWXtqq2Ch+nODn7VDSIhQAAAIA5iGHYbztSq8KnWj1J/6GTvsPp1JFqZ3hFX5wlGIV4XxBdeEZnCPrhYJumM7SRjYjWMpW5eqFNs5o3d+rJPFFwDo7yW10WC3Bfpo5xRxU35xf/aFAVbm3vi/HRQvv4cFrwTLvPHgNYGYdZiHXCXPoYIh+WoKT9n3MfrBXB4hpAmwAAAIEArkWuRnbjfPVFpXrWGw6kMPVdhOZr1ghdlG5bY31y4UKUlmHvXx5YZ776dSRSMJY2u4lS73+SFgwPdkmpgGma/rZdd9gly9T7SiSr/4qXJyS8Muh203xsAU3ukRocY8lsvllKEGiCJmrUTJWmj0UYEDsbqy2k/1Yz2Q/awygyk9c=")
(define %ecdsakey-pub-string
  "AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHcpje/fp21KjuZFKgmKAAwHeYJ6e3ny4LwEVjZr8hOCVlBvqj7/krVqxbwZI7EcowbpYI1F8ZszS7zfUhKT3U4=")


(test-equal "public-key->string, RSA"
  (public-key->string *rsa-pub-key*)
  %rsakey-pub-string)

(unless-dsa-supported
 (test-skip "public-key->string, DSA"))
(test-equal "public-key->string, DSA"
  (public-key->string *dsa-pub-key*)
  %dsakey-pub-string)

(when-openssl
 (test-equal "public-key->string, ECDSA"
   (public-key->string *ecdsa-pub-key*)
   %ecdsakey-pub-string))


(test-equal "string->public-key, RSA"
  (public-key->string (string->public-key %rsakey-pub-string 'rsa))
  %rsakey-pub-string)

(unless-dsa-supported
 (test-skip "string->public-key, DSA"))
(test-equal "string->public-key, DSA"
  (public-key->string (string->public-key %dsakey-pub-string 'dss))
  %dsakey-pub-string)

(when-openssl
 (test-equal "string->public-key, ECDSA"
   (if (string=? (cadr (string-split (get-libssh-version) #\.)) "9")
       (public-key->string (string->public-key %ecdsakey-pub-string 'ecdsa-p256))
       (public-key->string (string->public-key %ecdsakey-pub-string 'ecdsa)))
   %ecdsakey-pub-string))

(test-assert-with-log "string->public-key, RSA, gc test"
  (let ((max-keys 1000))
    (do ((idx 1 (+ idx 1)))
        ((> idx max-keys))
      (when (zero? (euclidean-remainder idx 100))
        (format-log/scm 'nolog "" (format #f "~d / ~d keys created ..."
                                          idx max-keys)))
      (public-key->string (string->public-key %rsakey-pub-string 'rsa)))
    #t))


(test-assert-with-log "make-keypair"
  (and (let ((key (make-keypair 'rsa 1024)))
         (and (key? key)
              (eq? (get-key-type key) 'rsa)))
       (let ((key (make-keypair 'dss 1024)))
         (and (key? key)
              (eq? (get-key-type key) 'dss)))
       (when-openssl
        (let ((key (make-keypair 'ecdsa 256)))
          (and (key? key)
               (or (eq? (get-key-type key) 'ecdsa) ; libssh < 0.9
                   (eq? (get-key-type key) 'ecdsa-p256)))))))

;;;
(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "key")

(exit (= 0 exit-status))

;;; key.scm ends here.
