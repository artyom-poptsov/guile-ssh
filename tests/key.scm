;;; key.scm -- Testing of Guile-SSH keys

;; Copyright (C) 2014, 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(test-begin "key")

(test-assert "private-key-from-file"
  (and (private-key-from-file %rsakey)
       (private-key-from-file %dsakey)
       (when-openssl
        (private-key-from-file %ecdsakey))))

(test-assert "public-key-from-file"
  (and (public-key-from-file %rsakey-pub)
       (public-key-from-file %dsakey-pub)
       (when-openssl
        (public-key-from-file %ecdsakey-pub))))

(define *rsa-key*       (private-key-from-file %rsakey))
(define *dsa-key*       (private-key-from-file %dsakey))
(define *ecdsa-key*     (when-openssl
                         (private-key-from-file %ecdsakey)))
(define *rsa-pub-key*   (public-key-from-file %rsakey-pub))
(define *dsa-pub-key*   (public-key-from-file %dsakey-pub))
(define *ecdsa-pub-key* (when-openssl
                         (public-key-from-file %ecdsakey-pub)))

(test-assert "key?"
  (and (not (key? "not a key"))
       (key? *rsa-key*)
       (key? *dsa-key*)
       (when-openssl
        (key? *ecdsa-key*))
       (key? *rsa-pub-key*)
       (key? *dsa-pub-key*)
       (when-openssl
        (key? *ecdsa-pub-key*))))

(test-assert "private-key?"
  (and (private-key? *rsa-key*)
       (not (private-key? *rsa-pub-key*))
       (not (private-key? "not a key"))))

(test-assert "public-key?"
  (and (public-key? *rsa-pub-key*)

       ;; XXX: Currently a SSH key that has been read from a file
       ;; has both public and private flags.
       (public-key? *rsa-key*)

       (not (public-key? "not a key"))))

(test-assert "private-key->public-key"
  (and (private-key->public-key *rsa-key*)
       (private-key->public-key *dsa-key*)
       (when-openssl
        (private-key->public-key *ecdsa-key*))))

(test-assert "get-key-type"
  (and (eq? 'rsa   (get-key-type *rsa-key*))
       (eq? 'dss   (get-key-type *dsa-key*))
       (when-openssl
        (eq? 'ecdsa (get-key-type *ecdsa-key*)))))


(test-assert "private-key-to-file"
  (when-openssl
   (let ((file-name "./tmp-rsa-key"))
     (private-key-to-file *rsa-key* file-name)
     (let ((key (private-key-from-file file-name)))
       (delete-file file-name)
       (and (key? key)
            (private-key? key))))))


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

(test-equal "string->public-key, DSA"
  (public-key->string (string->public-key %dsakey-pub-string 'dss))
  %dsakey-pub-string)

(when-openssl
 (test-equal "string->public-key, ECDSA"
   (public-key->string (string->public-key %ecdsakey-pub-string 'ecdsa))
   %ecdsakey-pub-string))


(test-assert "make-keypair"
  (and (let ((key (make-keypair 'rsa 1024)))
         (and (key? key)
              (eq? (get-key-type key) 'rsa)))
       (let ((key (make-keypair 'dss 1024)))
         (and (key? key)
              (eq? (get-key-type key) 'dss)))
       (when-openssl
        (let ((key (make-keypair 'ecdsa 256)))
          (and (key? key)
               (eq? (get-key-type key) 'ecdsa))))))

;;;

(test-end "key")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; key.scm ends here.
