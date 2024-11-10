;;; guix.scm -- GNU Guix package definition.

;; Copyright (C) 2022-2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; Use this file to build Guile-SSH with GNU Guix:
;;
;;   guix build -f guix.scm
;;
;; By default Guile-SSH builds with libssh 0.10, but it is possible to switch
;; it to libssh 0.9 by exporting an environment variable:
;;
;;   export GUILE_SSH_BUILD_WITH_LIBSSH_0_9=1
;;   guix build -f guix.scm


;;; Code:

(use-modules (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix git-download)
             (guix download)
             (guix utils)
             (guix build-system cmake)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages ssh)
             (gnu packages compression)
             (gnu packages kerberos)
             (gnu packages gnupg)
             (gnu packages texinfo)
             (gnu packages python)
             (gnu packages pkg-config)
             (gnu packages base))


(define %source-dir (dirname (current-filename)))



(define-public libssh10
  (package
    (name "libssh")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.libssh.org/files/"
                                  (version-major+minor version)
                                  "/libssh-" version ".tar.xz"))
              (sha256
               (base32
                "0mqbmz97p6wcq3k3lllnw2khvr3db3n2va45nz88m0yd6k2mih8d"))))
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (arguments
     '(#:configure-flags '("-DWITH_GCRYPT=ON")

       ;; TODO: Add 'CMockery' and '-DWITH_TESTING=ON' for the test suite.
       #:tests? #f))
    (inputs (list zlib libgcrypt mit-krb5))
    (synopsis "SSH client library")
    (description
     "libssh is a C library implementing the SSHv2 and SSHv1 protocol for client
and server implementations.  With libssh, you can remotely execute programs,
transfer files, and use a secure and transparent tunnel for your remote
applications.")
    (home-page "https://www.libssh.org")
    (license license:lgpl2.1+)))

(define-public libssh8-0
  (package
   (inherit libssh10)
   (name "libssh")
   (version "0.8.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://www.libssh.org/files/"
                                (version-major+minor version)
                                "/libssh-" version ".tar.xz"))
            (sha256
             (base32
              "1amgzvabl835vvzyv08hr05ak2ksp4jncbfnm2i0ayspf3b5qdg0"))))
   (native-inputs (list python))))

(define-public libssh8-1
  (package
   (inherit libssh10)
   (name "libssh")
   (version "0.8.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://www.libssh.org/files/"
                                (version-major+minor version)
                                "/libssh-" version ".tar.xz"))
            (sha256
             (base32
              "090r1fq8p89rwfv2x3wji3kyz31bf0z9mlv6pq7nrr55niki4zyi"))))
   (native-inputs (list python))))

(define-public libssh8
  (package
   (inherit libssh10)
   (name "libssh")
   (version "0.8.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://www.libssh.org/files/"
                                (version-major+minor version)
                                "/libssh-" version ".tar.xz"))
            (sha256
             (base32
              "1l19pl0l8lp00a8yawvf2yp8xhb4fjgsdmvprv9qqdpj0vv32brh")))) ))

(define-public libssh9
  (package
    (inherit libssh)
    (name "libssh")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.libssh.org/files/"
                                  (version-major+minor version)
                                  "/libssh-" version ".tar.xz"))
              (sha256
               (base32
                "19f7h8s044pqfhfk35ky5lj4hvqhi2p2p46xkwbcsqz6jllkqc15"))))))

(define-public libssh11
  (package
   (inherit libssh)
   (name "libssh")
   (version "0.11.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://www.libssh.org/files/"
                                (version-major+minor version)
                                "/libssh-" version ".tar.xz"))
            (sha256
             (base32
              "0y8v5ihrqnjxchvjhz8fcczndchaaxxim64bqm8q3q4i5v3xrdql"))))))

(define-public guile-ssh
  (package
    (name "guile-ssh")
    (version "git")
    (home-page "https://github.com/artyom-poptsov/guile-ssh")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(;; It makes no sense to build libguile-ssh.a.
       #:configure-flags '("--disable-static")

       #:phases (modify-phases %standard-phases
                  (add-before 'build 'fix-libguile-ssh-file-name
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Build and install libguile-ssh.so so that we can use
                      ;; its absolute file name in .scm files, before we build
                      ;; the .go files.
                      (let* ((out (assoc-ref outputs "out"))
                             (lib (string-append out "/lib")))
                        (invoke "make" "install"
                                "-C" "libguile-ssh"
                                "-j" (number->string
                                      (parallel-job-count)))
                        (substitute* (find-files "." "\\.scm$")
                          (("\"libguile-ssh\"")
                           (string-append "\"" lib "/libguile-ssh\"")))
                        #t)))
                  ,@(if (%current-target-system)
                        '()
                        '((add-before 'check 'fix-guile-path
                             (lambda* (#:key inputs #:allow-other-keys)
                               (let ((guile (assoc-ref inputs "guile")))
                                 (substitute* "tests/common.scm"
                                   (("/usr/bin/guile")
                                    (string-append guile "/bin/guile")))
                                 #t)))))
                  (add-after 'install 'remove-bin-directory
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (examples (string-append
                                        out "/share/guile-ssh/examples")))
                        (mkdir-p examples)
                        (rename-file (string-append bin "/ssshd.scm")
                                     (string-append examples "/ssshd.scm"))
                        (rename-file (string-append bin "/sssh.scm")
                                     (string-append examples "/sssh.scm"))
                        (delete-file-recursively bin)
                        #t))))))
    (native-inputs (list autoconf
                         automake
                         libtool
                         texinfo
                         pkg-config
                         which
                         guile-3.0)) ;needed when cross-compiling.
    (inputs (list guile-3.0 libssh10 libgcrypt))
    (synopsis "Guile bindings to libssh")
    (description
     "Guile-SSH is a library that provides access to the SSH protocol for
programs written in GNU Guile interpreter.  It is a wrapper to the underlying
libssh library.")
    (license license:gpl3+)))

(define-public guile-ssh/libssh8-0
  (package
   (inherit guile-ssh)
   (name "guile-ssh")
   (inputs (modify-inputs (package-inputs guile-ssh)
                          (replace "libssh" libssh8-0)))))

(define-public guile-ssh/libssh8-1
  (package
   (inherit guile-ssh)
   (name "guile-ssh")
   (inputs (modify-inputs (package-inputs guile-ssh)
                          (replace "libssh" libssh8-1)))))

(define-public guile-ssh/libssh8
  (package
   (inherit guile-ssh)
   (name "guile-ssh")
   (inputs (modify-inputs (package-inputs guile-ssh)
                          (replace "libssh" libssh8)))))

(define-public guile-ssh/libssh9
  (package
   (inherit guile-ssh)
   (name "guile-ssh")
   (inputs (modify-inputs (package-inputs guile-ssh)
             (replace "libssh" libssh9)))))

(define-public guile-ssh/libssh11
  (package
   (inherit guile-ssh)
   (name "guile-ssh")
   (inputs (modify-inputs (package-inputs guile-ssh)
                          (replace "libssh" libssh11)))))



(cond
 ((getenv "GUILE_SSH_BUILD_WITH_LIBSSH_0_8_0")
  guile-ssh/libssh8-0)
 ((getenv "GUILE_SSH_BUILD_WITH_LIBSSH_0_8_1")
  guile-ssh/libssh8-1)
 ((getenv "GUILE_SSH_BUILD_WITH_LIBSSH_0_8")
  guile-ssh/libssh8)
 ((getenv "GUILE_SSH_BUILD_WITH_LIBSSH_0_9")
  guile-ssh/libssh9)
 ((getenv "GUILE_SSH_BUILD_WITH_LIBSSH_0_11")
  guile-ssh/libssh11)
 (else
  guile-ssh))

;;; guix.scm ends here.
