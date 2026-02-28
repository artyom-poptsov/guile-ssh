;;; guix.scm -- GNU Guix package definition.

;; Copyright (C) 2022-2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;; Copyright (C) 2026 Nicolas Graves <ngraves@ngraves.fr>
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
             (gnu packages base)
             (gnu packages bash)
             (gnu packages check)
             (gnu packages compression)
             (gnu packages gnupg)
             (gnu packages guile)
             (gnu packages kerberos)
             (gnu packages pkg-config)
             (gnu packages python)
             (gnu packages ssh)
             (gnu packages texinfo)
             (gnu packages tls))


(define %source-dir (dirname (current-filename)))

(define (libssh-tarball version)
  (string-append "https://www.libssh.org/files/"
                 (version-major+minor version)
                 "/libssh-" version ".tar.xz"))



(define-public libssh
  (package
    (name "libssh")
    (version "0.10.6")
    (source (origin
              (method url-fetch)
              (uri (libssh-tarball version))
              (sha256
               (base32
                "1hcxvsb4brznxqq6cjwxkk7yv4c48w4fcwxwd8dp9wdnyncd8q8q"))))
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (arguments
     (list
      #:configure-flags
      #~(list #$@(if (%current-target-system)
                     #~()
                     #~("-DUNIT_TESTING=ON")))
      #:modules
      '((guix build cmake-build-system)
        (guix build utils)
        (srfi srfi-1))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-commands
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Runtime sources.
              (substitute* (filter file-exists?
                                   (list "src/config.c"
                                         "src/socket.c"))
                (("\"/bin/sh\"")
                 (format #f "~s" (search-input-file inputs "/bin/sh"))))
              ;; Test sources.
              (substitute* (filter file-exists?
                                   '("tests/server/test_server/default_cb.c"))
                (("\"/bin/sh\"")
                 (format #f "~s" (which "sh"))))
              (substitute* (find-files "tests/unittests" "\\.sh$")
                (("/bin/echo")
                 (which "echo")))))
          (add-before 'check 'prepare-for-tests
            ;; A few test rely on the assumption that HOME == user's pw_dir,
            ;; which is not satisfied in Guix, where `pw_dir' is '/' while
            ;; HOME is '/homeless-shelter'.
            (lambda _
              (setenv "HOME" "/"))))))
    (native-inputs (list cmocka))
    (inputs (list bash-minimal zlib openssl mit-krb5))
    (synopsis "SSH client library")
    (description
     "libssh is a C library implementing the SSHv2 and SSHv1 protocol for client
and server implementations.  With libssh, you can remotely execute programs,
transfer files, and use a secure and transparent tunnel for your remote
applications.")
    (home-page "https://www.libssh.org")
    (license license:lgpl2.1+)))

(define-public libssh-8
  (package
   (inherit libssh)
   (name "libssh")
   (version "0.8.3")
   (source (origin
            (method url-fetch)
            (uri (libssh-tarball version))
            (sha256
             (base32
              "1l19pl0l8lp00a8yawvf2yp8xhb4fjgsdmvprv9qqdpj0vv32brh"))))
   (arguments
    ;; XXX: Two tests fail.
    (list #:tests? #f))))

(define-public libssh-9
  (package
    (inherit libssh)
    (name "libssh")
    (version "0.9.8")
    (source (origin
              (method url-fetch)
              (uri (libssh-tarball version))
              (sha256
               (base32
                "1kg7ya1yc6m5iwld0nvgbprcr5xf21ymp0xyggb2im214drlp0wz"))))
    (arguments
     ;; XXX: Some tests require some configure flags.
     ;; OPENSSH_KEYS ; SSHD_EXECUTABLE
     (list #:tests? #f))))

(define-public libssh-10 libssh)

(define-public libssh-11
  (package
   (inherit libssh)
   (name "libssh")
   (version "0.11.4")
   (source (origin
            (method url-fetch)
            (uri (libssh-tarball version))
            (sha256
             (base32
              "00bp5692k05281dvzqzxksa4h35ahhz6wmy61q89wv6nwchc6ah0"))))))

(define-public libssh-12
  (package
   (inherit libssh)
   (name "libssh")
   (version "0.12.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://www.libssh.org/files/"
                                (version-major+minor version)
                                "/libssh-" version ".tar.xz"))
            (sha256
             (base32
              "08bidaiq4z911zl3v7xc2zb6s8i4p7syfpsfxznmwzijv0jg8shs"))))))

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
     (list
      ;; It makes no sense to build libguile-ssh.a.
      #:configure-flags #~(list "--disable-static")

      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fix-libguile-ssh-file-name
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Build and install libguile-ssh.so so that we can use
              ;; its absolute file name in .scm files, before we build
              ;; the .go files.
              (let ((lib (string-append #$output "/lib")))
                (invoke "make" "install"
                        "-C" "libguile-ssh"
                        "-j" (number->string (parallel-job-count)))
                (substitute* (find-files "." "\\.scm$")
                  (("\"libguile-ssh\"")
                   (string-append "\"" lib "/libguile-ssh\""))))))
          #$@(if (%current-target-system)
                 #~()
                 #~((add-before 'check 'fix-guile-path
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "tests/common.scm"
                          (("/usr/bin/guile")
                           (search-input-file inputs "/bin/guile")))))))
          (add-after 'install 'remove-bin-directory
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (examples (string-append #$output
                                             "/share/guile-ssh/examples")))
                (mkdir-p examples)
                (rename-file (string-append bin "/ssshd.scm")
                             (string-append examples "/ssshd.scm"))
                (rename-file (string-append bin "/sssh.scm")
                             (string-append examples "/sssh.scm"))
                (delete-file-recursively bin)))))))
    (native-inputs
     (list autoconf
           automake
           libtool
           texinfo
           pkg-config
           which
           guile-3.0)) ;needed when cross-compiling.
    (inputs (list guile-3.0 libssh-10 libgcrypt))
    (synopsis "Guile bindings to libssh")
    (description
     "Guile-SSH is a library that provides access to the SSH protocol for
programs written in GNU Guile interpreter.  It is a wrapper to the underlying
libssh library.")
    (license license:gpl3+)))

(define-public guile-ssh/libssh-8
  (package
   (inherit guile-ssh)
   (name "guile-ssh")
   (inputs (modify-inputs (package-inputs guile-ssh)
                          (replace "libssh" libssh-8)))))

(define-public guile-ssh/libssh-9
  (package
   (inherit guile-ssh)
   (name "guile-ssh")
   (inputs (modify-inputs (package-inputs guile-ssh)
             (replace "libssh" libssh-9)))))

(define-public guile-ssh/libssh-11
  (package
   (inherit guile-ssh)
   (name "guile-ssh")
   (inputs (modify-inputs (package-inputs guile-ssh)
                          (replace "libssh" libssh-11)))))

(define-public guile-ssh/libssh-12
  (package
   (inherit guile-ssh)
   (name "guile-ssh")
   (inputs (modify-inputs (package-inputs guile-ssh)
                          (replace "libssh" libssh-12)))))



(cond
 ((getenv "GUILE_SSH_BUILD_WITH_LIBSSH_0_8")
  guile-ssh/libssh-8)
 ((getenv "GUILE_SSH_BUILD_WITH_LIBSSH_0_9")
  guile-ssh/libssh-9)
 ((getenv "GUILE_SSH_BUILD_WITH_LIBSSH_0_11")
  guile-ssh/libssh-11)
 ((getenv "GUILE_SSH_BUILD_WITH_LIBSSH_0_12")
  guile-ssh/libssh-12)
 (else
  guile-ssh))

;;; guix.scm ends here.
