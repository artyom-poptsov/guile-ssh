(use-modules (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages ssh)
             (gnu packages gnupg)
             (gnu packages texinfo)
             (gnu packages pkg-config)
             (gnu packages base))


(define %source-dir (dirname (current-filename)))



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
    (inputs (list guile-3.0 libssh libgcrypt))
    (synopsis "Guile bindings to libssh")
    (description
     "Guile-SSH is a library that provides access to the SSH protocol for
programs written in GNU Guile interpreter.  It is a wrapper to the underlying
libssh library.")
    (license license:gpl3+)))



guile-ssh

;;; guix.scm ends here.
