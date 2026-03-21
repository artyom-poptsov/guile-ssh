;; Guile-SSH -- GNU Guile bindings of libssh

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
;; along with Guile-SSH.  If not, see <http://www.gnu.org/licenses/>.

;; Guix manifest to test relevant configurations, copied from Guile-Git.

(use-modules (guix gexp)
             (guix packages)
             (guix profiles)
             (guile-ssh-packages))

(define (exported-packages)
  "Return the list of exported Guile-SSH packages."
  (module-map (lambda (name variable)
                (let ((value (variable-ref variable)))
                  (and (package? value) value)))
              (resolve-interface '(guile-ssh-packages))))

(define* (package->manifest-entry* package system
                                   #:key target)
  "Return a manifest entry for PACKAGE on SYSTEM, optionally cross-compiled to
TARGET."
  (manifest-entry
    (inherit (package->manifest-entry package))
    (name (string-append (package-name package) "." system
                         (if target
                             (string-append "." target)
                             "")))
    (item (with-parameters ((%current-system system)
                            (%current-target-system target))
            package))))

(define variants
  ;; Guile-SSH variants, natively built.
  (packages->manifest (exported-packages)))

(define other-architectures
  ;; Guile-SSH built on systems other than x86_64-linux.
  (manifest
   (map (lambda (system)
          (package->manifest-entry* guile-ssh system))
        '("i686-linux" "aarch64-linux"))))

(define native-builds
  ;; Everything built natively.
  (concatenate-manifests (list variants other-architectures)))

(define cross-compiled
  ;; Cross-compiled package.
  (manifest
   (map (lambda (target)
          (package->manifest-entry* guile-ssh "x86_64-linux"
                                    #:target target))
        '(;;"i586-pc-gnu" ;FIXME: temporarily commented out
          "aarch64-linux-gnu"))))

(concatenate-manifests (list native-builds cross-compiled))
