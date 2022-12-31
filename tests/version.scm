;;; key.scm -- Testing of Guile-SSH keys

;; Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (ssh version)
             (tests common))


(define %test-suite-name "version")

(test-begin-with-log %test-suite-name)



(test-assert "get-libssh-version"
  (get-libssh-version))

(test-assert "get-library-version"
  (get-library-version))

(test-assert "get-crypto-library"
  (get-crypto-library))

(test-assert "zlib-support?"
  (object->string (zlib-support?)))

(test-assert "dsa-support?"
  (object->string (dsa-support?)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-suite-name)

(exit exit-status)

;;; version.scm ends here.
