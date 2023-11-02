;; -*- mode: scheme; eval: (guix-devel-mode 1); geiser-scheme-implementation: guile -*-

;;;
;;; Copyright © 2021 Quantile Technologies <phil.beadling@quantile.com>
;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix-science-nonfree packages gurobi)
  #:use-module (guix build-system python)
  #:use-module (guix-science-nonfree licenses)
  #:use-module (guix packages)
  #:use-module (guix download) ;; url-fetch
  #:use-module (guix git)      ;; git-checkout
  #:use-module (guix utils)
  #:use-module (gnu packages elf)) ;; patchelf

(define-public python-gurobipy
  (package
    (name "python-gurobipy")
    (version "10.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://packages.gurobi.com/"
                                  (version-major+minor version) "/gurobi"
                                  version "_linux64.tar.gz"))
              (sha256
               (base32
                "10yah1bhiic6h7wmj31w7kbariwpp97a241ww66cwhhc27didyc2"))))
    (build-system python-build-system)
    (arguments
     `(#:use-setuptools? #f ;; distuils package
       #:tests? #f ;; no tests in package
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'cd-to-source-dir
           (lambda _ (chdir "linux64") #t))
         ;; copy and symlink gurobi binary and adjust python lib rpath
         ;; to look for it in it's own directory.
         (add-after 'install 'install-gurobi-library
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((dir (string-append (site-packages inputs outputs)
                                        "/gurobipy/"))
                    (lib-to-install (string-append "libgurobi.so." ,(package-version this-package)))
                    (softlink-version (string-split ,(package-version this-package) #\.))
                    (softlink-lib (string-append
                                   "libgurobi"
                                   (car softlink-version)
                                   (cadr softlink-version)
                                   ".so"))
                    (lib-change-rpath "gurobipy.so"))
               (install-file (string-append "lib/" lib-to-install) dir)
               (with-directory-excursion dir
                 (symlink lib-to-install softlink-lib)
                 (invoke "chmod" "-v" "+w" lib-change-rpath)
                 (invoke "patchelf" "--debug" "--set-rpath" "$ORIGIN" lib-change-rpath)
                 (invoke "chmod" "-v" "-w" lib-change-rpath))))))))
    (native-inputs (list patchelf))
    (home-page "https://www.gurobi.com/products/gurobi-optimizer/")
    (synopsis
     "Python interface to the Gurobi Optimizer")
    (description
     "The Gurobi Optimizer is a commercial optimization solver for
linear programming (LP), quadratic programming (QP), quadratically
constrained programming (QCP), mixed integer linear programming
(MILP), mixed-integer quadratic programming (MIQP), and mixed-integer
quadratically constrained programming (MIQCP).  See here for more info:
@url{https://www.gurobi.com/documentation/9.0/quickstart_linux/cs_python.html}.")
    (license (nonfree "https://www.gurobi.com/products/licensing-options/"))))
