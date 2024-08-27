;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Romain Garbage <romain.garbage@inria.fr>
;;;
;;; This file is NOT part of GNU Guix, but is supposed to be used with GNU
;;; Guix and thus has the same license.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix-science-nonfree packages fabric-management)
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix-science-nonfree packages cuda))

(define-public gdrcopy
  (package
    (name "gdrcopy")
    (version "2.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/gdrcopy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (snippet
        ;; Prevent building the driver.
        #~(delete-file "src/gdrdrv/Makefile"))
       (sha256
        (base32
         "043ics03kmmxnifxhsyv3qw2lbr37q1gq7cqq763x3b7ckbg9w18"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               ;; No configure script.
               (delete 'configure)
               ;; stubs subfolder is in lib, not in lib64.
               (add-before 'build 'fix-stubs-path
                 (lambda _
                   (substitute* "tests/Makefile"
                     (("lib64/stubs")
                      "lib/stubs")))))
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   ;; cc compiler is not found if the above is not defined.
                   (string-append "prefix=" #$output)
                   (string-append "CUDA=" #$(this-package-input "cuda-toolkit"))
                   ;; Avoid building driver which requires additional
                   ;; dependencies.
                   "lib"
                   ;; The exes target builds test binaries and is
                   ;; required by the install target.
                   "exes")
           ;; No tests as they would require libcuda.so.
           #:tests? #f
           ;; No libcuda.so present.
           #:validate-runpath? #f))
    (inputs (list cuda))
    (home-page "https://github.com/NVIDIA/gdrcopy")
    (synopsis "A low-latency GPU memory copy library based on NVIDIA GPUDirect RDMA technology")
    (description "GDRCopy is a low-latency GPU memory copy library based on GPUDirect
RDMA technology that allows the CPU to directly map and access GPU
memory.")
    (license expat)))
