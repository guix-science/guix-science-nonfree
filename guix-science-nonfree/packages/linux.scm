;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ludovic Courtès <ludovic.courtes@inria.fr>
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

(define-module (guix-science-nonfree packages linux)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages linux)
  #:use-module (guix-science-nonfree packages cuda))

(define-public opa-hfi1-headers
  (package
    (name "opa-hfi1-headers")
    (version "10.11.0.1")
    (home-page "https://github.com/cornelisnetworks/opa-hfi1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "bfb3da47c6f90d89015c7c7ce01c88a4e7581c7c")))
              (file-name (git-file-name "opa-hfi1" version))
              (sha256
               (base32
                "08gp55zvp4y45x8jvjfmk7rh8ayphp9szplg58lcjykslhcl40wc"))
              (modules '((guix build utils)))
              (snippet
               #~(substitute* "include/uapi/rdma/hfi/hfi1_user.h"
                   (("#define _LINUX__HFI1_USER_H" all)
                    (string-append all "\n"
                                   "#define __packed __attribute__ ((__packed__))\n"))))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~'(("include" "include"))))
    (synopsis "Headers of the HFI1 Linux driver")
    (description
     "This package provides headers of the HFI1 Linux driver.")
    (license (list license:gpl2 license:bsd-3)))) ;dual-licensed

(define-public psm2-cuda
  (package
    (inherit psm2)
    (name "psm2-cuda")
    (inputs (modify-inputs (package-inputs psm2)
              (append cuda

                      ;; This package provides the definition for
                      ;; 'HFI1_CAP_GPUDIRECT_OT', which psm2 relies on:
                      ;; <https://github.com/cornelisnetworks/opa-psm2/issues/57>.
                      opa-hfi1-headers)))
    (arguments
     (list #:make-flags
           #~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
                   (string-append "IFS_HFI_HEADER_PATH="
                                  #$(this-package-input "opa-hfi1-headers")
                                  "/include/uapi")
                   "PSM_CUDA=1")
           #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
	       (delete 'configure)
	       (add-after 'unpack 'patch-Makefiles
		 (lambda _
		   (substitute* '("Makefile" "compat/Makefile")
		     (("/lib64") "/lib")
		     (("/usr") ""))))
	       (replace 'install
		 (lambda _
		   (setenv "DESTDIR" %output)
		   (invoke "make" "install"))))))
    (synopsis
     "Intel PSM2 communication library, with NVIDIA GPU Direct support")))

(define-public libfabric-cuda
  (package/inherit libfabric
    (name (string-append (package-name libfabric) "-cuda"))
    (arguments (substitute-keyword-arguments (package-arguments libfabric)
                 ((#:configure-flags flags)
                  #~(append (list (string-append "--with-cuda=" #$(this-package-input "cuda-toolkit")))
                            #$flags))
                 ((#:phases phases '%standard-phases)
                  #~(modify-phases #$phases
                      (add-before 'configure 'fix-library-path
                        (lambda _
                          ;; This is needed for the configure script
                          ;; to find libcudart.so.
                          (setenv "LIBRARY_PATH"
                                  (string-append (getenv "LIBRARY_PATH")
                                                 ":"
                                                 #$(this-package-input "cuda-toolkit")
                                                 "/lib/stubs"))))))
                 ((#:tests? #t #t)
                  ;; Disable tests as there is no libcuda.so.
                  #f)
                 ((#:validate-runpath? #t #t)
                  ;; Disable because of missing libcuda.so.
                  #f)))
    (inputs (modify-inputs (package-inputs libfabric)
              (replace "psm2" psm2-cuda)
              (append cuda)))))
