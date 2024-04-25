;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; However, note that this module provides packages for "non-free" software,
;;; which denies users the ability to study and modify it.  These packages
;;; are detrimental to user freedom and to proper scientific review and
;;; experimentation.  As such, we kindly invite you not to share it.
;;;
;;; Copyright © 2019, 2023 Inria

(define-module (guix-science-nonfree packages mkl)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix-science-nonfree licenses) #:prefix nonfree:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages maths)
  #:use-module (srfi srfi-26))

(define-public mkl-2020
  (package
    (name "mkl")
    (version "2020.4.304")
    (source (origin
              (method url-fetch)
              (uri "https://registrationcenter-download.intel.com/akdlm/irc_nas/tec/16917/l_mkl_2020.4.304.tgz")
              (sha256
               (base32
                "19byj6jh1bgsvpkp933rw9wpxp2ml7lzkr54y84bskcp6rjx8513"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (delete 'build)
         (add-before 'install 'extract-rpms
           (lambda _
             ;; Instead of running their 'install.sh', a script-generating
             ;; script that attempts to run obscure pre-built binaries,
             ;; simply extract the RPMs to ./opt/intel.
             (mkdir "opt")
             (for-each (lambda (rpm)
                         (format #t "extracting ~a...~%" rpm)
                         (let* ((command (string-append  "rpm2cpio "
                                                         rpm " | cpio -i"))

                                (status  (system command)))

                           (unless (zero? status)
                             (error (format #f "command '~a' failed with ~a"
                                            command status)))))
                       (find-files "rpm" "\\.rpm$"))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (source-prefix (string-append
                                    "opt/intel/compilers_and_libraries_"
                                    ,(package-version this-package) "/linux/mkl"))
                    (bindir (string-append out "/bin"))
                    (libdir (string-append out "/lib"))
                    (includedir (string-append out "/include"))
                    (linktool (string-append bindir "/mkl_link_tool")))

               ;; Install libraries.  Don't install .a files: that saves
               ;; ~1 GB and presumably nobody cares.
               (for-each (lambda (lib)
                           (install-file lib libdir))
                         (find-files (string-append source-prefix
                                                    "/lib/intel64_lin")
                                     "\\.so$"))

               (copy-recursively (string-append source-prefix "/include")
                                 includedir)
               (copy-recursively (string-append source-prefix "/bin")
                                 bindir)

               ;; 'mkl_link_tool' is in version 2020, but not in 2019.
               (when (file-exists? linktool)
                 ;; Note: 'set-file-runpath' from gremlin.scm can modify
                 ;; DT_RUNPATH, but it cannot add it when it's missing, hence
                 ;; PatchELF.
                 (invoke "patchelf" linktool
                         "--set-rpath"
                         (string-join
                          (list (dirname (search-input-file inputs
                                                            "/lib/libgcc_s.so"))
                                (dirname (search-input-file inputs
                                                            "/lib/libstdc++.so.6")))
                          ":")))))))

       ;; Let's not publish or obtain substitutes for that.
       #:substitutable? #f))
    (native-inputs
     (list tar gzip cpio rpm patchelf))
    (inputs
     (list `(,(canonical-package gcc) "lib")))
    (home-page "https://software.intel.com/en-us/mkl")
    ;; 32-bit libraries are not installed.
    (supported-systems '("x86_64-linux"))
    (synopsis "Non-free library of optimized math routines")
    (description
     "Intel® Math Kernel Library (MKL) is a proprietary library of
highly optimized, extensively threaded routines for applications that
require maximum performance.  The library provides Fortran and C
programming language interfaces.  Intel MKL C language interfaces can
be called from applications written in either C or C++, as well as in
any other language that can reference a C interface.")
    (license (nonfree:nonfree "https://developer.nvidia.com/nvidia-cuda-license"))))

(define-public mkl-2019
  (package
    (inherit mkl-2020)
    (name "mkl")
    (version "2019.1.144")
    (source (origin
              (method url-fetch)
              (uri "http://registrationcenter-download.intel.com/akdlm/irc_nas/tec/14895/l_mkl_2019.1.144.tgz")
              (sha256
               (base32
                "1fqgla8gzf656xmbqijk8phja31dkcw6g0w68ajgg1f6m5ha81aj"))
              (file-name (string-append name "-" version ".tar.gz"))))))
