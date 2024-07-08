;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; However, note that this module provides packages for "non-free" software,
;;; which denies users the ability to study and modify it.  These packages
;;; are detrimental to user freedom and to proper scientific review and
;;; experimentation.  As such, we kindly invite you not to share it.
;;;
;;; Copyright © 2019, 2023-2024 Inria

(define-module (guix-science-nonfree packages mkl)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix-science-nonfree licenses) #:prefix nonfree:)
  #:use-module (gnu packages base)
  #:use-module ((gnu packages bootstrap) #:select (glibc-dynamic-linker))
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


;;;
;;; oneAPI MKL.
;;;

(define (intel-mkl-url package version debversion suffix)
  (string-append "https://apt.repos.intel.com/oneapi/pool/main/intel-oneapi-mkl"
                 package "-" version "-" version "-" debversion
                 "_" suffix ".deb"))

(define (make-intel-oneapi-mkl version debversion suffix subpackage
                               hash)
  (package
    (inherit mkl-2020)
    (name (string-append "intel-oneapi-mkl"
                         (if (string=? "" subpackage) "-main" subpackage)))
    (version version)
    (source
     (origin
       (method url-fetch)
       (uri (intel-mkl-url subpackage version debversion suffix))
       (sha256 hash)))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; Let's not publish or obtain substitutes for that.
      #:substitutable? #f

      ;; XXX: This would check DT_RUNPATH, but patchelf populate DT_RPATH,
      ;; not DT_RUNPATH.
      #:validate-runpath? #f

      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'check)
          (delete 'build)
          (add-before 'install 'extract-deb
            (lambda _
              ;; extract all the debian packages to opt/intel
              (for-each (lambda (deb)
                          (format #t "extracting ~a...~%" deb)
                          (let* ((command (string-append "ar x " deb
                                                         " && tar xf data.tar.xz && rm data.tar.xz"))

                                 (status (system command)))

                            (unless (zero? status)
                              (error (format #f
                                             "command '~a' failed with ~a"
                                             command status)))))
                        (find-files "." "\\.deb$"))))

          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (define out
                (assoc-ref outputs "out"))
              (define libc
                (dirname
                 (search-input-file inputs "/lib/libc.so")))
              (define gcc-lib
                (dirname
                 (search-input-file inputs "/lib/libgcc_s.so")))
              (define source-prefix
                (string-append "opt/intel/oneapi/mkl/"
                               #$(package-version this-package)))
              (define source-inc
                (string-append source-prefix "/include"))
              (define source-bin
                (string-append source-prefix "/bin"))
              (define source-cmake
                (string-append source-prefix "/lib/cmake"))
              (define libdir
                (string-append out "/lib"))
              (define bindir
                (string-append out "/bin"))
              (define ld.so
                (string-append (dirname libc) #$(glibc-dynamic-linker)))
              (define linktool
                (string-append bindir "/intel64/mkl_link_tool"))
              (define includedir
                (string-append out "/include"))
              (define must-be-patched
                (filter (lambda (x)
                          (not (or (string-contains x "tbb") ;current link problems
                                   (string-contains x "sycl"))))
                        (find-files (string-append
                                     source-prefix
                                     "/lib/intel64")
                                    "\\.so(\\..*)?$"))) ; take into account .so and .so.2
              (define rpath
                (string-join (list "$ORIGIN" libc gcc-lib) ":"))

              (define (patch-elf file)
                (make-file-writable file)
                (unless (string-contains file ".so")
                  (format #t "Setting interpreter on '~a'...~%" file)
                  (invoke "patchelf" "--set-interpreter" ld.so
                          file))
                (format #t "Setting RPATH on '~a'...~%" file)
                (invoke "patchelf" "--set-rpath" rpath "--force-rpath" file))

              ;; Modify the RUNPATH of libraries...
              (for-each (lambda (lib)
                          (when (and (elf-file? lib)
                                     (not (symbolic-link? lib)))
                            (patch-elf lib)))     ;modify runpath
                        must-be-patched)

              ;; ... and install them.
              (for-each (lambda (lib) (install-file lib libdir))
                        must-be-patched)

              (when (directory-exists? source-inc)
                (copy-recursively source-inc includedir))
              (when (directory-exists? source-bin)
                (copy-recursively  source-bin bindir))
              (when (directory-exists? source-cmake)
                (copy-recursively source-cmake
                                  (string-append libdir "/cmake")))

              (when (file-exists? linktool)
                (invoke "patchelf" linktool "--set-rpath"
                        (string-join
                         (list (dirname (search-input-file
                                         inputs "/lib/libgcc_s.so"))
                               (dirname (search-input-file
                                         inputs "/lib/libstdc++.so.6")))
                         ":"))))))))))

(define-public intel-oneapi-mkl+main
  (make-intel-oneapi-mkl "2023.2.0" "49495" "amd64" ""
                         (base32
                          "0paj51gmn8i677ji0ggw2lkp85md7zm7kb944aprz31hm87scrbd")))
(define-public intel-oneapi-mkl+common+devel
  (make-intel-oneapi-mkl "2023.2.0" "49495" "all" "-common-devel"
                         (base32
                          "08jw9dm9inms94290fi487zn2r237n537jklc7gbgkxg2yhj9s9x")))
(define-public intel-oneapi-mkl+devel
  (make-intel-oneapi-mkl "2023.2.0" "49495" "amd64" "-devel"
                         (base32
                          "159sh6jq1q9mw6k950p1qrxnvppvmk6gcm9b0m6xd0pfpbmqp6ka")))

(define-public intel-oneapi-mkl
  (package
    (inherit mkl-2020)
    (name "intel-oneapi-mkl")                     ;meta-package
    (version "2023.2.0")
    (source #f)
    (build-system trivial-build-system)
    (propagated-inputs
     (list intel-oneapi-mkl+common+devel
           intel-oneapi-mkl+main
           intel-oneapi-mkl+devel))
    (arguments (list #:builder #~(mkdir #$output)))
    (synopsis
     "Non-free library of optimized math routines (oneAPI MKL meta-package)")))
