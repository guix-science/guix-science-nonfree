;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Cayetano Santos <csantosb@inventati.org>
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

(define-module (guix-science-nonfree packages electronics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages base)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config))

;;; * gnat

(define-public gnat
  (package
    (name "gnat")
    (version "14.2.0-1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/alire-project/GNAT-FSF-builds/"
         "releases/download/gnat-14.2.0-1/gnat-x86_64-linux-"
         version ".tar.gz"))
       (sha256
        (base32
         "08kpd3d7si73gsm2dfp5lmrhii9k96y972sw39h1sdvhgzpkvfq6"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; Let's not publish or obtain substitutes for that.
      #:substitutable? #f
      #:strip-binaries? #f
      ;; XXX: This would check DT_RUNPATH, but patchelf populate DT_RPATH,
      ;; not DT_RUNPATH.
      #:validate-runpath? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((source (assoc-ref inputs "source")))
                (invoke "tar" "xvzf" source)
                (chdir "gnat-x86_64-linux-14.2.0-1"))))
          (delete 'configure)
          (delete 'check)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (define libc
                (assoc-ref inputs "libc"))
              (define gcc-lib
                (assoc-ref inputs "gcc:lib"))
              (define ld.so
                (search-input-file inputs #$(glibc-dynamic-linker)))
              (define rpath
                (string-join (list "$ORIGIN"
                                   (string-append #$output "/lib")
                                   (string-append #$output "/lib64")
                                   (string-append libc "/lib")
                                   (string-append gcc-lib "/lib"))
                             ":"))

              ;; patchelf procedure
              (define (patch-elf file)
                (make-file-writable file)

                (unless (string-contains file ".so")
                  (unless (string-contains file ".o")
                    (format #t "Setting RPATH on '~a'...~%" file)
                    (invoke "patchelf" "--set-rpath" rpath "--force-rpath" file)))

                (unless (string-contains file ".so")
                  (unless (string-contains file ".o")
                    (format #t "Setting interpreter on '~a'...~%" file)
                    (invoke "patchelf" "--set-interpreter" ld.so file))))

              ;; patch files
              (for-each (lambda (file)
                          (when (elf-file? file)
                            (patch-elf file)))
                        (find-files "."
                                    (lambda (file stat)
                                      (eq? 'regular
                                           (stat:type stat)))))))

          (replace 'install
            (lambda* _
              (let ((bin (string-append #$output "/bin"))
                    (lib (string-append #$output "/lib"))
                    (lib64 (string-append #$output "/lib64"))
                    (libexec (string-append #$output "/libexec"))
                    (x86_64-pc-linux-gnu
                     (string-append #$output "/x86_64-pc-linux-gnu")))
                (mkdir-p #$output)
                (copy-recursively "bin" bin)
                (copy-recursively "lib" lib)
                (copy-recursively "lib64" lib64)
                (copy-recursively "libexec" libexec)
                (copy-recursively "x86_64-pc-linux-gnu"
                                  x86_64-pc-linux-gnu)))))))
    (native-inputs
     (list patchelf))
    (inputs
     `(("gcc:lib" ,gcc-14 "lib")))
    (home-page "https://github.com/alire-project/GNAT-FSF-builds")
    (synopsis
     "Builds of the GNAT Ada compiler from FSF GCC releases")
    (description
     "This package gathers binaries corresponding to the ADA compiler.")
    (license (list license:expat)))) ; MIT license

;;; * ghdl

(define-public ghdl-clang
  (let ((commit "eeab69c29b68eb3f7fd51e6337eedb924d7be829")
        (revision "0"))
    (package
      (name "ghdl-clang")
      (version (git-version "4.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ghdl/ghdl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1p495ax8cxspb13kbnfy0ba1s5kid7f9hmscdv30lf8y22plawb3"))))
      (build-system gnu-build-system)
      (arguments
       (list
        ;; XXX: This would check DT_RUNPATH, but patchelf populate DT_RPATH,
        ;; not DT_RUNPATH.
        #:validate-runpath? #f
        #:phases
        #~(modify-phases %standard-phases
            (replace 'configure
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((libc (assoc-ref inputs "libc")))
                  (mkdir "build")
                  (chdir "build")
                  (setenv "LIBRARY_PATH"
                          (string-append (string-append libc "/lib")
                                         (getenv "LIBRARY_PATH")))
                  (setenv "LD_LIBRARY_PATH"
                          (string-append (string-append libc "/lib")
                                         (getenv "LIBRARY_PATH")))
                  (invoke "../configure"
                          "--with-llvm-config"
                          "--enable-libghdl"
                          "--enable-synth"
                          "--disable-gplcompat"
                          (string-append "--prefix=" #$output)))))
            (delete 'check)
            (replace 'build
              (lambda* (#:key inputs #:allow-other-keys)
                (invoke "make" "ghdl_llvm" "-j 8")
                (invoke "patchelf"
                        "--set-interpreter"
                        (search-input-file inputs #$(glibc-dynamic-linker))
                        "ghdl_llvm")
                (invoke "make" "-j 8"))))))
      (propagated-inputs
       (list clang-toolchain))
      (inputs
       (list patchelf gnat))
      (home-page "https://github.com/ghdl/ghdl")
      (synopsis
       "Analyzer, compiler, simulator and (experimental) synthesizer for VHDL")
      (description
       "GHDL analyse and elaborate VHDL sources for generating machine code.")
      (license license:lgpl2.0))))

(define-public ghdl-yosys-plugin
  (let ((commit "511412f984d64ed7c46c4bdbd839f4b3c48f6fa5")
        (revision "0"))
    (package
      (name "ghdl-yosys-plugin")
      (version (git-version "0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ghdl/ghdl-yosys-plugin")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1mbg4s80sbmhgmrgh3wvjwnbg0q6ha2rsm8xgypdy7pq7bp0pc97"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (delete 'check)
            (replace 'install
              (lambda* _
                (let ((lib (string-append #$output "/lib")))
                  (mkdir-p lib)
                  (copy-file "ghdl.so" (string-append lib "/ghdl.so"))))))))
      (inputs
       (list yosys-clang ghdl-clang tcl readline))
      (home-page "https://github.com/ghdl/ghdl-yosys-plugin")
      (synopsis
       "Yosys VHDL synthesis module based on ghdl")
      (description
       "Shared library module for yosys to implement logical synthesis of VHDL designs.")
      (license license:gpl3+))))
