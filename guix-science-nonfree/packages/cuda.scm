;;; Copyright © 2018, 2019, 2020 Inria
;;; Copyright © 2021-2024 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2024 Atte Torri <atte.torri@protonmail.com>
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

(define-module (guix-science-nonfree packages cuda)
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix-science-nonfree licenses) #:prefix nonfree:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python))

(define (make-cuda version origin)
  (package
    (name "cuda-toolkit")
    (version version)
    (source origin)
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ;196 MiB
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match))

      ;; Let's not publish or obtain substitutes for that.
      #:substitutable? #f

      #:strip-binaries? #f              ;no need

      ;; XXX: This would check DT_RUNPATH, but patchelf populate DT_RPATH,
      ;; not DT_RUNPATH.
      #:validate-runpath? #f

      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((source (assoc-ref inputs "source")))
                (invoke "sh" source "--keep" "--noexec")
                (chdir "pkg/run_files")
                (match (find-files "." "^cuda-linux64-rel.*\\.run$")
                  ((run)
                   (invoke "sh" run "--keep" "--noexec")))
                (chdir "pkg"))))
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
                                   (string-append #$output "/nvvm/lib64")
                                   (string-append libc "/lib")
                                   (string-append gcc-lib "/lib"))
                             ":"))

              (define (patch-elf file)
                (make-file-writable file)
                (unless (string-contains file ".so")
                  (format #t "Setting interpreter on '~a'...~%" file)
                  (invoke "patchelf" "--set-interpreter" ld.so
                          file))
                (format #t "Setting RPATH on '~a'...~%" file)
                (invoke "patchelf" "--set-rpath" rpath
                        "--force-rpath" file))

              (for-each (lambda (file)
                          (when (elf-file? file)
                            (patch-elf file)))
                        (find-files "."
                                    (lambda (file stat)
                                      (eq? 'regular
                                           (stat:type stat)))))))
          (replace 'install
            (lambda _
              (let ((lib   (string-append #$output "/lib"))
                    (lib64 (string-append #$output "/lib64")))
                (mkdir-p #$output)
                (setenv "PERL5LIB" (getcwd)) ;for InstallUtils.pm
                (invoke "perl" "install-linux.pl"
                        (string-append "--prefix=" #$output))
                (rename-file lib64 lib))))
          (add-after 'install 'move-documentation
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out    (assoc-ref outputs "out"))
                     (doc    (assoc-ref outputs "doc"))
                     (docdir (string-append doc "/share/doc/cuda")))
                (mkdir-p (dirname docdir))
                (rename-file (string-append out "/doc") docdir)))))))
    (native-inputs
     (list patchelf perl python-2))
    (inputs
     `(("gcc:lib" ,gcc-8 "lib")))
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (synopsis
     "Compiler for the CUDA language and associated run-time support")
    (description
     "This package provides the CUDA compiler and the CUDA run-time support
libraries for NVIDIA GPUs, all of which are proprietary.")
    (license (nonfree:nonfree "https://developer.nvidia.com/nvidia-cuda-license"))))

(define-syntax-rule (cuda-source url hash)
  ;; Visit
  ;; <https://developer.nvidia.com/cuda-10.2-download-archive?target_os=Linux&target_arch=x86_64&target_distro=Fedora&target_version=29&target_type=runfilelocal> or similar to get the actual URL.
  (origin
    (uri url)
    (sha256 (base32 hash))
    (method url-fetch)))

(define-public cuda-8.0
  (make-cuda "8.0.61"
             (cuda-source
              "https://developer.nvidia.com/compute/cuda/8.0/Prod2/local_installers/cuda_8.0.61_375.26_linux-run"
              "1i4xrsqbad283qffvysn88w2pmxzxbbby41lw0j1113z771akv4w")))

(define-public cuda-11.0
  (package
    (inherit cuda-8.0)
    (version "11.0.3")
    (source
     (cuda-source
      "https://developer.download.nvidia.com/compute/cuda/11.0.3/local_installers/cuda_11.0.3_450.51.06_linux.run"
      "1h4c69nfrgm09jzv8xjnjcvpq8n4gnlii17v3wzqry5d13jc8ydh"))
    (outputs '("out"))                         ;XXX: no documentation for now
    (arguments
     (substitute-keyword-arguments (package-arguments cuda-8.0)
       ((#:modules modules)
        '((guix build utils)
          (guix build gnu-build-system)
          (ice-9 match)
          (ice-9 ftw)))                           ;for 'scandir'
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'unpack
              (lambda* (#:key inputs #:allow-other-keys)
                (define ld.so
                  (search-input-file inputs #$(glibc-dynamic-linker)))
                (let ((source (assoc-ref inputs "source")))
                  (invoke "sh" source "--keep" "--noexec")
                  (chdir "pkg"))))
            (add-after 'unpack 'remove-superfluous-stuff
              (lambda _
                ;; Remove things we have no use for.
                (with-directory-excursion "builds"
                  (for-each delete-file-recursively
                            '("nsight_compute" "nsight_systems" "cuda_gdb")))))
            (replace 'install
              (lambda _
                (define (copy-from-directory directory)
                  (for-each (lambda (entry)
                              (define sub-directory
                                (string-append directory "/" entry))

                              (define target
                                (string-append #$output "/" (basename entry)))

                              (when (file-exists? sub-directory)
                                (copy-recursively sub-directory target)))
                            '("bin" "targets/x86_64-linux/lib"
                              "targets/x86_64-linux/include"
                              "nvvm/bin" "nvvm/include"
                              "nvvm/lib64")))

                (setenv "COLUMNS" "200") ;wide backtraces!
                (with-directory-excursion "builds"
                  (for-each copy-from-directory
                            (scandir "." (match-lambda
                                           ((or "." "..") #f)
                                           (_ #t))))
                  ;; 'cicc' needs that directory.
                  (copy-recursively "cuda_nvcc/nvvm/libdevice"
                                    (string-append #$output "/nvvm/libdevice")))))
            (add-after 'install 'install-cupti
              (lambda _
                (copy-recursively "builds/cuda_cupti/extras/CUPTI" #$output)))
            (add-after 'install 'delete-stray-symlinks
              (lambda _
                (delete-file (string-append #$output "/include/include"))))
            ;; XXX: No documentation for now.
            (delete 'move-documentation)))))
    (native-inputs
     (modify-inputs (package-native-inputs cuda-8.0)
       (prepend which)))))

(define-public cuda-11.7
  (package
    (inherit cuda-11.0)
    (version "11.7.0")
    (source
     (cuda-source
      "https://developer.download.nvidia.com/compute/cuda/11.7.1/local_installers/cuda_11.7.1_515.65.01_linux.run"
      "1nq47szb31fk1a4glsba8dy2h58vr8z7w3pbzv8bfjb5f0lnla2j"))
    (inputs
     `(("gcc:lib" ,gcc-11 "lib")))))

(define-public cuda-11.8
  (package
    (inherit cuda-11.0)
    (version "11.8.0")
    (source
     (cuda-source
      "https://developer.download.nvidia.com/compute/cuda/11.8.0/local_installers/cuda_11.8.0_520.61.05_linux.run"
      "05jskb06lw0v1m52pg2zhm5v78837cb9pgcsxnxsgr7b7apw88wj"))
    (inputs
     `(("gcc:lib" ,gcc-11 "lib")))))

(define-public cuda-10.2
  (package
    (inherit cuda-11.0)
    (version "10.2.89")
    (source
     (cuda-source
      "https://developer.download.nvidia.com/compute/cuda/10.2/Prod/local_installers/cuda_10.2.89_440.33.01_linux.run"
      "04fasl9sjkb1jvchvqgaqxprnprcz7a8r52249zp2ijarzyhf3an"))
    (arguments
     (substitute-keyword-arguments (package-arguments cuda-11.0)
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; This phase doesn't work as is for 10.2.
            (delete 'remove-superfluous-stuff)
            (delete 'install-cupti)
            (delete 'delete-stray-symlinks)
            (add-after 'unpack 'fix-isinf
              (lambda _
                (substitute* "builds/cuda-toolkit/include/cuda_fp16.hpp"
                  (("\\(::isinf") "(std::isinf"))))
            (add-after 'install 'really-install-libdevice
              (lambda _
                ;; XXX: The 'install' phase of CUDA 11.0 looks for libdevice
                ;; in a location that's different from that of libdevice in
                ;; CUDA 10.  Copy it from the right place here.
                ;; 'cicc' needs that directory.
                (copy-recursively "builds/cuda-toolkit/nvvm/libdevice/"
                                  (string-append #$output "/nvvm/libdevice"))))))))))

(define-public cuda-12.3
  (package
    (inherit cuda-11.0)
    (version "12.3.2")
    (source
     (cuda-source
      "https://developer.download.nvidia.com/compute/cuda/12.3.2/local_installers/cuda_12.3.2_545.23.08_linux.run"
      "0nx67d4ls2nfwcfdmg81vf240z6lpwpdqypssr1wzn3hyz4szci4"))
   (inputs
     `(("gcc:lib" ,gcc-13 "lib")))))

(define-public cuda-11 cuda-11.8)

(define-public cuda-12 cuda-12.3)

(define-public cuda
  ;; Default version.
  ;;
  ;; Note: Pick a version that matches the actual "driver"--i.e.,
  ;; /usr/lib64/libcuda.so available on the target machine.
  cuda-12.3)

(define-public no-float128
  ;; FIXME: We cannot simply add it to 'propagated-inputs' of cuda-toolkit
  ;; because then it would come after glibc in CPLUS_INCLUDE_PATH.
  (package
    (name "no-float128")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))

          (let* ((header "/include/bits/floatn.h")
                 (target (string-append #$output (dirname header)))
                 (libc   #$(this-package-input "libc")))
            (mkdir-p target)
            (install-file (string-append libc header) target)
            (substitute* (string-append target "/" (basename header))
              (("#([[:blank:]]*)define __HAVE_FLOAT128[[:blank:]]+1"
                _ space)
               (string-append "#" space
                              "define __HAVE_FLOAT128 0")))))))
    (inputs `(("libc" ,glibc)))
    (home-page "https://hpc.guix.info")
    (synopsis "@file{<bits/floatn.h>} header that disables float128 support")
    (description
     "This package provides a @file{<bits/floatn.h>} header to override that
of glibc and disable float128 support.  This is required allow the use of
@command{nvcc} with CUDA 8.0 and glibc 2.26+.  Otherwise, @command{nvcc} fails like this:

@example
/gnu/store/…-glibc-2.26.105-g0890d5379c/include/bits/floatn.h(61): error: invalid argument to attribute \"__mode__\"

/gnu/store/…-glibc-2.26.105-g0890d5379c/include/bits/floatn.h(73): error: identifier \"__float128\" is undefined
@end example

See also
@url{https://devtalk.nvidia.com/default/topic/1023776/cuda-programming-and-performance/-request-add-nvcc-compatibility-with-glibc-2-26/1}.")
    (license license:gpl3+)))


;; Visit <https://docs.nvidia.com/deeplearning/cudnn/archives/index.html> for the support matrix.
(define-syntax-rule (cudnn-source url hash)
  (origin
    (uri url)
    (sha256 (base32 hash))
    (method url-fetch)))

(define (make-cudnn version origin)
  (package
    (name "cuda-toolkit-cudnn")
    (version version)
    (source origin)
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match))

      ;; Let's not publish or obtain substitutes for that.
      #:substitutable? #f

      #:strip-binaries? #f              ;no need

      ;; XXX: This would check DT_RUNPATH, but patchelf populate DT_RPATH,
      ;; not DT_RUNPATH.
      #:validate-runpath? #f

      #:phases
      #~(modify-phases %standard-phases
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
                                   (string-append #$output "/nvvm/lib64")
                                   (string-append libc "/lib")
                                   (string-append gcc-lib "/lib"))
                             ":"))

              (define (patch-elf file)
                (make-file-writable file)
                (unless (string-contains file ".so")
                  (format #t "Setting interpreter on '~a'...~%" file)
                  (invoke "patchelf" "--set-interpreter" ld.so
                          file))
                (format #t "Setting RPATH on '~a'...~%" file)
                (invoke "patchelf" "--set-rpath" rpath
                        "--force-rpath" file))

              (for-each (lambda (file)
                          (when (elf-file? file)
                            (patch-elf file)))
                        (find-files "."
                                    (lambda (file stat)
                                      (eq? 'regular
                                           (stat:type stat)))))))
          (replace 'install
            (lambda _
              (let ((lib (string-append #$output "/lib"))
                    (include (string-append #$output "/include")))
                (mkdir-p #$output)
                (copy-recursively "lib" lib)
                (copy-recursively "include" include)))))))
    (native-inputs
     (list patchelf))
    (inputs
     `(("gcc:lib" ,gcc-8 "lib")))
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (synopsis
     "NVIDIA CUDA Deep Neural Network library (cuDNN)")
    (description
     "This package provides the CUDA Deep Neural Network library.")
    (license (nonfree:nonfree "https://docs.nvidia.com/deeplearning/cudnn/sla/index.html"))))

(define-public cudnn-8.6
  (let ((version "8.6.0.163"))
    (make-cudnn version
                (cudnn-source
                 (string-append "https://developer.download.nvidia.com/compute/cudnn/redist/cudnn/linux-x86_64/cudnn-linux-x86_64-"
                                version "_cuda11-archive.tar.xz")
                 "17hm24bw4q3haksdr0hbr2zv33309mkh1ih9viz6ak198zgrdhxv"))))

(define-public cudnn-8.9.1.23
  (let ((version "8.9.1.23"))
    (make-cudnn version
                (cudnn-source
                 (string-append "https://developer.download.nvidia.com/compute/cudnn/redist/cudnn/linux-x86_64/cudnn-linux-x86_64-"
                                version "_cuda11-archive.tar.xz")
                 "0p286gnjslz06z9vff136pq8srkax75nbklmvg4r11g2cxr8ind6"))))

(define-public cutensor
  (package
    (name "cutensor")
    (version "2.0.1.2")
    (home-page "https://developer.nvidia.com/cutensor")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://developer.download.nvidia.com/\
compute/cutensor/redist/libcutensor/linux-x86_64/libcutensor-linux-x86_64-"
                                  version "-archive.tar.xz"))
              (sha256 (base32 "18l6qmfjcn75jsyzlsj66mji8lgab2ih19d0drqavfi2lqna3vgd"))))
    (build-system copy-build-system)
    (arguments
     (list #:substitutable? #f
           #:strip-binaries? #f
           #:validate-runpath? #f
           #:install-plan
           ''(("include" "include")
              ("lib" "lib")
              ("LICENSE" "LICENSE"))))
    (synopsis "Nvidia cuTENSOR library")
    (description "This package provides the proprietary cuTENSOR
library for NVIDIA GPUs.")
    (license
     (nonfree:nonfree "https://docs.nvidia.com/cuda/cutensor/latest/license.html"))))
