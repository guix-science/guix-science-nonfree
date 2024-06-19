;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023, 2024 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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

(define-module (guix-science-nonfree packages machine-learning)
  #:use-module (guix modules)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix-science packages bazel)
  #:use-module (guix-science packages machine-learning)
  #:use-module (guix-science packages python)
  #:use-module (guix-science-nonfree packages cuda)
  #:use-module (ice-9 match))

(define googletest/gcc8
  (package
    (inherit googletest)
    (native-inputs
     (modify-inputs (package-native-inputs googletest)
       (append gcc-8)))))

(define googlebenchmark/gcc8
  (package
    (inherit googlebenchmark)
    (native-inputs
     (modify-inputs (package-native-inputs googlebenchmark)
       (append gcc-8)))))

(define onnx/gcc8
  (package
    (inherit onnx)
    (name "onnx-gcc8")
    (native-inputs
     (modify-inputs (package-native-inputs onnx)
       (append gcc-8)
       (replace "googletest" googletest/gcc8)))))

(define onnx-optimizer/gcc8
  (package
    (inherit onnx-optimizer)
    (name "onnx-optimizer-gcc8")
    (inputs
     (modify-inputs (package-inputs onnx-optimizer)
       (replace "onnx" onnx/gcc8)))
    (native-inputs
     (modify-inputs (package-native-inputs onnx-optimizer)
       (append gcc-8)))))

(define-public gloo-cuda10
  (package
    (inherit gloo)
    (name "gloo-cuda10")
    (arguments
     (list
      #:tests? #false                   ;see linker error below
      #:configure-flags
      #~'("-DBUILD_SHARED_LIBS=ON"
          ;; We cannot build the tests because of a linker error with googletest:
          ;; /lib/libgtest.so.1.11.0: undefined reference to
          ;; `std::__cxx11::basic_ostringstream<char, std::char_traits<char>,
          ;; std::allocator<char> >::basic_ostringstream()@GLIBCXX_3.4.26'
          "-DBUILD_TEST=OFF"
          "-DUSE_CUDA=ON"
          #$@(if (this-package-input "rdma-core")
                 #~("-DUSE_IBVERBS=ON")
                 #~()))))
    (inputs
     (modify-inputs (package-inputs gloo)
       (append cuda-10.2)))
    ;; When building with CUDA 10 we cannot use any more recent GCC
    ;; than version 8.
    (native-inputs
     (modify-inputs (package-native-inputs gloo)
       (append gcc-8)))))

(define-public gloo-cuda11
  (let ((version "0.0.0")
        (commit "a01540ec3dabd085ad2579aa2b7a004406e2793b")
        (revision "20230315"))
    (package
      (inherit gloo-cuda10)
      (name "gloo-cuda11")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/facebookincubator/gloo")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rqcq46lmhr1xjz3bbr5mfmhyvff6qhnp88q2af5vfc9rrljvklj"))))
      (arguments
       (substitute-keyword-arguments (package-arguments gloo-cuda10)
         ((#:configure-flags flags '())
          #~'("-DBUILD_SHARED_LIBS=ON"
              "-DBUILD_TEST=OFF"
              "-DUSE_CUDA=ON"))
         ((#:phases phases '%standard-phases)
          `(modify-phases ,phases
             (add-after 'unpack 'drop-unsupported-arch
               (lambda _
                 (substitute* "cmake/Cuda.cmake"
                   (("gloo_known_gpu_archs \"[^\"]+\"")
                    "gloo_known_gpu_archs \"35 50 52 60 61 70 75\""))))))))
      (inputs
       (list cuda-11.7 openssl))
      (native-inputs
       (package-native-inputs gloo)))))

(define (tensorpipe-with-cuda cuda)
  (package
    (inherit tensorpipe)
    (name (string-append "tensorpipe-with-cuda"
                         (version-major (package-version cuda))))
    (arguments (substitute-keyword-arguments (package-arguments tensorpipe)
                 ((#:configure-flags flags #~'())
                  #~(cons "-DTP_USE_CUDA=ON" #$flags))))
    (inputs (modify-inputs (package-inputs tensorpipe)
              (append cuda)))))

(define-public tensorpipe-with-cuda10 (tensorpipe-with-cuda cuda-10.2))
(define-public tensorpipe-with-cuda11 (tensorpipe-with-cuda cuda-11.7))

(define-public python-pytorch-with-cuda10
  (package
    (inherit python-pytorch)
    (name "python-pytorch-with-cuda10")
    (source
     (origin
       (inherit (package-source python-pytorch))
       (snippet
        '(begin
           ;; We're using a separately built gloo, so this
           ;; target does not exist.
           (substitute* "cmake/Dependencies.cmake"
             (("add_dependencies\\(gloo_cuda nccl_external\\)") ""))
           ;; XXX: Let's be clear: this package is a bundling fest.  We
           ;; delete as much as we can, but there's still a lot left.
           (for-each (lambda (directory)
                       (delete-file-recursively
                        (string-append "third_party/" directory)))
                     '("benchmark" "cpuinfo" "eigen"

                       ;; FIXME: QNNPACK (of which XNNPACK is a fork)
                       ;; needs these.
                       ;; "FP16" "FXdiv" "gemmlowp" "psimd"

                       "gloo" "googletest"
                       "ios-cmake" "NNPACK"
                       "onnx" "protobuf" "pthreadpool"
                       "pybind11" "python-enum" "python-peachpy"
                       "python-six" "tbb" "XNNPACK" "zstd"))))))
    (arguments
     (substitute-keyword-arguments (package-arguments python-pytorch)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            ;; XXX: libcuda.so.1 is not on the RUNPATH.
            (delete 'validate-runpath)
            (add-after 'unpack 'do-not-build-tests
              (lambda _
                (setenv "INSTALL_TEST" "OFF")
                (setenv "BUILD_TESTS" "OFF")
                (setenv "BUILD_TEST" "OFF")))))))
    (inputs
     (modify-inputs (package-inputs python-pytorch)
       (append cuda)
       (delete "googletest")
       (delete "googlebenchmark")
       (replace "gloo" gloo-cuda10)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-pytorch)
       (replace "onnx" onnx/gcc8)
       (replace "onnx-optimizer" onnx-optimizer/gcc8)))
    ;; When building with CUDA 10 we cannot use any more recent GCC
    ;; than version 8.
    (native-inputs
     (modify-inputs (package-native-inputs python-pytorch)
       (append gcc-8)))))

(define-public python-pytorch-with-cuda11
  (package
    (inherit python-pytorch)
    (name "python-pytorch-with-cuda11")
    (arguments
     (substitute-keyword-arguments (package-arguments
                                    python-pytorch-with-cuda10)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            ;; XXX: Building with the bundled NCCL <https://github.com/nvidia/nccl>
            ;; fails with "undefined reference" errors.
            (add-after 'unpack 'disable-nccl
              (lambda _
                (substitute* "CMakeLists.txt"
                  (("USE_NCCL \"Use NCCL\" ON")
                   "USE_NCCL \"Use NCCL\" OFF"))))))))
    (inputs
     (modify-inputs (package-inputs python-pytorch)
       (append cuda-11.7)
       (replace "gloo" gloo-cuda11)))
    (propagated-inputs
     (package-propagated-inputs python-pytorch))
    (native-inputs
     (package-native-inputs python-pytorch))))

(define python-jaxlib/wheel-with-cuda11
  (let ((base (@@ (guix-science packages python) python-jaxlib/wheel)))
    (package
      (inherit base)
      (name "python-jaxlib-with-cuda11")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:fetch-targets _)
          '(list "//jaxlib/tools:build_wheel"
                 "//jaxlib/tools:build_gpu_plugin_wheel"
                 "@mkl_dnn_v1//:mkl_dnn"))
         ((#:vendored-inputs-hash _)
          "0n4p27rsb0hz7prk4lm2z9qlbi5vsrdvmd0a04kiw1kl7qzkkxxs")
         ((#:bazel-configuration conf)
          #~(begin
              #$conf
              ;; When building with CUDA, Bazel uses ldconfig and
              ;; complains that it can't open /etc/ld.so.cache.
              ;; So we fake ldconfig.
              (mkdir-p "/tmp/dummy-ldconfig")
              (symlink (which "true") "/tmp/dummy-ldconfig/ldconfig")
              (setenv "PATH"
                      (string-append "/tmp/dummy-ldconfig:"
                                     (getenv "PATH")))))
         ((#:bazel-arguments args)
          #~(append #$args
                    (list "--config=cuda"
                          (string-append "--action_env=CUDA_TOOLKIT_PATH="
                                         #$(this-package-input "cuda-toolkit"))
                          (string-append "--action_env=CUDNN_INSTALL_PATH="
                                         #$(this-package-input "cuda-toolkit-cudnn"))
                          (string-append "--action_env=TF_CUDA_PATHS="
                                         #$(this-package-input "cuda-toolkit")
                                         ","
                                         #$(this-package-input "cuda-toolkit-cudnn"))
                          (string-append "--action_env=TF_CUDA_VERSION="
                                         #$(version-major+minor
                                            (package-version (this-package-input "cuda-toolkit"))))
                          (string-append "--action_env=TF_CUDNN_VERSION="
                                         #$(version-major
                                            (package-version (this-package-input "cuda-toolkit-cudnn")))))))
         ((#:run-command cmd)
          #~(list
             (string-append "--output_path=" #$output)
             (string-append "--cpu="
                            #$(match (or (%current-target-system)
                                         (%current-system))
                                ("x86_64-linux"   "x86_64")
                                ("i686-linux"     "i686")
                                ("mips64el-linux" "mips64")
                                ("aarch64-linux"  "aarch64")
                                ;; Prevent errors when querying this
                                ;; package on unsupported platforms,
                                ;; e.g. when running "guix package
                                ;; --search="
                                (_                "UNSUPPORTED")))))
         ((#:phases phases)
          (with-imported-modules (source-module-closure '((guix build utils)
                                                          (guix build union)
                                                          (guix build gnu-build-system)
                                                          (guix-science build bazel-build-system)))
            #~(modify-phases #$phases
                (add-before 'configure 'patch-compiler-wrapper
                  (lambda _
                    (let ((bazel-out
                           (string-append (getenv "NIX_BUILD_TOP") "/output")))
                      (patch-shebang
                       (string-append bazel-out
                                      "/external/xla/third_party/tsl/third_party\
/gpus/crosstool/clang/bin/crosstool_wrapper_driver_is_not_gcc.tpl"))

                      ;; This wrapper insists on passing
                      ;; no-canonical-prefixes, which makes it
                      ;; impossible for GCC to find
                      ;; architecture-specific headers like
                      ;; bits/c++config.h.
                      (substitute* (string-append bazel-out
                                                  "/external/xla/third_party/tsl/third_party/gpus/crosstool/cc_toolchain_config.bzl.tpl")
                        (("\"-no-canonical-prefixes\",") "")))))
                ;; XXX: this should be a function of the features
                ;; supported by the given CUDA library version.  Our
                ;; CUDA 11 version does not support the virtual
                ;; architecture "compute_90", for example.
                (add-after 'configure 'set-cuda-capabilities
                  (lambda _
                    (setenv "TF_CUDA_COMPUTE_CAPABILITIES"
                            "sm_52,sm_60,sm_70,sm_80,compute_90")))
                (add-after 'set-cuda-capabilities 'configure-with-cuda
                  (lambda _
                    ;; When building with CUDA, Bazel uses ldconfig and
                    ;; complains that it can't open /etc/ld.so.cache.
                    ;; So we fake ldconfig.
                    (mkdir-p "/tmp/dummy-ldconfig")
                    (symlink (which "true") "/tmp/dummy-ldconfig/ldconfig")
                    (setenv "PATH"
                            (string-append "/tmp/dummy-ldconfig:"
                                           (getenv "PATH")))

                    ;; Bazel expects the GCC and CUDA toolchains to be
                    ;; under the same prefix.
                    (use-modules (guix build union))
                    (let ((toolchain (string-append (getenv "NIX_BUILD_TOP") "/toolchain")))
                      (union-build toolchain
                                   (cons
                                    #$(this-package-input "cuda-toolkit")
                                    (match '#$(standard-packages)
                                      (((labels directories . rest) ...)
                                       directories))))
                      (setenv "GCC_HOST_COMPILER_PREFIX"
                              (string-append toolchain "/bin"))
                      (setenv "GCC_HOST_COMPILER_PATH"
                              (string-append toolchain "/bin/gcc")))
                    (call-with-output-file ".jax_configure.bazelrc"
                      (lambda (port)
                        ;; Append at the end of this file
                        (seek port 0 SEEK_END)
                        (display
                         (string-append
                          "build --config=cuda\n"
                          "build:cuda --action_env TF_CUDA_COMPUTE_CAPABILITIES="
                          (getenv "TF_CUDA_COMPUTE_CAPABILITIES") "\n"
                          "build --action_env CUDA_TOOLKIT_PATH="
                          #$(this-package-input "cuda-toolkit") "\n"
                          "build --action_env CUDNN_INSTALL_PATH="
                          #$(this-package-input "cuda-toolkit-cudnn") "\n"
                          "build --action_env TF_CUDA_PATHS="
                          #$(this-package-input "cuda-toolkit")
                          ","
                          #$(this-package-input "cuda-toolkit-cudnn") "\n"
                          "build --action_env TF_CUDA_VERSION="
                          #$(version-major+minor
                             (package-version (this-package-input "cuda-toolkit"))) "\n"
                          "build --action_env TF_CUDNN_VERSION="
                          #$(version-major
                             (package-version (this-package-input "cuda-toolkit-cudnn"))))
                         port))))))))))
      (inputs
       (modify-inputs (package-inputs base)
         (append cuda-11.8 cudnn-8.9.1.23)))
      ;; For crosstool_wrapper_driver_is_not_gcc
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (append python-wrapper))))))

(define-public python-jaxlib-with-cuda11
  (package
    (inherit python-jaxlib/wheel-with-cuda11)
    (source #f)
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #false
      #:phases
      #~(modify-phases %standard-phases
          (delete 'unpack)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (mkdir-p "dist")
              (let ((wheel (car (find-files (assoc-ref inputs "python-jaxlib-with-cuda11")
                                            "jaxlib-.*\\.whl$"))))
                (install-file wheel "dist"))))
          ;; XXX: python-jaxlib/wheel-with-cuda11 builds libraries
          ;; without RUNPATH.
          (add-after 'install 'fix-rpath
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((libdir (string-append #$output "/lib"))
                    (rpath  (string-append #$output "/lib/python3.10/site-packages/jaxlib/mlir/_mlir_libs/:"
                                           (string-join (map (lambda (label+dir)
                                                               (string-append (cdr label+dir) "/lib"))
                                                             inputs)
                                                        ":"))))
                (for-each
                 (lambda (file)
                   (invoke "patchelf" "--set-rpath" rpath file)
                   ;; .so files should be executable
                   (chmod file #o555))
                 (find-files libdir ".*\\.so$"))))))))
    ;; XXX: for fix-rpath phase only
    (inputs
     `(("gcc:lib" ,gcc "lib")
       ,@(package-inputs python-jaxlib/wheel-with-cuda11)))
    (native-inputs
     (list patchelf python-jaxlib/wheel-with-cuda11))))

(define-public python-jax-with-cuda11
  (package
    (inherit python-jax)
    (name "python-jax-with-cuda11")
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-jax)
       (replace "python-jaxlib" python-jaxlib-with-cuda11)))))

(define-public tensorflow-with-cuda11
  (let ((base tensorflow))
    (package
      (inherit base)
      (name "tensorflow-with-cuda11")
      (version (package-version base))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:bazel _ #false)
          bazel-6.1)
         ((#:vendored-inputs-hash _)
          "02hypf92w8sp8f2bpc9x2xb0px18i0vsw5022xgwdq1l8vs0cap9")
         ((#:bazel-configuration conf)
          #~(begin
              #$conf
              ;; When building with CUDA, Bazel uses ldconfig and
              ;; complains that it can't open /etc/ld.so.cache.
              ;; So we fake ldconfig.
              (mkdir-p "/tmp/dummy-ldconfig")
              (symlink (which "true") "/tmp/dummy-ldconfig/ldconfig")
              (setenv "PATH"
                      (string-append "/tmp/dummy-ldconfig:"
                                     (getenv "PATH")))))
         ((#:bazel-arguments args)
          #~(append #$args
                    (list "--config=cuda"
                          "--local_ram_resources=8192"
                          "--action_env=TF_NEED_CUDA=1"
                          (string-append "--action_env=TF_CUDA_PATHS="
                                         #$(this-package-input "cuda-toolkit")
                                         ","
                                         #$(this-package-input "cuda-toolkit-cudnn"))
                          (string-append "--action_env=TF_CUDA_VERSION="
                                         #$(version-major+minor
                                            (package-version (this-package-input "cuda-toolkit"))))
                          (string-append "--action_env=TF_CUDNN_VERSION="
                                         #$(version-major
                                            (package-version (this-package-input "cuda-toolkit-cudnn")))))))
         ((#:phases phases)
          (with-imported-modules (source-module-closure '((guix build utils)
                                                          (guix build union)
                                                          (guix build gnu-build-system)
                                                          (guix-science build bazel-build-system)))
            #~(modify-phases #$phases
                (add-before 'configure 'patch-compiler-wrapper
                  (lambda _
                    (let ((bazel-out
                           (string-append (getenv "NIX_BUILD_TOP") "/output")))
                      (patch-shebang
                       "third_party/gpus/crosstool/clang/bin/crosstool_wrapper_driver_is_not_gcc.tpl")

                      ;; This wrapper insists on passing
                      ;; no-canonical-prefixes, which makes it
                      ;; impossible for GCC to find
                      ;; architecture-specific headers like
                      ;; bits/c++config.h.
                      (substitute* "third_party/gpus/crosstool/cc_toolchain_config.bzl.tpl"
                        (("\"-no-canonical-prefixes\",") "")))))
                ;; XXX: this should be a function of the features
                ;; supported by the given CUDA library version.  Our
                ;; CUDA 11 version does not support the virtual
                ;; architecture "compute_90", for example.
                (add-after 'configure 'set-cuda-capabilities
                  (lambda _
                    (setenv "TF_CUDA_COMPUTE_CAPABILITIES"
                            "sm_52,sm_60,sm_70,sm_80,compute_90")))
                (add-after 'set-cuda-capabilities 'configure-with-cuda
                  (lambda _
                    ;; When building with CUDA, Bazel uses ldconfig and
                    ;; complains that it can't open /etc/ld.so.cache.
                    ;; So we fake ldconfig.
                    (mkdir-p "/tmp/dummy-ldconfig")
                    (symlink (which "true") "/tmp/dummy-ldconfig/ldconfig")
                    (setenv "PATH"
                            (string-append "/tmp/dummy-ldconfig:"
                                           (getenv "PATH")))
                    ;; Bazel expects the GCC and CUDA toolchains to be
                    ;; under the same prefix.
                    (use-modules (guix build union))
                    (let ((toolchain (string-append (getenv "NIX_BUILD_TOP") "/toolchain")))
                      (union-build toolchain
                                   (cons
                                    #$(this-package-input "cuda-toolkit")
                                    (match '#$(standard-packages)
                                      (((labels directories . rest) ...)
                                       directories))))
                      (setenv "GCC_HOST_COMPILER_PREFIX"
                              (string-append toolchain "/bin"))
                      (setenv "GCC_HOST_COMPILER_PATH"
                              (string-append toolchain "/bin/gcc")))))
                (replace 'install
                  (lambda _
                    ;; Install library
                    (mkdir-p #$output)
                    (invoke "tar" "-xf"
                            "bazel-bin/tensorflow/tools/lib_package/libtensorflow.tar.gz"
                            "-C" #$output)

                    ;; Write pkgconfig file
                    (mkdir-p (string-append #$output "/lib/pkgconfig"))
                    (call-with-output-file (string-append #$output "/lib/pkgconfig/tensorflow.pc")
                      (lambda (port)
                        (format port "\
Name: TensorFlow CUDA
Version: ~a
Description: Library for computation using data flow graphs for scalable machine learning
Requires:
Libs: -L~a/lib -ltensorflow
Cflags: -I~a/include/tensorflow
" #$version #$output #$output)))

                    ;; Install python bindings
                    ;; Build the source code, then copy it to the "python" output.
                    ;;
                    ;; TODO: build_pip_package includes symlinks so we must
                    ;; dereference them.
                    (let ((here (string-append (getcwd) "/dist")))
                      (invoke "bazel-bin/tensorflow/tools/pip_package/build_pip_package"
                              "--gpu"
                              "--src" here)
                      (copy-recursively here #$output:python)))))))))
      (inputs
       (modify-inputs (package-inputs base)
         (replace "python-jax" python-jax-with-cuda11)
         ;; See compatibility matrix here:
         ;; https://www.tensorflow.org/install/source#gpu
         (append cuda-11.8 cudnn-8.6)))
      ;; For crosstool_wrapper_driver_is_not_gcc
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (append python-wrapper))))))

(define-public python-tensorflow-with-cuda11
  (package
    (inherit tensorflow-with-cuda11)
    (name "python-tensorflow-with-cuda11")
    (source #f)
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #false
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (mkdir-p "source")
              (copy-recursively #$tensorflow-with-cuda11:python "source")
              (chdir "source")
              ;; XXX: the "python" output of the tensorflow package
              ;; contains broken symlinks.
              (delete-file-recursively "third_party/eigen3")
              (mkdir-p "third_party/eigen3")
              (copy-recursively #$eigen-for-python-ml-dtypes
                                "third_party/eigen3")
              (with-output-to-file "third_party/eigen3/LICENSE"
                (lambda () (display "")))))
          (add-after 'unpack 'relax-dependencies
            (lambda _
              (substitute* "setup.py"
                ;; We don't have tensorflow-io yet
                (("'tensorflow-io-gcs-filesystem.*") "None,")
                (("'platform_system.*") "")
                ;; Versions above 0.4 break tests, but that's okay
                ;; because we aren't running them.
                (("gast >= 0.2.1, <= 0.4.0") "gast >= 0.2.1")
                ;; Drop all of tensorboard and tensorflow_estimator
                (("'(tensorboard|tensorflow_estimator) >.*',") " None,")
                ;; Our clang bindings have a different name.
                (("libclang") "clang")
                ;; No tensorboard, sorry.
                (("standard_or_nightly\\('tensorboard = tensorboard.main:run_main', None\\),") "")
                (("'import_pb_to_tensorboard = tensorflow.python.tools.import_pb_to_tensorboard:main',") "")
                ;; We don't have tensorflow-estimator yet.
                (("'estimator_ckpt_converter = '") "")
                (("'tensorflow_estimator.python.estimator.tools.checkpoint_converter:main',") ""))))
          ;; XXX: this is really ugly, but many shared objects cannot
          ;; find libtensorflow_framework.so.2 and libbfloat16.so.so
          (add-after 'unpack 'find-tensorflow-libraries
            (lambda _
              ;; XXX: not all .so files need this.
              (let ((libraries (find-files "." ".*\\.so$")))
                (for-each (lambda (lib)
                            (make-file-writable lib)
                            (system (format #false
                                            "patchelf --set-rpath ~a:~a:$(patchelf --print-rpath ~a) ~a"
                                            ;; for libtensorflow_framework.so.2
                                            (string-append
                                             #$(this-package-input "tensorflow-with-cuda11")
                                             "/lib")
                                            ;; for libbfloat16.so.so
                                            (string-append
                                             #$output
                                             "/lib/python3.10/site-packages/tensorflow/tsl/python/lib/core/")
                                            lib lib)))
                          libraries))))
          (add-after 'install 'install-missing-libraries
            (lambda _
              ;; libtensorflow_cc.so.2 is not installed.  See
              ;; https://github.com/tensorflow/tensorflow/issues/60326.
              (let ((dir (string-append #$output "/lib/python3.10/site-packages/tensorflow/"))
                    (lib (string-append "libtensorflow_cc.so." #$(package-version this-package))))
                (install-file (string-append "tensorflow/" lib) dir)
                (with-directory-excursion dir
                  (symlink lib "libtensorflow_cc.so.2"))))))))
    (outputs '("out"))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs tensorflow-with-cuda11)
       (append python-clang-13 python-keras-for-tensorflow)))
    (inputs (list tensorflow-with-cuda11))
    (native-inputs
     (list eigen-for-python-ml-dtypes
           patchelf
           `(,tensorflow-with-cuda11 "python")))))
