;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages machine-learning)
  #:use-module (guix-science-nonfree packages cuda))

(define-public gloo-cuda
  (package
    (inherit gloo)
    (name "gloo-cuda")
    (arguments
     (list
      #:tests? #false ;see linker error below
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
       (append cuda)))
    ;; When building with CUDA 10 we cannot use any more recent GCC
    ;; than version 8.
    (native-inputs
     (modify-inputs (package-native-inputs gloo)
       (append gcc-8)))))

(define-public python-pytorch-with-cuda
  (package
    (inherit python-pytorch)
    (name "python-pytorch-with-cuda")
    (source
     (origin
       (inherit (package-source python-pytorch))
       (snippet
        '(begin
           ;; We're using a separately built gloo, so this
           ;; target does not exist.
           (substitute* "cmake/Dependencies.cmake"
             (("add_dependencies\\(gloo_cuda nccl_external\\)") ""))
           ;; Tests cannot be built due to googletest linker error
           (substitute* "modules/module_test/CMakeLists.txt"
             (("set\\(BUILD_TEST ON\\)")
              "set(BUILD_TEST OFF)"))
           (substitute* '("c10/CMakeLists.txt"
                          "c10/cuda/CMakeLists.txt")
             (("add_subdirectory\\(test\\)") "")
             (("add_subdirectory\\(benchmark\\)") ""))
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
    (name "python-pytorch-with-cuda")
    (arguments
     (substitute-keyword-arguments (package-arguments python-pytorch)
       ((#:configure-flags flags '())
        '(list "-DBUILD_TESTS=OFF" "-DBUILD_TEST=OFF"))))
    (inputs
     (modify-inputs (package-inputs python-pytorch)
       (append cuda)
       (replace "gloo" gloo-cuda)
       ;; These need to be rebuilt because we're using a different
       ;; compiler.
       (replace "googletest"
         (package
           (inherit googletest)
           (native-inputs
            (modify-inputs (package-native-inputs googletest)
              (append gcc-8)))))
       (replace "googlebenchmark"
         (package
           (inherit googlebenchmark)
           (native-inputs
            (modify-inputs (package-native-inputs googlebenchmark)
              (append gcc-8)))))))
    ;; When building with CUDA 10 we cannot use any more recent GCC
    ;; than version 8.
    (native-inputs
     (modify-inputs (package-native-inputs python-pytorch)
       (append gcc-8)))))
