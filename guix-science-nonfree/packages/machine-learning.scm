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
  #:use-module (guix packages)
  #:use-module (gnu packages)
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
