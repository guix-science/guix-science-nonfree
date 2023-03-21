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

(define-module (guix-science-nonfree packages mpi)
  #:use-module (guix)
  #:use-module (gnu packages mpi)
  #:use-module (guix-science-nonfree packages cuda)
  #:use-module (guix-science-nonfree packages linux))

(define-public openmpi-cuda
  (package
    (inherit openmpi)
    (name "openmpi-cuda")
    (arguments
     ;; TODO: Check whether UCX is built with gdrcopy:
     ;; <https://www.open-mpi.org/faq/?category=buildcuda>.
     (substitute-keyword-arguments (package-arguments openmpi)
       ((#:configure-flags flags #~'())
        #~(append (list (string-append "--with-cuda="
                                       #$(this-package-input "cuda-toolkit"))
                        "--enable-mpi-ext=cuda")
                  #$flags))))
    (inputs (modify-inputs (package-inputs openmpi)
              (append cuda)
              (replace "psm2" psm2-cuda)))
    (synopsis "MPI-3 implementation, with CUDA support")))
