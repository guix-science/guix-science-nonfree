;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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

(define-module (guix-science-nonfree packages parallel)
  #:use-module (guix)
  #:use-module (gnu packages parallel)
  #:use-module (guix-science-nonfree packages cuda))

(define-public slurm-with-cuda
  (package
    (inherit slurm)
    (name "slurm-with-cuda")
    (inputs (modify-inputs (package-inputs slurm)
              (prepend cuda)))
    (arguments
     (substitute-keyword-arguments (package-arguments slurm)
       ;; Necessary because libnvidia-ml.so.1 does not actually exist.
       ((#:validate-runpath? _ #f)
        #false)
       ((#:configure-flags flags '())
        #~(append (list (string-append "--with-nvml="
                                       #$(this-package-input "cuda-toolkit")))
              #$flags))))))
