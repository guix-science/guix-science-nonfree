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

(define-module (guix-science-nonfree packages cran)
  #:use-module (guix-science-nonfree licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages gcc))

(define-public r-akima
  (package
    (name "r-akima")
    (version "0.6-3.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "akima" version))
       (sha256
        (base32 "0gmdh5kvq455k82pvdsqbkk69bi7g0960jq5rcl3cbhxm297arcm"))))
    (properties `((upstream-name . "akima")))
    (build-system r-build-system)
    (propagated-inputs (list r-sp))
    (native-inputs (list gfortran))
    (home-page "https://cran.r-project.org/package=akima")
    (synopsis "Interpolation of irregularly and regularly spaced data")
    (description
     "This package provides several cubic spline interpolation methods of H.
Akima for irregular and regular gridded data are available through this
package, both for the bivariate case and univariate case.  Linear
interpolation of irregular gridded data is also covered.  A bilinear
interpolator for regular grids was also added for comparison with the bicubic
interpolator on regular grids.")
    (license
     (nonfree "https://cran.r-project.org/web/packages/akima/LICENSE"
              "Non-commercial, ACM"))))
