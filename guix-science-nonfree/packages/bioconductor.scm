;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021, 2022 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2020 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
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

(define-module (guix-science-nonfree packages bioconductor)
  #:use-module ((guix licenses-nonfree) #:prefix nonfree:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages statistics))

(define-public r-motifdb
  (package
    (name "r-motifdb")
    (version "1.38.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "MotifDb" version))
              (sha256
               (base32
                "1cyfz0l0yvdii3idaiq5w39yzxlzfpifa4v5pv7hdjfjj83a8rbi"))))
    (properties `((upstream-name . "MotifDb")))
    (build-system r-build-system)
    (propagated-inputs (list r-biocgenerics
                             r-biostrings
                             r-genomicranges
                             r-iranges
                             r-rtracklayer
                             r-s4vectors
                             r-splitstackshape))
    (native-inputs (list r-knitr))
    (home-page "https://bioconductor.org/packages/MotifDb")
    (synopsis "Annotated collection of protein-DNA binding sequence motifs")
    (description
     "This package provides more than 9900 annotated position
frequency matrices from 14 public sources, for multiple organisms.")
    ;; It's complicated... the data from public sources are under
    ;; different licenses; some are not licensed at all.
    (license
     (nonfree "https://bioconductor.org/packages/release/bioc/licenses/MotifDb/LICENSE"))))

(define-public r-rankprod
  (package
    (name "r-rankprod")
    (version "3.22.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "RankProd" version))
              (sha256
               (base32
                "0rq14h9kjj84krgvfa09jbc5s8yks37fjbcv8z88daaib0j3fq2d"))))
    (properties `((upstream-name . "RankProd")))
    (build-system r-build-system)
    (propagated-inputs (list r-gmp r-rmpfr))
    (home-page "https://bioconductor.org/packages/RankProd")
    (synopsis "Identify differentially expressed genes")
    (description
     "Non-parametric method for identifying differentially expressed (up- or down-
regulated) genes based on the estimated percentage of false predictions (pfp).
The method can combine data sets from different origins (meta-analysis) to
increase the power of the identification.")
    (license
     (nonfree "https://bioconductor.org/packages/release/bioc/licenses/RankProd/LICENSE"
              "Non-commercial"))))
