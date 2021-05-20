;;;
;;; Copyright © 2021 Lars-Dominik Braun <ldb@leibniz-psychology.org>
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

(define-module (guix-science-nonfree packages cran)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix-science-nonfree licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages statistics))

;; The source code itself is free, but the dataset is non-free.
(define-public r-datasaurus
  (package
    (name "r-datasaurus")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "datasauRus" version))
        (sha256
          (base32
            "1w1yhwwrmh95bklacz44wjwynxd8cj3z8b9zvsnzmk18m5a4k0fl"))))
    (properties `((upstream-name . "datasauRus")))
    (build-system r-build-system)
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://github.com/lockedata/datasauRus")
    (synopsis "Datasets from the Datasaurus Dozen")
    (description
     "The Datasaurus Dozen is a set of datasets with the same summary
statistics.  They retain the same summary statistics despite having radically
different distributions.  The datasets represent a larger and quirkier object
lesson that is typically taught via Anscombe's Quartet (available in the
'datasets' package).  Anscombe's Quartet contains four very different
distributions with the same summary statistics and as such highlights the value
of visualisation in understanding data, over and above summary statistics.  As
well as being an engaging variant on the Quartet, the data is generated in a
novel way.  The simulated annealing process used to derive datasets from the
original Datasaurus is detailed in \"Same Stats, Different Graphs: Generating
Datasets with Varied Appearance and Identical Statistics through Simulated
Annealing\" @url{doi:10.1145/3025453.3025912}.")
    (license
     (list
      (nonfree "https://github.com/lockedata/datasauRus/blob/master/LICENSE")
      license:expat))))

