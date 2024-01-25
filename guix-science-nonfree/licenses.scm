;;;
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2021 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2024 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (guix-science-nonfree licenses)
  #:export (nonfree artistic-1.0 cc-by-nd4.0))

;; Guix does not export the license record constructor.
(define license (@@ (guix licenses) license))

(define* (nonfree uri #:optional (comment ""))
  "Return a nonfree license, whose full text can be found
at URI, which may be a file:// URI pointing the package's tree."
  (license "Nonfree"
           uri
           (string-append
            "This a nonfree license.  Check the URI for details.  "
            comment)))

(define artistic-1.0
  (license "Artistic 1.0"
           "https://opensource.org/license/artistic-1-0/"
           "Artistic License 1.0"))

(define cc-by-nd4.0
  (license "CC-BY-ND 4.0"
           "http://creativecommons.org/licenses/by-nd/4.0/"
           "Creative Commons Attribution-NoDerivatives 4.0 International"))

