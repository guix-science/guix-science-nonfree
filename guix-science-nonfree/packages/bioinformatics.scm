;;; Copyright © 2016-2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2015-2023 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2023 Navid Afkhami <navid.afkhami@mdc-berlin.de>
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

(define-module (guix-science-nonfree packages bioinformatics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix-science-nonfree licenses)
  #:use-module (guix-science-nonfree packages cuda)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (past packages boost)
  #:use-module (past packages perl)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))


(define (other-perl-package-name other-perl)
  "Return a procedure that returns NAME with a new prefix for
OTHER-PERL instead of \"perl-\", when applicable."
  (let ((other-perl-version (version-major+minor
                             (package-version other-perl))))
    (lambda (name)
      (if (string-prefix? "perl-" name)
          (string-append "perl" other-perl-version "-"
                         (string-drop name
                                      (string-length "perl-")))
          name))))

(define-public (package-for-other-perl other-perl pkg)
  ;; This is a procedure to replace PERL by OTHER-PERL, recursively.
  ;; It also ensures that rewritten packages are built with OTHER-PERL.
  (let* ((rewriter (package-input-rewriting `((,perl . ,other-perl))
                                            (other-perl-package-name other-perl)
                                            #:deep? #false))
         (new (rewriter pkg)))
    (package (inherit new)
      (arguments `(#:perl ,other-perl
                   #:tests? #f ; oh well...
                   ,@(package-arguments new))))))


(define-public amd
  (package
    (name "amd")
    (version "0")
    (source
     (origin
       (method url-fetch)
       (uri "https://doi.org/10.1371/journal.pone.0024576.s004")
       (sha256
        (base32 "1134yfcrgymhpzg2423a9z5j5nlcrfizfb9hm4kn36zcpclh4dq3"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false ;there are none
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (invoke "unrar-free" "-x" #$source)))
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "source")))
          (delete 'configure)
          (replace 'install
            (lambda _
              (install-file "AMD" (string-append #$output "/bin")))))))
    (native-inputs
     (list unrar-free))
    (home-page "https://doi.org/10.1371/journal.pone.0024576")
    (synopsis
     "Automated motif discovery using stepwise refinement of gapped consensuses")
    (description "Motif discovery is essential for deciphering
regulatory codes from high throughput genomic data, such as those from
ChIP-chip/seq experiments.  AMD is an automated tool that allows for
de novo discovery of transcription factor binding sites, regardless of
whether the motifs are long or short, gapped or contiguous.")
    (license (nonfree "No license declared"))))

;; TODO: this is not reproducible.  The /bin/bart executable differs
;; across builds in size and offsets.

;; This package is free software, but due to using CUDA it cannot be
;; added to the "guix-science" channel (or Guix itself).
(define-public bart-with-cuda
  (package
    (name "bart-with-cuda")
    (version "0.8.00")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mrirecon/bart")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05lcf7c3g7ms5h82bw1mi4kzkdv5wpqi1zrfhqfkgbcpd3irj6aq"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "utest"
      #:make-flags #~(list
                      (string-append "PREFIX=" #$output)
                      "CUDA=1"
                      (string-append "CUDA_BASE=" #$(this-package-input "cuda-toolkit"))
                      "OPENBLAS=1"
                      "SCALAPACK=1"
                      (string-append "BLAS_BASE=" #$(this-package-input "openblas"))
                      (string-append "FFTW_BASE=" #$(this-package-input "fftw")))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'patch-/bin/bash
            (lambda _
              (substitute* "tests/pics.mk"
                (("/bin/bash") (which "bash"))))))))
    (inputs
     (list cuda-11.7
           fftw
           fftwf
           libpng
           openblas
           python
           scalapack))
    (native-inputs
     (list doxygen
           util-linux))                 ;for flock
    (home-page "https://mrirecon.github.io/bart/")
    (synopsis "Toolbox for computational magnetic resonance imaging")
    (description "The Berkeley Advanced Reconstruction Toolbox (BART) is an
image-reconstruction framework for Computational Magnetic Resonance Imaging.
The tools in this software implement various reconstruction algorithms for
Magnetic Resonance Imaging.")
    (license license:bsd-3)))

(define-public bcl2fastq-2.18
  (package
    (name "bcl2fastq")
    (version "2.18.0.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://support.illumina.com/content/"
                                  "dam/illumina-support/documents/downloads/"
                                  "software/bcl2fastq/bcl2fastq2-v"
                                  (string-join (string-split version #\.) "-")
                                  "-tar.zip"))
              (sha256
               (base32
                "0anshb1qvpzm373q338qgr0gs1bjpw4ssyysl4gh7nfwidzmca25"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DBCL2FASTQ_VERSION:STRING=" #$version)
              "-DBCL2FASTQ_NAME_SHORT:STRING=bcl2fastq"
              "-DBCL2FASTQ_NAME_LONG:STRING=BCL to FASTQ file converter"
              "-DBCL2FASTQ_COPYRIGHT:STRING=Copyright (c) 2007-2016 Illumina, Inc."
              (string-append "-DBCL2FASTQ_SOURCE_DIR:STRING=" (getcwd) "/bcl2fastq/src"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "unzip" source)
              (invoke "tar" "-xvf"
                      (string-append "bcl2fastq2-v"
                                     #$version ".tar.gz"))))
          (add-after 'unpack 'chdir
            (lambda _ (chdir "bcl2fastq/src")))
          (add-after 'chdir 'patch-stuff
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Update for boost 1.54 -> 1.56
              (substitute* "cxx/lib/io/Xml.cpp"
                (("xml_writer_make_settings\\(")
                 "xml_writer_make_settings<ptree::key_type>("))
              (substitute* "cxx/include/common/Logger.hh"
                (("#include <ios>" m)
                 (string-append "#include <iostream>\n" m)))
              ;; Do not use bundled libraries
              (substitute* "cmake/cxxConfigure.cmake"
                (("\"\\$\\{LIBEXSLT_LIBRARIES\\}\"")
                 (string-append (assoc-ref inputs "libxslt")
                                "/lib/libexslt.so"))
                (("find_library_redist\\(LIBXSLT .*")
                 "bcl2fastq_find_library(LIBXSLT libxslt/xsltconfig.h xslt)\n")
                (("find_library_redist\\(LIBXML2 .*")
                 "bcl2fastq_find_library(LIBXML2 libxml/xpath.h xml2)\n")
                (("find_library_redist\\(LIBEXSLT .*")
                 "bcl2fastq_find_library(LIBEXSLT libexslt/exslt.h exslt)\n")
                (("redist_package") "#")
                (("^  +\"--prefix=.*") ""))
              ;; Work around broken version checking
              (substitute* "CMakeLists.txt"
                (("BCL2FASTQ_LIBXML2_VERSION 2.7.8")
                 (string-append "BCL2FASTQ_LIBXML2_VERSION "
                                #$(package-version (this-package-input "libxml2")) ))
                (("BCL2FASTQ_LIBXSLT_VERSION 1.1.26")
                 (string-append "BCL2FASTQ_LIBXSLT_VERSION "
                                #$(package-version (this-package-input "libxslt"))))))))))
    (inputs
     (list boost-1.58 libxml2 libxslt zlib))
    (native-inputs (list unzip))
    (home-page "http://support.illumina.com/downloads/bcl2fastq_conversion_software.html")
    (synopsis "Convert files in BCL format to FASTQ")
    (description
     "bcl2fastq is conversion software, which can be used to both
demultiplex data and convert BCL files to FASTQ files.")
    (license (nonfree "http://support.illumina.com/content/dam/\
illumina-support/documents/documentation/software_documentation/\
bcl2fastq/bcl2fastq2-v2-16-EULA.pdf"
                      "This is an extremely restrictive license and it
would be better to avoid using this proprietary program.  I encourage
people to write a free software alternative rather than using this
tool."))))

(define-public bcl2fastq
  (package (inherit bcl2fastq-2.18)
    (version "2.20.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://webdata2:webdata2@ussd-ftp.illumina.com"
                                  "/downloads/software/bcl2fastq/bcl2fastq2-v"
                                  (string-join (string-split version #\.) "-")
                                  "-tar.zip"))
              (sha256
               (base32
                "1qqz217ipsv5wq28wd5pp3jl870i5dbdxq3dwi6ali6hcx3h9lwd"))))
    (arguments
     (substitute-keyword-arguments (package-arguments bcl2fastq-2.18)
       ((#:configure-flags flags)
        #~(list (string-append "-DBCL2FASTQ_VERSION:STRING=" #$version)
                "-DBCL2FASTQ_NAME_SHORT:STRING=bcl2fastq"
                "-DBCL2FASTQ_NAME_LONG:STRING=BCL to FASTQ file converter"
                "-DBCL2FASTQ_COPYRIGHT:STRING=Copyright (c) 2007-2018 Illumina, Inc."
                (string-append "-DBCL2FASTQ_SOURCE_DIR:STRING=" (getcwd) "/bcl2fastq/src")))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (invoke "unzip" source)
                (invoke "tar" "-xvf"
                        (string-append "bcl2fastq2-v" #$version
                                       ".422-Source.tar.gz"))
                (substitute* "bcl2fastq/src/cxx/include/common/Logger.hh"
                  (("#include <ios>" m)
                   (string-append m "\n#include <iostream>")))))
            (add-after 'install 'rename-/bin/test
              (lambda _
                (rename-file (string-append #$output "/bin/test")
                             (string-append #$output "/bin/bcl2fastq-test"))))))))))

(define-public bcl2fastq1
  (package (inherit bcl2fastq-2.18)
    (name "bcl2fastq1")
    (version "1.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://webdata:webdata@ussd-ftp.illumina.com/"
                                  "Downloads/Software/bcl2fastq/bcl2fastq-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0hwkmnzvq5l7w9dq7wgb5vvrb86l7psxqw24abradnxxdz849bhk"))
              (snippet
               `(begin
                  (delete-file "redist/boost_1_44_0.tar.gz")
                  (delete-file "redist/cmake-2.8.4.tar.gz")))))
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DBOOST_ROOT="
			                 #$(this-package-input "boost"))
	          "-DBoost_DEBUG=ON"
	          ;; Needed for later versions of CMake with older versions of Boost
	          "-DBoost_NO_BOOST_CMAKE=ON")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'set-paths 'hide-default-gcc
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((gcc (assoc-ref inputs "gcc")))
                 ;; Remove the default GCC from CPLUS_INCLUDE_PATH to prevent
                 ;; conflicts with the GCC 5 input.
                 (setenv "CPLUS_INCLUDE_PATH"
                         (string-join
                          (delete (string-append gcc "/include/c++")
                                  (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                          ":")))))
           (add-after 'unpack 'chdir (lambda _ (chdir "src")))
           (add-after 'chdir 'fix-includes
             (lambda _
               (substitute* "c++/include/common/FileConversion.hh"
                 (("#pragma once" line)
                  (string-append line "\n#include <stdint.h>")))
               (substitute* "c++/include/demultiplex/BarcodeTranslationTable.hh"
                 (("^namespace casava" line)
                  (string-append "#include <stdint.h>\n" line)))))
           (add-after 'install 'wrap-perl-scripts
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Make sure perl scripts finds all perl inputs at runtime.
               (for-each (lambda (prog)
                           (wrap-program (string-append #$output "/bin/" prog)
                             `("PERL5LIB" ":" prefix
                               (,(getenv "PERL5LIB")))))
                         '("configureBclToFastq.pl"
                           "configureQseqToFastq.pl"
                           "configureValidation.pl")))))))
    (native-inputs
     `(("gcc@4" ,gcc-4.9)))
    ;; We need the older version of Boost although this could be built
    ;; with 1.55 with only minor changes.  The reason is option
    ;; parsing, which only bites us at runtime.
    (inputs
     (list boost-1.44
           libxml2
           libxslt
           (package-for-other-perl perl-5.14 perl-xml-simple)
           (package-for-other-perl perl-5.14 perl-xml-parser)
           perl-5.14
           zlib))))

(define-public bioprospector
  (package
    (name "bioprospector")
    (version "2004")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://motif.stanford.edu/"
                                  "distributions/bioprospector/"
                                  "BioProspector." version ".zip"))
              (sha256
               (base32
                "0yhdg3cp1a7hniw89cfq6jmj88zqjbzkiz54j8gk418m8l7inika"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false                   ;there is no check target
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (invoke "gcc" "-o" "BioProspector" "-lm"
                      "BioProspector.2004.c")))
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (install-file "BioProspector" bin)))))))
    (native-inputs (list unzip))
    (home-page "https://motif.stanford.edu/distributions/bioprospector/")
    (synopsis "Discover conserved transcription factor binding sites")
    (description "BioProspector discovers conserved transcription
factor binding sites among upstream sequences from co-regulated
genes.")
    (license (nonfree "Academic use only."))))

(define-public meme-4
  (package
    (name "meme")
    (version "4.11.3_1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://meme-suite.org/meme-software/"
                                  (car (string-split version #\_))
                                  "/meme_" version ".tar.gz"))
              (sha256
               (base32
                "08df4wgiz1baq3749slpmr7df0hg3q4i3cdvap97xw063kx2d9gc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;#:make-flags '(list "CFLAGS=-O2 -g -fcommon")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths-to-tools
            (lambda _
              (substitute* "src/utils.c"
                (("\"hostname")
                 (string-append "\"" (which "hostname"))))))
          (add-after 'unpack 'remove-unused-tests
            (lambda _
              ;; We don't build the web server stuff, so we don't need
              ;; to run the tests for that either.
              (substitute* "tests/scripts/Makefile.in"
                (("tomtom.test") ""))))
          (add-before 'configure 'check-perl-dependencies
            (lambda _
              (invoke "perl" "./scripts/dependencies.pl")))
          (add-after 'install 'wrap-perl-scripts
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Make sure perl scripts finds all perl inputs at runtime.
              (for-each (lambda (prog)
                          (wrap-program (string-append #$output "/bin/" prog)
                            `("PERL5LIB" ":" prefix
                              (,(getenv "PERL5LIB")))))
                        '("ama-qvalues"
                          "beeml2meme"
                          "chen2meme"
                          "dreme_xml_to_html"
                          "dreme_xml_to_txt"
                          "elm2meme"
                          "fasta-center"
                          "fasta-fetch"
                          "fasta-grep"
                          "fasta-make-index"
                          "fasta-most"
                          "fasta-subsample"
                          "fasta-unique-names"
                          "hart2meme-bkg"
                          "hartemink2psp"
                          "iupac2meme"
                          "jaspar2meme"
                          "mast_xml_to_html"
                          "mast_xml_to_txt"
                          "matrix2meme"
                          "meme-chip"
                          "meme-rename"
                          "meme_xml_to_html"
                          "nmica2meme"
                          "priority2meme"
                          "psp-gen"
                          "rna2meme"
                          "rsat-retrieve-seq"
                          "rsat-supported-organisms"
                          "scpd2meme"
                          "sites2meme"
                          "taipale2meme"
                          "tamo2meme"
                          "tomtom_xml_to_html"
                          "transfac2meme"
                          "uniprobe2meme")))))))
    (native-inputs (list gcc-8))
    (inputs
     (list perl
           perl-file-which
           perl-html-parser
           perl-html-template
           perl-xml-simple
           perl-xml-compile
           perl-xml-compile-wsdl11
           perl-xml-parser
           python-2                     ;only works with Python 2
           libxml2
           libxslt
           openmpi
           ghostscript
           inetutils                    ;for "hostname"
           zlib))
    (propagated-inputs
     ;; "which" must be propagated because of the weird way it is used
     ;; in "src/exec_parallel.c".  The buffer "cmd_len" is arranged to
     ;; be 6 characters longer than the argument, just enough for the
     ;; string "which ".  I don't want to mess with pointers and
     ;; buffer lengths just to hardcode a path to the "which"
     ;; executable.
     (list which))
    (home-page "http://www.tbi.univie.ac.at/RNA/index.html")
    (synopsis "Motif-based sequence analysis tools")
    (description
     "The MEME Suite allows the biologist to discover novel motifs in
collections of unaligned nucleotide or protein sequences, and to
perform a wide variety of other motif-based analyses.

The MEME Suite supports motif-based analysis of DNA, RNA and protein
sequences.  It provides motif discovery algorithms using both
probabilistic and discrete models, which have complementary strengths.
It also allows discovery of motifs with arbitrary insertions and
deletions (GLAM2).  In addition to motif discovery, the MEME Suite
provides tools for scanning sequences for matches to motifs (FIMO,
MAST and GLAM2Scan), scanning for clusters of motifs (MCAST),
comparing motifs to known motifs (Tomtom), finding preferred spacings
between motifs (SpaMo), predicting the biological roles of
motifs (GOMo), measuring the positional enrichment of sequences for
known motifs (CentriMo), and analyzing ChIP-seq and other large
datasets (MEME-ChIP).")
    (license (nonfree "http://meme-suite.org/doc/copyright.html"
                      "license forbids commercial usage"))))

(define-public meme
  (package
    (inherit meme-4)
    (name "meme")
    (version "5.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://meme-suite.org/meme/meme-software/"
                                  version
                                  "/meme-" version ".tar.gz"))
              (sha256
               (base32
                "0lq1b03jb508y1pp6wsydrdrps8ibqjjqyjf7ibg4nw55cf039nd"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths-to-tools
            (lambda _
              (substitute* "src/utils.c"
                (("\"hostname")
                 (string-append "\"" (which "hostname"))))))
          (add-after 'unpack 'remove-bad-tests
            (lambda _
              ;; We don't build the web server stuff, so we don't need
              ;; to run the tests for that either.
              (substitute* "tests/scripts/Makefile.in"
                (("tomtom.test") "")
                ;; These fail because sysfs is not mounted in the
                ;; build container.
                (("meme.test") "")
                (("meme-chip.test") "")
                ;; The probabilities differ slightly
                (("xstreme.test") ""))))
          (add-after 'install 'wrap-perl-scripts
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Make sure perl scripts finds all perl inputs at runtime.
              (for-each (lambda (prog)
                          (wrap-program (string-append #$output "/bin/" prog)
                            `("PERL5LIB" ":" prefix
                              (,(getenv "PERL5LIB")))))
                        '("meme-chip"
                          "xstreme")))))))
    (inputs
     (list perl
           perl-file-which
           perl-html-parser
           perl-html-template
           perl-xml-simple
           perl-xml-compile
           perl-xml-compile-wsdl11
           perl-xml-parser
           python
           libxml2
           libxslt
           openmpi
           ghostscript
           inetutils                    ;for "hostname"
           zlib))
    (propagated-inputs
     ;; "which" must be propagated because of the weird way it is used
     ;; in "src/exec_parallel.c".  The buffer "cmd_len" is arranged to
     ;; be 6 characters longer than the argument, just enough for the
     ;; string "which ".  I don't want to mess with pointers and
     ;; buffer lengths just to hardcode a path to the "which"
     ;; executable.
     (list which))))

(define-public rmats-turbo
  (package
    (name "rmats-turbo")
    (version "4.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Xinglab/rmats-turbo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02ygjng1rc3k4daw3hrg102797dlvkcisnmwlhn1qbk0m3lg8dcb"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules
      `(,@%gnu-build-system-modules
        (guix build python-build-system))
      #:modules
      '((guix build gnu-build-system)
        ((guix build python-build-system) #:prefix python:)
        (guix build utils))
      #:tests? #false ;there is no check target
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              "FC=gfortran"
              (string-append "INSTALL_PATH=" #$output "/bin"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "CPLUS_INCLUDE_PATH"
                      (string-append (assoc-ref inputs "bamtools")
                                     "/include/bamtools:"
                                     (or (getenv "CPLUS_INCLUDE_PATH") "")))
              (substitute* "rMATS_pipeline/setup.py"
                (("'-Wl,-static',") ""))))
          (add-before 'build 'chdir
            (lambda _ (chdir "rMATS_C")))
          (add-before 'install 'prepare-install
            (lambda _ (mkdir-p (string-append #$output "/bin"))))
          (add-after 'install 'build-pipeline
            (lambda _
              (chdir "../rMATS_pipeline")
              (invoke "python" "setup.py" "build_ext")))
          (add-after 'build-pipeline 'install-pipeline
            (assoc-ref python:%standard-phases 'install)))))
    (inputs
     (list bamtools
           gsl
           gfortran
           (list gfortran "lib")
           lapack
           openblas
           python-wrapper
           zlib))
    (native-inputs
     (list python-cython))
    (home-page "https://github.com/Xinglab/rmats-turbo")
    (synopsis "Detect differential alternative splicing events")
    (description "rMATS-turbo is a computational tool to detect
differential alternative splicing events from RNA-Seq data.")
    ;; The intent here is to disallow commercial use, but the license
    ;; is simply unclear.  The C part links with GSL and the directory
    ;; containing the sources contains a copy of the GPL version 2.
    ;; The root directory contains a copy of a BSD license, but is
    ;; prefixed with a restriction that contradicts the license.
    (license (list license:gpl2+
                   (nonfree "Personal and academic use only.")))))

(define-public r-flowsorted-bloodextended-epic
  (let ((commit "b32d3a069218874f0e87f84389bab62be94fe9ee")
        (revision "1"))
    (package
      (name "r-flowsorted-bloodextended-epic")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/immunomethylomics/FlowSorted.BloodExtended.EPIC")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1jncfxrpdllcb1059p8lha38q6nj9c3c05znidwkjw76mnwjkiys"))))
      (properties `((upstream-name . "FlowSorted.BloodExtended.EPIC")))
      (build-system r-build-system)
      (propagated-inputs (list r-experimenthub
                          r-flowsorted-blood-epic
                          r-genefilter
                          r-illuminahumanmethylationepicanno-ilm10b4-hg19
                          r-minfi
                          r-nlme
                          r-quadprog
                          r-s4vectors
                          r-summarizedexperiment))
      (native-inputs (list r-knitr))
      (home-page
       "https://github.com/immunomethylomics/FlowSorted.BloodExtended.EPIC")
      (synopsis
       "Illumina EPIC data on extended immunomagnetic sorted blood populations")
      (description
       "Raw data for improved blood cell estimation in minfi and similar packages.")
      ;; None free software
      (license (license:fsdg-compatible
                "https://github.com/immunomethylomics/FlowSorted.BloodExtended.EPIC/blob/main/LICENSE")))))

(define-public r-omixerrpm
  (let ((commit "184f1cc99aefed722e20eb00eda082348a064c4e")
        (revision "1"))
    (package
      (name "r-omixerrpm")
      (version (git-version "0.3.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/omixer/omixer-rpmR")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "10ipasg8zi4ln13a5zj2l4a35hd3sfy3p6gir0sw0vf3cbc11lnz"))))
      (properties `((upstream-name . "omixerRpm")))
      (build-system r-build-system)
      (home-page "https://github.com/omixer/omixer-rpmR")
      (synopsis "Metabolic module profiling of microbiome samples")
      (description
       "This package provides an R interface for microbiome metabolic
module profiling with omixer-rpm.")
      ;; The bundled omixer-rpm.jar is licensed under an Academic
      ;; non-commercial software license.
      (license license:gpl3))))

(define-public r-pdclust
  (let ((commit "775bfa6b54d3611a004b326215ef4f07fa20ef34")
        (revision "1"))
    (package
      (name "r-pdclust")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/hui-tony-zk/PDclust")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jzgp18mjm51497hm288afx5yy2s066ql79xvbgglafi39qdikpk"))))
      (properties `((upstream-name . "PDclust")))
      (build-system r-build-system)
      (propagated-inputs (list r-domc r-dplyr r-foreach r-tidyr))
      (native-inputs (list r-knitr))
      (home-page "https://github.com/hui-tony-zk/PDclust")
      (synopsis "Pairwise comparisons for single-cell DNA methylation data")
      (description
       "This package provides helper functions to do pairwise
comparisons of commonly covered @code{CpG} sites in single-cell DNA
Methylation data.")
      ;; https://github.com/hui-tony-zk/PDclust/issues/1
      (license (nonfree "Missing license")))))

(define-public igvtools
  (package
   (name "igvtools")
   (version "2.3.71")
   (source (origin
     (method url-fetch)
     (uri (string-append
           "http://data.broadinstitute.org/igv/projects/downloads/2.3/igvtools_"
           version ".zip"))
     (sha256
      (base32 "1z7fx79jfsqm0ry89mchifxxrj7vl1h9f98x6p2r2vcbx8f4zvi8"))))
   (build-system gnu-build-system)
   (inputs
    `(("icedtea" ,icedtea-8)))
   (native-inputs
    `(("unzip" ,unzip)))
   (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (add-before 'install 'fix-java-command
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "igvtools"
              (("java -D") (string-append
                            (assoc-ref inputs "icedtea")
                            "/bin/java -D")))))
        (replace 'install
          (lambda _
            (let* ((out (assoc-ref %outputs "out"))
                   (bin (string-append out "/share/java/" ,name)))
              (install-file "igvtools.jar" bin)
              (install-file "igvtools" bin)
              (mkdir (string-append bin "/genomes"))
              (copy-recursively "genomes" (string-append bin "/genomes"))))))))
   (home-page "http://www.broadinstitute.org/software/igv/")
   (synopsis "Integrative Genomics Viewer")
   (description "The Integrative Genomics Viewer (IGV) is a high-performance
visualization tool for interactive exploration of large, integrated genomic
datasets.  It supports a wide variety of data types, including array-based and
next-generation sequence data, and genomic annotations.")
   (license license:lgpl2.1)))

;; This is tainted because it depends on the tainted
;; python-gimmemotifs.
(define-public python-celloracle
  ;; Pypi has 0.12.1, but there's no corresponding tag in the git
  ;; repository, which we use because there are no tests in the Pypi
  ;; version.  So we just take the latest commit.
  (let ((commit "23c8fcad13da531a6201187dbc1a7baa7697b266")
        (revision "1"))
    (package
      (name "python-celloracle")
      (version (git-version "0.12.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/morris-lab/CellOracle")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0b2bgkkh29jacqgpi8748412iqhcv35f89bzv7r61z6vdlvq9hv8"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        ;; These tests are all about downloading data from the
        ;; internet.
        '(list "-k" "not _dl and not test_tutorial_")
        #:phases
        '(modify-phases %standard-phases
           ;; Genomepy needs to write to HOME
           (add-before 'check 'set-HOME
             (lambda _ (setenv "HOME" "/tmp")))
           ;; Numba needs a writable dir to cache functions.
           (add-before 'check 'set-numba-cache-dir
             (lambda _ (setenv "NUMBA_CACHE_DIR" "/tmp")))
           (add-after 'unpack 'relax
             (lambda _
               (substitute* "requirements.txt"
                 (("0.17.1") "0.17.2")))))))
      (propagated-inputs (list jupyter
                               python-genomepy
                               python-gimmemotifs-0.17
                               python-goatools
                               python-h5py
                               python-igraph
                               python-joblib
                               python-matplotlib
                               python-pyarrow
                               python-scanpy
                               python-scikit-learn
                               python-scipy
                               python-seaborn
                               python-tqdm
                               python-umap-learn
                               python-velocyto
                               python-vtraag-louvain))
      (native-inputs
       (list python-anndata
             python-cython
             python-numba
             python-numpy
             python-pandas
             python-pytest))
      (home-page "https://github.com/morris-lab/CellOracle")
      (synopsis
       "In silico gene perturbation analysis and GRN analysis with single cell data")
      (description
       "This package provides a Python library for in silico gene
perturbation analyses.  These libraries use single-cell omics data and
Gene Regulatory Network models.")
      (license license:asl2.0))))

(define-public python-decoupler
  (package
    (name "python-decoupler")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/saezlab/decoupler-py")
                    (commit "5ab28c22140359222de0f48033051b488e32423b")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wag3x9ggzazms3sng7hl4c2643bv1mir7h6kwys66449w3x45g8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Requires access to internet resources.
      '(list "--ignore=decoupler/tests/test_omnip.py")
      #:phases
      '(modify-phases %standard-phases
         ;; Numba needs a writable dir to cache functions.
         (add-before 'build 'set-numba-cache-dir
           (lambda _
             (setenv "NUMBA_CACHE_DIR" "/tmp")))
         ;; Omnipath needs HOME to write a config file
         (add-before 'build 'set-HOME
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-adjusttext
           python-anndata
           python-numba
           python-omnipath
           python-scanpy
           python-seaborn
           python-skranger
           python-tqdm
           python-typing-extensions))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/saezlab/decoupler-py")
    (synopsis "Methods to infer biological activities from omics data")
    (description
     "Decoupler is a package containing different statistical methods
to extract biological activities from omics data within a unified
framework.  This is its faster and memory efficient Python
implementation.")
    ;; GPLv3 except for the viper method, which has non-commercial terms.
    (license license:gpl3)))

;; This is tainted because it depends on all these non-free tools.
(define-public python-gimmemotifs
  (package
    (name "python-gimmemotifs")
    (version "0.18.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vanheeringen-lab/gimmemotifs/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jxr8884k7lic88vhr35l59q5qlpm64p4sv3xfq3l4y41ansh2z0"))
              (modules '((guix build utils)))
              ;; Delete included third-party binaries
              (snippet
               '(delete-file-recursively "src"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; A lot of the tests depend on a wide range of external tools.
      '(list "-k"
             (string-append "not test_tool"
                            ;; The denovo tests fail because no motifs
                            ;; are found.
                            " and not test1_denovo"
                            ;; not needed
                            " and not test_black_formatting"
                            " and not test_flake8_formatting"
                            " and not test_isort_formatting"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'do-not-copy-binaries
            (lambda _
              (substitute* "setup.py"
                (("^cmdclass\\[\"build_py.*") ""))))
          (add-after 'unpack 'patch-version
            (lambda _
              (substitute* "setup.py"
                (("version =.*")
                 (string-append "version = \"" #$version "\"")))))
          (add-after 'unpack 'set-HOME
            (lambda _
              (setenv "HOME" "/tmp")))
          (add-after 'unpack 'fix-tool-name
            (lambda _
              (substitute* '("gimmemotifs/tools/amd.py"
                             "data/cfg/gimmemotifs.default.cfg")
                (("AMD.bin") "AMD"))))
          (add-after 'install 'link-tools
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((target
                     (string-append #$output
                                    "/lib/python3.10/site-packages/gimmemotifs/included_tools/")))
                (for-each
                 (lambda (tool)
                   (symlink tool (string-append target (basename tool))))
                 (list (search-input-file inputs "/bin/AMD")
                       (search-input-file inputs "/bin/BioProspector")
                       (search-input-file inputs "/bin/meme")))))))))
    (propagated-inputs (list python-biofluff
                             python-configparser
                             python-diskcache
                             python-feather-format
                             python-genomepy
                             python-iteround
                             python-jinja2
                             python-logomaker
                             python-loguru
                             python-matplotlib
                             python-numpy
                             python-pandas
                             python-pybedtools
                             python-pysam
                             python-qnorm
                             python-scikit-learn
                             python-scipy
                             python-seaborn
                             python-setuptools
                             python-statsmodels
                             python-tqdm
                             python-xdg
                             python-xxhash))
    (inputs
     (list amd bioprospector homer meme))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/vanheeringen-lab/gimmemotifs/")
    (synopsis "GimmeMotifs is a motif prediction pipeline.")
    (description "GimmeMotifs is a suite of motif tools, including a motif
prediction pipeline for ChIP-seq experiments.")
    (license license:expat)))

;; This is tainted because it depends on all these non-free tools.
(define-public python-gimmemotifs-0.17
  (package
    (inherit python-gimmemotifs)
    (name "python-gimmemotifs")
    (version "0.17.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vanheeringen-lab/gimmemotifs/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ykybgfv10g2zzif6jsfscdff8cm3chagw2rkkrzy74cg0psvkvm"))
              (modules '((guix build utils)))
              ;; Delete included third-party binaries
              (snippet
               '(delete-file-recursively "src"))))
    (arguments
     (substitute-keyword-arguments (package-arguments python-gimmemotifs)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (replace 'patch-version
              (lambda _
                (substitute* "setup.py"
                  (("version =.*")
                   (string-append "version = \"" #$version "\"")))))))
       ((#:test-flags _ '())
        ;; A lot of the tests depend on a wide range of external tools.
        '(list "-k"
               (string-append "not test_tool"
                              ;; not needed
                              " and not test_black_formatting"
                              " and not test_flake8_formatting"
                              " and not test_isort_formatting")))))))

(define (varscan version commit hash)
  (let ((jar-file (string-append "varscan-" version ".jar")))
    (package
      (name "varscan")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/dkoboldt/varscan/raw/"
                      commit "/VarScan.v" version ".source.jar"))
                (sha256 (base32 hash))))
      (build-system ant-build-system)
      (arguments
       (list
        #:tests? #f ; No test target.
        #:phases
        #~(modify-phases %standard-phases
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (mkdir "source")
                (chdir "source")
                (and
                 ;; Unpack the Java archive containing the source files.
                 (invoke "jar" "xf" source)
                 ;; Remove existing compiled output.
                 (with-directory-excursion "net/sf/varscan/"
                   (for-each (lambda (file)
                               (unless (string= (string-take-right file 5) ".java")
                                 (delete-file file)))
                             (find-files "." #:directories? #f))))))
            (replace 'build
              (lambda _
                (let ((sources-dir "net/sf/varscan/"))
                  (with-directory-excursion sources-dir
                    (for-each
                     (lambda (file)
                       (when (string= (string-take-right file 5) ".java")
                         ;; Compile the source files.
                         (invoke "javac" file)))
                     (find-files "." #:directories? #f)))
                  ;; Construct the Java archive.
                  (apply invoke "jar" "cfm" #$jar-file
                         (cons "META-INF/MANIFEST.MF"
                               (find-files sources-dir
                                           "\\.class$"))))))
            (replace 'install
              (lambda _
                (install-file #$jar-file
                              (string-append #$output "/share/java/varscan/")))))))
      (home-page "http://dkoboldt.github.io/varscan/")
      (synopsis "Variant detection in massively parallel sequencing data")
      (description "")
      ;; Free for non-commercial use by academic, government, and
      ;; non-profit/not-for-profit institutions
      (license (license:non-copyleft
                "https://raw.githubusercontent.com/dkoboldt/\
varscan/master/VarScan.v2.4.0.description.txt")))))

(define-public varscan-2.4.0
  (varscan "2.4.0" "ed3227992f31725548d6106dc7fcd0bd8879ff1e"
           "1qyl93awj31qg4pbwaicm5vgq4zv5b9aqa10dpna9qrvbcqfdz90"))

(define-public varscan-2.4.1
  (varscan "2.4.1" "91f116629b2addce523a2eabe118b1cd7a538444"
           "0y45ympkza7qwcbcisg006286pwjbr5978n03hx5nvl09f0mapk8"))

(define-public varscan-2.4.2
  (varscan "2.4.2" "18425ce00e3ced8afc624bd86de142b1cd1e0eb0"
           "14f7fp0yaj3lsif1dpjdci7kz3b2fd9qic3299a2bvgk3rv3lp6n"))

(define-public blat
  (package
    (name "blat")
    (version "35")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://users.soe.ucsc.edu/~kent/src/blatSrc"
                           version ".zip"))
       (sha256
        (base32
         "081nwnd97p2ry4rjnnia6816cssm682hlm7hzqhlnjpc2kqvrn86"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false                   ;There is no test target
      #:make-flags '(list "MACHTYPE=i386"
                          "BINDIR=/tmp/bin"
                          "CFLAGS=-fcommon")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _ (mkdir-p "/tmp/bin")))
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (copy-recursively "/tmp/bin" bin)))))))
    (native-inputs (list unzip))
    (inputs (list libpng))
    (home-page "http://genome.ucsc.edu")
    (synopsis "Pairwise sequence alignment algorithm")
    (description "BLAT is a pairwise sequence alignment algorithm
that.  It was designed primarily to decrease the time needed to align
millions of mouse genomic reads and expressed sequence tags against
the human genome sequence.")
    (license (nonfree "Personal and academic use only."))))

(define-public clinvar
  (package
   (name "clinvar-vcf")
   (version "GRCh38-20200919")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/vcf_GRCh38/weekly/clinvar_20200919.vcf.gz"))
            (sha256
             (base32
	      "06wdfg6wkksra4if1hil78p9707l9zq8h74cc4mpqrhl1vv8j8sq"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let ((source-file (assoc-ref %build-inputs "source"))
              (output-dir (string-append %output "/share/clinvar/GRCh38")))
          (mkdir-p output-dir)
          (copy-file source-file
                     (string-append output-dir "/clinvar.vcf.gz"))))))
   (home-page "https://www.ncbi.nlm.nih.gov/clinvar/")
   (synopsis "Public archive of reports of human genetic variation")
   (description "ClinVar is a freely accessible, public archive of reports
of the relationships among human variations and phenotypes, with supporting
evidence.  ClinVar thus facilitates access to and communication about the
relationships asserted between human variation and observed health status,
and the history of that interpretation.  ClinVar processes submissions
reporting variants found in patient samples, assertions made regarding their
clinical significance, information about the submitter, and other supporting
data.  The alleles described in submissions are mapped to reference sequences,
and reported according to the HGVS standard.  ClinVar then presents the data
for interactive users as well as those wishing to use ClinVar in daily
workflows and other local applications.  ClinVar works in collaboration with
interested organizations to meet the needs of the medical genetics community
as efficiently and effectively as possible.")
   ;; No license specified.
   (license (license:non-copyleft "file:///dev/null"))))

(define-public clinvar-grch37
  (package (inherit clinvar)
    (version "GRCh37-20200919")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/vcf_GRCh37/weekly/clinvar_20200919.vcf.gz"))
             (sha256
              (base32
               "0srdr8mwf2wnch8v5gkdj0lqqmm50inzysh9cb4gb7ndrbwhharv"))))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let ((source-file (assoc-ref %build-inputs "source"))
              (output-dir (string-append %output "/share/clinvar/GRCh37")))
          (mkdir-p output-dir)
          (copy-file source-file
                     (string-append output-dir "/clinvar.vcf.gz"))))))))

(define-public dbsnp
  (package
    (name "dbsnp")
    (version "human_9606")
    (source (origin
              (method url-fetch)
              (uri "ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606/VCF/00-All.vcf.gz")
              (sha256
               (base32
                "0f2zzi0br0c1dvlx6wfgfm6f7rgp0kb19gb6p0kxzbs3n92viiqa"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source-file (assoc-ref %build-inputs "source"))
                (output-dir  (string-append %output "/share/dbsnp"))
                (output-file (string-append output-dir "/dbSnp.vcf.gz")))
           (mkdir-p output-dir)
           (copy-file source-file output-file)
           (symlink output-file (string-append output-dir "/00-All.vcf.gz"))))))
    (home-page "https://www.ncbi.nlm.nih.gov/projects/SNP/")
    (synopsis "Short genetic variations")
    (description "")
    ;; No license specified.
    (license (license:non-copyleft "file:///dev/null"))))

(define-public 1000genomes-phase1-indels
  (package
    (name "1000genomes-phase1-indels")
    (version "b37")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://"
                                  "gsapubftp-anonymous@"
                                  "ftp.broadinstitute.org/bundle/b37/"
                                  "1000G_phase1.indels.b37.vcf.gz"))
              (sha256
               (base32 "173kkmyvyvfa55v2rbpywsrp7159yyl1sx30y243jkxzkjrgc7bc"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source-file (assoc-ref %build-inputs "source"))
                (output-dir (string-append %output "/share/1000G"))
                (output-file-uncompressed (string-append output-dir
                                            "/1000G_phase1.indels.b37.vcf"))
                (output-file (string-append output-file-uncompressed ".gz"))
                (java (string-append (assoc-ref %build-inputs "icedtea")
                                     "/bin/java"))
                (igvtools (string-append (assoc-ref %build-inputs "igvtools")
                                         "/share/java/igvtools/igvtools.jar"))
                (path (string-append (assoc-ref %build-inputs "htslib") "/bin:"
                                     (assoc-ref %build-inputs "gzip") "/bin")))
           ;; The gunzip command needs to find gzip in PATH.
           (setenv "PATH" path)
           (mkdir-p output-dir)
           (copy-file source-file output-file)

           ;; To create the index, we need to compress the VCF file with
           ;; bgzip, instead of the regular gzip.
           (system* "gunzip" output-file)
           (system* "bgzip" output-file-uncompressed)

           ;; Finally, we can index the file using igvtools.
           (system* java "-jar" igvtools "index" output-file)))))
    (inputs
     `(("icedtea" ,icedtea-7)
       ("igvtools" ,igvtools)
       ("htslib" ,htslib)
       ("gzip" ,gzip)))
    (home-page "http://www.internationalgenome.org/")
    (synopsis "Initial map of insertions and deletions in the human genome")
    (description "")
    ;; No license specified.
    (license (license:non-copyleft "file:///dev/null"))))

(define-public mills-1000G-gold-standard-indels
  (package
    (name "1000genomes-mills-gold-standard-indels")
    (version "b37")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://"
                                  "gsapubftp-anonymous@"
                                  "ftp.broadinstitute.org/bundle/b37/"
                                  "Mills_and_1000G_gold_standard.indels.b37.vcf.gz"))
              (sha256
               (base32 "1n9bf6chfr9pxhk0mfiiqy28pmkyb0xpxz0rwvwrw031cw39dc1l"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source-file (assoc-ref %build-inputs "source"))
                (output-dir (string-append %output "/share/1000G"))
                (output-file-wo-ext
                 (string-append output-dir
                                "/Mills_and_1000G_gold_standard.indels.b37"))
                (bcf-output-file (string-append output-file-wo-ext ".bcf"))
                (output-file-uncompressed (string-append output-file-wo-ext ".vcf"))
                (output-file (string-append output-file-uncompressed ".gz"))
                (java (string-append (assoc-ref %build-inputs "icedtea")
                                     "/bin/java"))
                (igvtools (string-append (assoc-ref %build-inputs "igvtools")
                                         "/share/java/igvtools/igvtools.jar"))
                (path (string-append (assoc-ref %build-inputs "htslib") "/bin:"
                                     (assoc-ref %build-inputs "gzip") "/bin:"
                                     (assoc-ref %build-inputs "bcftools") "/bin:"
                                     (assoc-ref %build-inputs "grep") "/bin")))

           ;; The gunzip command needs to find gzip in PATH.
           (setenv "PATH" path)
           (mkdir-p output-dir)
           (copy-file source-file output-file)

           ;; To create the index, we need to compress the VCF file with
           ;; bgzip, instead of the regular gzip.
           (system* "gunzip" output-file)
           (chmod output-file-uncompressed #o644)

           ;; The "vcf" file seems to be actually a "bcf" file.  We can use bcftools to
           ;; convert it to a VCF file.
           (rename-file output-file-uncompressed bcf-output-file)
           (system (string-append "bcftools view "
                                  bcf-output-file
                                  " | grep -v bcftools_view > "
                                  output-file-uncompressed))

           (system* "bgzip" output-file-uncompressed)
           (delete-file bcf-output-file)

           ;; Finally, we can index the file using igvtools.
           (system* java "-jar" igvtools "index" output-file)))))
    (inputs
     `(("icedtea" ,icedtea-7)
       ("igvtools" ,igvtools)
       ("htslib" ,htslib)
       ("gzip" ,gzip)
       ("bcftools" ,bcftools)
       ("grep" ,grep)))
    (home-page "http://www.internationalgenome.org/")
    (synopsis "Initial map of insertions and deletions in the human genome")
    (description "")
    ;; No license specified.
    (license (license:non-copyleft "file:///dev/null"))))

(define-public dbsnp-138
  (package
    (name "dbsnp")
    (version "138-b37")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://"
                                  "gsapubftp-anonymous@"
                                  "ftp.broadinstitute.org/bundle/b37/"
                                  "dbsnp_138.b37.vcf.gz"))
              (sha256
               (base32 "0c7i6qw6j6chhqni826jr98b4kfjg72mql36wdfydiiv7679zx5n"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source-file (assoc-ref %build-inputs "source"))
                (output-dir (string-append %output "/share/1000G"))
                (output-file-uncompressed (string-append output-dir
                                            "/dbsnp_138.b37.vcf"))
                (output-file (string-append output-file-uncompressed ".gz"))
                (java (string-append (assoc-ref %build-inputs "icedtea")
                                     "/bin/java"))
                (igvtools (string-append (assoc-ref %build-inputs "igvtools")
                                         "/share/java/igvtools/igvtools.jar"))
                (path (string-append (assoc-ref %build-inputs "htslib") "/bin:"
                                     (assoc-ref %build-inputs "gzip") "/bin")))
           ;; The gunzip command needs to find gzip in PATH.
           (setenv "PATH" path)
           (mkdir-p output-dir)
           (copy-file source-file output-file)

           ;; To create the index, we need to compress the VCF file with
           ;; bgzip, instead of the regular gzip.
           (system* "gunzip" output-file)
           (system* "bgzip" output-file-uncompressed)

           ;; Finally, we can index the file using igvtools.
           (system* java "-jar" igvtools "index" output-file)))))
    (inputs
     `(("icedtea" ,icedtea-7)
       ("igvtools" ,igvtools)
       ("htslib" ,htslib)
       ("gzip" ,gzip)))
    (home-page "")
    (synopsis "")
    (description "")
    ;; No license specified.
    (license (license:non-copyleft "file:///dev/null"))))

(define-public dx-tracks
  (package
    (name "dx-tracks")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/UMCUGenetics/Dx_tracks/releases/"
                    "download/v" version "/v" version ".tar.gz"))
              (sha256
               (base32 "0vcyd888yq6qqal5n9l5g361nzx3wq70zlbn9bhza2qkhfd3n5pp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (input-file (assoc-ref %build-inputs "source"))
               (output-dir (string-append %output "/share/data/dx-tracks"))
               (PATH (string-append (assoc-ref %build-inputs "gzip") "/bin")))
           (setenv "PATH" PATH)
           (mkdir-p output-dir)
           (with-directory-excursion output-dir
             (system* tar "-xvf" input-file "--strip-components=1"))))))
    (inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (home-page "https://github.com/UMCUGenetics/Dx_tracks")
    (synopsis "")
    (description "")
    (license cc-by-nd4.0)))

(define-public dbnsfp
  (package
    (name "dbnsfp")
    (version "2.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://dbnsfp:dbnsfp@dbnsfp.softgenetics.com/dbNSFPv"
                    version ".zip"))
              (sha256
               (base32
                "132z7rayqdwc04b8bw19amvwyhg67vyscyv1zrb486r49icf73mz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source-file (assoc-ref %build-inputs "source"))
               (output-dir  (string-append %output "/share/dbnsfp"))
               (unzip       (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip"))
               (gzip        (string-append (assoc-ref %build-inputs "gzip") "/bin/gzip")))
           (mkdir-p output-dir)
           (with-directory-excursion output-dir
             (system* unzip source-file)
             (for-each (lambda (file)
                         (format #t "Compressing ~s~%" file)
                         (system* gzip file))
                       (find-files output-dir)))))))
    (inputs
     `(("unzip" ,unzip)
       ("gzip" ,gzip)))
    (home-page "https://sites.google.com/site/jpopgen/dbNSFP")
    (synopsis "Database for functional prediction of non-synonymous SNPs")
    (description " dbNSFP is a database developed for functional prediction and
annotation of all potential non-synonymous single-nucleotide variants (nsSNVs)
in the human genome.")
    ;; No license specified.
    (license (license:non-copyleft "file:///dev/null"))))

(define-public giab-na12878-high-confidence-regions
  (package
    (name "giab-na12878-high-confidence-regions")
    (version "NISTv3.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp-trace.ncbi.nlm.nih.gov/giab/ftp/release/"
                    "NA12878_HG001/" version "/NA12878_GIAB_highconf_IllFB"
                    "-IllGATKHC-CG-Ion-Solid_ALLCHROM_v3.2.2_highconf.bed"))
              (sha256
               (base32 "1adj878im498lfplklkir7v2chv1bxamgw3y2a62599wvbhap79q"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source-file (assoc-ref %build-inputs "source"))
               (output-dir (string-append %output "/share/giab")))
           (mkdir-p output-dir)
           (copy-file source-file
                      (string-append output-dir "/NA12878_GIAB_highconf_IllFB"
                                     "-IllGATKHC-CG-Ion-Solid_ALLCHROM_v3.2.2"
                                     "_highconf.bed"))))))
    (home-page "http://jimb.stanford.edu/giab")
    (synopsis "")
    (description "")
    ;; No license specified.
    (license (license:non-copyleft "file:///dev/null"))))

(define-public freec-mappability-tracks
  (package
    (name "freec-mappability-tracks")
    (version "hg19_100bp")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://xfer.curie.fr/get/nil/7hZIk1C63h0/"
                    version ".tar.gz"))
              (sha256
               (base32
                "1qp05na2lb7w35nqii9gzv4clmppi3hnk5w3kzfpz5sz27fw1lym"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((output-dir (string-append #$output "/share/freec"))
                (tar (string-append #$(this-package-input "tar") "/bin/tar"))
                (PATH (string-append #$(this-package-input "gzip") "/bin")))
            (setenv "PATH" PATH)
            (mkdir-p output-dir)
            (with-directory-excursion output-dir
              (invoke tar "-xvf" #$source))))))
    (inputs (list gzip tar))
    (home-page "http://boevalab.com/FREEC")
    (synopsis "Mappability track for hg19 genome")
    (description "This package provides the mappability track for the
hg19 human genome with 100 base pair read length.")
    ;; No license specified.
    (license (license:non-copyleft "file:///dev/null"))))

(define-public impute2-bin
  (package
    (name "impute2")
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://mathgen.stats.ox.ac.uk/impute/impute_v"
                    version "_x86_64_static.tgz"))
              (sha256
               (base32 "0py4m0asp1459nn1xsv552n3azqcfhipa4si8bzxs1a58q05jqcm"))))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out") "/bin")))
               (install-file "impute2" out)))))))
    (home-page "https://mathgen.stats.ox.ac.uk/impute/impute_v2.html")
    (synopsis "")
    (description "")
    ;; Non-commercial use only
    (license (license:non-copyleft
              "http://web.archive.org/web/20170915131927/\
http://www.stats.ox.ac.uk/~marchini/software/gwas/gwas.html#licence"))))

(define-public scan_for_matches
  (package
   (name "scan_for_matches")
   (version "0.0")
   (source (origin
            (method url-fetch)
            (uri "http://www.theseed.org/servers/downloads/scan_for_matches.tgz")
            (sha256
             (base32 "13ynw9i6j76884pdi249qhvgpvr6ii7hnfkwnllaryxxxwq7kcf6"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
          (lambda _
            (invoke "gcc" "-O2" "-o" "scan_for_matches"  "ggpunit.c" "scan_for_matches.c")))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "scan_for_matches" bin)))))))
   (home-page "https://blog.theseed.org/servers/2010/07/scan-for-matches.html")
   (synopsis "Utility for locating patterns in DNA")
   (description "This package provides a utility for locating patterns in DNA
 or protein FASTA files.")
   ;; No license specified.
    (license (license:non-copyleft "file:///dev/null"))))

(define-public viennarna
  (package
    (name "viennarna")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ViennaRNA/ViennaRNA"
                                  "/releases/download/v" version "/ViennaRNA-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1ds4gr5xsp7l3ynkcwj86glgd5fp5ddqiqikclizfpl2fkai8i5y"))))
    (build-system gnu-build-system)
    (arguments
     ;; Disable link-time optimization because this creates problems
     ;; when stripping.  Linking with the stripped static library
     ;; would fail when LTO is enabled.  See the discussion here:
     ;; https://github.com/s-will/LocARNA/issues/7
     `(#:configure-flags '("--disable-lto")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-search-path
           (lambda _
             ;; Work around test failure.
             (setenv "PERL5LIB"
                     (string-append (getcwd) "/tests:"
                                    (getenv "PERL5LIB"))))))))
    (inputs
     (list perl python))
    (native-inputs
     (list swig))
    (home-page "http://www.tbi.univie.ac.at/RNA/index.html")
    (synopsis "Prediction and comparison of RNA secondary structures")
    (description
     "RNA secondary structure prediction through energy minimization is the
most used function in the package.  Three kinds of dynamic programming
algorithms for structure prediction are provided: the minimum free energy
algorithm of Zuker & Stiegler (1981) which yields a single optimal structure,
the partition function algorithm of McCaskill (1990) which calculates base
pair probabilities in the thermodynamic ensemble, and the suboptimal folding
algorithm of Wuchty et.al (1999) which generates all suboptimal structures
within a given energy range of the optimal energy.  For secondary structure
comparison, the package contains several measures of
distance (dissimilarities) using either string alignment or
tree-editing (Shapiro & Zhang 1990).  Finally, an algorithm to design
sequences with a predefined structure (inverse folding) is provided.")
    (license
     (nonfree
      "https://github.com/ViennaRNA/ViennaRNA/blob/master/COPYING"
      "license forbids commercial usage"))))

(define-public viennarna-2.4
  (package (inherit viennarna)
    (version "2.4.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ViennaRNA/ViennaRNA"
                                  "/releases/download/v" version "/ViennaRNA-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0z35d59hkc2ynb7rh6np2kbgx9ignasm09r7r0hlisivgknwyxmj"))))))

(define-public viennarna-2.2.8
  (package (inherit viennarna)
    (version "2.2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.tbi.univie.ac.at/RNA/packages/source/ViennaRNA-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0b9h3rrndxjvj3r2wyixf095fghpspgiwx3acbd8hlv3lj6hpi1h"))))
    (arguments
     ;; Disable link-time optimization because this creates problems
     ;; when stripping.  Linking with the stripped static library
     ;; would fail when LTO is enabled.  See the discussion here:
     ;; https://github.com/s-will/LocARNA/issues/7
     `(#:configure-flags '("--disable-lto"
                           "CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-search-path
           (lambda _
             ;; Work around test failure.
             (setenv "PERL5LIB"
                     (string-append (getcwd) "/tests:"
                                    (getenv "PERL5LIB"))))))))
    (native-inputs
     (list swig gcc-8))))

(define-public viennarna-1.8
  (package (inherit viennarna)
    (version "1.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.tbi.univie.ac.at/RNA/download/sourcecode/"
                    "1_8_x/ViennaRNA-" version ".tar.gz"))
              (sha256
               (base32
                "1ygcs399xl07igj15ynfg6cd9ifi1amy8n0p6bl6awgpx95xkqpl"))))
    (arguments
     `(#:tests? #f ; no tests
       #:configure-flags
       (list "--without-perl"
	         "CFLAGS=-std=gnu89 -fcommon")))
    (inputs '())
    (native-inputs (list gcc-6))))

;; This is non-free because it contains ViennaRNA code, which is
;; released under a non-free license.
(define-public mafft-extensions
  (package (inherit mafft)
    (version (package-version mafft))
    (name "mafft-extensions")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://mafft.cbrc.jp/alignment/software/mafft-"
                                  version "-with-extensions-src.tgz"))
              (sha256
               (base32
                "06gk8csbx3fdsz18mizxl51iazlb5jfmn6l6sgxqr8cy12p76sdv"))))
    (arguments
     `(#:tests? #f ; no tests included
       #:make-flags
       ,#~(list (string-append "PREFIX=" #$output)
                (string-append "BINDIR=" #$output "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "extensions")))
         (delete 'configure))))
    (synopsis "Extensions for the MAFFT multiple sequence alignment package")
    (description
     "The extensions code includes code of the ViennaRNA package,
MXSCARNA and ProbConsRNA.")
    ;; FIXME: this is probably inaccurate.
    (license (package-license viennarna))))

(define-public cofold
  (package
    (inherit viennarna-1.8)
    (name "cofold")
    (version "1.0")
    (source (origin
              (method url-fetch)
              ;; XXX uggh there's no versioning for this tarball,
              ;; should migrate it to git-based fetching asap.
              (uri (string-append "http://www.e-rna.org/cofold/CoFold.tar.gz"))
              (sha256
               (base32
                "1hr1hnm3nxj0y6yd94wxiqw10y653wyr6prl9i02a27bd6c27gbz"))))
    (arguments
     `(#:tests? #f ; there are no tests
       #:parallel-build? #f)) ; build fails otherwise
    (synopsis "Predict RNA secondary structure considering co-transcriptional folding")
    (description "CoFold is a thermodynamics-based RNA secondary
structure folding algorithm that takes co-transcriptional folding in
account.  This has been shown to significantly improve the
state-of-art in terms of prediction accuracy, especially for long
sequences greater than 1000 nt in length.")
    (home-page "http://www.e-rna.org/cofold/")
    (license (package-license viennarna-1.8))))

;; This is free software, but it depends on the non-free ViennaRNA, so
;; we can't distribute it as part of the official Guix channels.
(define-public intarna
  (package
    (name "intarna")
    (version "3.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BackofenLab/IntaRNA"
                                  "/releases/download/v" version
                                  "/intaRNA-" version ".tar.gz"))
              (sha256
               (base32
                "1lcfx13xg74cli724d4ygqz2g2ahp5054ihnwcv2ddmnbgx77ljp"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--with-boost-libdir="
                             #$(this-package-input "boost") "/lib"))))
    (native-inputs
     (list catch2-1 pkg-config))
    (inputs
     (list boost viennarna zlib))
    (home-page "https://github.com/BackofenLab/IntaRNA")
    (synopsis "Efficient RNA-RNA interaction prediction")
    (description "IntaRNA is a general and fast approach to the
prediction of RNA-RNA interactions incorporating both the
accessibility of interacting sites as well as the existence of a
user-definable seed interaction.")
    (license license:expat)))

(define-public python-chess-hic
  (package
    (name "python-chess-hic")
    (version "0.3.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "chess-hic" version))
              (sha256
               (base32
                "0i6j6iabv7n67ppx9xnaxvrqa9vq56qyx2pnnlkl8w85jrxyzxpg"))))
    (build-system pyproject-build-system)
    (arguments '(#:tests? #false)) ;there are none
    (propagated-inputs
     (list python-cython
           python-fanc
           python-future
           python-intervaltree
           python-kneed
           python-numpy
           python-pandas
           python-pathos
           python-pybedtools
           python-scikit-image
           python-scipy
           python-tqdm))
    (home-page "https://github.com/vaquerizaslab/chess")
    (synopsis "Feature extraction for chromatin contact data")
    (description
     "This package is a command line tool for the quantitative
comparison and automatic feature extraction for chromatin contact
data.")
    ;; Non-commercial use only
    (license
     (nonfree
      "https://raw.githubusercontent.com/vaquerizaslab/chess/master/LICENSE"))))
