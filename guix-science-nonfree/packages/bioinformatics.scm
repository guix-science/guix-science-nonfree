;;; Copyright © 2016-2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2015-2023 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages xml)
  #:use-module (past packages boost)
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
       `(#:tests? #f ; No test target.
         #:phases
         (modify-phases %standard-phases
           (replace 'unpack
             (lambda _
               (mkdir "source")
               (chdir "source")
               (and
                ;; Unpack the Java archive containing the source files.
                (zero? (system* "jar" "xf" (assoc-ref %build-inputs "source")))
                ;; Remove existing compiled output.
                (with-directory-excursion "net/sf/varscan/"
                  (for-each (lambda (file)
                              (unless (string= (string-take-right file 5) ".java")
                                (zero? (system* "rm" file))))
                            (find-files "." #:directories? #f))))))
           (replace 'build
             (lambda _
               ;; Keep a list of files to be included in the JAR.
               (let ((out-files '("META-INF/MANIFEST.MF"))
                     (sources-dir "net/sf/varscan/"))
                 (and
                  (with-directory-excursion sources-dir
                    (for-each
                     (lambda (file)
                       (when (string= (string-take-right file 5) ".java")
                         ;; Compile the source files.
                         (zero? (system* "javac" file))
                         ;; Add to list of files to be included in the JAR.
                         (set! out-files
                               (append
                                out-files
                                (list (string-append sources-dir
                                  (string-drop-right (string-drop file 2) 5)
                                  ".class"))))))
                     (find-files "." #:directories? #f)))
                  ;; Construct the Java archive.
                  (let ((params (append '("jar" "cfm" ,jar-file) out-files)))
                    (zero? (apply system* params)))))))
           (replace 'install
             (lambda _
               (let ((out (string-append (assoc-ref %outputs "out")
                                         "/share/java/varscan/")))
                 (install-file ,jar-file out)))))))
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
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((source-file (assoc-ref %build-inputs "source"))
               (output-dir (string-append %output "/share/freec"))
               (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
               (PATH (string-append (assoc-ref %build-inputs "gzip") "/bin")))
           (setenv "PATH" PATH)
           (mkdir-p output-dir)
           (with-directory-excursion output-dir
             (system* tar "-xvf" source-file))))))
    (inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (home-page "http://boevalab.com/FREEC")
    (synopsis "")
    (description "")
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
     (list swig))))

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
    (version "0.3.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "chess-hic" version))
              (sha256
               (base32
                "121w8p67hlhbr7p8xzwsh2mwf88svy1ck5169zyn801kd5llznkk"))))
    (build-system pyproject-build-system)
    (arguments '(#:tests? #false)) ;there are none
    (propagated-inputs
     (list python-cython
           python-fanc
           python-future
           python-intervaltree
           python-kneed
           python-numpy
           python-msgpack
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
