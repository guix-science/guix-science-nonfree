;;;
;;; Copyright © 2016-2021 Roel Janssen <roel@gnu.org>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages python)
  #:use-module (gnu packages commencement)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public strelka-1.0.15
  (package
    (name "strelka")
    (version "1.0.15")
    (source (origin
      (method url-fetch)
      (uri (string-append
            ;;"ftp://strelka:''@ftp.illumina.com/v1-branch/v"
            ;;version "/strelka_workflow-" version ".tar.gz"))
            "https://sites.google.com/site/strelkasomaticvariantcaller/home/"
            "download/" name "_workflow-" version ".tar.gz"))
      (sha256
       (base32 "1cwad2wlhdk09702ivblfiyv921af0al7s1gm1dn2d3b0v31qrp2"))
      (patches (list (search-patch "strelka-disable-tests.patch")
                     (search-patch "strelka-disable-install.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'build-some-more
           (lambda _
             (with-directory-excursion "strelka"
               (zero? (system* "make" "-j" (number->string
                                            (parallel-job-count))
                               "install")))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (perl-lib-dir (string-append out "/lib/perl5/site_perl/"
                                                 ,(package-version perl)))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (libexec (string-append out "/libexec")))

               ;; Substitute the binary directories for samtools and bgzip.
               (substitute* '("src/perl/bin/configureStrelkaWorkflow.pl"
                              "src/perl/libexec/callSomaticVariants.pl"
                              "src/perl/libexec/consolidateResults.pl")
                            (("my \\$samtoolsDir = File::Spec->catdir\\(\\$optDir,'samtools'\\);")
                             (string-append "my $samtoolsDir = \""
                                            (assoc-ref inputs "samtools") "/bin\";"))
                            (("my \\$samtoolsBin = File::Spec->catfile\\(\\$optDir,'samtools','samtools'\\);")
                             (string-append "my $samtoolsBin = \""
                                            (assoc-ref inputs "samtools")
                                            "/bin/samtools\";")))

               (substitute* "src/perl/libexec/consolidateResults.pl"
                 (("my \\$bgzipBin = File::Spec->catfile\\(\\$optDir,'tabix','bgzip'\\);")
                  (string-append "my $bgzipBin = \"" (assoc-ref inputs "htslib") "/bin/bgzip\";"))
                 (("my \\$getHeaderCmd = \"bash")
                  (string-append "my $getHeaderCmd = \"" (assoc-ref inputs "bash") "/bin/bash")))

               (mkdir-p perl-lib-dir)
               (mkdir-p lib)
               (mkdir-p libexec)

               ;; Instead of patching out $optDir throughout the code, we can create
               ;; an empty directory so that these checks pass.  We already patched the
               ;; path to samtools and bgzip, so this should be fine.
               (mkdir-p (string-append out "/opt/samtools"))

               (install-file "src/c++/libexec/countFastaBases" libexec)
               (install-file "src/perl/bin/configureStrelkaWorkflow.pl" bin)
               (install-file "src/perl/libexec/consolidateResults.pl" libexec)
               (install-file "src/perl/libexec/filterSomaticVariants.pl" libexec)
               (install-file "src/perl/libexec/callSomaticVariants.pl" libexec)
               (install-file "src/perl/lib/Utils.pm" perl-lib-dir)
               (install-file "strelka/src/bin/strelka2" bin)
               (install-file "strelka/src/bin/starling2" bin)
               (install-file "strelka/src/bin/strelkaSiteSimulator" bin)

               ;; Also add Utils.pm to the lib folder, because strelka manipulates
               ;; its own Perl path to search in this folder.
               (install-file "src/perl/lib/Utils.pm" lib)

               ;; The configureStrelkaWorkflow.pl script looks for the
               ;; strelka2 binary in the libexec directory.
               (system* "ln" "--symbolic"
                        (string-append bin "/strelka2")
                        (string-append libexec "/strelka2"))))))))
    (inputs
     `(("boost" ,boost)
       ("perl" ,perl)
       ("bash" ,bash)
       ("zlib" ,zlib)
       ("samtools" ,samtools)))
    (native-inputs
     `(("bash" ,bash)
       ("python" ,python-2)
       ("gcc" ,gcc-toolchain-5)))
    (propagated-inputs
     `(("vcftools" ,vcftools)
       ("htslib" ,htslib)))
    (native-search-paths (package-native-search-paths perl))
    (home-page "https://sites.google.com/site/strelkasomaticvariantcaller/")
    (synopsis "Somatic variant calling workflow for matched tumor-normal samples")
    (description "Analysis package designed to detect somatic SNVs and small
indels from the aligned sequencing reads of matched tumor-normal samples")
    ;; WARNING: The license is "Illumina Open Source Software License 1".
    ;; This effectively makes it nonfree software.
    (license license:non-copyleft)))

(define-public strelka-1.0.14
  (package (inherit strelka-1.0.15)
    (name "strelka")
    (version "1.0.14")
    (source (origin
      (method url-fetch)
      (uri (string-append
        "ftp://strelka:''@ftp.illumina.com/v1-branch/v"
        version "/strelka_workflow-" version ".tar.gz"))
      (sha256
        (base32 "0f9g2pkr1f7s4r8sxl53jxr2cjpyx53zf3va0jj8fxzavxiwmbmk"))
      (patches (list (search-patch "strelka-disable-tests.patch")
                     (search-patch "strelka-disable-install.patch")))))
    (propagated-inputs
     `(("vcftools" ,vcftools)
       ("htslib" ,htslib)))))

(define-public r-gsalib
  (package
   (name "r-gsalib")
   (version "2.1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "gsalib" version))
     (sha256
      (base32
       "1k3zjdydzb0dfh1ihih08d4cw6rdamgb97cdqna9mf0qdjc3pcp1"))))
   (build-system r-build-system)
   (home-page "http://cran.r-project.org/web/packages/gsalib")
   (synopsis "Utility Functions For GATK")
   (description "This package contains utility functions used by the Genome
Analysis Toolkit (GATK) to load tables and plot data.  The GATK is a toolkit
for variant discovery in high-throughput sequencing data.")
   (license license:expat)))

(define-public gatk-bin-3.8.1-no-intel-deflation
  (package
    (name "gatk")
    (version "3.8.1-aa8764d6c")
    (source (origin
             (method url-fetch)
             (uri "https://www.roelj.com/gatk-3.8.1-aa8764d6c.jar")
             (sha256
              (base32
               "1w46s2jh1q7h1r8shjw09y8yw27q15wlkviiqby3wv20haaqqjcg"))))
    (build-system gnu-build-system)
    (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'unpack)
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/" ,name "/")))
              (mkdir-p out)
              (copy-file (assoc-ref %build-inputs "source")
                         (string-append out "/GenomeAnalysisTK.jar"))))))))
    (propagated-inputs
     `(("r-gsalib" ,r-gsalib)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gplots" ,r-gplots)
       ("r-reshape" ,r-reshape)
       ("r-optparse" ,r-optparse)
       ("r-dnacopy" ,r-dnacopy)
       ("r-naturalsort" ,r-naturalsort)
       ("r-dplyr" ,r-dplyr)
       ("r-data-table" ,r-data-table)
       ("r-hmm" ,r-hmm)
       ("gatk-queue-bin-3.8-1" ,gatk-queue-bin-3.8-1)))
    (home-page "https://www.broadinstitute.org/gatk/")
    (synopsis "Package for analysis of high-throughput sequencing")
    (description "The Genome Analysis Toolkit or GATK is a software package for
analysis of high-throughput sequencing data, developed by the Data Science and
Data Engineering group at the Broad Institute.  The toolkit offers a wide
variety of tools, with a primary focus on variant discovery and genotyping as
well as strong emphasis on data quality assurance.  Its robust architecture,
powerful processing engine and high-performance computing features make it
capable of taking on projects of any size.")
    ;; There are additional restrictions that make it nonfree.
    (license license:expat)))

(define-public gatk-queue-bin-3.8-1
  (package
    (name "gatk-queue")
    (version "3.8-1-0-gf15c1c3ef")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.roelj.com/gatk-queue-" version ".tar.bz2"))
              (sha256
               (base32 "0435lf2751w3l2m86m3h6girwr09kpiqahq3pj49gibqnyylx4sq"))))
    (build-system gnu-build-system)
    (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/gatk/")))
              (mkdir-p out)
              (install-file "Queue.jar" out)))))))
    (home-page "https://www.broadinstitute.org/gatk/")
    (synopsis "Package for analysis of high-throughput sequencing")
    (description "The Genome Analysis Toolkit or GATK is a software package for
analysis of high-throughput sequencing data, developed by the Data Science and
Data Engineering group at the Broad Institute.  The toolkit offers a wide
variety of tools, with a primary focus on variant discovery and genotyping as
well as strong emphasis on data quality assurance.  Its robust architecture,
powerful processing engine and high-performance computing features make it
capable of taking on projects of any size.")
    ;; There are additional restrictions that make it nonfree.
    (license license:expat)))

(define-public igv
  (package
    (name "igv")
    (version "2.8.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://data.broadinstitute.org/igv/projects/downloads/"
             "2.8/IGV_Linux_" version ".zip"))
       (sha256
        (base32 "1qrhsvl6z5h1kg3pji08fzqj08c6l2lxj0qv7hbvys3mymz4lfzv"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("openjdk11" ,openjdk11)))
    (native-inputs
     `(("unzip" ,unzip)))
    (arguments
     `(#:tests? #f  ; No tests available.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; Nothing to configure.
         (delete 'build) ; This is a binary package only.
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (share (string-append out "/share/igv")))
               (mkdir-p share)
               (mkdir-p lib)
               (mkdir-p bin)
               (copy-recursively "lib" lib)
               (substitute* "igv.sh"
                 (("prefix=")
                  (string-append "prefix=" lib " # "))
                 (("\\$\\{prefix\\}/igv.args")
                  (string-append share "/igv.args"))
                 (("--module-path=\"\\$\\{prefix\\}/lib\"")
                  (string-append "--module-path=" lib))
                 (("exec java")
                  (string-append "exec " (assoc-ref %build-inputs "openjdk11")
                                 "/bin/java")))
               (install-file "igv.args" share)
               (install-file "igv.sh" bin)))))))
   (home-page "http://www.broadinstitute.org/software/igv/")
   (synopsis "Integrative Genomics Viewer")
   (description "The Integrative Genomics Viewer (IGV) is a high-performance
visualization tool for interactive exploration of large, integrated genomic
datasets.  It supports a wide variety of data types, including array-based and
next-generation sequence data, and genomic annotations.")
   ;; No license specified.
   (license license:non-copyleft)))

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
   ;; No license specified.
   (license license:non-copyleft)))
