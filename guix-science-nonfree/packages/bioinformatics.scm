;;; Copyright © 2016-2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages java)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
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
    (license (license:non-copyleft
              "https://github.com/Illumina/licenses/blob/master/\
Illumina%20Public%20License%201.pdf"))))

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
   (license (license:non-copyleft "file:///dev/null"))))

(define-public snpeff-bin-4.1
  (package
   (name "snpeff")
   (version "4.1")
   (source (origin
             (method url-fetch)
            (uri "mirror://sourceforge/snpeff/snpEff_v4_1_core.zip")
            (sha256
             (base32 "1vjgj6aacjsw6iczy09h18q5kx8ppxrrcq8w38g159zq7y3732kb"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((current-dir (getcwd))
                   (out (assoc-ref %outputs "out"))
                   (bin (string-append out "/share/java/" ,name))
                   (share (string-append out "/share/snpeff"))
                   (clinvar-file (string-append
                                  (assoc-ref inputs "clinvar")
                                  "/share/clinvar/GRCh37/clinvar.vcf.gz"))
                   (snpeff-db-dir (string-append share "/data"))
                   (snpeff-db (assoc-ref inputs "snpeff-database"))
                   (dbsnp-file (string-append (assoc-ref inputs "dbsnp")
                                             "/share/dbsnp/dbSnp.vcf.gz"))
                   (create-and-copy
                    (lambda (dir)
                      (mkdir (string-append bin "/" dir))
                      (copy-recursively dir (string-append bin "/" dir)))))
              (mkdir-p bin)
              (mkdir-p share)
              (substitute* "snpEff.config"
                (("data.dir = ./data/")
                 (string-append "data.dir = " share "/data"))
                (("database.local.clinvar      = ./db/GRCh38/clinvar/clinvar-latest.vcf.gz")
                 (string-append "database.local.clinvar      = " clinvar-file))
                (("database.local.dbsnp        = ./db/GRCh38/dbSnp/dbSnp.vcf.gz")
                 (string-append "database.local.dbsnp        = " dbsnp-file)))
              (chdir share)
              (system* (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip") snpeff-db)
              (chdir current-dir)

              (install-file "snpEff.config" bin)
              (install-file "snpEff.jar" bin)
              (install-file "SnpSift.jar" bin)
              (map create-and-copy '("scripts" "galaxy"))))))))
   (native-inputs
    `(("unzip" ,unzip)
      ("perl" ,perl)
      ("python" ,python-2)
      ("bash" ,bash)
      ("r" ,r)))
   (inputs
    `(("perl" ,perl)
      ("python" ,python)
      ("bash" ,bash)
      ("r" ,r)
      ("icedtea" ,icedtea-7)
      ("clinvar" ,clinvar-grch37)
      ("gwascatalog" ,gwascatalog)
      ("dbnsfp" ,dbnsfp)
      ("snpeff-database"
       ,(origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/snpeff/databases/v4_1/"
               "snpEff_v4_1_GRCh37.74.zip"))
         (sha256
          (base32 "1p02n1dd4b04vf425wm7c5b749rjxj6va78ibbfzdhggl38wg345"))))
      ("dbsnp" ,dbsnp)))
   (home-page "http://snpeff.sourceforge.net/")
   (synopsis "Genetic variant annotation and effect prediction toolbox.")
   (description "Genetic variant annotation and effect prediction toolbox.
It annotates and predicts the effects of variants on genes (such as amino
acid changes).")
   ;; No license specified.
   (license (license:non-copyleft "file:///dev/null"))))

(define-public snpeff-bin-4.1h
 (package (inherit snpeff-bin-4.1)
  (name "snpeff")
  (version "4.1h")
  (source (origin
      (method url-fetch)
      (uri "mirror://sourceforge/snpeff/snpEff_v4_1h_core.zip")
      (sha256
        (base32 "1j45jp4y8wj0q01clxsx46w1f4jm2wh85yl1mbrha7qbqs8c1qn3"))))))

(define-public snpeff-bin-4.3t
 (package (inherit snpeff-bin-4.1)
  (name "snpeff")
  (version "4.3t")
  (source (origin
      (method url-fetch)
      (uri "mirror://sourceforge/snpeff/snpEff_v4_3t_core.zip")
      (sha256
       (base32 "0i12mv93bfv8xjwc3rs2x73d6hkvi7kgbbbx3ry984l3ly4p6nnm"))))
  (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (chdir "../snpEff")
            (let* ((current-dir (getcwd))
                   (out (assoc-ref %outputs "out"))
                   (bin (string-append out "/share/java/" ,name))
                   (patch-bin (string-append (assoc-ref %build-inputs "patch")
                                             "/bin/patch"))
                   (share (string-append out "/share/snpeff"))
                   (clinvar-file (string-append
                                  (assoc-ref inputs "clinvar")
                                  "/share/clinvar/GRCh37/clinvar.vcf.gz"))
                   (snpeff-db-dir (string-append share "/data"))
                   (snpeff-db (assoc-ref inputs "snpeff-database"))
                   (snpeff-db-GRCm38.86 (assoc-ref inputs "snpeff-database-GRCm38.86"))
                   (snpeff-db-GRCh37.75 (assoc-ref inputs "snpeff-database-GRCh37.75"))
                   (snpeff-db-UMD3.1.86 (assoc-ref inputs "snpeff-database-UMD3.1.86"))
                   (snpeff-db-GRCh38.86 (assoc-ref inputs "snpeff-database-GRCh38.86"))
                   (dbsnp-dir (string-append (assoc-ref inputs "dbsnp")
                                             "/share/dbsnp/"))
                   (gwascatalog-file (string-append
                                      (assoc-ref inputs "gwascatalog")
                                      "/share/gwascatalog/gwascatalog.txt"))
                   (dbnsfp-file (string-append
                                 (assoc-ref inputs "dbnsfp")
                                 "/share/dbnsfp/dbNSFP2.9_gene.complete.gz"))
                   (create-and-copy
                    (lambda (dir)
                      (mkdir (string-append bin "/" dir))
                      (copy-recursively dir (string-append bin "/" dir)))))
              (mkdir-p bin)
              (mkdir-p share)
              (substitute* "snpEff.config"
                (("data.dir = ./data/")
                 (string-append "data.dir = " share "/data"))
                (("database.clinvar.GRCh37                 = ./db/GRCh37/clinvar/clinvar-latest.vcf.gz")
                 (string-append "database.clinvar.GRCh37      = " clinvar-file))
                (("database.dbsnp.GRCh37                   = ./db/GRCh37/dbSnp/")
                 (string-append "database.dbsnp.GRCh37        = " dbsnp-dir))
                (("database.gwascatalog.GRCh37             = ./db/GRCh37/gwasCatalog/gwascatalog.txt")
                 (string-append "database.gwascatalog.GRCh37        = " gwascatalog-file))
                (("database.dbnsfp.GRCh37                  = ./db/GRCh37/dbNSFP/dbNSFP.txt.gz")
                 (string-append "database.dbnsfp.GRCh37                  = " dbnsfp-file)))
              (chdir share)
              (system* (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip") snpeff-db)
              (system* (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip") snpeff-db-GRCm38.86)
              (system* (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip") snpeff-db-GRCh37.75)
              (system* (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip") snpeff-db-GRCh38.86)
              (system* (string-append (assoc-ref inputs "unzip")
                                      "/bin/unzip") snpeff-db-UMD3.1.86)

              (chdir current-dir)
              (install-file "snpEff.config" bin)
              (install-file "snpEff.jar" bin)
              (install-file "SnpSift.jar" bin)
              (for-each create-and-copy '("scripts" "galaxy"))

              ;; Backport settings from an older snpEff version by
              ;; applying the following patch.
              (with-directory-excursion bin
                (format #t "Applying patches... ")
                (let ((patch-file (assoc-ref %build-inputs "patch-file")))
                  (format #t
                   (if (zero? (system (string-append patch-bin " < " patch-file)))
                       " Succeeded.~%"
                       " Failed.~%"))))

              #t))))))
  (native-inputs
    `(("unzip" ,unzip)
      ("perl" ,perl)
      ("python" ,python-2)
      ("bash" ,bash)
      ("r" ,r)
      ("patch" ,patch)
      ("patch-file"
       ,(origin
         (method url-fetch)
         (uri (search-patch "snpeff-4.3t-backport-settings.patch"))
         (sha256
          (base32
           "1hw44vzcb6k8fq66740kd7kcdmb68bf5zbibc467bcxiiay8xpca"))))))
  (inputs
    `(("perl" ,perl)
      ("python" ,python)
      ("bash" ,bash)
      ("r" ,r)
      ("icedtea" ,icedtea-7)
      ("clinvar" ,clinvar-grch37)
      ("gwascatalog" ,gwascatalog)
      ("dbnsfp" ,dbnsfp)
      ("snpeff-database"
       ,(origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/snpeff/databases/v4_3/"
               "snpEff_v4_3_hg19.zip"))
         (sha256
          (base32 "0rnaa858shjgxx284m73ikf2a1k11n3gc7861svczm2f98wwhar2"))))
    ("snpeff-database-GRCm38.86"
       ,(origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/snpeff/databases/v4_3/"
               "snpEff_v4_3_GRCm38.86.zip"))
         (sha256
          (base32 "0rsdgv01yc33ppr8z412gk07xq098vsl8qhhii7s34kchk0qa746"))))
    ("snpeff-database-UMD3.1.86"
       ,(origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/snpeff/databases/v4_3/"
               "snpEff_v4_3_UMD3.1.86.zip"))
         (sha256
          (base32 "0h4d7w3n5pr1lfbmf921z4rx163n93qfw2klv94qw7syl3db6lli"))))
    ("snpeff-database-GRCh38.86"
       ,(origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/snpeff/databases/v4_3/"
               "snpEff_v4_3_GRCh38.86.zip"))
         (sha256
          (base32 "1rf8q7l732ayjq2lpny4s75zpij05j00151374nqblk4wri2mz0i"))))

    ("snpeff-database-GRCh37.75"
       ,(origin
         (method url-fetch)
         (uri (string-append
               "mirror://sourceforge/snpeff/databases/v4_3/"
               "snpEff_v4_3_GRCh37.75.zip"))
         (sha256
          (base32 "19c8wwx91vq47z7j7f455vsv8jw067x5rd7449d1z0nln82zpmhm"))))
      ("dbsnp" ,dbsnp)))))

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

(define-public gwascatalog
  (package
   (name "gwascatalog")
   (version "GRCh37")
   (source (origin
            (method url-fetch)
            ;(uri "http://www.genome.gov/admin/gwascatalog.txt")
            (uri "http://www.roelj.com/gwascatalog.txt")
            (sha256
             (base32
              "137xb3r3w6k8syj6dh6a856fvszcjlylwpzp98m35w5q52vxhdnx"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let ((source-file (assoc-ref %build-inputs "source"))
              (output-dir (string-append %output "/share/gwascatalog")))
          (mkdir-p output-dir)
          (copy-file source-file
                     (string-append output-dir "/gwascatalog.txt"))))))
   (home-page "http://www.genome.gov/")
   (synopsis "Extra data sets used by snpEff.")
   (description "This package contains extra data sets used by snpEff.")
   ;; No license specified.
   (license (license:non-copyleft "file:///dev/null"))))

(define-public gnomad-sv-sites-2.1
  (package
   (name "gnomad-sv-sites")
   (version "2.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://storage.googleapis.com/gnomad-public/"
                  "papers/2019-sv/gnomad_v" version "_sv.sites.vcf.gz"))
            (sha256
             (base32
              "18gxfnar8n5r06mj0ykyq4fkw3q3qqbrfnprgi18db0xzf6lh94k"))))
   (build-system trivial-build-system)
   (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((gzip     (string-append (assoc-ref %build-inputs "gzip") "/bin/gzip"))
               (sv-sites (assoc-ref %build-inputs "source"))
               (out      (string-append %output "/share/gnomad")))
           (mkdir-p out)
           (with-directory-excursion out
             (zero? (system
                     (string-append
                      gzip " -d " sv-sites
                      " -c > gnomad_v2.1_sv.sites.vcf"))))))))
   (inputs `(("gzip" ,gzip)))
   (home-page "https://gnomad.broadinstitute.org")
   (synopsis "gnomAD structural variant sites")
   (description "This package provides in uncompressed version of the gnomAD
 structural variant sites.")
   (license license:cc0)))

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
    (version "2.4.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ViennaRNA/ViennaRNA"
                                  "/releases/download/v" version "/ViennaRNA-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0z35d59hkc2ynb7rh6np2kbgx9ignasm09r7r0hlisivgknwyxmj"))))
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
