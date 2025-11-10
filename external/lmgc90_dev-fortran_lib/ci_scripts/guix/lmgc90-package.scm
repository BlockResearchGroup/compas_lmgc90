;; package definition of LMGC90 python module

(use-modules (guix packages)
             (guix download) ;; to download source files
             ;;(guix gexp) ;; to use local files...
             (guix build-system cmake)
             (guix licenses)
             ;; for the dependencies
             (gnu  packages base)
             (gnu  packages compression)      ;; for unzip
             (gnu  packages cmake)
             (gnu  packages commencement)
             (gnu  packages maths)            ;; for openblas and HDF5
             (gnu  packages swig)
             (gnu  packages python)
             (gnu  packages python-xyz)       ;; for numpy and h5py
             (gnu  packages python-science)   ;; for scipy
             (gnu  packages image-processing) ;; for vtk
)

(define python-version
  (let* ( (version (package-version python) )
          (components (string-split version #\.)))
        ( string-append (list-ref components 0) "." (list-ref components 1))
  )
)
       
(define-public lmgc90_dev
  (package
    (name "lmgc90_dev")
    (version "latest")
    (source
      ;;(local-file "/storage/simple/projects/lmgc/LMGC90/lmgc90_dev" #:recursive? #t)
      ;;to test after the tag of the next version
      (origin (method url-fetch)
      ;;        (uri "https://seafile.lmgc.univ-montp2.fr/f/7c32c8f4d4c8456aaef9/?dl=1")
      ;;        (file-name "lmgc90_dev.tar.gz")
      ;;        (sha256 (base32 "0zap2f837i7yc9y5jyvw9vzzl601yw51r6q28zp9fafp1p8qgsfl"))
              (uri "https://lmgc90.pages-git-xen.lmgc.univ-montp2.fr/lmgc90_dev/downloads/lmgc90_user_2025.rc2.zip")
              (file-name "lmgc90_user_2025.rc2.zip")
              (sha256 (base32 "0mfb7x6v59wr0fczxv6q30fjdkldpv2nd88pp90kk2lmb7654s6w"))
              ;               "1234567890123456789012345678901234567890123456789012"
      )

    )
    (build-system cmake-build-system)
    (arguments
      `(
	;;copilot told me to put this to remove some warnings of matplotlib...
        ;;#:phases (modify-phases %standard-phases
        ;;           (add-before 'check 'set-matplotlib-cache
        ;;             (lambda _ (setenv "MPLCONFIGDIR" "/tmp/mpl-cache") #t)
	;;	   )
	;;	 )
	#:build-type ""
        #:configure-flags (list (string-append "-DCMAKE_INSTALL_PREFIX=" %output)
                                (string-append "-DGUIX_PYTHON_INSTALL_PATH=" %output "/lib/python" ,python-version "/site-packages")
                          )
	;;it's bad... but to uncommend on IO cluster...
	;;#:tests? #f
       )


    )
    (native-inputs `(("unzip", unzip)
                     ("cmake", cmake)
                     ("swig" , swig )
                     ("swig" , swig )
                    )
    )
    (inputs `(("gcc"     , gcc-toolchain     )
              ("gfortran", gfortran-toolchain)
              ("openblas", openblas          )
              ("hdf5"    , hdf5              )
             )
    )
    (propagated-inputs `(("python", python-wrapper    )
                         ("numpy" , python-numpy      )
                         ("scipy" , python-scipy      )
                         ("h5py"  , python-h5py       )
                         ("vtk"   , vtk               )
                        )
    )
    (synopsis "LMGC90 project")
    (description "LMGC90 is a software dedicated to DEM modelling using non regularized contact laws. Its core if written in Fortran and the main API is through Python.")
    (home-page "https://git-xen.lmgc.univ-montp2.fr/lmgc90/lmgc90_user/-/wikis/home")
    (license cecill)
  )
)

;; to install the package from the file...
lmgc90_dev
