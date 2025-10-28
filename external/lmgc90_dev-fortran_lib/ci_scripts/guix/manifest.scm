;; Manifest to build a LMGC90-friendly environment

(specifications->manifest
  (list
    "coreutils@9.1"
    "bash@5.2.37"
    "sed@4.9"
    "grep@3.11"
    "which@2.21"
    "vim@9.1.1525"
    "make@4.4.1"
    "cmake@4.0.3"
    "gcc-toolchain@14.3.0"
    "gfortran-toolchain@14.3.0"
    ;;"openblas-openmp@0.3.29"
    "openblas@0.3.29"
    "hdf5@1.14.6"
    "swig@4.3.1"
    "python-wrapper@3.11.11"
    ;; need an old version of numpy because of scipy and h5py
    "python-numpy@1.26.4"
    ;;"python-numpy@2.2.5"
    "python-scipy@1.12.0"
    "python-h5py@3.13.0"
    "vtk@9.3.1"
  )
)

