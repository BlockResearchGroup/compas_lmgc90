# LMGC90 Compas Wrapper

## TL;DR (Commands only)
```bash
# Dependencies
sudo apt install -y gfortran gcc g++ cmake libopenblas-dev liblapack-dev

# Build minimal LMGC90 core (no Python/SWIG/HDF5)
cd /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev
rm -rf build_core_min
mkdir build_core_min && cd build_core_min
cmake .. \
  -DCMAKE_BUILD_TYPE=Release \
  -DBUILD_ChiPy=OFF \
  -DBUILD_PRE=OFF \
  -DBUILD_POST=OFF \
  -DNO_DOXYGEN=ON \
  -DMATLIB_VERSION=none \
  -DSPARSE_LIBRARY=none \
  -DWITH_HDF5=OFF \
  -DBUILD_STANDALONE=OFF \
  -DBUILD_STANDALONE_MPI=OFF
cmake --build . -j

# Build and run wrapper
cd /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Sandbox/compas_fortran_c
export LMGC_INCLUDE=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min/modules
export LMGC_LIB_PATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min/lib
make clean
make all
LD_LIBRARY_PATH=$LMGC_LIB_PATH:$LD_LIBRARY_PATH make run
```

Simple C interface to LMGC90 mechanical simulation engine.

## Files
- `src/wrap_lmgc90_compas.f90` - Fortran wrapper with C interface
- `src/wrap_lmgc90_compas.h` - C header file
- `src/comgc90.c` - Example C program
- `DATBOX/` - Simulation input data
- `Makefile` - Build script

## API
- `lmgc90_initialize()` - Start simulation
{{ ... }}
- `lmgc90_compute()` - Run computation
- `lmgc90_finalize()` - Clean up
- `lmgc90_get_nb_inters()` - Get number of interactions
 - `lmgc90_get_all_inters()` - Get interaction data

## Quick Start (minimal, no Python/SWIG/HDF5)

```bash
# Build minimal LMGC90 core
cd /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev
rm -rf build_core_min
mkdir build_core_min && cd build_core_min
cmake .. \
  -DCMAKE_BUILD_TYPE=Release \
  -DBUILD_ChiPy=OFF \
  -DBUILD_PRE=OFF \
  -DBUILD_POST=OFF \
  -DNO_DOXYGEN=ON \
  -DMATLIB_VERSION=none \
  -DSPARSE_LIBRARY=none \
  -DWITH_HDF5=OFF \
  -DBUILD_STANDALONE=OFF \
  -DBUILD_STANDALONE_MPI=OFF
cmake --build . -j

# Build and run wrapper
cd /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Sandbox/compas_fortran_c
export LMGC_INCLUDE=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min/modules
export LMGC_LIB_PATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min/lib
make clean
make all
LD_LIBRARY_PATH=$LMGC_LIB_PATH:$LD_LIBRARY_PATH make run
```

## Installation (Linux)
```bash
sudo apt install -y gfortran gcc g++ cmake libopenblas-dev liblapack-dev
```

## Example Output
```
1 bulk materials found
90 RBDY3 found
Starting computation at time= 0.0000000D+00 Step= 0
860 PRPRx found
Total time of detection: 2.9068000000000538E-002
...
0 -> PRPRx 1/2 (stick)
1 -> PRPRx 3/4 (stick)
...
859 -> PRPRx 48/78 (stick)
```

Shows 90 rigid bodies with 859 contact interactions between particles.

## Notes
- ChiPy creates unified `liblmgc90.so` library
- DATBOX contains simulation input data
- Part of LMGC90 (CeCILL license)

# Windows
- https://www.intel.com/content/www/us/en/developer/tools/oneapi/hpc-toolkit-download.html
- https://github.com/Kitware/CMake/releases

