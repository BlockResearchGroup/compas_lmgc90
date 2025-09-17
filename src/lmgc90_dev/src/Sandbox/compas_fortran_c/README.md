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

REM Prereqs: Install Intel oneAPI HPC Toolkit + CMake

REM 1) Environment
call "%ONEAPI_ROOT%\setvars.bat"

REM 2) Build LMGC90 core (minimal)
cd C:\path\to\compas_lmgc90\src\lmgc90_dev
rmdir /S /Q build_core_min_win 2>nul
mkdir build_core_min_win && cd build_core_min_win
cmake .. -G "NMake Makefiles" ^
  -DCMAKE_BUILD_TYPE=Release ^
  -DBUILD_ChiPy=OFF ^
  -DBUILD_PRE=OFF ^
  -DBUILD_POST=OFF ^
  -DNO_DOXYGEN=ON ^
  -DMATLIB_VERSION=none ^
  -DSPARSE_LIBRARY=none ^
  -DWITH_HDF5=OFF ^
  -DBUILD_STANDALONE=OFF ^
  -DBUILD_STANDALONE_MPI=OFF ^
  -DBLA_VENDOR=Intel10_64lp
cmake --build . --config Release -j

REM 3) Build and run wrapper
cd C:\path\to\compas_lmgc90\src\lmgc90_dev\src\Sandbox\compas_fortran_c
set LMGC_INCLUDE=C:\path\to\compas_lmgc90\src\lmgc90_dev\build_core_min_win\modules
set LMGC_LIB_PATH=C:\path\to\compas_lmgc90\src\lmgc90_dev\build_core_min_win\lib

ifx /nologo /c src\wrap_lmgc90_compas.f90 /I "%LMGC_INCLUDE%"
cl  /nologo /c src\comgc90.c           /I "%LMGC_INCLUDE%"

ifx /nologo /Qmkl /Fe:comgc90.exe wrap_lmgc90_compas.obj comgc90.obj ^
  /link /LIBPATH:"%LMGC_LIB_PATH%" ^
  lmgc_core_shared.lib lmgc_core_rigid_3d.lib lmgc_core_contact_3d.lib lmgc_core_contactor_3d.lib ^
  lmgc_core_kernel_3d.lib lmgc_core_post_3d.lib lmgc_core_utils.lib lmgc_core_mailx.lib ^
  lmgc_core_shared_contribs.lib lmgc_core_other_contribs.lib lmgc_core_utils_contribs.lib ^
  lmgc_ann.lib lmgc_clipper.lib predicates.lib librtree.lib minpack.lib demmefi.lib ^
  lmgc_exception.lib lmgc_core_mbs3d.lib lmgc_core_mbs2d.lib ^
  lmgc_bindings_FEM.lib lmgc_bindings_user.lib lmgc_bindings_sparse_la.lib lmgc_bindings_MBS.lib ^
  Clipper2.lib Clipper2Z.lib Clipper2utils.lib Clipper2Zutils.lib ^
  ann_euclid.lib ann_manhattan.lib

set PATH=%LMGC_LIB_PATH%;%PATH%
mkdir OUTBOX POSTPRO
comgc90.exe

# Prereqs: Install MSYS2, open the "MSYS2 UCRT64" shell

# 1) Toolchain
pacman -Syu --noconfirm
pacman -S --needed --noconfirm \
  mingw-w64-ucrt-x86_64-gcc \
  mingw-w64-ucrt-x86_64-gcc-fortran \
  mingw-w64-ucrt-x86_64-cmake \
  mingw-w64-ucrt-x86_64-make \
  mingw-w64-ucrt-x86_64-openblas

# 2) Build LMGC90 core (minimal)
cd /c/path/to/compas_lmgc90/src/lmgc90_dev
rm -rf build_core_min_win
mkdir build_core_min_win && cd build_core_min_win
cmake .. -G "MinGW Makefiles" \
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

# 3) Build and run wrapper
cd /c/path/to/compas_lmgc90/src/lmgc90_dev/src/Sandbox/compas_fortran_c
export LMGC_INCLUDE=/c/path/to/compas_lmgc90/src/lmgc90_dev/build_core_min_win/modules
export LMGC_LIB_PATH=/c/path/to/compas_lmgc90/src/lmgc90_dev/build_core_min_win/lib

gfortran -c src/wrap_lmgc90_compas.f90 -I "$LMGC_INCLUDE"
gcc      -c src/comgc90.c              -I "$LMGC_INCLUDE"

gcc comgc90.o wrap_lmgc90_compas.o -o comgc90.exe -L"$LMGC_LIB_PATH" \
  -Wl,--start-group \
  -llmgc_core_shared -llmgc_core_rigid_3d -llmgc_core_contact_3d -llmgc_core_contactor_3d \
  -llmgc_core_kernel_3d -llmgc_core_post_3d -llmgc_core_utils -llmgc_core_mailx \
  -llmgc_core_shared_contribs -llmgc_core_other_contribs -llmgc_core_utils_contribs \
  -llmgc_ann -llmgc_clipper -lpredicates -llibrtree -lminpack -ldemmefi -llmgc_exception \
  -llmgc_core_mbs3d -llmgc_core_mbs2d \
  -llmgc_bindings_FEM -llmgc_bindings_user -llmgc_bindings_sparse_la -llmgc_bindings_MBS \
  -lClipper2 -lClipper2Z -lClipper2utils -lClipper2Zutils \
  -lann_euclid -lann_manhattan \
  -Wl,--end-group \
  -lopenblas -lgfortran -lquadmath -lstdc++ -lm

export PATH="$LMGC_LIB_PATH:$PATH"
mkdir -p OUTBOX POSTPRO
./comgc90.exe