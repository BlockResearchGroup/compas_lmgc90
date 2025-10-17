# LMGC90 Windows Build Instructions

## Prerequisites

- Intel oneAPI HPC Toolkit OR MSYS2 with GCC toolchain
- CMake 3.15+
- Python 3.8+ with development headers
- Git

## Code Fixes Required

Before building, ensure these fixes are applied:

### 1. Fix Fortran wrapper function signature

In `src/lmgc90_dev/src/Sandbox/Compas/wrap_lmgc90_compas.f90`, line 302:

**Change from:**
```fortran
call set_time_step(dt)
```

**Change to:**
```fortran
call set_time_step(dt, .false.)
```

### 2. Fix C++ binding function name

In `src/lmgc90.cpp`, lines 19 and 43:

**Change from:**
```cpp
void lmgc90_compute(void);
// ...
lmgc90_compute();
```

**Change to:**
```cpp
void lmgc90_compute_one_step(void);
// ...
lmgc90_compute_one_step();
```

Two options for building on Windows:

## Option 1: Intel oneAPI (Recommended)

**Prerequisites:** [Intel oneAPI HPC Toolkit](https://www.intel.com/content/www/us/en/developer/tools/oneapi/hpc-toolkit-download.html), [CMake](https://github.com/Kitware/CMake/releases)

```batch
REM Setup environment
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"

REM Build LMGC90 core
cd C:\brg\code_fortran\compas_lmgc90\src\lmgc90_dev
rmdir /S /Q build_core_min_win 2>nul
mkdir build_core_min_win && cd build_core_min_win
cmake .. -G "NMake Makefiles" ^
  -DCMAKE_BUILD_TYPE=Release -DBUILD_ChiPy=OFF -DBUILD_PRE=OFF -DBUILD_POST=OFF ^
  -DNO_DOXYGEN=ON -DMATLIB_VERSION=none -DSPARSE_LIBRARY=none -DWITH_HDF5=OFF ^
  -DBUILD_STANDALONE=OFF -DBUILD_STANDALONE_MPI=OFF -DBLA_VENDOR=Intel10_64lp ^
  -DCMAKE_PREFIX_PATH="%MKLROOT%" ^
  -DBLAS_LIBRARIES="%MKLROOT%/lib/mkl_intel_lp64.lib;%MKLROOT%/lib/mkl_sequential.lib;%MKLROOT%/lib/mkl_core.lib" ^
  -DLAPACK_LIBRARIES="%MKLROOT%/lib/mkl_intel_lp64.lib;%MKLROOT%/lib/mkl_sequential.lib;%MKLROOT%/lib/mkl_core.lib"

cmake --build . --config Release -j

REM Create lib directory and copy libraries (required for Python bindings)
mkdir lib
for /r %%i in (*.lib) do copy "%%i" lib\

REM Build wrapper
cd C:\brg\code_fortran\compas_lmgc90\src\lmgc90_dev\src\Sandbox\compas_fortran_c
set LMGC_INCLUDE=C:\brg\code_fortran\compas_lmgc90\src\lmgc90_dev\build_core_min_win\modules
set LMGC_LIB_PATH=C:\brg\code_fortran\compas_lmgc90\src\lmgc90_dev\build_core_min_win\lib

ifx /nologo /c src\wrap_lmgc90_compas.f90 /I "%LMGC_INCLUDE%"
cl  /nologo /MD /O2 /c src\comgc90.c /I "%LMGC_INCLUDE%"
ifx /nologo /Fe:comgc90.exe wrap_lmgc90_compas.obj comgc90.obj ^
  /link /LIBPATH:"%LMGC_LIB_PATH%" /LIBPATH:"%MKLROOT%\lib" ^
  lmgc_core_shared.lib lmgc_core_rigid_3d.lib lmgc_core_contact_3d.lib lmgc_core_contactor_3d.lib ^
  lmgc_core_kernel_3d.lib lmgc_core_post_3d.lib lmgc_core_utils.lib lmgc_core_mailx.lib ^
  lmgc_core_shared_contribs.lib lmgc_core_other_contribs.lib lmgc_core_utils_contribs.lib ^
  lmgc_ann.lib lmgc_clipper.lib predicates.lib librtree.lib minpack.lib demmefi.lib lmgc_exception.lib ^
  lmgc_core_mbs3d.lib lmgc_core_mbs2d.lib lmgc_bindings_FEM.lib lmgc_bindings_user.lib ^
  lmgc_bindings_sparse_la.lib lmgc_bindings_MBS.lib Clipper2.lib Clipper2Z.lib Clipper2utils.lib ^
  Clipper2Zutils.lib ann_euclid.lib ann_manhattan.lib mkl_intel_lp64.lib mkl_sequential.lib mkl_core.lib

set PATH=%LMGC_LIB_PATH%;%PATH%
mkdir OUTBOX POSTPRO
comgc90.exe
```

## Option 2: MSYS2

**Prerequisites:** [MSYS2](https://www.msys2.org/) (use MSYS2 UCRT64 shell)

```bash
# Install toolchain
pacman -S --needed mingw-w64-ucrt-x86_64-gcc mingw-w64-ucrt-x86_64-gcc-fortran ^
  mingw-w64-ucrt-x86_64-cmake mingw-w64-ucrt-x86_64-make mingw-w64-ucrt-x86_64-openblas

# Build LMGC90 core
cd /c/brg/code_fortran/compas_lmgc90/src/lmgc90_dev
rm -rf build_core_min_win && mkdir build_core_min_win && cd build_core_min_win

cmake .. -G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=Release -DBUILD_ChiPy=OFF ^
  -DBUILD_PRE=OFF -DBUILD_POST=OFF -DNO_DOXYGEN=ON -DMATLIB_VERSION=none ^
  -DSPARSE_LIBRARY=none -DWITH_HDF5=OFF -DBUILD_STANDALONE=OFF -DBUILD_STANDALONE_MPI=OFF

cmake --build . -j

# Create lib directory and copy libraries (required for Python bindings)
mkdir -p lib
find . -name "*.a" -exec cp {} lib/ \;

# Build wrapper
cd /c/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Sandbox/compas_fortran_c
export LMGC_INCLUDE=/c/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_win/modules
export LMGC_LIB_PATH=/c/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_win/lib

gfortran -c src/wrap_lmgc90_compas.f90 -I "$LMGC_INCLUDE"
gcc -c src/comgc90.c -I "$LMGC_INCLUDE"
gcc comgc90.o wrap_lmgc90_compas.o -o comgc90.exe -L"$LMGC_LIB_PATH" -Wl,--start-group ^
  -llmgc_core_shared -llmgc_core_rigid_3d -llmgc_core_contact_3d -llmgc_core_contactor_3d ^
  -llmgc_core_kernel_3d -llmgc_core_post_3d -llmgc_core_utils -llmgc_core_mailx ^
  -llmgc_core_shared_contribs -llmgc_core_other_contribs -llmgc_core_utils_contribs ^
  -llmgc_ann -llmgc_clipper -lpredicates -llibrtree -lminpack -ldemmefi -llmgc_exception ^
  -llmgc_core_mbs3d -llmgc_core_mbs2d -llmgc_bindings_FEM -llmgc_bindings_user ^
  -llmgc_bindings_sparse_la -llmgc_bindings_MBS -lClipper2 -lClipper2Z -lClipper2utils ^
  -lClipper2Zutils -lann_euclid -lann_manhattan -Wl,--end-group ^
  -lopenblas -lgfortran -lquadmath -lstdc++ -lm

export PATH="$LMGC_LIB_PATH:$PATH"
mkdir -p OUTBOX POSTPRO
./comgc90.exe
```

## Install Python Bindings

After the LMGC90 core is built and the code fixes are applied:

### For Intel oneAPI:
```batch
cd C:\brg\code_fortran\compas_lmgc90
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
pip install -e . --force-reinstall --no-deps
```

### For MSYS2:
```bash
cd /c/brg/code_fortran/compas_lmgc90
pip install -e . --force-reinstall --no-deps
```

## Troubleshooting

### Common Issues

1. **Missing libraries error**: Ensure the `lib` directory was created and populated after the core build
2. **Function signature errors**: Verify the code fixes in `wrap_lmgc90_compas.f90` and `lmgc90.cpp` are applied
3. **Intel oneAPI not found**: Make sure to call the setvars.bat script before building
4. **Python import errors**: Ensure all dependencies are installed: `pip install numpy compas compas_dem compas_viewer nanobind`
5. **MSYS2 path issues**: Use Unix-style paths (`/c/...`) in MSYS2 shell, Windows-style paths (`C:\...`) in Command Prompt