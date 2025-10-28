# CI Build Configuration

## Problem Solved

The GitHub Actions CI was failing with:
```
Could NOT find BLAS (missing: BLAS_LIBRARIES)
ERROR: Failed building wheel for compas_lmgc90
```

## Solution

Made the build **completely self-contained** with bundled Intel MKL libraries.

### Changes Made

#### 1. Updated CMakeLists.txt (Lines 92-107)

**Before:**
- Had fallback to OpenBLAS when Intel libs not found
- Would fail in CI because OpenBLAS not installed

**After:**
- **Always requires** bundled Intel MKL libraries
- **Fails explicitly** if libraries missing with clear error message
- No system BLAS/LAPACK dependencies needed

```cmake
# Use bundled Intel MKL libraries (self-contained, no system dependencies)
set(INTEL_LIBS_DIR "${CMAKE_CURRENT_SOURCE_DIR}/external/ubuntu/intel_libs")

# Check that bundled libraries exist
if(NOT EXISTS "${INTEL_LIBS_DIR}/libmkl_core.so.2")
    message(FATAL_ERROR 
        "Bundled Intel MKL libraries not found in: ${INTEL_LIBS_DIR}\n"
        "Please ensure external/ubuntu/intel_libs/ contains all required .so files.\n"
        "This package requires bundled Intel MKL libraries for BLAS/LAPACK support.")
endif()
```

#### 4. Pass BLAS Libraries to LMGC90 Core Build (Line 46)

**Critical fix for CI:**
The LMGC90 core build (as ExternalProject) was trying to find system BLAS using CMake's `find_package(BLAS)`, which would fail in CI.

**Solution:**
Explicitly pass bundled library paths to prevent auto-detection:

```cmake
-DBLAS_LIBRARIES=${CMAKE_CURRENT_SOURCE_DIR}/external/ubuntu/intel_libs/libmkl_intel_lp64.so.2;${CMAKE_CURRENT_SOURCE_DIR}/external/ubuntu/intel_libs/libmkl_sequential.so.2;${CMAKE_CURRENT_SOURCE_DIR}/external/ubuntu/intel_libs/libmkl_core.so.2
```

This tells CMake: "BLAS is already provided, don't search for it."

#### 2. Removed OpenBLAS Fallback

**Removed:**
```cmake
else()
    set(BLAS_LIBS -lopenblas)
    message(STATUS "Using OpenBLAS")
endif()
```

**Why:** 
- OpenBLAS won't be available in CI
- Package is now self-contained with bundled libs

#### 3. Simplified RPATH Configuration

**Before:**
```cmake
if(EXISTS "${INTEL_LIBS_DIR}/libmkl_core.so.2")
    set_target_properties(_lmgc90 PROPERTIES ...)
endif()
```

**After:**
```cmake
set_target_properties(_lmgc90 PROPERTIES
    BUILD_RPATH "${INTEL_LIBS_DIR}"
    INSTALL_RPATH "$ORIGIN/../external/ubuntu/intel_libs"
    INSTALL_RPATH_USE_LINK_PATH FALSE
)
```

**Why:** Redundant check removed since we verify libs exist earlier

## Bundled Libraries

### Location
```
external/ubuntu/intel_libs/  (20 files, 396 MB)
```

### Contents
**MKL Core (3):**
- libmkl_intel_lp64.so.2
- libmkl_sequential.so.2  
- libmkl_core.so.2

**MKL Optimized Kernels (4):**
- libmkl_def.so.2
- libmkl_avx2.so.2
- libmkl_avx512.so.2
- libmkl_mc3.so.2

**MKL Vector Math (5):**
- libmkl_vml_def.so.2
- libmkl_vml_avx2.so.2
- libmkl_vml_avx512.so.2
- libmkl_vml_mc3.so.2
- libmkl_vml_cmpt.so.2

**Intel Compiler Runtime (4):**
- libintlc.so.5
- libimf.so
- libsvml.so
- libirng.so

**Plus symlinks (4):**
- libmkl_core.so
- libmkl_intel_lp64.so
- libmkl_sequential.so
- libintlc.so

### Git Status
✅ All libraries committed to repository  
✅ Available in CI without installation

## CI Requirements

### What CI Needs
- ✅ GCC compilers (gcc, g++, gfortran) - **already in Ubuntu runners**
- ✅ Python build tools (pip, setuptools) - **already available**
- ✅ Git clone of repository - **includes bundled libs**

### What CI Does NOT Need
- ❌ Intel oneAPI installation
- ❌ System BLAS/LAPACK packages  
- ❌ `apt-get install libblas-dev liblapack-dev`
- ❌ Any environment variables

## Build Process

### Local Build
```bash
pip install -e .
```

### CI Build  
```bash
pip install .
```

Both work identically - no special setup required!

## Verification

### Test Build
```bash
rm -rf build
pip install -e .
python -c "from compas_lmgc90 import _lmgc90; print('Success!')"
```

### Check Libraries
```bash
ldd $(find . -name "_lmgc90*.so" | head -1) | grep mkl
```

Should show:
```
libmkl_core.so.2 => .../external/ubuntu/intel_libs/libmkl_core.so.2
libmkl_sequential.so.2 => .../external/ubuntu/intel_libs/libmkl_sequential.so.2
libmkl_intel_lp64.so.2 => .../external/ubuntu/intel_libs/libmkl_intel_lp64.so.2
```

## Build Configuration Output

Expected CMake output:
```
-- Using GCC compilers (no oneAPI environment required)
-- Using bundled Intel MKL from: .../external/ubuntu/intel_libs
===============================================
Build Configuration:
  Build Type: Release
  C++ Standard: C++17
  Compiler: g++
  BLAS: Intel MKL (bundled)
  Libs: .../external/ubuntu/intel_libs
===============================================
```

## Benefits

✅ **Self-Contained**: No system dependencies  
✅ **CI-Ready**: Works in GitHub Actions without setup  
✅ **Portable**: Same libs work across Ubuntu 20.04+  
✅ **Fast CI**: No package installation step needed  
✅ **Reproducible**: Same libs every build  
✅ **Performance**: Full Intel MKL optimization  

## Intel MKL Licensing

The bundled Intel MKL libraries are from Intel oneAPI 2025.2.  
Ensure compliance with [Intel Simplified Software License](https://www.intel.com/content/www/us/en/developer/articles/license/end-user-license-agreement.html) when distributing.

## Size Considerations

The bundled libraries add **~400 MB** to the repository size.  

**Alternatives if size is a concern:**
1. Use Git LFS for the `.so` files
2. Download libs during CI build (not recommended - adds complexity)
3. Use system OpenBLAS (requires CI modification)

**Current approach:** Bundle libraries for simplicity and reliability.
