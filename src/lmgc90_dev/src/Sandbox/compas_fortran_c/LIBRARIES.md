# LMGC90 Library Dependencies

Simple overview of what libraries are needed to build LMGC90 wrapper.

## Core Libraries (Required)

### **Compilers**
- **gfortran** - Fortran compiler (for LMGC90 core)
- **gcc/g++** - C/C++ compiler (for wrapper)

### **Build System**
- **cmake** - Cross-platform build system

### **Linear Algebra**
- **OpenBLAS** - Optimized BLAS/LAPACK implementation
- Provides: Matrix operations, linear solvers

### **Python Bindings (ChiPy)**
- **Python 3** - Python interpreter
- **NumPy** - Numerical arrays
- **SWIG** - Interface generator (Fortran ↔ Python)

## What Each Library Does

| Library | Purpose | Size | Complexity |
|---------|---------|------|------------|
| gfortran | Compile LMGC90 Fortran code | Medium | Low |
| gcc/g++ | Compile C++ wrapper | Small | Low |
| cmake | Build system | Small | Low |
| OpenBLAS | Fast matrix math | Medium | Medium |
| Python 3 | ChiPy bindings | Large | Low |
| NumPy | Python arrays | Medium | Low |
| SWIG | Generate bindings | Small | Low |

## Libraries We DON'T Need

- ❌ **HDF5** - File I/O (disabled with `-DWITH_HDF5=OFF`)
- ❌ **MatLib** - Material models (disabled with `-DMATLIB_VERSION=none`)
- ❌ **MUMPS** - Sparse solvers (disabled with `-DSPARSE_LIBRARY=none`)
- ❌ **MPI** - Parallel computing (not needed for basic wrapper)

## Direct Download Links (Manual Installation)

### **Compilers**

#### **Intel Fortran Compiler (Recommended for Windows)**
- **Download**: [Intel oneAPI HPC Toolkit](https://www.intel.com/content/www/us/en/developer/tools/oneapi/hpc-toolkit-download.html)
- **License**: Free for all users
- **Includes**: ifx, ifort, Intel MKL (BLAS/LAPACK)
- **Size**: ~3 GB

#### **MinGW-w64 (Alternative)**
- **Download**: [MinGW-w64 Releases](https://github.com/niXman/mingw-builds-binaries/releases)
- **Choose**: `x86_64-*-release-posix-seh-ucrt-*.7z`
- **Includes**: gfortran, gcc, g++
- **Size**: ~200 MB

#### **Visual Studio (C++ only)**
- **Download**: [Visual Studio Community](https://visualstudio.microsoft.com/downloads/)
- **License**: Free
- **Select**: C++ Desktop Development workload

### **Build System**
- **CMake**: [CMake Releases](https://github.com/Kitware/CMake/releases)
- **Choose**: `cmake-*-windows-x86_64.msi`
- **Size**: ~40 MB

### **Linear Algebra**

#### **Intel MKL (with Intel Fortran)**
- **Included**: With Intel oneAPI HPC Toolkit
- **Performance**: Fastest option
- **No separate download needed**

#### **OpenBLAS (Alternative)**
- **Download**: [OpenBLAS Releases](https://github.com/xianyi/OpenBLAS/releases)
- **Choose**: `OpenBLAS-*-x64.zip` (Windows)
- **Size**: ~20 MB
- **Manual setup required**

### **Python Ecosystem**
- **Python**: [Python.org Downloads](https://www.python.org/downloads/)
- **Choose**: Latest Python 3.x Windows installer
- **Size**: ~30 MB
- **Then install**: `pip install numpy`

#### **SWIG**
- **Download**: [SWIG Releases](https://github.com/swig/swig/releases)
- **Choose**: `swigwin-*.zip`
- **Size**: ~15 MB
- **Extract and add to PATH**

## Installation Order (Windows)

### **Option 1: Intel Fortran + Visual Studio**
1. **Visual Studio Community** → Install C++ workload
2. **Intel oneAPI HPC Toolkit** → Includes Fortran + MKL
3. **Python** → From python.org installer
4. **NumPy** → `pip install numpy`
5. **CMake** → From GitHub releases
6. **SWIG** → Extract and add to PATH

### **Option 2: MinGW-w64 Only**
1. **MinGW-w64** → Extract to `C:\mingw64\`
2. **Add to PATH** → `C:\mingw64\bin`
3. **OpenBLAS** → Extract and configure manually
4. **Python** → From python.org installer
5. **NumPy** → `pip install numpy`
6. **CMake** → From GitHub releases
7. **SWIG** → Extract and add to PATH

### **Linux (Ubuntu)**
```bash
sudo apt install gfortran gcc g++ cmake libopenblas-dev python3-dev python3-numpy swig
```
All libraries available in package manager.

## Manual OpenBLAS Setup (Windows)

If using MinGW-w64 + OpenBLAS manually:

### **1. Download and Extract**
```powershell
# Download OpenBLAS-*-x64.zip from GitHub releases
# Extract to C:\OpenBLAS\
```

### **2. Set Environment Variables**
```powershell
# Add to Windows environment:
set OPENBLAS_ROOT=C:\OpenBLAS
set PATH=%PATH%;C:\OpenBLAS\bin
```

### **3. CMake Configuration**
```powershell
cmake .. ^
  -G "MinGW Makefiles" ^
  -DCMAKE_BUILD_TYPE=Release ^
  -DBUILD_ChiPy=ON ^
  -DNO_DOXYGEN=ON ^
  -DMATLIB_VERSION=none ^
  -DSPARSE_LIBRARY=none ^
  -DWITH_HDF5=OFF ^
  -DBLA_VENDOR=OpenBLAS ^
  -DOpenBLAS_ROOT=C:\OpenBLAS
```

## Final Output

After building, you get:
- **liblmgc90.so** (Linux) or **lmgc90.dll** (Windows)
- **Fortran modules** (*.mod files)
- **C header** (wrap_lmgc90_compas.h)

Total size: ~10-15 MB unified library.
