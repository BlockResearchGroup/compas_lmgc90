# LMGC90 Ubuntu Build with Intel oneAPI

## Prerequisites

- Intel oneAPI HPC Toolkit (includes compilers and MKL)
- CMake 3.15+
- Python 3.8+ with development headers
- Git

## Install Intel oneAPI

```bash
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null
echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneapi.list
sudo apt update
sudo apt install -y intel-oneapi-compiler-fortran intel-oneapi-compiler-dpcpp-cpp-and-cpp-classic intel-oneapi-mkl-devel cmake
source /opt/intel/oneapi/setvars.sh
```

## Build LMGC90 Core

```bash
cd /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev
rm -rf build_core_min_oneapi && mkdir build_core_min_oneapi && cd build_core_min_oneapi

cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx \
  -DCMAKE_BUILD_TYPE=Release -DBUILD_ChiPy=OFF -DBUILD_PRE=OFF -DBUILD_POST=OFF \
  -DNO_DOXYGEN=ON -DMATLIB_VERSION=none -DSPARSE_LIBRARY=none -DWITH_HDF5=OFF \
  -DBUILD_STANDALONE=OFF -DBUILD_STANDALONE_MPI=OFF -DBLA_VENDOR=Intel10_64lp -DMKL_ROOT=$MKLROOT

cmake --build . -j

# Create lib directory and copy libraries (required for Python bindings)
mkdir -p lib
find . -name "*.a" -exec cp {} lib/ \;
```

**Note:** The build creates static libraries (.a files) in various subdirectories. The `lib` directory consolidation is required for the Python bindings to find all libraries.

## Code Fixes Required

Before building the wrapper, ensure these fixes are applied:

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

## Build Wrapper

```bash
cd /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Sandbox/Compas
export LMGC_INCLUDE=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/modules
export LMGC_LIB_PATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/lib

gfortran -c wrap_lmgc90_compas.f90 -I "$LMGC_INCLUDE" -fPIC
icx -c comgc90.c -I "$LMGC_INCLUDE" -fPIC

icpx -o comgc90 wrap_lmgc90_compas.o comgc90.o -L"$LMGC_LIB_PATH" -Wl,--start-group \
  -llmgc_core_shared -llmgc_core_rigid_3d -llmgc_core_contact_3d -llmgc_core_contactor_3d \
  -llmgc_core_kernel_3d -llmgc_core_post_3d -llmgc_core_utils -llmgc_core_mailx \
  -llmgc_core_shared_contribs -llmgc_core_other_contribs -llmgc_core_utils_contribs \
  -llmgc_ann -llmgc_clipper -lpredicates -llibrtree -lminpack -ldemmefi -llmgc_exception \
  -llmgc_core_mbs3d -llmgc_core_mbs2d -llmgc_bindings_FEM -llmgc_bindings_user \
  -llmgc_bindings_sparse_la -llmgc_bindings_MBS -lClipper2 -lClipper2Z -lClipper2utils \
  -lClipper2Zutils -lann_euclid -lann_manhattan -Wl,--end-group \
  -L${MKLROOT}/lib/intel64 -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lgfortran -lpthread -lm -ldl

mkdir -p OUTBOX POSTPRO
LD_LIBRARY_PATH=$LMGC_LIB_PATH:${MKLROOT}/lib:$LD_LIBRARY_PATH ./comgc90
```

## Alternative: GCC Build (No Intel oneAPI)

```bash
sudo apt install -y gfortran gcc g++ cmake libopenblas-dev liblapack-dev

cd /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev
rm -rf build_core_min && mkdir build_core_min && cd build_core_min

cmake .. -DCMAKE_BUILD_TYPE=Release -DBUILD_ChiPy=OFF -DBUILD_PRE=OFF -DBUILD_POST=OFF \
  -DNO_DOXYGEN=ON -DMATLIB_VERSION=none -DSPARSE_LIBRARY=none -DWITH_HDF5=OFF \
  -DBUILD_STANDALONE=OFF -DBUILD_STANDALONE_MPI=OFF

cmake --build . -j

cd /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Sandbox/Compas
export LMGC_INCLUDE=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min/modules
export LMGC_LIB_PATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min/lib
make clean && make all
LD_LIBRARY_PATH=$LMGC_LIB_PATH:$LD_LIBRARY_PATH make run
```

## Install Python Bindings

After the LMGC90 core is built and the code fixes are applied:

```bash
cd ~/brg/code_fortran/compas_lmgc90/
source /opt/intel/oneapi/setvars.sh --force
pip install -e . --force-reinstall --no-deps
```

## Complete Automated Workflow

Once all fixes are applied, you can use this single command to rebuild and run:

```bash
cd ~/brg/code_fortran/compas_lmgc90/ && clear && source /opt/intel/oneapi/setvars.sh --force && pip install -e . --force-reinstall --no-deps && cd src/lmgc90_dev/src/Sandbox/Compas && python /home/pv/brg/code_fortran/compas_lmgc90/temp/visualize_with_orientation.py
```
```bash
cd ~/brg/code_fortran/compas_lmgc90/ && clear && source /opt/intel/oneapi/setvars.sh --force && pip install -e . --force-reinstall --no-deps && cd src/lmgc90_dev/src/Sandbox/Compas && python /home/pv/brg/code_fortran/compas_lmgc90/temp/visualize_with_orientation_iterative.py
```

## Troubleshooting

### Common Issues

1. **Missing libraries error**: Ensure the `lib` directory was created and populated after the core build
2. **Function signature errors**: Verify the code fixes in `wrap_lmgc90_compas.f90` and `lmgc90.cpp` are applied
3. **Intel OneAPI not found**: Make sure to source the environment with `source /opt/intel/oneapi/setvars.sh --force`
4. **Python import errors**: Ensure all dependencies are installed: `pip install numpy compas compas_dem compas_viewer nanobind`