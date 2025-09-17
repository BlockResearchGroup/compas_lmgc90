# LMGC90 Compas Wrapper

Clean C API wrapper for LMGC90 integration with Compas-DEM framework using nanobind.

## Prerequisites

```bash
# Create conda environment
conda create -n lmgc90_env python=3.9
conda activate lmgc90_env
conda install -c conda-forge cmake gfortran clang clangxx
```

## Quick Start

```bash
# 1. Activate environment
conda activate lmgc90_env

# 2. Generate test data
cd /home/pv/brg/code_fortran/lmgc90_dev/src/Sandbox/Compas
python gen_dem_vault.py

# 3. Create required directories
mkdir -p OUTBOX POSTPRO

# 4. Set library path and run C wrapper
export LD_LIBRARY_PATH=/home/pv/brg/code_fortran/lmgc90_dev/build_fortran/lib:$LD_LIBRARY_PATH
./comgc90
```

## Build LMGC90 with Python Bindings

```bash
# Install dependencies
sudo apt install -y cmake gfortran gcc g++ python3-dev swig libblas-dev liblapack-dev

# Build LMGC90
cd /home/pv/brg/code_fortran/lmgc90_dev
mkdir build && cd build
cmake .. -DVENV_PATH=$CONDA_PREFIX -DSPARSE_LIBRARY=none -DMATLIB_VERSION=none -DWITH_HDF5=OFF
make
make install

# Generate test data
cd /home/pv/brg/code_fortran/lmgc90_dev/src/Sandbox/Compas
python command.py

# Build the wrapper for lmgc90
# The make wrap command uses the existing lmgc90.cfg file that was already created during the Fortran library build in build_fortran.
make wrap
export LD_LIBRARY_PATH=/home/pv/brg/code_fortran/lmgc90_dev/build_fortran/lib:$LD_LIBRARY_PATH
./comgc90
```

## C API Functions

```c
void lmgc90_initialize(void);
void lmgc90_compute(void);
void lmgc90_finalize(void);
int lmgc90_get_nb_inters();
void lmgc90_get_all_inters(struct lmgc90_inter_meca_3D * all_inters, int size);
```

## Files

- `wrap_lmgc90_compas.f90` - Fortran binding implementation
- `wrap_lmgc90_compas.h` - C header with API functions
- `comgc90.c` - Test program demonstrating usage
- `gen_dem_vault.py` - Generates vault geometry using compas_dem + pylmgc90

## Workflow

1. **Data Generation**: `gen_dem_vault.py` creates DATBOX files using pylmgc90 and compas_dem
2. **C API**: Clean interface for initialization, computation, and interaction data extraction
3. **Nanobind Ready**: Structured data perfect for Python integration

The C API provides exactly what's needed for Compas-DEM: simple functions and structured interaction data that can easily be converted to Python objects and numpy arrays.

## Install compas_dem

```bash
pip install compas_dem
```
