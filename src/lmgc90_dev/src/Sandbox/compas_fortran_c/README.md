# LMGC90 Compas Wrapper

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

## Quick Start

**Step 1: Build LMGC90**
```bash
cd /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev
rm -rf build_fortran  # Clean previous build
mkdir build_fortran && cd build_fortran
cmake .. \
  -DCMAKE_BUILD_TYPE=Release \
  -DBUILD_ChiPy=ON \
  -DNO_DOXYGEN=ON \
  -DMATLIB_VERSION=none \
  -DSPARSE_LIBRARY=none \
  -DWITH_HDF5=OFF
make -j$(nproc)

# Create symlink for wrapper compatibility
cd lib && ln -sf ../pylmgc90/chipy/_lmgc90.so liblmgc90.so
```

**Step 2: Build wrapper**
```bash
cd /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Sandbox/compas_fortran_c
make all
```

**Step 3: Run**
```bash
mkdir -p OUTBOX POSTPRO
make run
```

## Dependencies
```bash
sudo apt install gfortran gcc g++ cmake libopenblas-dev liblapack-dev python3-dev python3-numpy swig
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
