# Build Standalone C Example (comgc90)

This builds `comgc90.c` as a standalone executable (no Python/nanobind).

## Prerequisites

1. Main project must be built first (to generate LMGC90 core library):
   ```bash
   cd /home/pv/brg/code_fortran/compas_lmgc90
   conda activate lmgc90
   source /opt/intel/oneapi/setvars.sh --force
   pip install -e .
   ```

2. This creates `build/lmgc90_core/liblmgc90_core.a` and module files

## Build and Run

```bash
cd /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Sandbox/Compas
source /opt/intel/oneapi/setvars.sh --force
./build_and_run.sh
```

Or manually:

```bash
rm -rf build_c
mkdir build_c
cd build_c
cmake ..
make
./comgc90
```

## What it does

- Compiles `wrap_lmgc90_compas.f90` (Fortran wrapper)
- Links `comgc90.c` with the wrapper and LMGC90 core library
- Creates standalone executable `build_c/comgc90`
- Runs the simulation and prints body positions and interactions

## Output

The C program prints:
- Number of interactions found at each step
- Interaction details (geometry codes, body IDs, status)
- Body positions at each step

Compare this with the Python binding output to debug transformations!
