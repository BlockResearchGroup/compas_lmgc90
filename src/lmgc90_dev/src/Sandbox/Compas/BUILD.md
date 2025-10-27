# Building LMGC90 Compas Example

Minimal C example demonstrating LMGC90 rigid 3D polyhedron contact mechanics.

## Prerequisites

```bash
conda install -c conda-forge cmake=3.18 gfortran clang clangxx
# Optional: Intel oneAPI for MKL
```

## Build

```bash
cd /path/to/compas_lmgc90
conda activate lmgc90
source /opt/intel/oneapi/setvars.sh --force  # if using Intel MKL

cmake -S src/lmgc90_dev -B build_dev \
  -DCMAKE_BUILD_TYPE=Debug \
  -DBUILD_STANDALONE=ON \
  -DWITH_HDF5=OFF

cmake --build build_dev --target comgc90 -j
./build_dev/bin/comgc90
```

## Output

Simulates two falling polyhedra with contact detection:
```
@ step = 1
bodies to get: 2
0 -> (0.325000, 0.325000, 1.240000)
1 -> (0.325000, 0.325000, 0.740000)
```

## Troubleshooting

- **MUMPS errors**: Ensure oneAPI is sourced before building
- **Missing libs**: Run `pip install -e . --force-reinstall` first
