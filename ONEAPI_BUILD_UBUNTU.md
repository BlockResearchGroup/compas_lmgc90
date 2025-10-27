# LMGC90 Build Guide

Unified build system that compiles LMGC90 core + Python bindings in one step.

## Prerequisites

- Ubuntu 20.04+
- Anaconda or Miniconda
- Git

## Step 1: Install Minimal System Dependencies

**Required system packages (minimal set):**
```bash
sudo apt update
sudo apt install -y gfortran libopenblas-dev liblapack-dev cmake
```

**Install Intel oneAPI for optimized builds:**
```bash
# Add Intel repository
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | \
  gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null
echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" | \
  sudo tee /etc/apt/sources.list.d/oneapi.list

# Install Intel compilers and MKL
sudo apt update
sudo apt install -y intel-oneapi-compiler-fortran intel-oneapi-compiler-dpcpp-cpp-and-cpp-classic \
  intel-oneapi-mkl-devel
```

## Step 2: Create Dedicated Conda Environment

**Create a clean environment (avoids dependency conflicts):**
```bash
conda create -n lmgc90 python=3.12 -y
conda activate lmgc90
```

## Step 3: Install Python Dependencies

```bash
# In the lmgc90 environment
pip install numpy compas compas_viewer nanobind
```

## Step 4: Clone Repository

```bash
cd ~
git clone https://github.com/YOUR_USERNAME/compas_lmgc90.git
cd compas_lmgc90
```

## Step 5: Build and Install

**Activate environment and oneAPI, then build:**
```bash
# Activate conda environment
conda activate lmgc90

# Source Intel oneAPI (for optimized build)
source /opt/intel/oneapi/setvars.sh --force

# Build and install
pip install -e .
```

**What happens during build:**
1. LMGC90 core library compiled (Fortran/C++)
2. Static libraries consolidated
3. Fortran wrapper compiled
4. Python nanobind module `_lmgc90` built

All automatically in the correct order via unified CMakeLists.txt.

## Step 6: Verify Installation

```bash
python -c "from compas_lmgc90 import _lmgc90; print('✓ Module loaded successfully!')"
```


## Rebuild After Code Changes

```bash
cd /home/pv/brg/code_fortran/compas_lmgc90
conda activate lmgc90
source /opt/intel/oneapi/setvars.sh --force
rm -rf build  # Clean build directory
pip install -e . --force-reinstall
```

## Run Examples

```bash
# Simple simulation
python temp/example_new_api.py

# With visualization
python temp/example_new_api_visualize.py
```

## Python API

```python
from compas_lmgc90 import _lmgc90 as lmgc90

# Initialize
lmgc90.initialize_simulation(dt=1e-3, theta=0.5)
lmgc90.set_materials(1)
lmgc90.set_tact_behavs(1)
lmgc90.set_see_tables()
lmgc90.set_nb_bodies(2)

# Add geometry (1-indexed faces!)
lmgc90.set_one_polyr(
    coor=[x, y, z],           # Center of mass
    faces=[1,2,3, 1,2,4, ...], # Triangle faces (1-indexed!)
    vertices=[x1,y1,z1, ...],  # Flat vertex list
    fixed=False                # True for fixed bodies
)

# Finalize setup
lmgc90.close_before_computing()

# Run simulation
for step in range(n_steps):
    result = lmgc90.compute_one_step()
    
    # Access results:
    # - result.bodies: [[x,y,z], ...] positions
    # - result.body_frames: [R00,R10,R20,R01,...] rotation matrices (9 elements each)
    # - result.interaction_coords: [[x,y,z], ...] contact points
    # - result.interaction_rloc: [[T,N,S], ...] contact forces
    # - result.interaction_uc: [T0,T1,T2,N0,N1,N2,S0,S1,S2, ...] contact frames
    # - result.interaction_bodies: [[icd,ian], ...] body indices
    # - result.interaction_gap: [gap, ...] gap values
    # - result.interaction_status: ['stic', ...] contact status

# Cleanup
lmgc90.finalize_simulation()
```

**Available functions:**
- `initialize_simulation(dt, theta)` - Initialize with timestep and theta
- `set_materials(nb)` - Set number of materials
- `set_tact_behavs(nb)` - Set number of contact behaviors  
- `set_see_tables()` - Initialize SEE tables
- `set_nb_bodies(nb)` - Set number of bodies
- `set_one_polyr(coor, faces, vertices, fixed)` - Add polyhedron
- `close_before_computing()` - Finalize initialization
- `compute_one_step()` - Run one timestep → returns `SimResult`
- `finalize_simulation()` - Cleanup
- `run_simulation(dt, theta)` - Legacy one-shot run

## Important Notes

### Isolated Environment
- Uses dedicated conda environment `lmgc90`
- No interference with system Python or other projects
- Clean dependency management

### System Dependencies
- **gfortran**: Fortran compiler for LMGC90 core
- **libopenblas-dev**: Fallback BLAS/LAPACK (Intel MKL preferred)
- **cmake**: Build system

### Intel oneAPI Benefits
- Optimized C/C++ compilers (icx, icpx)
- Intel MKL for fast linear algebra
- ~20-30% performance improvement over GCC

## Troubleshooting

**Module not loading:**
```bash
# Check conda environment is active
conda activate lmgc90

# Verify module exists
python -c "import compas_lmgc90; print(compas_lmgc90._lmgc90)"

# If None, rebuild
cd ~/compas_lmgc90
rm -rf build
source /opt/intel/oneapi/setvars.sh --force
pip install -e . --force-reinstall
```

**Dependency conflicts:**
- Always use the dedicated `lmgc90` environment
- Don't install in base environment
- Recreate environment if needed: `conda env remove -n lmgc90`

**Build errors:**
- Ensure Intel oneAPI sourced: `source /opt/intel/oneapi/setvars.sh --force`
- Check system packages installed: `gfortran`, `libopenblas-dev`, `cmake`
- Clean build: `rm -rf build` before rebuilding