# LMGC90 Nanobind Wrapper Summary

## Overview
A concise nanobind wrapper (~95 lines) that exposes LMGC90 simulation data to Python.

## What It Returns

The `run_simulation()` function returns a `SimResult` object with:

### Body Data (90 bodies)
- **`bodies`**: List of [x, y, z] positions for each rigid body
- **`body_frames`**: List of [9 values] rotation matrices (3x3 flattened)

### Interaction Data (860 contacts) - **THE MOST IMPORTANT ARRAY**
- **`interaction_coords`**: Contact point coordinates [x, y, z]
- **`interaction_bodies`**: Body pair indices [icdbdy, ianbdy]
- **`interaction_rloc`**: Reaction forces [3 values]
- **`interaction_vloc`**: Velocities [3 values]
- **`interaction_gap`**: Penetration gap (scalar)
- **`interaction_status`**: Contact status string ('stic', 'slid', etc.)

## Usage

```python
from compas_lmgc90 import _lmgc90

# Run simulation
result = _lmgc90.run_simulation()

# Access body data
for i, pos in enumerate(result.bodies):
    frame = result.body_frames[i]  # 3x3 rotation matrix (9 values)
    print(f"Body {i}: position={pos}, frame={frame}")

# Access interaction data (the important array)
for i in range(len(result.interaction_coords)):
    coord = result.interaction_coords[i]
    bodies = result.interaction_bodies[i]  # [cdbdy, anbdy]
    forces = result.interaction_rloc[i]
    velocity = result.interaction_vloc[i]
    gap = result.interaction_gap[i]
    status = result.interaction_status[i]
    
    print(f"Interaction {i}:")
    print(f"  Contact at: {coord}")
    print(f"  Between bodies: {bodies[0]} and {bodies[1]}")
    print(f"  Forces: {forces}")
    print(f"  Status: {status}")
```

## Example Output

```
✓ Simulation completed!
  - Bodies: 90 vertices
  - Body frames: 90 rotation matrices
  - Interactions: 860 contacts

Interaction 0:
  Coord: [-4.387752, -1.500000, 1.122326]
  Bodies: [18, 19] (cdbdy=18, anbdy=19)
  Rloc (forces): [-7.163, 4856.949, -118.425]
  Vloc (velocity): [2.579e-07, 1.496e-07, -7.054e-08]
  Gap: -0.000000
  Status: 'stic'
```

## Building

The wrapper requires Intel oneAPI environment (for MKL and Intel runtime libraries):

```bash
source /opt/intel/oneapi/setvars.sh
pip install -e .
```

## Implementation Details

- **Language**: C++ with nanobind
- **Data transfer**: std::vector (automatically converted to Python lists)
- **Memory**: Properly managed with malloc/free for LMGC90 data
- **Source**: `/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90.cpp`
