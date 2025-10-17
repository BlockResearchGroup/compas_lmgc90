import os
import sys
from compas_lmgc90 import _lmgc90_iterative

# Change to the correct working directory first
os.chdir('/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Sandbox/Compas')

_lmgc90_iterative.initialize_simulation()

print("Testing if simulation state changes between steps...")
print("=" * 50)

# Get initial state
result0 = _lmgc90_iterative.compute_one_step()
print(f"Step 0 - Body 1 position: {result0.bodies[0]}")
print(f"Step 0 - Body 10 position: {result0.bodies[9]}")

# Advance a few steps
for i in range(1, 11):
    result = _lmgc90_iterative.compute_one_step()
    if i % 5 == 0:
        print(f"Step {i} - Body 1 position: {result.bodies[0]}")
        print(f"Step {i} - Body 10 position: {result.bodies[9]}")

# Check if positions changed
result10 = _lmgc90_iterative.compute_one_step()
print("=" * 50)
print("COMPARISON:")
print(f"Body 1 - Step 0: {result0.bodies[0]}")
print(f"Body 1 - Step 11: {result10.bodies[0]}")
print(f"Body 1 moved: {result0.bodies[0] != result10.bodies[0]}")
print()
print(f"Body 10 - Step 0: {result0.bodies[9]}")
print(f"Body 10 - Step 11: {result10.bodies[9]}")
print(f"Body 10 moved: {result0.bodies[9] != result10.bodies[9]}")

_lmgc90_iterative.finalize_simulation()
