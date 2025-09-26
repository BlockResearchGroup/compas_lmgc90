#!/usr/bin/env python3
"""
Minimal LMGC90 example - just call the methods
"""

import os
import compas_lmgc90._lmgc90_compas as lmgc

# Change to directory with DATBOX (required for LMGC90 to find input files)
os.chdir("/home/pv/brg/code_fortran/compas_lmgc90/src/compas_lmgc90/lmgc90")

print("🚀 Running minimal LMGC90 example...")

# Set number of time steps
lmgc.set_nb_steps(5)
print("✅ Set 5 time steps")

# Initialize simulation
lmgc.initialize()
print("✅ Initialized")

# Run computation
lmgc.compute()
print("✅ Computation complete")

# Get interactions
nb_inters = lmgc.get_nb_inters()
print(f"✅ Found {nb_inters} interactions")

if nb_inters > 0:
    inters = lmgc.get_all_inters()
    first = inters[0]
    print(f"✅ First interaction: {first['cdan']} bodies {first['icdbdy']}/{first['ianbdy']} shapes {first['cdtac']}/{first['antac']}")

# Clean up
lmgc.finalize()
print("✅ Finalized")

print("🎉 Done!")
