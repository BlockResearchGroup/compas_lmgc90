#!/bin/bash
set -e

echo "Building comgc90 C example..."

# Clean previous build
rm -rf build_c
mkdir -p build_c
cd build_c

# Configure and build
cmake ..
make

echo ""
echo "Build complete! Running comgc90..."
echo "=================================="
./comgc90
