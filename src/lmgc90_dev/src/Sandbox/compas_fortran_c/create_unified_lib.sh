#!/bin/bash
# Create unified LMGC90 library from existing static libraries (no Python/SWIG)

set -e  # Exit on error

echo "🚀 Creating unified LMGC90 library from static libraries..."

# Build LMGC90 without ChiPy first
LMGC90_ROOT="/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev"
cd $LMGC90_ROOT

# Clean and build without ChiPy
echo "🧹 Building LMGC90 static libraries (no Python)..."
rm -rf build_static
mkdir build_static && cd build_static

cmake .. \
  -DCMAKE_BUILD_TYPE=Release \
  -DBUILD_ChiPy=OFF \
  -DBUILD_PRE=OFF \
  -DBUILD_POST=OFF \
  -DNO_DOXYGEN=ON \
  -DMATLIB_VERSION=none \
  -DSPARSE_LIBRARY=none \
  -DWITH_HDF5=OFF \
  -DWITH_DOCSTRING=OFF

echo "🔨 Building static libraries..."
make -j$(nproc)

# Check what libraries were built
echo "📋 Available static libraries:"
find lib -name "*.a" | head -10

# Create unified shared library
echo "🔗 Creating unified shared library..."
cd lib

# Find all the core libraries we need
CORE_LIBS=$(find . -name "liblmgc_core_*.a" | tr '\n' ' ')
BINDINGS_LIBS=$(find . -name "liblmgc_bindings_*.a" | tr '\n' ' ')
EXTERNAL_LIBS=$(find . -name "liblmgc_ann.a" -o -name "libpredicates.a" -o -name "libminpack.a" -o -name "liblmgc_clipper.a" -o -name "libdemmefi.a" -o -name "liblmgc_exception.a" -o -name "libClipper2*.a" | tr '\n' ' ')

echo "Core libraries: $CORE_LIBS"
echo "Bindings libraries: $BINDINGS_LIBS"
echo "External libraries: $EXTERNAL_LIBS"

# Create the unified shared library
gcc -shared -fPIC -o liblmgc90.so \
  -Wl,--whole-archive \
  $CORE_LIBS \
  $BINDINGS_LIBS \
  $EXTERNAL_LIBS \
  -Wl,--no-whole-archive \
  -lopenblas -lgfortran -lm -lstdc++

# Verify the library was created
if [ -f "liblmgc90.so" ]; then
    echo "🎉 Success! Unified library created:"
    ls -lh liblmgc90.so
    
    # Copy to wrapper directory
    WRAPPER_DIR="$LMGC90_ROOT/src/Sandbox/compas_fortran_c"
    echo "📦 Copying to wrapper directory..."
    mkdir -p $WRAPPER_DIR/lib
    cp liblmgc90.so $WRAPPER_DIR/lib/
    
    # Also copy modules
    if [ -d "../modules" ]; then
        echo "📁 Copying Fortran modules..."
        mkdir -p $WRAPPER_DIR/modules
        cp -r ../modules/* $WRAPPER_DIR/modules/
    fi
    
    echo "📍 Library location: $WRAPPER_DIR/lib/liblmgc90.so"
    echo "📍 Modules location: $WRAPPER_DIR/modules/"
    
    # Test with the wrapper
    echo "🧪 Testing with wrapper..."
    cd $WRAPPER_DIR
    if [ -f "Makefile" ]; then
        # Update Makefile to use our library
        sed -i 's|LMGC_INCLUDE = .*|LMGC_INCLUDE = ./modules|' Makefile
        sed -i 's|LMGC_LIB_PATH = .*|LMGC_LIB_PATH = ./lib|' Makefile
        
        echo "🔨 Building wrapper with unified library..."
        make clean && make all
        
        if [ -f "comgc90" ]; then
            echo "✅ Wrapper built successfully!"
            echo "🎯 Ready to run: make run"
        else
            echo "⚠️  Wrapper build failed, but library was created"
        fi
    fi
    
else
    echo "❌ Error: Failed to create unified library"
    exit 1
fi

echo ""
echo "🎯 Summary:"
echo "  ✅ No Python dependencies used"
echo "  ✅ No SWIG dependencies used"
echo "  ✅ Unified liblmgc90.so created from static libraries"
echo "  ✅ Library ready for use in C++ projects"
