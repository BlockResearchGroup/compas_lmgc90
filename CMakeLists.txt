cmake_minimum_required(VERSION 3.15)
project(compas_lmgc90 LANGUAGES CXX Fortran C)

set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(ExternalProject)

message(STATUS "===============================================")
message(STATUS "   COMPAS LMGC90 Unified Build System")
message(STATUS "===============================================")

# ===================================================================
# STEP 1: Build LMGC90 Core Library
# ===================================================================

set(LMGC_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}/lmgc90_core")
set(LMGC_LIB_PATH "${LMGC_BUILD_DIR}/lib")
set(LMGC_INCLUDE "${LMGC_BUILD_DIR}/modules")

# Determine compilers (always use GCC - no oneAPI environment needed!)
set(USING_INTEL_ONEAPI FALSE)
set(CMAKE_C_COMPILER_ARG "gcc")
set(CMAKE_CXX_COMPILER_ARG "g++")
set(BLAS_VENDOR_ARG "")
message(STATUS "Using GCC compilers (no oneAPI environment required)")

ExternalProject_Add(lmgc90_core
    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/external/lmgc90_dev-fortran_lib
    BINARY_DIR ${LMGC_BUILD_DIR}
    CMAKE_ARGS
        -DCMAKE_Fortran_COMPILER=gfortran
        -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER_ARG}
        -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER_ARG}
        -DCMAKE_BUILD_TYPE=Release
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON
        -DCMAKE_CXX_FLAGS=-Wno-deprecated
        -DBUILD_ChiPy=OFF
        -DBUILD_PRE=OFF
        -DBUILD_POST=OFF
        -DNO_DOXYGEN=ON
        -DBUILD_MATLIB=ON
        -DSPARSE_LIBRARY=none
        -DWITH_HDF5=OFF
        -DBUILD_STANDALONE=OFF
        -DBUILD_STANDALONE_MPI=OFF
        -DCMAKE_SKIP_RPATH=OFF
    BUILD_COMMAND ${CMAKE_COMMAND} --build .
    INSTALL_COMMAND ""
    BUILD_ALWAYS OFF
)

# Post-build: consolidate libraries
ExternalProject_Add_Step(lmgc90_core consolidate_libs
    COMMAND ${CMAKE_COMMAND} -E make_directory ${LMGC_LIB_PATH}
    COMMAND bash -c "find ${LMGC_BUILD_DIR} -name '*.a' ! -path '${LMGC_LIB_PATH}/*' | xargs -I{} cp -n {} ${LMGC_LIB_PATH}/ || true"
    DEPENDEES build
    DEPENDERS install
    ALWAYS FALSE
)

message(STATUS "LMGC90 library path: ${LMGC_LIB_PATH}")
message(STATUS "LMGC90 include path: ${LMGC_INCLUDE}")

# ===================================================================
# STEP 2: Build Python Wrapper
# ===================================================================

# Find Python and nanobind
find_package(Python 3.8 REQUIRED COMPONENTS Interpreter Development.Module Development.SABIModule)
find_package(nanobind CONFIG REQUIRED)

# Compile the Fortran wrapper as a shared library
add_library(wrap_lmgc90_compas OBJECT 
    external/lmgc90_dev-fortran_lib/src/Sandbox/Compas/wrap_lmgc90_compas.f90
)

# Ensure LMGC90 core is built first
add_dependencies(wrap_lmgc90_compas lmgc90_core)

target_include_directories(wrap_lmgc90_compas PRIVATE ${LMGC_INCLUDE})

# Build nanobind module
nanobind_add_module(_lmgc90 STABLE_ABI NB_STATIC src/lmgc90.cpp)

# Ensure LMGC90 core is built before Python modules
add_dependencies(_lmgc90 lmgc90_core)

target_include_directories(_lmgc90 PRIVATE 
    ${CMAKE_CURRENT_SOURCE_DIR}/external/lmgc90_dev-fortran_lib/src/Sandbox/Compas
)

# Use bundled Intel MKL libraries (self-contained, no system dependencies)
set(INTEL_LIBS_DIR "${CMAKE_CURRENT_SOURCE_DIR}/external/ubuntu/intel_libs")

# Check that bundled libraries exist
if(NOT EXISTS "${INTEL_LIBS_DIR}/libmkl_core.so.2")
    message(FATAL_ERROR 
        "Bundled Intel MKL libraries not found in: ${INTEL_LIBS_DIR}\n"
        "Please ensure external/ubuntu/intel_libs/ contains all required .so files.\n"
        "This package requires bundled Intel MKL libraries for BLAS/LAPACK support.")
endif()

message(STATUS "Using bundled Intel MKL from: ${INTEL_LIBS_DIR}")

# Set library paths (BLAS functionality provided by Intel MKL)
set(BLAS_LIBS -L${INTEL_LIBS_DIR} -lmkl_intel_lp64 -lmkl_sequential -lmkl_core)
# Intel compiler runtime libraries (svml, irng, imf) are not needed when using GCC
# Only include intlc if available
if(EXISTS "${INTEL_LIBS_DIR}/libintlc.so" OR EXISTS "${INTEL_LIBS_DIR}/libintlc.so.5")
    set(INTEL_LIBS -L${INTEL_LIBS_DIR} -lintlc)
else()
    set(INTEL_LIBS "")
endif()

# Link against LMGC90 and the Fortran wrapper
target_link_libraries(_lmgc90 PRIVATE 
    wrap_lmgc90_compas
    -L${LMGC_LIB_PATH}
    -Wl,--start-group
    -llmgc_core_shared -llmgc_core_rigid_3d -llmgc_core_contact_3d 
    -llmgc_core_contactor_3d -llmgc_core_kernel_3d -llmgc_core_post_3d 
    -llmgc_core_utils -llmgc_core_mailx -llmgc_core_shared_contribs 
    -llmgc_core_other_contribs -llmgc_core_utils_contribs -llmgc_ann 
    -llmgc_clipper -lpredicates -llibrtree -lminpack -ldemmefi 
    -llmgc_exception -llmgc_core_mbs3d -llmgc_core_mbs2d 
    -llmgc_bindings_FEM -llmgc_bindings_user -llmgc_bindings_sparse_la 
    -llmgc_bindings_MBS -lClipper2 -lClipper2Z -lClipper2utils 
    -lClipper2Zutils -lann_euclid -lann_manhattan -lmatlib
    -Wl,--end-group
    ${BLAS_LIBS}
    ${INTEL_LIBS}
    gfortran
    -lpthread -lm -ldl
)

# Set RPATH so the module can find bundled Intel libraries and LMGC90 libraries at runtime
set_target_properties(_lmgc90 PROPERTIES
    BUILD_RPATH "${INTEL_LIBS_DIR}:${LMGC_LIB_PATH}"
    INSTALL_RPATH "$ORIGIN/../external/ubuntu/intel_libs:$ORIGIN/../external/lmgc90_libs"
    INSTALL_RPATH_USE_LINK_PATH FALSE
)

install(TARGETS _lmgc90 LIBRARY DESTINATION compas_lmgc90)

# Install bundled Intel libraries with the package
file(GLOB INTEL_SO_FILES "${INTEL_LIBS_DIR}/*.so.[0-9]" "${INTEL_LIBS_DIR}/*.so.[0-9][0-9]" "${INTEL_LIBS_DIR}/*.so")
list(LENGTH INTEL_SO_FILES INTEL_SO_COUNT)
message(STATUS "Found ${INTEL_SO_COUNT} Intel library files to install")

if(INTEL_SO_FILES)
    install(FILES ${INTEL_SO_FILES}
            DESTINATION external/ubuntu/intel_libs
            PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE 
                       GROUP_READ GROUP_EXECUTE 
                       WORLD_READ WORLD_EXECUTE)
else()
    message(WARNING "No Intel library files found in ${INTEL_LIBS_DIR}")
endif()

# Install LMGC90 shared libraries with the package
# Use install(CODE) because the libraries don't exist at configure time
install(CODE "
    file(GLOB LMGC_SO_FILES \"${LMGC_LIB_PATH}/*.so\" \"${LMGC_LIB_PATH}/*.so.*\")
    if(LMGC_SO_FILES)
        file(INSTALL \${LMGC_SO_FILES}
             DESTINATION \"\${CMAKE_INSTALL_PREFIX}/external/lmgc90_libs\"
             FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE 
                             GROUP_READ GROUP_EXECUTE 
                             WORLD_READ WORLD_EXECUTE)
        message(STATUS \"Installed LMGC90 shared libraries\")
    else()
        message(WARNING \"No LMGC90 shared libraries found in ${LMGC_LIB_PATH}\")
    endif()
")

message(STATUS "===============================================")
message(STATUS "Build Configuration:")
message(STATUS "  Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ Standard: C++${CMAKE_CXX_STANDARD}")
message(STATUS "  Compiler: ${CMAKE_CXX_COMPILER_ARG}")
message(STATUS "  BLAS: Intel MKL (bundled)")
message(STATUS "  Libs: ${INTEL_LIBS_DIR}")
message(STATUS "===============================================")