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

# Determine compilers based on environment
if(DEFINED ENV{MKLROOT})
    set(USING_INTEL_ONEAPI TRUE)
    set(CMAKE_C_COMPILER_ARG "icx")
    set(CMAKE_CXX_COMPILER_ARG "icpx")
    set(BLAS_VENDOR_ARG "-DBLA_VENDOR=Intel10_64lp")
    message(STATUS "Using Intel oneAPI compilers")
else()
    set(USING_INTEL_ONEAPI FALSE)
    set(CMAKE_C_COMPILER_ARG "gcc")
    set(CMAKE_CXX_COMPILER_ARG "g++")
    set(BLAS_VENDOR_ARG "")
    message(STATUS "Using GCC compilers")
endif()

ExternalProject_Add(lmgc90_core
    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src/lmgc90_dev
    BINARY_DIR ${LMGC_BUILD_DIR}
    CMAKE_ARGS
        -DCMAKE_Fortran_COMPILER=gfortran
        -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER_ARG}
        -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER_ARG}
        -DCMAKE_BUILD_TYPE=Release
        -DBUILD_ChiPy=OFF
        -DBUILD_PRE=OFF
        -DBUILD_POST=OFF
        -DNO_DOXYGEN=ON
        -DMATLIB_VERSION=none
        -DSPARSE_LIBRARY=none
        -DWITH_HDF5=OFF
        -DBUILD_STANDALONE=OFF
        -DBUILD_STANDALONE_MPI=OFF
        ${BLAS_VENDOR_ARG}
    BUILD_COMMAND ${CMAKE_COMMAND} --build . -j
    INSTALL_COMMAND ""
    BUILD_ALWAYS OFF
)

# Post-build: consolidate libraries
ExternalProject_Add_Step(lmgc90_core consolidate_libs
    COMMAND ${CMAKE_COMMAND} -E make_directory ${LMGC_LIB_PATH}
    COMMAND ${CMAKE_COMMAND} -E env bash -c "find ${LMGC_BUILD_DIR} -name '*.a' | xargs -I{} cp {} ${LMGC_LIB_PATH}/"
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
    src/lmgc90_dev/src/Sandbox/Compas/wrap_lmgc90_compas.f90
)

# Ensure LMGC90 core is built first
add_dependencies(wrap_lmgc90_compas lmgc90_core)

target_include_directories(wrap_lmgc90_compas PRIVATE ${LMGC_INCLUDE})

# Build nanobind module
nanobind_add_module(_lmgc90 STABLE_ABI NB_STATIC src/lmgc90.cpp)

# Ensure LMGC90 core is built before Python modules
add_dependencies(_lmgc90 lmgc90_core)

target_include_directories(_lmgc90 PRIVATE 
    ${CMAKE_CURRENT_SOURCE_DIR}/src/lmgc90_dev/src/Sandbox/Compas
)

# Find MKL or BLAS and Intel libraries
if(DEFINED ENV{MKLROOT})
    set(MKL_ROOT $ENV{MKLROOT})
    set(BLAS_LIBS -L${MKL_ROOT}/lib/intel64 -lmkl_intel_lp64 -lmkl_sequential -lmkl_core)
    message(STATUS "Using Intel MKL: ${MKL_ROOT}")
    
    # Add Intel compiler runtime libraries
    if(DEFINED ENV{CMPLR_ROOT})
        set(INTEL_LIBS -L$ENV{CMPLR_ROOT}/lib -lintlc -lsvml -lirng -limf)
    elseif(EXISTS "/opt/intel/oneapi/compiler/latest/lib")
        set(INTEL_LIBS -L/opt/intel/oneapi/compiler/latest/lib -lintlc -lsvml -lirng -limf)
    endif()
else()
    set(BLAS_LIBS -lopenblas)
    message(STATUS "Using OpenBLAS")
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
    -lClipper2Zutils -lann_euclid -lann_manhattan
    -Wl,--end-group
    ${BLAS_LIBS}
    ${INTEL_LIBS}
    gfortran
    -lpthread -lm -ldl
)

install(TARGETS _lmgc90 LIBRARY DESTINATION compas_lmgc90)

message(STATUS "===============================================")
message(STATUS "Build Configuration:")
message(STATUS "  Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ Standard: C++${CMAKE_CXX_STANDARD}")
message(STATUS "  Compiler: ${CMAKE_CXX_COMPILER_ARG}")
if(USING_INTEL_ONEAPI)
    message(STATUS "  BLAS: Intel MKL")
else()
    message(STATUS "  BLAS: OpenBLAS")
endif()
message(STATUS "===============================================")