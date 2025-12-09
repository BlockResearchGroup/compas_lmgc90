cmake_minimum_required(VERSION 3.15)
project(compas_lmgc90 LANGUAGES CXX Fortran C)

set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)

message(STATUS "===============================================")
message(STATUS "   COMPAS LMGC90 Unified Build System")
message(STATUS "===============================================")

# ===================================================================
# STEP 1: Build LMGC90 Core Library
# ===================================================================

# Determine compilers (always use GCC - no oneAPI environment needed!)
set(USING_INTEL_ONEAPI FALSE)
set(CMAKE_C_COMPILER_ARG "gcc")
set(CMAKE_CXX_COMPILER_ARG "g++")
set(BLAS_VENDOR_ARG "")
message(STATUS "Using GCC compilers (no oneAPI environment required)")
FetchContent_Declare(lmgc90 GIT_REPOSITORY https://compas:glpat-to65i8oMIi3pD9miaj0nBW86MQp1OjduCA.01.0y1p3o8t8@git-xen.lmgc.univ-montp2.fr/lmgc90/lmgc90_dev.git
                            GIT_TAG cbe1fd1ffe1ea32b3361c44e0cd13f7abce10e7e
)

set( LMGC90_BUILD_ChiPy    OFF    CACHE INTERNAL "" FORCE)
set( LMGC90_BUILD_PRE      OFF    CACHE INTERNAL "" FORCE)
set( LMGC90_BUILD_POST     OFF    CACHE INTERNAL "" FORCE)
set( LMGC90_ENABLE_TESTING OFF    CACHE INTERNAL "" FORCE)
set( LMGC90_ENABLE_DOC     OFF    CACHE INTERNAL "" FORCE)
set( LMGC90_WITH_HDF5      OFF    CACHE INTERNAL "" FORCE)
set( LMGC90_MATLIB_VERSION "none" CACHE INTERNAL "" FORCE)
set( LMGC90_SPARSE_LIBRARY "none" CACHE INTERNAL "" FORCE)
set( LMGC90_BUILD_Fortran_LIB ON  CACHE INTERNAL "" FORCE)
set( LMGC90_BUILD_SHARED_LIBS ON  CACHE INTERNAL "" FORCE)

FetchContent_MakeAvailable(lmgc90)

# ===================================================================
# STEP 2: Build Python Wrapper
# ===================================================================

# Find Python and nanobind
find_package(Python 3.8 REQUIRED COMPONENTS Interpreter Development.Module Development.SABIModule)
find_package(nanobind CONFIG REQUIRED)

# Compile the Fortran wrapper as a shared library
add_library(wrap_lmgc90_compas SHARED
    src/wrap_lmgc90_compas.f90
)

# Ensure LMGC90 core is built first
target_link_libraries(wrap_lmgc90_compas PRIVATE lmgc90_F_lib)


# Build nanobind module
nanobind_add_module(_lmgc90 STABLE_ABI NB_STATIC src/lmgc90.cpp)

# Ensure LMGC90 core is built before Python modules
target_link_libraries(_lmgc90 PRIVATE wrap_lmgc90_compas)


## Find BLAS/LAPACK (defaults to system OpenBLAS/LAPACK when available)
#find_package(BLAS REQUIRED)
#find_package(LAPACK REQUIRED)

message(STATUS "Using BLAS libraries: ${BLAS_LIBRARIES}")
message(STATUS "Using LAPACK libraries: ${LAPACK_LIBRARIES}")


install(TARGETS _lmgc90 LIBRARY DESTINATION compas_lmgc90)
install(TARGETS wrap_lmgc90_compas LIBRARY DESTINATION compas_lmgc90)
install(TARGETS lmgc90_F_lib LIBRARY DESTINATION compas_lmgc90)

set_target_properties(_lmgc90 PROPERTIES INSTALL_RPATH "$ORIGIN")
set_target_properties(wrap_lmgc90_compas PROPERTIES INSTALL_RPATH "$ORIGIN")

message(STATUS "===============================================")
message(STATUS "Build Configuration:")
message(STATUS "  Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ Standard: C++${CMAKE_CXX_STANDARD}")
message(STATUS "  Compiler: ${CMAKE_CXX_COMPILER_ARG}")
message(STATUS "  BLAS Libraries: ${BLAS_LIBRARIES}")
message(STATUS "  LAPACK Libraries: ${LAPACK_LIBRARIES}")
message(STATUS "===============================================")