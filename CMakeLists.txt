cmake_minimum_required(VERSION 3.24)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# ===================================================================
# macOS: Auto-detect Homebrew GCC/gfortran before project()
# ===================================================================
if(APPLE AND NOT DEFINED CMAKE_C_COMPILER)
    # Find Homebrew prefix
    execute_process(
        COMMAND brew --prefix gcc
        OUTPUT_VARIABLE HOMEBREW_GCC_PREFIX
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    if(HOMEBREW_GCC_PREFIX)
        # Find the latest gfortran version
        file(GLOB GFORTRAN_VERSIONS "${HOMEBREW_GCC_PREFIX}/bin/gfortran-*")
        if(GFORTRAN_VERSIONS)
            list(SORT GFORTRAN_VERSIONS)
            list(GET GFORTRAN_VERSIONS -1 GFORTRAN_PATH)
            get_filename_component(GFORTRAN_NAME ${GFORTRAN_PATH} NAME)
            string(REGEX REPLACE "gfortran-" "" GCC_VERSION "${GFORTRAN_NAME}")

            set(CMAKE_C_COMPILER "${HOMEBREW_GCC_PREFIX}/bin/gcc-${GCC_VERSION}" CACHE STRING "")
            set(CMAKE_CXX_COMPILER "${HOMEBREW_GCC_PREFIX}/bin/g++-${GCC_VERSION}" CACHE STRING "")
            set(CMAKE_Fortran_COMPILER "${HOMEBREW_GCC_PREFIX}/bin/gfortran-${GCC_VERSION}" CACHE STRING "")
            message(STATUS "macOS: Auto-detected Homebrew GCC ${GCC_VERSION}")
        endif()
    endif()
endif()

# ===================================================================
# Windows: Bootstrap MinGW-w64 + OpenBLAS before project()
# ===================================================================
include(BootstrapWindowsToolchain)

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

# Determine compilers
set(USING_INTEL_ONEAPI FALSE)
set(BLAS_VENDOR_ARG "")
message(STATUS "Using GCC compilers (no oneAPI environment required)")

# RPATH handling: $ORIGIN for Linux, @loader_path for macOS
# must be set before adding LMGC90 to inherit the correct value
if(APPLE)
    set(CMAKE_MACOSX_RPATH ON)
    set(_RPATH "@loader_path")
elseif(WIN32)
    # Windows has no RPATH; DLLs are resolved from the loading module's
    # directory, so co-installing them in compas_lmgc90/ is sufficient.
    set(_RPATH "")
else()
    set(_RPATH "$ORIGIN")
endif()
# ===================================================================
# Windows: static-link gcc/gfortran/stdc++ runtimes everywhere.
# Must be set BEFORE FetchContent_MakeAvailable(lmgc90) so that the
# LMGC90 contribs (Fortran SHARED libs) inherit the link flags.
# ===================================================================
if(WIN32 AND CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    # Apply per link-language so we don't pass -static-libgfortran to g++.
    add_link_options(
        $<$<LINK_LANGUAGE:CXX>:-static-libgcc>
        $<$<LINK_LANGUAGE:CXX>:-static-libstdc++>
        $<$<LINK_LANGUAGE:C>:-static-libgcc>
        $<$<LINK_LANGUAGE:Fortran>:-static-libgcc>
        $<$<LINK_LANGUAGE:Fortran>:-static-libstdc++>
        $<$<LINK_LANGUAGE:Fortran>:-static-libgfortran>
    )
endif()

FetchContent_Declare(lmgc90 GIT_REPOSITORY https://compas:glpat-to65i8oMIi3pD9miaj0nBW86MQp1OjduCA.01.0y1p3o8t8@git-xen.lmgc.univ-montp2.fr/lmgc90/lmgc90_dev.git
                            GIT_TAG compas_dev
)
#FetchContent_Declare(lmgc90 SOURCE_DIR /home/mozul/WORK/LMGC90/sources/lmgc90_dev)

set( LMGC90_BUILD_ChiPy    OFF    CACHE INTERNAL "" FORCE)
set( LMGC90_BUILD_PRE      OFF    CACHE INTERNAL "" FORCE)
set( LMGC90_BUILD_POST     OFF    CACHE INTERNAL "" FORCE)
set( LMGC90_ENABLE_TESTING OFF    CACHE INTERNAL "" FORCE)
set( LMGC90_ENABLE_DOC     OFF    CACHE INTERNAL "" FORCE)
set( LMGC90_WITH_HDF5      OFF    CACHE INTERNAL "" FORCE)
set( LMGC90_MATLIB_VERSION "none" CACHE INTERNAL "" FORCE)
set( LMGC90_SPARSE_LIBRARY "none" CACHE INTERNAL "" FORCE)
set( LMGC90_BUILD_SHARED_LIBS ON  CACHE INTERNAL "" FORCE)
set( LMGC90_BUILD_Fortran_LIB ON  CACHE INTERNAL "" FORCE)

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


install(TARGETS _lmgc90
        LIBRARY DESTINATION compas_lmgc90
        RUNTIME DESTINATION compas_lmgc90
        ARCHIVE DESTINATION compas_lmgc90
)
install(TARGETS wrap_lmgc90_compas
        LIBRARY DESTINATION compas_lmgc90
        RUNTIME DESTINATION compas_lmgc90
        ARCHIVE DESTINATION compas_lmgc90
)

if(NOT WIN32)
    set_target_properties(_lmgc90 PROPERTIES INSTALL_RPATH "${_RPATH}")
    set_target_properties(wrap_lmgc90_compas PROPERTIES INSTALL_RPATH "${_RPATH}")
endif()

# install of needed target of LMGC90 and reset rpath
set(LMGC90_DEPS lmgc90_F_lib demmefi Clipper2 minpack ann_euclid ann_manhattan librtree predicates)
foreach( LMGC90_DEP ${LMGC90_DEPS} )
  install(TARGETS ${LMGC90_DEP}
          LIBRARY DESTINATION compas_lmgc90
          RUNTIME DESTINATION compas_lmgc90
          ARCHIVE DESTINATION compas_lmgc90
  )
  if(NOT WIN32)
    set_target_properties(${LMGC90_DEP} PROPERTIES INSTALL_RPATH "${_RPATH}")
  endif()
endforeach( LMGC90_DEP ${LMGC90_DEPS} )

# ===================================================================
# Windows: install runtime DLLs alongside the extension module
# ===================================================================
if(WIN32)
    # OpenBLAS DLL — picked up by delvewheel during repair.
    if(COMPAS_LMGC90_OPENBLAS_DLL AND EXISTS "${COMPAS_LMGC90_OPENBLAS_DLL}")
        install(FILES "${COMPAS_LMGC90_OPENBLAS_DLL}"
                DESTINATION compas_lmgc90)
    endif()

    # Make sure SHARED libs export their symbols. CMake auto-generates a .def
    # file from object files. gcc/gfortran already export all symbols by
    # default for SHARED targets on Windows, but this is harmless.
    set_target_properties(_lmgc90 wrap_lmgc90_compas PROPERTIES
        WINDOWS_EXPORT_ALL_SYMBOLS ON)
    foreach(LMGC90_DEP ${LMGC90_DEPS})
        if(TARGET ${LMGC90_DEP})
            set_target_properties(${LMGC90_DEP} PROPERTIES
                WINDOWS_EXPORT_ALL_SYMBOLS ON)
        endif()
    endforeach()
endif()

message(STATUS "===============================================")
message(STATUS "Build Configuration:")
message(STATUS "  Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ Standard: C++${CMAKE_CXX_STANDARD}")
message(STATUS "  Compiler: ${CMAKE_CXX_COMPILER_ARG}")
message(STATUS "  BLAS Libraries: ${BLAS_LIBRARIES}")
message(STATUS "  LAPACK Libraries: ${LAPACK_LIBRARIES}")
message(STATUS "===============================================")
