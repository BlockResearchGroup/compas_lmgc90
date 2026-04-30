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
# Windows: link the toolchain runtimes (libgcc_s, libstdc++, libgfortran,
# libquadmath, libwinpthread) DYNAMICALLY and let delvewheel vendor them
# into the wheel during repair.
#
# We tried static linking via -static-libgcc/-static-libstdc++/
# -static-libgfortran, but on Windows MinGW that interacts badly with
# auto-export: each contrib DLL statically embeds libgcc_eh.a AND
# re-exports its symbols (_Unwind_Resume, …) via the auto-generated
# import library. The next link step that pulls in two such DLLs sees
# duplicate definitions ("multiple definition of `_Unwind_Resume'").
#
# Dynamic linking sidesteps the whole issue: each DLL just imports the
# shared toolchain runtime, no symbols leak, and delvewheel bundles the
# DLLs into the wheel so end users still get a self-contained install.
# ===================================================================

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
set( LMGC90_BUILD_Fortran_LIB ON  CACHE INTERNAL "" FORCE)
# When ChiPy/PRE are off, LMGC90's own install rules use GNUInstallDirs and
# would scatter bin/, lib/, include/, modules/ at the wheel root. We only
# want our explicit install(TARGETS ...) → compas_lmgc90/ rules, so disable
# the upstream install entirely.
set( LMGC90_INSTALL        OFF    CACHE INTERNAL "" FORCE)
# Build LMGC90 + every contrib as STATIC archives. They all get pulled into
# _lmgc90.pyd at link time so the wheel ships exactly one Python extension
# binary plus libopenblas.dll — no transitive DLL chains, no
# os.add_dll_directory, no delvewheel.
set( LMGC90_BUILD_SHARED_LIBS OFF CACHE INTERNAL "" FORCE)
# Static archives that get linked into a SHARED target (_lmgc90.pyd) need
# position-independent code on Linux/macOS. Windows DLLs are PIC by default
# but setting it doesn't hurt.
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

FetchContent_MakeAvailable(lmgc90)

# ===================================================================
# STEP 2: Build Python Wrapper
# ===================================================================

# Find Python and nanobind
find_package(Python 3.8 REQUIRED COMPONENTS Interpreter Development.Module Development.SABIModule)
find_package(nanobind CONFIG REQUIRED)

# Compile the Fortran wrapper as a static archive that gets pulled into
# _lmgc90.pyd along with the rest of LMGC90.
add_library(wrap_lmgc90_compas STATIC
    src/wrap_lmgc90_compas.f90
)
set_target_properties(wrap_lmgc90_compas PROPERTIES POSITION_INDEPENDENT_CODE ON)

# Pull in lmgc90_F_lib (and its transitive contribs) at link time.
target_link_libraries(wrap_lmgc90_compas PRIVATE lmgc90_F_lib)


# Build nanobind module — single .pyd containing all C++ + Fortran code.
nanobind_add_module(_lmgc90 STABLE_ABI NB_STATIC src/lmgc90.cpp)
target_link_libraries(_lmgc90 PRIVATE wrap_lmgc90_compas)

# On Windows, _lmgc90.pyd is the FINAL link. Statically embed the MinGW
# toolchain runtimes here (libgcc, libstdc++, libgfortran, libquadmath,
# libwinpthread) so the wheel has zero MinGW DLL dependencies. With every
# contrib being a static archive, this happens exactly once — no
# duplicate-symbol issue like the earlier all-SHARED build had.
if(WIN32 AND CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_link_options(_lmgc90 PRIVATE
        -static-libgcc
        -static-libstdc++
        "LINKER:-Bstatic"
        -lgfortran
        -lquadmath
        -lwinpthread
        "LINKER:-Bdynamic"
    )
endif()


## Find BLAS/LAPACK (defaults to system OpenBLAS/LAPACK when available)
#find_package(BLAS REQUIRED)
#find_package(LAPACK REQUIRED)

message(STATUS "Using BLAS libraries: ${BLAS_LIBRARIES}")
message(STATUS "Using LAPACK libraries: ${LAPACK_LIBRARIES}")


# ===================================================================
# Install: only _lmgc90.pyd ships as a Python extension. Everything else
# (wrap_lmgc90_compas, lmgc90_F_lib, all contribs, MinGW runtimes) is
# statically linked into it. On Windows, libopenblas.dll is installed
# alongside as the only sibling DLL — found via Python's default altered
# search path for .pyd direct imports.
# ===================================================================
install(TARGETS _lmgc90
        LIBRARY DESTINATION compas_lmgc90
        RUNTIME DESTINATION compas_lmgc90
        ARCHIVE DESTINATION compas_lmgc90
)

if(NOT WIN32)
    set_target_properties(_lmgc90 PROPERTIES INSTALL_RPATH "${_RPATH}")
endif()

if(WIN32 AND COMPAS_LMGC90_OPENBLAS_DLL AND EXISTS "${COMPAS_LMGC90_OPENBLAS_DLL}")
    install(FILES "${COMPAS_LMGC90_OPENBLAS_DLL}"
            DESTINATION compas_lmgc90)
endif()

message(STATUS "===============================================")
message(STATUS "Build Configuration:")
message(STATUS "  Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ Standard: C++${CMAKE_CXX_STANDARD}")
message(STATUS "  Compiler: ${CMAKE_CXX_COMPILER_ARG}")
message(STATUS "  BLAS Libraries: ${BLAS_LIBRARIES}")
message(STATUS "  LAPACK Libraries: ${LAPACK_LIBRARIES}")
message(STATUS "===============================================")
