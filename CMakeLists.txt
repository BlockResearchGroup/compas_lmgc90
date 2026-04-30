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

# MinGW runtime linkage strategy on Windows.
#
# We want _lmgc90.pyd to ship with as few sibling DLLs as possible.
# Statically embedding libgcc/libstdc++ is easy (driver flags). For
# libgfortran/libquadmath/libwinpthread, CMake auto-injects bare names
# via CMAKE_<LANG>_IMPLICIT_LINK_LIBRARIES, and ld resolves `-l<name>`
# to the .dll.a import lib whenever one exists — so the DLL ends up in
# the import table even when we wanted static. To force static linkage
# we have to (a) remove the bare name from the implicit lists so ld
# never goes looking, and (b) feed the static archive in by absolute
# path on the _lmgc90 target.
#
# That strategy only works if the static archive actually exists in
# the WinLibs distribution. WinLibs r7 ships libwinpthread as DLL-only
# (no libwinpthread.a), so for runtimes without a static archive we
# leave the dynamic linkage in place and bundle the runtime DLL
# alongside _lmgc90.pyd. Windows resolves direct-dep DLLs of a .pyd
# via LOAD_WITH_ALTERED_SEARCH_PATH (i.e. from the .pyd's directory),
# so bundling is sufficient — no os.add_dll_directory required.
set(_compas_lmgc90_static_runtimes "")
set(_compas_lmgc90_dynamic_runtimes "")
if(WIN32 AND CMAKE_Fortran_COMPILER_ID STREQUAL "GNU"
   AND DEFINED COMPAS_LMGC90_WINLIBS_BIN)
    set(_winlibs_lib "${COMPAS_LMGC90_WINLIBS_BIN}/../lib")
    foreach(_name gfortran quadmath winpthread)
        set(_a "${_winlibs_lib}/lib${_name}.a")
        if(EXISTS "${_a}")
            list(APPEND _compas_lmgc90_static_runtimes "${_a}")
            list(REMOVE_ITEM CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES ${_name})
            list(REMOVE_ITEM CMAKE_C_IMPLICIT_LINK_LIBRARIES ${_name})
            list(REMOVE_ITEM CMAKE_CXX_IMPLICIT_LINK_LIBRARIES ${_name})
            message(STATUS "compas_lmgc90: static-linking lib${_name} from ${_a}")
        else()
            file(GLOB _dlls "${COMPAS_LMGC90_WINLIBS_BIN}/lib${_name}-*.dll")
            if(_dlls)
                list(GET _dlls 0 _dll)
                list(APPEND _compas_lmgc90_dynamic_runtimes "${_dll}")
                message(STATUS "compas_lmgc90: bundling lib${_name} runtime DLL ${_dll}")
            else()
                message(WARNING "compas_lmgc90: no static archive or runtime DLL "
                                "found for lib${_name} in ${_winlibs_lib} / "
                                "${COMPAS_LMGC90_WINLIBS_BIN}")
            endif()
        endif()
    endforeach()
endif()

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
# LMGC90 source URL is read from the LMGC90_GIT_URL env var (or
# -DLMGC90_GIT_URL=... on the command line). The repo is currently a
# private GitLab; CI passes the URL with credentials via a GitHub Actions
# secret. Local builders need to provide the URL the same way:
#
#   export LMGC90_GIT_URL="https://USER:TOKEN@git-xen.lmgc.univ-montp2.fr/lmgc90/lmgc90_dev.git"
if(NOT DEFINED LMGC90_GIT_URL OR LMGC90_GIT_URL STREQUAL "")
    if(DEFINED ENV{LMGC90_GIT_URL} AND NOT "$ENV{LMGC90_GIT_URL}" STREQUAL "")
        set(LMGC90_GIT_URL "$ENV{LMGC90_GIT_URL}")
    endif()
endif()
# Defensive cleanup: GitHub secret-form pastes routinely capture a
# trailing newline, which makes git refuse the URL with
# "credential url cannot be parsed". Strip leading/trailing whitespace
# (newlines, CR, tabs, spaces).
if(LMGC90_GIT_URL)
    string(STRIP "${LMGC90_GIT_URL}" LMGC90_GIT_URL)
    string(REGEX REPLACE "[ \t\r\n]+$" "" LMGC90_GIT_URL "${LMGC90_GIT_URL}")
endif()
if(NOT LMGC90_GIT_URL)
    message(FATAL_ERROR
        "LMGC90_GIT_URL is not set. Pass -DLMGC90_GIT_URL=<url> or set "
        "the LMGC90_GIT_URL environment variable. The URL must include "
        "credentials if the LMGC90 repo is private.")
endif()

FetchContent_Declare(lmgc90
    GIT_REPOSITORY "${LMGC90_GIT_URL}"
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


# On Windows we need cp39 wheels (Rhino 8 ships Python 3.9.10/3.9.12), and
# nanobind's STABLE_ABI requires Python ≥ 3.12. Drop it so cibuildwheel can
# emit per-Python wheels (cp39, cp310, cp311, cp312, cp313).
nanobind_add_module(_lmgc90 NB_STATIC src/lmgc90.cpp)
target_link_libraries(_lmgc90 PRIVATE wrap_lmgc90_compas)

# Apply the runtime linkage strategy decided at the top of the file.
# libgcc/libstdc++ always go static (driver flags handle this cleanly).
# libgfortran/libquadmath/libwinpthread go static when a .a archive is
# available in WinLibs, and ride along as bundled sibling DLLs otherwise.
if(WIN32 AND CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_link_options(_lmgc90 PRIVATE
        -static-libgcc
        -static-libstdc++
    )
    if(_compas_lmgc90_static_runtimes)
        target_link_libraries(_lmgc90 PRIVATE ${_compas_lmgc90_static_runtimes})
    endif()
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

# Bundle any MinGW runtime DLLs we couldn't statically embed (e.g.
# libwinpthread-1.dll on WinLibs r7, which ships winpthread DLL-only).
# These are direct deps of _lmgc90.pyd; Windows finds them in the .pyd's
# directory via LOAD_WITH_ALTERED_SEARCH_PATH.
if(WIN32 AND _compas_lmgc90_dynamic_runtimes)
    install(FILES ${_compas_lmgc90_dynamic_runtimes}
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
