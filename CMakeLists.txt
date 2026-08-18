cmake_minimum_required(VERSION 3.24)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# ===================================================================
# macOS: Auto-detect Homebrew GCC/gfortran + OpenBLAS before project()
# -------------------------------------------------------------------
# We need a real gfortran for LMGC90, which Apple's clang doesn't provide.
# Homebrew gcc ships gfortran as `gfortran-<major>` (e.g. gfortran-15),
# so we discover the version by globbing brew's gcc keg, then pin
# CMAKE_<LANG>_COMPILER to absolute paths. OpenBLAS is also keg-only on
# Homebrew, so its install prefix is wired through to BLAS_LIBRARIES /
# LAPACK_LIBRARIES the same way we do on Windows.
# ===================================================================
if(APPLE AND NOT DEFINED CMAKE_C_COMPILER)
    # Homebrew's unversioned `gcc` formula tracks the newest major release.
    # The last green macOS build (main @ 44cfda28, 2026-05-04) used whatever
    # GCC was current then; after Homebrew moved on, linking _lmgc90 began
    # failing on arm64 with undefined nanobind func_create<...> template
    # instantiations out of lmgc90.cpp.o. Pin the major version rather than
    # tracking latest, and fall back to unversioned gcc if the pin is absent.
    # Keep the pin in sync with `before-all` in pyproject.toml.
    if(NOT DEFINED LMGC90_MACOS_BREW_GCC)
        set(LMGC90_MACOS_BREW_GCC "gcc@15")
    endif()

    # brew --prefix returns a path for any *known* formula whether or not it
    # is installed, so probe for a real gfortran binary rather than trusting
    # the prefix alone.
    foreach(_formula "${LMGC90_MACOS_BREW_GCC}" "gcc")
        execute_process(
            COMMAND brew --prefix ${_formula}
            OUTPUT_VARIABLE _prefix
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
        )
        if(_prefix)
            file(GLOB _gfortrans "${_prefix}/bin/gfortran-*")
            if(_gfortrans)
                set(HOMEBREW_GCC_PREFIX "${_prefix}")
                set(GFORTRAN_VERSIONS ${_gfortrans})
                message(STATUS "macOS: using Homebrew formula '${_formula}' at ${_prefix}")
                break()
            endif()
        endif()
    endforeach()

    if(GFORTRAN_VERSIONS)
        list(SORT GFORTRAN_VERSIONS)
        list(GET GFORTRAN_VERSIONS -1 GFORTRAN_PATH)
        get_filename_component(GFORTRAN_NAME ${GFORTRAN_PATH} NAME)
        string(REGEX REPLACE "gfortran-" "" GCC_VERSION "${GFORTRAN_NAME}")

        set(CMAKE_C_COMPILER "${HOMEBREW_GCC_PREFIX}/bin/gcc-${GCC_VERSION}" CACHE STRING "")
        set(CMAKE_CXX_COMPILER "${HOMEBREW_GCC_PREFIX}/bin/g++-${GCC_VERSION}" CACHE STRING "")
        set(CMAKE_Fortran_COMPILER "${HOMEBREW_GCC_PREFIX}/bin/gfortran-${GCC_VERSION}" CACHE STRING "")
        message(STATUS "macOS: Auto-detected Homebrew GCC ${GCC_VERSION}")
    else()
        message(WARNING "macOS: no Homebrew gfortran found; falling back to CMake defaults")
    endif()
endif()

# macOS uses Apple Accelerate via LMGC90's BLA_VENDOR=Apple — overriding
# BLAS_LIBRARIES here would pull Homebrew openblas → libomp.dylib → conda libomp double-init.

# ===================================================================
# Windows: Bootstrap MinGW-w64 + OpenBLAS before project()
# ===================================================================
include(BootstrapWindowsToolchain)

project(compas_lmgc90 LANGUAGES CXX Fortran C)

# MinGW runtime linkage strategy on Windows.
#
# libgcc and libstdc++ embed cleanly via driver flags (-static-libgcc,
# -static-libstdc++) on the final _lmgc90.pyd link.
#
# libgfortran, libquadmath, and libwinpthread are different. We tried
# absolute-path static archives, but gnu ld is single-pass: by the time
# it processes liblmgc90-core.a (a static archive containing thousands
# of Fortran objects with references like _gfortran_st_close), it has
# already moved past libgfortran.a earlier in the link line and refuses
# to look back, producing thousands of undefined-reference errors. We
# could wrap the whole archive set in --start-group/--end-group, but
# that requires knowing every transitive contrib that LMGC90 propagates
# (we don't have a stable list to hand), and CMake's LINK_GROUP genex
# doesn't reach into INTERFACE_LINK_LIBRARIES of dependencies.
#
# Simpler and still proper: leave the dynamic linkage for these three
# (CMake's auto-injected -lgfortran/-lquadmath/-lwinpthread resolves to
# the .dll.a import libs and the DLLs land in _lmgc90.pyd's import
# table) and bundle the runtime DLLs alongside _lmgc90.pyd. Windows'
# loader resolves direct-dep DLLs of a .pyd from the module's
# directory via LOAD_WITH_ALTERED_SEARCH_PATH — same mechanism that
# already works for libopenblas.dll. No os.add_dll_directory shim is
# needed; this is the standard wheel pattern (it's what delvewheel
# would have produced if delvewheel had worked on this layout).
set(_compas_lmgc90_runtime_dlls "")
if(WIN32 AND CMAKE_Fortran_COMPILER_ID STREQUAL "GNU"
   AND DEFINED COMPAS_LMGC90_WINLIBS_BIN)
    # Direct deps of _lmgc90.pyd: gfortran/quadmath/winpthread are added
    # to its import table by CMake's auto-injected -l<name>. libstdc++-6
    # and libgcc_s_seh-1 are statically embedded into _lmgc90.pyd via
    # -static-libgcc / -static-libstdc++ on its link, so they're NOT in
    # the .pyd's import table.
    #
    # However, libgfortran-5.dll itself (a prebuilt DLL from WinLibs)
    # has imports on libgcc_s_seh-1.dll and libquadmath-0.dll (which
    # in turn imports libgcc_s_seh-1.dll). Our -static-libgcc only
    # strips libgcc out of OUR link, not out of the prebuilt
    # libgfortran-5.dll. So we have to bundle libgcc_s_seh-1.dll too.
    # libstdc++-6.dll is included defensively (libgfortran-5 doesn't
    # need it on most MinGW builds, but other transitive contribs in
    # LMGC90 might).
    # Globs use a digit suffix (e.g. libgomp-[0-9]*.dll) rather than
    # libgomp-*.dll so we skip libgomp-plugin-nvptx-1.dll (the CUDA
    # offload plugin) and any other "libgomp-plugin-*"-style helpers
    # that share the libgomp- prefix.
    foreach(_pattern
            "libgfortran-[0-9]*.dll"
            "libquadmath-[0-9]*.dll"
            "libwinpthread-[0-9]*.dll"
            "libgcc_s_*.dll"
            "libstdc++-[0-9]*.dll"
            "libssp-[0-9]*.dll"
            "libgomp-[0-9]*.dll"
            "libatomic-[0-9]*.dll")
        file(GLOB _dlls "${COMPAS_LMGC90_WINLIBS_BIN}/${_pattern}")
        foreach(_dll IN LISTS _dlls)
            list(APPEND _compas_lmgc90_runtime_dlls "${_dll}")
            message(STATUS "compas_lmgc90: bundling MinGW runtime DLL ${_dll}")
        endforeach()
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
# Otherwise provide a local source directory (can be a user version)
#
#   export LMGC90_GIT_URL="https://USER:TOKEN@git-xen.lmgc.univ-montp2.fr/lmgc90/lmgc90_dev.git"
if(NOT DEFINED LMGC90_GIT_URL OR LMGC90_GIT_URL STREQUAL "")
    if(DEFINED ENV{LMGC90_GIT_URL} AND NOT "$ENV{LMGC90_GIT_URL}" STREQUAL "")
        set(LMGC90_GIT_URL "$ENV{LMGC90_GIT_URL}")
    endif()
endif()
if(NOT DEFINED LMGC90_SOURCE_DIR OR LMGC90_SOURCE_DIR STREQUAL "")
    if(DEFINED ENV{LMGC90_SOURCE_DIR} AND NOT "$ENV{LMGC90_SOURCE_DIR}" STREQUAL "")
        set(LMGC90_SOURCE_DIR "$ENV{LMGC90_SOURCE_DIR}")
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
if(NOT LMGC90_GIT_URL AND NOT LMGC90_SOURCE_DIR)
    message(FATAL_ERROR
        "Neither LMGC90_GIT_URL nor LMGC90_SOURCE_DIR is set. "
        "Pass -DLMGC90_GIT_URL=<url> or set "
        "the LMGC90_GIT_URL environment variable. The URL must include "
        "credentials if the LMGC90 repo is private."
        "Or provide a local directory with the sources with -DLMGC90_SOURCE_DIR=<path>")
endif()

if( LMGC90_GIT_URL )
    FetchContent_Declare(lmgc90
        GIT_REPOSITORY "${LMGC90_GIT_URL}"
        GIT_TAG aa4687c2eb0d2d67249ea6e04f17a990b894107f  # tip of fix_compas, 2026-07-22
    )
elseif( LMGC90_SOURCE_DIR )
    FetchContent_Declare(lmgc90 SOURCE_DIR ${LMGC90_SOURCE_DIR})
endif()

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

# Statically embed libgcc + libstdc++ via the gcc/g++ driver flags.
# libgfortran/libquadmath/libwinpthread stay dynamic and ride along as
# bundled sibling DLLs (see the install block below and the strategy
# comment near the top of this file).
if(WIN32 AND CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_link_options(_lmgc90 PRIVATE
        -static-libgcc
        -static-libstdc++
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

if(UNIX AND NOT APPLE)
    # Linux: explicit $ORIGIN so _lmgc90.so can find sibling .so libs in
    # compas_lmgc90/. CMake bakes this directly into DT_RUNPATH at link
    # time; no install_name_tool / patchelf rewrite at install time.
    #
    # Apple is excluded here on purpose: nanobind_add_module + scikit-
    # build-core already inject `@loader_path` into the .so's LC_RPATH
    # at link time, and macOS's install_name_tool refuses to add a
    # duplicate path ("would duplicate path, file already has LC_RPATH
    # for: @loader_path"), so re-setting INSTALL_RPATH to @loader_path
    # makes the install step itself error out before delocate runs.
    # Windows has no rpath concept; sibling DLLs are resolved via
    # LOAD_WITH_ALTERED_SEARCH_PATH from the .pyd's directory.
    set_target_properties(_lmgc90 PROPERTIES INSTALL_RPATH "${_RPATH}")
endif()

if(WIN32 AND COMPAS_LMGC90_OPENBLAS_DLL AND EXISTS "${COMPAS_LMGC90_OPENBLAS_DLL}")
    install(FILES "${COMPAS_LMGC90_OPENBLAS_DLL}"
            DESTINATION compas_lmgc90)
endif()

# Bundle the MinGW runtime DLLs alongside _lmgc90.pyd. They are direct
# deps of the .pyd (or transitive deps of libgfortran-5.dll, which the
# loader resolves from the same directory), so Windows' altered DLL
# search path picks them up without an os.add_dll_directory shim.
if(WIN32 AND _compas_lmgc90_runtime_dlls)
    install(FILES ${_compas_lmgc90_runtime_dlls}
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
