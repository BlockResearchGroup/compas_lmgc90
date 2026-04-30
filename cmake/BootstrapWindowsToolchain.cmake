# =============================================================================
# Windows toolchain bootstrap.
#
# When the user builds on Windows without an existing Fortran compiler, this
# module downloads a pinned WinLibs MinGW-w64 (UCRT) toolchain and a pinned
# OpenBLAS Windows binary into a project-local cache, then sets the CMake
# compiler / BLAS variables to point at them. The intent is that
# `pip install .` works on a fresh Windows box without any prerequisites.
#
# Must be included BEFORE the top-level project() call, because project()
# locks the choice of compiler.
# =============================================================================

if(NOT WIN32)
    return()
endif()

# If the user already pointed CMake at a Fortran compiler, respect it and exit.
if(CMAKE_Fortran_COMPILER OR DEFINED ENV{FC})
    message(STATUS "compas_lmgc90: Fortran compiler already set, skipping toolchain bootstrap")
    return()
endif()

set(_winlibs_url "https://github.com/brechtsanders/winlibs_mingw/releases/download/15.2.0posix-14.0.0-ucrt-r7/winlibs-x86_64-posix-seh-gcc-15.2.0-mingw-w64ucrt-14.0.0-r7.zip")
set(_winlibs_sha "cb2fbad6162540cdf5e1facdce08d4dac359e8cf64f7f696a99274291763b815")

set(_openblas_url "https://github.com/OpenMathLib/OpenBLAS/releases/download/v0.3.33/OpenBLAS-0.3.33-x64.zip")
set(_openblas_sha "7ad797ef0c9a5c42e28903bf726eaaaade307dafe187ff0e923d90cd4002780c")

# Cache outside the build dir so cibuildwheel and successive `pip install .`
# invocations can share the same download.
if(DEFINED ENV{COMPAS_LMGC90_TOOLCHAIN_CACHE})
    set(_cache_root "$ENV{COMPAS_LMGC90_TOOLCHAIN_CACHE}")
elseif(DEFINED ENV{LOCALAPPDATA})
    set(_cache_root "$ENV{LOCALAPPDATA}/compas_lmgc90/toolchain")
else()
    set(_cache_root "${CMAKE_BINARY_DIR}/_toolchain")
endif()
file(TO_CMAKE_PATH "${_cache_root}" _cache_root)

set(_winlibs_dir  "${_cache_root}/winlibs-15.2.0-ucrt-r7")
set(_openblas_dir "${_cache_root}/openblas-0.3.33")

set(_gfortran "${_winlibs_dir}/mingw64/bin/gfortran.exe")
set(_gcc      "${_winlibs_dir}/mingw64/bin/gcc.exe")
set(_gxx      "${_winlibs_dir}/mingw64/bin/g++.exe")

# ---- WinLibs ---------------------------------------------------------------
if(NOT EXISTS "${_gfortran}")
    set(_zip "${_cache_root}/winlibs.zip")
    file(MAKE_DIRECTORY "${_cache_root}")
    message(STATUS "compas_lmgc90: downloading WinLibs MinGW-w64 (~700 MB)…")
    file(DOWNLOAD "${_winlibs_url}" "${_zip}"
         EXPECTED_HASH SHA256=${_winlibs_sha}
         SHOW_PROGRESS
         TLS_VERIFY ON
         STATUS _dl_status)
    list(GET _dl_status 0 _dl_code)
    if(NOT _dl_code EQUAL 0)
        list(GET _dl_status 1 _dl_msg)
        message(FATAL_ERROR "compas_lmgc90: WinLibs download failed: ${_dl_msg}")
    endif()
    message(STATUS "compas_lmgc90: extracting WinLibs to ${_winlibs_dir}")
    file(MAKE_DIRECTORY "${_winlibs_dir}")
    file(ARCHIVE_EXTRACT INPUT "${_zip}" DESTINATION "${_winlibs_dir}")
    file(REMOVE "${_zip}")
endif()

if(NOT EXISTS "${_gfortran}")
    message(FATAL_ERROR "compas_lmgc90: gfortran.exe not found after WinLibs extraction at ${_gfortran}")
endif()

# ---- OpenBLAS --------------------------------------------------------------
if(NOT EXISTS "${_openblas_dir}/lib/libopenblas.dll.a")
    set(_zip "${_cache_root}/openblas.zip")
    file(MAKE_DIRECTORY "${_cache_root}")
    message(STATUS "compas_lmgc90: downloading OpenBLAS 0.3.33 (~40 MB)…")
    file(DOWNLOAD "${_openblas_url}" "${_zip}"
         EXPECTED_HASH SHA256=${_openblas_sha}
         SHOW_PROGRESS
         TLS_VERIFY ON
         STATUS _dl_status)
    list(GET _dl_status 0 _dl_code)
    if(NOT _dl_code EQUAL 0)
        list(GET _dl_status 1 _dl_msg)
        message(FATAL_ERROR "compas_lmgc90: OpenBLAS download failed: ${_dl_msg}")
    endif()
    message(STATUS "compas_lmgc90: extracting OpenBLAS to ${_openblas_dir}")
    file(MAKE_DIRECTORY "${_openblas_dir}")
    file(ARCHIVE_EXTRACT INPUT "${_zip}" DESTINATION "${_openblas_dir}")
    file(REMOVE "${_zip}")
endif()

# ---- Expose to the project -------------------------------------------------
set(CMAKE_C_COMPILER       "${_gcc}"       CACHE FILEPATH "" FORCE)
set(CMAKE_CXX_COMPILER     "${_gxx}"       CACHE FILEPATH "" FORCE)
set(CMAKE_Fortran_COMPILER "${_gfortran}"  CACHE FILEPATH "" FORCE)

# Prepend toolchain + OpenBLAS bin dirs to PATH for find_program (ar, ld, …),
# runtime DLL loading during CMake check_function_exists / try_run probes,
# and so that downstream tools can locate libopenblas.dll at link/run time.
set(ENV{PATH} "${_winlibs_dir}/mingw64/bin;${_openblas_dir}/bin;$ENV{PATH}")

# OpenBLAS provides BLAS + LAPACK in a single import library on Windows.
set(BLAS_LIBRARIES   "${_openblas_dir}/lib/libopenblas.dll.a" CACHE FILEPATH "" FORCE)
set(LAPACK_LIBRARIES "${_openblas_dir}/lib/libopenblas.dll.a" CACHE FILEPATH "" FORCE)

# Re-exported so the rest of the build can install/copy the runtime DLL.
set(COMPAS_LMGC90_OPENBLAS_DLL "${_openblas_dir}/bin/libopenblas.dll" CACHE FILEPATH "" FORCE)
set(COMPAS_LMGC90_WINLIBS_BIN  "${_winlibs_dir}/mingw64/bin"          CACHE PATH     "" FORCE)

message(STATUS "compas_lmgc90: using bootstrapped toolchain at ${_winlibs_dir}")
message(STATUS "compas_lmgc90: using bootstrapped OpenBLAS at  ${_openblas_dir}")
