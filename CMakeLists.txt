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

# Configure Intel MKL / runtime libraries
set(_intel_default_lib_dir "${CMAKE_CURRENT_SOURCE_DIR}/external/ubuntu/intel_libs")
set(INTEL_LIBS_DIR "" CACHE PATH "Directory containing Intel MKL runtime libraries (leave empty to auto-detect)")

set(_intel_candidate_dirs "")
if(INTEL_LIBS_DIR)
    list(APPEND _intel_candidate_dirs "${INTEL_LIBS_DIR}")
endif()
if(DEFINED ENV{INTEL_LIBS_DIR})
    list(APPEND _intel_candidate_dirs "$ENV{INTEL_LIBS_DIR}")
endif()
if(DEFINED ENV{MKLROOT})
    list(APPEND _intel_candidate_dirs
        "$ENV{MKLROOT}"
        "$ENV{MKLROOT}/lib"
        "$ENV{MKLROOT}/lib/intel64"
        "$ENV{MKLROOT}/lib/intel64_lin")
endif()
if(DEFINED ENV{ONEAPI_ROOT})
    list(APPEND _intel_candidate_dirs
        "$ENV{ONEAPI_ROOT}/mkl/latest/lib/intel64"
        "$ENV{ONEAPI_ROOT}/compiler/latest/linux/compiler/lib/intel64_lin")
endif()
if(DEFINED ENV{CONDA_PREFIX})
    list(APPEND _intel_candidate_dirs "$ENV{CONDA_PREFIX}/lib")
endif()
list(APPEND _intel_candidate_dirs "${_intel_default_lib_dir}")

set(INTEL_LIBRARY_SEARCH_PATHS "")
foreach(_candidate IN LISTS _intel_candidate_dirs)
    if(_candidate AND EXISTS "${_candidate}")
        file(REAL_PATH "${_candidate}" _candidate_normalized)
        list(APPEND INTEL_LIBRARY_SEARCH_PATHS "${_candidate_normalized}")
    endif()
endforeach()
list(REMOVE_DUPLICATES INTEL_LIBRARY_SEARCH_PATHS)

if(INTEL_LIBRARY_SEARCH_PATHS)
    list(GET INTEL_LIBRARY_SEARCH_PATHS 0 _intel_primary_dir)
    set_property(CACHE INTEL_LIBS_DIR PROPERTY VALUE "${_intel_primary_dir}")
else()
    message(FATAL_ERROR
        "Could not locate Intel MKL runtime libraries.\n"
        "Set INTEL_LIBS_DIR or ensure MKL/oneAPI is available on the system.")
endif()

message(STATUS "Searching Intel libraries in: ${INTEL_LIBRARY_SEARCH_PATHS}")

set(_intel_required_mkl_libs
    mkl_intel_lp64
    mkl_sequential
    mkl_core
)

set(BLAS_LIBS "")
set(_missing_required_libs "")
foreach(_lib IN LISTS _intel_required_mkl_libs)
    find_library(_lib_path
        NAMES ${_lib} lib${_lib}.so lib${_lib}.so.1 lib${_lib}.so.2
        PATHS ${INTEL_LIBRARY_SEARCH_PATHS}
        NO_DEFAULT_PATH
    )
    if(NOT _lib_path)
        find_library(_lib_path
            NAMES ${_lib}
            PATHS ${INTEL_LIBRARY_SEARCH_PATHS}
        )
    endif()
    if(_lib_path)
        list(APPEND BLAS_LIBS "${_lib_path}")
    else()
        list(APPEND _missing_required_libs "${_lib}")
    endif()
endforeach()

if(_missing_required_libs)
    message(FATAL_ERROR
        "Required Intel MKL libraries not found: ${_missing_required_libs}\n"
        "Searched in: ${INTEL_LIBRARY_SEARCH_PATHS}\n"
        "Set INTEL_LIBS_DIR to the directory containing these libraries.")
endif()

set(_intel_optional_runtime_libs
    intlc
    svml
    irng
    imf
)

set(INTEL_LIBS "")
set(_missing_optional_libs "")
foreach(_lib IN LISTS _intel_optional_runtime_libs)
    find_library(_lib_path
        NAMES ${_lib} lib${_lib}.so lib${_lib}.so.1 lib${_lib}.so.2 lib${_lib}.so.5
        PATHS ${INTEL_LIBRARY_SEARCH_PATHS}
        NO_DEFAULT_PATH
    )
    if(NOT _lib_path)
        find_library(_lib_path
            NAMES ${_lib}
            PATHS ${INTEL_LIBRARY_SEARCH_PATHS}
        )
    endif()
    if(_lib_path)
        list(APPEND INTEL_LIBS "${_lib_path}")
    else()
        list(APPEND _missing_optional_libs "${_lib}")
    endif()
endforeach()

if(_missing_optional_libs)
    message(WARNING
        "Optional Intel runtime libraries not found: ${_missing_optional_libs}\n"
        "The build will proceed, but ensure your MKL installation is complete if "
        "linker errors occur.")
endif()

set(_intel_all_libs ${BLAS_LIBS} ${INTEL_LIBS})
set(_intel_rpaths "")
foreach(_lib_path IN LISTS _intel_all_libs)
    get_filename_component(_lib_dir "${_lib_path}" DIRECTORY)
    list(APPEND _intel_rpaths "${_lib_dir}")
endforeach()
list(REMOVE_DUPLICATES _intel_rpaths)
set(_intel_build_rpath ${_intel_rpaths})
list(APPEND _intel_build_rpath "${LMGC_LIB_PATH}")
list(REMOVE_DUPLICATES _intel_build_rpath)

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
    BUILD_RPATH "${_intel_build_rpath}"
    INSTALL_RPATH "$ORIGIN/../external/ubuntu/intel_libs:$ORIGIN/../external/lmgc90_libs"
    INSTALL_RPATH_USE_LINK_PATH FALSE
)

install(TARGETS _lmgc90 LIBRARY DESTINATION compas_lmgc90)

# Install Intel libraries that were linked
if(_intel_all_libs)
    install(FILES ${_intel_all_libs} DESTINATION external/ubuntu/intel_libs
            PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                       GROUP_READ GROUP_EXECUTE
                       WORLD_READ WORLD_EXECUTE)
endif()

# Install LMGC90 libraries with the package
install(DIRECTORY ${LMGC_LIB_PATH}/ DESTINATION external/lmgc90_libs
        FILES_MATCHING PATTERN "*.so*"
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE 
                   GROUP_READ GROUP_EXECUTE 
                   WORLD_READ WORLD_EXECUTE)

message(STATUS "===============================================")
message(STATUS "Build Configuration:")
message(STATUS "  Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ Standard: C++${CMAKE_CXX_STANDARD}")
message(STATUS "  Compiler: ${CMAKE_CXX_COMPILER_ARG}")
message(STATUS "  BLAS: Intel MKL (bundled)")
message(STATUS "  Libs: ${INTEL_LIBS_DIR}")
message(STATUS "===============================================")