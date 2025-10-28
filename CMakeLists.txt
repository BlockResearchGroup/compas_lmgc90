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

# Find MKL or BLAS and Intel libraries
# Check if manually placed Intel libraries exist (no oneAPI env needed!)
set(INTEL_LIBS_DIR "${CMAKE_CURRENT_SOURCE_DIR}/external/ubuntu/intel_libs")
if(EXISTS "${INTEL_LIBS_DIR}/libmkl_core.so.2")
    # Use manually bundled Intel MKL libraries (no MKLROOT env variable needed!)
    message(STATUS "Found manually bundled Intel MKL in: ${INTEL_LIBS_DIR}")
    
    # ===== COMMENTED OUT: Auto-copy section (using manual libs in external/ubuntu) =====
    # # Create directory for bundled Intel libraries
    # file(MAKE_DIRECTORY ${INTEL_LIBS_DIR})
    # 
    # # Find and copy MKL shared libraries
    # set(MKL_LIB_SOURCE "${MKL_ROOT}/lib")
    # if(NOT EXISTS ${MKL_LIB_SOURCE})
    #     set(MKL_LIB_SOURCE "${MKL_ROOT}/lib/intel64")
    # endif()
    # 
    # # Copy MKL libraries
    # foreach(lib mkl_intel_lp64 mkl_sequential mkl_core)
    #     file(GLOB MKL_SO "${MKL_LIB_SOURCE}/lib${lib}.so*")
    #     if(MKL_SO)
    #         file(COPY ${MKL_SO} DESTINATION ${INTEL_LIBS_DIR})
    #         message(STATUS "Bundled: ${lib}")
    #     endif()
    # endforeach()
    # 
    # # Find and copy Intel compiler runtime libraries
    # set(COMPILER_LIB_DIR "")
    # if(DEFINED ENV{CMPLR_ROOT})
    #     set(COMPILER_LIB_DIR "$ENV{CMPLR_ROOT}/lib")
    # elseif(EXISTS "/opt/intel/oneapi/compiler/latest/lib")
    #     set(COMPILER_LIB_DIR "/opt/intel/oneapi/compiler/latest/lib")
    # endif()
    # 
    # if(COMPILER_LIB_DIR)
    #     foreach(lib intlc svml irng imf)
    #         file(GLOB COMP_SO "${COMPILER_LIB_DIR}/lib${lib}.so*")
    #         if(COMP_SO)
    #             file(COPY ${COMP_SO} DESTINATION ${INTEL_LIBS_DIR})
    #             message(STATUS "Bundled: ${lib}")
    #         endif()
    #     endforeach()
    # endif()
    # ===== END COMMENTED SECTION =====
    
    set(BLAS_LIBS -L${INTEL_LIBS_DIR} -lmkl_intel_lp64 -lmkl_sequential -lmkl_core)
    set(INTEL_LIBS -L${INTEL_LIBS_DIR} -lintlc -lsvml -lirng -limf)
    message(STATUS "Using Intel MKL with manually placed libraries: ${INTEL_LIBS_DIR}")
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

# Set RPATH so the module can find bundled Intel libraries
if(EXISTS "${INTEL_LIBS_DIR}/libmkl_core.so.2")
    set_target_properties(_lmgc90 PROPERTIES
        BUILD_RPATH "${INTEL_LIBS_DIR}"
        INSTALL_RPATH "$ORIGIN/../external/ubuntu/intel_libs"
        INSTALL_RPATH_USE_LINK_PATH FALSE
    )
endif()

install(TARGETS _lmgc90 LIBRARY DESTINATION compas_lmgc90)

# Install bundled Intel libraries
if(EXISTS ${INTEL_LIBS_DIR})
    install(DIRECTORY ${INTEL_LIBS_DIR}/ DESTINATION external/ubuntu/intel_libs
            FILES_MATCHING PATTERN "*.so*"
            PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE 
                       GROUP_READ GROUP_EXECUTE 
                       WORLD_READ WORLD_EXECUTE)
endif()

message(STATUS "===============================================")
message(STATUS "Build Configuration:")
message(STATUS "  Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ Standard: C++${CMAKE_CXX_STANDARD}")
message(STATUS "  Compiler: ${CMAKE_CXX_COMPILER_ARG}")
if(EXISTS "${INTEL_LIBS_DIR}/libmkl_core.so.2")
    message(STATUS "  BLAS: Intel MKL (from bundled libraries)")
else()
    message(STATUS "  BLAS: OpenBLAS")
endif()
message(STATUS "===============================================")