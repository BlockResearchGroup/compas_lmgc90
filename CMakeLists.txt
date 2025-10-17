cmake_minimum_required(VERSION 3.15)
project(compas_lmgc90 LANGUAGES CXX Fortran)

set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find Python and nanobind
find_package(Python 3.8 REQUIRED COMPONENTS Interpreter Development.Module Development.SABIModule)
find_package(nanobind CONFIG REQUIRED)

# Compile the Fortran wrapper as a shared library
add_library(wrap_lmgc90_compas OBJECT 
    src/lmgc90_dev/src/Sandbox/Compas/wrap_lmgc90_compas.f90
)

# Find LMGC90 library - default to local build
if(DEFINED ENV{LMGC_LIB_PATH})
    set(LMGC_LIB_PATH $ENV{LMGC_LIB_PATH})
else()
    set(LMGC_LIB_PATH "${CMAKE_CURRENT_SOURCE_DIR}/src/lmgc90_dev/build_core_min_oneapi/lib")
endif()

if(DEFINED ENV{LMGC_INCLUDE})
    set(LMGC_INCLUDE $ENV{LMGC_INCLUDE})
else()
    set(LMGC_INCLUDE "${CMAKE_CURRENT_SOURCE_DIR}/src/lmgc90_dev/build_core_min_oneapi/modules")
endif()

message(STATUS "LMGC90 library path: ${LMGC_LIB_PATH}")
message(STATUS "LMGC90 include path: ${LMGC_INCLUDE}")

target_include_directories(wrap_lmgc90_compas PRIVATE ${LMGC_INCLUDE})

# Build nanobind wrapper
nanobind_add_module(_lmgc90 STABLE_ABI NB_STATIC src/lmgc90.cpp)

target_include_directories(_lmgc90 PRIVATE 
    ${CMAKE_CURRENT_SOURCE_DIR}/src/lmgc90_dev/src/Sandbox/Compas
)

# Build iterative nanobind wrapper
nanobind_add_module(_lmgc90_iterative STABLE_ABI NB_STATIC src/lmgc90_iterative.cpp)

target_include_directories(_lmgc90_iterative PRIVATE 
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

# Link against LMGC90 and the Fortran wrapper for iterative module
target_link_libraries(_lmgc90_iterative PRIVATE 
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

install(TARGETS _lmgc90 _lmgc90_iterative LIBRARY DESTINATION compas_lmgc90)

message(STATUS "============= Build Configuration =============")
message(STATUS "Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "C++ Standard: C++${CMAKE_CXX_STANDARD}")
message(STATUS "=======================================")