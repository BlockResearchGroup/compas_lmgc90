cmake_minimum_required(VERSION 3.18)
project(comgc90_example C Fortran)

set(CMAKE_C_STANDARD 11)
set(CMAKE_Fortran_STANDARD 2008)

# If building inside the LMGC90 dev tree, prefer core targets (robust)
if(TARGET lmgc_core_shared AND TARGET lmgc_core_kernel_3d)
  add_executable(comgc90 comgc90_with_initial_transform.c wrap_lmgc90_compas.f90)
  # Use module dir from the dev build
  target_include_directories(comgc90 PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})
  target_compile_options(comgc90 PRIVATE -O0 -g $<$<COMPILE_LANGUAGE:Fortran>:-fbacktrace>)
  target_link_libraries(comgc90 PRIVATE
    lmgc_core_shared
    lmgc_core_mailx
    lmgc_core_rigid_3d
    lmgc_core_contactor_3d
    lmgc_core_contact_3d
    lmgc_core_kernel_3d
    lmgc_core_post_3d
    lmgc_bindings_sparse_la
    mumps_common
    pord
    mpiseq)
  # Ensure sparse libs (e.g., MUMPS) are linked the same way as in Bindings
  if(DEFINED SPARSE_LIBRARIES)
    target_link_libraries(comgc90 PRIVATE ${SPARSE_LIBRARIES})
  endif()
  # Fortran as linker language (like other standalones)
  set_property(TARGET comgc90 PROPERTY LINKER_LANGUAGE Fortran)
  message(STATUS "comgc90: using LMGC90 core targets from dev build")


else()
  # Fallback: standalone (fragile) mode, try to locate libs manually
  set(LMGC90_BUILD_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../../../../build/cp312-abi3-linux_x86_64/lmgc90_core")
  set(LMGC90_MODULES_DIR "${LMGC90_BUILD_DIR}/modules")
  set(LMGC90_LIB_DIR "${LMGC90_BUILD_DIR}/lib")

  if(NOT EXISTS ${LMGC90_LIB_DIR})
      message(FATAL_ERROR "LMGC90 libraries not found at: ${LMGC90_LIB_DIR}\n"
                          "Please build the main project first: pip install -e .")
  endif()

  file(GLOB LMGC90_LIBRARIES "${LMGC90_LIB_DIR}/liblmgc*.a")

  add_library(wrap_compas OBJECT wrap_lmgc90_compas.f90)
  target_include_directories(wrap_compas PRIVATE ${LMGC90_MODULES_DIR})
  target_compile_options(wrap_compas PRIVATE -O0 -g -fbacktrace -fbounds-check)

  set(PREDICATES_LIB "${LMGC90_LIB_DIR}/libpredicates.a")
  set(MINPACK_LIB "${LMGC90_LIB_DIR}/libminpack.a")

  if(DEFINED ENV{MKLROOT})
      set(BLAS_LIBRARIES "-L$ENV{MKLROOT}/lib -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl")
  else()
      set(SCIPY_BLAS "$ENV{CONDA_PREFIX}/lib/python3.12/site-packages/scipy.libs/libscipy_openblas-b75cc656.so")
      if(EXISTS ${SCIPY_BLAS})
          set(BLAS_LIBRARIES ${SCIPY_BLAS})
      else()
          set(BLAS_LIBRARIES "-lblas -llapack")
      endif()
  endif()

  add_executable(comgc90 comgc90_with_initial_transform.c)
  target_link_libraries(comgc90 PRIVATE 
      wrap_compas
      -Wl,--start-group ${LMGC90_LIBRARIES} -Wl,--end-group
      ${PREDICATES_LIB}
      ${MINPACK_LIB}
      ${BLAS_LIBRARIES}
      gfortran m stdc++ pthread dl)
  target_compile_options(comgc90 PRIVATE -O0 -g)
  message(STATUS "comgc90: using standalone manual libraries (fragile)")
endif()
