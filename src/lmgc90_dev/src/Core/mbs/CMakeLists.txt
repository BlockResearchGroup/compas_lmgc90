
include_directories( ${TEMPLATE_DIRECTORY} )

set(LMGC90_CORE_MBS2D_SRCS mod_mbs2D.f90)
set(LMGC90_CORE_MBS3D_SRCS mod_mbs3D.f90)

add_library(lmgc_core_mbs2d ${LMGC90_CORE_MBS2D_SRCS})
add_library(lmgc_core_mbs3d ${LMGC90_CORE_MBS3D_SRCS})

target_link_libraries(lmgc_core_mbs2d lmgc_core_shared
                                      lmgc_bindings_MBS
                     )

target_link_libraries(lmgc_core_mbs3d lmgc_core_shared
                                      lmgc_bindings_MBS
                     )


if( NOT ${NO_TEST} )
  # ugly but necessary
  if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lgcc_s")
  endif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")

  add_executable( test_mbs2d tst_mbs_2D.f90 )
  target_link_libraries(test_mbs2d lmgc_core_mbs2d)
  add_test( test_mbs2d ${EXECUTABLE_OUTPUT_PATH}/test_mbs2d )
  set_property(TARGET test_mbs2d PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )

  add_executable( test_mbs3d tst_mbs_3D.f90 )
  target_link_libraries(test_mbs3d lmgc_core_mbs3d)
  add_test( test_mbs3d ${EXECUTABLE_OUTPUT_PATH}/test_mbs3d )
  set_property(TARGET test_mbs3d PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
endif( NOT ${NO_TEST} )

