# configure version numbering in Fortran side
add_custom_target(version_init ALL
                  COMMAND ${CMAKE_COMMAND}
                          -Ddir=${CMAKE_SOURCE_DIR}
                          -Dfile_in=${CMAKE_CURRENT_SOURCE_DIR}/version.f90.in
                          -Dfile_out=${CMAKE_CURRENT_BINARY_DIR}/version.f90
                          -DGIT_EXECUTABLE=${GIT_EXECUTABLE}
                          -Drelease="${YEAR}${RCWITHDOT}"
                          -P ${CMAKE_SOURCE_DIR}/cmake/scripts/set_revision_number.cmake
                  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/version.f90.in
                 )

# do not forget to tell where to find the include file
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR} )

SET(LMGC90_CORE_UTILS_SRCS  mod_utilities.f90
                            mod_parameters.f90
                            mod_overall.f90
                           )

SET(LMGC90_CORE_SHARED_SRCS mod_a_EF.f90
                            mod_algebra.f90
                            mod_polygon.f90
                            mod_meca_polygon.f90
                            mod_DiscreteGeometry.f90
                            mod_renum.f90
                            mod_RigidKinematic.f90
                            mod_face2face.f90
                            mod_a_DOF.f90
                            mod_models.f90
                            mod_bulk_behav.f90
                            mod_tact_behav.f90
                            mod_a_MATRIX.f90
                            mod_a_system.f90
                            mod_anonymous.f90
                            mod_anonymous_container.f90
                            mod_anonymous_ptr_container.f90
                            mod_rough_detections.f90
                            mod_external_models_handler.f90
                            ${LMGC90_BINDINGS_MODELS_SRCS}
                           )

if(${WITH_OPENMP})
  set(LMGC90_CORE_UTILS_SRCS ${LMGC90_CORE_UTILS_SRCS} mod_ompTimer.f90)
else(${WITH_OPENMP})
  set(LMGC90_CORE_UTILS_SRCS ${LMGC90_CORE_UTILS_SRCS} mod_timer.f90)
endif(${WITH_OPENMP})

if(${WITH_MPI})
  SET(LMGC90_CORE_UTILS_SRCS ${LMGC90_CORE_UTILS_SRCS} mod_LMGC90_MPI.f90)
else(${WITH_MPI})
  SET(LMGC90_CORE_UTILS_SRCS ${LMGC90_CORE_UTILS_SRCS} mod_LMGC90_no_MPI.f90)
endif(${WITH_MPI})

if( ${OPT} STREQUAL "opt" )
  set(LMGC90_CORE_SHARED_SRCS ${LMGC90_CORE_SHARED_SRCS} 
                              mod_no_paranoid_checks.f90
                             )
else()
  set(LMGC90_CORE_SHARED_SRCS ${LMGC90_CORE_SHARED_SRCS} 
                              mod_paranoid_checks.f90
                             )
endif()


ADD_LIBRARY(lmgc_core_utils   ${LMGC90_CORE_UTILS_SRCS} )
target_link_libraries(lmgc_core_utils lmgc_core_utils_contribs)


INCLUDE_DIRECTORIES( ${TEMPLATE_DIRECTORY} )
INCLUDE_DIRECTORIES( ${LMGC90_BINDINGS_MODELS_INCLUDE} )

set(LMGC90_CORE_SHARED_TARGET_LIBS ${LMGC90_BINDINGS_MODELS_TARGET_LIBS}
                                   lmgc_core_utils
                                   lmgc_core_shared_contribs
                                   lmgc_bindings_sparse_la
                                   ${LAPACK_LIBRARIES}
                                   minpack
                                  )

ADD_LIBRARY(lmgc_core_shared  ${LMGC90_CORE_SHARED_SRCS})

TARGET_LINK_LIBRARIES(lmgc_core_shared ${LMGC90_CORE_SHARED_TARGET_LIBS})
add_dependencies(lmgc_core_utils version_init)
if(LMGC90_BINDINGS_MODELS_DEPS)
  add_dependencies(lmgc_core_shared ${LMGC90_BINDINGS_MODELS_DEPS})
endif(LMGC90_BINDINGS_MODELS_DEPS)


if( NOT ${NO_TEST} )
  # ugly but necessary
  if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lgcc_s")
  endif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")

  add_executable( test_paranoid tst_paranoid_checks.f90)
  target_link_libraries(test_paranoid ${LAPACK_LIBRARIES}
                                     lmgc_core_shared
                                    )
  add_executable( test_algebra tst_algebra.f90)
  target_link_libraries(test_algebra ${LAPACK_LIBRARIES}
                                     lmgc_core_shared
                                     lmgc_exception
                                    )

  add_executable( test_a_MATRIX tst_a_MATRIX.f90)
  target_link_libraries(test_a_MATRIX ${LAPACK_LIBRARIES}
                                      lmgc_core_shared
                                     )
  add_executable( test_case_vs_gmatrix tst_case_vs_gmatrix.f90)
  target_link_libraries(test_case_vs_gmatrix ${LAPACK_LIBRARIES}
                                      lmgc_core_shared
                                     )

  add_executable( test_DiscreteGeometry tst_DiscreteGeometry.f90 )
  target_link_libraries(test_DiscreteGeometry ${LAPACK_LIBRARIES}
                                              lmgc_core_shared
                                             )

  add_executable( test_anonymous_container tst_anonymous_container.f90 )
  target_link_libraries(test_anonymous_container ${LAPACK_LIBRARIES}
                                                 lmgc_core_shared
                                                )

  add_executable( test_anonymous_ptr_container tst_anonymous_ptr_container.f90 )
  target_link_libraries(test_anonymous_ptr_container ${LAPACK_LIBRARIES}
                                                     lmgc_core_shared
                                                    )

  add_executable( test_rough_detections tst_rough_detections.f90 )
  target_link_libraries(test_rough_detections ${LAPACK_LIBRARIES}
                                              lmgc_core_shared
                                             )

  add_executable( test_a_EF tst_a_EF.f90 )
  target_link_libraries(test_a_EF ${LAPACK_LIBRARIES}
                                  lmgc_core_shared
                                  )

  add_test( test_paranoid ${EXECUTABLE_OUTPUT_PATH}/test_paranoid)
  add_test( test_algebra ${EXECUTABLE_OUTPUT_PATH}/test_algebra)
  add_test( test_a_MATRIX ${EXECUTABLE_OUTPUT_PATH}/test_a_MATRIX)
  add_test( test_case_vs_gmatrix ${EXECUTABLE_OUTPUT_PATH}/test_case_vs_gmatrix)
  add_test( test_DiscreteGeometry ${EXECUTABLE_OUTPUT_PATH}/test_DiscreteGeometry)
  add_test( test_anonymous_container ${EXECUTABLE_OUTPUT_PATH}/test_anonymous_container )
  add_test( test_anonymous_ptr_container ${EXECUTABLE_OUTPUT_PATH}/test_anonymous_ptr_container )
  add_test( test_rough_detections ${EXECUTABLE_OUTPUT_PATH}/test_rough_detections)
  add_test( test_a_EF  ${EXECUTABLE_OUTPUT_PATH}/test_a_EF)

  set_property(TARGET test_paranoid PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
  set_property(TARGET test_algebra  PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
  set_property(TARGET test_a_MATRIX PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
  set_property(TARGET test_case_vs_gmatrix PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
  set_property(TARGET test_DiscreteGeometry PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
  set_property(TARGET test_anonymous_container PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
  set_property(TARGET test_anonymous_ptr_container PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
  set_property(TARGET test_rough_detections PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
  set_property(TARGET test_a_EF PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
endif( NOT ${NO_TEST} )

