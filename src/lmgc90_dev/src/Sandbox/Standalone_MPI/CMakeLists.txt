# creation de l'executable DKDK_standalone_DDM
# am : faire une lib avec le module de decomposition de domaine...
ADD_EXECUTABLE(DKDK_standalone_DDM_MPI DKDK_standalone_DDM_MPI.f90 mod_DDM_MPI_2D.f90)
ADD_EXECUTABLE(SPSP_standalone_DDM_MPI SPSP_standalone_DDM_MPI.f90 mod_DDM_MPI_3D.f90)
ADD_EXECUTABLE(PRPR_standalone_DDM_MPI PRPR_standalone_DDM_MPI.f90 mod_DDM_MPI_3D.f90)

ADD_EXECUTABLE(DKDK_standalone_DDM_MPI_DECENT DKDK_standalone_DDM_MPI_DECENT.f90 mod_DDM_MPI_DECENT_2D.f90)
ADD_EXECUTABLE(SPSP_standalone_DDM_MPI_DECENT SPSP_standalone_DDM_MPI_DECENT.f90 mod_DDM_MPI_DECENT_3D.f90)
ADD_EXECUTABLE(PRPR_standalone_DDM_MPI_DECENT PRPR_standalone_DDM_MPI_DECENT.f90 mod_DDM_MPI_DECENT_3D.f90)

if(${BUILD_MUMPS_PA_ENRICHIE})
   ADD_EXECUTABLE(DKDK_standalone_DDM_MPI_ENRICHIE DKDK_standalone_DDM_MPI_ENRICHIE.f90 mod_DDM_MPI_ENRICHIE_2D.f90)
endif(${BUILD_MUMPS_PA_ENRICHIE})

# dependences pour DKDK_standalone_DDM
SET(DKDK_standalone_DDM_MPI_TARGET_LINK_LIBRARIES 
				lmgc_core_shared
				lmgc_core_mailx
				lmgc_core_rigid_2d 
				lmgc_core_contactor_2d 
				lmgc_core_contact_2d 
				lmgc_core_kernel_2d
				lmgc_core_post_2d
   )

# dependences pour SPSP_standalone_DDM
SET(SPSP_standalone_DDM_MPI_TARGET_LINK_LIBRARIES 
				lmgc_core_shared
				lmgc_core_mailx
				lmgc_core_rigid_3d 
				lmgc_core_contactor_3d 
				lmgc_core_contact_3d 
				lmgc_core_kernel_3d
				lmgc_core_post_3d
   )

# dependences pour PRPR_standalone_DDM
SET(PRPR_standalone_DDM_MPI_TARGET_LINK_LIBRARIES 
				lmgc_core_shared
				lmgc_core_mailx
				lmgc_core_rigid_3d 
				lmgc_core_contactor_3d 
				lmgc_core_contact_3d 
				lmgc_core_kernel_3d
				lmgc_core_post_3d
   )


# dependences pour DKDK_standalone_DDM_DECENT
SET(DKDK_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES 
				lmgc_core_shared
				lmgc_core_mailx
				lmgc_core_rigid_2d 
				lmgc_core_contactor_2d 
				lmgc_core_contact_2d 
				lmgc_core_kernel_2d
				lmgc_core_post_2d
   )

# dependences pour SPSP_standalone_DDM_DECENT
SET(SPSP_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES 
				lmgc_core_shared
				lmgc_core_mailx
				lmgc_core_rigid_3d 
				lmgc_core_contactor_3d 
				lmgc_core_contact_3d 
				lmgc_core_kernel_3d
				lmgc_core_post_3d
   )

# dependences pour PRPR_standalone_DDM_DECENT
SET(PRPR_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES 
				lmgc_core_shared
				lmgc_core_mailx
				lmgc_core_rigid_3d 
				lmgc_core_contactor_3d 
				lmgc_core_contact_3d 
				lmgc_core_kernel_3d
				lmgc_core_post_3d
   )

if(${BUILD_MUMPS_PA_ENRICHIE})
   # dependences pour DKDK_standalone_DDM_ENRICHIE
   SET(DKDK_standalone_DDM_MPI_ENRICHIE_TARGET_LINK_LIBRARIES 
   				lmgc_core_shared
   				lmgc_core_mailx
   				lmgc_core_rigid_2d 
   				lmgc_core_contactor_2d 
   				lmgc_core_contact_2d 
   				lmgc_core_kernel_2d
   				lmgc_core_post_2d
      )
endif(${BUILD_MUMPS_PA_ENRICHIE})

# Lieu ou se trouve les .mod pour les USE de LAPACK95
INCLUDE_DIRECTORIES( ${LAPACK95_INCLUDE_DIR} )

if(${BUILD_MUMPS_PA_ENRICHIE})
  # Lieu ou se trouve les .h pour les USE de MUMPS
  INCLUDE_DIRECTORIES( ${SPARSE_LA_INCLUDE_DIR} )
endif(${BUILD_MUMPS_PA_ENRICHIE})

# si on a compile les external models avec la Matlib, on doit aussi la lier a l'executable Standalone
if(${WITH_MATLIB})
   SET(DKDK_standalone_DDM_MPI_TARGET_LINK_LIBRARIES ${DKDK_standalone_DDM_MPI_TARGET_LINK_LIBRARIES} ${MATLIB_LIBRARY} ${STDC++_LIB})
   SET(SPSP_standalone_DDM_MPI_TARGET_LINK_LIBRARIES ${SPSP_standalone_DDM_MPI_TARGET_LINK_LIBRARIES} ${MATLIB_LIBRARY} ${STDC++_LIB})
   SET(PRPR_standalone_DDM_MPI_TARGET_LINK_LIBRARIES ${PRPR_standalone_DDM_MPI_TARGET_LINK_LIBRARIES} ${MATLIB_LIBRARY} ${STDC++_LIB})
   SET(DKDK_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES ${DKDK_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES} ${MATLIB_LIBRARY} ${STDC++_LIB})
   SET(SPSP_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES ${SPSP_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES} ${MATLIB_LIBRARY} ${STDC++_LIB})
   SET(PRPR_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES ${PRPR_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES} ${MATLIB_LIBRARY} ${STDC++_LIB})
   if(${BUILD_MUMPS_PA_ENRICHIE})
      SET(DKDK_standalone_DDM_MPI_ENRICHIE_TARGET_LINK_LIBRARIES ${DKDK_standalone_DDM_MPI_ENRICHIE_TARGET_LINK_LIBRARIES} ${MATLIB_LIBRARY} ${STDC++_LIB})
   endif(${BUILD_MUMPS_PA_ENRICHIE})
endif(${WITH_MATLIB})

# edition de liens pour DKDK_standalone_DDM
TARGET_LINK_LIBRARIES(DKDK_standalone_DDM_MPI ${DKDK_standalone_DDM_MPI_TARGET_LINK_LIBRARIES}
                                          ${LAPACK_LIBRARIES}
                     )
# edition de liens pour SPSP_standalone_DDM
TARGET_LINK_LIBRARIES(SPSP_standalone_DDM_MPI ${SPSP_standalone_DDM_MPI_TARGET_LINK_LIBRARIES}
                                          ${LAPACK_LIBRARIES}
                     )
# edition de liens pour PRPR_standalone_DDM
TARGET_LINK_LIBRARIES(PRPR_standalone_DDM_MPI ${PRPR_standalone_DDM_MPI_TARGET_LINK_LIBRARIES}
                                          ${LAPACK_LIBRARIES}
                     )

# edition de liens pour DKDK_standalone_DDM_DECENT
TARGET_LINK_LIBRARIES(DKDK_standalone_DDM_MPI_DECENT ${DKDK_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES}
                                          ${LAPACK_LIBRARIES}
                     )
# edition de liens pour SPSP_standalone_DDM_DECENT
TARGET_LINK_LIBRARIES(SPSP_standalone_DDM_MPI_DECENT ${SPSP_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES}
                                          ${LAPACK_LIBRARIES}
                     )
# edition de liens pour PRPR_standalone_DDM_DECENT
TARGET_LINK_LIBRARIES(PRPR_standalone_DDM_MPI_DECENT ${PRPR_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES}
                                          ${LAPACK_LIBRARIES}
                     )

if(${BUILD_MUMPS_PA_ENRICHIE})
   # edition de liens pour DKDK_standalone_DDM_ENRICHIE
   TARGET_LINK_LIBRARIES(DKDK_standalone_DDM_MPI_ENRICHIE ${DKDK_standalone_DDM_MPI_ENRICHIE_TARGET_LINK_LIBRARIES}
                                          ${LAPACK_LIBRARIES} ${SPARSE_LA_LIB}
                     )
endif(${BUILD_MUMPS_PA_ENRICHIE})

if(${BUILD_MUMPS_PA_ENRICHIE})
   #tests pour MUMPS
   add_executable( test_mumps_dense_RHS tst_mumps_dense_RHS.f90)
   target_link_libraries(test_mumps_dense_RHS ${LAPACK_LIBRARIES} ${SPARSE_LA_LIB}
                        )
   add_test( test_mumps_dense_RHS ${EXECUTABLE_OUTPUT_PATH}/test_mumps_dense_RHS)
   
   #sparse RHS
   add_executable( test_mumps_sparse_RHS tst_mumps_sparse_RHS.f90)
   target_link_libraries(test_mumps_sparse_RHS ${LAPACK_LIBRARIES} ${SPARSE_LA_LIB}
                        )
   add_test( test_mumps_sparse_RHS ${EXECUTABLE_OUTPUT_PATH}/test_mumps_sparse_RHS)
endif(${BUILD_MUMPS_PA_ENRICHIE})



# rm's test for use in python
include(${SWIG_USE_FILE})
include_directories(${Python_INCLUDE_DIRS}
                    ${CMAKE_CURRENT_SOURCE_DIR}
                   )

SET(CMAKE_SWIG_FLAGS "") 
    

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/prpr_ddm_mpi_decent.i.in
               ${CMAKE_CURRENT_BINARY_DIR}/prpr_ddm_mpi_decent.i
              )

add_library(PRPR_DDM_MPI_DECENT mod_DDM_MPI_DECENT_3D.f90
                                mod_PRPR_DDM_MPI_DECENT.f90
                                wrap_PRPR_DDM_MPI_DECENT.f90
           )
target_link_libraries(PRPR_DDM_MPI_DECENT 
                      ${PRPR_standalone_DDM_MPI_DECENT_TARGET_LINK_LIBRARIES}
                     )
# defining swig target
SET_SOURCE_FILES_PROPERTIES(${CMAKE_CURRENT_BINARY_DIR}/prpr_ddm_mpi_decent.i PROPERTIES CPLUSPLUS ON )

if( ${CMAKE_VERSION} VERSION_LESS 3.8 )
  SWIG_ADD_MODULE(prpr_ddm_mpi_decent python ${CMAKE_CURRENT_BINARY_DIR}/prpr_ddm_mpi_decent.i)
else( ${CMAKE_VERSION} VERSION_LESS 3.8 )
  SWIG_ADD_LIBRARY(prpr_ddm_mpi_decent LANGUAGE python
                                       SOURCES  ${CMAKE_CURRENT_BINARY_DIR}/prpr_ddm_mpi_decent.i
                  )
endif( ${CMAKE_VERSION} VERSION_LESS 3.8 )

SWIG_LINK_LIBRARIES(prpr_ddm_mpi_decent ${PYTHON_LIBRARIES}
                                        PRPR_DDM_MPI_DECENT
                                        ${LAPACK_LIBRARIES}
                   )
set_property(TARGET ${SWIG_MODULE_prpr_ddm_mpi_decent_REAL_NAME} PROPERTY LINKER_LANGUAGE Fortran)
# ugly but necessary...
if( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
  set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} -nofor-main")
endif( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")

# moving created python module to lib directory
add_custom_command(TARGET ${SWIG_MODULE_prpr_ddm_mpi_decent_REAL_NAME} POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/prpr_ddm_mpi_decent.py 
                                                      ${CMAKE_BINARY_DIR}/lib/prpr_ddm_mpi_decent.py)
 
