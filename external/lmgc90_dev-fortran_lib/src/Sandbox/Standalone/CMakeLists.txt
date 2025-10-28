# creation de l'executable DKJC_standalone
ADD_EXECUTABLE(DKJC_standalone DKJC_standalone.f90)

# creation de l'executable DKDK_standalone_DDM
# am : faire une lib avec le module de decomposition de domaine...
ADD_EXECUTABLE(DKDK_standalone_DDM_MDS DKDK_standalone_DDM_MDS.f90 mod_DDM_MDS_2D.f90)

# creation de l'executable SPSP_standalone
ADD_EXECUTABLE(SPSP_standalone SPSP_standalone.f90)

# creation de l'executable PRPR_standalone
ADD_EXECUTABLE(PRPR_standalone PRPR_standalone.f90)

# creation de l'executable SPSP_standalone_DDM
ADD_EXECUTABLE(SPSP_standalone_DDM SPSP_standalone_DDM.f90)

# creation de l'executable PRPR_standalone_DDM
ADD_EXECUTABLE(PRPR_standalone_DDM PRPR_standalone_DDM.f90)

# bibliotheque pour la DDM 3D
add_library(ddm_3d mod_DDM_3D.f90)

add_executable(lmgc90_exec stand_alone.f90)
set_property(TARGET lmgc90_exec PROPERTY OUTPUT_NAME lmgc90)

target_link_libraries(lmgc90_exec lmgc_core_kernel_2d
                                  lmgc_core_kernel_3d
                                  lmgc_core_post_2d
                                  lmgc_core_post_3d
                                  )

# dependences pour DKJC_standalone
SET(DKJC_standalone_TARGET_LINK_LIBRARIES 
				lmgc_core_shared
				lmgc_core_mailx
				lmgc_core_rigid_2d 
				lmgc_core_contactor_2d 
				lmgc_core_contact_2d 
				lmgc_core_kernel_2d
				lmgc_core_post_2d
   )

# dependences pour DKDK_standalone_DDM
SET(DKDK_standalone_DDM_MDS_TARGET_LINK_LIBRARIES 
				lmgc_core_shared
				lmgc_core_mailx
				lmgc_core_rigid_2d 
				lmgc_core_contactor_2d 
				lmgc_core_contact_2d 
				lmgc_core_kernel_2d
				lmgc_core_post_2d
   )

# dependences pour SPSP_standalone
SET(SPSP_standalone_TARGET_LINK_LIBRARIES 
				lmgc_core_shared
				lmgc_core_mailx
				lmgc_core_rigid_3d 
				lmgc_core_contactor_3d 
				lmgc_core_contact_3d 
				lmgc_core_kernel_3d
				lmgc_core_post_3d
   )

# dependences pour PRPR_standalone
SET(PRPR_standalone_TARGET_LINK_LIBRARIES 
				lmgc_core_shared
				lmgc_core_mailx
				lmgc_core_rigid_3d 
				lmgc_core_contactor_3d 
				lmgc_core_contact_3d 
				lmgc_core_kernel_3d
				lmgc_core_post_3d
   )

# dependences pour SPSP_standalone_DDM
SET(SPSP_standalone_DDM_TARGET_LINK_LIBRARIES 
				lmgc_core_shared
				lmgc_core_mailx
				lmgc_core_rigid_3d 
				lmgc_core_contactor_3d 
				lmgc_core_contact_3d 
				lmgc_core_kernel_3d
				lmgc_core_post_3d
				ddm_3d
   )

# dependences pour PRPR_standalone_DDM
SET(PRPR_standalone_DDM_TARGET_LINK_LIBRARIES 
				lmgc_core_shared
				lmgc_core_mailx
				lmgc_core_rigid_3d 
				lmgc_core_contactor_3d 
				lmgc_core_contact_3d 
				lmgc_core_kernel_3d
				lmgc_core_post_3d
				ddm_3d
   )



target_link_libraries(DKJC_standalone ${DKJC_standalone_TARGET_LINK_LIBRARIES})
target_link_libraries(SPSP_standalone ${SPSP_standalone_TARGET_LINK_LIBRARIES})
target_link_libraries(PRPR_standalone ${PRPR_standalone_TARGET_LINK_LIBRARIES})
target_link_libraries(DKDK_standalone_DDM_MDS ${DKDK_standalone_DDM_MDS_TARGET_LINK_LIBRARIES})
target_link_libraries(SPSP_standalone_DDM ${SPSP_standalone_DDM_TARGET_LINK_LIBRARIES})
target_link_libraries(PRPR_standalone_DDM ${PRPR_standalone_DDM_TARGET_LINK_LIBRARIES})

target_link_libraries(ddm_3d lmgc_core_shared
                             lmgc_core_rigid_3d
                             lmgc_core_contact_3d
                             lmgc_core_kernel_3d
                     )


# set Fortran as the linker language for every standalone
set( list_of_standalone DKJC_standalone 
                        DKDK_standalone_DDM_MDS
                        SPSP_standalone
                        PRPR_standalone
                        SPSP_standalone_DDM
                        PRPR_standalone_DDM
                        lmgc90_exec
   )
foreach( t ${list_of_standalone} )
  set_property(TARGET ${t} PROPERTY LINKER_LANGUAGE Fortran)
endforeach( t )
install(TARGETS lmgc90_exec DESTINATION bin)
