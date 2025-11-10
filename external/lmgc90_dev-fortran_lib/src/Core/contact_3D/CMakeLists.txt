
set(LMGC90_CORE_CONTACT3D_SRCS 
                 mod_inter_meca3D.f90
                 mod_CSPRx.f90
                 mod_CSASp.f90
                 mod_PRASp.f90
                 mod_PRPLx.f90
                 mod_PRPRx.f90
                 mod_PTPT3.f90
                 mod_SPCDx.f90
                 mod_SPDCx.f90
                 mod_SPPLx.f90
                 mod_SPSPx.f90
                 mod_CDCDx.f90
                 mod_CDPLx.f90
                 mod_SPPRx.f90
                 mod_inter_handler_3D.f90
                )

include_directories( ${LAPACK95_INCLUDE_DIR} )
include_directories( ${TEMPLATE_DIRECTORY} )

add_library(lmgc_core_contact_3d ${LMGC90_CORE_CONTACT3D_SRCS})
target_link_libraries(lmgc_core_contact_3d lmgc_bindings_detection
                                           lmgc_core_mailx
                                           lmgc_core_rigid_3d
                                           lmgc_core_contactor_3d
                                           ${LAPACK_LIBRARIES}
                                          )

