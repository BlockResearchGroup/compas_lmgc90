
set(LMGC90_CORE_CONTACT2D_SRCS mod_inter_meca2D.f90
                               mod_CLALp.f90
                               mod_CLJCx.f90
                               mod_DKALp.f90
                               mod_DKDKL.f90
                               mod_DKDKx.f90
                               mod_DKJCx.f90
                               mod_DKKDx.f90
                               mod_DKPLx.f90
                               mod_P2P2L.f90
                               mod_PLALp.f90
                               mod_PLJCx.f90
                               mod_PLPLx.f90
                               mod_PTPT2.f90
                               mod_inter_handler_2D.f90
                              )

include_directories( ${LAPACK95_INCLUDE_DIR} )
include_directories( ${TEMPLATE_DIRECTORY} )

add_library(lmgc_core_contact_2d ${LMGC90_CORE_CONTACT2D_SRCS})
target_link_libraries(lmgc_core_contact_2d lmgc_core_mailx
                                           lmgc_core_rigid_2d
                                           lmgc_core_contactor_2d
                                           lmgc_bindings_detection
                                           ${LAPACK_LIBRARIES}
                                          )

