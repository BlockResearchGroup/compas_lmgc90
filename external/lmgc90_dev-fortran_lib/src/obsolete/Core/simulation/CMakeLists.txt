
  SET(LMGC90_CORE_SIMULATION_SRCS mod_modelization.f90
                                  mod_state.f90
                                  mod_integrator.f90
                                  mod_integrator_list.f90
                                  mod_soe.f90
                                  mod_model_handler.f90
                                  mod_contactor.f90
                                  mod_interaction_handler.f90
                                  mod_nlgs_new_arch.f90
                                 )

INCLUDE_DIRECTORIES( ${LAPACK95_INCLUDE_DIR} )
INCLUDE_DIRECTORIES( ${TEMPLATE_DIRECTORY} )

ADD_LIBRARY(lmgc_core_simulation ${LMGC90_CORE_SIMULATION_SRCS})
TARGET_LINK_LIBRARIES(lmgc_core_simulation lmgc_core_shared
                                           lmgc_core_parameters
                                           lmgc_core_rigid_2d
                                           lmgc_core_rigid_3d
                                           lmgc_core_mailx
                                           lmgc_core_contact_2d
                                           lmgc_core_contactor_2d
                                           lmgc_core_contactor_3d
                                          )
