if(WITH_NEW)
  SET(LMGC90_CORE_PARAMETERS_SRCS mod_integrator_parameters.f90
                                  mod_mdl_hdl_parameters.f90
                                 )
else(WITH_NEW)
  SET(LMGC90_CORE_PARAMETERS_SRCS mod_no_mdl_hdl_parameters.f90)
endif(WITH_NEW)

ADD_LIBRARY(lmgc_core_parameters ${LMGC90_CORE_PARAMETERS_SRCS})
TARGET_LINK_LIBRARIES(lmgc_core_parameters lmgc_core_shared)

