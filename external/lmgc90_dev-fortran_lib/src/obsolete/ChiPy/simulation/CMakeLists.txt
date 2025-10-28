
SET(LMGC90_PYTHON_SIMULATION_SRCS wrap_model_handler.f90
                                  wrap_interaction_handler.f90
                                  wrap_integrator.f90
                                  wrap_soe.f90
                                  wrap_nlgs_newarch.f90
                                 )

# ?
INCLUDE_DIRECTORIES( ${LAPACK95_INCLUDE_DIR} )

ADD_LIBRARY(lmgc_python_simulation ${LMGC90_PYTHON_SIMULATION_SRCS})
TARGET_LINK_LIBRARIES(lmgc_python_simulation lmgc_core_simulation)

#FILE(GLOB _headers ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.h)
SET(_headers ${CMAKE_CURRENT_SOURCE_DIR}/wrap_model_handler.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_interaction_handler.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_integrator.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_soe.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_nlgs_newarch.h
            )
set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

#FILE(GLOB _HEADERS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.h)
SET(_HEADERS wrap_model_handler.h
             wrap_integrator.h
             wrap_soe.h
            )

if(BUILD_C_LIB)
  #FILE(GLOB _sources ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.f90)
  set(_sources ${CMAKE_CURRENT_SOURCE_DIR}/wrap_model_handler.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_interaction_handler.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_integrator.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_soe.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_nlgs_newarch.f90
              )
  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/simulation
         )
endif(BUILD_C_LIB)
