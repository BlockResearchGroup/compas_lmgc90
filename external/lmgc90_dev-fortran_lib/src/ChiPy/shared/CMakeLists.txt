SET(LMGC90_PYTHON_SHARED_SRCS wrap_ExternalModels.f90
                              wrap_overall.f90
                              wrap_bulk_behav.f90
                              wrap_models.f90
                              wrap_tact_behav.f90
                              wrap_timer.f90
                              wrap_utilities.f90
                              wrap_a_EF.f90
                              wrap_meca_polygon.f90
                              wrap_parameters.f90
                             )

ADD_LIBRARY(lmgc_python_shared  ${LMGC90_PYTHON_SHARED_SRCS})
TARGET_LINK_LIBRARIES(lmgc_python_shared lmgc_core_shared )

FILE(GLOB _headers ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.h)
set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

FILE(GLOB _HEADERS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.h)

if(${BUILD_C_LIB})
  FILE(GLOB _sources ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.f90)
  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/shared
         )
endif(${BUILD_C_LIB})
