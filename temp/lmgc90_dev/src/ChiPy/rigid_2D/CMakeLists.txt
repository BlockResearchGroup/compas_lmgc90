
SET(LMGC90_PYTHON_RIGID2D_SRCS wrap_DISKx.f90
                               wrap_JONCx.f90
                               wrap_POLYG.f90
                               wrap_PT2Dx.f90
                               wrap_xKSID.f90
                               wrap_RBDY2.f90
                              )

INCLUDE_DIRECTORIES( ${LAPACK95_INCLUDE_DIR} )
ADD_LIBRARY(lmgc_python_rigid_2d ${LMGC90_PYTHON_RIGID2D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_python_rigid_2d lmgc_core_rigid_2d
                                           lmgc_core_contactor_2d
                                           ${LAPACK_LIBRARIES}
                                          )

FILE(GLOB _headers ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.h)
set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

FILE(GLOB _HEADERS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.h)

if(${BUILD_C_LIB})
  FILE(GLOB _sources ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.f90)
  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/rigid_2D
         )
endif(${BUILD_C_LIB})
