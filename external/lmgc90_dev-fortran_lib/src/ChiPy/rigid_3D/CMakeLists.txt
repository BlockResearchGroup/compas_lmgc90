
SET(LMGC90_PYTHON_RIGID3D_SRCS wrap_CYLND.f90
                               wrap_DNLYC.f90
                               wrap_PLANx.f90
                               wrap_POLYR.f90
                               wrap_PT3Dx.f90
                               wrap_RBDY3.f90
                               wrap_SPHER.f90
                              )


INCLUDE_DIRECTORIES( ${LAPACK95_INCLUDE_DIR} )
ADD_LIBRARY(lmgc_python_rigid_3d ${LMGC90_PYTHON_RIGID3D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_python_rigid_3d lmgc_core_rigid_3d
                                           lmgc_core_contactor_3d
                                           ${LAPACK_LIBRARIES}
                                          )

FILE(GLOB _headers ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.h)
set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

FILE(GLOB _HEADERS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.h)

if(${BUILD_C_LIB})
  FILE(GLOB _sources ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.f90)
  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/rigid_3D
         )
endif(${BUILD_C_LIB})
