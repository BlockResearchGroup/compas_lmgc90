

SET(LMGC90_PYTHON_POST2D_SRCS wrap_postpro.f90)
SET(LMGC90_PYTHON_POST3D_SRCS wrap_postpro_3D.f90
                             )

INCLUDE_DIRECTORIES( ${LAPACK95_INCLUDE_DIR} )

ADD_LIBRARY(lmgc_python_post_2d ${LMGC90_PYTHON_POST2D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_python_post_2d lmgc_core_post_2d
                                          ${LAPACK_LIBRARIES}
                                         )

ADD_LIBRARY(lmgc_python_post_3d ${LMGC90_PYTHON_POST3D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_python_post_3d lmgc_core_post_3d
                                          ${LAPACK_LIBRARIES}
                                         )

FILE(GLOB _headers ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.h)
set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

FILE(GLOB _HEADERS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.h)

if(${BUILD_C_LIB})
  FILE(GLOB _sources ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.f90)
  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/post
         )
endif(${BUILD_C_LIB})
