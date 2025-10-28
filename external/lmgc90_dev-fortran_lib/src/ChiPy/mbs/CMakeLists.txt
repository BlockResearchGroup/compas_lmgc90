
set(LMGC90_PYTHON_MBS2D_SRCS wrap_mbs2D.f90
                             )

add_library(lmgc_python_mbs2d ${LMGC90_PYTHON_MBS2D_SRCS})
target_link_libraries(lmgc_python_mbs2d lmgc_core_mbs2d
                                        lmgc_core_contactor_2d
                     )

set(LMGC90_PYTHON_MBS3D_SRCS wrap_mbs3D.f90
                             )

add_library(lmgc_python_mbs3d ${LMGC90_PYTHON_MBS3D_SRCS})
target_link_libraries(lmgc_python_mbs3d lmgc_core_mbs3d
                                        lmgc_core_contactor_3d
                     )


file(GLOB _headers ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.h)
set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

file(GLOB _HEADERS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.h)

if(${BUILD_C_LIB})
  file(GLOB _sources ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.f90)
  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/rigid_3D
         )
endif(${BUILD_C_LIB})
