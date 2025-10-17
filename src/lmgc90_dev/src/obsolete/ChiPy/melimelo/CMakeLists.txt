
SET(LMGC90_PYTHON_MELIMELO_SRCS wrap_melimelo.f90
                                wrap_visitor.f90
                               )

ADD_LIBRARY(lmgc_python_melimelo ${LMGC90_PYTHON_MELIMELO_SRCS})
TARGET_LINK_LIBRARIES(lmgc_python_melimelo lmgc_core_melimelo)

FILE(GLOB _headers ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.h)
set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

FILE(GLOB _HEADERS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.h)

if(BUILD_C_LIB)
  FILE(GLOB _sources ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.f90)
  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/melimelo
         )
endif(BUILD_C_LIB)
