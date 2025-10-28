
SET(LMGC90_PYTHON_IO_SRCS wrap_io_hdf5_hl.f90 )

ADD_LIBRARY(lmgc_python_io ${LMGC90_PYTHON_IO_SRCS})
TARGET_LINK_LIBRARIES(lmgc_python_io lmgc_core_io)

FILE(GLOB _headers ${CMAKE_CURRENT_SOURCE_DIR} wrap_io_hdf5_hl.h)
set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

FILE(GLOB _HEADERS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} wrap_io_hdf5_hl.h)

if(${BUILD_C_LIB})
  FILE(GLOB _sources ${CMAKE_CURRENT_SOURCE_DIR} wrap_io_hdf5_hl.f90)
  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/io
         )
endif(${BUILD_C_LIB})
