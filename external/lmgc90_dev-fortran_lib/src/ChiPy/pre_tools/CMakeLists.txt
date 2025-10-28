
set(LMGC90_PYTHON_PRE_TOOLS_SRCS wrap_deposit2D.f90
                                 wrap_deposit3D.f90
                                 wrap_cut2D.f90
                                 wrap_mesh2D.f90
                                 wrap_surface_T3.f90
                                )

add_library(lmgc_python_pre_tools ${LMGC90_PYTHON_PRE_TOOLS_SRCS})
target_link_libraries(lmgc_python_pre_tools lmgc_core_pre_tools)

FILE(GLOB _headers ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.h)
set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

FILE(GLOB _HEADERS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.h)
