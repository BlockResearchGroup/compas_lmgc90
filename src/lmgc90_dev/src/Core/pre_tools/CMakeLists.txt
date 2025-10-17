
SET(LMGC90_CORE_PRE_TOOLS_SRCS mod_deposit2D.f90
                               mod_deposit3D.f90
                               mod_cut2D.f90
                               mod_mesh2D.f90
                               mod_a_surface_T3.f90
                              )

INCLUDE_DIRECTORIES( ${LAPACK95_INCLUDE_DIR} )
ADD_LIBRARY(lmgc_core_pre_tools ${LMGC90_CORE_PRE_TOOLS_SRCS})
TARGET_LINK_LIBRARIES(lmgc_core_pre_tools lmgc_core_shared
                                          ${LAPACK_LIBRARIES}
                                         )

