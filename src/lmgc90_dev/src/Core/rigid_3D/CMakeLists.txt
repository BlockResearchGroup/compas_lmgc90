
SET(LMGC90_CORE_RIGID3D_SRCS mod_a_BDARY_CYLND.f90
                             mod_a_BDARY_DNLYC.f90
                             mod_a_BDARY_PLANx.f90
                             mod_a_BDARY_POLYR.f90
                             mod_a_BDARY_PT3Dx.f90
                             mod_a_BDARY_SPHER.f90
                             mod_RBDY3_type.f90
                             mod_RBDY3.f90
                            )

INCLUDE_DIRECTORIES(${LAPACK95_INCLUDE_DIR})

ADD_LIBRARY(lmgc_core_rigid_3d ${LMGC90_CORE_RIGID3D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_core_rigid_3d lmgc_core_shared
                                         ${LAPACK_LIBRARIES}
                                         )

