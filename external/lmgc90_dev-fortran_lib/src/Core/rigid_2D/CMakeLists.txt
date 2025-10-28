
SET(LMGC90_CORE_RIGID2D_SRCS mod_a_BDARY_DISKx.f90
                             mod_a_BDARY_JONCx.f90
                             mod_a_BDARY_POLYG.f90
                             mod_a_BDARY_PT2Dx.f90
                             mod_a_BDARY_xKSID.f90
                             mod_RBDY2_type.f90
                             mod_RBDY2.f90
                            )

INCLUDE_DIRECTORIES(${LAPACK95_INCLUDE_DIR})

ADD_LIBRARY(lmgc_core_rigid_2d ${LMGC90_CORE_RIGID2D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_core_rigid_2d lmgc_core_shared 
                                         ${LAPACK_LIBRARIES}
                                         )
