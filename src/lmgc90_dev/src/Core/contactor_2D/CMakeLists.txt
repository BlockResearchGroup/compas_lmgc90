
SET(LMGC90_CORE_CONTACTOR2D_SRCS mod_ALpxx.f90
                                 mod_CLxxx.f90
                                 mod_DISKL.f90
                                 mod_PT2DL.f90
                                 mod_DISKx.f90
                                 mod_JONCx.f90
                                 mod_POLYG.f90
                                 mod_PT2Dx.f90
                                 mod_xKSID.f90
                                )

include_directories( ${LAPACK95_INCLUDE_DIR} )
include_directories( ${TEMPLATE_DIRECTORY} )

add_library(lmgc_core_contactor_2d ${LMGC90_CORE_CONTACTOR2D_SRCS})
target_link_libraries(lmgc_core_contactor_2d lmgc_core_shared
                                             lmgc_core_rigid_2d
                                             lmgc_core_mbs2d
                                             lmgc_core_mailx
                                             ${LAPACK_LIBRARIES}
                                             ) 

