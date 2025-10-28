
SET(LMGC90_CORE_CONTACTOR3D_SRCS mod_xSpxx.f90
                                 mod_ASpxx.f90
                                 mod_CSxxx.f90
                                 mod_CYLND.f90
                                 mod_DNLYC.f90
                                 mod_PLANx.f90
                                 mod_POLYR.f90
                                 mod_PT3Dx.f90
                                 mod_SPHER.f90
                                )

INCLUDE_DIRECTORIES( ${LAPACK95_INCLUDE_DIR} )
ADD_LIBRARY(lmgc_core_contactor_3d ${LMGC90_CORE_CONTACTOR3D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_core_contactor_3d lmgc_core_shared
                                             lmgc_core_rigid_3d
                                             lmgc_core_mailx
                                             lmgc_core_mbs3d
                                             ${LAPACK_LIBRARIES}
                                             ) 

