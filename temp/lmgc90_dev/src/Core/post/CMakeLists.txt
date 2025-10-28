
SET(LMGC90_CORE_POST2D_SRCS mod_geompack.f90
                            mod_postpro_2D.f90
                            )

SET(LMGC90_CORE_POST3D_SRCS mod_postpro_3D.f90)


INCLUDE_DIRECTORIES( ${LAPACK95_INCLUDE_DIR} )

ADD_LIBRARY(lmgc_core_post_2d ${LMGC90_CORE_POST2D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_core_post_2d lmgc_core_contact_2d
                                        lmgc_core_kernel_2d
                                        ${LAPACK_LIBRARIES}
                                        )


ADD_LIBRARY(lmgc_core_post_3d ${LMGC90_CORE_POST3D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_core_post_3d lmgc_core_contact_3d
                                        lmgc_core_kernel_3d
                                        )

