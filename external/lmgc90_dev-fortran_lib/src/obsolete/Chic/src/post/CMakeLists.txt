

SET(LMGC90_CHIC_POST2D_SRCS
		       wrap_postpro.f90
		       wrap_post2D.f90)
SET(LMGC90_CHIC_POST3D_SRCS 
		       wrap_postpro_3D.f90
		       wrap_post3D.f90)
#if(WITH_VTK)
  SET(LMGC90_CHIC_POST3D_SRCS   wrap_display_3D.f90 ${LMGC90_CHIC_POST3D_SRCS}) 
#else(WITH_VTK)
#  SET(LMGC90_CHIC_POST3D_SRCS   wrap_display_3D_without_vtk.f90 ${LMGC90_CHIC_POST3D_SRCS}) 
#endif(WITH_VTK)

INCLUDE_DIRECTORIES(${LMGC90_CORE_SHARED_DIR}
		    ${LMGC90_CORE_POST_DIR}
		    ${LMGC90_CHIC_SHARED_DIR}
		    ${LMGC90_CORE_RIGID2D_DIR}
		    ${LMGC90_CORE_CONTACT2D_DIR}
		    ${LMGC90_CORE_MAILx_DIR}
		    ${LMGC90_CORE_KERNEL_DIR})

ADD_LIBRARY(lmgc_chic_post_2d ${LMGC90_CHIC_POST2D_SRCS})
			
TARGET_LINK_LIBRARIES(lmgc_chic_post_2d lmgc_core_post_2d)
ADD_LIBRARY(lmgc_chic_post_3d ${LMGC90_CHIC_POST3D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_chic_post_3d lmgc_core_post_3d
					lmgc_core_kernel_3d
					lmgc_chic_kernel_3d)
