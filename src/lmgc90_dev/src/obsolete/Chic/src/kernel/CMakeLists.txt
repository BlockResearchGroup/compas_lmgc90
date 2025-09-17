

SET(LMGC90_CHIC_KERNEL2_SRCS wrap_nlgs.f90
			wrap_cpg.f90
			wrap_mp_solver.f90
			)
SET(LMGC90_CHIC_KERNEL3_SRCS wrap_nlgs_3D.f90
			wrap_cpg_3D.f90
			wrap_mp_solver_3D.f90
			)

INCLUDE_DIRECTORIES(${LMGC90_CORE_SHARED_DIR}
	            ${LMGC90_CHIC_SHARED_DIR}
		    ${LMGC90_CORE_RIGID2D_DIR}
		    ${LMGC90_CORE_CONTACT2D_DIR}
		    ${LMGC90_CORE_KERNEL_DIR}
		    ${LMGC90_External_Linear_algebra_dir})

ADD_LIBRARY(lmgc_chic_kernel_2d ${LMGC90_CHIC_KERNEL2_SRCS})
		
TARGET_LINK_LIBRARIES(lmgc_chic_kernel_2d lmgc_chic_shared lmgc_core_kernel_2d)

ADD_LIBRARY(lmgc_chic_kernel_3d ${LMGC90_CHIC_KERNEL3_SRCS})
			
TARGET_LINK_LIBRARIES(lmgc_chic_kernel_3d lmgc_chic_shared lmgc_core_kernel_2d)
