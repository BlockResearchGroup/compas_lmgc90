
SET(LMGC90_CHIC_RIGID3D_SRCS
			wrap_CYLND.f90
			wrap_DNLYC.f90
			wrap_PLANx.f90
			wrap_POLYR.f90
			wrap_PT3Dx.f90
			wrap_RBDY3.f90
			wrap_SPHER.f90)

INCLUDE_DIRECTORIES(${LMGC90_External_Linear_algebra_dir}
		    ${LMGC90_CORE_SHARED_DIR}
		    ${LMGC90_CHIC_SHARED_DIR}
		    ${LMGC90_BINDINGS_DIR}
		    ${LMGC90_CORE_RIGID3D_DIR}
		    ${LMGC90_CHIC_RIGID3D_DIR})

ADD_LIBRARY(lmgc_chic_rigid_3d ${LMGC90_CHIC_RIGID3D_SRCS})
target_link_libraries(lmgc_chic_rigid_3d lmgc_core_rigid_3d lmgc_chic_shared)
