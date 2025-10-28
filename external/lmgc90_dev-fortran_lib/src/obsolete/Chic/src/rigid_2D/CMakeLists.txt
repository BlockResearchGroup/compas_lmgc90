
SET(LMGC90_CHIC_RIGID2D_SRCS
			wrap_prepro.f90
			wrap_DISKx.f90
			wrap_DISPx.f90
			wrap_JONCx.f90
			wrap_POLYG.f90
			wrap_PT2Dx.f90
			wrap_xKSID.f90
			wrap_xPSID.f90
			wrap_RBDY2.f90)

INCLUDE_DIRECTORIES(${LMGC90_CORE_SHARED_DIR} 
		    ${LMGC90_CHIC_SHARED_DIR}
		    ${LMGC90_BINDINGS_DIR}
		    ${LMGC90_CORE_RIGID2D_DIR})

ADD_LIBRARY(lmgc_chic_rigid_2d ${LMGC90_CHIC_RIGID2D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_chic_rigid_2d lmgc_core_rigid_2d
					 lmgc_chic_shared)	
