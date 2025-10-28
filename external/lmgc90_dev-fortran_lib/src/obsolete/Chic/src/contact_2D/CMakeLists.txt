

SET(LMGC90_CHIC_CONTACT2D_SRCS wrap_CLALp.f90
			wrap_CLJCx.f90
			wrap_DKALp.f90
			wrap_DKDKL.f90
			wrap_DKDKx.f90
			wrap_DKDPx.f90
			wrap_DKJCx.f90
			wrap_DKKDx.f90
			wrap_DKPDx.f90
			wrap_DKPLx.f90
			wrap_DPALp.f90
			wrap_P2P2L.f90
			wrap_PLALp.f90
			wrap_PLJCx.f90
			wrap_PLPLx.f90	
			wrap_PTPT2.f90)

INCLUDE_DIRECTORIES(${LMGC90_CORE_SHARED_DIR} 
		    ${LMGC90_CHIC_SHARED_DIR} 
		    ${LMGC90_BINDINGS_DIR}
		    ${LMGC90_CORE_RIGID2D_DIR}
		    ${LMGC90_MAILx_DIR}
		    ${LMGC90_CORE_CONTACT2D_DIR})

ADD_LIBRARY(lmgc_chic_contact_2d ${LMGC90_CHIC_CONTACT2D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_chic_contact_2d lmgc_core_contact_2d lmgc_chic_shared)
