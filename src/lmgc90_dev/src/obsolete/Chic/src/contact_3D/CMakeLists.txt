
SET(LMGC90_CHIC_CONTACT3D_SRCS 
			wrap_CSPRx.f90
			wrap_CSASp.f90
			#mod_PRASp.f90
			wrap_PRPLx.f90
			wrap_PRPRx.f90
			wrap_PTPT3.f90
			wrap_SPCDx.f90
			wrap_SPDCx.f90
			wrap_SPPLx.f90
			wrap_SPSPx.f90
			wrap_CDCDx.f90
			wrap_CDPLx.f90)

INCLUDE_DIRECTORIES(${LMGC90_CORE_SHARED_DIR}
	            ${LMGC90_CHIC_SHARED_DIR} 
                    ${LMGC90_CHIC_CONTACT3D_DIR}
                    ${LMGC90_CORE_CONTACT3D_DIR}
		    ${LMGC90_BINDINGS_DIR}
		    ${LMGC90_CORE_RIGID3D_DIR}
		    ${LMGC90_CORE_MAILx_DIR})

ADD_LIBRARY(lmgc_chic_contact_3d ${LMGC90_CHIC_CONTACT3D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_chic_contact_3d  lmgc_core_rigid_3d lmgc_core_contact_3d lmgc_chic_shared)
			
