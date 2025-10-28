# A FINIR

SET(LMGC90_CHIC_MAILx_LIB_SRCS wrap_ALpxx.f90
			  wrap_ASpxx.f90
			  wrap_CLxxx.f90
			  wrap_CSxxx.f90
			  wrap_DISKL.f90
			  wrap_MAILx.f90
			  wrap_PT2DL.f90
			  wrap_mecaMAILx.f90
			  wrap_therMAILx.f90
			  )

INCLUDE_DIRECTORIES(${LMGC90_CORE_SHARED_DIR}
		    ${LMGC90_CHIC_SHARED_DIR}
		    ${LMGC90_BINDINGS_DIR} 
		    ${LMGC90_CORE_RIGID2D_DIR}
		    ${LMGC90_CORE_MAILx_DIR}	)

add_library(lmgc_chic_mailx STATIC ${LMGC90_CHIC_MAILx_LIB_SRCS})
target_link_libraries(lmgc_chic_mailx lmgc_chic_shared
                                      lmgc_bindings)
