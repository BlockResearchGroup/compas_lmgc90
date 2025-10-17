SET(LMGC90_CHIC_SHARED_LIB_SRCS mod_chic.f90
		    wrap_overall.f90
		    wrap_bulk_behav.f90
		    wrap_models.f90
		    wrap_tact_behav.f90
		    wrap_timer.f90
		   )


INCLUDE_DIRECTORIES(${LMGC90_External_Linear_algebra_dir}
		    ${LMGC90_CORE_SHARED_DIR}
		    ${LMGC90_BINDINGS_DIR})
ADD_LIBRARY(lmgc_chic_shared  ${LMGC90_CHIC_SHARED_LIB_SRCS})
TARGET_LINK_LIBRARIES(lmgc_chic_shared lmgc_core_shared)
