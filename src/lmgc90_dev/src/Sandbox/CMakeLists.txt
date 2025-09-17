
###############################################
####       Setting options for Sandbox     ####
set(CMAKE_C_FLAGS       ${LMGC90_C_FLAGS}      )
set(CMAKE_CXX_FLAGS     ${LMGC90_CXX_FLAGS}    )
set(CMAKE_Fortran_FLAGS ${LMGC90_Fortran_FLAGS})
###############################################

if(${BUILD_STANDALONE})
   ADD_SUBDIRECTORY(Standalone)
endif(${BUILD_STANDALONE})
if(${BUILD_STANDALONE_MPI})
   ADD_SUBDIRECTORY(Standalone_MPI)
endif(${BUILD_STANDALONE_MPI})

if(${BUILD_Fortran_LIB})
  add_subdirectory(meca_lib)
endif(${BUILD_Fortran_LIB})
