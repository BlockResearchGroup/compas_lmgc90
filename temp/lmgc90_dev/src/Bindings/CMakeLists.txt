################################################
####     Setting options for Bindings       ####
set(CMAKE_C_FLAGS       ${LMGC90_C_FLAGS}      )
set(CMAKE_CXX_FLAGS     ${LMGC90_CXX_FLAGS}    )
set(CMAKE_Fortran_FLAGS ${LMGC90_Fortran_FLAGS})
################################################


unset(LMGC90_BINDINGS_MODELS_SRCS)
unset(LMGC90_BINDINGS_MODELS_TARGET_LIBS)
unset(LMGC90_BINDINGS_MODELS_DEPS)
unset(LMGC90_BINDINGS_MODELS_INCLUDE)

# Demmefi settings
set(DEMMEFI_SRCS ${CMAKE_CURRENT_SOURCE_DIR}/Demmefi_ExternalModels.f90)
list(APPEND LMGC90_BINDINGS_MODELS_SRCS ${DEMMEFI_SRCS})
list(APPEND LMGC90_BINDINGS_MODELS_TARGET_LIBS demmefi)

# MatLib settings
if(${MATLIB_VERSION} STREQUAL "none")

    message(STATUS "No external models library used")
    set(LMGC90_BINDINGS_MATLIB_SRCS ${CMAKE_CURRENT_SOURCE_DIR}/noMatLib_ExternalModels.f90)

else()

  if(${MATLIB_VERSION} STREQUAL "v3")
    set(LMGC90_BINDINGS_MATLIB_SRCS ${CMAKE_CURRENT_SOURCE_DIR}/MatLib_ExternalModels_v3.f90)
  else()
    set(LMGC90_BINDINGS_MATLIB_SRCS ${CMAKE_CURRENT_SOURCE_DIR}/MatLib_ExternalModels_v4.f90)
  endif()

  if(${BUILD_MATLIB})
    set(LMGC90_BINDINGS_MATLIB_DEPS matlib)
  endif(${BUILD_MATLIB})

  if(DEFINED MATLIB_LIBRARY)
    message(STATUS "Matlib Library used: " ${MATLIB_LIBRARY})
    set(LMGC90_BINDINGS_MATLIB_TARGET_LIBS ${MATLIB_LIBRARY})
  else(DEFINED MATLIB_LIBRARY)
    set(LMGC90_BINDINGS_MATLIB_TARGET_LIBS matlib)
  endif(DEFINED MATLIB_LIBRARY)

endif()

list(APPEND LMGC90_BINDINGS_MODELS_SRCS        ${LMGC90_BINDINGS_MATLIB_SRCS})
list(APPEND LMGC90_BINDINGS_MODELS_TARGET_LIBS ${LMGC90_BINDINGS_MATLIB_TARGET_LIBS})
list(APPEND LMGC90_BINDINGS_MODELS_DEPS        ${LMGC90_BINDINGS_MATLIB_DEPS})

# setting external models sources/libraries/deps/include to parent scope
set(LMGC90_BINDINGS_MODELS_SRCS        ${LMGC90_BINDINGS_MODELS_SRCS}        PARENT_SCOPE)
set(LMGC90_BINDINGS_MODELS_TARGET_LIBS ${LMGC90_BINDINGS_MODELS_TARGET_LIBS} PARENT_SCOPE)
set(LMGC90_BINDINGS_MODELS_DEPS        ${LMGC90_BINDINGS_MODELS_DEPS}        PARENT_SCOPE)
set(LMGC90_BINDINGS_MODELS_INCLUDE     ${LMGC90_BINDINGS_MODELS_INCLUDE}     PARENT_SCOPE)

# Compiling ExternalDetection library :
if( EXT_DETECTION )

  message(STATUS "External detection library include path found: ${DETECTION_INCLUDE_DIR}")
  message(STATUS "External detection library libraries found: ${DETECTION_LIBRARIES}")
  message(STATUS "External detection library binding found: ${LMGC90_BINDINGS_DETECTION_SRC}")

  include_directories( ${DETECTION_INCLUDE_DIR} )
  add_library(lmgc_bindings_detection ${LMGC90_BINDINGS_DETECTION_SRC})
  target_link_libraries(lmgc_bindings_detection ${DETECTION_LIBRARIES} lmgc_core_shared)

else( EXT_DETECTION )

  set(LMGC90_BINDINGS_DETECTION_SRC ${CMAKE_CURRENT_SOURCE_DIR}/Std_ExternalDetection.f90)
  set(LMGC90_BINDINGS_DETECTION_TARGET_LIBS)
  add_library(lmgc_bindings_detection ${LMGC90_BINDINGS_DETECTION_SRC})
  target_link_libraries(lmgc_bindings_detection ${LMGC90_BINDINGS_DETECTION_TARGET_LIBS}
                                                lmgc_core_shared                        )

endif( EXT_DETECTION )

# Compiling User library :
set(LMGC90_BINDINGS_USER_SRCS ${CMAKE_CURRENT_SOURCE_DIR}/User.f90)
set(LMGC90_BINDINGS_USER_TARGET_LIBS ${LAPACK_LIBRARIES})
add_library(lmgc_bindings_user ${LMGC90_BINDINGS_USER_SRCS})
target_link_libraries(lmgc_bindings_user ${LMGC90_BINDINGS_USER_TARGET_LIBS})

####################################################################################
# Compiling Sparse ExternalLinearAlgebra library :
####################################################################################
if(${SPARSE_LIBRARY} STREQUAL "none") # no sparse algebra library

    message(STATUS "No sparse linear algebra library used")
    set(LMGC90_BINDINGS_SPARSE_LA_SRCS ${CMAKE_CURRENT_SOURCE_DIR}/Std_SparseLinearAlgebra.f90)
    add_library(lmgc_bindings_sparse_la ${LMGC90_BINDINGS_SPARSE_LA_SRCS})

elseif(${SPARSE_LIBRARY} STREQUAL "mumps")

    # some sanity check regarding mumps+openmp
    if( ${WITH_OPENMP} AND NOT ${BUILD_MUMPS} )
      message(FATAL_ERROR "Cannot use openMP without building our own mumps...
  either build our mumps by setting -DBUILD_MUMPS=ON 
  or deactivate the openmp flag by setting -DWITH_OPENMP=OFF")
    endif( ${WITH_OPENMP} AND NOT ${BUILD_MUMPS} )
    # find corresponding module
    set(LMGC90_BINDINGS_SPARSE_LA_SRCS "${CMAKE_SOURCE_DIR}/src/contribs/Mumps_SparseLinearAlgebra.f90")
    message(STATUS "Sparse linear algebra include path found: ${SPARSE_INCLUDE_DIR}")
    message(STATUS "Sparse linear algebra libraries found: ${SPARSE_LIBRARIES}")
    message(STATUS "Sparse linear algebra binding found: ${LMGC90_BINDINGS_SPARSE_LA_SRCS}")
    message(STATUS "Sparse linear algebra dependencies : ${SPARSE_DEPENDENCIES}")

    include_directories( ${SPARSE_INCLUDE_DIR} )
    add_library(lmgc_bindings_sparse_la ${LMGC90_BINDINGS_SPARSE_LA_SRCS})
    target_link_libraries(lmgc_bindings_sparse_la lmgc_core_utils ${SPARSE_LIBRARIES})
    add_dependencies(lmgc_bindings_sparse_la ${SPARSE_DEPENDENCIES})

elseif(${SPARSE_LIBRARY} STREQUAL "umfpack")

    # find corresponding module
    set(LMGC90_BINDINGS_SPARSE_LA_SRCS "${CMAKE_SOURCE_DIR}/src/contribs/UMFPack_SparseLinearAlgebra.f90")
    message(STATUS "Sparse linear algebra include path found: ${SPARSE_INCLUDE_DIR}")
    message(STATUS "Sparse linear algebra libraries found: ${SPARSE_LIBRARIES}")
    message(STATUS "Sparse linear algebra binding found: ${LMGC90_BINDINGS_SPARSE_LA_SRCS}")

    include_directories( ${SPARSE_INCLUDE_DIR} )
    add_library(lmgc_bindings_sparse_la ${LMGC90_BINDINGS_SPARSE_LA_SRCS})
    target_link_libraries(lmgc_bindings_sparse_la ${SPARSE_LIBRARIES})

endif()
    

# Compiling ExternalFEM library :
if(EXT_FEM_VERSION STREQUAL "none")
  set(LMGC90_BINDINGS_FEM_SRCS ${CMAKE_CURRENT_SOURCE_DIR}/Std_ExternalFEM.f90)
  set(LMGC90_BINDINGS_FEM_TARGET_LIBS)
  add_library(lmgc_bindings_FEM ${LMGC90_BINDINGS_FEM_SRCS})
  target_link_libraries(lmgc_bindings_FEM ${LMGC90_BINDINGS_FEM_TARGET_LIBS})
else()
  FIND_PACKAGE(ExternalFEM_FOR_LMGC REQUIRED)
  message(STATUS "EXT_FEM Library used  : ${EXT_FEM_LIBRARY}")
  message(STATUS "EXT_FEM binding found : ${EXT_FEM_F90_MODULE}")
  set(LMGC90_BINDINGS_FEM_SRCS ${EXT_FEM_F90_MODULE})
  set(LMGC90_BINDINGS_FEM_TARGET_LIBS ${EXT_FEM_LIBRARY})
  add_library(lmgc_bindings_FEM ${LMGC90_BINDINGS_FEM_SRCS})
  target_link_libraries(lmgc_bindings_FEM ${LMGC90_BINDINGS_FEM_TARGET_LIBS})

  message(STATUS "EXT_FEM_WRAP files found : ${EXT_FEM_WRAP_SRC} ${EXT_FEM_WRAP_HEADER}")
  #\todo : the variables EXT_WRAP_USER_* store only one variable... it should holds
  #        a list instead (so that the wrap_user.* files could be generated with the cat command
  set(EXT_WRAP_USER_SRC    ${EXT_FEM_WRAP_SRC}    PARENT_SCOPE)
  set(EXT_WRAP_USER_HEADER ${EXT_FEM_WRAP_HEADER} PARENT_SCOPE)

endif()

# Compiling with ExternalMBS library :
if(EXT_MBS_VERSION STREQUAL "none")
  set(LMGC90_BINDINGS_MBS_SRCS ${CMAKE_CURRENT_SOURCE_DIR}/Std_ExternalMBS.f90)
  set(LMGC90_BINDINGS_MBS_TARGET_LIBS)
  add_library(lmgc_bindings_MBS ${LMGC90_BINDINGS_MBS_SRCS})
  target_link_libraries(lmgc_bindings_MBS ${LMGC90_BINDINGS_MBS_TARGET_LIBS} lmgc_core_shared)
else()
  find_package(ExternalMBS_FOR_LMGC REQUIRED)
  message(STATUS "EXT_MBS binding found : ${EXT_MBS_F90_MODULE}")
  message(STATUS "EXT_MBS libraries : ${LMGC90_BINDINGS_MBS_TARGET_LIBS}")
  set(LMGC90_BINDINGS_MBS_SRCS ${EXT_MBS_F90_MODULE})
  add_library(lmgc_bindings_MBS ${LMGC90_BINDINGS_MBS_SRCS})
  target_link_libraries(lmgc_bindings_MBS ${LMGC90_BINDINGS_MBS_TARGET_LIBS} lmgc_core_shared)
endif()

####################################################################################

