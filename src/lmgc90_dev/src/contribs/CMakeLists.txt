
include( ExternalProject )

# set compilation for internal project contribs flags
set(CMAKE_CXX_FLAGS ${CONTRIBS_CXX_FLAGS})
set(CMAKE_C_FLAGS ${CONTRIBS_C_FLAGS})
set(CMAKE_Fortran_FLAGS ${CONTRIBS_Fortran_FLAGS})

if(${NO_INSTALL})
  set(CONTRIB_INSTALL_PREFIX ${CMAKE_BINARY_DIR})
else(${NO_INSTALL})
  set(CONTRIB_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
endif(${NO_INSTALL})
message(STATUS "contribs install prefix is now ${CONTRIB_INSTALL_PREFIX}")

add_subdirectory(demmefi)
add_subdirectory(robustPredicates)
add_subdirectory(rTree)
add_subdirectory(ann-1.1.2)
if(${CLIPPER_VERSION} STREQUAL "1")
  add_subdirectory(clipper-6.4)
  set(CLIPPER_LIBRARIES clipper PARENT_SCOPE)
  set(CLIPPER_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/src/contribs/clipper-6.4/cpp PARENT_SCOPE)
  #set(CLIPPER_DEPENDENCIES "" PARENT_SCOPE)
elseif(${CLIPPER_VERSION} STREQUAL "2")
  ExternalProject_Add(clipper_project
                      PREFIX ""
                      SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/clipper2-1.4.0/CPP
                      BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/clipper_build
                      CMAKE_ARGS -DLIBRARY_OUTPUT_PATH=${CMAKE_BINARY_DIR}/lib
                                 -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                                 -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
                                 -DCLIPPER2_EXAMPLES=FALSE
                                 -DCLIPPER2_TESTS=FALSE
                                 -DCMAKE_INSTALL_PREFIX=${CONTRIB_INSTALL_PREFIX}
                                 -DCMAKE_SKIP_BUILD_RPATH=FALSE
                                 -DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE
                                 -DCMAKE_INSTALL_RPATH=${CONTRIB_INSTALL_PREFIX}/lib
                                 -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE
                                 -DCMAKE_INSTALL_NAME_DIR=${CONTRIB_INSTALL_PREFIX}/lib
                      BUILD_IN_SOURCE 0
                      )
  set(CLIPPER_LIBRARIES ${CONTRIB_INSTALL_PREFIX}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}Clipper2${CMAKE_STATIC_LIBRARY_SUFFIX} PARENT_SCOPE)
  set(CLIPPER_INCLUDE_DIR ${CONTRIB_INSTALL_PREFIX}/include PARENT_SCOPE)
  set(CLIPPER_DEPENDENCIES clipper_project PARENT_SCOPE)
endif(${CLIPPER_VERSION} STREQUAL "1")

add_subdirectory(minpack)

###############################
# Optional building of mumps  #
if(${BUILD_MUMPS})

  if( NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/MUMPS_THREADSAFE_4.9.2" )
    execute_process(COMMAND tar -xzf ${CMAKE_CURRENT_SOURCE_DIR}/MUMPS_THREADSAFE_4.9.2.tar.gz -C ${CMAKE_CURRENT_SOURCE_DIR} )
  endif()

  # decide if to build sequential or parallel
  if( ${WITH_MPI} )
    set( ENABLE_SEQUENTIAL FALSE )
  else( ${WITH_MPI} )
    set( ENABLE_SEQUENTIAL TRUE)
  endif( ${WITH_MPI} )

  ExternalProject_Add(mumps_project
                      PREFIX ""
                      SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/MUMPS_THREADSAFE_4.9.2
                      BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/MUMPS_THREADSAFE_4.9.2_build
                      CMAKE_ARGS -DLIBRARY_OUTPUT_PATH=${CMAKE_BINARY_DIR}/lib
                                 -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                                 -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
                                 -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
                                 -DCMAKE_C_FLAGS=${MUMPS_C_FLAGS}
                                 -DCMAKE_Fortran_FLAGS=${MUMPS_Fortran_FLAGS}
                                 -DENABLE_EXAMPLES=OFF
                                 -DENABLE_SEQUENTIAL=${ENABLE_SEQUENTIAL}
                                 -DCMAKE_INSTALL_PREFIX=${CONTRIB_INSTALL_PREFIX}
                                 -DCMAKE_SKIP_BUILD_RPATH=FALSE
                                 -DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE
                                 -DCMAKE_INSTALL_RPATH=${CONTRIB_INSTALL_PREFIX}/lib
                                 -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE
                                 -DCMAKE_INSTALL_NAME_DIR=${CONTRIB_INSTALL_PREFIX}/lib
                      BUILD_IN_SOURCE 0
                      )

  #add_library(dmumps SHARED IMPORTED GLOBAL)
  #set_property(TARGET dmumps PROPERTY
  #             IMPORTED_LOCATION ${CONTRIB_INSTALL_PREFIX}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}dmumps${CMAKE_SHARED_LIBRARY_SUFFIX})
  #
  #get_property(dmumps_name TARGET dmumps PROPERTY IMPORTED_LIB_NAME )
  #message(STATUS "dmumps name : ${dmumps_name}")
  #add_dependencies(dmumps mumps_project)
  #set(SPARSE_LIBRARIES ${dmumps} PARENT_SCOPE)
  set(SPARSE_LIBRARIES ${CONTRIB_INSTALL_PREFIX}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}dmumps${CMAKE_SHARED_LIBRARY_SUFFIX} PARENT_SCOPE)
  set(SPARSE_INCLUDE_DIR ${CONTRIB_INSTALL_PREFIX}/include PARENT_SCOPE)
  set(SPARSE_DEPENDENCIES mumps_project)
endif()
set(SPARSE_DEPENDENCIES ${SPARSE_DEPENDENCIES} lmgc_core_utils PARENT_SCOPE)
#                             #
###############################

if(${SPARSE_LIBRARY} STREQUAL "umfpack")

    add_subdirectory(UMFPackFortranBinding)
    set(SPARSE_LIBRARIES ${SPARSE_LIBRARIES} PARENT_SCOPE)

endif(${SPARSE_LIBRARY} STREQUAL "umfpack")

if(${BUILD_MATLIB})

  set(CMAKE_MATLIB_CXX_FLAGS ${CMAKE_CXX_FLAGS})
  set(CMAKE_MATLIB_ADDITIONAL_ARGS "")
  set(CMAKE_MATLIB_CXX_FLAGS "${CMAKE_MATLIB_CXX_FLAGS} -Wno-deprecated")
  string( FIND "${CMAKE_MATLIB_CXX_FLAGS}" "-std=c++17" std_17)
  if( ${std_17} GREATER 0 )
    string( REPLACE "-std=c++17" "-std=gnu++11" CMAKE_MATLIB_CXX_FLAGS ${CMAKE_MATLIB_CXX_FLAGS})
  else()
    set(CMAKE_MATLIB_CXX_FLAGS  "-std=gnu++11 ${CMAKE_MATLIB_CXX_FLAGS}")
  endif()
  if ( CONDA_BUILD )
    if ( WINDOWS_COMPILATION )
      message( STATUS "Windows compilation, add CMAKE_SH-NOTFOUND")
      set(CMAKE_MATLIB_ADDITIONAL_ARGS ${CMAKE_MATLIB_ADDITIONAL_ARGS} "-DCMAKE_SH=${CMAKE_SH-NOTFOUND}")
    endif( WINDOWS_COMPILATION )
  endif ( CONDA_BUILD )

  if( NOT (${MATLIB_VERSION} STREQUAL "none") )
      if( ${MATLIB_VERSION} STREQUAL "v4" )
          set( MATLIB_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/MatLib4-LMGC-2018-1)
          set( MATLIB_BUILD_DIR  ${CMAKE_CURRENT_BINARY_DIR}/MatLib4-2018-build)
      else()
          #if( ${MATLIB_VERSION} STREQUAL "default" )
          set( MATLIB_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/MatLib4-LMGC-2021-1)
          set( MATLIB_BUILD_DIR  ${CMAKE_CURRENT_BINARY_DIR}/MatLib4-2021-build)

      endif()

      # compiler flags for MatLib4 should be decided by itself
      # but an ugly patching to work with macports is needed... to improve !
      if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" AND APPLE)
        set(CMAKE_MATLIB_CXX_FLAGS "${CMAKE_MATLIB_CXX_FLAGS} -flax-vector-conversions")
      endif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" AND APPLE)

      ExternalProject_Add(matlib
                          PREFIX ""
                          SOURCE_DIR ${MATLIB_SOURCE_DIR}
                          BINARY_DIR ${MATLIB_BUILD_DIR}
                          CMAKE_ARGS -DLIBRARY_OUTPUT_PATH=${CMAKE_BINARY_DIR}/lib
                                     -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
                                     -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                                     -DCMAKE_CXX_FLAGS=${CMAKE_MATLIB_CXX_FLAGS}
                                     -DLAPACK_FOUND=TRUE
                                     -DLAPACK_LIBRARIES=${LAPACK_LIBRARIES}
                                     -DPYTHON_LIBRARY=${Python_LIBRARIES}
                                     -DPYTHON_INCLUDE_DIR=${Python_INCLUDE_DIRS}
                                     -DENABLE_TESTING=OFF
                                     -DCMAKE_INSTALL_PREFIX=${CONTRIB_INSTALL_PREFIX}
                                     ${CMAKE_MATLIB_ADDITIONAL_ARGS}
                          BUILD_IN_SOURCE 0
                          )

      set(MATLIB_LIBRARY ${CONTRIB_INSTALL_PREFIX}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}matlib${CMAKE_SHARED_LIBRARY_SUFFIX})
      set(MATLIB_LIBRARY ${MATLIB_LIBRARY} PARENT_SCOPE)

  endif( NOT (${MATLIB_VERSION} STREQUAL "none") )
endif(${BUILD_MATLIB})

if(${BUILD_SICONOS_NUMERICS})

  if( ${SPARSE_LIBRARY} STREQUAL "mumps" )
    set(WITH_MUMPS ON)
    if( ${BUILD_MUMPS} )
      set(CMAKE_SICONOS_ADDITIONAL_ARGS "-DMUMPS_LIBRARIES=${SPARSE_LIBRARIES}")
    else( ${BUILD_MUMPS} )
      unset(SICONOS_DEPS)
      set(MUMPS_LIBRARIES ${MUMPS_FOUND} ${MUMPS_COMMON_FOUND})
      set(MUMPS_INCLUDE_DIR ${MUMPS_INCLUDE_DIRS})
      set(CMAKE_SICONOS_ADDITIONAL_ARGS "-DMUMPS_LIBRARIES=${MUMPS_LIBRARIES} -DMUMPS_INCLUDE_DIR=${MUMPS_INCLUDE_DIR}")
    endif( ${BUILD_MUMPS} )
  else( ${SPARSE_LIBRARY} STREQUAL "mumps" )
    message(FATAL_ERROR "Cannot use SiconosNumerics library without mumps...
  either active it with -DSPARSE_LIBRARY=mumps
  or deactivate the use of Siconos with -DWITH_SICONOS_NUMERICS=OFF"
            )
    set(WITH_MUMPS OFF)
    unset(SICONOS_DEPS)
  endif( ${SPARSE_LIBRARY} STREQUAL "mumps" )
  
  if( NOT IS_DIRECTORY ${SICONOS_SOURCE_DIR} )
    message(FATAL_ERROR "SICONOS_SOURCE_DIR variable must point to an existing directory : ${SICONOS_SOURCE_DIR}")
  endif( NOT IS_DIRECTORY ${SICONOS_SOURCE_DIR} )
  message(STATUS "STARTING SICONOS CONFIG")
  ExternalProject_Add(Siconos
                      #GIT_REPOSITORY "https://github.com/siconos/siconos.git"
                      #GIT_TAG        "4.2.0"
                      #GIT_PROGRESS   ON
                      #GIT_SHALLOW    ON
                      DEPENDS ${SICONOS_DEPS}
                      PREFIX ""
                      SOURCE_DIR ${SICONOS_SOURCE_DIR}
                      BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/siconos-build
                      CMAKE_ARGS #-DLIBRARY_OUTPUT_PATH=${CMAKE_BINARY_DIR}/lib
                                 -DCMAKE_INSTALL_PREFIX=${CONTRIB_INSTALL_PREFIX}
                                 -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
                                 -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
                                 -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                                 -DCMAKE_C_FLAGS=${CMAKE_C_FLAGS}
                                 -DUSER_OPTIONS_FILE=${CMAKE_CURRENT_SOURCE_DIR}/siconos.cmake
                                 ${CMAKE_SICONOS_ADDITIONAL_ARGS}
                      BUILD_IN_SOURCE 0
                      )
  message(STATUS "DONE SICONOS CONFIG")

  set(Siconos_Numerics_LIBRARIES ${CONTRIB_INSTALL_PREFIX}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}siconos_numerics${CMAKE_SHARED_LIBRARY_SUFFIX}
                                 ${CONTRIB_INSTALL_PREFIX}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}siconos_externals${CMAKE_SHARED_LIBRARY_SUFFIX}
         )
  set(SiconosNumerics_LIBRARIES ${Siconos_Numerics_LIBRARIES} PARENT_SCOPE )
  if( ${BUILD_MUMPS} )
      add_dependencies(Siconos mumps_project)
  endif( ${BUILD_MUMPS} )
endif(${BUILD_SICONOS_NUMERICS})


