
include( ExternalProject )
include( FetchContent )

# set compilation for internal project contribs flags
set(CMAKE_CXX_FLAGS ${CONTRIBS_CXX_FLAGS})
set(CMAKE_C_FLAGS ${CONTRIBS_C_FLAGS})
set(CMAKE_Fortran_FLAGS ${CONTRIBS_Fortran_FLAGS})

add_subdirectory(demmefi)
add_subdirectory(robustPredicates)
add_subdirectory(rTree)
add_subdirectory(ann-1.1.2)

if(${CLIPPER_VERSION} STREQUAL "1")
  add_subdirectory(clipper-6.4)
  set(CLIPPER_LIBRARIES clipper PARENT_SCOPE)
  set(CLIPPER_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/src/contribs/clipper-6.4/cpp PARENT_SCOPE)
elseif(${CLIPPER_VERSION} STREQUAL "2")
  # first provide the source to add
  FetchContent_Declare(clipper_project
                       SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/clipper2-1.4.0/CPP
                      )
  # set their cmake variables values... may conflict with
  # current ones
  set(CLIPPER2_EXAMPLES FALSE)
  set(CLIPPER2_TESTS FALSE)
  # then make it so the project can be configure/build
  FetchContent_MakeAvailable(clipper_project)
  # now the targets of the clipper project can be used
  # (again you have to look for the names inside the subproject)
  set(CLIPPER_LIBRARIES Clipper2 PARENT_SCOPE)
endif(${CLIPPER_VERSION} STREQUAL "1")

add_subdirectory(minpack)

###############################
# Optional building of mumps  #
if(${BUILD_MUMPS})

  if( NOT( IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/MUMPS_THREADSAFE_4.9.2" ) )
    if( ${CMAKE_VERSION} VERSION_LESS "3.18" )
      execute_process(COMMAND tar -xzf ${CMAKE_CURRENT_SOURCE_DIR}/MUMPS_THREADSAFE_4.9.2.tar.gz -C ${CMAKE_CURRENT_SOURCE_DIR} )
    else() # the right way ?
      file(ARCHIVE_EXTRACT INPUT        ${CMAKE_CURRENT_SOURCE_DIR}/MUMPS_THREADSAFE_4.9.2.tar.gz
                           DESTINATION  ${CMAKE_CURRENT_SOURCE_DIR}
          
          )
    endif()
  endif()

  # decide if to build sequential or parallel
  if( ${WITH_MPI} )
    set( ENABLE_SEQUENTIAL FALSE )
  else( ${WITH_MPI} )
    set( ENABLE_SEQUENTIAL TRUE)
  endif( ${WITH_MPI} )

  set(MUMPS_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/MUMPS_THREADSAFE_4.9.2)
  FetchContent_Declare(mumps_project
                       SOURCE_DIR ${MUMPS_SOURCE_DIR}
                      )
  # set their cmake variables values... may conflict with
  # current ones
  set( CMAKE_C_FLAGS       ${MUMPS_C_FLAGS}      )
  set( CMAKE_Fortran_FLAGS ${MUMPS_Fortran_FLAGS})
  set( ENABLE_EXAMPLES     OFF                   )
  set( ENABLE_SEQUENTIAL   ${ENABLE_SEQUENTIAL}  )
  # then make it so the project can be configure/build
  FetchContent_MakeAvailable(mumps_project)
  # now the targets of the clipper project can be used
  # (again you have to look for the names inside the subproject)
  set(SPARSE_LIBRARIES dmumps PARENT_SCOPE)

  # but the include dir is currently the source one... obviously.
  set(SPARSE_INCLUDE_DIR  ${MUMPS_SOURCE_DIR}/include PARENT_SCOPE)
  set(SPARSE_DEPENDENCIES "")
endif()
set(SPARSE_DEPENDENCIES ${SPARSE_DEPENDENCIES} lmgc_core_utils PARENT_SCOPE)
#                             #
###############################

if(${SPARSE_LIBRARY} STREQUAL "umfpack")

    add_subdirectory(UMFPackFortranBinding)
    set(SPARSE_LIBRARIES ${SPARSE_LIBRARIES} PARENT_SCOPE)

endif(${SPARSE_LIBRARY} STREQUAL "umfpack")

if(${BUILD_MATLIB})

  if( NOT (${MATLIB_VERSION} STREQUAL "none") )
      if( ${MATLIB_VERSION} STREQUAL "v4" )
          set( MATLIB_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/MatLib4-LMGC-2018-1)
      else()
          set( MATLIB_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/MatLib4-LMGC-2021-1)

      endif()

      FetchContent_Declare(matlib_project
                           SOURCE_DIR ${MATLIB_SOURCE_DIR}
                          )
      # set their cmake variables values... may conflict with
      # current ones
      set( CMAKE_CXX_STANDARD 11                  )
      set( CMAKE_CXX_FLAGS    ${MATLIB_CXX_FLAGS} )
      set( CMAKE_CXX_FLAGS    "${MATLIB_CXX_FLAGS} -flax-vector-conversions" )
      # this is an 'option' and not a variable...
      set( ENABLE_TESTING OFF CACHE BOOL "" FORCE)
      # then make it so the project can be configure/build
      FetchContent_MakeAvailable(matlib_project)
      # now the targets of the clipper project can be used
      # (again you have to look for the names inside the subproject)
      set(MATLIB_LIBRARY matlib PARENT_SCOPE)

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


