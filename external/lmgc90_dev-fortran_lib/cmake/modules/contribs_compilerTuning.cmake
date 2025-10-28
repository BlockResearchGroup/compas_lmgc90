
# MUMPs is not to be debugged/profiled by us
# thus only OPT options are used
function(get_mumps_compiler_options)

  # remove default option of conda
  # there is probably better to do...
  # as told there: https://conda.io/projects/conda-build/en/latest/resources/compiler-tools.html#customizing-the-compilers
  if(${CONDA_BUILD})
    string(REPLACE "-fopenmp " ""
           CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS}
          )
  endif(${CONDA_BUILD})

  
  if( NOT WIN32 )
    ##### Set flags for C compiler   #####
    set(MUMPS_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
    ##### Set flags for Fortran compiler #####
    set(MUMPS_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC" )
  endif( NOT WIN32 )

  #######    OpenMP  management    ###########
  if( ${WITH_OPENMP} )
    find_package(OpenMP REQUIRED)

    set(MUMPS_C_FLAGS       "${MUMPS_C_FLAGS}       ${OpenMP_C_FLAGS}"      )
    set(MUMPS_Fortran_FLAGS "${MUMPS_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")

  endif (${WITH_OPENMP} )

  ##### Set flags for Fortran compiler #####

  if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")

    if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 10)
        set(MUMPS_Fortran_FLAGS "${MUMPS_Fortran_FLAGS} -O3")
    else(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 10)
        set(MUMPS_Fortran_FLAGS "${MUMPS_Fortran_FLAGS} -O3 -fallow-argument-mismatch")
    endif(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 10)

  elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "G95")

    set(MUMPS_Fortran_FLAGS "${MUMPS_Fortran_FLAGS} -O3")

  elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")

    if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 19.0)
      set(opt_prefetch "-opt-prefetch")
    else(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 19.0)
      set(opt_prefetch "-qopt-prefetch")
    endif(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 19.0)

    set(MUMPS_Fortran_FLAGS "${MUMPS_Fortran_FLAGS} -align ${opt_prefetch} -scalar_rep -rcd -fp -O3 -w -prec_div")

  elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")
    set(MUMPS_Fortran_FLAGS "${MUMPS_Fortran_FLAGS} -fast")
  else()
      message(STATUS "unknown compiler id (default options will be used)... ${CMAKE_Fortran_COMPILER_ID}")
  endif()

  set(MUMPS_C_FLAGS "${MUMPS_C_FLAGS}" PARENT_SCOPE)
  set(MUMPS_Fortran_FLAGS "${MUMPS_Fortran_FLAGS}" PARENT_SCOPE)

endfunction()


# Get flags for all contribs...
# Specific functions may be defined latter
# Only optimized function right now since it is not
# intend to debug/profile these libraries
function(get_contribs_compiler_options)

  # remove default option of conda
  # there is probably better to do...
  # as told there: https://conda.io/projects/conda-build/en/latest/resources/compiler-tools.html#customizing-the-compilers
  if(${CONDA_BUILD})
    string(REPLACE "-fopenmp " ""
           CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS}
          )
  endif(${CONDA_BUILD})

  if( NOT WIN32 )
    ##### Set flags for C compiler   #####
    set(CONTRIBS_C_FLAGS "${CMAKE_C_FLAGS} -fPIC -O3")
    ##### Set flags for CXX compiler #####
    set(CONTRIBS_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -O3")
    ##### Set flags for Fortran compiler #####
    set(CONTRIBS_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")
  endif( NOT WIN32 )

  if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
    set(CONTRIBS_Fortran_FLAGS "${CONTRIBS_Fortran_FLAGS} -O3")
  elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "G95")
    set(CONTRIBS_Fortran_FLAGS "${CONTRIBS_Fortran_FLAGS} -O3")
  elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 19.0)
      set(opt_prefetch "-opt-prefetch")
    else(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 19.0)
      set(opt_prefetch "-qopt-prefetch")
    endif(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 19.0)
    set(CONTRIBS_Fortran_FLAGS "${CONTRIBS_Fortran_FLAGS} -align ${opt_prefetch} -scalar_rep -rcd -fp -O3 -w -prec_div")
  elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")
    set(CONTRIBS_Fortran_FLAGS "${CONTRIBS_Fortran_FLAGS} -fast")
  else()
      message(STATUS "unknown compiler id (default options will be used)... ${CMAKE_Fortran_COMPILER_ID}")
  endif()

  set(CONTRIBS_C_FLAGS       "${CONTRIBS_C_FLAGS}"       PARENT_SCOPE)
  set(CONTRIBS_CXX_FLAGS     "${CONTRIBS_CXX_FLAGS}"     PARENT_SCOPE)
  set(CONTRIBS_Fortran_FLAGS "${CONTRIBS_Fortran_FLAGS}" PARENT_SCOPE)

endfunction()

