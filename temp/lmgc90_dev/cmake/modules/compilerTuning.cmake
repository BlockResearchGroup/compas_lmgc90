
function(get_lmgc90_compiler_options)

  # remove default option of conda
  # there is probably better to do...
  # as told there: https://conda.io/projects/conda-build/en/latest/resources/compiler-tools.html#customizing-the-compilers
  if(${CONDA_BUILD})
    string(REPLACE "-fopenmp " ""
           CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS}
          )
  endif(${CONDA_BUILD})

  if( NOT WIN32 )
    set(LMGC90_C_FLAGS       "${CMAKE_C_FLAGS} -fPIC")
    set(LMGC90_CXX_FLAGS     "${CMAKE_CXX_FLAGS} -fPIC")
    set(LMGC90_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")
  endif( NOT WIN32 )

  #######    OpenMP  management    ###########
  if( ${WITH_OPENMP} )
    find_package(OpenMP REQUIRED)
    set(LMGC90_C_FLAGS       "${LMGC90_C_FLAGS}       ${OpenMP_C_FLAGS}"      )
    set(LMGC90_CXX_FLAGS     "${LMGC90_CXX_FLAGS}     ${OpenMP_CXX_FLAGS}"    )
    set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
  endif (${WITH_OPENMP} )

  #######  OPTIONS FOR C COMPILER  ###########
  if(${CMAKE_C_COMPILER_ID} STREQUAL "GNU")
    if(${OPT} STREQUAL "opt")
      set(LMGC90_C_FLAGS "${LMGC90_C_FLAGS} -O3" )
    elseif(${OPT} STREQUAL "profiling")
      set(LMGC90_C_FLAGS "${LMGC90_C_FLAGS} -O3 -g" )
    elseif(${OPT} STREQUAL "debug")
      set(LMGC90_C_FLAGS "${LMGC90_C_FLAGS} -Og -g" )
    elseif(${OPT} STREQUAL "check")
      set(LMGC90_C_FLAGS "${LMGC90_C_FLAGS} -O0 -g" )
    else()
      message(STATUS "OPT should be 'opt', 'profiling', 'debug' or 'check' and not : ${OPT}")
    endif()
  else()
    message(STATUS "unknown CXX compiler id (default options will be used)... ${CMAKE_CXX_COMPILER_ID}")
  endif()

  ####### OPTIONS FOR CXX COMPILER ###########
  if(${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU")
    if(${OPT} STREQUAL "opt")
      set(LMGC90_CXX_FLAGS "${LMGC90_CXX_FLAGS} -O3" )
    elseif(${OPT} STREQUAL "profiling")
      set(LMGC90_CXX_FLAGS "${LMGC90_CXX_FLAGS} -O3 -g" )
    elseif(${OPT} STREQUAL "debug")
      set(LMGC90_CXX_FLAGS "${LMGC90_CXX_FLAGS} -Og -g" )
    elseif(${OPT} STREQUAL "check")
      set(LMGC90_CXX_FLAGS "${LMGC90_CXX_FLAGS} -O0 -g" )
    else()
      message(STATUS "OPT should be 'opt', 'profiling', 'debug' or 'check' and not : ${OPT}")
    endif()
  else()
    message(STATUS "unknown CXX compiler id (default options will be used)... ${CMAKE_CXX_COMPILER_ID}")
  endif()


  ####### OPTIONS FOR Fortran compiler COMPILER ###########

  ##### SET FLAGS for GNU compiler #####
  if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
      # check minimum version of gnu fortran compiler :
      # prior to gfortran 4.5the ouptut of -dumpversion is not correctly formated
      #exec_program(${CMAKE_Fortran_COMPILER}
      #             ARGS ${CMAKE_Fortran_COMPILER_ARG1} --version
      #             OUTPUT_VARIABLE gfortran_VERSION
      #            )
      #string(REGEX REPLACE "^[^0-9]*([0-9])\\.([0-9])(\\.[0-9])?.*$" "\\1\\2"
      #       gfortran_VERSION ${gfortran_VERSION}
      #      )
      if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 4.5)
        message(FATAL_ERROR "gfortran minimum version needed is 4.5... ${CMAKE_Fortran_COMPILER_VERSION} found")
      endif(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 4.5)

      #if( ${WITH_NEW} )
      #  if( gfortran_VERSION LESS 46 )
      #    message(FATAL_ERROR "gfortran minimum version needed is 4.6 when using WITH_NEW option... ${gfortran_VERSION} found")
      #  endif( gfortran_VERSION LESS 46 )
      #else( ${WITH_NEW} )
      #if( gfortran_VERSION LESS 43 )
      #  message(FATAL_ERROR "gfortran minimum version needed is 4.3... ${gfortran_VERSION} found")
      #endif( gfortran_VERSION LESS 43 )
      set(gfortran_VERSION ${gfortran_VERSION} PARENT_SCOPE)
      #endif( ${WITH_NEW} )

      # gfortran options common to all build type
      #set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -fno-second-underscore -ffree-line-length-none -cpp")
      set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -fno-second-underscore -ffree-line-length-none")
      # now specific options
      if(${OPT} STREQUAL "opt")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -O2 -fexternal-blas" )
      elseif(${OPT} STREQUAL "profiling")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -O2 -g -fexternal-blas" )
      elseif(${OPT} STREQUAL "debug")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -Og -g" )
      elseif(${OPT} STREQUAL "check")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -O0 -g -fbacktrace -fdump-core -fbounds-check -fcheck-array-temporaries")
      else()
        message(STATUS "OPT should be 'opt', 'profiling', 'debug' or 'check' and not : ${OPT}")
      endif()
  #                                    #
  ##### SET FLAGS for G95 compiler #####
  #                                    #
  elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "G95")
      # gfortran options common to all build type
      #set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -fno-second-underscore -cpp")
      set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -fno-second-underscore")
      # now specific options
      if(${OPT} STREQUAL "opt")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -O2 -fexternal-blas")
      elseif(${OPT} STREQUAL "profiling")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -O2 -g -pg -fexternal-blas" )
      elseif(${OPT} STREQUAL "debug")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -Og -g -pg -ftrace=full")
      elseif(${OPT} STREQUAL "check")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -O0 -g -pg -C -ftrace=full")
      else()
        message(STATUS "OPT should be 'opt', 'debug' or 'check' and not : ${OPT}")
      endif()
  #                                      #
  ##### SET FLAGS for Intel compiler #####
  #                                      #
  elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
      if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 19.0)
        set(opt_prefetch "-opt-prefetch")
      else(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 19.0)
        set(opt_prefetch "-qopt-prefetch")
        set(opt_checks   "-check point -check bounds")
      endif(${CMAKE_Fortran_COMPILER_VERSION} VERSION_LESS 19.0)
      ## ifort options common to all build type
      #set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -cpp")
      # now specific options
      if(${OPT} STREQUAL "opt")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -align ${opt_prefetch} -scalar_rep -rcd -fp -O3 -w -prec_div")
      elseif(${OPT} STREQUAL "profiling")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -align ${opt_prefetch} -scalar_rep -rcd -fp -O3 -w -prec_div -g -p" )
      elseif(${OPT} STREQUAL "debug")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -O0 -g -p")
      elseif(${OPT} STREQUAL "check")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -C -w -traceback -fpe0 -g -p ${opt_checks}")
      else()
        message(STATUS "OPT should be 'opt', 'profiling', 'debug' or 'check' and not : ${OPT} (default options will be used)")
      endif()
  #                                         #
  ##### SET FLAGS for Portland compiler #####
  #                                         #
  elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")
      ## pgf90 options common to all build type
      #set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -Mcpp")
      # now specific options
      if(${OPT} STREQUAL "opt")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -fast")
      elseif(${OPT} STREQUAL "profiling")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -fast -gopt -pg" )
      elseif(${OPT} STREQUAL "debug")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -O0 -g")
      elseif(${OPT} STREQUAL "check")
        set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS} -O0 -g -C")
      else()
        message(STATUS "OPT should be 'opt', 'profiling', 'debug' or 'check' and not : ${OPT} (default options will be used)")
      endif()
  #                                         #
  ##### Unknown compiler : you loose !! #####
  #                                         #
  else()
      message(STATUS "unknown compiler id (default options will be used)... ${CMAKE_Fortran_COMPILER_ID}")
  endif()

  set(LMGC90_C_FLAGS "${LMGC90_C_FLAGS}" PARENT_SCOPE)
  set(LMGC90_CXX_FLAGS "${LMGC90_CXX_FLAGS}" PARENT_SCOPE)
  set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS}" PARENT_SCOPE)

endfunction()

