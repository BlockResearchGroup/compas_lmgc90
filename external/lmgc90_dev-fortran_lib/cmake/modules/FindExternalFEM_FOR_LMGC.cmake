# - Try to find ExternalFEM
# Once done, this will define
#
#  EXT_FEM_FOUND - system has MatLib
#  EXT_FEM_LIBRARY - link these to use external fem (looked for in EXT_FEM_LIB_PATH)
#  EXT_FEM_F90_MODULE - the external fem binding module (looked for in EXT_FEM_PATH)

include(LibFindMacros)


if(${EXT_FEM_VERSION} STREQUAL "Xper")

  if(${OPT} STREQUAL "opt")
    find_library(EXT_FEM_LIBRARY
                 NAMES Xper0
                 PATHS ${EXT_FEM_LIB_PATH}
                 NO_DEFAULT_PATH
                )
  else()
    find_library(EXT_FEM_LIBRARY
                 NAMES Xper2
                 PATHS ${EXT_FEM_LIB_PATH}
                 NO_DEFAULT_PATH
                )
  endif()

  if(${EXT_FEM_LIBRARY} STREQUAL "EXT_FEM_LIBRARY-NOTFOUND")
      message( FATAL_ERROR "unable to find appliPEL library in ${EXT_FEM_LIB_PATH}\n"
                           "Please set correctly EXT_FEM_PATH variable or check your build."
             )
  endif()

  find_file(EXT_FEM_F90_MODULE
            NAMES Xper_ExternalFEM.f90
            PATHS ${EXT_FEM_PATH}
                  ${CMAKE_SOURCE_DIR}/../LMGC90v2_BindingExternalFEM
           )

  find_file(EXT_FEM_WRAP_SRC
            NAMES Xper_wrap_user.f90
            PATHS ${EXT_FEM_PATH}
                  ${CMAKE_SOURCE_DIR}/../LMGC90v2_BindingExternalFEM
           )
  find_file(EXT_FEM_WRAP_HEADER
            NAMES Xper_wrap_user.h
            PATHS ${EXT_FEM_PATH}
                  ${CMAKE_SOURCE_DIR}/../LMGC90v2_BindingExternalFEM
           )

  if(${EXT_FEM_F90_MODULE} STREQUAL "EXT_FEM_F90_MODULE-NOTFOUND")
    message( FATAL_ERROR "unable to find Xper_ExternalFEM.f90 file. "
                         "Please set correctly EXT_FEM_PATH."
           )
  endif()

elseif(${EXT_FEM_VERSION} STREQUAL "tense_dd")

  if( ${MUMPS_VERSION} STREQUAL "none" )
    message( FATAL_ERROR "tense_dd external FEM demands to compile with MUMPS"
                         "Please set MUMPS_VERSION variable to 'sequential' or 'parallel'."
           )
  endif( ${MUMPS_VERSION} STREQUAL "none" )

  find_file(EXT_FEM_F90_MODULE
            NAMES tense_dd_ExternalFEM.f90
            HINTS ${EXT_FEM_PATH}
           )

  if(${EXT_FEM_F90_MODULE} STREQUAL "EXT_FEM_F90_MODULE-NOTFOUND")
    message( FATAL_ERROR "unable to find tense_dd_ExternalFEM.f90 file. "
                         "Please set correctly EXT_FEM_PATH."
           )
  endif()

  add_library(tense_dd ${EXT_FEM_PATH}/mod_common0.f90
                       ${EXT_FEM_PATH}/mod_secondary.f90
             )
  target_link_libraries(tense_dd ${MUMPS_LIBRARIES}
                                 ${LAPACK_LIBRARIES}
                        )
  set(EXT_FEM_LIBRARY tense_dd)
endif()
