# - Try to find ExternalMBS
# Once done, this will define
#
#  EXT_MBS_FOUND - system has external MBS library
#  EXT_MBS_LIBRARY - link these to use external mbs (looked for in EXT_MBS_LIB_PATH)
#  EXT_MBS_F90_MODULE - the external fem binding module (looked for in EXT_MBS_PATH)

include(LibFindMacros)

if(${EXT_MBS_VERSION} STREQUAL "Robotran")

  if( EXT_MBS_FILE )
    include(${EXT_MBS_FILE})
    set(LMGC90_BINDINGS_MBS_TARGET_LIBS ${MBS_EXTERNAL_LINK_LIBRARIES})
  endif( EXT_MBS_FILE )

  if( NOT ${CMAKE_C_COMPILER_ID} STREQUAL ${MBS_C_COMPILER_ID})
    message(FATAL_ERROR "Different compiler between Robotran and LMGC90 projects
                        ${CMAKE_C_COMPILER_ID}/${MBS_C_COMPILER_ID}"
           )
  endif()
  #message(STATUS "${C_COMPILER_ID} ${MBS_EXTERNAL_LINK_LIBRARIES} ${EXT_MBS_F90_MODULE}")
elseif(${EXT_MBS_VERSION} STREQUAL "FiberModel")

  if( EXT_MBS_FILE )
    include(${EXT_MBS_FILE})
    set(LMGC90_BINDINGS_MBS_TARGET_LIBS ${MBS_EXTERNAL_LINK_LIBRARIES})
  endif( EXT_MBS_FILE )

  if( NOT ${CMAKE_CXX_COMPILER_ID} STREQUAL ${MBS_CXX_COMPILER_ID})
    message(FATAL_ERROR "Different compiler between FiberModel and LMGC90 projects
                        ${CMAKE_CXX_COMPILER_ID}/${MBS_CXX_COMPILER_ID}"
           )
  endif()

endif()

