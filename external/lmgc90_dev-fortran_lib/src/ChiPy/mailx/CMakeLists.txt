SET(LMGC90_PYTHON_MAILx_LIB_SRCS wrap_ALpxx.f90
                                 wrap_ASpxx.f90
                                 wrap_CLxxx.f90
                                 wrap_CSxxx.f90
                                 wrap_DISKL.f90
                                 wrap_MAILx.f90
                                 wrap_PT2DL.f90
                                 wrap_mecaMAILx.f90
                                 wrap_therMAILx.f90
                                 wrap_poroMAILx.f90
                                 wrap_multiMAILx.f90
                                )

include_directories( ${LAPACK95_INCLUDE_DIR} )
add_library(lmgc_python_mailx ${LMGC90_PYTHON_MAILx_LIB_SRCS})
target_link_libraries(lmgc_python_mailx lmgc_core_mailx
                                        lmgc_core_contactor_2d
                                        lmgc_core_contactor_3d
                                        ${LAPACK_LIBRARIES}
                                       )


SET(_headers ${CMAKE_CURRENT_SOURCE_DIR}/wrap_ALpxx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_ASpxx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CLxxx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CSxxx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DISKL.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_MAILx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PT2DL.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_mecaMAILx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_therMAILx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_poroMAILx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_multiMAILx.h
            )

SET(_HEADERS wrap_ALpxx.h
             wrap_ASpxx.h
             wrap_CLxxx.h
             wrap_CSxxx.h
             wrap_DISKL.h
             wrap_MAILx.h
             wrap_PT2DL.h
             wrap_mecaMAILx.h
             wrap_therMAILx.h
             wrap_poroMAILx.h
             wrap_multiMAILx.h
            )

set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

if(${BUILD_C_LIB})
  FILE(GLOB _sources ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.f90)
  SET(_sources ${CMAKE_CURRENT_SOURCE_DIR}/wrap_ALpxx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_ASpxx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CLxxx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CSxxx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DISKL.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_MAILx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PT2DL.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_mecaMAILx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_therMAILx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_poroMAILx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_multiMAILx.f90
              )
  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/mailx
         )
endif(${BUILD_C_LIB})
