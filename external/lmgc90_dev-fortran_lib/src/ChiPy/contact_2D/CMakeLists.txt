SET(LMGC90_PYTHON_CONTACT2D_SRCS wrap_CLALp.f90
                                 wrap_CLJCx.f90
                                 wrap_DKALp.f90
                                 wrap_DKDKL.f90
                                 wrap_DKDKx.f90
                                 wrap_DKJCx.f90
                                 wrap_DKKDx.f90
                                 wrap_DKPLx.f90
                                 wrap_P2P2L.f90
                                 wrap_PLALp.f90
                                 wrap_PLJCx.f90
                                 wrap_PLPLx.f90
                                 wrap_PTPT2.f90
                                 wrap_inter_handler_2D.f90
                                )

INCLUDE_DIRECTORIES( ${LAPACK95_INCLUDE_DIR} )
ADD_LIBRARY(lmgc_python_contact_2d ${LMGC90_PYTHON_CONTACT2D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_python_contact_2d lmgc_core_contact_2d
                                             lmgc_core_shared
                                             ${LAPACK_LIBRARIES})
SET(_headers ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CLALp.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CLJCx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DKALp.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DKDKL.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DKDKx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DKJCx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DKKDx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DKPLx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_P2P2L.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PLALp.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PLJCx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PLPLx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PTPT2.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_inter_handler_2D.h
            )

set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

SET(_HEADERS wrap_CLALp.h
             wrap_CLJCx.h
             wrap_DKALp.h
             wrap_DKDKL.h
             wrap_DKDKx.h
             wrap_DKJCx.h
             wrap_DKKDx.h
             wrap_DKPLx.h
             wrap_P2P2L.h
             wrap_PLALp.h
             wrap_PLJCx.h
             wrap_PLPLx.h
             wrap_PTPT2.h
             wrap_inter_handler_2D.h)

if(${BUILD_C_LIB})
  SET(_sources ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CLALp.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CLJCx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DKALp.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DKDKL.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DKDKx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DKJCx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DKKDx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DKPLx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_P2P2L.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PLALp.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PLJCx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PLPLx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PTPT2.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_inter_handler_2D.f90
              )

  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/contact_2D
         )
endif(${BUILD_C_LIB})
