
SET(LMGC90_PYTHON_CONTACT3D_SRCS wrap_CDCDx.f90
                                 wrap_CDPLx.f90
                                 wrap_CSASp.f90
                                 wrap_CSPRx.f90
                                 wrap_PRASp.f90
                                 wrap_PRPLx.f90
                                 wrap_PRPRx.f90
                                 wrap_PTPT3.f90
                                 wrap_SPCDx.f90
                                 wrap_SPDCx.f90
                                 wrap_SPPLx.f90
                                 wrap_SPPRx.f90				 
                                 wrap_SPSPx.f90
                                 wrap_inter_handler_3D.f90
                                )

ADD_LIBRARY(lmgc_python_contact_3d ${LMGC90_PYTHON_CONTACT3D_SRCS})
TARGET_LINK_LIBRARIES(lmgc_python_contact_3d lmgc_core_contact_3d
                                             ${LAPACK_LIBRARIES}
                     )

#FILE(GLOB _headers ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.h)
#set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)
#
#FILE(GLOB _HEADERS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.h)
SET(_headers ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CDCDx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CDPLx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CSASp.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CSPRx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PRASp.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PRPLx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PRPRx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PTPT3.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_SPCDx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_SPDCx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_SPPLx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_SPPRx.h	     
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_SPSPx.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_inter_handler_3D.h
            )

set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

SET(_HEADERS wrap_CDCDx.h
             wrap_CDPLx.h
             wrap_CSASp.h
             wrap_CSPRx.h
             wrap_PRASp.h
             wrap_PRPLx.h
             wrap_PRPRx.h
             wrap_PTPT3.h
             wrap_SPCDx.h
             wrap_SPDCx.h
             wrap_SPPLx.h
             wrap_SPPRx.h	     
             wrap_SPSPx.h
             wrap_inter_handler_3D.h)

if(${BUILD_C_LIB})
  set(_sources ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CDCDx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CDPLx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CSASp.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_CSPRx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PRASp.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PRPLx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PRPRx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_PTPT3.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_SPCDx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_SPDCx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_SPPLx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_SPPRx.f90	       
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_SPSPx.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/wrap_inter_handler_3D.f90
              )

  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/contact_3D
         )
endif(${BUILD_C_LIB})
