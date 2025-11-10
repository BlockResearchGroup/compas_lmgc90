################################################################################
#  $Id: CMakeLists.txt 124 2013-01-11 16:41:33Z lstainier $
################################################################################
#  This file is part of ZorgLib, a computational simulation framework          #
#  for thermomechanics of solids and structures (systems in general).          #
#                                                                              #
#  Copyright (c) 2001-2013, L. Stainier.                                       #
#  See file LICENSE.txt for license information.                               #
#  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.     #
################################################################################

find_package(SWIG REQUIRED)

include(${SWIG_USE_FILE})
include_directories(${PYTHON_INCLUDE_DIR})

SET_SOURCE_FILES_PROPERTIES(matlib.i PROPERTIES CPLUSPLUS ON )

if(NOT ${PYTHON_VERSION_MAJOR} LESS 3)
  set(CMAKE_SWIG_FLAGS "-py3")
endif(NOT ${PYTHON_VERSION_MAJOR} LESS 3)

if( ${CMAKE_VERSION} VERSION_LESS 3.8 )
  SWIG_ADD_MODULE(matlib python "matlib.i")
else( ${CMAKE_VERSION} VERSION_LESS 3.8 )
  SWIG_ADD_LIBRARY(matlib LANGUAGE python SOURCES "matlib.i")
endif( ${CMAKE_VERSION} VERSION_LESS 3.8 )

SWIG_LINK_LIBRARIES(matlib matlib ${PYTHON_LIBRARIES})

#add_custom_command(TARGET ${SWIG_MODULE_matlib_REAL_NAME} POST_BUILD
#                   COMMAND ${CMAKE_COMMAND} -E rename
#                   ${CMAKE_CURRENT_BINARY_DIR}/matlib.py 
#                   ${CMAKE_BINARY_DIR}/lib/matlib.py
#                  )
install( TARGETS ${SWIG_MODULE_matlib_REAL_NAME} 
         LIBRARY DESTINATION python
         BUNDLE DESTINATION python )
install( FILES ${CMAKE_CURRENT_BINARY_DIR}/matlib.py DESTINATION python)

