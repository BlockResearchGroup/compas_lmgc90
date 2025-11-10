################################################################################
#  $Id: CMakeLists.txt 265 2020-03-19 18:02:48Z lstainier $
################################################################################
#  This file is part of ZorgLib, a computational simulation framework          #
#  for thermomechanics of solids and structures (systems in general).          #
#                                                                              #
#  Copyright (c) 2001-2019, L. Stainier.                                       #
#  See file LICENSE.txt for license information.                               #
#  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.     #
################################################################################

if(POLICY CMP0078)
  # use old SWIG target naming scheme
  cmake_policy(SET CMP0078 OLD)
endif(POLICY CMP0078)

if(POLICY CMP0086)
  # pass module name to SWIG if defined
  cmake_policy(SET CMP0086 NEW)
endif(POLICY CMP0086)

find_package(PythonLibs REQUIRED)
find_package(SWIG REQUIRED)

include(${SWIG_USE_FILE})
include_directories(${PYTHON_INCLUDE_DIR})

SET_SOURCE_FILES_PROPERTIES(matlib.i PROPERTIES CPLUSPLUS ON )

if(NOT ${PYTHON_VERSION_MAJOR} LESS 3)
  set(CMAKE_SWIG_FLAGS "-py3")
endif(NOT ${PYTHON_VERSION_MAJOR} LESS 3)

if(CMAKE_VERSION VERSION_LESS 3.8.2)
  SWIG_ADD_MODULE(matlib python "matlib.i")
else(CMAKE_VERSION VERSION_LESS 3.8.2)
  SWIG_ADD_LIBRARY(matlib LANGUAGE python SOURCES "matlib.i")
endif(CMAKE_VERSION VERSION_LESS 3.8.2)

if(${USING_ANACONDA})
  SWIG_LINK_LIBRARIES(matlib matlib)
  set_target_properties(${SWIG_MODULE_matlib_REAL_NAME} PROPERTIES
                        CXX_STANDARD 11
                        LINK_FLAGS "-undefined dynamic_lookup")
else(${USING_ANACONDA})
  SWIG_LINK_LIBRARIES(matlib matlib ${PYTHON_LIBRARIES})
  set_target_properties(${SWIG_MODULE_matlib_REAL_NAME} PROPERTIES
                        CXX_STANDARD 11 )
endif(${USING_ANACONDA})

#add_custom_command(TARGET ${SWIG_MODULE_matlib_REAL_NAME} POST_BUILD
#                   COMMAND ${CMAKE_COMMAND} -E rename
#                   ${CMAKE_CURRENT_BINARY_DIR}/matlib.py 
#                   ${CMAKE_BINARY_DIR}/lib/matlib.py
#                  )

install( TARGETS ${SWIG_MODULE_matlib_REAL_NAME}
         LIBRARY DESTINATION python
         BUNDLE DESTINATION python )
install( FILES ${CMAKE_CURRENT_BINARY_DIR}/matlib.py DESTINATION python)

