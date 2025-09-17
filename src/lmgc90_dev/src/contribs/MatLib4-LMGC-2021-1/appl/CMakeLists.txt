################################################################################
#  $Id: CMakeLists.txt 484 2016-02-29 13:54:04Z lstainier $
################################################################################
#  This file is part of ZorgLib, a computational simulation framework          #
#  for thermomechanics of solids and structures (systems in general).          #
#                                                                              #
#  Copyright (c) 2001-2016, L. Stainier.                                       #
#  See file LICENSE.txt for license information.                               #
#  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.     #
################################################################################

MESSAGE(STATUS "You are trying to build MatLibRun ...")

find_package(PythonLibs REQUIRED)
include_directories(${PYTHON_INCLUDE_DIR})

# create executable
add_executable(MatLibRun MatLibRun.cpp)
target_link_libraries( MatLibRun matlib ${PYTHON_LIBRARIES} ${M_LIB})

# installing application
install( TARGETS MatLibRun RUNTIME DESTINATION bin )
