################################################################################
#  $Id: CMakeLists.txt 220 2016-10-21 13:36:53Z lstainier $
################################################################################
#  This file is part of ZorgLib, a computational simulation framework          #
#  for thermomechanics of solids and structures (systems in general).          #
#                                                                              #
#  Copyright (c) 2001-2016, L. Stainier.                                       #
#  See file LICENSE.txt for license information.                               #
#  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.     #
################################################################################

cmake_minimum_required(VERSION 3.5.0)
# policy management
#if(POLICY CMP0078)
#  cmake_policy(SET CMP0078 OLD)
#endif(POLICY CMP0078)
if(POLICY CMP0086)
  cmake_policy(SET CMP0086 NEW)
endif(POLICY CMP0086)

if(NOT DEFINED CMAKE_MACOSX_RPATH)
  set(CMAKE_MACOSX_RPATH 0)
endif(NOT DEFINED CMAKE_MACOSX_RPATH)

# if CMAKE_BUILD_TYPE is specified use it; otherwise set the default build
# type to "RelWithDebInfo" ("-O2 -g" with gcc) prior to calling project()
if(DEFINED CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "Choose build type")
else(DEFINED CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Choose build type")
endif(DEFINED CMAKE_BUILD_TYPE)

project(MatLib CXX C)

MESSAGE(STATUS "You are trying to build MatLib ...")

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

include(CMakeDependentOption)

option(ENABLE_BLAS_LAPACK   "Use BLAS/Lapack for basic linear algebra" ON)
option(ENABLE_NAMESPACE     "Use namespace" ON)
option(MATLIB_USE_MATLIB_NAMESPACE "Use namespace matlib" OFF)
CMAKE_DEPENDENT_OPTION(MATLIB_USE_ZORGLIB_NAMESPACE "Use namespace zorglib" ON
                       "ENABLE_NAMESPACE;NOT MATLIB_USE_MATLIB_NAMESPACE" OFF)
option(ENABLE_TESTING       "Create executables for testing" ON)

find_package(PythonLibs REQUIRED)
string(REGEX REPLACE "^([0-9]*).+$" "\\1" PYTHON_VERSION_MAJOR "${PYTHONLIBS_VERSION_STRING}")

# generating config.h
include(matlib_config)
configure()

# some path for output : libs, modules, executables
set(LIBRARY_OUTPUT_PATH    ${CMAKE_BINARY_DIR}/lib)
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bin)

# gathering source files
file(GLOB_RECURSE DATA_SRCS "src/data/*.cpp")
source_group(data FILES ${DATA_SRCS})
file(GLOB_RECURSE MATH_SRCS "src/math/*.cpp")
source_group(math FILES ${MATH_SRCS})
file(GLOB_RECURSE OPTIMIZATION_SRCS "src/opti/*.cpp")
source_group(opti FILES ${OPTIMIZATION_SRCS})
file(GLOB_RECURSE MATERIALS_SRCS "src/matl/*.cpp")

file(GLOB MATERIALS_FILES "src/matl/*.cpp")
source_group(matl FILES ${MATERIALS_FILES})
file(GLOB_RECURSE MATERIALS_DIFF_FILES "src/matl/diff/*.cpp")
source_group(matl\\diff FILES ${MATERIALS_DIFF_FILES})
file(GLOB_RECURSE MATERIALS_DIFFMECA_FILES "src/matl/diffmeca/*.cpp")
source_group(matl\\diffmeca FILES ${MATERIALS_DIFFMECA_FILES})
file(GLOB_RECURSE MATERIALS_MECA_FILES "src/matl/meca/*.cpp")
source_group(matl\\meca FILES ${MATERIALS_MECA_FILES})
file(GLOB_RECURSE MATERIALS_THERMO_FILES "src/matl/thermo/*.cpp")
source_group(matl\\thermo FILES ${MATERIALS_THERMO_FILES})
file(GLOB_RECURSE MATERIALS_THERMOMECA_FILES "src/matl/thermomeca/*.cpp")
source_group(matl\\thermomeca FILES ${MATERIALS_THERMOMECA_FILES})

# gathering public header files
file(GLOB DATA_HEADERS "src/data/*.h")
source_group(data FILES ${DATA_HEADERS})
file(GLOB PUBLIC_MATH_HEADERS "src/math/Vector*.h" "src/math/MathUtils.h")
file(GLOB MATH_HEADERS "src/math/*.h")
source_group(math FILES ${MATH_HEADERS})
file(GLOB PUBLIC_OPTI_HEADERS "src/opti/Opti*.h")
file(GLOB OPTI_HEADERS "src/opti/*.h")
source_group(opti FILES ${OPTI_HEADERS})
file(GLOB PUBLIC_MATL_HEADERS "src/matl/*.h")
file(GLOB_RECURSE MATL_HEADERS "src/matl/*.h")
source_group(matl FILES ${PUBLIC_MATL_HEADERS})
set(PUBLIC_HEADERS_LIST "${CMAKE_CURRENT_SOURCE_DIR}/include/MatLib.h")
list(APPEND PUBLIC_HEADERS_LIST "${CMAKE_CURRENT_SOURCE_DIR}/include/c_matlib.h")
list(APPEND PUBLIC_HEADERS_LIST "${CMAKE_CURRENT_SOURCE_DIR}/include/matlib_macros.h")
list(APPEND PUBLIC_HEADERS_LIST "${CMAKE_CURRENT_BINARY_DIR}/include/matlib_config.h")
set(HEADERS_LIST ${PUBLIC_HEADERS_LIST})
list(APPEND PUBLIC_HEADERS_LIST ${DATA_HEADERS} ${PUBLIC_MATH_HEADERS} ${PUBLIC_OPTI_HEADERS}
                                ${PUBLIC_MATL_HEADERS})
list(APPEND HEADERS_LIST ${DATA_HEADERS} ${MATH_HEADERS} ${OPTI_HEADERS} ${MATL_HEADERS})

file(GLOB_RECURSE MATERIALS_DIFF_HEADERS "src/matl/diff/*.h")
source_group(matl\\diff FILES ${MATERIALS_DIFF_HEADERS})
file(GLOB_RECURSE MATERIALS_DIFFMECA_HEADERS "src/matl/diffmeca/*.h")
source_group(matl\\diffmeca FILES ${MATERIALS_DIFFMECA_HEADERS})
file(GLOB_RECURSE MATERIALS_MECA_HEADERS "src/matl/meca/*.h")
source_group(matl\\meca FILES ${MATERIALS_MECA_HEADERS})
file(GLOB_RECURSE MATERIALS_THERMO_HEADERS "src/matl/thermo/*.h")
source_group(matl\\thermo FILES ${MATERIALS_THERMO_HEADERS})
file(GLOB_RECURSE MATERIALS_THERMOMECA_HEADERS "src/matl/thermomeca/*.h")
source_group(matl\\thermomeca FILES ${MATERIALS_THERMOMECA_HEADERS})

# include paths
include_directories(${CMAKE_CURRENT_BINARY_DIR}/include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src)

# use, i.e. don't skip the full RPATH for the build tree
set(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE) 

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# the RPATH to be used when installing, but only if it's not a system
# directory
list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES
          "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
if("${isSystemDir}" STREQUAL "-1")
   set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
endif("${isSystemDir}" STREQUAL "-1")

# building shared library
add_library( matlib SHARED ${DATA_SRCS} ${MATH_SRCS} ${OPTIMIZATION_SRCS} ${MATERIALS_SRCS}
             ${HEADERS_LIST} )
set_target_properties( matlib PROPERTIES
                       PUBLIC_HEADER "${PUBLIC_HEADERS_LIST}"
                       INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib")
target_link_libraries( matlib ${LAPACK_LIBRARIES} ${M_LIB})

# installing shared library
install( TARGETS matlib LIBRARY DESTINATION lib
                        ARCHIVE DESTINATION lib
                        RUNTIME DESTINATION lib
                        PUBLIC_HEADER DESTINATION include )

# building python module
#add_subdirectory(python)

# building application
add_subdirectory(appl)

# testing
if(ENABLE_TESTING)
  enable_testing()

  # test directory
  add_subdirectory(test)
endif(ENABLE_TESTING)
