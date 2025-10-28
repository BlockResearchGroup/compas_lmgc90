# Install script for directory: /home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libmatlib.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libmatlib.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libmatlib.so"
         RPATH "/usr/local/lib")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE SHARED_LIBRARY FILES "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/lib/libmatlib.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libmatlib.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libmatlib.so")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libmatlib.so"
         OLD_RPATH "::::::::::::::"
         NEW_RPATH "/usr/local/lib")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libmatlib.so")
    endif()
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/include/MatLib.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/include/c_matlib.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/include/matlib_macros.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/_deps/matlib_project-build/include/matlib_config.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/Chronometer.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/Cloneable.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/ConstantFunction.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/Copiable.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/Exceptions.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/FileException.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/Function.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/Function2.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/Property.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/Rotation.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/Rotation2D.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/Rotation3D.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/ShortArray.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/ShortMatrix.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/ShortSqrMatrix.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/StringMap.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/SyntaxError.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/TabulatedFunction.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/data/TabulatedFunction2.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/math/MathUtils.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/math/Vector1D.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/math/Vector2D.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/math/Vector3D.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/opti/OptiFunction.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/opti/OptiMethod.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/opti/OptiProblem.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/matl/ConstitutiveModel.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/matl/CriterionDictionary.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/matl/MaterialCriterion.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/matl/MaterialModel.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/matl/MaterialProperties.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/matl/MaterialState.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/MatLib4-LMGC-2021-1/src/matl/ModelDictionary.h"
    )
endif()

