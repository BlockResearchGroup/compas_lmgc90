# Install script for directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/MUMPS_THREADSAFE_4.9.2/include

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/home/pv/anaconda3/envs/lmgc90")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Debug")
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

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/MUMPS_THREADSAFE_4.9.2/include/mumps_compat.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/MUMPS_THREADSAFE_4.9.2/include/mumps_c_types.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/MUMPS_THREADSAFE_4.9.2/include/dmumps_c.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/MUMPS_THREADSAFE_4.9.2/include/dmumps_root.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/MUMPS_THREADSAFE_4.9.2/include/dmumps_struc.h"
    )
endif()

