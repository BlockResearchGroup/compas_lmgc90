# Install script for directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/ChiPy

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

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/src/ChiPy/shared/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/src/ChiPy/mailx/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/src/ChiPy/contact_2D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/src/ChiPy/contact_3D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/src/ChiPy/rigid_2D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/src/ChiPy/rigid_3D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/src/ChiPy/mbs/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/src/ChiPy/kernel/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/src/ChiPy/post/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/src/ChiPy/user/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/src/ChiPy/pre_tools/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/src/ChiPy/utils/cmake_install.cmake")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}/home/pv/anaconda3/envs/lmgc90/lib/python3.12/site-packages/pylmgc90/chipy/_lmgc90.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}/home/pv/anaconda3/envs/lmgc90/lib/python3.12/site-packages/pylmgc90/chipy/_lmgc90.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}/home/pv/anaconda3/envs/lmgc90/lib/python3.12/site-packages/pylmgc90/chipy/_lmgc90.so"
         RPATH "/home/pv/anaconda3/envs/lmgc90/lib")
  endif()
  list(APPEND CMAKE_ABSOLUTE_DESTINATION_FILES
   "/home/pv/anaconda3/envs/lmgc90/lib/python3.12/site-packages/pylmgc90/chipy/_lmgc90.so")
  if(CMAKE_WARN_ON_ABSOLUTE_INSTALL_DESTINATION)
    message(WARNING "ABSOLUTE path INSTALL DESTINATION : ${CMAKE_ABSOLUTE_DESTINATION_FILES}")
  endif()
  if(CMAKE_ERROR_ON_ABSOLUTE_INSTALL_DESTINATION)
    message(FATAL_ERROR "ABSOLUTE path INSTALL DESTINATION forbidden (by caller): ${CMAKE_ABSOLUTE_DESTINATION_FILES}")
  endif()
file(INSTALL DESTINATION "/home/pv/anaconda3/envs/lmgc90/lib/python3.12/site-packages/pylmgc90/chipy" TYPE MODULE FILES "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/pylmgc90/chipy/_lmgc90.so")
  if(EXISTS "$ENV{DESTDIR}/home/pv/anaconda3/envs/lmgc90/lib/python3.12/site-packages/pylmgc90/chipy/_lmgc90.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}/home/pv/anaconda3/envs/lmgc90/lib/python3.12/site-packages/pylmgc90/chipy/_lmgc90.so")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}/home/pv/anaconda3/envs/lmgc90/lib/python3.12/site-packages/pylmgc90/chipy/_lmgc90.so"
         OLD_RPATH "/home/pv/anaconda3/envs/lmgc90/lib:/home/pv/brg/code_fortran/compas_lmgc90/build_dev/lib:"
         NEW_RPATH "/home/pv/anaconda3/envs/lmgc90/lib")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}/home/pv/anaconda3/envs/lmgc90/lib/python3.12/site-packages/pylmgc90/chipy/_lmgc90.so")
    endif()
  endif()
endif()

