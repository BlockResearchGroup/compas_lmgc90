# Install script for directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/Core

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

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/src/Core/contribs/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/src/Core/shared/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/src/Core/rigid_2D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/src/Core/rigid_3D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/src/Core/mailx/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/src/Core/mbs/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/src/Core/contactor_2D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/src/Core/contactor_3D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/src/Core/contact_2D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/src/Core/contact_3D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/src/Core/kernel/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/src/Core/post/cmake_install.cmake")
endif()

