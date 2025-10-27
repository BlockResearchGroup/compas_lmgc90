# Install script for directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/Pre/prepro_grains

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
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/Couette/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/box/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/box_cluster/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/box_disk_defo/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/box_polyg/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/polygons_from_mesh2D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/polyhedrons_from_mesh3D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/square_lattice/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/triangular_lattice/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/big_disk_vs_wall/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/big_sphere_vs_wall/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/box3D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/box_big_particles/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/cylinder3D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/drum/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/extruded_box/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/extruded_box_cluster/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/extruded_box_cylinders/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/extruded_drum/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/rough_box/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/rough_box3D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/rough_box_granulo/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/rough_box_granulo_polyg/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/rough_box_polyg/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_grains/smooth_box/cmake_install.cmake")
endif()

