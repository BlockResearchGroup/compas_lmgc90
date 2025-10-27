# Install script for directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D

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
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/1DK_BoxJC/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/4POLYG_SHEAR_CYCLE_CZM/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/BouncingBallsDK/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/BridgePL_GroundJC/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/POLYG_Vther/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/ViscoElasticRepell_BouncingBall/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/arche_plein_ceintre_2D/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/arche_shrink/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/clusterDKPL_GroundJC/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/poteau_shrink/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/1024DK_BoxJC/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/1024DK_BoxJC_ElectricalConduction/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/1024DK_BoxJC_ThermalConduction/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/200DK_BoxJC_MD/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/500PL_PerioBoxJC/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/DISKx_Vther/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/Drum_THERMORIGID/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/MembraneBiaxialDK/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/SourcePointPL_BoxJC/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/WallPL_GroundJC/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/basic_NARD/cmake_install.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for the subdirectory.
  include("/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/siloDK_hoperJC/cmake_install.cmake")
endif()

