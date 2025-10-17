# Install script for directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP

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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/_deps/clipper_project-build/libClipper2.a")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/clipper2" TYPE FILE FILES
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.version.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.core.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.engine.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.export.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.minkowski.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.offset.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.rectclip.h"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/_deps/clipper_project-build/libClipper2Z.a")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/clipper2" TYPE FILE FILES
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.version.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.core.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.engine.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.export.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.minkowski.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.offset.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.rectclip.h"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/pkgconfig" TYPE FILE FILES
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/_deps/clipper_project-build/Clipper2.pc"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/_deps/clipper_project-build/Clipper2Z.pc"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/_deps/clipper_project-build/libClipper2.a")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/clipper2" TYPE FILE FILES
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.version.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.core.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.engine.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.export.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.minkowski.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.offset.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.rectclip.h"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/_deps/clipper_project-build/libClipper2Z.a")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/clipper2" TYPE FILE FILES
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.version.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.core.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.engine.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.export.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.minkowski.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.offset.h"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP/Clipper2Lib/include/clipper2/clipper.rectclip.h"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2" TYPE FILE FILES
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/_deps/clipper_project-build/Clipper2Config.cmake"
    "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/_deps/clipper_project-build/Clipper2ConfigVersion.cmake"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2/Clipper2Targets.cmake")
    file(DIFFERENT _cmake_export_file_changed FILES
         "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2/Clipper2Targets.cmake"
         "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/_deps/clipper_project-build/CMakeFiles/Export/d8256b042a676486d1b2b19831a8c686/Clipper2Targets.cmake")
    if(_cmake_export_file_changed)
      file(GLOB _cmake_old_config_files "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2/Clipper2Targets-*.cmake")
      if(_cmake_old_config_files)
        string(REPLACE ";" ", " _cmake_old_config_files_text "${_cmake_old_config_files}")
        message(STATUS "Old export file \"$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2/Clipper2Targets.cmake\" will be replaced.  Removing files [${_cmake_old_config_files_text}].")
        unset(_cmake_old_config_files_text)
        file(REMOVE ${_cmake_old_config_files})
      endif()
      unset(_cmake_old_config_files)
    endif()
    unset(_cmake_export_file_changed)
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2" TYPE FILE FILES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/_deps/clipper_project-build/CMakeFiles/Export/d8256b042a676486d1b2b19831a8c686/Clipper2Targets.cmake")
  if(CMAKE_INSTALL_CONFIG_NAME MATCHES "^([Rr][Ee][Ll][Ee][Aa][Ss][Ee])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2" TYPE FILE FILES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_core_min_oneapi/_deps/clipper_project-build/CMakeFiles/Export/d8256b042a676486d1b2b19831a8c686/Clipper2Targets-release.cmake")
  endif()
endif()

