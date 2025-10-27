# Install script for directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP

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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/clipper_project-build/libClipper2.a")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
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

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/clipper_project-build/libClipper2Z.a")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
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

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/pkgconfig" TYPE FILE FILES
    "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/clipper_project-build/Clipper2.pc"
    "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/clipper_project-build/Clipper2Z.pc"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/clipper_project-build/libClipper2.a")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
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

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/clipper_project-build/libClipper2Z.a")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
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

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2" TYPE FILE FILES
    "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/clipper_project-build/Clipper2Config.cmake"
    "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/clipper_project-build/Clipper2ConfigVersion.cmake"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2/Clipper2Targets.cmake")
    file(DIFFERENT EXPORT_FILE_CHANGED FILES
         "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2/Clipper2Targets.cmake"
         "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/clipper_project-build/CMakeFiles/Export/lib/cmake/clipper2/Clipper2Targets.cmake")
    if(EXPORT_FILE_CHANGED)
      file(GLOB OLD_CONFIG_FILES "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2/Clipper2Targets-*.cmake")
      if(OLD_CONFIG_FILES)
        message(STATUS "Old export file \"$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2/Clipper2Targets.cmake\" will be replaced.  Removing files [${OLD_CONFIG_FILES}].")
        file(REMOVE ${OLD_CONFIG_FILES})
      endif()
    endif()
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2" TYPE FILE FILES "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/clipper_project-build/CMakeFiles/Export/lib/cmake/clipper2/Clipper2Targets.cmake")
  if("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Dd][Ee][Bb][Uu][Gg])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/clipper2" TYPE FILE FILES "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/clipper_project-build/CMakeFiles/Export/lib/cmake/clipper2/Clipper2Targets-debug.cmake")
  endif()
endif()

