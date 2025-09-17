# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.5)

file(MAKE_DIRECTORY
  "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/clipper2-1.4.0/CPP"
  "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/src/contribs/clipper_build"
  "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/src/contribs/clipper_project-prefix"
  "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/src/contribs/clipper_project-prefix/tmp"
  "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/src/contribs/clipper_project-prefix/src/clipper_project-stamp"
  "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/src/contribs/clipper_project-prefix/src"
  "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/src/contribs/clipper_project-prefix/src/clipper_project-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/src/contribs/clipper_project-prefix/src/clipper_project-stamp/${subDir}")
endforeach()
if(cfgdir)
  file(MAKE_DIRECTORY "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/src/contribs/clipper_project-prefix/src/clipper_project-stamp${cfgdir}") # cfgdir has leading slash
endif()
