# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.5)

file(MAKE_DIRECTORY
  "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/MatLib4-LMGC-2021-1"
  "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/matlib_project-build"
  "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/matlib_project-subbuild/matlib_project-populate-prefix"
  "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/matlib_project-subbuild/matlib_project-populate-prefix/tmp"
  "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/matlib_project-subbuild/matlib_project-populate-prefix/src/matlib_project-populate-stamp"
  "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/matlib_project-subbuild/matlib_project-populate-prefix/src"
  "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/matlib_project-subbuild/matlib_project-populate-prefix/src/matlib_project-populate-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/matlib_project-subbuild/matlib_project-populate-prefix/src/matlib_project-populate-stamp/${subDir}")
endforeach()
if(cfgdir)
  file(MAKE_DIRECTORY "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/matlib_project-subbuild/matlib_project-populate-prefix/src/matlib_project-populate-stamp${cfgdir}") # cfgdir has leading slash
endif()
