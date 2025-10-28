# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.5)

file(MAKE_DIRECTORY
  "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib/src/contribs/clipper2-1.4.0/CPP"
  "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/_deps/clipper_project-build"
  "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/_deps/clipper_project-subbuild/clipper_project-populate-prefix"
  "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/_deps/clipper_project-subbuild/clipper_project-populate-prefix/tmp"
  "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/_deps/clipper_project-subbuild/clipper_project-populate-prefix/src/clipper_project-populate-stamp"
  "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/_deps/clipper_project-subbuild/clipper_project-populate-prefix/src"
  "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/_deps/clipper_project-subbuild/clipper_project-populate-prefix/src/clipper_project-populate-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/_deps/clipper_project-subbuild/clipper_project-populate-prefix/src/clipper_project-populate-stamp/${subDir}")
endforeach()
if(cfgdir)
  file(MAKE_DIRECTORY "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core/_deps/clipper_project-subbuild/clipper_project-populate-prefix/src/clipper_project-populate-stamp${cfgdir}") # cfgdir has leading slash
endif()
