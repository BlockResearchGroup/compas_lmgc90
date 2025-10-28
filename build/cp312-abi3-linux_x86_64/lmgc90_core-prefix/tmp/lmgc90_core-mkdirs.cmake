# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.5)

file(MAKE_DIRECTORY
  "/home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib"
  "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core"
  "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core-prefix"
  "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core-prefix/tmp"
  "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core-prefix/src/lmgc90_core-stamp"
  "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core-prefix/src"
  "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core-prefix/src/lmgc90_core-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core-prefix/src/lmgc90_core-stamp/${subDir}")
endforeach()
if(cfgdir)
  file(MAKE_DIRECTORY "/home/pv/brg/code_fortran/compas_lmgc90/build/cp312-abi3-linux_x86_64/lmgc90_core-prefix/src/lmgc90_core-stamp${cfgdir}") # cfgdir has leading slash
endif()
