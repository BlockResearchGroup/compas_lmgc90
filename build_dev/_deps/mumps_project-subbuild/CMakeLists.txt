# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.18.4)

# We name the project and the target for the ExternalProject_Add() call
# to something that will highlight to the user what we are working on if
# something goes wrong and an error message is produced.

project(mumps_project-populate NONE)

include(ExternalProject)
ExternalProject_Add(mumps_project-populate
                     "UPDATE_DISCONNECTED" "False"
                    SOURCE_DIR          "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/contribs/MUMPS_THREADSAFE_4.9.2"
                    BINARY_DIR          "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/_deps/mumps_project-build"
                    CONFIGURE_COMMAND   ""
                    BUILD_COMMAND       ""
                    INSTALL_COMMAND     ""
                    TEST_COMMAND        ""
                    USES_TERMINAL_DOWNLOAD  YES
                    USES_TERMINAL_UPDATE    YES
)


