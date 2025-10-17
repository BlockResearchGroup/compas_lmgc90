# rm: Taken from Siconos but slightly modified...
#
# Find MUMPS includes and libraries.
# The following variables are set if MUMPS is found.  If MUMPS is not
# found, MUMPS_FOUND is set to false.
#  MUMPS_FOUND        - True when the MUMPS include directory is found.
#  MUMPS_INCLUDE_DIRS - the path to where the Siconos MUMPS include files are.
#  MUMPS_LIBRARY_DIRS - The path to where the Siconos library files are.
#  MUMPS_LIBRARIES    - The libraries to link against Siconos MUMPS

# One may want to use a specific MUMPS Library by setting
# MUMPS_LIRARIES and MUMPS_INCLUDE_DIRS
# or just to set some path to look in by setting
# MUMPS_LIBRARY_DIRECTORY, METIS_LIBRARY_DIRECTORY and MUMPS_INCLUDE_DIRECTORY
# before FIND_PACKAGE(MUMPS)

if( DEFINED MUMPS_LIBRARY AND DEFINED MUMPS_INCLUDE_DIR )
  set(MUMPS_FOUND TRUE)
  set(SPARSE_LIBRARIES ${MUMPS_LIBRARY})
  set(SPARSE_INCLUDE_DIR ${MUMPS_INCLUDE_DIR})
  return()
endif( DEFINED MUMPS_LIBRARY AND DEFINED MUMPS_INCLUDE_DIR )

if( MUMPS_LIBRARY_DIRECTORY )

  find_library(MUMPS_FOUND dmumps HINTS "${MUMPS_LIBRARY_DIRECTORY}" NO_DEFAULT_PATH)
  find_library(MUMPS_COMMON_FOUND mumps_common PATHS "${MUMPS_LIBRARY_DIRECTORY}"
                                               NO_DEFAULT_PATH)
  find_library(METIS_FOUND metis PATHS "${METIS_LIBRARY_DIRECTORY} ${MUMPS_LIBRARY_DIRECTORY}" NO_DEFAULT_PATH)

  if(NOT METIS_FOUND)
    # but let's also look for libpord
    find_library(PORD_FOUND pord PATHS "${MUMPS_LIBRARY_DIRECTORY}" NO_DEFAULT_PATH)
  endif()

  if( MUMPS_FOUND )
    set(MUMPS_LIBRARIES ${MUMPS_FOUND})
    find_path(MUMPS_INCLUDE_DIRS dmumps_struc.h NO_DEFAULT_PATH
              PATHS ${MUMPS_INCLUDE_DIRECTORY} ${MUMPS_LIBRARY_DIRECTORY}/..
              PATH_SUFFIXES include include/MUMPS
             )
    if(NOT MUMPS_INCLUDE_DIRS )
      message(FATAL_ERROR "Required MUMPS include not found. Please specify include directory location in MUMPS_INCLUDE_DIRECTORY")
    endif(NOT MUMPS_INCLUDE_DIRS )
  endif( MUMPS_FOUND )

else( MUMPS_LIBRARY_DIRECTORY )

  find_library(MUMPS_FOUND dmumps )
  find_library(MUMPS_COMMON_FOUND mumps_common )
  find_library(METIS_FOUND metis )

  if(NOT METIS_FOUND )
    # but let's also look for libpord
    find_library(PORD_FOUND pord )
  endif()

  if( MUMPS_FOUND )
    set(MUMPS_LIBRARIES ${MUMPS_FOUND})
    find_path(MUMPS_INCLUDE_DIRS dmumps_struc.h)
    if(NOT MUMPS_INCLUDE_DIRS )
      message(FATAL_ERROR "Required MUMPS include not found.")
    endif(NOT MUMPS_INCLUDE_DIRS )
  endif( MUMPS_FOUND )

endif( MUMPS_LIBRARY_DIRECTORY )

IF( MUMPS_FOUND )

  if( MUMPS_COMMON_FOUND )
    SET(MUMPS_LIBRARIES ${MUMPS_LIBRARIES} ${MUMPS_COMMON_FOUND})
  endif( MUMPS_COMMON_FOUND )

  if( METIS_FOUND )
    SET(MUMPS_LIBRARIES ${MUMPS_LIBRARIES} ${METIS_FOUND})
  endif( METIS_FOUND )

  if( PORD_FOUND )
    SET(MUMPS_LIBRARIES ${MUMPS_LIBRARIES} ${PORD_FOUND})
  endif( PORD_FOUND )

ENDIF( MUMPS_FOUND )

set(SPARSE_LIBRARIES ${MUMPS_LIBRARIES})
set(SPARSE_INCLUDE_DIR ${MUMPS_INCLUDE_DIRS})


