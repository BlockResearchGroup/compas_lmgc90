
# Find UMFPACK include and library.

# The following variables are set if UMFPACK is found.  If UMFPACK is not
# found, UMFPACK_FOUND is set to false.
#  UMFPACK_FOUND        - True when the UMFPACK library is found.
#  UMFPACK_INCLUDE_DIR - the path to where the UMFPACK include file is.
#  UMFPACK_LIBRARY     - The library to link against UMFPACK

# One may want to use a specific UMFPACK Library by setting
# UMFPACK_LIBRARY_DIRECTORY and UMFPACK_INCLUDE_DIRECTORY
# before FIND_PACKAGE(UMFPACK)

message(STATUS "Looking for umfpack")
if( ${UMFPACK_LIBRARY_DIRECTORY} )
  find_library(UMFPACK_FOUND umfpack
               HINTS "${UMFPACK_LIBRARY_DIRECTORY}"
               NO_DEFAULT_PATH REQUIRED)
else( ${UMFPACK_LIBRARY_DIRECTORY} )
  find_library(UMFPACK_FOUND umfpack)
endif( ${UMFPACK_LIBRARY_DIRECTORY} )

if( ${UMFPACK_INCLUDE_DIRECTORY} )
  find_path(UMFPACK_INCLUDE_DIR umfpack.h
            HINTS "${UMFPACK_INCLUDE_DIRECTORY}"
            NO_DEFAULT_PATH)
else( ${UMFPACK_INCLUDE_DIRECTORY} )
  find_path(UMFPACK_INCLUDE_DIR umfpack.h
            PATH_SUFFIXES suitesparse)
endif( ${UMFPACK_INCLUDE_DIRECTORY} )

message(STATUS "UMFPACK LIBRARY: ${UMFPACK_FOUND}")
message(STATUS "UMFPACK INCLUDE: ${UMFPACK_INCLUDE_DIR}")

set(UMFPACK_LIBRARY ${UMFPACK_FOUND})

