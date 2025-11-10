# - Try to find SiconosNumerics
# Once done, this will define
#
#  SiconosNumerics_LIBRARY - the library or SiconosNumerics_LIBRARY-NOTFOUND

include(LibFindMacros)

find_library(SiconosNumerics_LIBRARIES
             NAMES siconos_numerics
             HINTS ${SiconosNumerics_LIBRARY_PATH}
            )

if( NOT SiconosNumerics_LIBRARIES )
  message(STATUS "SiconosNumerics library not found... to compile")
  set(BUILD_SICONOS_NUMERICS ON)
else( NOT SiconosNumerics_LIBRARIES )
  if( ${BUILD_MUMPS} )
    message(FATAL_ERROR "Cannot use external SiconosNumerics library and building our version of mumps...
  either build siconos by setting -DBUILD_SICONOS_NUMERICS=ON
  or provide the same version of mumps used by siconos by setting -DMUMPS_LIBRARY_DIRECTORY=/usr/lib/
  or -DMUMPS_LIBRARY=/usr/lib/libdmumps_seq.so -DMUMPS_INCLUDE_DIR=/usr/include"
           )
  endif( ${BUILD_MUMPS} )
  message(STATUS "Found SiconosNumerics library : ${SiconosNumerics_LIBRARIES}")
endif( NOT SiconosNumerics_LIBRARIES )
