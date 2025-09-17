#and look for hdf5 files and library
find_package(HDF5 REQUIRED)

message(WARNING "Med library must be at least 3.0 be usable.")

include(CheckSymbolExists)
check_symbol_exists(H5_HAVE_PARALLEL ${HDF5_INCLUDE_DIR}/H5pubconf.h HDF5_HAVE_PARALLEL)
if( HDF5_HAVE_PARALLEL AND NOT HDF5_IS_PARALLEL)
  set(HDF5_IS_PARALLEL True)
endif( HDF5_HAVE_PARALLEL AND NOT HDF5_IS_PARALLEL)
if(HDF5_IS_PARALLEL)
  find_package(MPI REQUIRED)
  message(STATUS "MPI_LIBRARIES : ${MPI_LIBRARIES}")
  message(STATUS "MPI_INCLUDE_PATH : ${MPI_INCLUDE_PATH}")
endif(HDF5_IS_PARALLEL)
message(STATUS "HDF5_LIBRARIES : ${HDF5_LIBRARIES}")
message(STATUS "HDF5_INCLUDE_DIRS : ${HDF5_INCLUDE_DIRS}")
message(STATUS "HDF5_INCLUDE_DIR : ${HDF5_INCLUDE_DIR}")

#if MED_PATH is given look for library in MED_PATH/lib
if(MED_PATH)
  find_library(MED_LIBRARY
               NAMES medC
               PATHS ${MED_PATH}/lib
              )
else(MED_PATH)
  #if no MED_PATH but MED_PATH_LIB look for library in it
  if(MED_LIB_PATH)
    find_library(MED_LIBRARY
                 NAME medC
                 PATHS ${MED_LIB_PATH}
                )
  else(MED_LIB_PATH)
    #if ASTER_ROOT environment variable exists, it is looked into it too
    find_library(MED_LIBRARY
                 NAME medC
                 PATHS ENV ASTER_ROOT
                 PATH_SUFFIXES public/med-3.0.4/lib med-3.0.5/lib
                )
  endif(MED_LIB_PATH)
endif(MED_PATH)

#if libmedC was found, search for med.h file
if(MED_LIBRARY)

  if( MED_INC_PATH )
    message(STATUS "looking with MED_INC_PATH")
    find_path(MED_INCLUDE_DIR
              NAMES med.h
              HINTS ${MED_INC_PATH}
             )
  endif( MED_INC_PATH )

  if( MED_PATH AND NOT MED_INCLUDE_DIR )
    message(STATUS "looking with MED_PATH: ${MED_PATH}")
    find_path(MED_INCLUDE_DIR
              NAMES med.h
              HINTS ${MED_PATH}/include
             )
    message(STATUS "found MED_PATH: ${MED_INCLUDE}")
  endif( MED_PATH AND NOT MED_INCLUDE_DIR )

  if( NOT MED_INCLUDE_DIR )
    get_filename_component(MED_LIB_DIR ${MED_LIBRARY} PATH)
    get_filename_component(MED_LIB_DIR_DIR ${MED_LIB_DIR} PATH)
    find_path(MED_INCLUDE_DIR
              NAMES med.h
              PATH  ${MED_LIB_DIR_DIR}/include
             )
  endif( NOT MED_INCLUDE_DIR )

endif(MED_LIBRARY)

if(MED_LIBRARY AND MED_INCLUDE_DIR)
  set(MED_FOUND TRUE)
  message(STATUS "med include : ${MED_INCLUDE_DIR}")
  message(STATUS "med library : ${MED_LIBRARY}")
else(MED_LIBRARY AND MED_INCLUDE_DIR)
  set(MED_FOUND FALSE)
  message(STATUS "med include : ${MED_INCLUDE_DIR}")
  message(STATUS "med library : ${MED_LIBRARY}")
  message(STATUS "use MED_PATH to give path to med install path
   or use MED_LIB_PATH and MED_INC_PATH to med libraries and include files")
endif(MED_LIBRARY AND MED_INCLUDE_DIR)

