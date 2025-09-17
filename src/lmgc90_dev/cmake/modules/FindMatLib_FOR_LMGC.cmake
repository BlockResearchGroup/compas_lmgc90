# - Try to find MatLib
# Once done, this will define
#
#  MATLIB_FOUND - system has MatLib
#  MATLIB_INCLUDE_DIR - the MatLib include directories
#  MATLIB_LIBRARY - link these to use MatLib

include(LibFindMacros)

# looking for matlib library
if( NOT BUILD_MATLIB )
  find_library(MATLIB_LIBRARY
               NAMES matlib
               PATHS ${MATLIB_PATH}
              )
endif( NOT BUILD_MATLIB )

if(NOT MATLIB_LIBRARY)
  set(BUILD_MATLIB True)
endif(NOT MATLIB_LIBRARY)

# Mettre test en fonction du compilateur
execute_process(COMMAND ${CMAKE_CXX_COMPILER} --print-libgcc-file
                OUTPUT_VARIABLE LIBGCC
                OUTPUT_STRIP_TRAILING_WHITESPACE
                RESULT_VARIABLE LIBGCC-NOTFOUND)

get_filename_component(STDC++_LIB_DIR ${LIBGCC} PATH)

find_library(STDC++_LIB
             NAMES stdc++
             PATHS ${STDC++_LIB_DIR}
                   ${STDC++_LIB_DIR}/..)

