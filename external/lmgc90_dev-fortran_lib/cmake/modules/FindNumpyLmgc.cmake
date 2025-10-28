# - Find numpy
# Find the native numpy includes
# This module defines
#  NUMPY_INCLUDE_DIR, where to find numpy/arrayobject.h, etc.
#  NUMPY_FOUND, If false, do not try to use numpy headers.

if( NOT PYTHONINTERP_FOUND )
  find_package(PythonInterp REQUIRED)
endif( NOT PYTHONINTERP_FOUND )

if (NUMPY_INCLUDE_DIR)
  # in cache already
  set (NUMPY_FIND_QUIETLY TRUE)
endif (NUMPY_INCLUDE_DIR)

if( VENV_PATH )
  execute_process(COMMAND ${PYTHON_EXECUTABLE} -c "import sys; print(str(sys.version_info[0])+'.'+str(sys.version_info[1]))"
                  OUTPUT_VARIABLE PYTHON_VERSION
                  OUTPUT_STRIP_TRAILING_WHITESPACE
                 )

  set( PYTHON_VENV_PATH ${VENV_PATH}/lib/python${PYTHON_VERSION}/site-packages  )
  set( PYTHON_VENV_PATH ${PYTHON_VENV_PATH} PARENT_SCOPE )
 set( numpy_path    "import sys; sys.path.append('${PYTHON_VENV_PATH}'); import numpy; print(numpy.get_include())" )
 set( numpy_version "import sys; sys.path.append('${PYTHON_VENV_PATH}'); import numpy; print(numpy.__version__)" )
else( VENV_PATH )
 set( numpy_path    "import numpy; print(numpy.get_include())" )
 set( numpy_version "import numpy; print(numpy.__version__)" )
endif( VENV_PATH )

execute_process(COMMAND ${PYTHON_EXECUTABLE} -c "${numpy_path}"
                OUTPUT_VARIABLE NUMPY_INCLUDE_DIR
                OUTPUT_STRIP_TRAILING_WHITESPACE
                RESULT_VARIABLE NUMPY_NOT_FOUND
               )


if (NUMPY_NOT_FOUND)
  if(Numpy_FIND_REQUIRED)
    message (FATAL_ERROR "Numpy package/header is missing")
  endif (Numpy_FIND_REQUIRED)
else (NUMPY_NOT_FOUND)
  if (NOT NUMPY_FIND_QUIETLY)
    message (STATUS "Numpy headers found: ${NUMPY_INCLUDE_DIR}")
  endif (NOT NUMPY_FIND_QUIETLY)
endif (NUMPY_NOT_FOUND)

if (NUMPY_INCLUDE_DIR)
  set (NUMPY_FOUND TRUE)
  set (NUMPY_INCLUDE_DIR ${NUMPY_INCLUDE_DIR} CACHE STRING "Numpy include path")
else (NUMPY_INCLUDE_DIR)
  set(NUMPY_FOUND FALSE)
endif (NUMPY_INCLUDE_DIR)

MARK_AS_ADVANCED (NUMPY_INCLUDE_DIR)

# checking numpy.i file to include...
message(STATUS "numpy version : ${numpy_version}")
execute_process( COMMAND ${PYTHON_EXECUTABLE} -c "${numpy_version}"
                 OUTPUT_VARIABLE NUMPY_VERSION
                 OUTPUT_STRIP_TRAILING_WHITESPACE
               )

message(STATUS "numpy version : ${NUMPY_VERSION}")
string(REGEX REPLACE "([0-9]*)\\.([0-9]*)(\\.[0-9]*)?" "\\1\\2\\3"
       NUMPY_VERSION ${NUMPY_VERSION}
      )

if( NUMPY_VERSION GREATER 17)
  set(NUMPY_DOT_I ${CMAKE_SOURCE_DIR}/src/tools/swig/numpy-1.8.i)
elseif( NUMPY_VERSION EQUAL 17)
  set(NUMPY_DOT_I ${CMAKE_SOURCE_DIR}/src/tools/swig/numpy-1.7.i)
else( )
  set(NUMPY_DOT_I ${CMAKE_SOURCE_DIR}/src/tools/swig/numpy-1.6.i)
endif( )

