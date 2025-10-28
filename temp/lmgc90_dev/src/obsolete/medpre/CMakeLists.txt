
include_directories(${HDF5_INCLUDE_DIR}
                    ${Python_INCLUDE_DIRS}
                    ${MED_INCLUDE_DIR}
                    ${CMAKE_CURRENT_SOURCE_DIR}
                   )
if(HDF5_IS_PARALLEL)
  include_directories(${MPI_INCLUDE_PATH})
endif(HDF5_IS_PARALLEL)

#include_directories(${CMAKE_SOURCE_DIR}/tools/swig)

set(LIBRARY_OUTPUT_PATH ${CMAKE_CURRENT_BINARY_DIR})

#add_library(med_pre ${CMAKE_CURRENT_SOURCE_DIR}/medpre.c)
#target_link_libraries(med_pre ${MED_LIB})

# defining swig target
if(NOT ${PYTHON_VERSION_MAJOR} LESS 3)
    set(CMAKE_SWIG_FLAGS "-py3")
else(NOT ${PYTHON_VERSION_MAJOR} LESS 3)
    set(CMAKE_SWIG_FLAGS "")
endif(NOT ${PYTHON_VERSION_MAJOR} LESS 3)
    
if( ${CMAKE_VERSION} VERSION_LESS 3.8 )
  SWIG_ADD_MODULE(medpre python ${CMAKE_CURRENT_SOURCE_DIR}/medpre.i medpre.c)
else( ${CMAKE_VERSION} VERSION_LESS 3.8 )
  SWIG_ADD_LIBRARY(medpre LANGUAGE python
                          SOURCES  ${CMAKE_CURRENT_SOURCE_DIR}/medpre.i medpre.c
                  )
endif( ${CMAKE_VERSION} VERSION_LESS 3.8 )

SWIG_LINK_LIBRARIES(medpre ${MED_LIBRARY}
                           ${HDF5_LIBRARIES}
                           ${PYTHON_LIBRARIES}
                   )

if(HDF5_IS_PARALLEL OR HDF5_HAVE_PARALLEL)
  SWIG_LINK_LIBRARIES(medpre ${MPI_LIBRARIES})
endif(HDF5_IS_PARALLEL OR HDF5_HAVE_PARALLEL)

add_dependencies(${SWIG_MODULE_medpre_REAL_NAME}
                 pre_py_module
                )
