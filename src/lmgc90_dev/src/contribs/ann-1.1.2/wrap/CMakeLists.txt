
include(${SWIG_USE_FILE})

set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR}/pylmgc90/ann)

include_directories(${Python_ALL_INCLUDE})

#ann with euclidian norm python module
set(norm_type eucl)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/ann.i.in
               ${CMAKE_CURRENT_BINARY_DIR}/ann_eucl.i
              )

if(NOT ${Python_VERSION_MAJOR} LESS 3)
    set(CMAKE_SWIG_FLAGS "-py3")
else(NOT ${Python_VERSION_MAJOR} LESS 3)
    set(CMAKE_SWIG_FLAGS "")
endif(NOT ${Python_VERSION_MAJOR} LESS 3)
    
set_source_files_properties(${CMAKE_CURRENT_BINARY_DIR}/ann_eucl.i PROPERTIES CPLUSPLUS ON )
set(SWIG_MODULE_ann_eucl_EXTRA_DEPS ${NUMPY_DOT_I})

if( ${CMAKE_VERSION} VERSION_LESS 3.8 )
  SWIG_ADD_MODULE(ann_eucl python ${CMAKE_CURRENT_BINARY_DIR}/ann_eucl.i)
else( ${CMAKE_VERSION} VERSION_LESS 3.8 )
  SWIG_ADD_LIBRARY(ann_eucl LANGUAGE python SOURCES ${CMAKE_CURRENT_BINARY_DIR}/ann_eucl.i)
endif( ${CMAKE_VERSION} VERSION_LESS 3.8 )

#add_dependencies(${SWIG_MODULE_pyann_REAL_NAME} pyann_swig_docstrings)
swig_link_libraries(ann_eucl ${Python_LIBRARIES} ann_euclid)
add_custom_command(TARGET ${SWIG_MODULE_ann_eucl_REAL_NAME} POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/ann_eucl.py 
                                                      ${CMAKE_BINARY_DIR}/pylmgc90/ann/ann_eucl.py)


#ann with manhattan norm python module
set(norm_type manh)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/ann.i.in
               ${CMAKE_CURRENT_BINARY_DIR}/ann_manh.i
              )

if(NOT ${Python_VERSION_MAJOR} LESS 3)
    set(CMAKE_SWIG_FLAGS "-py3")
else(NOT ${Python_VERSION_MAJOR} LESS 3)
    set(CMAKE_SWIG_FLAGS "")
endif(NOT ${Python_VERSION_MAJOR} LESS 3)
    
set_source_files_properties(${CMAKE_CURRENT_BINARY_DIR}/ann_manh.i PROPERTIES CPLUSPLUS ON )
set(SWIG_MODULE_ann_manh_EXTRA_DEPS ${NUMPY_DOT_I})

if( ${CMAKE_VERSION} VERSION_LESS 3.8 )
  SWIG_ADD_MODULE(ann_manh python ${CMAKE_CURRENT_BINARY_DIR}/ann_manh.i)
else( ${CMAKE_VERSION} VERSION_LESS 3.8 )
  SWIG_ADD_LIBRARY(ann_manh LANGUAGE python SOURCES ${CMAKE_CURRENT_BINARY_DIR}/ann_manh.i)
endif( ${CMAKE_VERSION} VERSION_LESS 3.8 )

#add_dependencies(${SWIG_MODULE_pyann_REAL_NAME} pyann_swig_docstrings)
swig_link_libraries(ann_manh ${Python_LIBRARIES} ann_manhattan)
add_custom_command(TARGET ${SWIG_MODULE_ann_manh_REAL_NAME} POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/ann_manh.py 
                                                      ${CMAKE_BINARY_DIR}/pylmgc90/ann/ann_manh.py)


#to do only once
add_custom_target(ann ALL
                  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/__init__.py
                                                   ${CMAKE_BINARY_DIR}/pylmgc90/ann/
                  DEPENDS ${SWIG_MODULE_ann_eucl_REAL_NAME} ${SWIG_MODULE_ann_manh_REAL_NAME}
                 )

