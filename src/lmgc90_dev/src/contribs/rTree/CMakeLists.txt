add_library(librtree tree23.cpp)

if(${BUILD_rTree_BINDING})

  # strange windows specific when using cython
  if( ${WIN32} )
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DMS_WIN64")
  endif( ${WIN32} )

  add_library(libptree ptree.cpp)

  include_directories(${Python_ALL_INCLUDE}
                      ${CMAKE_CURRENT_SOURCE_DIR}
                     )

  if(${Python_VERSION_MAJOR} VERSION_GREATER 2)
      set(CYTHON_PY_FLAG "-3")
  else(${Python_VERSION_MAJOR} VERSION_GREATER 2)
      set(CYTHON_PY_FLAG "-2")
  endif(${Python_VERSION_MAJOR} VERSION_GREATER 2)

  add_custom_command(OUTPUT pyRtree.cxx COMMAND ${cython} --cplus ${CYTHON_PY_FLAG}
                     ${CMAKE_CURRENT_SOURCE_DIR}/pyRtree.pyx -o ${CMAKE_CURRENT_BINARY_DIR}/pyRtree.cxx
                    DEPENDS libptree ptree.h ptree.pxd pyRtree.pyx)
  add_library(pyRtree SHARED ${CMAKE_CURRENT_BINARY_DIR}/pyRtree.cxx)
  set_target_properties(pyRtree PROPERTIES PREFIX "")
  set_target_properties(pyRtree PROPERTIES SUFFIX ".so")
  target_link_libraries(pyRtree libptree ${Python_LIBRARIES})
  add_custom_command(TARGET pyRtree POST_BUILD
                     COMMAND ${CMAKE_COMMAND} -E copy
                             ${CMAKE_BINARY_DIR}/lib/pyRtree.so
                             ${CMAKE_BINARY_DIR}/pylmgc90/
                    )

  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test_rtree_binding.py
                 ${EXECUTABLE_OUTPUT_PATH}/test_rtree_binding.py
                )

  if( NOT ${NO_TEST} )
    add_test( NAME test_rtree_binding
              COMMAND ${Python_EXECUTABLE} ${EXECUTABLE_OUTPUT_PATH}/test_rtree_binding.py)
  set_property(TEST test_rtree_binding
               PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
               PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
              )
  endif( NOT ${NO_TEST} )

endif(${BUILD_rTree_BINDING})
