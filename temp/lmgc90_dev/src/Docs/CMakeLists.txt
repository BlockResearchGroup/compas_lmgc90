
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/doxygen.cfg.in
               ${CMAKE_CURRENT_BINARY_DIR}/doxygen.cfg
              )

add_custom_target(docs ${DOXYGEN_EXECUTABLE}
                  ${CMAKE_CURRENT_BINARY_DIR}/doxygen.cfg
                 )

find_program(SPHINX_EXECUTABLE sphinx-build)
if( SPHINX_EXECUTABLE )
  ADD_SUBDIRECTORY(sphinx)
  add_dependencies(docs lmgc90_sphinx_docs)
endif( SPHINX_EXECUTABLE )
