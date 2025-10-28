
install( PROGRAMS cleanlmgc
                  mkdirlmgc
                  compactlmgc
                  uncompactlmgc
                  update_gen_2016
         DESTINATION bin
       )


#copying gmshutils module to build directory
add_custom_target(gmshutils_module ALL
                  ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/gmshutils.py
                                           ${CMAKE_BINARY_DIR}/pylmgc90/gmshutils.py
                 )
