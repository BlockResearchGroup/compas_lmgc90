
if( BUILD_STANDALONE )

  file(COPY DATBOX input.txt
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
  configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/run_standalone.sh.in
                  ${CMAKE_CURRENT_BINARY_DIR}/run_standalone.sh
                 )

  add_test(NAME stand_alone_lmgc90
           COMMAND sh run_standalone.sh
          )

endif( BUILD_STANDALONE )
