
file(COPY colonne.msh
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name polyd_1block_coro)
createTest( ${dir_name}_${test_name} quick gen_sample.py command.py "examples/${dir_name}/${test_name}"  )
