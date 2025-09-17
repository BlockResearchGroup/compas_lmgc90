
file(COPY geometrie.dat 
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name arche_shrink)
createTest( ${dir_name}_${test_name} quick gen_sample.py command.py "examples/${dir_name}/${test_name}"  )

