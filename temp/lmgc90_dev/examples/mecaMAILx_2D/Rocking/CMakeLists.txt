
file(COPY block.msh 
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )
file(COPY groun.msh 
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name Rocking)
createTest( ${dir_name}_${test_name} quick gen_sample.py command.py "examples/${dir_name}/${test_name}"  )

