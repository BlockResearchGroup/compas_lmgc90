
file(COPY Arche_JP.msh
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name polyd_arche_dila)
createTest( ${dir_name}_${test_name} quick gen_sample.py command.py "examples/${dir_name}/${test_name}"  )
