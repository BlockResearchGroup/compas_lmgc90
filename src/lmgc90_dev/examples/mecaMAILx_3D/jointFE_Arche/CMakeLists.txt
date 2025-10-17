file(COPY display_gpv.py DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

set(test_name jointFE_Arche)
createTest( ${dir_name}_${test_name} "all;quick" gen.py command.py "examples/${dir_name}/${test_name}" )


