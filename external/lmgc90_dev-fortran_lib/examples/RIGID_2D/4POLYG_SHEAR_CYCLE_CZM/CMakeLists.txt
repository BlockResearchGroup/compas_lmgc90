

file(COPY cycles_tanh.py DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

set(test_name 4POLYG_SHEAR_CYCLE_CZM)
createTest( ${dir_name}_${test_name} quick gen_sample.py command.py "examples/${dir_name}/${test_name}"  )

