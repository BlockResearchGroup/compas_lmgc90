
set(test_name 1J_comp_shear)
createTest( ${dir_name}_${test_name} "all;quick" gen_sample.py command_qs.py "examples/${dir_name}/${test_name}" )
