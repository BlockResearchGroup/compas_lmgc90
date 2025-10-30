
set(test_name jointFE_traction_shear)
createTest( ${dir_name}_${test_name} "all;quick" gen.py command.py  "examples/${dir_name}/${test_name}")
