
set(test_name 1SP_PR)
set( ref_path "examples/${dir_name}/${test_name}" )

createTest( ${dir_name}_${test_name} "all;quick" gen_sample.py command.py ${ref_path} )

