
### MISSING gen_sample : useless ####

set(test_name cubes_H20)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;quick" "" command.py ${ref_path} )

