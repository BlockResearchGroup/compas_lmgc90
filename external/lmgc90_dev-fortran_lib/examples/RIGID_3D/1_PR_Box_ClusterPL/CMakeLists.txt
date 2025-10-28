
### MISSING gen_sample : useless ####

set(test_name 1_PR_Box_ClusterPL)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} all "" command.py ${ref_path} )

