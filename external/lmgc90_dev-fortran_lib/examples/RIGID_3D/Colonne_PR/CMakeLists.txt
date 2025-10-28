
set(test_name Colonne_PR)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;quick" gen_sample.py command_for_dummies.py ${ref_path} )


