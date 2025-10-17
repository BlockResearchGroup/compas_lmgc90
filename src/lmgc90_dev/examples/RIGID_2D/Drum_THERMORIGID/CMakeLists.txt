
set(test_name Drum_THERMORIGID)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;slow" gen_drum.py command.py ${ref_path} )

