
set(test_name big_disk_vs_wall)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;correct" gen_sample.py command.py ${ref_path} )


