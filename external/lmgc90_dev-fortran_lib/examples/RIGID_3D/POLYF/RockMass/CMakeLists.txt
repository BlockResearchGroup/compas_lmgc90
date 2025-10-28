

file(COPY essai_init.dat
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name RockMass)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;correct" all_resobloc.py command.py ${ref_path} )

