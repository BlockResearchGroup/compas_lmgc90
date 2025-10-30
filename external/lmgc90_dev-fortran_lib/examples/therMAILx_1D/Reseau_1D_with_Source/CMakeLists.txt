
file(COPY MESH
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

set(test_name Reseau1D_with_source)
createTest( ${dir_name}_${test_name} "all;quick" gen_sample.py command.py ${ref_path} )

