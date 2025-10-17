
file(COPY abeille2.dat
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name Vault)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;quick" gen_with_gmsh.py command.py ${ref_path} )


