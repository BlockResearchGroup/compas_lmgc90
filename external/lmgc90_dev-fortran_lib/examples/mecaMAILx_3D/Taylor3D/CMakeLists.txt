
file(COPY gmsh
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name Taylor3D)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;long" gen_sample.py command.py ${ref_path} )

set(test_name Taylor3DExplicit)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;long" gen_sample.py command_explicit.py ${ref_path} )

