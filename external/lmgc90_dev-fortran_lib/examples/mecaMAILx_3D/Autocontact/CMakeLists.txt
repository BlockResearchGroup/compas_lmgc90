
file(COPY annulus_3D_scale.msh
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name Autocontact)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;long" gen_sample.py command.py ${ref_path} )

