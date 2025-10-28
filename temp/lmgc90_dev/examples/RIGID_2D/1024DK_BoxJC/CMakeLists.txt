
file(COPY DATBOX
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name 1024DK_BoxJC)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;slow" "" command.py ${ref_path} )
createTest( ${dir_name}_${test_name}_post "all;slow" "" command_post.py ${ref_path} )
set_tests_properties(${dir_name}_${test_name}_post
                     PROPERTIES DEPENDS ${dir_name}_${test_name})





