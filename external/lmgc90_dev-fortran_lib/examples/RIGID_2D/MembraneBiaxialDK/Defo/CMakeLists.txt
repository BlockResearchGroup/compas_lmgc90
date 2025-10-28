
file(COPY DATBOX
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name Defo)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;slow" "" command.py ${ref_path} )


#set_property(TEST MembraneBiaxialDK_Defo
#             PROPERTY TIMEOUT "3600"
#            )
