
file(COPY DATBOX
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

file(COPY MESH
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name Stokes_Tube)
createTest( ${dir_name}_${test_name} quick gen_sample.py command.py "examples/${dir_name}/${test_name}"  )

