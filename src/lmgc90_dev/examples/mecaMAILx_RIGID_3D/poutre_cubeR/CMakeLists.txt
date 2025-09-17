
file(COPY DATBOX
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name poutre_cubeR)
createTest( ${dir_name}_${test_name} quick "" command.py "examples/${dir_name}/${test_name}"  )

