
file(COPY box.msh
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name discreteFE_suspendedblock)
createTest( ${dir_name}_${test_name} quick gen.py run.py "examples/${dir_name}/${test_name}"  )
