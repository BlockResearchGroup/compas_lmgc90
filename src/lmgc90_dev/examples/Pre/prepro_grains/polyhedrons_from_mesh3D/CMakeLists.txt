
file(COPY gmsh
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name polyhedrons_from_mesh3D)
createTest( ${dir_name}_${test_name} quick gen_sample.py command.py "examples/Pre/prepro_grains/${test_name}"  )

