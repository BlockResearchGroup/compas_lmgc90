
file(COPY gmsh
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name polygons_from_mesh2D)
createTest( ${dir_name}_${test_name} long gen_sample.py command.py "examples/Pre/prepro_grains/${test_name}"  )

