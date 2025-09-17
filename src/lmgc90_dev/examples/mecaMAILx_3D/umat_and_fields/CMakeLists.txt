
file(COPY pierre.mat
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name umat_and_fields)
set( ref_path "examples/${dir_name}/${test_name}" )

createTest( ${dir_name}_${test_name} "all;quick" gen.py run.py ${ref_path} )

