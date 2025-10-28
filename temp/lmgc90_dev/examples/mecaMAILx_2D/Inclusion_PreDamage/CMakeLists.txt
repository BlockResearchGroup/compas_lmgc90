
file(COPY Inclusion.msh 
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(ref_path "NO_COMP" )
set(test_name Inclusion_PreDamage)
createTest( ${dir_name}_${test_name} long gen_sample.py command.py ${ref_path}  )

