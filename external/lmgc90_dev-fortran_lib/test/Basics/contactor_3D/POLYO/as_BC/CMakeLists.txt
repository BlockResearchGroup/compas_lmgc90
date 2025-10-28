
set(copy_files test.py
               slope.msh
   )
foreach(f ${copy_files})
  file(COPY ${f}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
endforeach(f ${copy_files})

set(test_name "python_basic_contactor_3D_POLYO_as_BC")
add_test( NAME ${test_name}
          COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py
        )
  
set_property(TEST ${test_name}
             PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
             PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
             PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
             PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
            )

