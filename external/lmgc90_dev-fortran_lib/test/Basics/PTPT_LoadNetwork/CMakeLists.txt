
# python files to copy to build
set(py_files test.py
            )

# copy files to build
foreach(f ${py_files})
  file(COPY ${f}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
endforeach(f ${py_files})

# add test to run for 3D
set(test_name python_basic_PTPT_LoadNetwork_3D)
add_test( NAME ${test_name}
          COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py 3
        )
set_property(TEST ${test_name}
             PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
             PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
             PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
             PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
            )

# add test to run for 2D
set(test_name python_basic_PTPT_LoadNetwork_2D)
add_test( NAME ${test_name}
          COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py 2
        )
set_property(TEST ${test_name}
             PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
             PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
             PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
             PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
            )

