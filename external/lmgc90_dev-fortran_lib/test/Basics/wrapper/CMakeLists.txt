
set(py_files run_all.py
             bodies.py
   )

foreach(f ${py_files})
  file(COPY ${f}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
endforeach(f ${py_files})
add_test(NAME python_basic_wrapper
         COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/run_all.py
        )
set_property(TEST python_basic_wrapper
             PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
             PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
             PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
             PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
            )
