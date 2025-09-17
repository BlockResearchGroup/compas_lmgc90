
set(py_files gen_sample.py
             computation.py
             test.py
   )

foreach(f ${py_files})
  file(COPY ${f}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
endforeach(f ${py_files})

add_test(NAME python_restart_postpro_2d
         COMMAND ${Python_EXECUTABLE} test.py --novisu
        )
set_property(TEST python_restart_postpro_2d
             PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
             PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
             PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
             PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
            )


