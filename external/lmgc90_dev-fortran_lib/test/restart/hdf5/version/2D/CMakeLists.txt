
if( ${WITH_MATLIB} )

  set(py_files DATBOX
               v0_1.h5
               v0_2.h5
               read_file.py
               read_pre.py
     )
  foreach(f ${py_files})
    file(COPY ${f}
         DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
        )
  endforeach(f ${py_files})
  
  add_test(NAME python_restart_hfile_2D
           COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/read_file.py &&
          )
  
  set_property(TEST python_restart_hfile_2D
               PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
               PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
              )

  add_test(NAME python_read_pre_hfile_2D
           COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/read_pre.py &&
          )

  set_property(TEST python_read_pre_hfile_2D
               PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
               PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
              )

endif( ${WITH_MATLIB} )
