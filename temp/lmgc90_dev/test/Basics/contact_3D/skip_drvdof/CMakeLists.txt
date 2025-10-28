# python and brep files to copy to build
set(py_files gen_sample.py
             command.py
             run_all.py
            )

foreach(f ${py_files})
  file(COPY ${f}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
endforeach(f ${py_files})

# add test to run
add_test( NAME python_basic_3D_skip_drvdof
          COMMAND ${Python_EXECUTABLE} run_all.py )

set_property(TEST python_basic_3D_skip_drvdof
             PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
             PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
             PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
             PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
            )

