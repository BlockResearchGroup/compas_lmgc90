
set(py_files run_all.py
             detection_3D.py
             post_mortem.py
   )

foreach(f ${py_files})
  file(COPY ${f}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
endforeach(f ${py_files})

if( ${WITH_HDF5} AND ${WITH_MATLIB} )
  add_test( NAME python_basic_detection_3D
            COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/run_all.py
          )
  
  set_property(TEST python_basic_detection_3D
               PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
               PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
              )
endif( ${WITH_HDF5} AND ${WITH_MATLIB} )

