
set(l_files test_2d.py
            test_3d.py
            gmsh
   )

foreach(f ${l_files})
  file(COPY ${f}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
endforeach(f ${l_files})

if( ${WITH_MATLIB} )
  add_test(NAME python_basic_recuprloc_2D
           COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test_2d.py &&
          )
  set_property(TEST python_basic_recuprloc_2D
               PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
               PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
              )

  add_test(NAME python_basic_recuprloc_3D
           COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test_3d.py &&
          )
  set_property(TEST python_basic_recuprloc_3D
               PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
               PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
              )
endif( ${WITH_MATLIB} )

