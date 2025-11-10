
set(py_files run_all.py
             tact_behav.py
             bulk_behav.py
             models.py
             order.py
             bodies_2d.py
             bodies_3d.py
             xxl.py
             last.py
             utils.py
             barre1D.msh
   )

foreach(f ${py_files})
  file(COPY ${f}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
endforeach(f ${py_files})

if( ${WITH_HDF5} )
  add_subdirectory(hdf5)
  set( py_opt "--with-hdf5" )
endif( ${WITH_HDF5} )

add_test(NAME python_basic_io
         COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/run_all.py ${py_opt}
        )
add_test(NAME python_basic_io_xxl
         COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/xxl.py
         CONFIGURATIONS "non_reg"
        )

set_property(TEST python_basic_io
             PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
             PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
             PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
             PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
            )
set_property(TEST python_basic_io_xxl
             PROPERTY LABEL "long"
             PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
             PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
             PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
             PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
            )




