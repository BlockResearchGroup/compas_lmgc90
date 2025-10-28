
set(all_files run_all.py
              utilities.py
   )


foreach(f ${all_files})
  file(COPY ${f}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
endforeach(f ${all_files})

if( ${WITH_HDF5} )

    add_test(NAME laws_elastic_repell
             COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/run_all.py --novisu 
            )
    set_property(TEST laws_elastic_repell
                 PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
                 PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
                 PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
                 PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
                )
    
endif( ${WITH_HDF5} )
