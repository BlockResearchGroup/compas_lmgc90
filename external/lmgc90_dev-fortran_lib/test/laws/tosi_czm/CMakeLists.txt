
set(all_files run_all.py
              utilities.py
              2D_incre_Normal_ref.txt
              2D_incre_Tangent_ref.txt
              2D_monor_Normal_ref.txt
              2D_monor_Tangent_ref.txt
              3D_incre_Normal_ref.txt
              3D_incre_Tangent_ref.txt
              3D_monor_Normal_ref.txt
              3D_monor_Tangent_ref.txt
   )


foreach(f ${all_files})
  file(COPY ${f}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
endforeach(f ${all_files})

if( ${WITH_HDF5} )

    add_test(NAME laws_tosi_czm
             COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/run_all.py --novisu 
             CONFIGURATIONS "slow"
            )
    set_property(TEST laws_tosi_czm
                 PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
                 PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
                 PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
                 PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
                )
    
endif( ${WITH_HDF5} )
