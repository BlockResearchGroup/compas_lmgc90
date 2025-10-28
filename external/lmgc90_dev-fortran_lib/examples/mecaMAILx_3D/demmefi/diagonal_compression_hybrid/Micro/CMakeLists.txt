
set(copy_files pierre_brick.mat
               pierre_mortar.mat
               mesh3D.msh
               )

foreach(f ${copy_files})
  file(COPY ${f}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
endforeach(f ${py_files})

set(test_name demmefi_compression_hybrid_micro)
set( ref_path "NO_COMP" )
createTest( "${test_name}" "all;slow" gen_sample.py command.py ${ref_path})

