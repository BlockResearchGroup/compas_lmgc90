
file(COPY pierre_M3.mat DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

set(test_name demmefi_compression_hybrid_macro)
set( ref_path "NO_COMP" )
createTest( "${test_name}" "all;slow" gen_sample.py command.py ${ref_path})

