
file(COPY pierre_mod.mat DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

set(test_name demmefi_shear_hybrid)
set(ref_path "NO_COMP")
createTest( "${test_name}" "all;long" gen_sample.py command.py ${ref_path})
