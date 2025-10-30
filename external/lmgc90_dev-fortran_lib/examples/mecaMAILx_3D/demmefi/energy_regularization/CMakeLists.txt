
file(COPY pierre_mod.mat DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

set(test_name demmefi_energy_regul)
set(ref_path "examples/${dir_name}/${test_name}")
createTest( "${test_name}" "all;quick" gen_sample.py command.py ${ref_path})
