
file(COPY pierre_mod.mat DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

set(test_name demmefi_compression_temperature)
createTest( "${test_name}" "all;quick" gen_sample.py command.py "examples/${dir_name}/${test_name}")
