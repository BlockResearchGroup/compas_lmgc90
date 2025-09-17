# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/demmefi/compression_temperature
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/mecaMAILx_3D/demmefi/compression_temperature
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
if(CTEST_CONFIGURATION_TYPE MATCHES "^([Nn][Oo][Nn]_[Rr][Ee][Gg])$")
  add_test(demmefi_compression_temperature "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/run_test.sh" "/home/pv/anaconda3/bin/python3" "demmefi_compression_temperature" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/mecaMAILx_3D/demmefi/compression_temperature/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/mecaMAILx_3D/demmefi/compression_temperature/command.py" "/examples/mecaMAILx_3D/demmefi/demmefi_compression_temperature")
  set_tests_properties(demmefi_compression_temperature PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran:" LABELS "all;quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;40;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/demmefi/compression_temperature/CMakeLists.txt;5;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/demmefi/compression_temperature/CMakeLists.txt;0;")
endif()
if(CTEST_CONFIGURATION_TYPE MATCHES "^([Ss][Aa][Vv][Ee]_[Rr][Ee][Gg])$")
  add_test(save_demmefi_compression_temperature "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/save_test.sh" "/home/pv/anaconda3/bin/python3" "demmefi_compression_temperature" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/mecaMAILx_3D/demmefi/compression_temperature/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/mecaMAILx_3D/demmefi/compression_temperature/command.py" "/examples/mecaMAILx_3D/demmefi/demmefi_compression_temperature")
  set_tests_properties(save_demmefi_compression_temperature PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran:" LABELS "all;quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;59;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/demmefi/compression_temperature/CMakeLists.txt;5;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/demmefi/compression_temperature/CMakeLists.txt;0;")
endif()
