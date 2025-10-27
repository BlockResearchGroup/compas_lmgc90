# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/Pre/prepro_mesh2D/test_2blocks_exploded
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_mesh2D/test_2blocks_exploded
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
if("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Jj][Uu][Ss][Tt]_[Rr][Uu][Nn])$")
  add_test(Pre_mesh2D_2_blocks_exploded "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/just_run_it.sh" "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "Pre_mesh2D_2_blocks_exploded" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_mesh2D/test_2blocks_exploded/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/Pre/prepro_mesh2D/test_2blocks_exploded/command.py")
  set_tests_properties(Pre_mesh2D_2_blocks_exploded PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi" LABELS "all;long" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;25;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/Pre/prepro_mesh2D/test_2blocks_exploded/CMakeLists.txt;6;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/Pre/prepro_mesh2D/test_2blocks_exploded/CMakeLists.txt;0;")
endif("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Jj][Uu][Ss][Tt]_[Rr][Uu][Nn])$")
