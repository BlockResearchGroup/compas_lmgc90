# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/Pre/prepro_mesh2D/test_beamT3
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/Pre/prepro_mesh2D/test_beamT3
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
if(CTEST_CONFIGURATION_TYPE MATCHES "^([Nn][Oo][Nn]_[Rr][Ee][Gg])$")
  add_test(Pre_mesh2D_test_beamT3 "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/run_test.sh" "/home/pv/anaconda3/bin/python3" "Pre_mesh2D_test_beamT3" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/Pre/prepro_mesh2D/test_beamT3/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/Pre/prepro_mesh2D/test_beamT3/command.py" "/examples/Pre/prepro_mesh2D/test_beamT3")
  set_tests_properties(Pre_mesh2D_test_beamT3 PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran:" LABELS "quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;40;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/Pre/prepro_mesh2D/test_beamT3/CMakeLists.txt;3;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/Pre/prepro_mesh2D/test_beamT3/CMakeLists.txt;0;")
endif()
if(CTEST_CONFIGURATION_TYPE MATCHES "^([Ss][Aa][Vv][Ee]_[Rr][Ee][Gg])$")
  add_test(save_Pre_mesh2D_test_beamT3 "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/save_test.sh" "/home/pv/anaconda3/bin/python3" "Pre_mesh2D_test_beamT3" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/Pre/prepro_mesh2D/test_beamT3/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/Pre/prepro_mesh2D/test_beamT3/command.py" "/examples/Pre/prepro_mesh2D/test_beamT3")
  set_tests_properties(save_Pre_mesh2D_test_beamT3 PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran:" LABELS "quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;59;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/Pre/prepro_mesh2D/test_beamT3/CMakeLists.txt;3;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/Pre/prepro_mesh2D/test_beamT3/CMakeLists.txt;0;")
endif()
