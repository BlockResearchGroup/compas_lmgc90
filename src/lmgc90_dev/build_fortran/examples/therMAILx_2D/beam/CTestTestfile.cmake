# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/therMAILx_2D/beam
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/therMAILx_2D/beam
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
if(CTEST_CONFIGURATION_TYPE MATCHES "^([Nn][Oo][Nn]_[Rr][Ee][Gg])$")
  add_test(therMAILx_2D_beam "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/run_test.sh" "/home/pv/anaconda3/bin/python3" "therMAILx_2D_beam" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/therMAILx_2D/beam/command.py" "/examples/therMAILx_2D/beam")
  set_tests_properties(therMAILx_2D_beam PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran:" LABELS "quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;40;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/therMAILx_2D/beam/CMakeLists.txt;7;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/therMAILx_2D/beam/CMakeLists.txt;0;")
endif()
if(CTEST_CONFIGURATION_TYPE MATCHES "^([Ss][Aa][Vv][Ee]_[Rr][Ee][Gg])$")
  add_test(save_therMAILx_2D_beam "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/save_test.sh" "/home/pv/anaconda3/bin/python3" "therMAILx_2D_beam" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/therMAILx_2D/beam/command.py" "/examples/therMAILx_2D/beam")
  set_tests_properties(save_therMAILx_2D_beam PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran:" LABELS "quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;59;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/therMAILx_2D/beam/CMakeLists.txt;7;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/therMAILx_2D/beam/CMakeLists.txt;0;")
endif()
