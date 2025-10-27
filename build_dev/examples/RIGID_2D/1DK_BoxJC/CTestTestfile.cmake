# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/1DK_BoxJC
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/1DK_BoxJC
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
if("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Nn][Oo][Nn]_[Rr][Ee][Gg])$")
  add_test(RIGID_2D_1DK_BoxJC "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/run_test.sh" "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "RIGID_2D_1DK_BoxJC" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/1DK_BoxJC/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/1DK_BoxJC/command.py" "/examples/RIGID_2D/1DK_BoxJC")
  set_tests_properties(RIGID_2D_1DK_BoxJC PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi" LABELS "quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;40;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/1DK_BoxJC/CMakeLists.txt;4;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/1DK_BoxJC/CMakeLists.txt;0;")
endif("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Nn][Oo][Nn]_[Rr][Ee][Gg])$")
if("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Ss][Aa][Vv][Ee]_[Rr][Ee][Gg])$")
  add_test(save_RIGID_2D_1DK_BoxJC "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/save_test.sh" "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "RIGID_2D_1DK_BoxJC" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/1DK_BoxJC/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/1DK_BoxJC/command.py" "/examples/RIGID_2D/1DK_BoxJC")
  set_tests_properties(save_RIGID_2D_1DK_BoxJC PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi" LABELS "quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;59;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/1DK_BoxJC/CMakeLists.txt;4;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/1DK_BoxJC/CMakeLists.txt;0;")
endif("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Ss][Aa][Vv][Ee]_[Rr][Ee][Gg])$")
