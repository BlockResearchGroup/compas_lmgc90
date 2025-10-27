# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/1024DK_BoxJC
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/1024DK_BoxJC
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
if("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Jj][Uu][Ss][Tt]_[Rr][Uu][Nn])$")
  add_test(RIGID_2D_1024DK_BoxJC "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/just_run_it.sh" "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "RIGID_2D_1024DK_BoxJC" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/1024DK_BoxJC/command.py")
  set_tests_properties(RIGID_2D_1024DK_BoxJC PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi" LABELS "all;slow" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;25;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/1024DK_BoxJC/CMakeLists.txt;10;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/1024DK_BoxJC/CMakeLists.txt;0;")
endif("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Jj][Uu][Ss][Tt]_[Rr][Uu][Nn])$")
if("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Jj][Uu][Ss][Tt]_[Rr][Uu][Nn])$")
  add_test(RIGID_2D_1024DK_BoxJC_post "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/just_run_it.sh" "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "RIGID_2D_1024DK_BoxJC_post" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/RIGID_2D/1024DK_BoxJC/command_post.py")
  set_tests_properties(RIGID_2D_1024DK_BoxJC_post PROPERTIES  DEPENDS "RIGID_2D_1024DK_BoxJC" ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi" LABELS "all;slow" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;25;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/1024DK_BoxJC/CMakeLists.txt;11;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/1024DK_BoxJC/CMakeLists.txt;0;")
endif("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Jj][Uu][Ss][Tt]_[Rr][Uu][Nn])$")
