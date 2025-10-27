# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_RIGID_3D/2_briques
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/mecaMAILx_RIGID_3D/2_briques
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
if("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Jj][Uu][Ss][Tt]_[Rr][Uu][Nn])$")
  add_test(mecaMAILx_RIGID_3D_2_briques "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/just_run_it.sh" "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "mecaMAILx_RIGID_3D_2_briques" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/mecaMAILx_RIGID_3D/2_briques/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/mecaMAILx_RIGID_3D/2_briques/command.py")
  set_tests_properties(mecaMAILx_RIGID_3D_2_briques PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi" LABELS "all;quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;25;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_RIGID_3D/2_briques/CMakeLists.txt;10;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_RIGID_3D/2_briques/CMakeLists.txt;0;")
endif("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Jj][Uu][Ss][Tt]_[Rr][Uu][Nn])$")
