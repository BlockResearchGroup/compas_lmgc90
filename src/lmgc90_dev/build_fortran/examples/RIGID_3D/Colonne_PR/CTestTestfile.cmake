# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_3D/Colonne_PR
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/RIGID_3D/Colonne_PR
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
if(CTEST_CONFIGURATION_TYPE MATCHES "^([Jj][Uu][Ss][Tt]_[Rr][Uu][Nn])$")
  add_test(RIGID_3D_Colonne_PR "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/just_run_it.sh" "/home/pv/anaconda3/bin/python3" "RIGID_3D_Colonne_PR" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/RIGID_3D/Colonne_PR/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/RIGID_3D/Colonne_PR/command_for_dummies.py")
  set_tests_properties(RIGID_3D_Colonne_PR PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran:" LABELS "all;quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;25;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_3D/Colonne_PR/CMakeLists.txt;6;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_3D/Colonne_PR/CMakeLists.txt;0;")
endif()
