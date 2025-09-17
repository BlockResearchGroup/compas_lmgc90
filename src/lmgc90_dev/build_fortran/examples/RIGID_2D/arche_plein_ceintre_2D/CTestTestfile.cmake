# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/arche_plein_ceintre_2D
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/RIGID_2D/arche_plein_ceintre_2D
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
if(CTEST_CONFIGURATION_TYPE MATCHES "^([Nn][Oo][Nn]_[Rr][Ee][Gg])$")
  add_test(RIGID_2D_arche_plein_ceintre_2D "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/run_test.sh" "/home/pv/anaconda3/bin/python3" "RIGID_2D_arche_plein_ceintre_2D" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/RIGID_2D/arche_plein_ceintre_2D/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/RIGID_2D/arche_plein_ceintre_2D/command.py" "/examples/RIGID_2D/arche_plein_ceintre_2D")
  set_tests_properties(RIGID_2D_arche_plein_ceintre_2D PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran:" LABELS "quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;40;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/arche_plein_ceintre_2D/CMakeLists.txt;3;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/arche_plein_ceintre_2D/CMakeLists.txt;0;")
endif()
if(CTEST_CONFIGURATION_TYPE MATCHES "^([Ss][Aa][Vv][Ee]_[Rr][Ee][Gg])$")
  add_test(save_RIGID_2D_arche_plein_ceintre_2D "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/save_test.sh" "/home/pv/anaconda3/bin/python3" "RIGID_2D_arche_plein_ceintre_2D" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/RIGID_2D/arche_plein_ceintre_2D/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/RIGID_2D/arche_plein_ceintre_2D/command.py" "/examples/RIGID_2D/arche_plein_ceintre_2D")
  set_tests_properties(save_RIGID_2D_arche_plein_ceintre_2D PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran:" LABELS "quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;59;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/arche_plein_ceintre_2D/CMakeLists.txt;3;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/RIGID_2D/arche_plein_ceintre_2D/CMakeLists.txt;0;")
endif()
