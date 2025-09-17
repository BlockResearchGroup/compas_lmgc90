# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/jointFE_Arche
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/mecaMAILx_3D/jointFE_Arche
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
if(CTEST_CONFIGURATION_TYPE MATCHES "^([Nn][Oo][Nn]_[Rr][Ee][Gg])$")
  add_test(mecaMAILx_3D_jointFE_Arche "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/run_test.sh" "/home/pv/anaconda3/bin/python3" "mecaMAILx_3D_jointFE_Arche" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/mecaMAILx_3D/jointFE_Arche/gen.py" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/mecaMAILx_3D/jointFE_Arche/command.py" "/examples/mecaMAILx_3D/jointFE_Arche")
  set_tests_properties(mecaMAILx_3D_jointFE_Arche PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran:" LABELS "all;quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;40;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/jointFE_Arche/CMakeLists.txt;4;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/jointFE_Arche/CMakeLists.txt;0;")
endif()
if(CTEST_CONFIGURATION_TYPE MATCHES "^([Ss][Aa][Vv][Ee]_[Rr][Ee][Gg])$")
  add_test(save_mecaMAILx_3D_jointFE_Arche "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/save_test.sh" "/home/pv/anaconda3/bin/python3" "mecaMAILx_3D_jointFE_Arche" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/mecaMAILx_3D/jointFE_Arche/gen.py" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/examples/mecaMAILx_3D/jointFE_Arche/command.py" "/examples/mecaMAILx_3D/jointFE_Arche")
  set_tests_properties(save_mecaMAILx_3D_jointFE_Arche PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran:" LABELS "all;quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;59;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/jointFE_Arche/CMakeLists.txt;4;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/jointFE_Arche/CMakeLists.txt;0;")
endif()
