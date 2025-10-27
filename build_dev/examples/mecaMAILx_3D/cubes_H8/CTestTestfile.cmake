# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/cubes_H8
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/mecaMAILx_3D/cubes_H8
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
if("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Nn][Oo][Nn]_[Rr][Ee][Gg])$")
  add_test(mecaMAILx_3D_cubes_H8 "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/run_test.sh" "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "mecaMAILx_3D_cubes_H8" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/mecaMAILx_3D/cubes_H8/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/mecaMAILx_3D/cubes_H8/command.py" "/examples/mecaMAILx_3D/cubes_H8")
  set_tests_properties(mecaMAILx_3D_cubes_H8 PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi" LABELS "quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;40;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/cubes_H8/CMakeLists.txt;3;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/cubes_H8/CMakeLists.txt;0;")
endif("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Nn][Oo][Nn]_[Rr][Ee][Gg])$")
if("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Ss][Aa][Vv][Ee]_[Rr][Ee][Gg])$")
  add_test(save_mecaMAILx_3D_cubes_H8 "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/save_test.sh" "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "mecaMAILx_3D_cubes_H8" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/mecaMAILx_3D/cubes_H8/gen_sample.py" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/examples/mecaMAILx_3D/cubes_H8/command.py" "/examples/mecaMAILx_3D/cubes_H8")
  set_tests_properties(save_mecaMAILx_3D_cubes_H8 PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi" LABELS "quick" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/ci_scripts/make_test.cmake;59;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/cubes_H8/CMakeLists.txt;3;createTest;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/examples/mecaMAILx_3D/cubes_H8/CMakeLists.txt;0;")
endif("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Ss][Aa][Vv][Ee]_[Rr][Ee][Gg])$")
