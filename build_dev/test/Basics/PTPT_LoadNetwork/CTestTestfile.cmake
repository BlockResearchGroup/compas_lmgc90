# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/PTPT_LoadNetwork
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/build_dev/test/Basics/PTPT_LoadNetwork
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(python_basic_PTPT_LoadNetwork_3D "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/test/Basics/PTPT_LoadNetwork/test.py" "3")
set_tests_properties(python_basic_PTPT_LoadNetwork_3D PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:;OMP_SCHEDULE=STATIC;OMP_NUM_THREADS=1;OPENBLAS_NUM_THREADS=1" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/PTPT_LoadNetwork/CMakeLists.txt;15;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/PTPT_LoadNetwork/CMakeLists.txt;0;")
add_test(python_basic_PTPT_LoadNetwork_2D "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/test/Basics/PTPT_LoadNetwork/test.py" "2")
set_tests_properties(python_basic_PTPT_LoadNetwork_2D PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:;OMP_SCHEDULE=STATIC;OMP_NUM_THREADS=1;OPENBLAS_NUM_THREADS=1" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/PTPT_LoadNetwork/CMakeLists.txt;27;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/PTPT_LoadNetwork/CMakeLists.txt;0;")
