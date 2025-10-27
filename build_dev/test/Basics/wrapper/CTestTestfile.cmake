# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/wrapper
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/build_dev/test/Basics/wrapper
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(python_basic_wrapper "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/test/Basics/wrapper/run_all.py")
set_tests_properties(python_basic_wrapper PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:;OMP_SCHEDULE=STATIC;OMP_NUM_THREADS=1;OPENBLAS_NUM_THREADS=1" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/wrapper/CMakeLists.txt;11;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/wrapper/CMakeLists.txt;0;")
