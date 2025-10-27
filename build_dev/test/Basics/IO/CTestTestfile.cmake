# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/IO
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/build_dev/test/Basics/IO
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(python_basic_io "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/test/Basics/IO/run_all.py")
set_tests_properties(python_basic_io PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:;OMP_SCHEDULE=STATIC;OMP_NUM_THREADS=1;OPENBLAS_NUM_THREADS=1" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/IO/CMakeLists.txt;26;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/IO/CMakeLists.txt;0;")
if("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Nn][Oo][Nn]_[Rr][Ee][Gg])$")
  add_test(python_basic_io_xxl "/home/pv/anaconda3/envs/lmgc90/bin/python3.1" "/home/pv/brg/code_fortran/compas_lmgc90/build_dev/test/Basics/IO/xxl.py")
  set_tests_properties(python_basic_io_xxl PROPERTIES  ENVIRONMENT "long;PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/build_dev:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:/opt/intel/oneapi/advisor/2025.2/pythonapi:;OMP_SCHEDULE=STATIC;OMP_NUM_THREADS=1;OPENBLAS_NUM_THREADS=1" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/IO/CMakeLists.txt;29;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/IO/CMakeLists.txt;0;")
endif("${CTEST_CONFIGURATION_TYPE}" MATCHES "^([Nn][Oo][Nn]_[Rr][Ee][Gg])$")
