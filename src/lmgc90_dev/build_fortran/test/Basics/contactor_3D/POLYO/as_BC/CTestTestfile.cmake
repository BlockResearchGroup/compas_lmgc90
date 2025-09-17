# CMake generated Testfile for 
# Source directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/contactor_3D/POLYO/as_BC
# Build directory: /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/test/Basics/contactor_3D/POLYO/as_BC
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(python_basic_contactor_3D_POLYO_as_BC "/home/pv/anaconda3/bin/python3" "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran/test/Basics/contactor_3D/POLYO/as_BC/test.py")
set_tests_properties(python_basic_contactor_3D_POLYO_as_BC PROPERTIES  ENVIRONMENT "PYTHONPATH=/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/build_fortran::;OMP_SCHEDULE=STATIC;OMP_NUM_THREADS=1;OPENBLAS_NUM_THREADS=1" _BACKTRACE_TRIPLES "/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/contactor_3D/POLYO/as_BC/CMakeLists.txt;12;add_test;/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/test/Basics/contactor_3D/POLYO/as_BC/CMakeLists.txt;0;")
