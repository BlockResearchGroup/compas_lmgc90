#!/bin/bash

set -e -x

export OMP_NUM_THREADS=1
export OMP_SCHEDULE=STATIC

/home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev/src/addons/mkdirlmgc
/home/pv/brg/code_fortran/compas_lmgc90/build_dev/bin/lmgc90 < input.txt

