#!/bin/bash

set -e

# expects in order :
# - the python executable
# - the generation script (optional)
# - the command script
# - the path where to save the result
# 
# the number of arguments is counted
# to decide if generation script is present or not

python_exec=$1

name=$2
stdout_gen_log="$2_log_save_gen_stdout.txt"
stderr_gen_log="$2_log_save_gen_stderr.txt"
stdout_com_log="$2_log_save_com_stdout.txt"
stderr_com_log="$2_log_save_com_stderr.txt"

# in case of OpenMP use :
export OMP_NUM_THREADS=1
export OMP_SCHEDULE=STATIC

# run generation script if present
if [ $# -eq 5 ]; then
  $python_exec $3 --novisu --norand 1>${stdout_gen_log} 2>${stderr_gen_log}
  if [ $? -eq 0 ]; then
    echo 'running generation script : SUCESS'
  else
    echo 'running generation script : FAIL'
    exit 1
  fi
  # rename argument
  com=$4
  ref_dir=$5

else
  com=$3
  ref_dir=$4
fi

# create ref dir if needed
if [ ! -d ${ref_dir} ]; then
  mkdir -p ${ref_dir}
fi

# run command script
$python_exec $com --novisu --norand 1>${stdout_com_log} 2>${stderr_com_log}
if [ $? -eq 0 ]; then
  echo 'running computation script : SUCESS'
else
  echo 'running computation script : FAIL'
  exit 1
fi

# remove DISPLAY to spare some space
rm -rf ${PWD}/DISPLAY

# save the datbox whatever happen
# to be able to regenerate DISPLAY
echo 'SAVING DATBOX'
rm -rf ${ref_dir}/DATBOX
cp -r ${PWD}/DATBOX ${ref_dir}/

# save the outbox
echo 'SAVING OUTBOX'
rm -rf ${ref_dir}/OUTBOX
mv ${PWD}/OUTBOX ${ref_dir}/

