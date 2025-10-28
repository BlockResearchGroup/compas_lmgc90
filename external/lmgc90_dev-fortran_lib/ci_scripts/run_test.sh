#!/bin/bash

#set -e

# expects in order :
# - the python executable
# - the generation script (optional)
# - the command script
# - the path where is the reference result
# 
# the number of arguments is counted
# to decide if generation script is present or not

python_exec=$1

name=$2
stdout_gen_log="$2_log_gen_stdout.txt"
stderr_gen_log="$2_log_gen_stderr.txt"
stdout_com_log="$2_log_com_stdout.txt"
stderr_com_log="$2_log_com_stderr.txt"
stdout_dif_log="$2_log_dif_stdout.txt"
stderr_dif_log="$2_log_dif_stderr.txt"

# in case of OpenMP use :
export OPENBLAS_NUM_THREADS=1
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

  # test here that DATBOX has not changed
  datbox=$( find . -type d -name "DATBOX")
  l_files=$(ls ${datbox}/{BODIES*,BULK_BEHAV*,TACT_BEHAV*,MODEL*,DRV_DOF*,DOF*,VlocRloc*,GPV*} 2>/dev/null ) || :

  succes=1
  for file in ${l_files}
  do
      (diff -bB ${ref_dir}/${file} ${file} &>/dev/null)# 1>> ${stdout_dif_log} 2> ${stderr_dif_log})
      res_diff=$?
      # if [ "$(diff -bB ${ref_dir}/${file} ${wk_dir}/${file})" ]
      if [ $res_diff -ne 0 ]; then
          fail_file+=(${file})
          if [ ${res_diff} -eq 1 ]; then
             # Print the file which differ during the progress
             echo "Differences: ${file}"
          fi
          if [ ${res_diff} -ge 2 ]; then
             # Print the file which are not find or other
             echo "res_diff: ${res_diff} ; Not found?!? : ${file}"
          fi
          succes=0
      fi
  done
  if [ ${succes} -eq 1 ]; then
      echo 'comparing with reference DATBOX : SUCESS'
  else
      echo 'comparing with reference DATBOX : FAIL'
      exit 1
  fi


else
  com=$3
  ref_dir=$4
fi


# run command script
$python_exec $com --novisu --norand 1>${stdout_com_log} 2>${stderr_com_log}
if [ $? -eq 0 ]; then
  echo 'running computation script : SUCESS'
else
  echo 'running computation script : FAIL'
  exit 1
fi


# copied from diff_dir script of frozar

# Get the destination directory from the command line

nb_succes=0
nb_failed=0

fail_file=()

# Print the two directory which are compared
echo "reference directory: ${ref_dir}"
echo "working directory  : ${PWD}"

# Make the comparison only with OUTBOX directories reachable from current directory
outbox=$( find . -type d -name "OUTBOX")

l_files=$(ls ${outbox}/{GPV*,DOF*,Vloc_Rloc*} 2>/dev/null ) || :

succes=1

for file in ${l_files}
do
    (diff -bB ${ref_dir}/${file} ${file} &>/dev/null)# 1>> ${stdout_dif_log} 2> ${stderr_dif_log})
    res_diff=$?
    # if [ "$(diff -bB ${ref_dir}/${file} ${wk_dir}/${file})" ]
    if [ $res_diff -ne 0 ]; then
        fail_file+=(${file})
        if [ ${res_diff} -eq 1 ]; then
           # Print the file which differ during the progress
           echo "Differences: ${file}"
        fi
        if [ ${res_diff} -ge 2 ]; then
           # Print the file which are not find or other
           echo "res_diff: ${res_diff} ; Not found?!? : ${file}"
        fi
        succes=0
    fi

done

if [ ${succes} -eq 1 ]; then
    ((++nb_succes))
    echo 'comparing with reference results : SUCESS'
else
    ((++nb_failed))
    echo 'comparing with reference results : FAIL'
fi

exit ${nb_failed}


