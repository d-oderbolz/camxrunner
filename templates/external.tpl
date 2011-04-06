#!/bin/bash

# This template is expanded by the -e option of the
# CAMxRunner. You may use variables in here.
# Local variables cannot (and should not) be expanded
# make sure that these are not on the same line as an expandable variable
# because we work line-per-line.

#SBATCH --ntasks=$CXR_NUMBER_OF_TASKS
#SBATCH --ntasks-per-node=$CXR_TASKS_PER_NODE
#SBATCH --ncpus-per-task=$CXR_CPUS_PER_TASK
#SBATCH --time=00:30:00

export OMP_NUM_THREADS=$CXR_TASKS_PER_NODE

tmpfile=$(mktemp /tmp/cxr.XXXXXXX)

ls -1 CAMx.????????.in > $tmpfile

while read file
do
	ln -s -f $file CAMx.in
	aprun -n CXR_NUMBER_OF_TASKS -N CXR_TASKS_PER_NODE -d $CXR_CPUS_PER_TASK $CXR_MODEL_EXEC
done < $tmpfile

rm $tmpfile

