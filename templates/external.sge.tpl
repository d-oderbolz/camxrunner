#!/bin/bash

# This template is expanded by the -e option of the
# CAMxRunner. You may use variables in here.
#
# The code here is written for the Merlin4 system at PSI,
# which uses the SGE (Sun grid engine, soon certainly called OGE) queuing system.
#
# Local variables cannot (and should not) be expanded
# make sure that these are not on the same line as an expandable variable
# because we work line-per-line.
#
# We could think about using the task array (-t) feature of SGE later

# BEGIN SGE INSTRUCTIONS

# Use bash
$(echo -en "\\043")$ -S /bin/bash

#
# If you need to expand a commented line, echo the comment using -en "\\043"
# In general, try to avoid command expansion as the expansion is local.
# One way around this is to generate a $ sign ( and ) using the trick above
# Set Emailaddress and send email at beginning and end
$(echo -en "\\043")$ -o $CXR_MAILADDR
$(echo -en "\\043")$ -m b e

# Name the job
$(echo -en "\\043")$ -N $CXR_RUN

# Redirect STDOUT and STDERR
$(echo -en "\\043")$ -o $CXR_EXTERNAL_STDLOG
$(echo -en "\\043")$ -e $CXR_EXTERNAL_ERRLOG

# Run task from the current directory
$(echo -en "\\043")$ -cwd

# Choose parallel Environment (e. g. orte)
$(echo -en "\\043")$ -pe $CXR_EXTERNAL_PE $CXR_EXTERNAL_NUMBER_OF_TASKS

# We ask for soft realtime
$(echo -en "\\043")$ -l s_rt=$CXR_EXTERNAL_TIME_NEEDED

# Fix the SGE environment-handling bug (bash):
source /usr/share/Modules/init/sh
export -n -f module

# END SGE INSTRUCTIONS

days_per_job=$CXR_EXTERNAL_DAYS_PER_JOB

export OMP_NUM_THREADS=$CXR_EXTERNAL_TASKS_PER_NODE

# Store all CAMx.in files in a tempfile
tmpfile=$(echo -en "\\044\\050")mktemp /tmp/cxr.XXXXXXXXXXX$(echo -en "\\051")

$(echo -e "ls -1 CAMx.????????.in > \\044tmpfile")

# Count the days
$(echo -e "ndays=\\044\\050cat \\044tmpfile \\0174 wc -l\\051")


tmpfile_red=$(echo -en "\\044\\050")mktemp /tmp/cxr_red.XXXXXXXXXXX$(echo -en "\\051")

# Extract the next days_per_job lines
if [[ -e last_day ]]
then
	# We start at last_day + 1
	last=$(echo -en "\\044\\050")cat last_day$(echo -en "\\051")
	
	$(echo -e "line=\\044\\050 grep -n \\044last \\044tmpfile | cut -d: -f1  \\051")
	
	start=$(echo -en "\\044\\050\\050 \\044")line + 1 $(echo -en "\\051")$(echo -en "\\051")
	
	if [[ $(echo -en "\\044")start -gt $(echo -en "\\044")ndays ]]
	then
		# No more days
		echo "It seems that all days where processed."
		exit
	else
		# There are more days
		# First get the lines after starting,        then get the first n ones
		
		$(echo -e "tail -n\\044\\050\\050 \\044ndays - \\044start + 1\\051\\051 \\044tmpfile \\0174 head -n\\044days_per_job > \\044tmpfile_red")
	fi
else
	# We start at the beginnig
	$(echo -e "head -n\\044days_per_job \\044tmpfile > \\044tmpfile_red")
fi

while read file
do
	ln -s -f $(echo -en "\\044")file CAMx.in
	aprun -n $CXR_EXTERNAL_NUMBER_OF_TASKS -N $CXR_EXTERNAL_TASKS_PER_NODE -d $CXR_EXTERNAL_CPUS_PER_TASK $CXR_EXTERNAL_MODEL_EXEC
	
	# Store the last day processed
	$(echo -e "echo \\044file > last_day")
done $(echo -en "\\074 \\044")tmpfile_red


rm $(echo -en "\\044")tmpfile $(echo -en "\\044")tmpfile_red

