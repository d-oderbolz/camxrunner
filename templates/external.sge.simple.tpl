#!/usr/bin/pagsh.openafs

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
#
#
# Submit this script froem the directory containing all the CAMx.in files
# (This must not reside on AFS!)

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

$(echo -en "\\043")$ -v PATH
$(echo -en "\\043")$ -v LD_LIBRARY_PATH

# Name the job
$(echo -en "\\043")$ -N $CXR_RUN

# Redirect STDOUT and STDERR
$(echo -en "\\043")$ -o $CXR_EXTERNAL_STDLOG
$(echo -en "\\043")$ -e $CXR_EXTERNAL_ERRLOG

# Run task from the current directory
$(echo -en "\\043")$ -cwd

# Choose parallel Environment (e. g. smp) and request slots
$(echo -en "\\043")$ -pe $CXR_EXTERNAL_PE


# Fix the SGE environment-handling bug (bash):
source /usr/share/Modules/init/sh
export -n -f module

# END SGE INSTRUCTIONS

echo "Running on $(/bin/hostname) at $(/bin/date) using ticket cache $KRB5CCNAME"

export OMP_NUM_THREADS=$CXR_EXTERNAL_TASKS_PER_NODE

# Store all CAMx.in files in a tempfile
tmpfile=$(echo -en "\\044\\050")mktemp /gpfs/home/oderbolz/cxr.XXXXXXXXXXX$(echo -en "\\051")

$(echo -e "ls -1 CAMx.????????.in > \\044tmpfile")

while read file
do
	ln -s -f $(echo -en "\\044")file CAMx.in
	$CXR_EXTERNAL_MODEL_EXEC
done $(echo -en "\\074 \\044")tmpfile


rm $(echo -en "\\044")tmpfile 

