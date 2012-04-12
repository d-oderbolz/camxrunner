#!/bin/bash

export OMP_NUM_THREADS=$(echo -en "\\074 \\044")NSLOTS

# Store all CAMx.in files in a tempfile
tmpfile=$(echo -en "\\044\\050")mktemp /gpfs/home/oderbolz/cxr.XXXXXXXXXXX$(echo -en "\\051")

$(echo -e "ls -1 CAMx.????????.in > \\044tmpfile")

# Create a file called CONTINUE to allow external stop
touch CONTINUE

while read file
do
	ln -s -f $(echo -en "\\044")file CAMx.in
	$CXR_EXTERNAL_MODEL_EXEC
	
	if [[ ! -f CONTINUE ]]
	then
		echo "CONTINUE file missing, we stop."
		break
	fi
	
done $(echo -en "\\074 \\044")tmpfile

rm $(echo -en "\\044")tmpfile 

