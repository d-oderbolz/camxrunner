#!/usr/bin/env bash
#
# Development script for CAMxRunner.
#
# Provides a simple way to extract .ask data from a template.
# Must be kept up to date should the format of .ask files change
#
# Version: $Id$ 
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# 
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Who		When		What
# dco		18.03.2009	Created 
################################################################################
# TODO: Add questions for
# 	- Datatype
#		- Question
#		- Default
# 		Also ask, if Defaults should be applied
################################################################################

progname=$(basename $0)

################################################################################
# Function: usage
#
# Shows the proper usage of extract_ask.sh
#
################################################################################
function usage()
################################################################################
{
	# At least in theory compatible with help2man
	cat <<EOF
	----------------------------------------------------------------------------
	Usage: $progname [Options] template-file ask-file
	----------------------------------------------------------------------------
	
	$progname - a support program for developers of CAMxRunner. 
	Extracts names of variables from the template-file to create
	a legal new-ask-file to be used with the common.user.getAnswers function.
	If the ask file already exists, the only new entries will be added 
	(no duplicates will be created)
	
	Find detailed information here: 
	http://people.web.psi.ch/oderbolz/CAMxRunner

	This is revision \$Rev$ of $progname

	----------------------------------------------------------------------------

	Written by Daniel C. Oderbolz (dco)
	CAMxRunner@psi.ch

	----------------------------------------------------------------------------
	Options:
	-h    shows this screen
	-c    checks if all variables mentioned in the ask file are present in the template
	-f    forces the creation of a new ask file (overwrites existing files)
	
	
	Find detailed information here: 
	http://people.web.psi.ch/oderbolz/CAMxRunner
EOF

exit 0
}	

################################################################################
# Start of program
################################################################################

	# Handle Options
	
	# When using getopts, never directly call a function inside the case,
	# otherwise getopts does not process any parametres that come later
	while getopts ":dlvVFmct:sP:Ixi:o:CNp:f:h" opt
	do
		case "${opt}" in
			h) CXR_HOLLOW=true; main_usage ;;
			\?) CXR_HOLLOW=true; main_usage ;; 
			*) CXR_HOLLOW=true; main_usage ;;  # Show usage also for unrecognised options
		esac
	done
	
	# This is not strictly needed, but it allows to read 
	# non-named command line options
	shift $((${OPTIND} - 1))
	
	# Make getopts ready again
	unset OPTSTRING
	unset OPTIND

	if [[ $# -ne 2  ]]
	then
		echo -e "\n\tYou must pass 2 parameters: a template file containing @VARIABLES@ and the name of an ask file (non-existent) to be created!\n"
		usage
	fi

	# We need a filename
	if [[ ! -f "$1"  ]]
	then
		echo -e "\n\tYou must pass a readable file (a template containing @VARIABLES@) as input!\n"
		usage
	fi

	# We need a filename
	if [[ -f "$2"  ]]
	then
		echo -e "\n\tThe output file already exists!\n"
		usage
	fi

	# Extract a list of unique variables, without @@
	VARIABLES=$(grep -o -e '@.*@' "$1" | sort | uniq | cut -d@ -f2 )

	for VARIABLE in $VARIABLES
	do
		echo "$VARIABLE:S::" >> "$2"
	done # Variable loop
	


