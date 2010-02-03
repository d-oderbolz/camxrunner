#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains functions to simplify user interaction
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# TODO: 
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=0

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains functions to simplify user interaction"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2009), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

# just needed for stand-alone usage help
progname=$(basename $0)
################################################################################


################################################################################
# Function: usage
#
# Shows that this script can only be used from within the CAMxRunner
# For common scripts, remove the reference to CAMxRunner options
#
################################################################################
function usage() 
################################################################################
{
	# At least in theory compatible with help2man
	cat <<EOF

	$progname - A part of the CAMxRunner tool chain.

	Can ONLY be called by the CAMxRunner.

	Written by $CXR_META_MODULE_AUTHOR
	License: $CXR_META_MODULE_LICENSE
	
	Find more info here:
	$CXR_META_MODULE_DOC_URL
EOF
exit 1
}

################################################################################
# Function: cxr_common_countdown
#
# Most of the Time, CAMxRunner is run non-interactively. Sometimes, the Runner might take decisions
# to which the user objects (e. g. the repetiton of one-time preprocessing in case
# a a prolonged rerun). To implement this, we give the user some time.
#
#
# Parameters:
# $1 - Message to show
# $2 - Time in s from which to cxr_common_countdown (default 30)
# $3 - Interval in s when to show remaining time (default 5)
################################################################################
function cxr_common_countdown()
################################################################################
{
	if [ $# -lt 1 ]
	then
		cxr_main_logger -e "$FUNCNAME" "Need at least a message to show"
	fi
	
	MESSAGE="${1}"
	
	# Start must be positive!
	START_SEC="$(cxr_common_abs ${2:-30})"
	MOD_SEC="${3:-5}"
	
	for i in $(seq $START_SEC -1 0)
	do
		if [ $(expr $i % $MOD_SEC) -eq 0 ]
		then
			cxr_main_logger -w -B "$FUNCNAME" "$i seconds left."
		fi
	done
}

################################################################################
# Function: cxr_common_get_consent
#	
# Asks te user if he/she agrees (Y) to a question or not (N).
# Automatically adds [Y/N] to the question.
#
# Loops until either Y or N is given. (Case is ignored)
#
# Returns:
# true - user says Y
# false - user says N
#
# Example (Note the double quotes around the call):
#> if [ "$(cxr_common_get_consent "Do you want to run the installer for the CAMxRunner, CAMx and the testcase \n (you can select the steps you want later)?" )" == false ]
#> then
#>	exit
#> fi
#
# Cannot be used in a loop like (because read is used internally): 
#
#>while read LINE
#>do
#>done
#
# Parameters:
# $1 - Question
# [$2] - Optional default value (either Y or N)
################################################################################
function cxr_common_get_consent()
################################################################################
{

	# Code repeated for clarity
	# Default only accepted if either Y or N
	if [ \( -s "${2:-}" \) -a \( "${2:-}" == Y -o "${2:-}" == N \) ]
	then
			
		########################################
		# Default is set
		########################################
		DEFAULT=${2:-}

		
		MESSAGE="$1 [Y/N/D], D=$DEFAULT"
		
		ANSWER=$(cxr_common_get_user_input "$MESSAGE")
	
		until [  "$ANSWER" == Y -o "$ANSWER" == y \
							-o "$ANSWER" == N -o "$ANSWER" == n \
							-o "$ANSWER" == D -o "$ANSWER" == d ]
		do
			ANSWER=$(cxr_common_get_user_input "$MESSAGE\nAnswer with either Y, N or D")
		done
		
		if [ "$ANSWER" == d -o "$ANSWER" == D ]
		then
			ANSWER=$DEFAULT
		fi
		
	else
	
		########################################
		# No Default
		########################################	

		MESSAGE="$1 [Y/N]"
		
		ANSWER=$(cxr_common_get_user_input "$MESSAGE")
	
		until [  "$ANSWER" == Y -o "$ANSWER" == y \
							-o "$ANSWER" == N -o "$ANSWER" == n ]
		do
			ANSWER=$(cxr_common_get_user_input "$MESSAGE\nAnswer with either Y or N.")
		done
		
	fi
	
	if [ "$ANSWER" == Y -o "$ANSWER" == y ]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: cxr_common_get_user_input
#	
# Poses a question to the user and returns the answer.
# Optionally, the lenght of the input can be restricted.
#
# Returns:
# the value the user enters.
#
# Example:
#> MEAL=$(cxr_common_get_user_input "Whats you favourite meal?")
#
# Cannot be used in a loop like (because read is used internally): 
#
#>while read LINE
#>do
#>done
#
# Parameters:
# $1 - Question
# [$2] - Expected lenght (Optional)
################################################################################
function cxr_common_get_user_input() 
################################################################################
{
	MESSAGE=$1
	
	# Communicate with the user on STDERR.
	echo "${CXR_SINGLE_LINE}" 1>&2
	echo -e "${MESSAGE}" 1>&2
	
	if [ $(cxr_main_is_numeric "${2:-}") == true ]
	then
		read -n $2 ANSWER
	else
		read ANSWER
	fi

	echo $ANSWER
}

################################################################################
# Function: cxr_common_pause
#	
# Waits until the user presses a button
#
# Example:
#> pause
#
# Cannot be used in a loop like (because read is used internally): 
#
#>while read LINE
#>do
#>done
#
################################################################################
function cxr_common_pause() 
################################################################################
{
	echo "${CXR_SINGLE_LINE}"
	echo "Press a key of your choice to continue..."
	read -n 1 DUMMY
}

################################################################################
# Function: cxr_common_get_menu_choice
#	
# Poses a question to the user and presents a menu of choices.
# The choices are either given using a pattern (ls-like) for filenames
# or using a string containing space-delimited options.
#
# If the user enters a non-numeric sequence, the defauld value is returned.
#
# Returns:
# the value the user selects (the string, not the number).
#
# Cannot be used in a loop like (because read is used internally): 
#
#>while read LINE
#>do
#>	:
#>done
#
# Example:
#> MEAL=$(cxr_common_get_menu_choice "Whats you favourite meal?" "hot-dog hamburger pizza organic" "pizza")
# In the next 2 examples, the quotes around the second parameter are vital!
#> File=$(cxr_common_get_menu_choice "Select a file" "*" )
#> Version=$(cxr_common_get_menu_choice "Select a Version" "$CXR_SUPPORTED_MODEL_VERSIONS" )
#
# Can be nicley used in conjuntion with the case statement:
# 
#>WHAT=$(cxr_common_get_menu_choice "Which part of the state database do you want to clean?" "all none" "none")
#>
#>case "$WHAT" in 
#> all) 
#>	delete_all ;;
#>none)
#>	cxr_main_logger "${FUNCNAME}"  "Will not delete any state information" ;;
#>esac
#
# Parameters:
# $1 - Question
# $2 - Option list (eiter a pattern or a string)
# [$3] - Optional default value
################################################################################
function cxr_common_get_menu_choice()
################################################################################
{
	
	echo "${CXR_SINGLE_LINE}" 1>&2
	MESSAGE="$1\nEnter the *number* of your choice:"
	OPTIONS=$2
	
	if [ -s "${3:-}" ]
	then
		# We have a default, set advanced prompt and add default as last
		DEFAULT="${3:-}"
		PS3="Your choice (a number - press [d] for default value [$DEFAULT]): "
	else
		# no default, set normal prompt
		PS3="Your choice (a number): "
	fi
	
	# Communicate with the user on STDERR.
	echo -e "${MESSAGE}" 1>&2
	
	select CHOICE in ${OPTIONS};
	do
	
		CHOSEN="$CHOICE"
		# Without the break we are left in an endless loop...
		break
		
	done
	
	# Default handling. If the user presses d (or any non-numeric character),
	# Value is empty (are we depending on implementation-specific bevaviour here?)
	if [ -s "${DEFAULT:-}" -a -z "$(cxr_common_trim "$CHOSEN")" ]
	then
		CHOSEN="$DEFAULT"
	fi
	
	echo "$CHOSEN"
}

################################################################################
# Function: cxr_common_get_answers
#	
# Poses questions given in a so called ask-file to the user.
# Creates a so-called play-file that contains the answer of the user 
# as key-value pairs.
#
# Loop through all lines of the ask file,
# Split - ask question - store result
#
# Format of .ask files:
# Variable|Datatype|Question|Default|Value1|Value2|...|ValueN
# A minimal line looks like this (An integer value without question and default):
# MXCOLA|I||
#
# A support program (bin/extract_ask.sh) can automatically extract variables from
# a template and create a raw .ask file. IF THE FORMAT OF .ASK SHOULD CHANGE,
# bin/extract_ask.sh NEEDS TO BE CHANGED AS WELL
#
# All but the first string are optional, the number of Values at the end is not fixed.
# Actually, a line could just consist of a Variablename,
# the installer would then choose these vales:
# * Datatype S
# * Question: What is the value of Variablename?
# * Default: Value of the variable from environment if set
#
# You can use variables or combinations thereof as Default (all that is allowed in file rules is allowed here)
#
# At the end, a variable amount of possible values is allowed - but you can also specify a variable name
# that contains a list of values (makes maintenance easier, you can maintain these lists in base.conf or something)
#
# Datatype can be (as supported by cxr_common_check_datataype)
# S (String - Default)
# I (Integer)
# F (Float)
# B (Boolean - either true or false)
# D (Directory - a high level string that is checked)
#
# Of course, the "|"  is reserved, # is a comment (as first char in the line ONLY)
# Lines of the Form COMMENT|String are echoed as is
#
#
# Format of resulting .play files:
# Variable:Value
#
# Returns:
# void - aborts program on failure.
#
# Example:
# cxr_common_get_answers $ASKFILE $PLAYFILE
#
# Parameters:
# $1 - Askfile (with full path)
# $2 - Playfile to store answers in (with full path - WILL BE OVERWRITTEN!)
################################################################################
function cxr_common_get_answers()
################################################################################
{
	ASKFILE="$1"
	PLAYFILE="$2"
	
	cxr_main_logger -a "${FUNCNAME}" "Using ask-file ${PLAYFILE}"
	
	# Write a comment to the file
	echo -e "#This is a machine-generated file. Generated on $(date) by user $USER\n#It can be used to replay an installation." > $PLAYFILE
	
	# The canonical way to loop through a file is
	# while read LINE
	# do
	# done
	# This is not possible because I want to read input from stdin as well.
	
	CURLINE=1

	while [ $CURLINE -le $(wc -l < $ASKFILE) ]
	do
		# Read the line (I know this is not nice, read comment above)
		# This is a ugly standard construct: Read first $CURLINE lines
		# the last of which is line $CURLINE
		LINE="$(head -n $CURLINE $ASKFILE | tail -n 1)"
		
		# LOV (List of values) is unset yet
		LOV=""

		# Ignore Comments - but only if in first column
		if [ "${LINE:0:1}" == \# ]
		then
			CURLINE=$(( $CURLINE + 1 ))
			continue
		fi
		
		# Ignore empty lines
		if [ -z "$(cxr_common_trim "$LINE")" ]
		then
			CURLINE=$(( $CURLINE + 1 ))
			continue
		fi

		# Parse it - structure Variable|Datatype|Question|Default|Value1|Value2|...|ValueN
		# All but the first are optional, the number of Values at the end is not fixed.
		# 

		# We mainpulate word-splitting to get the list of values

		# Save old IFS
		oIFS="$IFS"

		IFS="$CXR_DELIMITER"

		# Suck line into LINE_ARRAY
		LINE_ARRAY=($LINE)
		
		# Reset IFS
		IFS="$oIFS"

		NUM_ELEMENTS=${#LINE_ARRAY[@]}

		if [ "$NUM_ELEMENTS" -ge 1 ]
		then
			VARIABLE=${LINE_ARRAY[0]}
		else
			# No variable - skip
			CURLINE=$(( $CURLINE + 1 ))
			continue
		fi
		
		if [ "${VARIABLE}" == COMMENT ]
		then
			########################################
			# A comment the user should see, then skip
			########################################
			echo "${CXR_DOUBLE_LINE}"
			echo -e "${LINE_ARRAY[1]}"
			echo "${CXR_DOUBLE_LINE}"
			CURLINE=$(( $CURLINE + 1 ))
			continue
		fi

		if [ "$NUM_ELEMENTS" -ge 2 ]
		then
			DATATYPE=${LINE_ARRAY[1]}
		else
			DATATYPE=S
		fi

		if [ "$NUM_ELEMENTS" -ge 3 ]
		then
			QUESTION=${LINE_ARRAY[2]}
		else
			QUESTION="What should be the value of $VARIABLE?"
		fi

		if [ "$NUM_ELEMENTS" -ge 4 ]
		then
			DEFAULT=${LINE_ARRAY[3]}
			
			# If it starts with CXR_ we knwo its a variable and try to resolve it
			if [ ${DEFAULT:0:4} == CXR_ ]
			then
				# Create an expandable rule
				DEFAULT="\$$DEFAULT"
			fi
			
		else
			# Otherwise just try to create an expandable rule
			DEFAULT="\$$VARIABLE"
		fi

		########################################	
		# Handling of an optinal list of values (LOV)
		########################################	
		if [ "$NUM_ELEMENTS" -eq 5 ]
		then
			# If we have exactly one value, it is taken as the list of values
			# this is to simplify the maintenance of base.ask
			LOV="${LINE_ARRAY[4]}"
		elif [ "$NUM_ELEMENTS" -gt 5 ]
		then
			# We must go through a list and reconstruct it
			for CURLOV in $(seq 4 $(( $NUM_ELEMENTS - 1 )) )
			do
				LOV="$LOV ${LINE_ARRAY[$CURLOV]}"
			done
		fi

		cxr_main_logger -v "${FUNCNAME}"  "VARIABLE: $VARIABLE"
		cxr_main_logger -v "${FUNCNAME}"  "QUESTION: $QUESTION"
		cxr_main_logger -v "${FUNCNAME}"  "DEFAULT: $DEFAULT"
		cxr_main_logger -v "${FUNCNAME}"  "DATATYPE: $DATATYPE"
		cxr_main_logger -v "${FUNCNAME}"  "LOV: $LOV"


		if [ -z "$(cxr_common_trim "${VARIABLE}")" ]
		then
			# No variable - skip
			CURLINE=$(( $CURLINE + 1 ))
			continue
		else
			########################################	
			# A standard variable
			########################################	

			# Some optimizations

			# If the question is empty, supply your own
			if [ -z "$(cxr_common_trim "$QUESTION")" ]
			then
				QUESTION="What should be the value of $VARIABLE?"
			fi

			# Default - might contain an expandable rule
			# It does not matter if the rule does not expand (are we?)
			# We call the rule DEFAULT
			DEFAULT=$(cxr_common_evaluate_rule "$DEFAULT" true DEFAULT)
			
			# Do we have a list of values?
			if [ -z "$LOV" ]
			then
				# No
				# Add Default to question
				QUESTION="$QUESTION\n(Default: $DEFAULT)"

				VALUE=$(cxr_common_get_user_input "$QUESTION\n[D] for Default")

				if [ "$VALUE" == D -o "$VALUE" == d ]
				then
					# Use Default
					VALUE="$DEFAULT"
				fi

			else
				# Yes
				VALUE=$(cxr_common_get_menu_choice "$QUESTION" "$LOV")
			fi

			if [ "$(cxr_common_check_datataype "$VALUE" "$DATATYPE")" == false ]
			then
				cxr_main_logger "${FUNCNAME}" "Datatpe of $VALUE is not $DATATYPE! I use the default instead."
				# Use Default
				VALUE="$DEFAULT"
			fi

			# If the user does not want to write this 
			# value, we will ask the same question again (by not increasing CURLINE)
			# Thats actually an advantage of this form of loop.
			if [ "$(cxr_common_get_consent "Is the value $VALUE for Variable $VARIABLE correct?" Y )" == true ]
			then
			
				# Write data to play-file
				echo "$VARIABLE:$VALUE" >> $PLAYFILE

				CURLINE=$(( $CURLINE + 1 ))
			fi
		fi
	done
}


################################################################################
# Function: cxr_common_apply_playfile
#	
# Complementary function of <cxr_common_get_answers>. Loops trough a playfile and 
# uses its information to search for variables in all listed files and
# replaces them by the repective value.
#
# Format of input .play files:
# Variable|Value
# Some older playfiles use : as delimiter, we can detect this
#
# Returns:
# void - aborts program on failure.
#
# Example:
# cxr_common_apply_playfile $PLAYFILE "file1 file2"
#
# Parameters:
# $1 - Playfile (with full path)
# $2 - A list of files to operate on (search & replace) - they WILL BE OVERRIDDEN!
################################################################################
function cxr_common_apply_playfile()
################################################################################
{
	# The delimiter in use here (might change below if file has a different format)
	local DELIMITER="${CXR_DELIMITER}"


	if [ $# -ne 2 ]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a Playfile and a list of input files as input"
	fi

	PLAYFILE="$1"
	FILES="$2"

	########################################
	cxr_main_logger "${FUNCNAME}" "Playback of $PLAYFILE..."
	########################################
	
	# Quickly check if is old-style
	# We count the number of lines containing colons
	# The first line is ok (comment contains a time like 15:10:55)
	if [ "$(grep -c : "$PLAYFILE")" -gt 1 ]
	then	
		# More than 1 line contains :
		cxr_main_logger -w $FUNCNAME "More than one line of the file $PLAYFILE contains colons (the former delimiter used for .play files). I will now assume : as delimiter, but please replace : by | in the file manually, thanks!"
		# Use old DELIMITER
		DELIMITER=":"
	fi
	
	# This one will be re-used
	TMPFILE=$(cxr_common_create_tempfile sed)
	
	# Start at line 1
	CURLINE=1
	
	cxr_main_logger -a "${FUNCNAME}" "Applying changes to the template files..."
		
	# Loop trough playfile, CURLINE is the line index 1..n
	while [ $CURLINE -le $(wc -l < $PLAYFILE) ]
	do
		# Read the current line (needed because we use read in the loop)
		# This is an ugly standard construct: Read first $CURLINE lines
		# the last of which is line $CURLINE
		LINE=$(head -n $CURLINE $PLAYFILE | tail -n 1)
			
			# Ignore Comments - but only if in first column
			if [ "${LINE:0:1}" == \# ]
			then
				CURLINE=$(( $CURLINE + 1 ))
				continue
			fi
			
			# Ignore empty lines
			if [ -z "$(cxr_common_trim "$LINE")" ]
			then
				CURLINE=$(( $CURLINE + 1 ))
				continue
			fi
			
			# Split
			oIFS="$IFS"

			IFS="$DELIMITER"
		
			# Suck line into LINE_ARRAY
			LINE_ARRAY=($LINE)
		
			# Reset IFS
			IFS="$oIFS"
			
			VARIABLE=${LINE_ARRAY[0]}
			VALUE=${LINE_ARRAY[1]}
			
			if [ -z "$VARIABLE" ]
			then
				# Skip empty variables
				CURLINE=$(( $CURLINE + 1 ))
				continue
			fi

			cxr_main_logger -v "${FUNCNAME}" "For each variable, we go through all files now."

			# Replace the current variable in all files listed
			for CURRENT_FILE in $2
			do
				cxr_main_logger -v "${FUNCNAME}" "CURRENT_FILE: $CURRENT_FILE"
				
				# replace the @VARIABLE@ with the value (globally) 
				# send the output to $TMPFILE (sed cannot work on the same file as the input)
				# The TEMPFILE is automatically re-used (overwritten)
				sed -e "s/@$VARIABLE@/$VALUE/g" $CURRENT_FILE > $TMPFILE

				# Copy file back
				cp $TMPFILE $CURRENT_FILE || cxr_main_die_gracefully "Could not copy $TMPFILE to the draft file $CURRENT_FILE"
				
				# Empty tempfile
				: > $TMPFILE
				
			done # Loop through files
			
			# Advance to next line in playfile
			CURLINE=$(( $CURLINE + 1 ))

		done # Loop trough Playfile

		if [ "$(cxr_common_get_consent "Do you want to have a look at the new files?" N )" == true ]
		then
			for CURRENT_FILE in $2
			do
				echo -e "${CXR_BOX_HUGE}"
				
				echo "File $CURRENT_FILE:"
				
				echo -e "${CXR_BOX_HUGE}"
				
				cat $CURRENT_FILE
				
				echo -e "${CXR_BOX_HUGE}\n\n"
				echo "(That was file ${CURRENT_FILE})"
			
				cxr_common_pause
			done
		fi

}

################################################################################
# Function: test_module
#
# Runs the predefined tests for this module. If you add or remove tests, please
# update CXR_META_MODULE_NUM_TESTS in the header!
# 
################################################################################	
function test_module()
################################################################################
{
	if [ "${CXR_TESTING_FROM_HARNESS:-false}" == false ]
	then
		# We need to do initialisation
	
		# This is the run we use to test this
		CXR_RUN=$CXR_META_MODULE_TEST_RUN
	
		# Safety measure if script is not called from .
		MY_DIR=$(dirname $0) && cd $MY_DIR
	
		# We step down the directory tree until we either find CAMxRunner.sh
		# or hit the root directory /
		while [ $(pwd) != / ]
		do
			cd ..
			# If we find CAMxRunner, we are there
			ls CAMxRunner.sh >/dev/null 2>&1 && break
			
			# If we are in root, we have gone too far
			if [ $(pwd) == / ]
			then
				echo "Could not find CAMxRunner.sh!"
				exit 1
			fi
		done
		
		# Save the number of tests, as other modules
		# will overwrite this (major design issue...)
		MY_META_MODULE_NUM_TESTS=$CXR_META_MODULE_NUM_TESTS
		
		# Include the init code
		source inc/init_test.inc
		
		# Plan the number of tests
		plan_tests $MY_META_MODULE_NUM_TESTS
		
	fi
	
	########################################
	# Setup tests if needed
	########################################
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	# None yet.

	########################################
	# teardown tests if needed
	########################################

}

################################################################################
# Are we running stand-alone? 
################################################################################


# If the CXR_META_MODULE_NAME  is not set
# somebody started this script alone
# Normlly this is not allowed, except to test using -t
if [ -z "${CXR_META_MODULE_NAME:-}" ]
then

	# When using getopts, never directly call a function inside the case,
	# otherwise getopts does not process any parametres that come later
	while getopts ":dvFST" opt
	do
		case "${opt}" in
		
			d) CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_LOG_EXT="-dry" ;;
			v) CXR_USER_TEMP_VERBOSE=true ; echo "Enabling VERBOSE (-v) output. All lines starting with % would not be present otherwise" ;;
			F) CXR_USER_TEMP_FORCE=true ;;
			S) CXR_USER_TEMP_SKIP_EXISTING=true ;;
			
			T) TEST_IT=true;;
			
		esac
	done
	
	# This is not strictly needed, but it allows to read 
	# non-named command line options
	shift $((${OPTIND} - 1))

	# Make getopts ready again
	unset OPTSTRING
	unset OPTIND
	
	# This is needed so that getopts surely processes all parameters
	if [ "${TEST_IT:-false}" == true ]
	then
		test_module
	else
		usage
	fi

fi