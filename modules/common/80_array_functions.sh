#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Array Functions
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################
# TODO: 
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=7

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the Array functions for the CAMxRunner"

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
# Function: common.array.allElementsZero?
#	
# Checks if the values in a given array are all zero and returns true if its ok (all 0),
# false otherwise.
#
# Example:
# > a=$(ls | wc -k 2>/dev/null )
# >if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
# >then
#	>	main.log -e "Pipe at $LINENO:failed"
# >fi
#
# Parameters:
# $1 - The array to test, pass it as "${array[@]}" (including quotes!)
################################################################################
function common.array.allElementsZero?() 
################################################################################
{
	# Here we store our status, default is OK
	local status=true
	local i_arr
	
	# We must suck the array passed as list into an array again
	local array
	
	array=( "$@" )
	
	for i_arr in $(seq 0 $(( ${#array[@]} - 1 )) )
	do
		if [[ "${array[$i_arr]}" != 0 ]]
		then
			status=false
		fi
	done
	
	echo $status
}

################################################################################
# Function: common.array.countDelimitedElements
#
# Counts the number of elements in a delimited string.
# Caution: Does not yet work well with newline as delimiter 
# NUM_MODULE_TYPES=$(common.array.countDelimitedElements "$MODULE_TYPES" "$'\n'")
# Yields a number that is off by one! 
# Use $(echo $String | wc -l) or something similar in these cases
#
# Parameters:
# $1 - String to parse
# [$2] - Optional Delimiter (Default $CXR_DELIMITER)
################################################################################
function common.array.countDelimitedElements ()
################################################################################
{
	local string
	local delimiter
	local array
	
	if [[ $# -ne 2  ]]
	then
		string="$1"
		delimiter="$CXR_DELIMITER"
	else
		string="$1"
		delimiter="$2"
	fi
	
	# Save old IFS
	oIFS="$IFS"

	IFS="$delimiter"
	
	# Suck line into array
	array=($string)
	
	# Reset IFS
	IFS="$oIFS"

	# Echo the count
	echo ${#array[@]}
}

################################################################################
# Function: common.array.exportArray
#
# Exports / Serialises an array to the environment.
#
# Bash does not support exporting arrays easily, they must be
# serialised to a string, which is done here and then can be deserialised using <common.array.importArrays>
#
# The name of the serialised array is the original name with _ARR_X at the end
#
# This function is deprecated but if might be useful to allow communication over
# environment variables
# 
# Parameters:
# $1 - Name of the array to serialise
################################################################################
function common.array.exportArray()
################################################################################
{
	# Example of idea:
	# CXR_NUMBER_OF_LAYERS_ARR_X=$(printf '%q ' "${CXR_NUMBER_OF_LAYERS[@]}")

	export ${1}_ARR_X="$(printf '%q ' $(eval "echo \${${1}[@]}"))"
}

################################################################################
# Function: common.array.importArrays
#	
# Imports arrays into the current scope
# Was needed because we used env to reflect about arrays
# and bash cannot really export arrays... (now with set that is no longer an issue)
# Partner function of <common.array.exportArray>
################################################################################
function common.array.importArrays()
################################################################################
{
	# Bash does not support exporting arrays easily, they must be
	# serialised to a string and then be deserialised, which is done here
	# These arrays are all 0-indexed, therefore the index 0 positions are pure dummies
	
	local var
	local arr_name
	
	for var in $(set | sort | grep ^CXR_.*_ARR_X= | cut -d= -f1)
	do
		# With this pattern, we extract the name of the Array
		arr_name=$(expr match $var '\(.*\)_ARR_X$')
		
		# Then we import
		# This code is due to stephane_chazelas_at_yahoo.fr - http://unix.derkeiler.com/Newsgroups/comp.unix.shell/2003-05/0603.html
		# Idea: eval "CXR_OUTPUT_SPECIES_NAMES=( ${CXR_OUTPUT_SPECIES_NAMES_ARR_X[*]} )"
		# This is its dynamic form:
		set $arr_name=$(eval "echo \${${var}[*]}")
	done
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
	if [[ "${CXR_TESTING_FROM_HARNESS:-false}" == false  ]]
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
			if [[ $(pwd) == /  ]]
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
	
	# Arrays to count zeros
	a=(1 1 1 1)
	b=(0 0 0 0)
	c=(-1 1 1 -1)
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	is $(common.array.allElementsZero? "${a[@]}") false "common.array.allElementsZero? all 1"
	is $(common.array.allElementsZero? "${b[@]}") true "common.array.allElementsZero? all 0"
	is $(common.array.allElementsZero? "${c[@]}") false "common.array.allElementsZero? all cancelling out"
	is $(common.array.countDelimitedElements "one${CXR_DELIMITER}two") 2 "common.array.countDelimitedElements with 2 elements, default delimiter"
	is $(common.array.countDelimitedElements "one two" " ") 2 "common.array.countDelimitedElements with 2 elements, space"
	is $(common.array.countDelimitedElements "one two " " ") 2 "common.array.countDelimitedElements with 2 elements, space at end"
	is $(common.array.countDelimitedElements "") 0 "common.array.countDelimitedElements with 0 elements"

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
if [[ -z "${CXR_META_MODULE_NAME:-}"  ]]
then

	# When using getopts, never directly call a function inside the case,
	# otherwise getopts does not process any parametres that come later
	while getopts ":dvFST" opt
	do
		case "${opt}" in
		
			d) CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_LOG_EXT="-dry" ;;
			v) CXR_USER_TEMP_VERBOSE=true ; echo "Enabling VERBOSE (-v) output. " ;;
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
	if [[ "${TEST_IT:-false}" == true  ]]
	then
		test_module
	else
		usage
	fi

fi


