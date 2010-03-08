#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# 
#
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=21

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the String functions for the CAMxRunner"

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
# Function: common.string.isSubstringPresent?
#
# Returns true if a substring needle was found within haystack, false otherwise.
# TODO: most calls of thsi function are better replaced by a hash (has?) operation.
#
# Parameters:
# $1 - haystack, the string in which we search
# $2 - needle, the substring to be found
################################################################################
function common.string.isSubstringPresent?() 
################################################################################
{
	local found=$(expr match " $1" ".*$2.*")
	# For safety, here        ^ is a space, so that things never start at 0
	
	if [[ $found -gt 0  ]]
	then
		main.log -v   "Substring $2 matches (partially) with $1"
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.string.toLower
#
# Converts the given argument into an all lower case string.
#
# Parameters:
# $1 - string to be converted
################################################################################
function common.string.toLower() 
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		echo ""
	else
		echo $1 | tr "[:upper:]" "[:lower:]"
	fi
} 

################################################################################
# Function: common.string.toUpper
#
# Converts the given argument into an all lower case string.
#
# Parameters:
# $1 - string to be converted
################################################################################
function common.string.toUpper() 
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		echo ""
	else
		echo $1 | tr "[:lower:]" "[:upper:]"
	fi
}

################################################################################
# Function: common.string.trim
#
# Trims spaces or other characters on both ends of a string.
#
# Parameters:
# $1 - the string to be trimmed
# [$2] - the character that should be removed (default: space-trims tabs, too)
################################################################################
function common.string.trim() 
################################################################################
{
	local string="${1}"
	local to_remove="${2:-" "}"
	local out
	
	if [[ $# -lt 1 || $# -gt 2 ]]
	then
		main.log -e  "Programming error: wrong call."
		echo ""
	else
		# trim in front
		out=${string##$to_remove}
		# and at the back
		out=${out%%$to_remove}
		
		echo $out
	fi
}

################################################################################
# Function: common.string.len
#
# Returns the lenght of a string
#
# Parameters:
# $1 - string that should be measured
################################################################################
function common.string.len() 
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		echo 0
	else
		echo ${#1}
	fi
}

################################################################################
# Function: common.string.getCharacters
#
# Extracts a number of characters from the start of a string
#
# Parameters:
# $1 - string from which the chars should be extracted
# $2 - number of chars to return
################################################################################
function common.string.getCharacters() 
################################################################################
{
	if [[ $# -ne 2  ]]
	then
		echo ""
	else
		# We extract $2 chars starting on position 0 of variable $1
		echo ${1:0:$2}
	fi
}


################################################################################
# Function: common.string.leftPadZero
#
# Makes sure a number has n digits (left padded with 0). Can be used in File rules to make sure
# you get something like my_file0001.dat or something.
#
# Parameters:
# $1 - a number to be padded
# $2 - number of digits to be used
################################################################################
function common.string.leftPadZero
################################################################################
{
	if [[ $# -ne 2  ]]
	then
		main.dieGracefully "We need 2 digits as input: the number to pad and the number of digits to pad to"
	fi
	
	local number="$1"
	local digits="$2"
	
	printf "%0${digits}d" "$number"
}

################################################################################
# Function: common.string.getPrefixNumber
#
# Extracts a 2-digit number from the beginning of a string
#
# Parameters:
# $1 - string from which the numbers should be extracted
################################################################################
function common.string.getPrefixNumber
################################################################################
{
	# Define & Initialize local vars
	local string
	local numbers
	
	if [[ $# -ne 1 ]]
	then
		echo 0
	else
		string=$1
		
		#Dont be afraid, the bash does not seem to recognize the quantor +
		numbers=$(expr match $string '\([0-9]*\)')
		
		# Extract 2 Digits
		numbers=$(common.string.getCharacters "${numbers}" 2)
		
		echo $(common.string.leftPadZero "${numbers}" 2)
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
	if [[ "${CXR_TESTING_FROM_HARNESS:-false}" == false  ]]
	then
		# We need to do initialisation
	
		# This is the run we use to test this
		CXR_RUN=$CXR_META_MODULE_TEST_RUN
	
		# Safety measure if script is not called from .
		MY_DIR=$(dirname $0) && cd $MY_DIR
	
		# We step down the directory tree until we either find CAMxRunner.sh
		# or hit the root directory /
		while [[ $(pwd) != / ]]
		do
			# If we find CAMxRunner, we are there
			ls CAMxRunner.sh >/dev/null 2>&1 && break
			
			# If we are in root, we have gone too far
			if [[ $(pwd) == / ]]
			then
				echo "Could not find CAMxRunner.sh!"
				exit 1
			fi
			
			cd ..
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
	
	is "$(common.string.isSubstringPresent? abc a)" true "common.string.isSubstringPresent? is a in abc?"
	
	is "$(common.string.toLower QUERTY)" querty "common.string.toLower QUERTY"
	is "$(common.string.toLower querty)" querty "common.string.toLower querty"
	is "$(common.string.toLower '')" "" "common.string.toLower empty string"
	
	is "$(common.string.toUpper QUERTY)" QUERTY "common.string.toUpper QUERTY"
	is "$(common.string.toUpper querty)" QUERTY "common.string.toUpper querty"
	is "$(common.string.toUpper "")" '' "common.string.toUpper empty string"
	
	is "$(common.string.trim "")" '' "common.string.trim empty string (implicit)"
	is "$(common.string.trim "    ")" '' "common.string.trim spaces (implicit)"
	is "$(common.string.trim "		")" '' "common.string.trim tabs (implicit)"
	is "$(common.string.trim " hallo")" "hallo" "common.string.trim front (implicit)"
	is "$(common.string.trim "hallo ")" "hallo" "common.string.trim back (implicit)"
	is "$(common.string.trim " hallo ")" "hallo" "common.string.trim both (implicit)"
	is "$(common.string.trim "aba" "a" )" 'b' "common.string.trim remove all a (explicit)"
	is "$(common.string.trim " b " " " )" 'b' "common.string.trim remove spaces (explicit)"
	is "$(common.string.trim "guaggc" "guagg" )" 'c' "common.string.trim remove a whole string (explicit)"
	
	is "$(common.string.getCharacters 1234 2)" 12 "common.string.getCharacters"
	
	is "$(common.string.leftPadZero 1 2)" 01 "cxr_common_two_digits 1"
	is "$(common.string.leftPadZero 01 2)" 01 "cxr_common_two_digits 01"
	is "$(common.string.leftPadZero 001 2)" 01 "cxr_common_two_digits too long"
	
	is "$(common.string.getPrefixNumber 00_ )" 00 "common.string.getPrefixNumber not at beginning"
	
	########################################
	# teardown tests if needed
	########################################
	
	if [[ "${CXR_TESTING_FROM_HARNESS:-false}" == false ]]
	then
		# We where called stand-alone, cleanupo is needed
		main.doCleanup
	fi
	
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