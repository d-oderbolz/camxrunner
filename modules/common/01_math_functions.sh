#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Date functions
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################

################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=10

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains some date functions for the CAMxRunner"

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

	Is designed to be called by the CAMxRunner.
	
	You can, however, call it like this:
	
	$ $progname -T
	
	this starts the self-test of the module.

	
	Written by $CXR_META_MODULE_AUTHOR
	License: $CXR_META_MODULE_LICENSE
	
	Find more info here:
	$CXR_META_MODULE_DOC_URL
EOF
exit 1
}

################################################################################
# Function: cxr_common_abs
#
# Calculates the absolute value of a number
#
# Parameters:
# $1 - number to be treated
################################################################################
function cxr_common_abs()
################################################################################
{
	if [ $# -ne 1 -o "$(cxr_main_is_numeric "$1")" == false ]
	then
		cxr_main_logger -e "$FUNCNAME" "$FUNCNAME:$LINENO - needs a number as input"
		echo false
		return $CXR_RET_ERROR
	fi

	# Is the number 0?
	if [ $1 -eq 0 ]
	then
		echo 0
	# Is the number negative?
	elif [ $1 -lt 0 ]
	then
		echo `expr 0 - $1`
	else
		echo $1
	fi
	
	return 0 # OK

}


################################################################################
# Function: cxr_common_fp_calculate
#
# Performs FP arithmetic (other than $(()) )
# If the result is a whole number, a . is appended at the end (FORTRAN compatibility)
#
# Parameters:
# $1 - an expression
# $2 - an optional scale parameter (default CXR_NUM_DIGITS). 0 Means truncate
# $3 - an optional boolean parameter indicating if a trailing decimal should be added (default: true)
################################################################################
function cxr_common_fp_calculate()
################################################################################
{
	if [ $# -lt 1 ]
	then
		cxr_main_die_gracefully "$FUNCNAME" "$FUNCNAME:$LINENO - needs at least an expression as input"
	fi
	
	RESOLUTION=${2:-$CXR_NUM_DIGITS}
	ADD_TRAILING_DP=${3:-true}
	
	# Set resolution & pass expression
	RESULT=$( echo "scale=$RESOLUTION; $1" | bc )
	
	if [ "$RESOLUTION" -eq 0 ]
	then
		# Chop off the decimals
		RESULT=${RESULT%%\.*}
	fi
	
	# The scale function counts digits after the decimal point
	if [ "${ADD_TRAILING_DP}" == true -a "$( echo "scale(${RESULT})" | bc )" -eq 0 ]
	then
		# Integer,  and we need to add a trailing .
		echo ${RESULT}.
	else
		echo ${RESULT}
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
	
	is $(cxr_common_abs 0) 0 "cxr_common_abs of 0"
	is $(cxr_common_abs -0) 0 "cxr_common_abs of -0"
	is $(cxr_common_abs 1) 1 "cxr_common_abs of 1"
	is $(cxr_common_abs -1) 1 "cxr_common_abs of -1"
	is $(cxr_common_abs -1000000) 1000000 "cxr_common_abs of -1000000"
	is $(cxr_common_fp_calculate "-1000000 * -1" )  1000000. "cxr_common_fp_calculate -1000000 * -1"
	is $(cxr_common_fp_calculate "12.43 * 2" ) 24.86 "cxr_common_fp_calculate 12.43 * 2"
	is $(cxr_common_fp_calculate "5" )  5. "cxr_common_fp_calculate 5 (trailing .)"
	is $(cxr_common_fp_calculate "5" "" false )  5 "cxr_common_fp_calculate 5 false (no trailing .)"
	is $(cxr_common_fp_calculate "0.0000224784 * 300000" 0 false) 6 "cxr_common_fp_calculate Rounding"

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
	if [ "${TEST_IT:-false}" == true ]
	then
		test_module
	else
		usage
	fi

fi

