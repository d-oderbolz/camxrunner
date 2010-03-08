#!/usr/bin/env bash
#
# Processor module for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
################################################################################
# TODO:
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# This is a very important field needed for the scheduling of parallel processes.
# It contains a comma-separated list of modules (just their names without numbers or extensions)
# from whose output this module DIRECTLY depends on.
#
# A process can only start if its dependencies have finished. Only list direct dependencies.
# There are some special dependencies:
# all_once_preprocessors - all pre_start_preprocessors must have finished
# all_daily_preprocessors - all daily_preprocessors must have finished
# all_model - all model modules must have finished
# all_daily_postprocessors - all daily_postprocessors must have finished
# all_once_postprocessors - all finish_postprocessors must have finished

# the special predicate - refers to the previous model day, so all_model- means that all model modules of the previous day must be successful

CXR_META_MODULE_DEPENDS_ON=""

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="This is an interesting module \n totally untested.\nAlso specify portability"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_DAILY}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=0

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=94

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=94

# URL where to find more information
CXR_META_MODULE_DOC_URL=-

# Who wrote this module?
CXR_META_MODULE_AUTHOR=-

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE=-

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" NAME" 
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

	
	If you want to run just this part of the processing,
	look at the options 
	-D (to process one day),
	-i (a step of the input prep) and 
	-o (a part of the output prep) of the CAMxRunner
	
	Written by $CXR_META_MODULE_AUTHOR
	License: $CXR_META_MODULE_LICENSE
	
	Find more info here:
	$CXR_META_MODULE_DOC_URL
EOF
exit 1
}

################################################################################
# Function: set_variables
#
# Sets the appropriate variables
#
# Parameters:
# $1 - ...
################################################################################
function set_variables() 
################################################################################
{	
	# First of all, reset checks.
	# We will later continuously add entries to these 2 lists.
	# CAREFUL: If you add files to CXR_CHECK_THESE_OUTPUT_FILES,
	# these are deleted if he user runs the -F option. Do not mix up with input files!
	CXR_CHECK_THESE_INPUT_FILES=
	CXR_CHECK_THESE_OUTPUT_FILES=
	
	########################################################################
	# Define checks
	########################################################################
	
	# If you need special checks on the first or last day,
	# use the functions common.date.isFirstDayOfSimulation? and common.date.isLastDayOfSimulation?
	
	########################################################################
	# Set variables
	########################################################################
	
	# Grid specific
	for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
	do
		# Emission Source Files
		CXR_EMISSION_INPUT_ARR_FILES[${i}]=$(common.runner.evaluateRule "$CXR_EMISSION_ASC_FILE_RULE" false CXR_EMISSION_ASC_FILE_RULE)

		# Emission Target files
		# Output files must not be decompressed!
		CXR_EMISSION_OUTPUT_ARR_FILES[${i}]=$(common.runner.evaluateRule "$CXR_EMISSION_BIN_FILE_RULE" false CXR_EMISSION_BIN_FILE_RULE false)
	
		# Update Lists of files to be checked
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES CXR_EMISSION_INPUT_ARR_FILES[${i}]"
		CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES CXR_EMISSION_OUTPUT_ARR_FILES[${i}]"
	
	done

}

################################################################################
# Function: name
#
# Does the actual work. This function gets called from autside and its name must
# be the same as the name of the module file without number and extension.
# So if this file i called 10_my_module.sh this function MUST be called my_module
#
# Parameters:
# $1 - ...
################################################################################
function name() 
################################################################################
{
	# We check if this stage was already excuted before
	if [[ $(common.state.storeState ${CXR_STATE_START}) == true  ]]
	then
	
		#  --- Setup the Environment of the current day
		set_variables 
		
		#  --- Check Settings. 
		# Decide if you crash if errors are found or not.
		# We do not stop the run here if the module failed, this is decided by the 
		# task management. We only stop the run, if a task depends on
		# this failed one - otherwise we can go on!
		if [[ $(common.check.preconditions) == false  ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		main.log   "Running NAME..."
		
		# Normally the actual work is done in a loop for all grids
		# But this depends!
		
		for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
		do
	
			# Determine the file names
			INPUT_FILE=${CXR_EMISSION_INPUT_ARR_FILES[${i}]}
			OUTPUT_FILE=${CXR_EMISSION_OUTPUT_ARR_FILES[${i}]}
			
			if [[  -f "$OUTPUT_FILE" && "$CXR_SKIP_EXISTING" == true   ]]
			then
				# Skip it
				main.log   "File ${OUTPUT_FILE} exists - because of CXR_SKIP_EXISTING, file will skipped."
				continue
			fi
	
			# Increase global indent level
			main.increaseLogIndent
	
			main.log  "Converting ${INPUT_FILE} to ${OUTPUT_FILE}"     
	
			if [[ "$CXR_DRY" == false  ]]
			then
				# Do what needs to be done
				# Normally, there is a call of this form
				# ${CXR_SOME_EXEC}  ${INPUT_FILE} ${OUTPUT_FILE} 2>&1 | tee -a $CXR_LOG
				# Call an exec stored in a variable and redirect output and error to file and stdout.
	
			else
				main.log -w   "Dryrun - no action performed"
			fi
	
			# Decrease global indent level
			main.decreaseLogIndent
	
			# Check if all went well
			# We do not stop the run here if the module failed, this is decided by the 
			# task management. We only stop the run, if a task depends on
			# this failed one - otherwise we can go on!
			if [[ $(common.check.postconditions) == false  ]]
			then
				main.log  "Postconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
				# We notify the caller of the problem
				return $CXR_RET_ERR_POSTCONDITIONS
			fi
			
			
			# -v option shows message only if the log level is high enough
			main.log -v   "Some verbose message here"
	
		done
	
		# Store the state
		common.state.storeState ${CXR_STATE_STOP} > /dev/null ${STAGE}
		
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
	
	# Example test
	is $(common.math.abs 0) 0 "common.math.abs of 0"

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
