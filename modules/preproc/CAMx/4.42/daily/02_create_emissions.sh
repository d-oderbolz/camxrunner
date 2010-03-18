#!/usr/bin/env bash
#
# Preprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
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
CXR_META_MODULE_DESCRIPTION="Generates the emissions for the current day by calling Hannes IDL proc"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_DAILY}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=1

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=CAMx-v4.51-test

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|idl"

# Min CAMxRunner Version needed (Revision number)
# We set this ridiculously high for now
CXR_META_MODULE_REQ_RUNNER_VERSION=1049

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=1049

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
# Sets the appropriate variables needed for <create_emissions>
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
	
	# We really need the IDL script
	CXR_CHECK_THESE_INPUT_FILES="$CXR_IDL_EMISSION_GENERATOR"
	
	########################################################################
	# Set variables
	########################################################################
	
	# Evaluate some rules
	# Output files must not be decompressed!
	
	# Grid specific
	for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
	do
		# This is a bit faked, we just use it to check preconditions
		# the IDL procedures set the names themselves
		CXR_EMISSION_OUTPUT_FILE="$(common.runner.evaluateRule "$CXR_EMISSION_ASC_FILE_RULE" false CXR_EMISSION_ASC_FILE_RULE false)"

		# CXR_CHECK_THESE_OUTPUT_FILES is a space separated list of output files to check
		CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES $CXR_EMISSION_OUTPUT_FILE"
	done
}

################################################################################
# Function: create_emissions
#	
# Generates all emissions needed
################################################################################
function create_emissions() 
################################################################################
{
	# Define & Initialize local vars
	local i
	local exec_tmp_file
	local stop_h
	local start_h
	
	# This flag controls if only bio must be calculated (after 1 week)
	local bio_only = 0
	
	#Was this stage already completed?
	if [[ $(common.state.storeState ${CXR_STATE_START}) == true  ]]
	then
		#  --- Setup the Environment
		set_variables 
		
		#  --- Check Settings
		if [[ $(common.check.preconditions) == false ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		if [[ ! -f "${CXR_EMISSION_OUTPUT_FILE}" ]]
		then
			#File does not exist
			
			# We need a hash that contains the weekdays already processed
			# TODO: This is global, but must be cleaned after run!!
			common.hash.init create_emission_weekdays $CXR_HASH_TYPE_GLOBAL
			
			
			if [[ "$(common.hash.has? create_emission_weekdays $CXR_HASH_TYPE_GLOBAL $CXR_WOY)" == true ]] 
			then
				main.log -v "Weekday $CXR_WOY was already calculated. We will only re-calculate biogenic emissions"
				bio_only=2
			else
				common.hash.add create_emission_weekdays $CXR_HASH_TYPE_GLOBAL $CXR_WOY
				main.log -v "Weekday $CXR_WOY not yet calculated. We will calculate biogenic and anthropogenic emissions"
				bio_only=0
			fi
			
			# Increase global indent level
			main.increaseLogIndent
	
			main.log "Preparing current emission file..."
			
			# We will write the IDL call into a temporary file
			exec_tmp_file=$(common.runner.createTempFile $FUNCNAME)
			
			main.log  "Creating a temporary IDL command file in $exec_tmp_file"
			
			# Go there
			cd $(dirname ${CXR_IDL_EMISSION_GENERATOR}) || return $CXR_RET_ERROR
			
			# We create the emissions for each grid
			for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
			do
				main.log  "Creating Emissions for grid $i..."
				
				# Create the file to run IDL
				# Emissions stop at 23, while the model stops at 24
				# we ned to cut off 2 digits
				if [[ "${CXR_STOP_HOUR}" == 2400 ]]
				then
					stop_h=23
				else
					stop_h=${CXR_STOP_HOUR:0:2}
				fi
				
				start_h=${CXR_START_HOUR:0:2}
				
				cat <<-EOF > $exec_tmp_file
				.run $(basename ${CXR_IDL_EMISSION_GENERATOR})
				$(basename ${CXR_IDL_EMISSION_GENERATOR} .pro),${CXR_YEAR},${CXR_MONTH},${CXR_DAY},${start_h},${CXR_YEAR},${CXR_MONTH},${CXR_DAY},${stop_h},${i},'${CXR_MET_PROJECT}','${CXR_EMMISS_SCENARIO}','${CXR_MET_SCENARIO}',${bio_only},'${CXR_EMISSION_SOURCE_DIR}'
				exit
				EOF
				
				# Get a copy of the call
				cat ${exec_tmp_file} | tee -a $CXR_LOG
				
				# Only run if we are not in a dry run
				if [[ "$CXR_DRY" == false  ]]
				then
					# Then we run it in the background, while preserving the output
					${CXR_IDL_EXEC} < ${exec_tmp_file} 2>&1 | tee -a ${CXR_LOG}_create_emissions_${CXR_DATE}_${i} &
				else
					main.log "This is a dry-run, no action required"
				fi
			done
			
			# we need to wait for the processes
			# TODO: Check interaction wint parallel implementation!
			main.log -a "Waiting for ${CXR_NUMBER_OF_GRIDS} background processes..."
			wait

			# Get back
			cd ${CXR_RUN_DIR}  || return $CXR_RET_ERROR
	
			# Decrease global indent level
			main.decreaseLogIndent
	
			# Check if all went well
			if [[ $(common.check.postconditions) == false  ]]
			then
				main.log  "Postconditions for ${CXR_META_MODULE_NAME} are not met!"
				# We notify the caller of the problem
				return $CXR_RET_ERR_POSTCONDITIONS
			fi
			
		else
			# File exists. That is generally bad,
			# unless user wants to skip
			if [[ "$CXR_SKIP_EXISTING" == true  ]]
			then
				# Skip it
				main.log -w   "File $CXR_EMISSION_OUTPUT_FILE exists - because -S option was supplied, file will skipped."
				return 0
			else
				# Fail!
				main.log -e  "File $CXR_EMISSION_OUTPUT_FILE exists - to force the re-creation run ${CXR_CALL} -F"
				return $CXR_RET_ERROR
			fi
		fi

		# Store the state
		common.state.storeState ${CXR_STATE_STOP} > /dev/null
	else
		main.log  "Stage $(common.state.getStageName) was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
	fi
}

################################################################################
# Function: test_module
#
# Runs the predefined tests for this module
# 
# Parameters:
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
	
	# Initialise the date variables for first day
	day_offset=0
	common.date.setVars "$CXR_START_DATE" "$day_offset"
	set_variables

	# For this module, testing is harder 
	# compared to date_functions because we cannot just compare
	# Expected with actual results
	create_emissions
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.fs.isNotEmpty? ${CXR_EMISSION_OUTPUT_FILE}) true "create_emissions simple existence check, inspect ${CXR_EMISSION_OUTPUT_FILE}"
	
	
	
	if [[ "${CXR_TESTING_FROM_HARNESS:-false}" == false ]]
	then
		# We where called stand-alone, cleanupo is needed
		main.doCleanup
	fi
	
}


################################################################################
# Are we running stand-alone? 
################################################################################


# If the CXR_META_MODULE_NAME  is a subset of the progname,
# somebody started this script alone
# Normlly this is not allowed, exept to test using -t
if [[ $(expr match "$progname" ".*$CXR_META_MODULE_NAME.*") -gt 0  ]]
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
	fi
	
	usage
	
fi

################################################################################
# Code beyond this point is not executed in stand-alone operation
################################################################################



