#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Input Preparation
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# TODO: What do we do if a module returns non-zero? (Currently: Store and continue)
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
CXR_META_MODULE_DESCRIPTION="Contains the functions to run modules (only used for installer modules) for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008), CAMxRunner@psi.ch"

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
# Function: cxr_common_get_module_type
#
# Searches for a module name (not in common and installers) and returns its type. Module names need to be unique
# across all module types! Used by <cxr_common_is_dependency_ok>.
# We look at
#	CXR_PREPROCESSOR_DAILY_INPUT_DIR
#	CXR_PREPROCESSOR_ONCE_INPUT_DIR
#	CXR_POSTPROCESSOR_DAILY_INPUT_DIR
#	CXR_POSTPROCESSOR_ONCE_INPUT_DIR
#	CXR_MODEL_INPUT_DIR
# 
# Parameters:
# $1 - name of module (without prefix or suffix, just something like "convert output"
################################################################################
function cxr_common_get_module_type()
################################################################################
{
	if [ $# -ne 1 ]
	then
		cxr_main_logger -e "$FUNCNAME" "Need a module name as input"
	fi
	
	MODULE_TYPE=""
	NAME="??_${1}.sh"
	
	# Create an array of directories to work on
	# And an equivalent one for the types
	DIRS=($CXR_PREPROCESSOR_DAILY_INPUT_DIR $CXR_PREPROCESSOR_ONCE_INPUT_DIR $CXR_POSTPROCESSOR_DAILY_INPUT_DIR $CXR_POSTPROCESSOR_ONCE_INPUT_DIR $CXR_MODEL_INPUT_DIR)
	TYPES=($CXR_TYPE_PREPROCESS_DAILY $CXR_TYPE_PREPROCESS_ONCE $CXR_TYPE_POSTPROCESS_DAILY $CXR_TYPE_POSTPROCESS_ONCE $CXR_TYPE_MODEL)
	
	for i in $(seq 0 $(( ${#DIRS[@]} - 1 )) )
	do
		DIR=${DIRS[$i]}
		
		# Find the thingy
		if [ $(find $DIR -noleaf -name $NAME | wc -l) -ne 0 ] 
		then
			MODULE_TYPE=${TYPES[$i]}
			break
		fi
	done
	
	if [ "$MODULE_TYPE" ]
	then
		echo "$MODULE_TYPE"
	else
		die_gracefully "$FUNCNAME: Could not find module $1!"
	fi
	
}

################################################################################
# Function: cxr_common_run_modules
#	
# Calls all or just one single module (adressed by their module name) at a specific 
# point in time (or One-Time)
# Here, a sequential approach is implied.
# If a module is enabled explicitly it is always used (wins over disabled), 
# If its not enabled but disabled, the module is not run.
# 
#
# Parameters:
# $1 - Type of modules to run
# [$2] - Single Step (number)
################################################################################
function cxr_common_run_modules()
################################################################################
{
	MODULE_TYPE="$1"
	
	# Either contains a number (the only step to run)
	# or the string "all"
	RUN_ONLY="${2:-${CXR_RUN_ALL}}"
	
	#Return value - set optimisticaly
	RET_VAL=$CXR_RET_OK
	
	# Normally, we check continue, except installers
	local CHECK_CONTINUE=true
	
	# Contains the date for which we currently run if needed
	# We set it to the empty string if date is not relevant (One-Time modules)
	local OUR_DATE
	
	# Variables:
	# MODULE_DIRECOTRIES - is a list of directories that will be used to search for modules
	# ENABLED_MODULES - is a list of explicitly enables modules of the current type
	# DISABLED_MODULES - is a list of disabled modules of the current type
	case "$MODULE_TYPE" in
	
		"${CXR_TYPE_COMMON}" ) 
			cxr_main_die_gracefully "Common modules cannot be run this way!" ;;
			
		"${CXR_TYPE_PREPROCESS_ONCE}" ) 
			MODULE_DIRECTORIES="$CXR_PREPROCESSOR_ONCE_INPUT_DIR"
			ENABLED_MODULES="$CXR_ENABLED_ONCE_PREPROC"
			DISABLED_MODULES="$CXR_DISABLED_ONCE_PREPROC"
			OUR_DATE=;;
			
		"${CXR_TYPE_PREPROCESS_DAILY}" ) 
			MODULE_DIRECTORIES="$CXR_PREPROCESSOR_DAILY_INPUT_DIR"
			ENABLED_MODULES="$CXR_ENABLED_DAILY_PREPROC"
			DISABLED_MODULES="$CXR_DISABLED_DAILY_PREPROC"
			OUR_DATE=${CXR_DATE:-};;
			
		"${CXR_TYPE_POSTPROCESS_DAILY}" ) 
			MODULE_DIRECTORIES="$CXR_POSTPROCESSOR_DAILY_INPUT_DIR"
			ENABLED_MODULES="$CXR_ENABLED_DAILY_POSTPROC"
			DISABLED_MODULES="$CXR_DISABLED_DAILY_POSTPROC"
			OUR_DATE=${CXR_DATE:-};;
			
		"${CXR_TYPE_POSTPROCESS_ONCE}" ) 
			MODULE_DIRECTORIES="$CXR_POSTPROCESSOR_ONCE_INPUT_DIR"
			ENABLED_MODULES="$CXR_ENABLED_ONCE_POSTPROC"
			DISABLED_MODULES="$CXR_DISABLED_ONCE_POSTPROC"
			OUR_DATE=;;
			
		"${CXR_TYPE_MODEL}" ) 
			MODULE_DIRECTORIES="$CXR_MODEL_INPUT_DIR"
			ENABLED_MODULES="$CXR_ENABLED_MODEL"
			DISABLED_MODULES="$CXR_DISABLED_MODEL"
			OUR_DATE=${CXR_DATE:-};;
			
		"${CXR_TYPE_INSTALLER}" ) 
			MODULE_DIRECTORIES="$CXR_INSTALLER_INPUT_DIR $CXR_INSTALLER_MODEL_INPUT_DIR $CXR_INSTALLER_VERSION_INPUT_DIR" 
			ENABLED_MODULES="$CXR_ENABLED_INSTALLER"
			DISABLED_MODULES="$CXR_DISABLED_INSTALLER"
			CHECK_CONTINUE=false
			OUR_DATE=;;
			
		* ) 
			cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Unknown module type $MODULE_TYPE" ;;

	esac
	
	# Increase global indent level
	cxr_main_increase_log_indent
	
	# Check if we need any of them at all
	# If the user wants to run a specific module, we enter anyway
	if [ ! \( "${ENABLED_MODULES}" == "" -a "${DISABLED_MODULES}" == "${CXR_SKIP_ALL}" -a "$RUN_ONLY" == "${CXR_RUN_ALL}" \) ]
	then
	
		# We did not turn off everything or we need only a specific module to be run
	
		# Loop through available input dirs
		for MODULE_DIRECTORY in $MODULE_DIRECTORIES
		do
			cxr_main_logger "${FUNCNAME}" "Loading $MODULE_TYPE modules from $MODULE_DIRECTORY..."
		
			for FUNCTION_FILE in $(ls ${MODULE_DIRECTORY}/??_*.sh 2>/dev/null)
			do
				
				# Check if we are still happy if needed
				if [ "${CHECK_CONTINUE}" == true ]
				then
					cxr_common_do_we_continue || cxr_main_die_gracefully "Continue file no longer present."
				fi
				
				FILE_NAME=$(basename "$FUNCTION_FILE")
				
				# Before loading a new module, remove old meta variables
				unset ${!CXR_META_MODULE*}
				
				# Export the module name
				CXR_META_MODULE_NAME=$(cxr_main_extract_module_name $FUNCTION_FILE)
				
				if [ "$RUN_ONLY" != "${CXR_RUN_ALL}" ]
				then
				
					# is this the module we should run?
					# here we do no further checks on disabled/enabled
					if [ "$RUN_ONLY" == "${CXR_META_MODULE_NAME}" ]
					then
						# First source the file to get the CXR_META_MODULE_NAME
						source $FUNCTION_FILE
						
						# This is not needed for installers
						
						if [ "$MODULE_TYPE" != "$CXR_TYPE_INSTALLER" ]
						then
							cxr_main_logger -a -b "${FUNCNAME}"  "Running $FILE_NAME ${OUR_DATE:-}"
						fi
						
						# Show dependencies, if any
						if [ "${CXR_META_MODULE_DEPENDS_ON:-}" ]
						then
							cxr_main_logger -a -B "${FUNCNAME}" "This module depends on these modules:\n${CXR_META_MODULE_DEPENDS_ON}\nif it fails, run these dependencies first"
						fi

						# Increase global indent level
						cxr_main_increase_log_indent
						
						if [ "$(cxr_common_check_module_version)" == true ]
						then
							cxr_main_logger -v "${FUNCNAME}"  "Starting Module $CXR_META_MODULE_NAME"
							"$CXR_META_MODULE_NAME" || RET_VAL=$CXR_RET_ERROR
						else
							cxr_main_logger "${FUNCNAME}" "Version check for $CXR_META_MODULE_NAME failed. Either change the values in the head of the module or manipulate the revision numbers of either CAMxRunner.sh or the configuration.\nModule skipped."
						fi

						# Take note that this module was already announced
						CXR_ANNOUNCED_MODULES="${CXR_ANNOUNCED_MODULES} ${CXR_META_MODULE_NAME}"
							
						# Decrease global indent level
						cxr_main_decrease_log_indent
					fi
				else
					#Run all modules of the given type
				
					# First source the file to get the meta info
					source $FUNCTION_FILE
					
					# Check if we must run this
					# if the module name is in the enabled list, run it,no matter what
					if [ "$(cxr_common_is_substring_present "$ENABLED_MODULES" "$CXR_META_MODULE_NAME")" == true ]
					then
						# Module was explicitly enabled
						RUN_IT=true
					elif [ "$(cxr_common_is_substring_present "$DISABLED_MODULES" "$CXR_META_MODULE_NAME")" == false -a "${DISABLED_MODULES}" != "${CXR_SKIP_ALL}" ]
					then
						# Module was not explicitly disabled and we did not disable all
						RUN_IT=true
					else
						# If the name of the module is in the disabled list, this should not be run (except if it is in the enabled list)
						RUN_IT=false
						cxr_main_logger "${FUNCNAME}" "Step $FILE_NAME is disabled, skipped"
					fi
					
					# Execute if needed
					if [ "$RUN_IT" == true ]
					then
					
						if [ "$MODULE_TYPE" != "$CXR_TYPE_INSTALLER" ]
						then
							cxr_main_logger -a -b "${FUNCNAME}"  "Running $FILE_NAME ${OUR_DATE:-}"
						fi
						
						# Increase global indent level
						cxr_main_increase_log_indent
						
						if [ "$(cxr_common_check_module_version)" == true ]
						then
							cxr_main_logger -v "${FUNCNAME}"  "Starting Module $CXR_META_MODULE_NAME"
							"$CXR_META_MODULE_NAME" || RET_VAL=$CXR_RET_ERROR
						else
							cxr_main_logger "${FUNCNAME}" "Version check for $CXR_META_MODULE_NAME failed. Either change the values in the head of the module or manipulate the revision numbers of either CAMxRunner.sh or the configuration.\nModule skipped."
						fi

						# Take note that this module was already announced
						CXR_ANNOUNCED_MODULES="${CXR_ANNOUNCED_MODULES} ${CXR_META_MODULE_NAME}"
							
						# Decrease global indent level
						cxr_main_decrease_log_indent
					fi
					
				fi
			done
		done # Loop through module dirs
	else
		cxr_main_logger "${FUNCNAME}" "You disabled all modules of type $MODULE_TYPE by setting  CXR_DISABLED_... to ${CXR_SKIP_ALL}, none executed."
	fi 
	
	# Decrease global indent level
	cxr_main_decrease_log_indent
	
	return ${RET_VAL}
}

################################################################################
# Function: cxr_common_process_sequentially
#	
# Executes all (needed) modules in sequential order (non-parallel version of the task_functions)
# If only one process is used, this is faster because there is less overhead.
# For each part of the processing, we check if we need to run it and pass any limitations
# on to <cxr_common_run_modules>.
#
################################################################################
function cxr_common_process_sequentially
################################################################################
{
	# Set up return value
	RET_VAL=$CXR_RET_OK

	# Setup environment
	cxr_common_set_date_variables "$CXR_START_DATE" 0
	
	if [ ${CXR_RUN_PRE_ONCE} == true ]
	then
		cxr_common_run_modules ${CXR_TYPE_PREPROCESS_ONCE} ${CXR_RUN_PRE_ONCE_STEP:-${CXR_RUN_ALL}} || RET_VAL=$CXR_RET_ERROR
	else
		cxr_main_logger -w $FUNCNAME "We do not run ${CXR_TYPE_PREPROCESS_ONCE} modules."
	fi

	## Now we need to loop through the days
	# but only if the user wants any of this
	
	if [ ${CXR_RUN_PRE_DAILY} == true -o ${CXR_RUN_MODEL} == true -o ${CXR_RUN_POST_DAILY} == true ]
	then
		for DAY_OFFSET in $(seq 0 $((${CXR_NUMBER_OF_SIM_DAYS} -1 )) )
		do
		
			# if we run only 1 day, do it
			if [ "${CXR_ONE_DAY}" ]
			then
				DAY_OFFSET="$(cxr_common_date2offset ${CXR_ONE_DAY})"
				cxr_main_logger "$FUNCNAME" "${CXR_ONE_DAY} corresponds to offset ${DAY_OFFSET}."
			fi
		
			# Setup environment
			cxr_common_set_date_variables "$CXR_START_DATE" "$DAY_OFFSET"
			
			cxr_main_logger -B $FUNCNAME "Processing ${CXR_DATE:-now}"
			
			# Run the three daily module types in order
			
			if [ ${CXR_RUN_PRE_DAILY} == true ]
			then
				cxr_common_run_modules ${CXR_TYPE_PREPROCESS_DAILY} ${CXR_RUN_PRE_DAILY_STEP:-${CXR_RUN_ALL}} || RET_VAL=$CXR_RET_ERROR
			else
				cxr_main_logger -w $FUNCNAME "We do not run ${CXR_TYPE_PREPROCESS_DAILY} modules."
			fi
			
			if [ ${CXR_RUN_MODEL} == true ]
			then
				cxr_common_run_modules ${CXR_TYPE_MODEL} ${CXR_RUN_MODEL_SINGLE_STEP:-${CXR_RUN_ALL}} || RET_VAL=$CXR_RET_ERROR
			else
				cxr_main_logger -w $FUNCNAME "We do not run ${CXR_TYPE_MODEL} modules."
			fi
			
			if [ ${CXR_RUN_POST_DAILY} == true ]
			then
				cxr_common_run_modules ${CXR_TYPE_POSTPROCESS_DAILY} ${CXR_RUN_POST_DAILY_STEP:-${CXR_RUN_ALL}} || RET_VAL=$CXR_RET_ERROR
			else
				cxr_main_logger -w $FUNCNAME "We do not run ${CXR_TYPE_POSTPROCESS_DAILY} modules."
			fi
			
			# If we do only 1 day, that's it
			if [ "${CXR_ONE_DAY}" ]
			then
				break
			fi
			
		done # Loop through days
	else
		cxr_main_logger -w $FUNCNAME "We do not run any daily modules"
	fi
	
	if [ ${CXR_RUN_POST_ONCE} == true ]
	then
		cxr_common_run_modules ${CXR_TYPE_POSTPROCESS_ONCE} ${CXR_RUN_POST_ONCE_STEP:-${CXR_RUN_ALL}} || RET_VAL=$CXR_RET_ERROR
	else
		cxr_main_logger -w $FUNCNAME "We do not run ${CXR_TYPE_POSTPROCESS_ONCE} modules."
	fi
	
	return $RET_VAL
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