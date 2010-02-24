#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Functions for the built-in task management - a very central part of the CAMxRunner.
# Prepares a pool of tasks, which are then harvested by cxr_common_worker threads. These threads can run parallel
# - even on different machines.
#
# First, a list of tasks is generated (this may pre-exist, then we leave this step out).
# This list is sorted according to the dependencies (topological sorting), so that tasks with no or few
# dependencies appear first.
# Then, we create a number of workers, which each get an exclusive entry of this list. They check first if the dependencies
# are already fulfilled, if not they wait.
# If they are fulfilled, the task starts.
# After successful execution, the worker gets the next task from the list.
# A task is identified by its module name and a day offset, like create_emissions_01. 
# One-Time Tasks have a day offset of 0, like initial_conditions_0. 
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# TODO: Simplify!
# TODO: Use tsort to create a topologically sorted list of tasks
# TODO: Periodically check CXR_MAX_PARALLEL_PROCS
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
CXR_META_MODULE_REQ_RUNNER_VERSION=750

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the functions to manage parallel task execution"

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
# Function: cxr_common_resolve_dependency
# 
# For a given dependency string and day offset, returns a resolved dependency string.
# This means that 
# - the predicate "-" is resolved to the day before (if available)
# - the special dependencies like "all_model" are resolved to all active modules of that class.
#
# Parameters:
# $1 - name of the dependency (a module name or a special dependency like "all_model"
# $2 - the day_offset for which this is to resolve
################################################################################
function cxr_common_resolve_dependency()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}" "Entering $FUNCNAME"
	
	if [[ "$CXR_IGNORE_ANY_DEPENDENCIES" == true  ]]
	then
		cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_ANY_DEPENDENCIES to true. We will not check dependencies (pretty dangerous...)"
		echo true
		cxr_main_logger -v "${FUNCNAME}" "Leaving $FUNCNAME"
		return $CXR_RET_OK
	fi
	
	if [[ $# -ne 2  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a depdendency and a day_offset as input"
	fi

	DEPENDENCY=$1
	DAY_OFFSET=$2
	
	cxr_main_logger -v "${FUNCNAME}" "Evaluating dependency on $DEPENDENCY for day offset $DAY_OFFSET"
	
	# Is there a predicate?
	# The last character of a string
	# We get 1 character at the position $(( ${#DEPENDENCY} -1)) (length of string minus 1)
	PREDICATE=${DEPENDENCY:$(( ${#DEPENDENCY} -1)):1}
	
	case $PREDICATE in
	
		-) # Currently, we only support -
			cxr_main_logger -v "${FUNCNAME}" "Found the - predicate, will check last days results"
			DAY_OFFSET=$(( $DAY_OFFSET - 1 ))
			
			# Cut off final -
			DEPENDENCY=${DEPENDENCY%-}
			if [[ $DAY_OFFSET -lt 0  ]]
			then
				# At day 0, all previous day predicates are fulfilled
				cxr_main_logger -v "${FUNCNAME}" "We are at day 0 - previous day dependency ok"
				echo true
				cxr_main_logger -v "${FUNCNAME}" "Leaving $FUNCNAME"
				return $CXR_RET_OK
			fi
			;;
		
	esac
	
	# Check if we look at a special dependency or not
	case $DEPENDENCY in
	
		$CXR_DEP_ALL_ONCE_PRE)
			LIST_FILE=$CXR_ACTIVE_ONCE_PRE_LIST
			MY_PREFIX="";; # Day irrelevant
			
		$CXR_DEP_ALL_DAILY_PRE) 
			LIST_FILE=$CXR_ACTIVE_DAILY_PRE_LIST
			MY_PREFIX=${DAY_OFFSET}_;;
			
		$CXR_DEP_ALL_DAILY_POST) 
			LIST_FILE=$CXR_ACTIVE_DAILY_POST_LIST
			MY_PREFIX=${DAY_OFFSET}_;;
			
		$CXR_DEP_ALL_ONCE_POST) 
			LIST_FILE=$CXR_ACTIVE_ONCE_POST_LIST
			MY_PREFIX="";; # Day irrelevant
			
		$CXR_DEP_ALL_MODEL) 
			LIST_FILE=$CXR_ACTIVE_MODEL_LIST
			MY_PREFIX=${DAY_OFFSET}_;;
			
	
		*) # A boring standard case
		
			# Is the dependency disabled?
			if [[ $(cxr_common_is_substring_present  "$(cat $CXR_ACTIVE_ALL_LIST)" "$DEPENDENCY") == false  ]]
			then
				# Do we care?
				if [[ "$CXR_IGNORE_DISABLED_DEPENDENCIES" == true  ]]
				then
					# No, user wants to ignore this
					cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_DISABLED_DEPENDENCIES to true and $DEPENDENCY is disabled. We will not check if this module was run"
					echo true
					cxr_main_logger -v "${FUNCNAME}" "Leaving $FUNCNAME"
					return $CXR_RET_OK
				else
					# Yes, we return false
					cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_DISABLED_DEPENDENCIES to false and $DEPENDENCY is disabled. The dependency $DEPENDENCY is not fulfilled!"
					echo false
					cxr_main_logger -v "${FUNCNAME}" "Leaving $FUNCNAME"
					return $CXR_RET_OK
				fi
			fi
			
			# Determine type
			MODULE_TYPE="$(cxr_common_get_module_type "$DEPENDENCY")"
			
			# Convert date
			cxr_common_raw_date="$(cxr_common_offset2_raw_date ${DAY_OFFSET})"
			
			MY_STAGE="$(cxr_common_get_stage_name "$MODULE_TYPE" "$DEPENDENCY" "$cxr_common_raw_date" )"
			
			# Is this known to have worked?
			if [[ "$(cxr_common_has_finished "$MY_STAGE")" == true  ]]
			then
				cxr_main_logger -v "${FUNCNAME}"  "Dependency ${DEPENDENCY} fullfilled"
				echo true
			else
				# Dependency NOK, Find out why
				
				# Find out if Dependency failed - if so, we crash
				if [[ "$(cxr_common_has_failed "$MY_STAGE")" == true  ]]
				then
					# It failed
					# Destroy run if no dryrun
					if [[ $CXR_DRY == false  ]]
					then
						cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - dependency ${DAY_OFFSET}_${DEPENDENCY} failed!"
					else
						cxr_main_logger -v "${FUNCNAME}" "The dependency ${DEPENDENCY} failed - but this is a dryrun, so we keep going!"
						echo true
					fi
				else
					# It did not fail, it seems that it was not yet run - we have to wait
					cxr_main_logger -v "${FUNCNAME}" "${DEPENDENCY} has not yet finished - we need to wait."
					echo false
				fi
			fi
			cxr_main_logger -v "${FUNCNAME}" "Leaving $FUNCNAME"
			return $CXR_RET_OK
			;;
	
	esac
	
	
	# Here, we handle the special cases
	# This dependencies require that all modules listed in
	# the LIST_FILE must have been run successfully.
	# Since the LIST_FILE only contains modules that are really run,
	# there is no need to check for isabled stuff!
	SKIP_IT=false
	
	cxr_main_logger -v "${FUNCNAME}" "We check the special dependency ${DEPENDENCY} using ${MY_PREFIX}..."
	
	while read MODULE
	do
	
		cxr_main_logger -v "${FUNCNAME}" "MY_PREFIX: ${MY_PREFIX}"
		cxr_main_logger -v "${FUNCNAME}" "MODULE: ${MODULE}"
		cxr_main_logger -v "${FUNCNAME}" "Found dependency on $MODULE - checking file $CXR_TASK_SUCCESSFUL_DIR/${MY_PREFIX}${MODULE}"
	
		# Determine type
		MODULE_TYPE="$(cxr_common_get_module_type "$MODULE")"
		
		# Convert date
		cxr_common_raw_date="$(cxr_common_offset2_raw_date ${DAY_OFFSET})"
		
		MY_STAGE="$(cxr_common_get_stage_name "$MODULE_TYPE" "$MODULE" "$cxr_common_raw_date" )"

	
		if [[ "$(cxr_common_has_finished "$MY_STAGE")" == true  ]]
		then
			# this one is ok, check next
			cxr_main_logger -v "${FUNCNAME}" "Dependency on $MODULE fullfilled - checking further..."
			continue
		else
			# Dependency NOK, Find out why
			cxr_main_logger -v "${FUNCNAME}" "Dependency on $MODULE not fullfilled, checking file $CXR_TASK_FAILED_DIR/${MY_PREFIX}${MODULE}"
			# Find out if Dependency failed - if so, we crash
			if [[ "$(cxr_common_has_failed "$MY_STAGE")" == true  ]]
			then
				# It failed
				# Destroy run if no dryrun
				if [[ $CXR_DRY == false  ]]
				then
					cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - dependency $DEPENDENCY failed because of $MODULE!"
				else
					cxr_main_logger -v "${FUNCNAME}" "The dependency ${DEPENDENCY} failed because of $MODULE - but this is a dryrun, so we keep going!"
					continue
				fi
			else
				# It did not fail, it seems that it was not yet run - we have to wait
				# Still, we must check the rest 
				cxr_main_logger -v "${FUNCNAME}" "The dependency ${DEPENDENCY} is not yet fulfilled, we must wait!"
				SKIP_IT=true
				# We continue, because we might need to fail in another case (failed dependency)
				continue
			fi
		fi
		
	done < "$LIST_FILE" # Loop over listed modules
	
	if [[ $SKIP_IT == true  ]]
	then
		cxr_main_logger -v "${FUNCNAME}" "Dependency $DEPENDENCY not ok!"
		echo false
	else
		cxr_main_logger -v "${FUNCNAME}" "Dependency $DEPENDENCY ok"
		echo true
	fi
	
	cxr_main_logger -v "${FUNCNAME}" "Leaving $FUNCNAME"
	return $CXR_RET_OK
}



################################################################################
# Function: cxr_common_create_task_descriptor
#
# Creates a file that identifies a task, later stored in state/RUN/tasks
# In the subdirectories of task, there will be links to these task descriptors.
# The name of a descriptor is a unique number identifying the task and giving the task
# its rank (they are executed ordered by rank).
#
# This function also fills files of "active modules" 
# ($CXR_ACTIVE_ONCE_PRE_LIST ...)
# This is later used for module type based dependencies like "all_preprocessors"
#
# Think about the parameters being positional parameters, so you need to give 
# at least the empty string "" for a parameter if you want to skip a parameter in between.
#
# Parameters:
# $1 - the task id (UNIQUE)
# $2 - the relative path to the module (from CXR_RUN_DIR)
# $3 - the module type
# $4 - the day offset to run the module with. 0 if the date is irrelevant (we need to initialize date vars anyway!)
# [$5] - the optional dependency string (a space separated list of module names that this module depends on)
# [$6] - optional flag, if true, this module is run exclusively (after starting it, no new tasks will be issued on the executing machine)
################################################################################
function cxr_common_create_task_descriptor()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}"  "Entering $FUNCNAME"
	
	NAME=$CXR_TASK_POOL_DIR/$(printf "%0${CXR_TASK_ID_DIGITS}d" $1)
	
	# Is this a unique number?
	if [[ -f $NAME  ]]
	then
		cxr_main_die_gracefully "The task Id $1 is already taken"
	fi
	
	MODULE_PATH="$2"
	MODULE_TYPE="$3"
	DAY_OFFSET="$4"
	DEPENDENCIES="$5"
	RUN_EXCLUSIVELY="$6"
	
	# List management
	case $MODULE_TYPE in
		
		"${CXR_TYPE_COMMON}" ) 
			cxr_main_die_gracefully "Common modules cannot be run this way!" ;;
			
		"${CXR_TYPE_PREPROCESS_ONCE}" ) 
			ACTIVE_LIST=$CXR_ACTIVE_ONCE_PRE_LIST;; 
			
		"${CXR_TYPE_PREPROCESS_DAILY}" ) 
			ACTIVE_LIST=$CXR_ACTIVE_DAILY_PRE_LIST;;
			
		"${CXR_TYPE_POSTPROCESS_DAILY}" ) 
			ACTIVE_LIST=$CXR_ACTIVE_DAILY_POST_LIST;;
			
		"${CXR_TYPE_POSTPROCESS_ONCE}" ) 
			ACTIVE_LIST=$CXR_ACTIVE_ONCE_POST_LIST;; 
			
		"${CXR_TYPE_MODEL}" ) 
			ACTIVE_LIST=$CXR_ACTIVE_MODEL_LIST;;
			
		"${CXR_TYPE_INSTALLER}" ) 
			cxr_main_die_gracefully "Installer modules cannot be run this way!" ;;
			
		* ) 
			cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Unknown module type $MODULE_TYPE" ;;
	esac
	
	# touch the list so we can cat later
	touch "$ACTIVE_LIST"
	
	MODULE_NAME="$(cxr_main_extract_module_name "$MODULE_PATH")"
	
	cxr_main_logger -v "${FUNCNAME}"  "${FUNCNAME}:${LINENO} - module name $MODULE_NAME, type $MODULE_TYPE"
	
	if [[ $(cxr_common_is_substring_present  "$(cat $ACTIVE_LIST)" "$MODULE_NAME") == false  ]]
	then
		# Add this module to the list if not already there
		echo -n " $MODULE_NAME" >> $ACTIVE_LIST
	fi
	
	# Also fill the "all" list
	if [[ $(cxr_common_is_substring_present  "$(cat $CXR_ACTIVE_ALL_LIST)" "$MODULE_NAME") == false  ]]
	then
		# Add this module to the list if not already there
		echo -n " $MODULE_NAME" >> $CXR_ACTIVE_ALL_LIST
	fi
	
	# We put all the values into a : Delimited string
	# Relative Path to module, Module type, Day offset, Run exclusively (boolean flag), Dependencies
	echo "${MODULE_PATH}${CXR_DELIMITER}${MODULE_TYPE}${CXR_DELIMITER}${DAY_OFFSET}${CXR_DELIMITER}${RUN_EXCLUSIVELY:-}${CXR_DELIMITER}${DEPENDENCIES:-}" > $NAME

	## Create the todo links
	if [[ ! -d "$CXR_TASK_TODO_DIR"  ]]
	then
		mkdir -p "$CXR_TASK_TODO_DIR"
	fi
	
	cd $CXR_TASK_TODO_DIR || die_gracefully "Could not change to dir $CXR_TASK_TODO_DIR"
	
	# General
	ln -s $NAME
	
	cd $CXR_RUN_DIR ||  die_gracefully "Could not change back to rundir."
		
	cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
}

################################################################################
# Function: cxr_common_add_modules
#	
# This is the workhorse of <cxr_common_create_task_list>
# Adds all single module pre/postproc/model but not installers
# to the task directory.
# Cannot be used to run single steps (this is always done sequentially)
#
# The function also checks if any module has dependencies to a disabled module
# that was not yet run. If it had, we need to fail (the run could not finish otherwise)
# Setting  "CXR_IGNORE_DISABLED_DEPENDENCIES=true" turns this check off
#
# Also, all modules are checked for their version compliance, only the good ones
# are entered.
#
# Parameters:
# $1 - Type of modules to run
################################################################################
function cxr_common_add_modules()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}" "Entering $FUNCNAME"
	
	MODULE_TYPE="$1"
	
	cxr_main_logger "${FUNCNAME}" "Loading $MODULE_TYPE modules..."
	
	# Variables:
	# MODULE_DIRECOTRIES - is a list of directories that will be used to search for modules
	# ENABLED_MODULES - is a list of explicitly enables modules of the current type
	# DISABLED_MODULES - is a list of disabled modules of the current type
	case "$MODULE_TYPE" in
	
		"${CXR_TYPE_COMMON}" ) 
			cxr_main_die_gracefully "Common modules cannot be run this way!" ;;
			
		"${CXR_TYPE_PREPROCESS_ONCE}" ) 
			MODULE_DIRECTORIES="$CXR_PREPROCESSOR_ONCE_INPUT_DIR" 
			DISABLED_MODULES="$CXR_DISABLED_ONCE_PREPROC"
			ENABLED_MODULES="$CXR_ENABLED_ONCE_PREPROC"
			SIMDAYS=0 # This module type is "timeless"
			cxr_main_logger "${FUNCNAME}" "One-Time Preprocessors are run only once";;
			
		"${CXR_TYPE_PREPROCESS_DAILY}" ) 
			MODULE_DIRECTORIES="$CXR_PREPROCESSOR_DAILY_INPUT_DIR" 
			DISABLED_MODULES="$CXR_DISABLED_DAILY_PREPROC"
			ENABLED_MODULES="$CXR_ENABLED_DAILY_PREPROC"
			SIMDAYS=$((${CXR_NUMBER_OF_SIM_DAYS} -1 ))
			cxr_main_logger "${FUNCNAME}" "Single Preprocessors are run for each of the ${CXR_NUMBER_OF_SIM_DAYS} simulation days";;
			
		"${CXR_TYPE_POSTPROCESS_DAILY}" ) 
			MODULE_DIRECTORIES="$CXR_POSTPROCESSOR_DAILY_INPUT_DIR" 
			DISABLED_MODULES="$CXR_DISABLED_DAILY_POSTPROC"
			ENABLED_MODULES="$CXR_ENABLED_DAILY_POSTPROC"
			SIMDAYS=$((${CXR_NUMBER_OF_SIM_DAYS} -1 ))
			cxr_main_logger "${FUNCNAME}" "Single Postprocessors are run for each of the ${CXR_NUMBER_OF_SIM_DAYS}  simulation days";;
			
		"${CXR_TYPE_POSTPROCESS_ONCE}" ) 
			MODULE_DIRECTORIES="$CXR_POSTPROCESSOR_ONCE_INPUT_DIR" 
			DISABLED_MODULES="$CXR_DISABLED_ONCE_POSTPROC"
			ENABLED_MODULES="$CXR_ENABLED_ONCE_POSTPROC"
			SIMDAYS=0 # This module type is "timeless"
			cxr_main_logger "${FUNCNAME}" "Finish Postprocessors are run only once" ;; 
			
		"${CXR_TYPE_MODEL}" ) 
			MODULE_DIRECTORIES="$CXR_MODEL_INPUT_DIR" 
			DISABLED_MODULES="$CXR_DISABLED_MODEL"
			ENABLED_MODULES="$CXR_ENABLED_MODEL"
			SIMDAYS=$((${CXR_NUMBER_OF_SIM_DAYS} -1 ))
			cxr_main_logger "${FUNCNAME}" "Model Modules are run for each of the ${CXR_NUMBER_OF_SIM_DAYS}  simulation days";;
			
		"${CXR_TYPE_INSTALLER}" ) 
			cxr_main_die_gracefully "Installer modules cannot be run this way!" ;;
			
		* ) 
			cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Unknown module type $MODULE_TYPE" ;;

	esac
	
	if [[ "$DISABLED_MODULES:-"  ]]
	then
		cxr_main_logger $FUNCNAME "Disabled modules: $DISABLED_MODULES"
	fi
	
	# Increase global indent level
	cxr_main_increase_log_indent
	
	## Now we need to loop through the days
	for DAY_OFFSET in $(seq 0 $SIMDAYS )
	do
	
		# if we run only 1 day, do it
		if [[ "${CXR_ONE_DAY}"  ]]
		then
			DAY_OFFSET="$(cxr_common_date2offset ${CXR_ONE_DAY})"
			cxr_main_logger "$FUNCNAME" "${CXR_ONE_DAY} corresponds to offset ${DAY_OFFSET}."
		fi
	
		cxr_main_logger -v "${FUNCNAME}"  "Scheduling $MODULE_TYPE tasks for day offset $DAY_OFFSET"
	
		# Check if we need any of them at all
		if [[ "${DISABLED_MODULES}" != "${CXR_SKIP_ALL}"  ]]
		then
		
			# We did not turn off everything.
		
			# Loop through available input dirs
			for MODULE_DIRECTORY in $MODULE_DIRECTORIES
			do
				# Loop through all files
				for FUNCTION_FILE in $(ls ${MODULE_DIRECTORY}/??_*.sh 2>/dev/null)
				do
					FILE_NAME=$(basename "$FUNCTION_FILE")
					
					# Before loading a new module, remove old meta variables
					unset ${!CXR_META_MODULE*}
					
					# Export the module name
					CXR_META_MODULE_NAME=$(cxr_main_extract_module_name $FUNCTION_FILE)
				
					# source the file to get the rest of the metadata
					source $FUNCTION_FILE
					
					# Check if we must run this
					# if the module name is in the enabled list, run it,no matter what
					if [[ "$(cxr_common_is_substring_present "$ENABLED_MODULES" "$CXR_META_MODULE_NAME")" == true  ]]
					then
						RUN_IT=true
					elif [[  "$(cxr_common_is_substring_present "$DISABLED_MODULES" "$CXR_META_MODULE_NAME")" == false && "${DISABLED_MODULES}" != "${CXR_SKIP_ALL}"   ]]
					then
						# Module was not explicitly disabled and we did not disable all
						RUN_IT=true
					else
						# If the name of the module is in the disabled list, this should not be run (except if it is in the enabled list)
						RUN_IT=false
						cxr_main_logger "${FUNCNAME}" "Step $FILE_NAME is disabled, skipped"
					fi
				
					# Execute if needed
					if [[ "$RUN_IT" == true  ]]
					then
							# Check the version
							if [[ "$(cxr_common_check_module_version)" == true  ]]
							then
								# Add this module
								cxr_common_create_task_descriptor "$CXR_CURRENT_ID" "$FUNCTION_FILE" "$MODULE_TYPE" "${DAY_OFFSET}" "${CXR_META_MODULE_DEPENDS_ON:-}" "${CXR_META_MODULE_RUN_EXCLUSIVELY:-false}"
							
								# Increase ID
								CXR_CURRENT_ID=$(( $CXR_CURRENT_ID + 1 ))
							else
								cxr_main_logger "${FUNCNAME}" "Version check for $CXR_META_MODULE_NAME failed - we do not run this module"
							fi
							
							# Take note that this module was already announced
							CXR_ANNOUNCED_MODULES="${CXR_ANNOUNCED_MODULES} ${CXR_META_MODULE_NAME}"
					fi
					
				done # Loop through files
				
			done # Loop through module dirs
		else
			cxr_main_logger "${FUNCNAME}" "You disabled all modules of type $MODULE_TYPE by setting  CXR_DISABLED_... to ${CXR_SKIP_ALL}, none will be executed."
		fi 
		
		# Decrease global indent level
		cxr_main_decrease_log_indent
		
		# If we do only 1 day, that's it
		if [[ "${CXR_ONE_DAY}"  ]]
		then
			break
		fi
		
	done # Loop through days
	
	cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
	
	return ${CXR_RET_OK}
}

################################################################################
# Function: cxr_common_create_task_list
# 
# Creates a process dependency tree
# 
# 
################################################################################
function cxr_common_create_task_list()
################################################################################
{

	cxr_main_logger -v "${FUNCNAME}"  "Entering $FUNCNAME"

	# Check if we already have tasks - fail if this is the case
	if [[ $(find "$CXR_TASK_POOL_DIR" -noleaf -maxdepth 1 -type f 2>/dev/null | wc -l ) -ne 0  ]]
	then
		cxr_main_logger "${FUNCNAME}" "There is already a tasklist - we will use it.\nIf you want to start from scratch, delete all state info using\n ${CXR_CALL} -c\n"
		cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
		return 0
	fi
	
	# Reset the ID counter
	CXR_CURRENT_ID=1
	
	# Add the different module types
	if [[ "$CXR_RUN_PRE_ONCE" == true  ]]
	then
		cxr_common_add_modules "${CXR_TYPE_PREPROCESS_ONCE}"
	fi
	
	if [[ "$CXR_RUN_PRE_DAILY" == true  ]]
	then
		cxr_common_add_modules "${CXR_TYPE_PREPROCESS_DAILY}"
	fi
	
	if [[ "$CXR_RUN_MODEL" == true  ]]
	then
		cxr_common_add_modules "${CXR_TYPE_MODEL}"
	fi
	
	if [[ "$CXR_RUN_POST_DAILY" == true  ]]
	then
		cxr_common_add_modules "${CXR_TYPE_POSTPROCESS_DAILY}"
	fi
	
	if [[ "$CXR_RUN_POST_ONCE" == true  ]]
	then
		cxr_common_add_modules "${CXR_TYPE_POSTPROCESS_ONCE}"
	fi
	
	cxr_main_logger "${FUNCNAME}" "This run consists of $(( $CXR_CURRENT_ID -1 )) tasks."
	
	cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
	
}

################################################################################
# Returns a sorted list of open tasks (machine based) to stdout.
# First come the machine-relevant tasks, then the general ones.
# Contains the full path of the todo links
#
# Note that ls sorts automatically
#
################################################################################
function cxr_common_get_open_tasks()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}"  "Entering $FUNCNAME"

	MACHINE_DIR=$CXR_TASK_TODO_DIR/$(uname -n)
	GENERAL_DIR=$CXR_TASK_TODO_DIR
	
	# First check out the machine dir if available
	if [[ -d $MACHINE_DIR  ]]
	then
		find "$MACHINE_DIR" -noleaf -maxdepth 1 -type l  2>/dev/null
	fi
	
	# Concatenate with rest
	find "$GENERAL_DIR" -noleaf -maxdepth 1 -type l 2>/dev/null
	
	cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
}

################################################################################
# Function: cxr_common_count_open_tasks
#
# Returns the number of open tasks. 
# Make sure you call this in a critical section (lock acquired), otherwise 
# a race condition might occur
# 
#
################################################################################
function cxr_common_count_open_tasks()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}" "Entering $FUNCNAME"

	# Find all links below CXR_TASK_TODO_DIR
	TASK_COUNT=$(find "$CXR_TASK_TODO_DIR" -noleaf -type l 2>/dev/null | wc -l)
	
	cxr_main_logger -v "${FUNCNAME}"  "Found $TASK_COUNT open tasks"
	
	echo $TASK_COUNT
	
	cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
}

################################################################################
# Function: cxr_common_is_dependency_ok
#
# Checks if a given dependency is fullfilled. If the dependency has failed,
# the run is destroyed, if the depdendency was not yet started, false is returned,
# if it is OK, true is returned.
#
# Can handle dependencies on a whole type and the predicate - (previous day)
#
# Checks if a dependency is listed in the list of active modules.
# This is better than checking for dependencies to be disabled
#
# If this is a dryrun, we go on even if a dependency failed.
#
# There are 2 main reasons why a dependency is not fulfilled:
# - it was not started yet or is still running, in this case we can wait
# - it failed, in this case we have to stop the run
# 
# We access the state DB using <cxr_common_has_finished>.
#
# Parameters:
# $1 - resolved name of the dependency like create_emissions_01
################################################################################
function cxr_common_is_dependency_ok()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}" "Entering $FUNCNAME"
	
	if [[ "$CXR_IGNORE_ANY_DEPENDENCIES" == true  ]]
	then
		cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_ANY_DEPENDENCIES to true. We will not check dependencies (pretty dangerous...)"
		echo true
		cxr_main_logger -v "${FUNCNAME}" "Leaving $FUNCNAME"
		return $CXR_RET_OK
	fi
	
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a depdendency as input"
	fi

	DEPENDENCY=$1
	
	cxr_main_logger -v "${FUNCNAME}" "Evaluating dependency on $DEPENDENCY for day offset $DAY_OFFSET"

	# Is the dependency disabled?
	if [[ $(cxr_common_is_substring_present  "$(cat $CXR_ACTIVE_ALL_LIST)" "$DEPENDENCY") == false  ]]
	then
	
		# Do we care?
		if [[ "$CXR_IGNORE_DISABLED_DEPENDENCIES" == true  ]]
		then
			# No, user wants to ignore this
			cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_DISABLED_DEPENDENCIES to true and $DEPENDENCY is disabled. We will not check if this module was run"
			echo true
			cxr_main_logger -v "${FUNCNAME}" "Leaving $FUNCNAME"
			return $CXR_RET_OK
		else
			# Yes, we return false
			cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_DISABLED_DEPENDENCIES to false and $DEPENDENCY is disabled. The dependency $DEPENDENCY is not fulfilled!"
			echo false
			cxr_main_logger -v "${FUNCNAME}" "Leaving $FUNCNAME"
			return $CXR_RET_OK
		fi
		
	fi
			
	# Determine type
	MODULE_TYPE="$(cxr_common_get_module_type "$DEPENDENCY")"
	
	# Convert date
	cxr_common_raw_date="$(cxr_common_offset2_raw_date ${DAY_OFFSET})"
	
	MY_STAGE="$(cxr_common_get_stage_name "$MODULE_TYPE" "$DEPENDENCY" "$cxr_common_raw_date" )"
	
	# Is this known to have worked?
	if [[ "$(cxr_common_has_finished "$MY_STAGE")" == true  ]]
	then
		cxr_main_logger -v "${FUNCNAME}"  "Dependency ${DEPENDENCY} fullfilled"
		echo true
	else
		# Dependency NOK, Find out why
		
		# Find out if Dependency failed - if so, we crash
		if [[ "$(cxr_common_has_failed "$MY_STAGE")" == true  ]]
		then
			# It failed
			# Destroy run if no dryrun
			if [[ $CXR_DRY == false  ]]
			then
				cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - dependency ${DAY_OFFSET}_${DEPENDENCY} failed!"
			else
				cxr_main_logger -v "${FUNCNAME}" "The dependency ${DEPENDENCY} failed - but this is a dryrun, so we keep going!"
				echo true
			fi
		else
			# It did not fail, it seems that it was not yet run - we have to wait
			cxr_main_logger -v "${FUNCNAME}" "${DEPENDENCY} has not yet finished - we need to wait."
			echo false
		fi
	fi
	cxr_main_logger -v "${FUNCNAME}" "Leaving $FUNCNAME"
	return $CXR_RET_OK
}

################################################################################
# Function: cxr_common_get_next_task_descriptor
#
# Returns the file of the task descriptor for the next task to execute
# 
#
# Parameters:
# $1 - the TASK_PID of the calling cxr_common_worker (to terminate it if needed)
################################################################################
function cxr_common_get_next_task_descriptor()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a TASK_PID as input"
	fi
	
	TASK_PID=$1
	
	# Here, we could account fo changes in CXR_MAX_PARALLEL_PROCS
	# 	Start new workers
	# 	Kill idle ones
	# 	BUT: CXR_MAX_PARALLEL_PROCS is a per-machine setting!
	
	# If the system wants exclusive access to all processors,
	# (e. g. to run the model in parallel mode) do not give out new assignments
	if [[ "$CXR_BLOCK_ASSIGNMENTS" == true  ]]
	then
		cxr_main_logger -v "${FUNCNAME}"  "Currently, no new tasks are being assigned. Probably the model is running"
		
		echo /dev/null
		return 0
	fi 

	# Entering critical section...
	cxr_common_get_lock cxr_common_get_next_task_descriptor
	
	TASK_COUNT=$(cxr_common_count_open_tasks)
	
	# Are there open tasks at all?
	if [[ "$TASK_COUNT" -eq 0  ]]
	then
		cxr_main_logger "${FUNCNAME}" "All tasks have been processed, I terminate all workers"
		# there are no more tasks, remove all continue file
		cxr_common_delete_continue_files
		echo /dev/null
		
		# Release lock
		cxr_common_release_lock cxr_common_get_next_task_descriptor
		
		cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
		return $CXR_RET_OK
	else
		cxr_main_logger -v "${FUNCNAME}"  "There are $TASK_COUNT unfinished tasks - we choose the most urgent one with fulfilled dependencies."
	fi

	# Now loop in search of a match
	for POTENTIAL_TASK in $(cxr_common_get_open_tasks)
	do
		SKIP_IT=false
		
		if [[ ! -f "${POTENTIAL_TASK}"  ]]
		then
			# There is no file behind the link!
			# Unset task to prevent issues if this was the last check!
			POTENTIAL_TASK=""
		else

			######################
			# Parse the descriptor
			######################
			
			# Save old IFS
			oIFS="$IFS"
			IFS="$CXR_DELIMITER"
			
			# Suck one line into DESCRIPTOR
			DESCRIPTOR=($(cat "${POTENTIAL_TASK}" | head -n1 ))
			
			# Reset IFS
			IFS="$oIFS"
			
			# extract the data from the array
			TASK="${DESCRIPTOR[$CXR_TASK_DESCR_I_PATH]}"
			TYPE="${DESCRIPTOR[$CXR_TASK_DESCR_I_TYPE]}"
			DAY_OFFSET="${DESCRIPTOR[$CXR_TASK_DESCR_I_OFFSET]}"
			EXCLUSIVE="${DESCRIPTOR[$CXR_TASK_DESCR_I_EXCLUSIVE]}"
			DEPENDENCIES="${DESCRIPTOR[$CXR_TASK_DESCR_I_DEPENDENCIES]:-}"
	
			if [[ -z "$DEPENDENCIES"  ]]
			then
			
				# If the string is empty, there are no dependencies,
				# we are happy to leave the loop
				cxr_main_logger -v "${FUNCNAME}" "No dependencies found - $TASK is the next task!"
				break
				
			else
			
				cxr_main_logger -v "${FUNCNAME}" "We must check the dependencies of $TASK $DAY_OFFSET"
			
				# Go trough each dependency
				for DEPENDENCY in $DEPENDENCIES
				do
				
					# cxr_common_is_dependency_ok terminates run, if the dependency has failed
					if [[ "$(cxr_common_is_dependency_ok "$DEPENDENCY" "$DAY_OFFSET")" == true  ]]
					then
						# OK - check next dependency
						cxr_main_logger -v "${FUNCNAME}" "Dependency $DEPENDENCY ok."
					else
						# We must wait
						# We continue because we want to check if further dependencies have failed
						SKIP_IT=true
					fi
				done # DEPENDENCY
	
				if [[ $SKIP_IT == true  ]]
				then
					# Go to next potential task
					cxr_main_logger -v "${FUNCNAME}" "The task $TASK $DAY_OFFSET cannot be run yet - check next one"
					# Unset task to prevent issues if this was the last check!
					POTENTIAL_TASK=""
				else
					# Found a good task
					cxr_main_logger -v "${FUNCNAME}" "Ready to execute $TASK $DAY_OFFSET"
					break
				fi
				
			fi # Dependencies found
		fi # Task available
		
	done # POTENTIAL_TASK
	
	# It can be that we did not get a string
	if [[ -z "$POTENTIAL_TASK"  ]]
	then
		# No task!
		cxr_main_logger -v "${FUNCNAME}" "Did not find anything suitable..."
		echo /dev/null
		cxr_main_logger -v "${FUNCNAME}" "Leaving $FUNCNAME"
		
		# Release lock
		cxr_common_release_lock cxr_common_get_next_task_descriptor
		
		return $CXR_RET_OK
	else
		# We got a task
		# Immediately assign it, so that we can leave the protected area (not an atomic operation!)
		NEW_DESCRIPTOR_NAME=$CXR_TASK_RUNNING_DIR/${TASK_PID}_$(basename $POTENTIAL_TASK)
		
		cxr_main_logger -v "${FUNCNAME}"  "New Descriptor is $NEW_DESCRIPTOR_NAME"
		
		mv $POTENTIAL_TASK $NEW_DESCRIPTOR_NAME
		
		# Release lock
		cxr_common_release_lock cxr_common_get_next_task_descriptor
		
		# Return full path
		echo "$NEW_DESCRIPTOR_NAME"
		cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
		
		return $CXR_RET_OK
	fi
}

################################################################################
# Function: cxr_common_change_task_status
#
# Just moves the tosk descriptor (used only for the users reference).
# As a workaround, we also notify the state DB (modules should do this!)
#
# Parameters:
# $1 - Descriptor file of task
# $2 - Status (SUCCESS/FAILURE)
################################################################################
function cxr_common_change_task_status()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}"  "Entering $FUNCNAME"

	if [[ $# -ne 2  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a task descriptor and a status as input"
	fi
	
	TASK_DESCRIPTOR_PATH="$1"
	TASK_DESCRIPTOR="$(basename "$TASK_DESCRIPTOR_PATH")"
	STATUS="$2"
	
	case $STATUS in
	
		$CXR_STATUS_SUCCESS) 
			DIRECTORY="$CXR_TASK_SUCCESSFUL_DIR";;
			
		$CXR_STATUS_FAILURE) 
			DIRECTORY="$CXR_TASK_FAILED_DIR"
			
			# Notify state DB (Environment is still OK)
			cxr_common_store_state ${CXR_STATE_ERROR}
			;;
			
		*)
			cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Status $STATUS not supported!"
	
	esac
	
	mv "$TASK_DESCRIPTOR_PATH" "$DIRECTORY/$TASK_DESCRIPTOR"
	
	cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
}

################################################################################
# Function: cxr_common_worker_waiting
#
# Add a TASK_PID file in the CXR_WAITING_WORKER_DIR
# Remove it from CXR_RUNNING_WORKER_DIR
#
# Parameters:
# $1 - TASK_PID of cxr_common_worker
################################################################################
function cxr_common_worker_waiting ()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}"  "Entering $FUNCNAME"
	
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a TASK_PID as input"
	fi
	
	TASK_PID=$1
	
	rm -f $CXR_RUNNING_WORKER_DIR/$TASK_PID >/dev/null 2>&1
	
	touch $CXR_WAITING_WORKER_DIR/$TASK_PID
	
	cxr_main_logger -v "${FUNCNAME}"  "cxr_common_worker (TASK_PID: $TASK_PID) changed its state to waiting"
	
	cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
}

################################################################################
# Function: cxr_common_worker_working
#
# Add a TASK_PID file in the CXR_RUNNING_WORKER_DIR
# Remove it from CXR_WAITING_WORKER_DIR
#
# Parameters:
# $1 - TASK_PID of cxr_common_worker
################################################################################
function cxr_common_worker_working ()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}"  "Entering $FUNCNAME"
	
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a TASK_PID as input"
	fi
	
	TASK_PID=$1
	
	rm -f $CXR_WAITING_WORKER_DIR/$TASK_PID >/dev/null 2>&1
	
	touch $CXR_RUNNING_WORKER_DIR/$TASK_PID
	
	cxr_main_logger -v "${FUNCNAME}"  "cxr_common_worker (TASK_PID: $TASK_PID) changed its state to working"
	
	cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
}

################################################################################
# Function: cxr_common_remove_worker
#
# kills the cxr_common_worker of the given TASK_PID and alse removes it from the process list.
# For this the TASK_PID is parsed
################################################################################
function cxr_common_remove_worker()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}"  "Entering $FUNCNAME"
	
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a TASK_PID as input"
	fi
	
	# Remove identifier
	rm -f $CXR_WORKER_DIR/$TASK_PID
	
	TASK_PID=$1
	
	PID=$(echo $TASK_PID | cut -d_ -f1)
	NODE=$(echo $TASK_PID | cut -d_ -f2)
	
	if [[ "$NODE" != "$(uname -n)"  ]]
	then
		cxr_main_logger "${FUNCNAME}" "Strange: $TASK_PID seems to run on $NODE rather than on $(uname -n)"
		return $CXR_RET_ERROR
	fi
	
	# Add check if the process is running at all!
	kill $PID 2>/dev/null
	
	# We do not care if the process was waiting or running
	rm -f $CXR_WAITING_WORKER_DIR/$TASK_PID >/dev/null 2>&1
	rm -f $CXR_RUNNING_WORKER_DIR/$TASK_PID >/dev/null 2>&1
	
	
	cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
}

################################################################################
# Function: cxr_common_request_exclusive_access
#
# Notifies controller that no new tasks should be given out for the time being
#
################################################################################
function cxr_common_request_exclusive_access()
################################################################################
{
	CXR_BLOCK_ASSIGNMENTS=true
}

################################################################################
# Function: cxr_common_release_exclusive_access
#
# Releases exculsive access
#
################################################################################
function cxr_common_release_exclusive_access()
################################################################################
{
	CXR_BLOCK_ASSIGNMENTS=false
}

################################################################################
# Function: cxr_common_worker
#
# This function is the workhorse of the parallel CAMxRunner. The Runner spawns one 
# or more of this functions to operate on the existing tasks.
# This can even be done from more than one machine.
################################################################################
function cxr_common_worker()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}"  "Entering $FUNCNAME"
	
	#Getting the PID is not easy, we do not want to many processes...
	TMP=$(cxr_common_create_tempfile $FUNCNAME)
	
	# The PID is the parent of the awk process
	# and the 4th field of /proc/self/stat is the PPID
	awk '{print $4}' /proc/self/stat > $TMP
	# We add the machine name so that it is unique among all machines
	TASK_PID=$(cat $TMP)_$(uname -n)
	
	# Create a file identifying the cxr_common_worker in the cxr_common_worker dir
	touch $CXR_WORKER_DIR/$TASK_PID
	
	cxr_main_logger -B "${FUNCNAME}" "Starting new cxr_common_worker (TASK_PID $TASK_PID)..."

	# Do we have more than 1 process?
	# If so, define process-specific stuff
	if [[  "$CXR_MAX_PARALLEL_PROCS" -gt 1 && "$CXR_DO_FILE_LOGGING" == true   ]]
	then
		# Set TASK_PID-dependent logfile to disentangle things
		CXR_LOG=${CXR_LOG%.log}_${TASK_PID}.log
		
		cxr_main_logger "${FUNCNAME}" "This cxr_common_worker will use its own logfile: ${CXR_LOG}"
	fi

	# We are not yet busy
	cxr_common_worker_waiting $TASK_PID

	# We stay in this loop as long as the continue file exists
	while [ -f "$CXR_CONTINUE_FILE" ]
	do
		# Do we stop here?
		cxr_common_do_we_continue || cxr_main_die_gracefully "Continue file no longer present."
	
		# cxr_common_get_next_task_descriptor must provide tasks in an atomic fashion (locking needed)
		# already moves the task descriptor into "running" position
		# We get a full file name
		NEW_TASK_DESCRIPTOR=$(cxr_common_get_next_task_descriptor $TASK_PID)
		
		# If we are on wait state, we get the non-file /dev/null back
		# Note that links to files are handled properly (they are files, too)
		if [[ -f "$NEW_TASK_DESCRIPTOR"  ]]
		then
			# Time to work
			cxr_common_worker_working $TASK_PID
			
			######################
			# Parse the descriptor
			######################
			
			# Save old IFS
			oIFS="$IFS"
			IFS="$CXR_DELIMITER"
			
			# Suck one line into DESCRIPTOR
			DESCRIPTOR=($(cat "$NEW_TASK_DESCRIPTOR" | head -n1 ))
			
			# Reset IFS
			IFS="$oIFS"
			
			# extract the needed data from the array
			TASK="${DESCRIPTOR[$CXR_TASK_DESCR_I_PATH]}"
			DAY_OFFSET="${DESCRIPTOR[$CXR_TASK_DESCR_I_OFFSET]}"
			EXCLUSIVE="${DESCRIPTOR[$CXR_TASK_DESCR_I_EXCLUSIVE]}"
			
			cxr_main_logger -v "${FUNCNAME}"  "TASK: $TASK\nDAY_OFFSET: $DAY_OFFSET\nEXCLUSIVE: $EXCLUSIVE"
			
			#Reserve resources if needed
			if [[ "$EXCLUSIVE" == true  ]]
			then
				cxr_main_logger "${FUNCNAME}" "This task needs exclusive access, we suspend the assignment of new tasks temporarily"
				cxr_common_request_exclusive_access
			fi
		
			# Setup environment
			cxr_common_set_date_variables "$CXR_START_DATE" "$DAY_OFFSET"
			
			cxr_main_logger -B "${FUNCNAME}"  "cxr_common_worker $TASK_PID assigned to $TASK for $CXR_DATE"
			
			# Before loading a new module, remove old meta variables
			unset ${!CXR_META_MODULE*}
			
			# Export the module name
			CXR_META_MODULE_NAME=$(cxr_main_extract_module_name $TASK)
			
			# source the file to get the rest of the metadata
			source $TASK
			
			# Now start the work.
			# The function we use is the Module name (derived from the file name)
			# We use the return status to determine if it was sucessful
			$CXR_META_MODULE_NAME || cxr_common_change_task_status $NEW_TASK_DESCRIPTOR $CXR_STATUS_FAILURE
			
			# This is a simple check (task_failed moves the descriptor away)
			if [[ -f $NEW_TASK_DESCRIPTOR  ]]
			then
				cxr_common_change_task_status $NEW_TASK_DESCRIPTOR $CXR_STATUS_SUCCESS
			fi
			
			#Release resources if needed
			if [[ "$EXCLUSIVE" == true  ]]
			then
				cxr_main_logger "${FUNCNAME}" "Activating the assignment of new tasks again."
				cxr_common_release_exclusive_access
			fi
		else
			# If we don't get anything, but we should, we terminate
			if [[ $CXR_BLOCK_ASSIGNMENTS == false  ]]
			then
				cxr_main_logger -v  "${FUNCNAME}" "cxr_common_worker $TASK_PID did not receive an assignment - there seem to be too many workers (starvation)"
			fi
		
			# This means that someone wants exclusive access, so we sleep
			cxr_common_worker_waiting $TASK_PID
			# Sleep a bit
			sleep $CXR_WAITING_SLEEP_SECONDS
		fi
	done
	
	# We have done our duty
	cxr_common_remove_worker $TASK_PID

	cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"

	exit $CXR_RET_OK
}

################################################################################
# Function: cxr_common_spawn_workers
#
# Parameters:
# $1 - number of workers to spawn
################################################################################
function cxr_common_spawn_workers()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}"  "Entering $FUNCNAME"
	
	cxr_main_logger "${FUNCNAME}" "We create now a number of $1 cxr_common_worker threads"
	
	for i in $(seq 1 $1)
	do
		# Create a cxr_common_worker and send it to background		
		cxr_common_worker &
	done
	
	cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
}

################################################################################
# Function: cxr_common_remove_all_workers
# 
# Removes all workers
# 
################################################################################
function cxr_common_remove_all_workers()
################################################################################
{
	cxr_main_logger -v "${FUNCNAME}" "Entering $FUNCNAME"
	
	cxr_main_logger "${FUNCNAME}" "We remove all workers now."
	
	for TASK_PID in $(find "$CXR_WORKER_DIR" -noleaf -name \*_$(uname -n) )
	do
		cxr_common_remove_worker "$(basename $TASK_PID)"
	done
	
	cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
}

################################################################################
# Function: cxr_common_wait_for_workers
#
# Basically a sleep function: we loop and check if the continue file is there.
#
################################################################################
function cxr_common_wait_for_workers()
################################################################################
{
		cxr_main_logger -v "${FUNCNAME}"  "Entering $FUNCNAME"
		cxr_main_logger "${FUNCNAME}" "Entering a wait loop (the work is carried out by backgound processes. I check every $CXR_WAITING_SLEEP_SECONDS seconds if all is done.)"
		
		while [ -f "$CXR_CONTINUE_FILE" ]
		do
			sleep $CXR_WAITING_SLEEP_SECONDS
		done
		
		cxr_main_logger -B "${FUNCNAME}"  "The Continue file is gone, all workers will stop asap."
		
		# OK, remove the workers now
		cxr_common_remove_all_workers
		
		cxr_main_logger -v "${FUNCNAME}"  "Leaving $FUNCNAME"
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
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	# None yet

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
	