#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Functions to manage parallel execution of modules.
# The most important aspect of this is the management of the varius dependencies between modules.
#
# Prepares a pool of tasks, which are then harvested by cxr_common_parallel_worker threads. 
# These threads can run parallel - even on different machines.
#
# First, a list of tasks is generated (this may pre-exist, then we leave this step out).
# This list is sorted according to the dependencies (topological sorting), so that tasks with no or few
# dependencies appear first.
# Then, we create a number of workers, which each get a unique entry of this list. They check first if the dependencies
# are already fulfilled, if not they wait.
# If they are fulfilled, the task starts.
# After successful execution, the worker gets the next task from the list.
# A task is identified by its module name and a day offset, like create_emissions_01. 
# One-Time Tasks have no day offset, like initial_conditions. 
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# TODO: Periodically check CXR_MAX_PARALLEL_PROCS
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=1

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|dot|optional exec|tsort"

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
# Function: cxr_common_parallel_resolve_dependency
# 
# For a given dependency string, returns a resolved dependency string.
# This means that
# - the offset is taken from the dependency (if available), so create_emissions_0 means create emissions for the initial day
# - the predicate "-" is resolved to the day before (if available)
# - the special dependencies like "all_model" are resolved to all active modules of that class.
# - the resolved string is stored in the hash resolved_dependency_${CXR_MODEL}_${CXR_MODEL_VERSION}_$day_offset
#
# If a module is disabled, it is not resolved, but a warning is issued (but only if
# a module is mentioned directly, so if create_emissions is disabled, the dependency "all_daily_preprocessors"
# will not cause a warning, but "create_emissions" will. 
# This is because the hashes used to resolve these special dependencies are built much earlier.
#
# The returned string looks similar to this: "boundary_conditions_1 create_emissions_1"
#
# Parameters:
# $1 - name of the dependency with added day offset (a module name or a special dependency like "all_model_1")
################################################################################
function cxr_common_parallel_resolve_dependency()
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a depdendency as input"
	fi

	local dependency=$1
	local predicate
	local my_prefix
	local list_file
	local module_type
	local raw_date
	
	cxr_main_logger -v "${FUNCNAME}" "Resolving dependency $dependency for day offset $day_offset"
	
	# Is there a predicate?
	# The last character of a string
	# We get 1 character at the position $(( ${#dependency} -1)) (length of string minus 1)
	predicate=${dependency:$(( ${#dependency} -1)):1}
	
	case $predicate in
	
		-) # Currently, we only support -
			cxr_main_logger -v "${FUNCNAME}" "Found the - predicate, will check last days results"
			day_offset=$(( $day_offset - 1 ))
			
			# Cut off final -
			dependency=${dependency%-}
			if [[ $day_offset -lt 0  ]]
			then
				# At day 0, all previous day predicates are fulfilled
				cxr_main_logger -v "${FUNCNAME}" "We are at day 0 - previous day dependency ok"
				echo true
				
				return $CXR_RET_OK
			fi
			;;
		
	esac
	
	# Check if we look at a special dependency or not
	case $dependency in
	
		$CXR_DEP_ALL_ONCE_PRE)
			list_file=$CXR_ACTIVE_ONCE_PRE_LIST
			my_prefix="";; # Day irrelevant
			
		$CXR_DEP_ALL_DAILY_PRE) 
			list_file=$CXR_ACTIVE_DAILY_PRE_LIST
			my_prefix=${day_offset}_;;
			
		$CXR_DEP_ALL_DAILY_POST) 
			list_file=$CXR_ACTIVE_DAILY_POST_LIST
			my_prefix=${day_offset}_;;
			
		$CXR_DEP_ALL_ONCE_POST) 
			list_file=$CXR_ACTIVE_ONCE_POST_LIST
			my_prefix="";; # Day irrelevant
			
		$CXR_DEP_ALL_MODEL) 
			list_file=$CXR_ACTIVE_MODEL_LIST
			my_prefix=${day_offset}_;;
			
	
		*) # A boring standard case
		
			# Is the dependency disabled?
			if [[ $(cxr_common_is_substring_present  "$(cat $CXR_ACTIVE_ALL_LIST)" "$dependency") == false  ]]
			then
				# Do we care?
				if [[ "$CXR_IGNORE_DISABLED_DEPENDENCIES" == true  ]]
				then
					# No, user wants to ignore this
					cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_DISABLED_DEPENDENCIES to true and $dependency is disabled. We will not check if this module was run"
					echo true
					
					return $CXR_RET_OK
				else
					# Yes, we return false
					cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_DISABLED_DEPENDENCIES to false and $dependency is disabled. The dependency $dependency is not fulfilled!"
					echo false
					
					return $CXR_RET_OK
				fi
			fi
			
			# Determine type
			module_type="$(cxr_common_get_module_type "$dependency")"
			
			# Convert date
			raw_date="$(cxr_common_offset2_raw_date ${day_offset})"
			
			MY_STAGE="$(cxr_common_get_stage_name "$module_type" "$dependency" "$raw_date" )"
			
			# Is this known to have worked?
			if [[ "$(cxr_common_has_finished "$MY_STAGE")" == true  ]]
			then
				cxr_main_logger -v "${FUNCNAME}"  "dependency ${dependency} fullfilled"
				echo true
			else
				# dependency NOK, Find out why
				
				# Find out if dependency failed - if so, we crash
				if [[ "$(cxr_common_has_failed "$MY_STAGE")" == true  ]]
				then
					# It failed
					# Destroy run if no dryrun
					if [[ $CXR_DRY == false  ]]
					then
						cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - dependency ${day_offset}_${dependency} failed!"
					else
						cxr_main_logger -v "${FUNCNAME}" "The dependency ${dependency} failed - but this is a dryrun, so we keep going!"
						echo true
					fi
				else
					# It did not fail, it seems that it was not yet run - we have to wait
					cxr_main_logger -v "${FUNCNAME}" "${dependency} has not yet finished - we need to wait."
					echo false
				fi
			fi
			
			return $CXR_RET_OK
			;;
	
	esac
	
	# Here, we handle the special cases
	# This dependencies require that all modules listed in
	# the list_file must have been run successfully.
	# Since the list_file only contains modules that are really run,
	# there is no need to check for isabled stuff!
	skip_it=false
	
	cxr_main_logger -v "${FUNCNAME}" "We check the special dependency ${dependency} using ${my_prefix}..."
	
	while read MODULE
	do
	
		cxr_main_logger -v "${FUNCNAME}" "my_prefix: ${my_prefix}"
		cxr_main_logger -v "${FUNCNAME}" "MODULE: ${MODULE}"
		cxr_main_logger -v "${FUNCNAME}" "Found dependency on $MODULE - checking file $CXR_TASK_SUCCESSFUL_DIR/${my_prefix}${MODULE}"
	
		# Determine type
		module_type="$(cxr_common_get_module_type "$MODULE")"
		
		# Convert date
		raw_date="$(cxr_common_offset2_raw_date ${day_offset})"
		
		MY_STAGE="$(cxr_common_get_stage_name "$module_type" "$MODULE" "$raw_date" )"

	
		if [[ "$(cxr_common_has_finished "$MY_STAGE")" == true  ]]
		then
			# this one is ok, check next
			cxr_main_logger -v "${FUNCNAME}" "dependency on $MODULE fullfilled - checking further..."
			continue
		else
			# dependency NOK, Find out why
			cxr_main_logger -v "${FUNCNAME}" "dependency on $MODULE not fullfilled, checking file $CXR_TASK_FAILED_DIR/${my_prefix}${MODULE}"
			# Find out if dependency failed - if so, we crash
			if [[ "$(cxr_common_has_failed "$MY_STAGE")" == true  ]]
			then
				# It failed
				# Destroy run if no dryrun
				if [[ $CXR_DRY == false  ]]
				then
					cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - dependency $dependency failed because of $MODULE!"
				else
					cxr_main_logger -v "${FUNCNAME}" "The dependency ${dependency} failed because of $MODULE - but this is a dryrun, so we keep going!"
					continue
				fi
			else
				# It did not fail, it seems that it was not yet run - we have to wait
				# Still, we must check the rest 
				cxr_main_logger -v "${FUNCNAME}" "The dependency ${dependency} is not yet fulfilled, we must wait!"
				skip_it=true
				# We continue, because we might need to fail in another case (failed dependency)
				continue
			fi
		fi
		
	done < "$list_file" # Loop over listed modules
	
	if [[ $skip_it == true  ]]
	then
		cxr_main_logger -v "${FUNCNAME}" "dependency $dependency not ok!"
		echo false
	else
		cxr_main_logger -v "${FUNCNAME}" "dependency $dependency ok"
		echo true
	fi
	
	return $CXR_RET_OK
}

################################################################################
# Function: cxr_common_parallel_create_resolved_dep_list
# 
#
# Parameters:
# $1 - path to a unresolved dependency file
# $2 - path to the new, resolved file
################################################################################
function cxr_common_parallel_create_resolved_dep_list()
################################################################################
{
	:
}

################################################################################
# Function: cxr_common_parallel_create_descriptor
#
# Creates a file that identifies a task, later stored in state/RUN/tasks
# In the subdirectories of task, there will be links to these task descriptors.
# The name of a descriptor is a unique number identifying the task and giving the task
# its rank (they are executed ordered by rank).
#
# Think about the parameters being positional parameters, so you need to give 
# at least the empty string "" for a parameter if you want to skip a parameter in between.
#
# Parameters:
# $1 - the task id (UNIQUE)
# $2 - the relative path to the module (from CXR_RUN_DIR)
# $3 - the day offset to run the module with. 0 if the date is irrelevant (we need to initialize date vars anyway!)
# [$4] - optional flag, if true, this module is run exclusively (after starting it, no new tasks will be issued on the executing machine)
# [$5] - the optional dependency string (a space separated list of module names that this module depends on)
################################################################################
function cxr_common_parallel_create_descriptor()
################################################################################
{
	NAME=$CXR_TASK_POOL_DIR/$(printf "%0${CXR_TASK_ID_DIGITS}d" $1)
	
	# Is this a unique number?
	if [[ -f $NAME  ]]
	then
		cxr_main_die_gracefully "The task Id $1 is already taken"
	fi
	
	local module_path="$2"
	local day_offset="$3"
	local run_exclusively="$4"
	local dependencies="$5"
	local module_name="$(cxr_main_extract_module_name "$module_path")"
	
	cxr_main_logger -v "${FUNCNAME}"  "${FUNCNAME}:${LINENO} - module name $module_name, type $module_type"
	
	# We put all the values into a : Delimited string
	# Relative Path to module, Module type, Day offset, Run exclusively (boolean flag), dependencies
	echo "${module_path}${CXR_DELIMITER}${day_offset}${CXR_DELIMITER}${run_exclusively:-}${CXR_DELIMITER}${dependencies:-}" > $NAME

	## Create the todo links
	if [[ ! -d "$CXR_TASK_TODO_DIR"  ]]
	then
		mkdir -p "$CXR_TASK_TODO_DIR"
	fi
	
	cd $CXR_TASK_TODO_DIR || cxr_main_die_gracefully "Could not change to dir $CXR_TASK_TODO_DIR"
	
	# General
	ln -s $NAME
	
	cd $CXR_RUN_DIR ||  cxr_main_die_gracefully "Could not change back to rundir."
		
	
}

################################################################################
# Function: cxr_common_parallel_create_raw_dep_list
#	
# This is the workhorse of <cxr_common_parallel_init>
# Adds all single module pre/postproc/model but not installers
# to the file given. 
#
# The function checks if any module has dependencies to a disabled module
# that was not yet run. If it had, we need to fail (the run could never succeed otherwise)
# Setting  "CXR_IGNORE_DISABLED_DEPENDENCIES=true" turns this check off.
#
# This function also fills files of "active modules" 
# ($CXR_ACTIVE_ONCE_PRE_LIST ...)
# This is later used for module type based dependencies like "all_preprocessors"
#
# Also, all modules are checked for their version compliance, only the good ones
# are added.
#
# Parameters:
# $1 - File where to store the dependencies (for tsort)
################################################################################
function cxr_common_parallel_create_raw_dep_list()
################################################################################
{
	local dep_file="$1"
	local module_type
	local module_types
	local active_list
	local module_directories
	local module_directory
	local disabled_modules
	local enabled_modules
	local simdays
	local day_offset
	local function_file
	local file_name
	local run_it=false
	
	# Add the different module types
	if [[ "$CXR_RUN_PRE_ONCE" == true ]]
	then
		module_types="${module_types} ${CXR_TYPE_PREPROCESS_ONCE}"
	fi
	
	if [[ "$CXR_RUN_PRE_DAILY" == true ]]
	then
		module_types="${module_types} ${CXR_TYPE_PREPROCESS_DAILY}"
	fi
	
	if [[ "$CXR_RUN_MODEL" == true ]]
	then
		module_types="${module_types} ${CXR_TYPE_MODEL}"
	fi
	
	if [[ "$CXR_RUN_POST_DAILY" == true ]]
	then
		module_types="${module_types} ${CXR_TYPE_POSTPROCESS_DAILY}"
	fi
	
	if [[ "$CXR_RUN_POST_ONCE" == true ]]
	then
		module_types="${module_types} ${CXR_TYPE_POSTPROCESS_ONCE}"
	fi
	
	# Loop through the module types and add them
	for module_type in $module_types
	do
	
		cxr_main_logger "${FUNCNAME}" "Determining dependencies of $module_type modules..."
		
		# Variables:
		# module_directories - is a list of directories that will be used to search for modules
		# enabled_modules - is a list of explicitly enables modules of the current type
		# disabled_modules - is a list of disabled modules of the current type
		case "$module_type" in
		
			"${CXR_TYPE_COMMON}" ) 
				cxr_main_die_gracefully "Common modules cannot be run this way!" ;;
				
			"${CXR_TYPE_PREPROCESS_ONCE}" ) 
				active_list=$CXR_ACTIVE_ONCE_PRE_LIST
				module_directories="$CXR_PREPROCESSOR_ONCE_INPUT_DIR" 
				disabled_modules="$CXR_DISABLED_ONCE_PREPROC"
				enabled_modules="$CXR_ENABLED_ONCE_PREPROC"
				simdays=0 # This module type is "timeless"
				cxr_main_logger "${FUNCNAME}" "One-Time Preprocessors are run only once";;
				
			"${CXR_TYPE_PREPROCESS_DAILY}" )
				active_list=$CXR_ACTIVE_DAILY_PRE_LIST
				module_directories="$CXR_PREPROCESSOR_DAILY_INPUT_DIR" 
				disabled_modules="$CXR_DISABLED_DAILY_PREPROC"
				enabled_modules="$CXR_ENABLED_DAILY_PREPROC"
				simdays=$((${CXR_NUMBER_OF_SIM_DAYS} -1 ))
				cxr_main_logger "${FUNCNAME}" "Single Preprocessors are run for each of the ${CXR_NUMBER_OF_SIM_DAYS} simulation days";;
				
			"${CXR_TYPE_POSTPROCESS_DAILY}" )
				active_list=$CXR_ACTIVE_DAILY_POST_LIST
				module_directories="$CXR_POSTPROCESSOR_DAILY_INPUT_DIR" 
				disabled_modules="$CXR_DISABLED_DAILY_POSTPROC"
				enabled_modules="$CXR_ENABLED_DAILY_POSTPROC"
				simdays=$((${CXR_NUMBER_OF_SIM_DAYS} -1 ))
				cxr_main_logger "${FUNCNAME}" "Single Postprocessors are run for each of the ${CXR_NUMBER_OF_SIM_DAYS}  simulation days";;
				
			"${CXR_TYPE_POSTPROCESS_ONCE}" )
				active_list=$CXR_ACTIVE_ONCE_POST_LIST
				module_directories="$CXR_POSTPROCESSOR_ONCE_INPUT_DIR" 
				disabled_modules="$CXR_DISABLED_ONCE_POSTPROC"
				enabled_modules="$CXR_ENABLED_ONCE_POSTPROC"
				simdays=0 # This module type is "timeless"
				cxr_main_logger "${FUNCNAME}" "Finish Postprocessors are run only once" ;; 
				
			"${CXR_TYPE_MODEL}" )
				active_list=$CXR_ACTIVE_MODEL_LIST
				module_directories="$CXR_MODEL_INPUT_DIR" 
				disabled_modules="$CXR_DISABLED_MODEL"
				enabled_modules="$CXR_ENABLED_MODEL"
				simdays=$((${CXR_NUMBER_OF_SIM_DAYS} -1 ))
				cxr_main_logger "${FUNCNAME}" "Model Modules are run for each of the ${CXR_NUMBER_OF_SIM_DAYS}  simulation days";;
				
			"${CXR_TYPE_INSTALLER}" ) 
				cxr_main_die_gracefully "Installer modules cannot be run this way!" ;;
				
			* ) 
				cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Unknown module type $module_type" ;;
	
		esac
		
		# touch the active lists so we can cat later
		touch "$active_list"
		touch "$CXR_ACTIVE_ALL_LIST"
		
		if [[ "$disabled_modules:-"  ]]
		then
			cxr_main_logger $FUNCNAME "Disabled modules: $disabled_modules"
		fi
		
		# Increase global indent level
		cxr_main_increase_log_indent
		
		## Now we need to loop through the days
		for day_offset in $(seq 0 $simdays )
		do
		
			# if we run only 1 day, do it
			if [[ "${CXR_ONE_DAY}" ]]
			then
				day_offset="$(cxr_common_date2offset ${CXR_ONE_DAY})"
				cxr_main_logger "$FUNCNAME" "${CXR_ONE_DAY} corresponds to offset ${day_offset}."
			fi
		
			cxr_main_logger -v "${FUNCNAME}"  "Scheduling $module_type tasks for day offset $day_offset"
		
			# Check if we need any of them at all
			if [[ "${disabled_modules}" != "${CXR_SKIP_ALL}" ]]
			then
			
				# We did not turn off everything.
			
				# Loop through available input dirs
				for module_directory in $module_directories
				do
					# Loop through all files
					for function_file in $(ls ${module_directory}/??_*.sh 2>/dev/null)
					do
						file_name=$(basename "$function_file")
						
						# Before loading a new module, remove old meta variables
						unset ${!CXR_META_MODULE*}
						
						# Export the module name
						CXR_META_MODULE_NAME=$(cxr_main_extract_module_name $function_file)
					
						# source the file to get the rest of the metadata
						source $function_file
						
						# Check if we must run this
						# if the module name is in the enabled list, run it,no matter what
						if [[ "$(cxr_common_is_substring_present "$enabled_modules" "$CXR_META_MODULE_NAME")" == true  ]]
						then
							run_it=true
						elif [[  "$(cxr_common_is_substring_present "$disabled_modules" "$CXR_META_MODULE_NAME")" == false && "${disabled_modules}" != "${CXR_SKIP_ALL}"   ]]
						then
							# Module was not explicitly disabled and we did not disable all
							run_it=true
						else
							# If the name of the module is in the disabled list, this should not be run (except if it is in the enabled list)
							run_it=false
							cxr_main_logger -w "${FUNCNAME}" "Step $file_name is disabled, skipped.\nIt is possible that the run fails because dependencies cannot be fullfilled."
						fi
					
						# Execute if needed
						if [[ "$run_it" == true ]]
						then
								# Check the version
								if [[ "$(cxr_common_check_module_requirements)" == true  ]]
								then
									# Loop through dependencies and add them
									for dependency in ${CXR_META_MODULE_DEPENDS_ON:-}
									do
										# if simdays is 0 we have a "timeless" module type
										if [[ "${simdays}" -eq 0 ]]
										then
											echo "${dependency} ${CXR_META_MODULE_NAME}" >> "$dep_file"
										else
											echo "${dependency}_${day_offset} ${CXR_META_MODULE_NAME}_${day_offset}" >> "$dep_file"
										fi
									done
								else
									cxr_main_logger "${FUNCNAME}" "Version check for $CXR_META_MODULE_NAME failed - we do not run this module"
								fi
								
								# Add to active list
								if [[ $(cxr_common_is_substring_present  "$(cat $active_list)" "$CXR_META_MODULE_NAME") == false  ]]
								then
									# Add this module to the list if not already there
									echo -n " $CXR_META_MODULE_NAME" >> $active_list
								fi
								
								# Also fill the "all" list
								if [[ $(cxr_common_is_substring_present  "$(cat $CXR_ACTIVE_ALL_LIST)" "$CXR_META_MODULE_NAME") == false  ]]
								then
									# Add this module to the list if not already there
									echo -n " $CXR_META_MODULE_NAME" >> $CXR_ACTIVE_ALL_LIST
								fi
								
								# Take note that this module was already announced
								CXR_ANNOUNCED_MODULES="${CXR_ANNOUNCED_MODULES} ${CXR_META_MODULE_NAME}"
						fi
						
					done # Loop through files
					
				done # Loop through module dirs
			else
				cxr_main_logger "${FUNCNAME}" "You disabled all modules of type $module_type by setting  CXR_DISABLED_... to ${CXR_SKIP_ALL}, none will be executed."
			fi 
			
			# Decrease global indent level
			cxr_main_decrease_log_indent
			
			# If we do only 1 day, that's it
			if [[ "${CXR_ONE_DAY}" ]]
			then
				break
			fi
			
		done # Loop through days
	done # Loop trough module types
	
	return ${CXR_RET_OK}
}


################################################################################
# Function: cxr_common_parallel_draw_dependency_graph
# 
# Creates an image of the dependency graphy using dot (graphviz)
# 
# Parameters:
# $1 - a file describing the dependencies in tsort format
# [$2] - an output file (PDF)
################################################################################
function cxr_common_parallel_draw_dependency_graph()
################################################################################
{
	local input_file="$1"
	local output_file="${2:-$CXR_RUN_DIR/${CXR_RUN}_dep_$(date +"%Y_%m_%d_%H_%M").pdf}"
	local dot_file=$(cxr_common_create_tempfile $FUNCNAME)
	local elements
	
	echo "digraph dependencies" > "$dot_file"
	echo "{" >> "$dot_file"
	
	# Now go through each entry of the file, the form is
	# independent_mod dependent_mod
	# if the two names are the same, ignore them
	# always exchange the order.
	while read line
	do
		# IFS is ok with space
		elements=($line)
		indep=${elements[0]}
		dep=${elements[1]}
		
		if [[ $indep != $dep ]]
		then
			echo "    ${dep} -> ${indep} ;" >> "$dot_file"
		else
			cxr_main_logger -v "$FUNCNAME" "$indep equals $dep"
		fi
	
	done < "${input_file}"
	
	echo "}" >> "$dot_file"
	
	# Now call dot
	${CXR_DOT_EXEC} -Tpdf "${dot_file}" -o "${output_file}" 2>&1 | tee -a $CXR_LOG
	
	if [[ $(cxr_common_array_zero "${PIPESTATUS[@]}") == false ]]
	then
		cxr_main_logger -e "$FUNCNAME" "Could not visualize the dependencies."
	else	
		cxr_main_logger -a "$FUNCNAME" "You find a visualisation of the modules dependencies in the file ${output_file}"
	fi
}

################################################################################
# Function: cxr_common_parallel_init
# 
# Creates a sorted lists of files in the TODO directory. Calls <cxr_common_parallel_create_raw_dep_list>
# 
# 
################################################################################
function cxr_common_parallel_init()
################################################################################
{
	# Check if we already have tasks - fail if this is the case
	if [[ $(find "$CXR_TASK_POOL_DIR" -noleaf -maxdepth 1 -type f 2>/dev/null | wc -l ) -ne 0  ]]
	then
		cxr_main_logger "${FUNCNAME}" "There is already a tasklist - we will use it.\nIf you want to start from scratch, delete all state info using\n ${CXR_CALL} -c\n"
		return 0
	fi
	
	# Some tempfiles we need
	local unresolved_file="$(cxr_common_create_tempfile dep-unresolved)"
	local resolved_file="$(cxr_common_create_tempfile dep-resolved)"
	local sorted_file="$(cxr_common_create_tempfile tsort-out)"
	
	# Reset the ID counter
	local current_id=1
	
	cxr_main_logger -a "${FUNCNAME}" "Creating a list of dependencies..."
	
	cxr_common_parallel_create_raw_dep_list "$unresolved_file"
	
	cxr_main_logger -a "${FUNCNAME}" "Resolving dependencies..."
	
	cxr_common_parallel_create_resolved_dep_list "$unresolved_file" "$resolved_file"
	
	cxr_main_logger -a "${FUNCNAME}" "Ordering tasks..."
	
	${CXR_TSORT_EXEC} "$resolved_file" > "$sorted_file"
	
	if [[ $? -ne 0 ]]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - I could not figure out the correct order to execute the tasks. Most probably there is a cycle (Module A depends on B which in turn depends on A)"
	fi
	
	cxr_main_logger -a "${FUNCNAME}" "Creating todo-structure"
	
	while read line 
	do
	
		module_path=...
		day_offset=...
		
		
	
		# each line consists of a module[_day_offset]
		cxr_common_create_task_descriptor "$current_id" "$function_file" "${day_offset}" "${CXR_META_MODULE_RUN_EXCLUSIVELY:-false} "${CXR_META_MODULE_DEPENDS_ON:-}""
		
		# Increase ID
		current_id=$(( $current_id + 1 ))
	
	done < "$sorted_file"
	
	cxr_main_logger -a "${FUNCNAME}" "This run consists of $(( $current_id -1 )) tasks."
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
	

	# Find all links below CXR_TASK_TODO_DIR
	task_count=$(find "$CXR_TASK_TODO_DIR" -noleaf -type l 2>/dev/null | wc -l)
	
	cxr_main_logger -v "${FUNCNAME}"  "Found $task_count open tasks"
	
	echo $task_count
	
	
}

################################################################################
# Function: cxr_common_parallel_dependencies_ok?
#
# Checks if all given dependencies are fullfilled. If any dependency has failed,
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
# There are 2 main reasons why a dependency may not be fulfilled:
# - it was not started yet or is still running, in this case we can wait
# - it failed, in this case we have to stop the run
# 
# We access the state DB using <cxr_common_has_finished>.
#
# Parameters:
# $1 - a list of unresolved dependencies
# $2 - a day offset used as reference
################################################################################
function cxr_common_parallel_dependencies_ok?()
################################################################################
{
	if [[ "$CXR_IGNORE_ANY_DEPENDENCIES" == true  ]]
	then
		cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_ANY_DEPENDENCIES to true. We will not check dependencies (pretty dangerous...)"
		echo true
		return $CXR_RET_OK
	fi
	
	if [[ $# -ne 1 ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a depdendency as input"
	fi

	dependency=$1
	day_offset=$2
	
	cxr_main_logger -v "${FUNCNAME}" "Evaluating dependency on $dependency for day offset $day_offset"

	# Is the dependency disabled?
	if [[ $(cxr_common_is_substring_present  "$(cat $CXR_ACTIVE_ALL_LIST)" "$dependency") == false  ]]
	then
	
		# Do we care?
		if [[ "$CXR_IGNORE_DISABLED_DEPENDENCIES" == true  ]]
		then
			# No, user wants to ignore this
			cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_DISABLED_DEPENDENCIES to true and $dependency is disabled. We will not check if this module was run"
			echo true
			
			return $CXR_RET_OK
		else
			# Yes, we return false
			cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_DISABLED_DEPENDENCIES to false and $dependency is disabled. The dependency $dependency is not fulfilled!"
			echo false
			
			return $CXR_RET_OK
		fi
		
	fi
			
	# Determine type
	module_type="$(cxr_common_get_module_type "$dependency")"
	
	# Convert date
	raw_date="$(cxr_common_offset2_raw_date ${day_offset})"
	
	MY_STAGE="$(cxr_common_get_stage_name "$module_type" "$dependency" "$raw_date" )"
	
	# Is this known to have worked?
	if [[ "$(cxr_common_has_finished "$MY_STAGE")" == true ]]
	then
		cxr_main_logger -v "${FUNCNAME}"  "dependency ${dependency} fullfilled"
		echo true
	else
		# dependency NOK, Find out why
		
		# Find out if dependency failed - if so, we crash
		if [[ "$(cxr_common_has_failed "$MY_STAGE")" == true  ]]
		then
			# It failed
			# Destroy run if no dryrun
			if [[ $CXR_DRY == false  ]]
			then
				cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - dependency ${day_offset}_${dependency} failed!"
			else
				cxr_main_logger -v "${FUNCNAME}" "The dependency ${dependency} failed - but this is a dryrun, so we keep going!"
				echo true
			fi
		else
			# It did not fail, it seems that it was not yet run - we have to wait
			cxr_main_logger -v "${FUNCNAME}" "${dependency} has not yet finished - we need to wait."
			echo false
		fi
	fi
	
	return $CXR_RET_OK
}

################################################################################
# Function: cxr_common_parallel_get_next_task
#
# Returns the file of the task descriptor for the next task to execute.
# If there are no more tasks or a module runs exclusively, /dev/null is returned.
# If all tasks are executed, deletes the continue file.
# 
# Parameters:
# $1 - the task_pid of the calling cxr_common_parallel_worker (to terminate it if needed)
################################################################################
function cxr_common_parallel_get_next_task()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a task_pid as input"
	fi
	
	local task_pid=$1
	local task_count
	local potential_task
	local skip_it
	local descriptor
	local new_descriptor_name
	
	# If the system wants exclusive access to all processors,
	# (e. g. to run the model in parallel mode) do not give out new assignments
	if [[ "$CXR_BLOCK_ASSIGNMENTS" == true  ]]
	then
		cxr_main_logger -v "${FUNCNAME}"  "Currently, no new tasks are being assigned. Probably the model is running"
		
		echo /dev/null
		return 0
	fi 

	# Entering critical section...
	cxr_common_get_lock cxr_common_parallel_get_next_task
	
	task_count=$(cxr_common_count_open_tasks)
	
	# Are there open tasks at all?
	if [[ "$task_count" -eq 0  ]]
	then
		cxr_main_logger "${FUNCNAME}" "All tasks have been processed, notifing system..."
		# there are no more tasks, remove all continue file
		cxr_common_delete_continue_files
		echo /dev/null
		
		# Release lock
		cxr_common_release_lock cxr_common_parallel_get_next_task
		return $CXR_RET_OK
	else
		cxr_main_logger -v "${FUNCNAME}"  "There are $task_count unfinished tasks - we choose the top one."
	fi
	
	# get first file
	potential_task="$(ls "$CXR_TASK_TODO_DIR" 2>/dev/null | head -n1)"
	
	# It can be that we did not get a string
	if [[ -z "$potential_task" ]]
	then
		# No task!
		cxr_main_logger -v "${FUNCNAME}" "Did not find any task..."
		echo /dev/null
		
		# Release lock
		cxr_common_release_lock cxr_common_parallel_get_next_task
		
		return $CXR_RET_OK
	else
		# We got a task
		# Immediately assign it, so that we can leave the protected area (not an atomic operation!)
		new_descriptor_name=$CXR_TASK_RUNNING_DIR/${task_pid}_$(basename $potential_task)
		
		cxr_main_logger -v "${FUNCNAME}"  "New descriptor is $new_descriptor_name"
		
		mv $potential_task $new_descriptor_name
		
		# Release lock
		cxr_common_release_lock cxr_common_parallel_get_next_task
		
		# Return full path
		echo "$new_descriptor_name"
		return $CXR_RET_OK
	fi
}

################################################################################
# Function: cxr_common_task_change_status
#
# Just moves the tosk descriptor (used only for the users reference).
# As a workaround, we also notify the state DB (modules should do this!)
#
# Parameters:
# $1 - descriptor file of task
# $2 - status (SUCCESS/FAILURE)
################################################################################
function cxr_common_task_change_status()
################################################################################
{
	local task_descriptor_path
	local task_descriptor
	local status
	
	if [[ $# -ne 2  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a task descriptor and a status as input"
	fi
	
	task_descriptor_path="$1"
	task_descriptor="$(basename "$task_descriptor_path")"
	status="$2"
	
	case $status in
	
		$CXR_STATUS_SUCCESS) 
			DIRECTORY="$CXR_TASK_SUCCESSFUL_DIR";;
			
		$CXR_STATUS_FAILURE) 
			DIRECTORY="$CXR_TASK_FAILED_DIR"
			
			# Notify state DB (Environment is still OK)
			cxr_common_store_state ${CXR_STATE_ERROR}
			;;
			
		*)
			cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - status $status not supported!"
	
	esac
	
	mv "$task_descriptor_path" "$DIRECTORY/$task_descriptor"
	
	
}

################################################################################
# Function: cxr_common_parallel_worker_waiting
#
# Add a task_pid file in the CXR_WAITING_WORKER_DIR
# Remove it from CXR_RUNNING_WORKER_DIR
#
# Parameters:
# $1 - task_pid of cxr_common_parallel_worker
################################################################################
function cxr_common_parallel_worker_waiting ()
################################################################################
{
	
	
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a task_pid as input"
	fi
	
	local task_pid=$1
	
	rm -f $CXR_RUNNING_WORKER_DIR/$task_pid >/dev/null 2>&1
	
	touch $CXR_WAITING_WORKER_DIR/$task_pid
	
	cxr_main_logger -v "${FUNCNAME}"  "cxr_common_parallel_worker (task_pid: $task_pid) changed its state to waiting"
	
	
}

################################################################################
# Function: cxr_common_parallel_worker_working
#
# Add a task_pid file in the CXR_RUNNING_WORKER_DIR
# Remove it from CXR_WAITING_WORKER_DIR
#
# Parameters:
# $1 - task_pid of cxr_common_parallel_worker
################################################################################
function cxr_common_parallel_worker_working ()
################################################################################
{
	
	
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a task_pid as input"
	fi
	
	local task_pid=$1
	
	rm -f $CXR_WAITING_WORKER_DIR/$task_pid >/dev/null 2>&1
	
	touch $CXR_RUNNING_WORKER_DIR/$task_pid
	
	cxr_main_logger -v "${FUNCNAME}"  "cxr_common_parallel_worker (task_pid: $task_pid) changed its state to working"
	
	
}

################################################################################
# Function: cxr_common_parallel_remove_worker
#
# kills the cxr_common_parallel_worker of the given task_pid and alse removes it from the process list.
# For this the task_pid is parsed
################################################################################
function cxr_common_parallel_remove_worker()
################################################################################
{
	
	
	local task_pid
	local pid
	local node
	
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a task_pid as input"
	fi
	
	# Remove identifier
	rm -f $CXR_WORKER_DIR/$task_pid
	
	task_pid=$1
	
	pid=$(echo $task_pid | cut -d_ -f1)
	node=$(echo $task_pid | cut -d_ -f2)
	
	if [[ "$node" != "$(uname -n)"  ]]
	then
		cxr_main_logger "${FUNCNAME}" "Strange: $task_pid seems to run on $node rather than on $(uname -n)"
		return $CXR_RET_ERROR
	fi
	
	# Add check if the process is running at all!
	kill $pid 2>/dev/null
	
	# We do not care if the process was waiting or running
	rm -f $CXR_WAITING_WORKER_DIR/$task_pid >/dev/null 2>&1
	rm -f $CXR_RUNNING_WORKER_DIR/$task_pid >/dev/null 2>&1
	
	
	
}

################################################################################
# Function: cxr_common_parallel_exclusive_start
#
# Notifies controller that no new tasks should be given out for the time being
#
################################################################################
function cxr_common_parallel_exclusive_start()
################################################################################
{
	CXR_BLOCK_ASSIGNMENTS=true
}

################################################################################
# Function: cxr_common_parallel_exclusive_stop
#
# Releases exculsive access
#
################################################################################
function cxr_common_parallel_exclusive_stop()
################################################################################
{
	CXR_BLOCK_ASSIGNMENTS=false
}

################################################################################
# Function: cxr_common_parallel_worker
#
# This function is the workhorse of the parallel CAMxRunner. The Runner spawns one 
# or more of this functions to operate on the existing tasks.
# This can even be done from more than one machine.
#
# The worker gets a new task via <cxr_common_parallel_get_next_task>, but he must then 
# make sure (using <cxr_common_parallel_dependencies_ok?>)
# it waits until the dependencies of this task are fullfilled. 
################################################################################
function cxr_common_parallel_worker()
################################################################################
{
	local tmp
	local task_pid
	local new_task_descriptor
	local oIFS
	local descriptor
	local task
	local exclusive
	local dependencies
	
	#Getting the pid is not easy, we do not want to many processes...
	tmp=$(cxr_common_create_tempfile $FUNCNAME)
	
	# The pid is the parent of the awk process
	# and the 4th field of /proc/self/stat is the PPID
	awk '{print $4}' /proc/self/stat > $tmp
	# We add the machine name so that it is unique among all machines
	task_pid=$(cat $tmp)_$(uname -n)
	
	# Create a file identifying the cxr_common_parallel_worker in the cxr_common_parallel_worker dir
	touch $CXR_WORKER_DIR/$task_pid
	
	cxr_main_logger -B "${FUNCNAME}" "Starting new cxr_common_parallel_worker (task_pid $task_pid)..."

	# Do we have more than 1 process?
	# If so, define process-specific stuff
	if [[  "$CXR_MAX_PARALLEL_PROCS" -gt 1 && "$CXR_DO_FILE_LOGGING" == true ]]
	then
		# Set task_pid-dependent logfile to disentangle things
		CXR_LOG=${CXR_LOG%.log}_${task_pid}.log
		
		cxr_main_logger "${FUNCNAME}" "This cxr_common_parallel_worker will use its own logfile: ${CXR_LOG}"
	fi

	# We are not yet busy
	cxr_common_parallel_worker_waiting $task_pid

	# We stay in this loop as long as the continue file exists
	while [[ -f "$CXR_CONTINUE_FILE" ]]
	do
		# Do we stop here?
		cxr_common_do_we_continue || cxr_main_die_gracefully "Continue file no longer present."
	
		# cxr_common_parallel_get_next_task must provide tasks in an atomic fashion (locking needed)
		# already moves the task descriptor into "running" position
		# We get a full file name
		new_task_descriptor=$(cxr_common_parallel_get_next_task $task_pid)
		
		# If we are on wait state, we get the non-file /dev/null back
		if [[ -f "$new_task_descriptor" ]]
		then
			
			######################
			# Parse the descriptor
			######################
			
			# Save old IFS
			oIFS="$IFS"
			IFS="$CXR_DELIMITER"
			
			# Suck one line into descriptor
			descriptor=($(cat "$new_task_descriptor" | head -n1 ))
			
			# Reset IFS
			IFS="$oIFS"
			
			# extract the needed data from the array
			task="${descriptor[$CXR_TASK_DESCR_I_PATH]}"
			day_offset="${descriptor[$CXR_TASK_DESCR_I_OFFSET]}"
			exclusive="${descriptor[$CXR_TASK_DESCR_I_EXCLUSIVE]}"
			dependencies="${descriptor[$CXR_TASK_DESCR_I_DEPENDENCIES]}"
			
			# We need to wait until all dependencies are ok
			cxr_common_parallel_worker_waiting $task_pid
			until [[ "$(cxr_common_parallel_dependencies_ok? "$dependencies" "$day_offset" )" == true ]]
			do
				sleep $CXR_WAITING_SLEEP_SECONDS
			done
			
			# Time to work
			cxr_common_parallel_worker_working $task_pid
			
			cxr_main_logger -v "${FUNCNAME}"  "task: $task\nDAY_OFFSET: $day_offset\nEXCLUSIVE: $exclusive"
			
			#Reserve resources if needed
			if [[ "$exclusive" == true  ]]
			then
				cxr_main_logger "${FUNCNAME}" "This task needs exclusive access, we suspend the assignment of new tasks temporarily"
				cxr_common_parallel_exclusive_start
			fi
		
			# Setup environment
			cxr_common_set_date_variables "$CXR_START_DATE" "$day_offset"
			
			cxr_main_logger -B "${FUNCNAME}"  "cxr_common_parallel_worker $task_pid assigned to $task for $CXR_DATE"
			
			# Before loading a new module, remove old meta variables
			unset ${!CXR_META_MODULE*}
			
			# Export the module name
			CXR_META_MODULE_NAME=$(cxr_main_extract_module_name $task)
			
			# source the file to get the rest of the metadata
			source $task
			
			# Now start the work.
			# The function we use is the Module name (derived from the file name)
			# We use the return status to determine if it was successful
			$CXR_META_MODULE_NAME || cxr_common_task_change_status $new_task_descriptor $CXR_STATUS_FAILURE
			
			# This is a simple check (task_failed moves the descriptor away)
			if [[ -f $new_task_descriptor  ]]
			then
				cxr_common_task_change_status $new_task_descriptor $CXR_STATUS_SUCCESS
			fi
			
			#Release resources if needed
			if [[ "$exclusive" == true  ]]
			then
				cxr_main_logger "${FUNCNAME}" "Activating the assignment of new tasks again."
				cxr_common_parallel_exclusive_stop
			fi
		else
			# If we don't get anything, but we should, we terminate
			if [[ $CXR_BLOCK_ASSIGNMENTS == false ]]
			then
				cxr_main_logger -v  "${FUNCNAME}" "cxr_common_parallel_worker $task_pid did not receive an assignment - there seem to be too many workers (starvation)"
			fi
		
			# This means that someone wants exclusive access, so we sleep
			cxr_common_parallel_worker_waiting $task_pid
			# Sleep a bit
			sleep $CXR_WAITING_SLEEP_SECONDS
		fi
	done
	
	# We have done our duty
	cxr_common_parallel_remove_worker $task_pid

	

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
	
	
	cxr_main_logger "${FUNCNAME}" "We create now a number of $1 cxr_common_parallel_worker threads"
	
	for i in $(seq 1 $1)
	do
		# Create a cxr_common_parallel_worker and send it to background		
		cxr_common_parallel_worker &
	done
	
	
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
	cxr_main_logger "${FUNCNAME}" "We remove all workers now."
	
	for task_pid in $(find "$CXR_WORKER_DIR" -noleaf -name \*_$(uname -n) )
	do
		cxr_common_parallel_remove_worker "$(basename $task_pid)"
	done
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
		cxr_main_logger "${FUNCNAME}" "Entering a wait loop (the work is carried out by backgound processes. I check every $CXR_WAITING_SLEEP_SECONDS seconds if all is done.)"
		
		while [ -f "$CXR_CONTINUE_FILE" ]
		do
			sleep $CXR_WAITING_SLEEP_SECONDS
		done
		
		cxr_main_logger -B "${FUNCNAME}"  "The Continue file is gone, all workers will stop asap."
		
		# OK, remove the workers now
		cxr_common_remove_all_workers
		
		
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
	# Fake a dep file
	local dep_file=$(cxr_common_create_tempfile $FUNCNAME)
	
	# This is just to play
	echo "create_emissions_0 model_0" >> "${dep_file}"
	echo "convert_emissions_0 model_0" >> "${dep_file}"
	echo "convert_input_0 model_0" >> "${dep_file}"
	echo "initial_conditions model_0" >> "${dep_file}"
	echo "boundary_conditions_0 model_0" >> "${dep_file}"
	
	pdf_file=$CXR_RUN_DIR/${CXR_RUN}_dep_$(date +"%Y_%m_%d_%H_%M").pdf
	cxr_common_parallel_draw_dependency_graph "$dep_file" "$pdf_file"
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is "$(cxr_common_file_non_empty? "$pdf_file")" true "cxr_common_parallel_draw_dependency_graph simple existence check. Look at $pdf_file "

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
	