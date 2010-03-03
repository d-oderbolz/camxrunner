#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Functions to check and change the state af a run
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
CXR_META_MODULE_NUM_TESTS=1

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the functions to manage the (simple) state Database of the CAMxRunner"

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
# Function: cxr_common_is_repeated_run
#
# Returns true, if this run was already started earlier (basically a check if the 
# state directory is empty)
# 
################################################################################
function cxr_common_is_repeated_run()
################################################################################
{
	local count
	
	count=$(find ${CXR_STATE_DIR} -maxdepth 1 -noleaf -type f 2>/dev/null | wc -l)
	
	cxr_main_logger -v "$FUNCNAME" "File count in state directory: $count"
	
	if [[ "$count" -gt 0  ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: cxr_common_get_last_day_modelled
#
# If the run was already done, returns the last day done in ISO Format. If it was not done, 
# returns the empty string.
# 
################################################################################
function cxr_common_get_last_day_modelled()
################################################################################
{
	local max_day
	local date
	
	if [[ $(cxr_common_is_repeated_run) == true  ]]
	then
		# A similar expression is used in cxr_common_cleanup_state
		max_day=$(find ${CXR_STATE_DIR} -noleaf -type f 2>/dev/null | xargs -i basename \{\} |  grep -o '^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]' - | sort | uniq | tail -n1)
		
		if [[ "$max_day"  ]]
		then
			# Convert to ISO
			date=$(cxr_common_to_iso_date "$max_day")
		
			echo "$date"
		else
			echo ""
		fi
	else
		echo ""
	fi
	
	return $CXR_RET_OK
}

################################################################################
# Function: cxr_common_get_stage_name
#
# Generates the name of the current stage from the parameters or the environment (if no parameters are given) 
# Takes the CXR_META_MODULE_  and the date variables into account.
# Names are dependent on the type of module.
# Names have the form
# ${CXR_DATE_RAW}@${CXR_META_MODULE_TYPE}@${CXR_META_MODULE_NAME}
# We choose names that sort more or less nicely.
#
# Note that the first 3 parameters are semi-optional: if one of them is given, all of them must be present!
# 
# Parameters:
# [$1] - module type
# [$2] - module name
# [$3] - raw date
################################################################################
function cxr_common_get_stage_name()
################################################################################
{
	if [[ $# -lt 3  ]]
	then
		# Use the environment
		local module_type="${CXR_META_MODULE_TYPE}"
		local module_name="${CXR_META_MODULE_NAME}"
		local date=${CXR_DATE_RAW:-date_not_set}
	else
		# Use parameters
		local module_type="${1}"
		local module_name="${2}"
		local date="${3}"
	fi

	case "${module_type}" in
		${CXR_TYPE_COMMON}) 
			# A common module normally does not need this, but who knows?
			echo ${date}@${module_type}@${module_name} ;; 
		
		${CXR_TYPE_PREPROCESS_DAILY}) 
			echo ${date}@${module_type}@${module_name} ;;
			
		${CXR_TYPE_PREPROCESS_ONCE}) 
			echo _@${module_type}@${module_name} ;;
			
		${CXR_TYPE_POSTPROCESS_DAILY}) 
			echo ${date}@${module_type}@${module_name} ;;
		
		${CXR_TYPE_POSTPROCESS_ONCE}) 
			echo ZZ_@${module_type}@${module_name} ;;
			
		${CXR_TYPE_MODEL} ) 
			echo ${date}@${module_type}@${module_name} ;;
			
		${CXR_TYPE_INSTALLER}) 
			echo ${module_type}@${module_name} ;;
			
	 *) cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Unknown module type ${module_type}";;
	esac
}

################################################################################
# Function: cxr_common_delete_continue_files
#
# Deletes the continue files of all instances
################################################################################
function cxr_common_delete_continue_files()
################################################################################
{
	cxr_main_logger -w "${FUNCNAME}" "The continue files of all instances of this run will be deleted now!"
	
	find ${CXR_ALL_INSTANCES_DIR} -noleaf -name ${CXR_CONTINUE} -exec rm -f {} \;
}

################################################################################
# Function: cxr_common_delete_instance_data
#
# Deletes this instances runtime-data including CONTINUE File. Used at the end of a run.
################################################################################
function cxr_common_delete_instance_data()
################################################################################
{
	cxr_main_logger -w "${FUNCNAME}" "The instance data in  ${CXR_INSTANCE_DIR} will be deleted now!"
	rm -rf ${CXR_INSTANCE_DIR} >/dev/null 2>&1
}

################################################################################
# Function: cxr_common_initialize_state_db
#
# Creates the state DB directories for the current run
################################################################################
function cxr_common_initialize_state_db()
################################################################################
{
	local var
	local file
	
	if [[ -z "${CXR_STATE_DIR}"  ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "CXR_STATE_DIR not set!"
		return 1
	fi
	
	# Not started yet - create state dir first
	if [[ ! -d "${CXR_STATE_DIR}"  ]]
	then
		mkdir -p "${CXR_STATE_DIR}"
		
	fi
	
	# Create the global dirs
	mkdir -p "${CXR_GLOBAL_DIR}"
	mkdir -p "${CXR_UNIVERSAL_HASH_DIR}"
	
	# Create any instance dirs
	mkdir -p "${CXR_INSTANCE_DIR}"
	mkdir -p "${CXR_INSTANCE_HASH_DIR}"
	
	# Create all the task directories
	mkdir -p "${CXR_TASK_POOL_DIR}"
	mkdir -p "${CXR_TASK_TODO_DIR}"
	mkdir -p "${CXR_TASK_RUNNING_DIR}"
	mkdir -p "${CXR_TASK_SUCCESSFUL_DIR}"
	mkdir -p "${CXR_TASK_FAILED_DIR}"

	mkdir -p "${CXR_WORKER_DIR}"
	mkdir -p "${CXR_RUNNING_WORKER_DIR}"
	mkdir -p "${CXR_WAITING_WORKER_DIR}"
	
	# Init a few Hashes
	cxr_common_hash_init MD5 universal
	
	
	# Creating .continue file
	cxr_main_logger -n -i "${FUNCNAME}" "Creating the file ${CXR_CONTINUE_FILE}. If this file is deleted, the process  stops at the next possible stage"
	echo "If you remove this file, the process  ($0) on $(uname -n) will stop" > ${CXR_CONTINUE_FILE}
	
	# Create the instance files and secure them
	for var in $(set | sort | grep ^CXR_INSTANCE_FILE_.*= )
	do
		# Now Var contains a VAR=Value string
		# Extract the value
		file="$(echo "${var}" | cut -d= -f2)"
	
		# Empty file
		:> "$file"
		
		# Secure file
		chmod 600 "$file"
	done
	
	# This file should not be deleted
	touch "$CXR_INSTANCE_FILE_OUTPUT_LIST"
	chmod 600 "$CXR_INSTANCE_FILE_OUTPUT_LIST"
}

################################################################################
# Function: cxr_common_store_state
#
# Stores the current state of a run, can be either $CXR_STATE_START, $CXR_STATE_STOP or $CXR_STATE_ERROR.
# In the case of ERROR, we will take note of the fact.
#
# If a stage starts, checks if this stage was already executed.
# The name of the stage is determined via <cxr_common_get_stage_name> if it is not passed in.
# The ability to pass a stage name is used by installers and tests alike, they use a slightly different approach.
#
# If the user wants to run a specific module (CXR_RUN_LIMITED_PROCESSING=true), we will disregard the fact that
# the step already ran, but we advise -F
#
# Returns:
# 	$CXR_RET_OK if OK (true)
# 	$CXR_RET_ALREADY_RUN if this stage was already started (false)
#
# Parameters:
# $1 - State (START, STOP, ERROR)
# [$2] - an optional name of a state, currently used by installation. 
#        Installation prefix the names with the model name and version.
################################################################################
function cxr_common_store_state()
################################################################################
{
	
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a state like $CXR_STATE_ERROR as Input"   
	fi
	
	local state=$1
	# stage is either passed or set using cxr_common_get_stage_name
	local stage="${2:-$(cxr_common_get_stage_name)}"
	
	# Do we care at all?
	# Set CXR_ENABLE_STATE_DB to false in tests etc.
	if [[ "$CXR_ENABLE_STATE_DB" == false  ]]
	then
		cxr_main_logger -v "${FUNCNAME}"  "You disabled the state DB (CXR_ENABLE_STATE_DB=false), new state will not be stored."
		echo true
		return $CXR_RET_OK
	fi
	
	case "$state" in
	
		"$CXR_STATE_START") 
			# Check if this was already started
			if [[ $(cxr_common_has_finished "$stage") == true  ]]
			then
				
				if [[ "$CXR_RUN_LIMITED_PROCESSING" == true  ]]
				then
					# Ran already, but user wants to run specifically this
					cxr_main_logger -w "${FUNCNAME}" "${FUNCNAME}:${LINENO} - stage $stage was already started, but since you requested this specific module, we run it. If it fails try to run \n \t ${CXR_CALL} -F \n to remove existing output files."
					echo true
				else
					# Oops, this stage was already started	
					cxr_main_logger -w "${FUNCNAME}" "${FUNCNAME}:${LINENO} - stage $stage was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
					
					# false means already run
					echo false
					return $CXR_RET_ALREADY_RUN
				fi 
			else
				echo true
			fi
			;;
	
			"$CXR_STATE_STOP")
				cxr_main_logger -i "${FUNCNAME}" "stage $stage successfully completed."
				echo true
				;;
	
			"$CXR_STATE_ERROR")
				CXR_STATUS=$CXR_STATUS_FAILURE
				cxr_main_logger -e "An error has occured during the execution of $stage!"
				echo false
			;;
			
			*)
				CXR_STATUS=$CXR_STATUS_FAILURE
				cxr_main_die_gracefully "Unknown state $state given"
				echo false
			;;
	esac
	
	# Touch your state file
	touch "$(cxr_common_get_state_file_name "$state" "$stage")" 
	return $CXR_RET_OK
}

################################################################################
# Function: cxr_common_get_state_file_name
#
# Returns:
# The start/stop/error filename of a stage
#
# Parameters:
# $1 - state
# $2 - stage 
################################################################################
function cxr_common_get_state_file_name()
################################################################################
{
	if [[ $# -ne 2  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a state and a stage as Input" 
	fi
	
	local state=$1
	local stage=$2
	
	echo ${CXR_STATE_DIR}/${stage}.${state}	
}

################################################################################
# Function: cxr_common_detect_running_instances
#
# Check if there are still living processes by checking the continue files
#
################################################################################
function cxr_common_detect_running_instances()
################################################################################
{
	local process_count=$(ls ${CXR_ALL_INSTANCES_DIR}/*${CXR_STATE_CONTINUE} 2> /dev/null | wc -l ) 
	
	if [[  ${process_count} -ne 0 && ${CXR_ALLOW_MULTIPLE} == false   ]]
	then
		# There are other processes running and this is not allowed
		cxr_main_logger -e "${FUNCNAME}"  "Found other Continue files - maybe these processes died or they are still running:\n(Check their age!)"
		
		ls -la ${CXR_STATE_DIR}/*${CXR_STATE_CONTINUE} | tee -a ${CXR_LOG}
		
		cxr_main_logger -e "${FUNCNAME}"  "Check manually if the processes still run, if not clean the state db by runnig \n\t ${CXR_CALL} -c \n or (experts only) you can run your instance anyway using \n \t ${CXR_RUN} -m [options]"    
		
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Process stopped"
	fi
}

################################################################################
# Function: cxr_common_has_finished
#	
# Check if a specific stage has finished.
#
# Parameters:	
# $1 - stage to test
################################################################################
function cxr_common_has_finished()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a state and a stage as Input" 
		echo false
	fi
	
	local stage="$1"
	local start_file=$(cxr_common_get_state_file_name "${CXR_STATE_START}" "${stage}")
	local stop_file=$(cxr_common_get_state_file_name "${CXR_STATE_STOP}" "${stage}")
	
	if [[ -f "$stop_file"  ]]
	then
	
		if [[ -f "$start_file"  ]]
		then
			cxr_main_logger -v "${FUNCNAME}" "Found a START file for ${stage}"
		fi
		
		echo true
	else
	
		# There is no stop file. Still there might be a start file:
		if [[ -f "$start_file"  ]]
		then
			cxr_main_logger -w "${FUNCNAME}" "The stage ${stage} was started, but did not (yet) finish."
		fi
	
		echo false
	fi
}

################################################################################
# Function: cxr_common_has_failed
#	
# Check if a specific stage has failed. We do not consider if we have a start file,
# we check if we have an error file.
#
# Parameters:	
# $1 - stage to test
################################################################################
function cxr_common_has_failed()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a state and a stage as Input" 
		echo false
	fi
	
	local stage="$1"
	local error_file=$(cxr_common_get_state_file_name "${CXR_STATE_ERROR}" "${stage}")
	
	if [[ -f "$error_file"  ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: cxr_common_cleanup_state
#	
# Depending on user selection:
# - Deletes all state information
# - Deletes only part of the state information
# - Respects also the task structures
# All is in a endless loop so one can quickly delete a lot of stuff
#
# There is a lot of redundant code here, must be refactored if time permits
# 
################################################################################
function cxr_common_cleanup_state()
################################################################################
{
	local what
	local what_detail
	local days
	local which_day
	local module_types
	local num_module_types
	local steps
	local which_step
	
	while [ "$(cxr_common_get_consent "Do you want to (further) change the state database?" )" == true ]
	do
		# what do you want?
		what=$(cxr_common_get_menu_choice "Which part of the state database do you want to clean (none exits this function)?\nNote that you might need to delete output files in order to repeat a run, or run with ${CXR_CALL} -F (overwrite existing files)" "all existing-instances specific tasks none" "none")
		
		case "$what" in 
		
			all)
					cxr_main_logger -w "${FUNCNAME}" "The following files will be deleted:"
						
					find ${CXR_STATE_DIR} -noleaf -type f -maxdepth 1 | xargs -i basename \{\}
			
					# Do we do this?
					if [[ "$(cxr_common_get_consent "Do you really want to delete these files?" )" == false  ]]
					then
						# No 
						cxr_main_logger -i "${FUNCNAME}"  "Will not delete any state information"
						return 0
					else
						# Yes
						rm -rf ${CXR_STATE_DIR}/* 2>/dev/null
						cxr_main_logger -i "${FUNCNAME}"  "Done."
						# When all is cleared, we do not need to go further
						break
					fi
			;;
			
			existing-instances)
			
					cxr_main_logger -w "${FUNCNAME}" "The following directories and files therein will be deleted:"
						
					find ${CXR_ALL_INSTANCES_DIR} -noleaf -type d | xargs -i basename \{\}
			
					# Do we do this?
					if [[ "$(cxr_common_get_consent "Do you really want to delete these files?" )" == false  ]]
					then
						# No 
						cxr_main_logger -i "${FUNCNAME}"  "Will not delete any state information"
						return 0
					else
						# Yes
						rm -rf ${CXR_ALL_INSTANCES_DIR}/* 2>/dev/null
						cxr_main_logger -i "${FUNCNAME}"  "Done."
					fi
			
			;;
					
			specific) 
			
				# Possibilities:
				# one day 
				# one step
				# Both
				
				what_detail="$(cxr_common_get_menu_choice "You have 3 options: (1) we delete all state information about one specific day , (2) about one specific step or (3) a combination thereof (none exits this function)?" "day step both none" "none")"
				
				case "$what_detail" in
				
					day)
						# To enumerate all days, we use a little grep-magic, also we add none at the end
						# The basename is needed to strip off the path, because the pattern starts with ^
						days="$(find ${CXR_STATE_DIR} -noleaf -type f | xargs -i basename \{\} |  grep -o '^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]' - | sort | uniq ) none"
						
						which_day="$(cxr_common_get_menu_choice "Which days state information should be deleted  (none exits this function)?" "$days" "none" )"
						
						case "$which_day" in
						
							none) 
								cxr_main_logger -w "${FUNCNAME}"  "Will not delete any state information" 
								return 0
							;;
								
							*)
								cxr_main_logger -w "${FUNCNAME}" "The following files will be deleted:"
						
								ls ${CXR_STATE_DIR}/${which_day}@*@* | xargs -i basename \{\}
								
								if [[ "$(cxr_common_get_consent "Do you really want to delete these files?" )" == false  ]]
								then
									# No 
									cxr_main_logger -w "${FUNCNAME}"  "Will not delete any state information"
									return 0
								else
									#Yes
									rm -f ${CXR_STATE_DIR}/${which_day}@*@* 2>/dev/null
									cxr_main_logger -i "${FUNCNAME}"  "Done."
								fi
							;;
							
						esac
					;;
						
					step)
						# To enumerate all steps, we use a little grep-magic
						# We can tell between module types and steps
						# This code is not yet substage-safe
						module_types="$(find ${CXR_STATE_DIR} -noleaf -type f | grep -o '@.*@' - | sort | uniq) "
						
						# Count the module types
						num_module_types=$(echo "$module_types" | wc -l)
						
						steps="$(find ${CXR_STATE_DIR} -noleaf -type f | grep -o '@.*@.*\.' - | sort | uniq) none"
						
						which_step="$(cxr_common_get_menu_choice "Which module types (the first $num_module_types entries in the list) or steps state information should be deleted \n(none exits this function)?" "${module_types}${steps}" "none" )"
						
						case "$which_step" in
						
							none) 
								cxr_main_logger -w "${FUNCNAME}"  "Will not delete any state information" 
								return 0
							;;
								
							*)
							
								cxr_main_logger -w "${FUNCNAME}" "The following files will be deleted:"
								
								ls ${CXR_STATE_DIR}/*${which_step}* | xargs -i basename \{\}
								
								if [[ "$(cxr_common_get_consent "Do you really want to delete these files?" )" == false  ]]
								then
									# No 
									cxr_main_logger -w "${FUNCNAME}"  "Will not delete any state information"
									return 0
								else
									#Yes
									rm -f ${CXR_STATE_DIR}/*${which_step}* 2>/dev/null
									cxr_main_logger -i "${FUNCNAME}"  "Done."
								fi
							;;
						esac
					;;
						
					both)
						# First get the day, then the step
						
						# To enumerate all days, we use a little grep-magic, also we add none at the end
						# The basename is needed to strip off the path, because the pattern starts with ^
						days="$(find ${CXR_STATE_DIR} -noleaf -type f | xargs -i basename \{\} |  grep -o '^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]' - | sort | uniq ) none"
						
						which_day="$(cxr_common_get_menu_choice "Which days state information should be deleted  (none exits this function)?" "$days" "none" )"
					
						case "$which_day" in
				
							none) 
								cxr_main_logger -w "${FUNCNAME}"  "Will not delete any state information" 
								return 0
							;;
								
							*)
								# Now get the step
								# To enumerate all steps, we use a little grep-magic
								
								steps="$(find ${CXR_STATE_DIR} -noleaf -type f | grep -o ${which_day}'@.*@.*\.' - | sort | uniq) all none"
								
								which_step="$(cxr_common_get_menu_choice "Which steps state information should be deleted \n(none exits this function)?" "${steps}" "none" )"
								
								case "$which_step" in
								
									none) 
										cxr_main_logger -w "${FUNCNAME}"  "Will not delete any state information" 
										return 0
									;;
									
									all) 
										cxr_main_logger -w "${FUNCNAME}" "The following files will be deleted:"
										ls ${CXR_STATE_DIR}/${which_day}@*@* | xargs -i basename \{\}
										
										if [[ "$(cxr_common_get_consent "Do you really want to delete these files?" )" == false  ]]
										then
											# No 
											cxr_main_logger -w "${FUNCNAME}"  "Will not delete any state information"
											return 0
										else
											#Yes
											rm -f ${CXR_STATE_DIR}/${which_day}@*@* 2>/dev/null
											cxr_main_logger -i "${FUNCNAME}"  "Done."
										fi
									;;
									
									*)	cxr_main_logger -w "${FUNCNAME}" "The following files will be deleted:"
								
										ls ${CXR_STATE_DIR}/*${which_step}* | xargs -i basename \{\}
										
										if [[ "$(cxr_common_get_consent "Do you really want to delete these files?" )" == false  ]]
										then
											# No 
											cxr_main_logger -w "${FUNCNAME}"  "Will not delete any state information"
											return 0
										else
											#Yes
											rm -f ${CXR_STATE_DIR}/*${which_step}* 2>/dev/null
											cxr_main_logger -i "${FUNCNAME}"  "Done."
										fi
									;;
									
								esac
							;;
						esac
					;;
				
					none) 
						cxr_main_logger -w "${FUNCNAME}"  "Will not delete any state information" 
						return 0
					;;
				esac
				;;
				
			tasks)
					cxr_main_logger -w "${FUNCNAME}" "The following files will be deleted:"
						
					ls ${CXR_TASK_POOL_DIR}/* ${CXR_WORKER_DIR}/* {CXR_LOCK_DIR}/* | xargs -i basename \{\}
			
					# Do we do this?
					if [[ "$(cxr_common_get_consent "Do you really want to delete these files?" )" == false  ]]
					then
						# No 
						cxr_main_logger -w "${FUNCNAME}"  "Will not delete any state information"
						return 0
					else
	
						# Yes
						rm -rf ${CXR_TASK_POOL_DIR}/* 2>/dev/null
						rm -rf ${CXR_WORKER_DIR}/* 2>/dev/null
						rm -rf ${CXR_LOCK_DIR}/* 2>/dev/null
						cxr_main_logger -i "${FUNCNAME}"  "Done."
					fi
			;;
			
			none)
				cxr_main_logger -w "${FUNCNAME}"  "Will not delete any state information" 
				return 0
			;;
		
		esac
	
	done
}

################################################################################
# Function: cxr_common_do_we_continue
#	
# Checks if the .continue file still exists,
# if not, CXR_RET_CONTINUE_MISSING is returned. Also checks the error threshold
# ends run if we are too high and toches the alive file
################################################################################
function cxr_common_do_we_continue()
################################################################################
{
	local error_count=$(cxr_main_get_error_count)
	
	# Report error count
	cxr_main_logger -v -b "${FUNCNAME}"  "Current Error Count: $error_count"

	# Check error threshold, but only if the value of
	# of CXR_ERROR_THRESHOLD is not -1
	if [[  ( ${CXR_ERROR_THRESHOLD} != ${CXR_NO_ERROR_THRESHOLD} ) && ( ${error_count} -gt ${CXR_ERROR_THRESHOLD} )   ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - The number of errors occured (${error_count}) exceeds the threshold (${CXR_ERROR_THRESHOLD})"
	fi
	
	# Do we care at all?
	# Set this in tests etc.
	if [[ "$CXR_ENABLE_STATE_DB" == false  ]]
	then
		cxr_main_logger -v "${FUNCNAME}"  "You disabled the state DB, cannot determine presence of the continue file!"
		return $CXR_RET_OK
	fi
	
	# If the variable is not defined, nothing will happen
	if [[ "${CXR_CONTINUE_FILE}"  ]]
	then
		if [[ ! -f ${CXR_CONTINUE_FILE}  ]]
		then
			cxr_main_logger -w "${FUNCNAME}" "The Continue file no longer exists, exiting."
			return $CXR_RET_CONTINUE_MISSING
		else
			# We touch the continue file
			touch ${CXR_CONTINUE_FILE}
		fi
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
	
	cxr_main_logger -a "$FUNCNAME" "Initialising state DB in ${CXR_STATE_DIR}"
	cxr_common_initialize_state_db
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(cxr_common_is_repeated_run) false "cxr_common_is_repeated_run"

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



