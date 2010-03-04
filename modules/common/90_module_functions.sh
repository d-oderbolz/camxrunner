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
CXR_META_MODULE_NUM_TESTS=2

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
# Function: cxr_common_module_get_meta_field
# 
# For a given module name, returns the given meta-data item.
#
# Parameters:
# $1 - name of a module
# $2 - the name of the item
################################################################################
function cxr_common_module_get_meta_field()
################################################################################
{
	local module="$1"
	local item="$2"
	local module_path
	local value

	
	if [[ "$(cxr_common_hash_has? $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module)" == true ]]
	then
		module_path="$(cxr_common_hash_get $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module)"
	else
		cxr_main_die_gracefully "$FUNCNAME - cannot find path of $module"
	fi
	
	# source module
	source "$module_path"
	
	if [[ $? -ne 0 ]]
	then
		cxr_main_die_gracefully "$FUNCNAME - could not source $module ($module_path)"
	fi
	
	# Do we have this variable?
	set | grep $item 2>&1 > /dev/null
	
	if [[ $? -ne 0 ]]
	then
		# variable not known!
		cxr_main_die_gracefully "$FUNCNAME - variable $item not found!"
	else
		# Return value (indirect)
		echo ${!item}
	fi
}

################################################################################
# Function: cxr_common_module_get_raw_dependencies
# 
# For a given module name, returns the raw dependency string that is
# in the header (CXR_META_MODULE_DEPENDS_ON)
#
# Parameters:
# $1 - name of a module
################################################################################
function cxr_common_module_get_raw_dependencies()
################################################################################
{
	local module="$1"
	local raw_dependencies
	
	raw_dependencies=$(cxr_common_module_get_meta_field "$module" "CXR_META_MODULE_DEPENDS_ON")

	echo "${raw_dependencies}"
}

################################################################################
# Function: cxr_common_module_get_exlusive
# 
# For a given module name, returns the exclusive string that is
# in the header (CXR_META_MODULE_RUN_EXCLUSIVELY)
#
# Parameters:
# $1 - name of a module
################################################################################
function cxr_common_module_get_exlusive()
################################################################################
{
	local module="$1"
	local exclusive
	
	exclusive=$(cxr_common_module_get_meta_field "$module" "CXR_META_MODULE_RUN_EXCLUSIVELY")

	echo "${exclusive}"
}

################################################################################
# Function: cxr_common_module_resolve_single_dependency
# 
# resolves a single dependenency string (containig just one dependency), depending 
# on the day offset.
# Any dependency that is not relevant (like module- for the first day) is not returned.
# If a dependency is disabled, we either terminate or ignore this (depends on CXR_IGNORE_DISABLED_DEPENDENCIES)
# 
# Hashes:
# CXR_ACTIVE_ALL_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active modules (dummy value)
# CXR_ACTIVE_ONCE_PRE_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active One-Time preprocessing modules (dummy value)
# CXR_ACTIVE_DAILY_PRE_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active daily preprocessing modules (dummy value)
# CXR_ACTIVE_MODEL_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active model modules (dummy value)
# CXR_ACTIVE_DAILY_POST_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active daily postprocessing modules (dummy value)
# CXR_ACTIVE_ONCE_POST_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active One-Time postprocessing modules (dummy value)

# Parameters:
# $1 - name of the dependency like "create_emissions" or a special dependency like "all_model")
# [$2] - day offset (if not given, we are resolving for a One-Time module)
################################################################################
function cxr_common_module_resolve_single_dependency()
################################################################################
{
	if [[ $# -lt 1 && $# -gt 2 ]]
	then
		cxr_main_die_gracefully "Programming error @ ${FUNCNAME}:${LINENO} - needs at least a depdendency and an optional day_offset as input"
	fi

	local dependency="$1"
	local day_offset="${2:-}"
	local predicate
	local my_prefix
	local active_hash
	local module_type
	local raw_date
	local resolved_dependencies
	local resolved_dependency
	local active_modules
	local module
	local resolved_list
	local first=true
	
	if [[ "$day_offset" ]]
	then
		cxr_main_logger -v "${FUNCNAME}" "Resolving dependency $dependency for day offset $day_offset"
	else
		cxr_main_logger -v "${FUNCNAME}" "Resolving dependency $dependency (no day offset)"
	fi
	
	# Is there a predicate?
	# The last character of a string
	# We get 1 character at the position $(( ${#dependency} -1)) (length of string minus 1)
	predicate=${dependency:$(( ${#dependency} -1)):1}
	
	case $predicate in
		-) 
			# Currently, we only support -
			# if there is no day offset, this dependency is not relevant
			if [[ "$day_offset" ]]
			then
				cxr_main_logger -v "${FUNCNAME}" "Found the - predicate, will check last days results"
				day_offset=$(( $day_offset - 1 ))
				
				# Cut off final -
				dependency=${dependency%-}
				if [[ ! $day_offset -le 0 ]]
				then
					# At day 0, all previous day predicates are fulfilled
					cxr_main_logger -v "${FUNCNAME}" "We are at day 0 - previous day dependency not relevant"
					echo ""
					return $CXR_RET_OK
				fi
			else
				echo ""
				return $CXR_RET_OK
			fi # day_offset present?
			;;
	esac
	
	# Check if we look at a special dependency or not
	case $dependency in
	
		$CXR_DEP_ALL_ONCE_PRE) active_hash=$CXR_ACTIVE_ONCE_PRE_HASH;; 
			
		$CXR_DEP_ALL_DAILY_PRE) active_hash=$CXR_ACTIVE_DAILY_PRE_HASH;;
			
		$CXR_DEP_ALL_DAILY_POST) active_hash=$CXR_ACTIVE_DAILY_POST_HASH;;
			
		$CXR_DEP_ALL_ONCE_POST) active_hash=$CXR_ACTIVE_ONCE_POST_HASH;;
			
		$CXR_DEP_ALL_MODEL) active_hash=$CXR_ACTIVE_MODEL_HASH;;
			
		*) # A boring standard case
		
			# Is the dependency disabled?
			if [[ $(cxr_common_hash_has? $CXR_ACTIVE_ALL_HASH $CXR_HASH_TYPE_UNIVERSAL "$dependency") == false  ]]
			then
				# Do we care?
				if [[ "$CXR_IGNORE_DISABLED_DEPENDENCIES" == true  ]]
				then
					# No, user wants to ignore this
					cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_DISABLED_DEPENDENCIES to true and $dependency is disabled. We will not check if this module was run"
					echo ""
					
					return $CXR_RET_OK
				else
					# Yes, we terminate
					cxr_main_die_gracefully "${FUNCNAME} - You set CXR_IGNORE_DISABLED_DEPENDENCIES to false and $dependency is disabled. The dependency $dependency is not fulfilled!"
				fi
			fi
			
			# We return something like create_emissions0
			echo ${dependency}${day_offset}
			
			return $CXR_RET_OK
			;;
	
	esac
	
	########################################
	# Here, we handle the special cases
	########################################	
	cxr_main_logger -v "${FUNCNAME}" "We resolve the special dependency ${dependency}..."
	
	# Create list of active modules
	active_modules="$(cxr_common_hash_keys $active_hash $CXR_HASH_TYPE_GLOBAL)"
	
	for module in $active_modules
	do
		cxr_main_logger -v "${FUNCNAME}" "Found dependency on $module"
		
		resolved_dependency=${module}${day_offset}
		
		if [[ "$first" == true ]]
		then
			# First iteration
			resolved_list="$resolved_dependency"
			first=false
		else
			# any other iteration
			resolved_list="$resolved_list $resolved_dependency"
		fi
	done  # Loop over active modules
	
	echo "$resolved_list"
	return $CXR_RET_OK
}

################################################################################
# Function: cxr_common_module_resolve_all_dependencies
# 
# resolves a complete dependenency string (containig more than one dependency), depending 
# on the day offset.
# 
# Parameters:
# $1 - a list of dependencies "create_emissions all_module-"
# [$2] - day offset (if not given, we are resolving for a One-Time module)
################################################################################
function cxr_common_module_resolve_all_dependencies()
################################################################################
{
	local dependencies="$1"
	local day_offset="$2"
	local dependency
	local resolved_dependency
	local resolved_list
	local first=true
	
	#Loop through dependencies
	for dependency in $dependencies
	do
		resolved_dependency="$(cxr_common_module_resolve_single_dependency "$dependency" "$day_offset")"
		if [[ "$first" == true ]]
		then
			# First iteration
			resolved_list="$resolved_dependency"
			first=false
		else
			# any other iteration
			resolved_list="$resolved_list $resolved_dependency"
		fi
	done
	
	echo "$resolved_list"
}

################################################################################
# Function: cxr_common_module_dependencies_ok?
#
# Checks if all given raw dependencies are fullfilled. If any dependency has failed,
# the run is destroyed, if the depdendency was not yet started, false is returned,
# if it is OK, true is returned.
#
# Can handle dependencies on a whole type (like all_model) and the predicate - (previous day)
#
# Checks if a dependency is listed in the list of active modules.
#
# If this is a dryrun, we go on even if a dependency failed.
# 
# We access the state DB using <cxr_common_has_finished> to test if stuff has finished.
#
# Parameters:
# $1 - a list of raw dependencies
# $2 - a day offset used as reference
################################################################################
function cxr_common_module_dependencies_ok?()
################################################################################
{
	if [[ "$CXR_IGNORE_ANY_DEPENDENCIES" == true  ]]
	then
		cxr_main_logger "${FUNCNAME}" "You set CXR_IGNORE_ANY_DEPENDENCIES to true. We will not check dependencies (pretty dangerous...)"
		echo true
		return $CXR_RET_OK
	fi
	
	if [[ $# -ne 2 ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs a depdendency list and a day offset as input"
	fi

	local raw_dependencies="$1"
	local day_offset="$2"
	local dep_day_offset
	local dep_module_name
	local dependencies="$(cxr_common_module_resolve_all_dependencies "$raw_dependencies" "$day_offset" )"
	local dependency
	local my_stage
	
	cxr_main_logger -v "${FUNCNAME}" "Evaluating dependencies on $dependencies for day offset $day_offset"

	for dependency in $dependencies
	do
		# We need to parse the dependency
		# Remove potential digits
		dep_module_name=$(expr match "$dependency" '\(\<[_a-z]\{1,\}\)')
		# get only the digits at the end
		dep_day_offset=$(expr match "$dependency" '.*\([0-9]\{1,\}\>\)')
		
		# Determine type
		module_type="$(cxr_common_module_get_type "$dep_module_name")"
		
		# Convert date
		raw_date="$(cxr_common_offset2_raw_date "${dep_day_offset}")"
		
		my_stage="$(cxr_common_get_stage_name "$module_type" "$dependency" "$raw_date" )"
		
		# Is this known to have worked?
		if [[ "$(cxr_common_has_finished "$my_stage")" == true ]]
		then
			cxr_main_logger -v "${FUNCNAME}"  "dependency ${dependency} fullfilled"
		else
			# dependency NOK, Find out why
			
			# Find out if dependency failed - if so, we crash
			if [[ "$(cxr_common_has_failed "$my_stage")" == true  ]]
			then
				# It failed
				# Destroy run 
				if [[ $CXR_DRY == false  ]]
				then
					cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - dependency ${day_offset}_${dependency} failed!"
				else
					cxr_main_logger -v "${FUNCNAME}" "The dependency ${dependency} failed - but this is a dryrun, so we keep going!"
				fi
			else
				# It did not fail, it seems that it was not yet run - we have to wait
				cxr_main_logger -v "${FUNCNAME}" "${dependency} has not yet finished - we need to wait."
				echo false
				return $CXR_RET_OK
			fi
		fi
	done # Loop over all dependencies
	
	# If we arrive here, all is swell
	echo true
	
	return $CXR_RET_OK
}

################################################################################
# Function: cxr_common_module_update_info
#
# Goes through all available modules for the current model and version, and collects 
# vital information in various hashes.
#
# Hashes:
# CXR_MODULE_PATH_HASH ($CXR_HASH_TYPE_UNIVERSAL) - maps module names to their path
# CXR_MODULE_TYPE_HASH ($CXR_HASH_TYPE_UNIVERSAL) - maps module names to their type
#
# CXR_ACTIVE_ALL_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active modules (dummy value)
# CXR_ACTIVE_ONCE_PRE_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active One-Time preprocessing modules (dummy value)
# CXR_ACTIVE_DAILY_PRE_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active daily preprocessing modules (dummy value)
# CXR_ACTIVE_MODEL_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active model modules (dummy value)
# CXR_ACTIVE_DAILY_POST_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active daily postprocessing modules (dummy value)
# CXR_ACTIVE_ONCE_POST_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active One-Time postprocessing modules (dummy value)
################################################################################
function cxr_common_module_update_info()
################################################################################
{
	cxr_main_logger -v "$FUNCNAME" "Updating module information..."
	
	# Increase global indent level
	cxr_main_increase_log_indent
	
	local i
	local dirs
	local dir
	local types
	local type
	local hashes
	local active_hash
	local files
	local file
	local module_name
	
	# Create a few working arrays we will go through
	types=($CXR_TYPE_PREPROCESS_ONCE $CXR_TYPE_PREPROCESS_DAILY  $CXR_TYPE_MODEL $CXR_TYPE_POSTPROCESS_DAILY $CXR_TYPE_POSTPROCESS_ONCE)
	dirs=($CXR_PREPROCESSOR_ONCE_INPUT_DIR $CXR_PREPROCESSOR_DAILY_INPUT_DIR $CXR_MODEL_INPUT_DIR $CXR_POSTPROCESSOR_DAILY_INPUT_DIR $CXR_POSTPROCESSOR_ONCE_INPUT_DIR)
	hashes=($CXR_ACTIVE_ONCE_PRE_HASH $CXR_ACTIVE_DAILY_PRE_HASH $CXR_ACTIVE_MODEL_HASH $CXR_ACTIVE_DAILY_POST_HASH $CXR_ACTIVE_ONCE_POST_HASH)
	
	for i in $(seq 0 $(( ${#dirs[@]} - 1 )) )
	do
		type=${types[$i]}
		dir=${dirs[$i]}
		active_hash=${hashes[$i]}
		
		cxr_main_logger -v "$FUNCNAME" "Adding $type modules..."
		
		# Find all of them
		files="$(find $dir -noleaf -maxdepth 1 -name $name)"
		
		echo $files
		
		for file in $files
		do
			module_name="$(cxr_main_extract_module_name $file)"
			cxr_main_logger -v "$FUNCNAME" "Adding module $module_name in $file"
			
			# Path 
			cxr_common_hash_put $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module_name $file
			
			# Type
			cxr_common_hash_put $CXR_MODULE_TYPE_HASH $CXR_HASH_TYPE_UNIVERSAL $module_name $type
			
			# All Hash (value is dummy)
			cxr_common_hash_put $CXR_ACTIVE_ALL_HASH $CXR_HASH_TYPE_GLOBAL $module_name true
		
			# The current types active hash
			cxr_common_hash_put $active_hash $CXR_HASH_TYPE_GLOBAL $module_name true
		
		done # Loop over files
	done # loop over type-index
	
	# decrease global indent level
	cxr_main_decrease_log_indent
}

################################################################################
# Function: cxr_common_module_get_type
#
# Gets its information directly from the CXR_MODULE_TYPE_HASH
# 
# Parameters:
# $1 - name of module (without prefix or suffix, just something like "convert output"
################################################################################
function cxr_common_module_get_type()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		cxr_main_logger -e "$FUNCNAME" "Need a module name as input"
	fi
	
	local name="${1}"
	local module_type

	if [[ "$(cxr_common_hash_has? "$CXR_MODULE_TYPE_HASH" $CXR_HASH_TYPE_UNIVERSAL "$name" )" == true ]]
	then
		module_type="$(cxr_common_hash_get "$CXR_MODULE_TYPE_HASH" $CXR_HASH_TYPE_UNIVERSAL "$name" )"
		
		if [[ "$module_type" ]]
		then
			echo "$module_type"
		else
			cxr_main_die_gracefully "$FUNCNAME: Could not find module type of $name!"
		fi
	else
		cxr_main_die_gracefully "$FUNCNAME: Could not find module $name!"
	fi
	
}

################################################################################
# Function: cxr_common_module_run_type
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
function cxr_common_module_run_type()
################################################################################
{
	local module_type="$1"
	
	# Either contains a number (the only step to run)
	# or the string "all"
	local run_only="${2:-${CXR_RUN_ALL}}"
	
	#Return value - set optimisticaly
	local ret_val=$CXR_RET_OK
	
	# Normally, we check continue, except installers
	local check_continue=true
	
	# Contains the date for which we currently run if needed
	# We set it to the empty string if date is not relevant (One-Time modules)
	local our_date
	local module_directories
	local enabled_modules
	local disabled_modules
	local run_it
	
	# Variables:
	# module_direcotries - is a list of directories that will be used to search for modules
	# enabled_modules - is a list of explicitly enables modules of the current type
	# disabled_modules - is a list of disabled modules of the current type
	case "$module_type" in
	
		"${CXR_TYPE_COMMON}" ) 
			cxr_main_die_gracefully "Common modules cannot be run this way!" ;;
			
		"${CXR_TYPE_PREPROCESS_ONCE}" ) 
			module_directories="$CXR_PREPROCESSOR_ONCE_INPUT_DIR"
			enabled_modules="$CXR_ENABLED_ONCE_PREPROC"
			disabled_modules="$CXR_DISABLED_ONCE_PREPROC"
			our_date=;;
			
		"${CXR_TYPE_PREPROCESS_DAILY}" ) 
			module_directories="$CXR_PREPROCESSOR_DAILY_INPUT_DIR"
			enabled_modules="$CXR_ENABLED_DAILY_PREPROC"
			disabled_modules="$CXR_DISABLED_DAILY_PREPROC"
			our_date=${CXR_DATE:-};;
			
		"${CXR_TYPE_POSTPROCESS_DAILY}" ) 
			module_directories="$CXR_POSTPROCESSOR_DAILY_INPUT_DIR"
			enabled_modules="$CXR_ENABLED_DAILY_POSTPROC"
			disabled_modules="$CXR_DISABLED_DAILY_POSTPROC"
			our_date=${CXR_DATE:-};;
			
		"${CXR_TYPE_POSTPROCESS_ONCE}" ) 
			module_directories="$CXR_POSTPROCESSOR_ONCE_INPUT_DIR"
			enabled_modules="$CXR_ENABLED_ONCE_POSTPROC"
			disabled_modules="$CXR_DISABLED_ONCE_POSTPROC"
			our_date=;;
			
		"${CXR_TYPE_MODEL}" ) 
			module_directories="$CXR_MODEL_INPUT_DIR"
			enabled_modules="$CXR_ENABLED_MODEL"
			disabled_modules="$CXR_DISABLED_MODEL"
			our_date=${CXR_DATE:-};;
			
		"${CXR_TYPE_INSTALLER}" ) 
			module_directories="$CXR_INSTALLER_INPUT_DIR $CXR_INSTALLER_MODEL_INPUT_DIR $CXR_INSTALLER_VERSION_INPUT_DIR" 
			enabled_modules="$CXR_ENABLED_INSTALLER"
			disabled_modules="$CXR_DISABLED_INSTALLER"
			check_continue=false
			our_date=;;
			
		* ) 
			cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Unknown module type $module_type" ;;

	esac
	
	# Increase global indent level
	cxr_main_increase_log_indent
	
	# Check if we need any of them at all
	# If the user wants to run a specific module, we enter anyway
	if [[ ! ( "${enabled_modules}" == "" && "${disabled_modules}" == "${CXR_SKIP_ALL}" && "$run_only" == "${CXR_RUN_ALL}" ) ]]
	then
	
		# We did not turn off everything or we need only a specific module to be run
	
		# Loop through available input dirs
		for module_directory in $module_directories
		do
			cxr_main_logger "${FUNCNAME}" "Loading $module_type modules from $module_directory..."
		
			for function_file in $(ls ${module_directory}/??_*.sh 2>/dev/null)
			do
				# Check if we are still happy if needed
				if [[ "${check_continue}" == true  ]]
				then
					cxr_common_do_we_continue || cxr_main_die_gracefully "Continue file no longer present."
				fi
				
				FILE_NAME=$(basename "$function_file")
				
				# Before loading a new module, remove old meta variables
				unset ${!CXR_META_MODULE*}
				
				# Export the module name
				CXR_META_MODULE_NAME=$(cxr_main_extract_module_name $function_file)
				
				if [[ "$run_only" != "${CXR_RUN_ALL}"  ]]
				then
					# is this the module we should run?
					# here we do no further checks on disabled/enabled
					if [[ "$run_only" == "${CXR_META_MODULE_NAME}"  ]]
					then
						# First source the file to get the CXR_META_MODULE_NAME
						source $function_file
						
						# This is not needed for installers
						
						if [[ "$module_type" != "$CXR_TYPE_INSTALLER"  ]]
						then
							cxr_main_logger -a -b "${FUNCNAME}"  "Running $FILE_NAME ${our_date:-}"
						fi
						
						# Show dependencies, if any
						if [[ "${CXR_META_MODULE_DEPENDS_ON:-}"  ]]
						then
							cxr_main_logger -a -B "${FUNCNAME}" "This module depends on these modules:\n${CXR_META_MODULE_DEPENDS_ON}\nif it fails, run these dependencies first"
						fi

						# Increase global indent level
						cxr_main_increase_log_indent
						
						if [[ "$(cxr_common_check_module_requirements)" == true  ]]
						then
							cxr_main_logger -v "${FUNCNAME}"  "Starting Module $CXR_META_MODULE_NAME"
							"$CXR_META_MODULE_NAME" || ret_val=$CXR_RET_ERROR
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
					source $function_file
					
					# Check if we must run this
					# if the module name is in the enabled list, run it,no matter what
					if [[ "$(cxr_common_is_substring_present "$enabled_modules" "$CXR_META_MODULE_NAME")" == true  ]]
					then
						# Module was explicitly enabled
						run_it=true
					elif [[  "$(cxr_common_is_substring_present "$disabled_modules" "$CXR_META_MODULE_NAME")" == false && "${disabled_modules}" != "${CXR_SKIP_ALL}"   ]]
					then
						# Module was not explicitly disabled and we did not disable all
						run_it=true
					else
						# If the name of the module is in the disabled list, this should not be run (except if it is in the enabled list)
						run_it=false
						cxr_main_logger "${FUNCNAME}" "Step $FILE_NAME is disabled, skipped"
					fi
					
					# Execute if needed
					if [[ "$run_it" == true  ]]
					then
					
						if [[ "$module_type" != "$CXR_TYPE_INSTALLER"  ]]
						then
							cxr_main_logger -a -b "${FUNCNAME}"  "Running $FILE_NAME ${our_date:-}"
						fi
						
						# Increase global indent level
						cxr_main_increase_log_indent
						
						if [[ "$(cxr_common_check_module_requirements)" == true  ]]
						then
							cxr_main_logger -v "${FUNCNAME}"  "Starting Module $CXR_META_MODULE_NAME"
							"$CXR_META_MODULE_NAME" || ret_val=$CXR_RET_ERROR
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
		cxr_main_logger "${FUNCNAME}" "You disabled all modules of type $module_type by setting  CXR_DISABLED_... to ${CXR_SKIP_ALL}, none executed."
	fi 
	
	# Decrease global indent level
	cxr_main_decrease_log_indent
	
	return ${ret_val}
}

################################################################################
# Function: cxr_common_module_process_sequentially
#	
# Executes all (needed) modules in sequential order (non-parallel version of the task_functions)
# If only one process is used, this is faster because there is less overhead.
# For each part of the processing, we check if we need to run it and pass any limitations
# on to <cxr_common_module_run_type>.
#
################################################################################
function cxr_common_module_process_sequentially
################################################################################
{
	# Set up return value
	local ret_val=$CXR_RET_OK
	local day_offset
	

	# Setup environment
	cxr_common_set_date_variables "$CXR_START_DATE" 0
	
	if [[ ${CXR_RUN_PRE_ONCE} == true  ]]
	then
		cxr_common_module_run_type ${CXR_TYPE_PREPROCESS_ONCE} ${CXR_RUN_PRE_ONCE_STEP:-${CXR_RUN_ALL}} || ret_val=$CXR_RET_ERROR
	else
		cxr_main_logger -w $FUNCNAME "We do not run ${CXR_TYPE_PREPROCESS_ONCE} modules."
	fi

	## Now we need to loop through the days
	# but only if the user wants any of this
	
	if [[ ${CXR_RUN_PRE_DAILY} == true || ${CXR_RUN_MODEL} == true || ${CXR_RUN_POST_DAILY} == true ]]
	then
		for day_offset in $(seq 0 $((${CXR_NUMBER_OF_SIM_DAYS} -1 )) )
		do
		
			# if we run only 1 day, do it
			if [[ "${CXR_ONE_DAY}"  ]]
			then
				day_offset="$(cxr_common_date2offset ${CXR_ONE_DAY})"
				cxr_main_logger "$FUNCNAME" "${CXR_ONE_DAY} corresponds to offset ${day_offset}."
			fi
		
			# Setup environment
			cxr_common_set_date_variables "$CXR_START_DATE" "$day_offset"
			
			cxr_main_logger -B $FUNCNAME "Processing ${CXR_DATE:-now}"
			
			# Run the three daily module types in order
			
			if [[ ${CXR_RUN_PRE_DAILY} == true  ]]
			then
				cxr_common_module_run_type ${CXR_TYPE_PREPROCESS_DAILY} ${CXR_RUN_PRE_DAILY_STEP:-${CXR_RUN_ALL}} || ret_val=$CXR_RET_ERROR
			else
				cxr_main_logger -w $FUNCNAME "We do not run ${CXR_TYPE_PREPROCESS_DAILY} modules."
			fi
			
			if [[ ${CXR_RUN_MODEL} == true  ]]
			then
				cxr_common_module_run_type ${CXR_TYPE_MODEL} ${CXR_RUN_MODEL_SINGLE_STEP:-${CXR_RUN_ALL}} || ret_val=$CXR_RET_ERROR
			else
				cxr_main_logger -w $FUNCNAME "We do not run ${CXR_TYPE_MODEL} modules."
			fi
			
			if [[ ${CXR_RUN_POST_DAILY} == true  ]]
			then
				cxr_common_module_run_type ${CXR_TYPE_POSTPROCESS_DAILY} ${CXR_RUN_POST_DAILY_STEP:-${CXR_RUN_ALL}} || ret_val=$CXR_RET_ERROR
			else
				cxr_main_logger -w $FUNCNAME "We do not run ${CXR_TYPE_POSTPROCESS_DAILY} modules."
			fi
			
			# If we do only 1 day, that's it
			if [[ "${CXR_ONE_DAY}"  ]]
			then
				break
			fi
			
		done # Loop through days
	else
		cxr_main_logger -w $FUNCNAME "We do not run any daily modules"
	fi
	
	if [[ ${CXR_RUN_POST_ONCE} == true  ]]
	then
		cxr_common_module_run_type ${CXR_TYPE_POSTPROCESS_ONCE} ${CXR_RUN_POST_ONCE_STEP:-${CXR_RUN_ALL}} || ret_val=$CXR_RET_ERROR
	else
		cxr_main_logger -w $FUNCNAME "We do not run ${CXR_TYPE_POSTPROCESS_ONCE} modules."
	fi
	
	return $ret_val
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
	
	is $(cxr_common_module_get_type boundary_conditions) ${CXR_TYPE_PREPROCESS_DAILY} "cxr_common_module_get_type boundary_conditions"
	is $(cxr_common_module_get_exlusive model) true "cxr_common_module_get_exlusive model"



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