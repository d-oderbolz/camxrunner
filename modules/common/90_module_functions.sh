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
# Function: common.module.getMetaField
# 
# For a given module name, returns the given meta-data item.
#
# Parameters:
# $1 - name of a module
# $2 - the name of the item
################################################################################
function common.module.getMetaField()
################################################################################
{
	local module="$1"
	local item="$2"
	local module_path
	local value

	
	if [[ "$(common.hash.has? $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module)" == true ]]
	then
		module_path="$(common.hash.get $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module)"
	else
		main.die_gracefully "cannot find path of $module"
	fi
	
	# source module
	source "$module_path"
	
	if [[ $? -ne 0 ]]
	then
		main.die_gracefully "could not source $module ($module_path)"
	fi
	
	# Do we have this variable?
	set | grep $item 2>&1 > /dev/null
	
	if [[ $? -ne 0 ]]
	then
		# variable not known!
		main.die_gracefully "variable $item not found!"
	else
		# Return value (indirect)
		echo ${!item}
	fi
}

################################################################################
# Function: common.module.getRawDependencies
# 
# For a given module name, returns the raw dependency string that is
# in the header (CXR_META_MODULE_DEPENDS_ON)
#
# Parameters:
# $1 - name of a module
################################################################################
function common.module.getRawDependencies()
################################################################################
{
	local module="$1"
	local raw_dependencies
	
	raw_dependencies=$(common.module.getMetaField "$module" "CXR_META_MODULE_DEPENDS_ON")

	echo "${raw_dependencies}"
}

################################################################################
# Function: common.module.getExclusive
# 
# For a given module name, returns the exclusive string that is
# in the header (CXR_META_MODULE_RUN_EXCLUSIVELY)
#
# Parameters:
# $1 - name of a module
################################################################################
function common.module.getExclusive()
################################################################################
{
	local module="$1"
	local exclusive
	
	exclusive=$(common.module.getMetaField "$module" "CXR_META_MODULE_RUN_EXCLUSIVELY")

	echo "${exclusive}"
}

################################################################################
# Function: common.module.resolveSingleDependency
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
function common.module.resolveSingleDependency()
################################################################################
{
	if [[ $# -lt 1 && $# -gt 2 ]]
	then
		main.die_gracefully "Programming error @ needs at least a depdendency and an optional day_offset as input"
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
		main.log -v  "Resolving dependency $dependency for day offset $day_offset"
	else
		main.log -v  "Resolving dependency $dependency (no day offset)"
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
				main.log -v  "Found the - predicate, will check last days results"
				day_offset=$(( $day_offset - 1 ))
				
				# Cut off final -
				dependency=${dependency%-}
				if [[ ! $day_offset -le 0 ]]
				then
					# At day 0, all previous day predicates are fulfilled
					main.log -v  "We are at day 0 - previous day dependency not relevant"
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
			if [[ $(common.hash.has? $CXR_ACTIVE_ALL_HASH $CXR_HASH_TYPE_UNIVERSAL "$dependency") == false  ]]
			then
				# Do we care?
				if [[ "$CXR_IGNORE_DISABLED_DEPENDENCIES" == true  ]]
				then
					# No, user wants to ignore this
					main.log  "You set CXR_IGNORE_DISABLED_DEPENDENCIES to true and $dependency is disabled. We will not check if this module was run"
					echo ""
					
					return $CXR_RET_OK
				else
					# Yes, we terminate
					main.die_gracefully "You set CXR_IGNORE_DISABLED_DEPENDENCIES to false and $dependency is disabled. The dependency $dependency is not fulfilled!"
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
	main.log -v  "We resolve the special dependency ${dependency}..."
	
	# Create list of active modules
	active_modules="$(common.hash.getKeys $active_hash $CXR_HASH_TYPE_GLOBAL)"
	
	for module in $active_modules
	do
		main.log -v  "Found dependency on $module"
		
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
# Function: common.module.resolveAllDependencies
# 
# resolves a complete dependenency string (containig more than one dependency), depending 
# on the day offset.
# 
# Parameters:
# $1 - a list of dependencies "create_emissions all_module-"
# [$2] - day offset (if not given, we are resolving for a One-Time module)
################################################################################
function common.module.resolveAllDependencies()
################################################################################
{
	local dependencies="$1"
	local day_offset="${2:-}"
	local dependency
	local resolved_dependency
	local resolved_list
	local first=true
	
	#Loop through dependencies
	for dependency in $dependencies
	do
		resolved_dependency="$(common.module.resolveSingleDependency "$dependency" "$day_offset")"
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
# Function: common.module.areDependenciesOk?
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
# We access the state DB using <common.state.hasFinished?> to test if stuff has finished.
#
# Parameters:
# $1 - a list of raw dependencies
# $2 - a day offset used as reference
################################################################################
function common.module.areDependenciesOk?()
################################################################################
{
	if [[ "$CXR_IGNORE_ANY_DEPENDENCIES" == true  ]]
	then
		main.log  "You set CXR_IGNORE_ANY_DEPENDENCIES to true. We will not check dependencies (pretty dangerous...)"
		echo true
		return $CXR_RET_OK
	fi
	
	if [[ $# -ne 2 ]]
	then
		main.die_gracefully "needs a depdendency list and a day offset as input"
	fi

	local raw_dependencies="$1"
	local day_offset="${2:-}"
	local dep_day_offset
	local dep_module_name
	local dependencies="$(common.module.resolveAllDependencies "$raw_dependencies" "$day_offset" )"
	local dependency
	local my_stage
	
	main.log -v  "Evaluating dependencies on $dependencies for day offset $day_offset"

	for dependency in $dependencies
	do
		# We need to parse the dependency
		# Remove potential digits
		dep_module_name=$(expr match "$dependency" '\(\<[_a-z]\{1,\}\)')
		# get only the digits at the end
		dep_day_offset=$(expr match "$dependency" '.*\([0-9]\{1,\}\>\)')
		
		# Determine type
		module_type="$(common.module.getType "$dep_module_name")"
		
		# Convert date
		raw_date="$(common.date.toRaw $(common.date.OffsetToDate "${dep_day_offset}"))"
		
		my_stage="$(common.state.getStageName "$module_type" "$dependency" "$raw_date" )"
		
		# Is this known to have worked?
		if [[ "$(common.state.hasFinished? "$my_stage")" == true ]]
		then
			main.log -v   "dependency ${dependency} fullfilled"
		else
			# dependency NOK, Find out why
			
			# Find out if dependency failed - if so, we crash
			if [[ "$(common.state.hasFailed? "$my_stage")" == true  ]]
			then
				# It failed
				# Destroy run 
				if [[ $CXR_DRY == false  ]]
				then
					main.die_gracefully "dependency ${day_offset}_${dependency} failed!"
				else
					main.log -v  "The dependency ${dependency} failed - but this is a dryrun, so we keep going!"
				fi
			else
				# It did not fail, it seems that it was not yet run - we have to wait
				main.log -v  "${dependency} has not yet finished - we need to wait."
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
# Function: common.module.updateInfo
#
# Goes through all available modules for the current model and version, and collects 
# vital information in various hashes.
# If a module name is non-unique, we fail.
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
function common.module.updateInfo()
################################################################################
{
	# Its possible that this is called more than once (e. g. during testing).
	# But this makes it hard to detect duplicates.
	if [[ "${CXR_MODULES_UP_TO_DATE:-false}" == true ]]
	then
		return $CXR_RET_OK
	fi

	main.log -v  "Updating module information..."
	
	# Increase global indent level
	main.increaseLogIndent
	
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
		
		main.log -v  "Adding $type modules..."
		
		# Find all of them
		files="$(find $dir -noleaf -maxdepth 1 -name '*.sh')"

		for file in $files
		do
			module_name="$(main.getModuleName $file)"
			main.log -v  "Adding module $module_name in $file"
			
			# Is there a new entry of this name? (this would indicate non-uniqueness!)
			if [[ $(common.hash.has? $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module_name) == true && $(common.hash.isNew? $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module_name) == true ]]
			then
				main.die_gracefully "There seem to be more than one module called ${module_name}. This is not allowed - please adjust the names!"
			fi
			
			# Path 
			common.hash.put $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module_name $file
			
			# Type
			common.hash.put $CXR_MODULE_TYPE_HASH $CXR_HASH_TYPE_UNIVERSAL $module_name $type
			
			# All Hash (value is dummy)
			common.hash.put $CXR_ACTIVE_ALL_HASH $CXR_HASH_TYPE_GLOBAL $module_name true
		
			# The current types active hash
			common.hash.put $active_hash $CXR_HASH_TYPE_GLOBAL $module_name true
		
		done # Loop over files
	done # loop over type-index
	
	# decrease global indent level
	main.decreaseLogIndent
	
	CXR_MODULES_UP_TO_DATE=true
}

################################################################################
# Function: common.module.getType
#
# Gets its information directly from the CXR_MODULE_TYPE_HASH
# 
# Parameters:
# $1 - name of module (without prefix or suffix, just something like "convert output"
################################################################################
function common.module.getType()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		main.log -e  "Need a module name as input"
	fi
	
	local name="${1}"
	local module_type

	if [[ "$(common.hash.has? "$CXR_MODULE_TYPE_HASH" $CXR_HASH_TYPE_UNIVERSAL "$name" )" == true ]]
	then
		module_type="$(common.hash.get "$CXR_MODULE_TYPE_HASH" $CXR_HASH_TYPE_UNIVERSAL "$name" )"
		
		if [[ "$module_type" ]]
		then
			echo "$module_type"
		else
			main.die_gracefully "Could not find module type of $name!"
		fi
	else
		main.die_gracefully "Could not find module $name!"
	fi
	
}

################################################################################
# Function: common.module.runType
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
function common.module.runType()
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
			main.die_gracefully "Common modules cannot be run this way!" ;;
			
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
			main.die_gracefully "Unknown module type $module_type" ;;

	esac
	
	# Increase global indent level
	main.increaseLogIndent
	
	# Check if we need any of them at all
	# If the user wants to run a specific module, we enter anyway
	if [[ ! ( "${enabled_modules}" == "" && "${disabled_modules}" == "${CXR_SKIP_ALL}" && "$run_only" == "${CXR_RUN_ALL}" ) ]]
	then
	
		# We did not turn off everything or we need only a specific module to be run
	
		# Loop through available input dirs
		for module_directory in $module_directories
		do
			main.log  "Loading $module_type modules from $module_directory..."
		
			for function_file in $(ls ${module_directory}/??_*.sh 2>/dev/null)
			do
				# Check if we are still happy if needed
				if [[ "${check_continue}" == true  ]]
				then
					common.state.doContinue? || main.die_gracefully "Continue file no longer present."
				fi
				
				FILE_NAME=$(basename "$function_file")
				
				# Before loading a new module, remove old meta variables
				unset ${!CXR_META_MODULE*}
				
				# Export the module name
				CXR_META_MODULE_NAME=$(main.getModuleName $function_file)
				
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
							main.log -a -b   "Running $FILE_NAME ${our_date:-}"
						fi
						
						# Show dependencies, if any
						if [[ "${CXR_META_MODULE_DEPENDS_ON:-}"  ]]
						then
							main.log -a -B  "This module depends on these modules:\n${CXR_META_MODULE_DEPENDS_ON}\nif it fails, run these dependencies first"
						fi

						# Increase global indent level
						main.increaseLogIndent
						
						if [[ "$(common.check.ModuleRequirements)" == true  ]]
						then
							main.log -v   "Starting Module $CXR_META_MODULE_NAME"
							"$CXR_META_MODULE_NAME" || ret_val=$CXR_RET_ERROR
						else
							main.log  "Version check for $CXR_META_MODULE_NAME failed. Either change the values in the head of the module or manipulate the revision numbers of either CAMxRunner.sh or the configuration.\nModule skipped."
						fi

						# Take note that this module was already announced
						CXR_ANNOUNCED_MODULES="${CXR_ANNOUNCED_MODULES} ${CXR_META_MODULE_NAME}"
							
						# Decrease global indent level
						main.decreaseLogIndent
					fi
				else
					#Run all modules of the given type
				
					# First source the file to get the meta info
					source $function_file
					
					# Check if we must run this
					# if the module name is in the enabled list, run it,no matter what
					if [[ "$(common.string.isSubstringPresent? "$enabled_modules" "$CXR_META_MODULE_NAME")" == true  ]]
					then
						# Module was explicitly enabled
						run_it=true
					elif [[  "$(common.string.isSubstringPresent? "$disabled_modules" "$CXR_META_MODULE_NAME")" == false && "${disabled_modules}" != "${CXR_SKIP_ALL}"   ]]
					then
						# Module was not explicitly disabled and we did not disable all
						run_it=true
					else
						# If the name of the module is in the disabled list, this should not be run (except if it is in the enabled list)
						run_it=false
						main.log  "Step $FILE_NAME is disabled, skipped"
					fi
					
					# Execute if needed
					if [[ "$run_it" == true  ]]
					then
					
						if [[ "$module_type" != "$CXR_TYPE_INSTALLER"  ]]
						then
							main.log -a -b   "Running $FILE_NAME ${our_date:-}"
						fi
						
						# Increase global indent level
						main.increaseLogIndent
						
						if [[ "$(common.check.ModuleRequirements)" == true  ]]
						then
							main.log -v   "Starting Module $CXR_META_MODULE_NAME"
							"$CXR_META_MODULE_NAME" || ret_val=$CXR_RET_ERROR
						else
							main.log  "Version check for $CXR_META_MODULE_NAME failed. Either change the values in the head of the module or manipulate the revision numbers of either CAMxRunner.sh or the configuration.\nModule skipped."
						fi

						# Take note that this module was already announced
						CXR_ANNOUNCED_MODULES="${CXR_ANNOUNCED_MODULES} ${CXR_META_MODULE_NAME}"
							
						# Decrease global indent level
						main.decreaseLogIndent
					fi
					
				fi
			done
		done # Loop through module dirs
	else
		main.log  "You disabled all modules of type $module_type by setting  CXR_DISABLED_... to ${CXR_SKIP_ALL}, none executed."
	fi 
	
	# Decrease global indent level
	main.decreaseLogIndent
	
	return ${ret_val}
}

################################################################################
# Function: common.module.processSequentially
#	
# Executes all (needed) modules in sequential order (non-parallel version of the task_functions)
# If only one process is used, this is faster because there is less overhead.
# For each part of the processing, we check if we need to run it and pass any limitations
# on to <common.module.runType>.
#
################################################################################
function common.module.processSequentially
################################################################################
{
	# Set up return value
	local ret_val=$CXR_RET_OK
	local day_offset
	

	# Setup environment
	common.date.setVars "$CXR_START_DATE" 0
	
	if [[ ${CXR_RUN_PRE_ONCE} == true  ]]
	then
		common.module.runType ${CXR_TYPE_PREPROCESS_ONCE} ${CXR_RUN_PRE_ONCE_STEP:-${CXR_RUN_ALL}} || ret_val=$CXR_RET_ERROR
	else
		main.log -w "We do not run ${CXR_TYPE_PREPROCESS_ONCE} modules."
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
				day_offset="$(common.date.toOffset ${CXR_ONE_DAY})"
				main.log  "${CXR_ONE_DAY} corresponds to offset ${day_offset}."
			fi
		
			# Setup environment
			common.date.setVars "$CXR_START_DATE" "$day_offset"
			
			main.log -B "Processing ${CXR_DATE:-now}"
			
			# Run the three daily module types in order
			
			if [[ ${CXR_RUN_PRE_DAILY} == true  ]]
			then
				common.module.runType ${CXR_TYPE_PREPROCESS_DAILY} ${CXR_RUN_PRE_DAILY_STEP:-${CXR_RUN_ALL}} || ret_val=$CXR_RET_ERROR
			else
				main.log -w "We do not run ${CXR_TYPE_PREPROCESS_DAILY} modules."
			fi
			
			if [[ ${CXR_RUN_MODEL} == true  ]]
			then
				common.module.runType ${CXR_TYPE_MODEL} ${CXR_RUN_MODEL_SINGLE_STEP:-${CXR_RUN_ALL}} || ret_val=$CXR_RET_ERROR
			else
				main.log -w "We do not run ${CXR_TYPE_MODEL} modules."
			fi
			
			if [[ ${CXR_RUN_POST_DAILY} == true  ]]
			then
				common.module.runType ${CXR_TYPE_POSTPROCESS_DAILY} ${CXR_RUN_POST_DAILY_STEP:-${CXR_RUN_ALL}} || ret_val=$CXR_RET_ERROR
			else
				main.log -w "We do not run ${CXR_TYPE_POSTPROCESS_DAILY} modules."
			fi
			
			# If we do only 1 day, that's it
			if [[ "${CXR_ONE_DAY}"  ]]
			then
				break
			fi
			
		done # Loop through days
	else
		main.log -w "We do not run any daily modules"
	fi
	
	if [[ ${CXR_RUN_POST_ONCE} == true  ]]
	then
		common.module.runType ${CXR_TYPE_POSTPROCESS_ONCE} ${CXR_RUN_POST_ONCE_STEP:-${CXR_RUN_ALL}} || ret_val=$CXR_RET_ERROR
	else
		main.log -w "We do not run ${CXR_TYPE_POSTPROCESS_ONCE} modules."
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
	
	is $(common.module.getType boundary_conditions) ${CXR_TYPE_PREPROCESS_DAILY} "common.module.getType boundary_conditions"
	is $(common.module.getExclusive model) true "common.module.getExclusive model"



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