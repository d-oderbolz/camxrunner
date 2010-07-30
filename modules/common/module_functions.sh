# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains functions to manage modules.
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

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=20

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


################################################################################
# Function: common.module.parseIdentifier
# 
# Parses an identifier string of the form Module[Offset][@Invocation],
# e. g. prepare_output_dir0 or convert_output0@1
# Currently, no correction is done if a value is empty (this is by design)
#
# Output variables:
# _module
# _day_offset
# _invocation
#
# Parameters:
# $1 - an identifier
################################################################################
function common.module.parseIdentifier()
################################################################################
{
	if [[ $# -ne 1 || -z "$1" ]]
	then
		main.dieGracefully "Needs a non-empty identifier as Input"
	fi
	
	identifier="$1"
	
	main.log -v "Parsing $identifier"
	
	# Get just lowercase text at the beginning
	_module="$(expr match "$identifier" '\(\<[_a-zA-Z]\{1,\}\)')"  || :
		
	# get only the digits after the name, must handle the empty case using || : (otherwise we die here)
	# the @-sign might be missing
	_day_offset="$(expr match "$identifier" '\<[_a-zA-Z]\{1,\}\([0-9]\{1,\}\)@\{0,\}[0-9]\{0,\}\>')" || :
		
	# get invocation - needs the @-sign
	_invocation="$(expr match "$identifier" '.*@\([0-9]\{1,\}\>\)')" || :
	
	main.log -v "module: $_module day_offset: $_day_offset invocation: $_invocation"
}

################################################################################
# Function: common.module.getNumInvocations
# 
# For a given module name, returns the value of getNumInvocations
# Caveat: This function modifies the environment - always call like this:
# > b=$(common.module.getNumInvocations "$module")
# The $() construct opens a subshell.
#
# TODO: This should be stored in the "modules" table
#
# Parameters:
# $1 - name of a module
################################################################################
function common.module.getNumInvocations()
################################################################################
{
	local module
	local numInvocations
	
	module="$1"
	
	module_path="$(common.module.getPath "$module")"
	
	# Before sourcing, set this Meta var
	CXR_META_MODULE_NAME=$module
	
	# source module
	source "$module_path"
	
	if [[ $? -ne 0 ]]
	then
		main.dieGracefully "could not source $module ($module_path)"
	fi
	
	# Call the function
	numInvocations=$(getNumInvocations)
	
	echo $numInvocations
}

################################################################################
# Function: common.module.getPath
# 
# For a given module name, returns the whole path of its file out of the state DB.
#
# Parameters:
# $1 - name of a module
# $2 - the name of the item
################################################################################
function common.module.getPath()
################################################################################
{
	path=$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT path FROM modules WHERE module='$1'")
	echo "$path"
}

################################################################################
# Function: common.module.getType
# 
# For a given module name, returns the type of the module.
#
# Parameters:
# $1 - name of a module
# $2 - the name of the item
################################################################################
function common.module.getType()
################################################################################
{
	type=$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT type FROM modules WHERE module='$1'")
	echo "$type"
}

################################################################################
# Function: common.module.getMetaField
# 
# For a given module name, returns the given meta-data item by looking at the DB.
#
# Parameters:
# $1 - name of a module
# $2 - the name of the item
################################################################################
function common.module.getMetaField()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a module name and a meta item as input"
	fi
	
	local module

	# Do we have this variable?
	value=$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT value FROM metadata WHERE module='$1' and field='$2'")
	
	echo "$value"
}

################################################################################
# Function: common.module.isActive?
# 
# Tests if a given module is active (listed in the modules table)
#
# Parameters:
# $1 - name of a module
################################################################################
function common.module.isActive?()
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "needs a module name as input"
	fi
	
	local module
	local count

	# Count entries
	count=$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT COUNT(*) FROM modules WHERE module='$1'")
	
	if [[ $count -eq 1 ]]
	then
		echo true
	else
		echo false
	fi
}
################################################################################
# Function: common.module.resolveSingleDependency
# 
# Resolves a single dependenency string (containig just one dependency), depending 
# on the day offset.
# - turns the predicate "-" into actual day offsets
# - turn all_* dependecies like "CXR_DEP_ALL_ONCE_PRE" into actual module names
# - adds the different invocations if needed
# So "all_model- " becomes "model1@1" at day 2, "albedo_haze_ozone" becomes "albedo_haze_ozone@1 albedo_haze_ozone@2" 
# if albedo_haze_ozone can be split into two invocations.
#
# Any dependency that is not relevant (like module- for the first day) is not returned.
# If a dependency is disabled and this is allowed, we warn the user, otherwise we terminate
# 
#
# Parameters:
# $1 - name of the dependency like "create_emissions" or a special dependency like "${CXR_DEP_ALL_MODEL}")
# [$2] - day offset (if not given, we are resolving for a One-Time module)
################################################################################
function common.module.resolveSingleDependency()
################################################################################
{
	if [[ $# -lt 1 || $# -gt 2 ]]
	then
		main.dieGracefully "Programming error - we need at least a depdendency and an optional day_offset as input"
	fi

	local dependency
	local day_offset
	local our_day_offset
	local predicate
	local my_prefix
	local active_hash
	local module_type
	local resolved_dependencies
	local resolved_dependency
	local module
	local first
	local iKey
	local iInvocation
	local nInvocations
	local arrKeys
	local keyString
	
	
	dependency="$1"
	day_offset="${2:-}"
	our_day_offset="${2:-}"
	first=true
	
	if [[ "$day_offset" ]]
	then
		main.log -v "Resolving dependency $dependency for day offset $day_offset"
	else
		main.log -v "Resolving dependency $dependency (no day offset)"
	fi
	
	# Is there a predicate?
	# The last character of a string
	# We get 1 character at the position $(( ${#dependency} -1)) (length of string minus 1)
	predicate=${dependency:$(( ${#dependency} -1)):1}
	
	case $predicate in
		-) 
			# This predicate refers to the same module, executed in the preceding simulation day
			# if there is no day offset, this dependency is not relevant
			if [[ "$day_offset" ]]
			then
				# Test day offset
				if [[ $day_offset -le 0 ]]
				then
					# At day 0, all previous day predicates are fulfilled
					main.log -v  "We are at day 0 - previous day dependency $dependency not relevant"
					echo ""
					return $CXR_RET_OK
				fi
				
				# Correct offset
				day_offset=$(( $day_offset - 1 ))
				
				# Cut off final -
				# The actual resolving happens further down.
				dependency=${dependency%-}
				
			else
				main.log -v  "Dependency $dependency not relevant since there is no day offset"
				# No offset
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
			if [[ $(common.module.isActive? "$dependency") == false ]]
			then
				# not active
				# Do we care?
				if [[ "$CXR_IGNORE_DISABLED_DEPENDENCIES" == true  ]]
				then
					# No, user wants to ignore this
					main.log  -v "You set CXR_IGNORE_DISABLED_DEPENDENCIES to true and $dependency is disabled. We will not check if this module was run"
				else
					# Yes, we terminate
					main.dieGracefully "You set CXR_IGNORE_DISABLED_DEPENDENCIES to false and $dependency is disabled. The dependency $dependency is not fulfilled!"
				fi # disabled dependencies allowed?
			fi # dependency disabled?
			
			# We ned the module type to know if we pass a day offset or not
			module_type=$(common.module.getType $dependency)
			
			nInvocations=$(common.module.getNumInvocations "$dependency")
			for iInvocation in $(seq 1 $nInvocations )
			do
				resolved_dependency="$(common.task.getId "$dependency" "$day_offset" "$iInvocation")"
				
				if [[ "$first" == true ]]
				then
					# First iteration
					resolved_dependencies="$resolved_dependency"
					first=false
				else
					resolved_dependencies="${resolved_dependencies} ${resolved_dependency}"
				fi
			
			done # invocations
			
			echo ${resolved_dependencies}
			
			return $CXR_RET_OK
			;;
	
	esac
	
	########################################
	# Here, we handle the special cases
	########################################	
	main.log -v  "We resolve the special dependency ${dependency}..."
	
	oIFS="$IFS"
	keyString="$(common.hash.getKeys $active_hash $CXR_HASH_TYPE_GLOBAL)"
	IFS="$CXR_DELIMITER"
	# Turn string into array (we cannot call <common.hash.getKeys> directly here!)
	arrKeys=( $keyString )
	# Reset Internal Field separator
	IFS="$oIFS"
	
	# looping through keys (safest approach)
	for iKey in $( seq 0 $(( ${#arrKeys[@]} - 1)) )
	do
		module="${arrKeys[$iKey]}"
		main.log -v  "Found dependency on $module"
		
		# We ned the module type to know if we pass a day offset or not
		module_type=$(common.module.getType $module)
		
		nInvocations=$(common.module.getNumInvocations "$module")
		for iInvocation in $(seq 1 $nInvocations )
		do
			resolved_dependency="$(common.task.getId "$module" "$day_offset" "$iInvocation")"
		
			if [[ "$first" == true ]]
			then
				# First iteration
				resolved_dependencies="$resolved_dependency"
				first=false
			else
				# any other iteration
				resolved_dependencies="$resolved_dependencies $resolved_dependency"
			fi
		
		done # invocations
		
	done  # Loop over active modules
	
	echo "$resolved_dependencies"
	return $CXR_RET_OK
}

################################################################################
# Function: common.module.resolveAllDependencies
# 
# Resolves a complete dependenency string (containig more than one dependency), depending 
# on the day offset. 
# 
# Parameters:
# $1 - a list of dependencies like "create_emissions all_module-"
# [$2] - day offset (if not given, we are resolving for a One-Time module)
################################################################################
function common.module.resolveAllDependencies()
################################################################################
{
	local dependencies
	local day_offset
	local dependency
	local resolved_dependency
	local resolved_dependencies
	local first
	
	dependencies="$1"
	day_offset="${2:-}"
	resolved_dependency=""
	resolved_dependencies=""
	first=true
	
	#Loop through dependencies
	for dependency in $dependencies
	do
		resolved_dependency="$(common.module.resolveSingleDependency "$dependency" "$day_offset")"
		if [[ "$first" == true ]]
		then
			# First iteration
			resolved_dependencies="$resolved_dependency"
			first=false
		else
			# any other iteration
			resolved_dependencies="$resolved_dependencies $resolved_dependency"
		fi
	done
	
	echo "$resolved_dependencies"
}

################################################################################
# Function: common.module.areDependenciesOk?
#
# Checks if all given raw dependencies are fullfilled. If any dependency has failed,
# the run is destroyed, if the depdendency was not yet started, false is returned,
# if it is OK, true is returned.
#
# Can handle dependencies on a whole type (like ${CXR_DEP_ALL_MODEL}) and the predicates + and -
#
# Checks if a dependency is listed in the list of active modules.
# 
# We access the state DB using <common.state.hasFinished?> to test if a step has finished.
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
		main.dieGracefully "needs a depdendency list and a day offset as input"
	fi

	local raw_dependencies
	local day_offset
	local dep_day_offset
	local dep_module
	local dependency
	local my_stage
	local iInvocation
	local nInvocations
	local dependencies
	
	raw_dependencies="$1"
	day_offset="${2:-}"
	
	if [[ ! "$raw_dependencies" ]]
	then
		echo true
		main.log -v "Dependency empty - OK"
		return $CXR_RET_OK
	fi
		
	main.log -v  "Evaluating dependencies on $raw_dependencies for day offset ${day_offset:-0}"
	
	# Resolve them
	dependencies="$(common.module.resolveAllDependencies "$raw_dependencies" "$day_offset" )"
	
	main.log -v "Resolved dependencies: $dependencies"

	for dependency in $dependencies
	do
		# We need to parse the dependency
		# this sets a couple of _variables
		common.module.parseIdentifier "$dependency"
		
		dep_module="$_module"
		dep_day_offset="$_day_offset"
		dep_invocation="$_invocation"
		
		# If there is no offset set in to 0 (TODO: Check correctness!)
		if [[ -z "$(common.string.trim "$dep_day_offset")" ]]
		then
			dep_day_offset=0
		fi
		
		# Check if it is disabled
		if [[ $(common.module.isActive? "$dep_module") == false  ]]
		then
			# Do we care?
			if [[ "$CXR_IGNORE_DISABLED_DEPENDENCIES" == true ]]
			then
				# No, user wants to ignore this
				main.log  -w "You set CXR_IGNORE_DISABLED_DEPENDENCIES to true and $dep_module is disabled. We will not check if this module was run"
				# Next dependency
				continue
			else
				# Yes, we terminate
				main.dieGracefully "You set CXR_IGNORE_DISABLED_DEPENDENCIES to false and $dep_module is disabled. The dependency $dep_module cannot be fulfilled!"
			fi # disabled dependencies allowed?
		fi # dependency disabled?
		
		# Determine type
		module_type="$(common.module.getType "$dep_module")"
		
		my_stage="$(common.task.getId "$dep_module" "$dep_day_offset" "$dep_invocation" )"
		
		# Is this known to have worked?
		if [[ "$(common.state.hasFinished? "$dep_module" "$dep_day_offset" "$dep_invocation")" == true ]]
		then
			main.log -v "dependency ${dependency} fullfilled at $my_stage"
		else
			# dependency NOK, Find out why
			
			# Find out if dependency failed - if so, we crash
			if [[ "$(common.state.hasFailed? "$dep_module" "$dep_day_offset" "$dep_invocation")" == true ]]
			then
				# It failed
				# Destroy run (we used not to do this in dryruns. But this can result in a lockup!
				main.dieGracefully "dependency ${dependency} failed at $my_stage !"
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
}

################################################################################
# Function: common.module.listModuleType
#	
# Shows a list of recognised modules of the given type and displays how they can
# be called individually.
#
# Parameters:
# $1 - Type of modules to be shown
################################################################################
function common.module.listModuleType()
################################################################################
{
	local module_type
	local call
	local module_directories
	local disabled_modules
	local module_directory
	local pattern
	local num_modules
	local function_file
	local file_name
	local total_call
	local found
	
	module_type="$1"
	
	
	# What kind of module?
	# - MODULE_DIRECOTRIES is a list of directories that will be used to search for modules
	# - disabled_modules is a list of disabled modules of the current type
	
	case "$module_type" in
	
		"${CXR_TYPE_COMMON}" ) 
		main.dieGracefully "Common modules cannot be run this way!" ;;
			
		"${CXR_TYPE_PREPROCESS_ONCE}" ) 
			call="-r"
			module_directories="${CXR_PREPROCESSOR_ONCE_INPUT_DIR}" 
			disabled_modules="${CXR_DISABLED_ONCE_PREPROC:-}";;
			
		"${CXR_TYPE_PREPROCESS_DAILY}" ) 
			call="-r"
			module_directories="${CXR_PREPROCESSOR_DAILY_INPUT_DIR}" 
			disabled_modules="${CXR_DISABLED_DAILY_PREPROC:-}";;
			
			"${CXR_TYPE_MODEL}" ) 
			call="-r"
			module_directories="${CXR_POSTPROCESSOR_DAILY_INPUT_DIR}" 
			disabled_modules="${CXR_DISABLED_DAILY_POSTPROC:-}";;
			
		"${CXR_TYPE_POSTPROCESS_DAILY}" ) 
			call="-r"
			module_directories="${CXR_POSTPROCESSOR_DAILY_INPUT_DIR}" 
			disabled_modules="${CXR_DISABLED_DAILY_POSTPROC:-}";;
			
		"${CXR_TYPE_POSTPROCESS_ONCE}" ) 
			call="-r"
			module_directories="${CXR_POSTPROCESSOR_ONCE_INPUT_DIR}" 
			disabled_modules="${CXR_DISABLED_ONCE_POSTPROC:-}";;
			
		* ) 
			main.dieGracefully "Unknown module type $module_type" ;;

	esac
	
	# Increase global indent level
	main.increaseLogIndent
	
	# Loop through available input dirs
	for module_directory in $module_directories
	do
		# How do processors look like?
		pattern="${module_directory}/*.sh"
		
		#Count modules
		num_modules=$(ls ${pattern} 2> /dev/null | wc -l)
		
		if [[ "$num_modules" -gt 0  ]]
		then
			main.log -v  "  $(printf %-32s%-75s Name call)"
			
			# Active Processors
			for function_file in $(ls ${pattern} 2> /dev/null)
			do
				file_name=$(basename $function_file)
				total_call="${CXR_CALL} ${call}\"$(main.getModuleName $function_file)\""
				
				main.log -a  "  $(printf %-32s%-75s "${file_name}" "${total_call}")"
				found=true
			done
		else
			# Increase global indent level
			main.increaseLogIndent
	
			main.log -a "No enabled modules of type $module_type where found. Is this intended?\n"
			
			# Decrease global indent level
			main.decreaseLogIndent
		fi
	
		if [[ "$disabled_modules" ]]
		then
			main.log -w  "You disabled these modules in the configuration: $disabled_modules"
		fi
	
		main.log "\n  These $module_type modules are disabled physically (if any) - to run them, remove the .${CXR_DISABLED_EXT} in the filename:\n"
		
		# How do disabled modules look like?
		pattern="${module_directory}/??_*.${CXR_DISABLED_EXT}"
		
		#Count processors
		num_modules=$(ls ${pattern} 2> /dev/null | wc -l)
		
		if [[ "$num_modules" -gt 0 ]]
		then
			# Disabled Processors
			for function_file in $(ls ${pattern} 2> /dev/null)
			do
				file_name=$(basename $function_file)
				main.log -w   "$file_name"
			done
		else
			# Increase global indent level
			main.increaseLogIndent
	
			main.log -a   "  No disabled modules of type $module_type where found.\n"
			
			# Decrease global indent level
			main.decreaseLogIndent
		fi
		
	done # Loop through directories
	
	# Decrease global indent level
	main.decreaseLogIndent
}

################################################################################
# Function: common.module.listAllModules
#	
# Shows a list of all recognised modules that can
# be called individually.
#
################################################################################
function common.module.listAllModules
################################################################################
{
		main.log -a -B "\n  These modules are available for $CXR_MODEL $CXR_MODEL_VERSION.\n  All of these can be combined in one single -r"" statement."
		
		main.log -a "\n  One-Time pre-processing:\n"
		
		# module_functions.sh
		common.module.listModuleType ${CXR_TYPE_PREPROCESS_ONCE}
		
		main.log -a "\n  Daily pre-processing:\n"
		
		# module_functions.sh
		common.module.listModuleType ${CXR_TYPE_PREPROCESS_DAILY}
		
		main.log -a "\n  Model:\n"
		
		# module_functions.sh
		common.module.listModuleType ${CXR_TYPE_MODEL}
		
		main.log -a "\n  Daily post-processing:\n"
		
		# module_functions.sh
		common.module.listModuleType ${CXR_TYPE_POSTPROCESS_DAILY}
		
		main.log -a "\n  One-Time post-processing steps:\n"
		
		# module_functions.sh
		common.module.listModuleType ${CXR_TYPE_POSTPROCESS_ONCE}
		
		main.log -a "\n  To disable single modules, you can add the name of a module you do *not* want to run to either of the lists\n CXR_DISABLED_DAILY_PREPROC,\n CXR_DISABLED_ONCE_PREPROC,\n CXR_DISABLED_DAILY_POSTPROC or\n CXR_DISABLED_ONCE_POSTPROC\nin your configuration file. Setting any of these strings to \"${CXR_SKIP_ALL}\" disables all modules of this class."    
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
	########################################
	# Setup tests if needed
	########################################
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	# Testing parser. Always in groups of 3 with a call at the beginning
	common.module.parseIdentifier convert_emissions
	is "$_module" convert_emissions "common.module.parseIdentifier only module - name"
	is "$_day_offset" "" "common.module.parseIdentifier only module - day offset"
	is "$_invocation" "" "common.module.parseIdentifier only module - invocation"
	
	common.module.parseIdentifier some_module0
	is "$_module" some_module "common.module.parseIdentifier module and day offset - name"
	is "$_day_offset" 0 "common.module.parseIdentifier module and day offset - day offset"
	is "$_invocation" "" "common.module.parseIdentifier module and day offset - invocation"
	
	common.module.parseIdentifier differentcase@2
	is "$_module" differentcase "common.module.parseIdentifier module and invocation - name"
	is "$_day_offset" "" "common.module.parseIdentifier module and invocation - day offset"
	is "$_invocation" 2 "common.module.parseIdentifier module and invocation - invocation"
	
	common.module.parseIdentifier ThatsNowAll3@1
	is "$_module" ThatsNowAll "common.module.parseIdentifier complete - name"
	is "$_day_offset" 3 "common.module.parseIdentifier complete - day offset"
	is "$_invocation" 1 "common.module.parseIdentifier complete - invocation"
	
	common.module.parseIdentifier ThatsNowAll123@24
	is "$_module" ThatsNowAll "common.module.parseIdentifier multidigit - name"
	is "$_day_offset" 123 "common.module.parseIdentifier multidigit - day offset"
	is "$_invocation" 24 "common.module.parseIdentifier multidigit - invocation"
	
	is $(common.module.getType boundary_conditions) ${CXR_TYPE_PREPROCESS_DAILY} "common.module.getType boundary_conditions"
	is $(common.module.getMetaField model "CXR_META_MODULE_RUN_EXCLUSIVELY") true "common.module.getMetaField model CXR_META_MODULE_RUN_EXCLUSIVELY"
	
	# Testing the resolvers for dependencies
	is "$(common.module.resolveSingleDependency "model-" 0)" "" "common.module.resolveSingleDependency minus on first day"
	is "$(common.module.resolveSingleDependency "model-" 1)" "model0" "common.module.resolveSingleDependency minus on second day"
	
	is "$(common.module.resolveAllDependencies "model- create_emissions" 1)" "model0 create_emissions1@1 create_emissions1@2 create_emissions1@3" "common.module.resolveAllDependencies on second day"
	
	
	########################################
	# teardown tests if needed
	########################################
	
}
