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

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=20

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


################################################################################
# Function: common.module.parseIdentifier
# 
# Parses an identifier string of the form Module[Offset][@Invocation],
# e. g. prepare_output_dir0 or convert_output0@1
# Currently, no correction is done if a value is empty (this is by design)
#
# Output variables:
# _module_name
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
	_module_name="$(expr match "$identifier" '\(\<[_a-z]\{1,\}\)')"
		
	# get only the digits after the name, must handle the empty case using || : (otherwise we die here)
	# the @-sign might be missing
	_day_offset="$(expr match "$identifier" '\<[_a-z]\{1,\}\([0-9]\{1,\}\)@\{0,\}[0-9]\{0,\}\>')" || :
		
	# get invocation - needs the @-sign
	_invocation="$(expr match "$identifier" '.*@\([0-9]\{1,\}\>\)')" || :
	
	main.log -v "module: $_module_name day_offset: $_day_offset invocation: $_invocation"
}

################################################################################
# Function: common.module.getNumInvocations
# 
# For a given module name, returns the value of getNumInvocations
# Caveat: This function modifies the environment - always call like this:
# > b=$(common.module.getNumInvocations "$module")
# The $() construct opens a subshell.
# Values are cached in a hash.
#
# Parameters:
# $1 - name of a module
################################################################################
function common.module.getNumInvocations()
################################################################################
{
	local module="$1"
	local numInvocations
	local cache="CACHE_NumInvocations"
	
	# This call sets _has and _value
	common.hash.has? $cache $CXR_HASH_TYPE_UNIVERSAL $module > /dev/null
	if [[ "$_has" == true ]]
	then
		# It's in the cache
		echo "$_value"
	else
	
		# This call sets _has and _value
		common.hash.has? $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module > /dev/null
		if [[ "$_has" == true ]]
		then
			module_path="$_value"
		else
			main.dieGracefully "cannot find path of $module"
		fi
		
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
		
		# Add to cache
		common.hash.put $cache $CXR_HASH_TYPE_UNIVERSAL "$module" "${numInvocations}"
		
		echo $numInvocations
	fi
}

################################################################################
# Function: common.module.getMetaField
# 
# For a given module name, returns the given meta-data item.
# Caveat: This function modifies the environment - always call like this:
# > b=$(common.module.getMetaField "$module" "CXR_META_MODULE_RUN_EXCLUSIVELY")
# The $() construct opens a subshell.
# We cache the results in a hash, whose name depends on the item to be retrieved.
#
# Parameters:
# $1 - name of a module
# $2 - the name of the item
################################################################################
function common.module.getMetaField()
################################################################################
{
	if [[ $# -ne 2  ]]
	then
		main.dieGracefully "needs a module name and a meta item as input"
	fi
	
	local module="$1"
	local item="$2"
	local module_path
	local value
	local cache="CACHE_${item}"
	
	# This call sets _has and _value
	common.hash.has? $cache $CXR_HASH_TYPE_UNIVERSAL $module > /dev/null
	if [[ "$_has" == true ]]
	then
		# It's in the cache
		echo "$_value"
	else
		if [[ "$(common.hash.has? $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL "$module")" == true ]]
		then
			module_path="$(common.hash.get $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL "$module")"
		else
			main.dieGracefully "cannot find path of $module"
		fi
		
		# Before sourcing, set this Meta var
		CXR_META_MODULE_NAME="$module"
		
		# source module
		source "$module_path"
		
		if [[ $? -ne 0 ]]
		then
			main.dieGracefully "could not source $module ($module_path)"
		fi
		
		# Do we have this variable?
		set | grep $item 2>&1 > /dev/null
		
		if [[ $? -ne 0 ]]
		then
			# variable not known!
			main.dieGracefully "variable $item not found!"
		else
			main.log -v "${item}: ${!item}"
			
			# Add to cache
			common.hash.put $cache $CXR_HASH_TYPE_UNIVERSAL "$module" "${!item}"
			
			# Return value (indirect)
			echo ${!item}
		fi
		
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
# Function: common.module.getExclusive
# 
# For a given module name, returns the exclusive string that is
# in the header (CXR_META_MODULE_NUM_TESTS)
#
# Parameters:
# $1 - name of a module
################################################################################
function common.module.getNumTests()
################################################################################
{
	local module="$1"
	local num_tests
	
	num_tests=$(common.module.getMetaField "$module" "CXR_META_MODULE_NUM_TESTS")

	echo "${num_tests}"
}

################################################################################
# Function: common.module.resolveSingleDependency
# 
# Resolves a single dependenency string (containig just one dependency), depending 
# on the day offset.
# - turns the predicates "+" and "-" into actual day offsets
# - turn all_* dependecies like "CXR_DEP_ALL_ONCE_PRE" into actual module names
# - adds the different invorations if needed
# So "all_model- " becomes "model1@1" at day 2, "albedo_haze_ozone" becomes "albedo_haze_ozone@1 albedo_haze_ozone@2" 
# if albedo_haze_ozone can be split into two invocations.
#
# Any dependency that is not relevant (like module- for the first day) is not returned.
# If a dependency is disabled and this is allowed, we warn the user, otherwise we terminate
# 
# Hashes:
# CXR_ACTIVE_ALL_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active modules (dummy value)
# CXR_ACTIVE_ONCE_PRE_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active One-Time preprocessing modules (dummy value)
# CXR_ACTIVE_DAILY_PRE_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active daily preprocessing modules (dummy value)
# CXR_ACTIVE_MODEL_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active model modules (dummy value)
# CXR_ACTIVE_DAILY_POST_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active daily postprocessing modules (dummy value)
# CXR_ACTIVE_ONCE_POST_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active One-Time postprocessing modules (dummy value)

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

	local dependency="$1"
	local day_offset="${2:-}"
	local our_day_offset="${2:-}"
	local predicate
	local my_prefix
	local active_hash
	local module_type
	local raw_date
	local resolved_dependencies
	local resolved_dependency
	local module
	local first=true
	local iKey
	local iInvocation
	local nInvocations
	
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
			
		+) 
			# This predicate refers to all simulation days
			# It is used to refer to a daily module from a One-Time module
			# makes sure that the daily model was executed for all days
			# Can only be used to refer to a name of a module, so
			# ${CXR_DEP_ALL_MODEL}+ is invalid
			
			# Cut off final +
			dependency=${dependency%+}
			main.log -v  "Found the + predicate, will check that $dependency ran for all days"
			
			case $dependency in
	
				$CXR_DEP_ALL_ONCE_PRE | \
				$CXR_DEP_ALL_DAILY_PRE | \
				$CXR_DEP_ALL_DAILY_POST | \
				$CXR_DEP_ALL_ONCE_POST | \
				$CXR_DEP_ALL_MODEL) main.dieGracefully "Predicate + not allowed for $dependency" ;; 

				*) # Handle + Predicate
					for our_day_offset in $(seq 0 $((${CXR_NUMBER_OF_SIM_DAYS} -1 )) )
					do
						nInvocations=$(common.module.getNumInvocations "$dependency")
						for iInvocation in $(seq 1 $nInvocations )
						do
							module_type=$(common.hash.get $CXR_MODULE_TYPE_HASH $CXR_HASH_TYPE_UNIVERSAL $dependency)
							resolved_dependency="$(common.parallel.formatDependency "$dependency" "$module_type" "$our_day_offset" "$iInvocation" "$nInvocations")"
							
							if [[ "$first" == true ]]
							then
								# First iteration
								resolved_dependencies="$resolved_dependency"
								first=false
							else
								resolved_dependencies="${resolved_dependencies} ${resolved_dependency}"
							fi
						done # invocations
					done # days
					
					echo ${resolved_dependencies}
					return $CXR_RET_OK
					;;
					
			esac
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
			if [[ $(common.hash.has? $CXR_ACTIVE_ALL_HASH $CXR_HASH_TYPE_GLOBAL "$dependency") == false ]]
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
			module_type=$(common.hash.get $CXR_MODULE_TYPE_HASH $CXR_HASH_TYPE_UNIVERSAL $dependency)
			
			nInvocations=$(common.module.getNumInvocations "$dependency")
			for iInvocation in $(seq 1 $nInvocations )
			do
				resolved_dependency="$(common.parallel.formatDependency "$dependency" "$module_type" "$day_offset" "$iInvocation" "$nInvocations")"
				
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
	local keyString="$(common.hash.getKeys $active_hash $CXR_HASH_TYPE_GLOBAL)"
	IFS="$CXR_DELIMITER"
	# Turn string into array (we cannot call <common.hash.getKeys> directly here!)
	local arrKeys=( $keyString )
	# Reset Internal Field separator
	IFS="$oIFS"
	
	# looping through keys (safest approach)
	for iKey in $( seq 0 $(( ${#arrKeys[@]} - 1)) )
	do
		module="${arrKeys[$iKey]}"
		main.log -v  "Found dependency on $module"
		
		# We ned the module type to know if we pass a day offset or not
		module_type=$(common.hash.get $CXR_MODULE_TYPE_HASH $CXR_HASH_TYPE_UNIVERSAL $module)
		
		nInvocations=$(common.module.getNumInvocations "$module")
		for iInvocation in $(seq 1 $nInvocations )
		do
			resolved_dependency="$(common.parallel.formatDependency "$module" "$module_type" "$day_offset" "$iInvocation" "$nInvocations")"
		
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
# Function: common.module.dependsOn?
#
# Returns true if the first argument depends on the second
#
# Parameters:
# $1 - name of a step like create_emissions1@1
# $2 - name of a step like create_emissions1@1
################################################################################
function common.module.dependsFirstOnSecond?()
################################################################################
{
	local first="$1"
	local second="$2"
	
	local first_module
	local second_module
	
	local first_dep_raw
	local first_dep
	
	local found=false
	
	# The question is if the first argument depends on the second, 
	# or in other words if the module name of the second appears in the expanded version of the 
	# dependencies of the first (and the day offset matches)
	
	common.module.parseIdentifier "$first"
	first_module=$_module_name
	first_offset=$_day_offset
	
	# Get the expanded dependencies of the first
	first_dep_raw=$(common.module.getRawDependencies "$first_module")
	first_dep=$(common.module.resolveAllDependencies "$first_dep_raw" "$first_offset")
	
	common.module.parseIdentifier "$second"
	second_module=$_module_name
	second_offset=$_day_offset
	
	for dep in $first_dep
	do
		common.module.parseIdentifier "$dep"
		
		if [[ "$_module_name" == "$second_module" && "$second_offset" == "$_day_offset" ]]
		then
			found=true
			break
		fi
		
	done
	
	echo $found
	
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
	local dependencies="$1"
	local day_offset="${2:-}"
	local dependency
	local resolved_dependency=""
	local resolved_dependencies=""
	local first=true
	
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

	local raw_dependencies="$1"
	local day_offset="${2:-}"
	local dep_day_offset
	local dep_module_name
	local dependency
	local my_stage
	local iInvocation
	local nInvocations
	
	
	if [[ ! "$raw_dependencies" ]]
	then
		echo true
		main.log -v "Dependency empty - OK"
		return $CXR_RET_OK
	fi
		
	main.log -v  "Evaluating dependencies on $raw_dependencies for day offset ${day_offset:-0}"
	
	# Resolve them
	local dependencies="$(common.module.resolveAllDependencies "$raw_dependencies" "$day_offset" )"
	
	main.log -v "Resolved dependencies: $dependencies"

	for dependency in $dependencies
	do
		# We need to parse the dependency
		# this sets a couple of _variables
		common.module.parseIdentifier "$dependency"
		
		dep_module_name="$_module_name"
		dep_day_offset="$_day_offset"
		dep_invocation="$_invocation"
		
		# If there is no offset set in to 0 (TODO: Check correctness!)
		if [[ -z "$(common.string.trim "$dep_day_offset")" ]]
		then
			dep_day_offset=0
		fi
		
		# Check if it is disabled
		if [[ $(common.hash.has? $CXR_ACTIVE_ALL_HASH $CXR_HASH_TYPE_GLOBAL "$dep_module_name") == false  ]]
		then
			# Do we care?
			if [[ "$CXR_IGNORE_DISABLED_DEPENDENCIES" == true ]]
			then
				# No, user wants to ignore this
				main.log  -w "You set CXR_IGNORE_DISABLED_DEPENDENCIES to true and $dep_module_name is disabled. We will not check if this module was run"
				# Next dependency
				continue
			else
				# Yes, we terminate
				main.dieGracefully "You set CXR_IGNORE_DISABLED_DEPENDENCIES to false and $dep_module_name is disabled. The dependency $dep_module_name cannot be fulfilled!"
			fi # disabled dependencies allowed?
		fi # dependency disabled?
		
		# Determine type
		module_type="$(common.module.getType "$dep_module_name")"
		
		# Convert date
		raw_date="$(common.date.toRaw $(common.date.OffsetToDate "${dep_day_offset}"))"
		
		my_stage="$(common.state.getStageName "$module_type" "$dep_module_name" "$raw_date" "$dep_invocation" )"
		
		# Is this known to have worked?
		if [[ "$(common.state.hasFinished? "$my_stage")" == true ]]
		then
			main.log -v "dependency ${dependency} fullfilled at $my_stage"
		else
			# dependency NOK, Find out why
			
			# Find out if dependency failed - if so, we crash
			if [[ "$(common.state.hasFailed? "$my_stage")" == true ]]
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
# Function: common.module.updateInfo
#
# Goes through all available modules for the current model and version, and collects 
# vital information in various hashes.
# For performance reasons, we check the hashes mtimes compared to the mtime of the configuration.
# If any USER_TEMP variable is set, we do it anyway.
# If a module name is non-unique, we fail.
#
# TODO: Make faster, also update CACHE_${item} hashes of common.module.getMetaField
# and Cache_Performance of common.performance.estimateRuntime
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

	main.log -a  "Updating module information, might take a while..."
	
	
	# Increase global indent level
	main.increaseLogIndent
	
	local iIteration
	local dirs
	local dir
	local types
	local type
	local hashes
	local active_hash
	local files
	local file
	local module_name
	local run_it
	local confMtime
	local HashMtime
	local cliCount
	
	# First check if the config changed after last update
	
	# Mtime of configfile. Strictly speaking we should look at all files of the hierarchy
	confMtime=$(common.fs.getMtime $CXR_CONFIG)
	# We use the path hash as representative hash
	HashMtime=$(common.hash.getMtime $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL)
	
	main.log -v "Mtime Conf: $confMtime Mtime Hash:$HashMtime"
	
	if [[ $confMtime -lt $HashMtime ]]
	then
		# Conf was not changed.
		# Test if user supplied any CLI-settings that change the active modules
		
		if [[ "${CXR_RUN_LIMITED_PROCESSING}" == false ]]
		then
			# User did not supply any settings
			main.log -a "It seems that module info is up-to-date. If not, touch $CXR_CONFIG and rerun"
			return $CXR_RET_OK
		fi
	fi
	
	# Create a few working arrays we will go through
	# Note that there are three kinds of installer modules: general ones, model specific and version specific ones
	# The same is true for common modules
	types=($CXR_TYPE_INSTALLER \
	       $CXR_TYPE_INSTALLER \
	       $CXR_TYPE_INSTALLER \
	       $CXR_TYPE_COMMON \
	       $CXR_TYPE_COMMON \
	       $CXR_TYPE_COMMON \
	       $CXR_TYPE_PREPROCESS_ONCE \
	       $CXR_TYPE_PREPROCESS_DAILY  \
	       $CXR_TYPE_MODEL \
	       $CXR_TYPE_POSTPROCESS_DAILY \
	       $CXR_TYPE_POSTPROCESS_ONCE)
	       
	dirs=($CXR_INSTALLER_INPUT_DIR \
	      $CXR_INSTALLER_MODEL_INPUT_DIR \
	      $CXR_INSTALLER_VERSION_INPUT_DIR \
	      $CXR_COMMON_INPUT_DIR \
	      $CXR_COMMON_MODEL_INPUT_DIR \
	      $CXR_COMMON_VERSION_INPUT_DIR \
	      $CXR_PREPROCESSOR_ONCE_INPUT_DIR \
	      $CXR_PREPROCESSOR_DAILY_INPUT_DIR \
	      $CXR_MODEL_INPUT_DIR \
	      $CXR_POSTPROCESSOR_DAILY_INPUT_DIR \
	      $CXR_POSTPROCESSOR_ONCE_INPUT_DIR)
	
	# we ignore any hash called "-"
	hashes=(- \
	        - \
	        - \
	        - \
	        - \
	        - \
	        $CXR_ACTIVE_ONCE_PRE_HASH \
	        $CXR_ACTIVE_DAILY_PRE_HASH \
	        $CXR_ACTIVE_MODEL_HASH \
	        $CXR_ACTIVE_DAILY_POST_HASH \
	        $CXR_ACTIVE_ONCE_POST_HASH)
	
	# Clear global hashes if we are alone
	if [[ ${CXR_ALLOW_MULTIPLE} == false ]]
	then
		common.hash.destroy $CXR_ACTIVE_ALL_HASH $CXR_HASH_TYPE_GLOBAL
		common.hash.destroy $CXR_ACTIVE_ONCE_PRE_HASH $CXR_HASH_TYPE_GLOBAL
		common.hash.destroy $CXR_ACTIVE_DAILY_PRE_HASH $CXR_HASH_TYPE_GLOBAL
		common.hash.destroy $CXR_ACTIVE_MODEL_HASH $CXR_HASH_TYPE_GLOBAL
		common.hash.destroy $CXR_ACTIVE_DAILY_POST_HASH $CXR_HASH_TYPE_GLOBAL
		common.hash.destroy $CXR_ACTIVE_ONCE_POST_HASH $CXR_HASH_TYPE_GLOBAL
	fi
	
	
	for iIteration in $(seq 0 $(( ${#dirs[@]} - 1 )) )
	do
		type=${types[$iIteration]}
		dir=${dirs[$iIteration]}
		active_hash=${hashes[$iIteration]}
		
		# The enabled and disabled strings ore lists - we cannot do the array trick...
		case "$type" in
	
			"${CXR_TYPE_COMMON}" ) 
				enabled_modules=""
				disabled_modules="";;
				
			"${CXR_TYPE_PREPROCESS_ONCE}" ) 
				enabled_modules="$CXR_ENABLED_ONCE_PREPROC"
				disabled_modules="$CXR_DISABLED_ONCE_PREPROC"
				;;
				
			"${CXR_TYPE_PREPROCESS_DAILY}" ) 
				enabled_modules="$CXR_ENABLED_DAILY_PREPROC"
				disabled_modules="$CXR_DISABLED_DAILY_PREPROC"
				;;
				
			"${CXR_TYPE_POSTPROCESS_DAILY}" ) 
				enabled_modules="$CXR_ENABLED_DAILY_POSTPROC"
				disabled_modules="$CXR_DISABLED_DAILY_POSTPROC"
				;;
				
			"${CXR_TYPE_POSTPROCESS_ONCE}" ) 
				enabled_modules="$CXR_ENABLED_ONCE_POSTPROC"
				disabled_modules="$CXR_DISABLED_ONCE_POSTPROC"
				;;
				
			"${CXR_TYPE_MODEL}" ) 
				enabled_modules="$CXR_ENABLED_MODEL"
				disabled_modules="$CXR_DISABLED_MODEL"
				;;
				
			"${CXR_TYPE_INSTALLER}" ) 
				enabled_modules="$CXR_ENABLED_INSTALLER"
				disabled_modules="$CXR_DISABLED_INSTALLER"
				;;
				
			* ) 
				main.dieGracefully "Unknown module type $type" ;;
		esac
		
		main.log -v "Adding $type modules..."
		
		if [[ -d "$dir" ]]
		then
			# Find all of them
			files="$(find "$dir" -noleaf -maxdepth 1 -name '*.sh')"

			for file in $files
			do
				# We are still alive...
				common.user.showProgress
				
				module_name="$(main.getModuleName $file)"
				main.log -v  "Adding module $module_name in $file"
				
				# Is there a new entry of this name? (this would indicate non-uniqueness!)
				if [[ $(common.hash.isNew? $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module_name) == true ]]
				then
					main.dieGracefully "There seem to be more than one module called ${module_name}. This is not allowed - please adjust the names!"
				fi
				
				# Path 
				common.hash.put $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module_name $file
				
				# Type
				common.hash.put $CXR_MODULE_TYPE_HASH $CXR_HASH_TYPE_UNIVERSAL $module_name $type
				
				# Is this module active? (Enabled wins over disabled)
				# if the module name is in the enabled list, run it,no matter what
				if [[ "$(common.string.isSubstringPresent? "$enabled_modules" "$module_name")" == true ]]
				then
					# Module was explicitly enabled
					run_it=true
				elif [[  "$(common.string.isSubstringPresent? "$disabled_modules" "$module_name")" == false && "${disabled_modules}" != "${CXR_SKIP_ALL}"   ]]
				then
					# Module was not explicitly disabled and we did not disable all
					run_it=true
				else
					# If the name of the module is in the disabled list, this should not be run (except if it is in the enabled list)
					run_it=false
					main.log -a "Module $module_name is disabled, skipped"
				fi
				
				if [[ "$run_it" == true ]]
				then
					# All Hash (value is dummy)
					common.hash.put $CXR_ACTIVE_ALL_HASH $CXR_HASH_TYPE_GLOBAL $module_name true
					
					if [[ "$active_hash" != - ]]
					then
						# The current types active hash
						common.hash.put $active_hash $CXR_HASH_TYPE_GLOBAL $module_name true
					fi
				fi
			
			done # Loop over files
			
		else
			main.log -w "Tried to add modules in $dir - directory not found. (May not be relevant)"
		fi # Directory exists?
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

	# This call sets _has and _value
	common.hash.has? "$CXR_MODULE_TYPE_HASH" $CXR_HASH_TYPE_UNIVERSAL "$name" > /dev/null
	if [[ "$_has" == true ]]
	then
		module_type="$_value"
		
		if [[ "$module_type" ]]
		then
			echo "$module_type"
		else
			main.dieGracefully "Could not find module type of $name!"
		fi
	else
		main.dieGracefully "Could not find module $name!"
	fi
	
}

################################################################################
# Function: common.module.runType
#	
# Calls all or just one single module at a specific 
# point in time (or One-Time)
# Here, a sequential approach is implied.
# If a module is enabled explicitly it is always used (wins over disabled), 
# If its not enabled but disabled, the module is not run.
# 
#
# Parameters:
# $1 - Type of modules to run
################################################################################
function common.module.runType()
################################################################################
{
	local module_type="$1"
	
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
	local iInvocation
	local nInvocations
	
	# Variables:
	# module_directories - is a list of directories that will be used to search for modules
	# enabled_modules - is a list of explicitly enables modules of the current type
	# disabled_modules - is a list of disabled modules of the current type
	case "$module_type" in
	
		"${CXR_TYPE_COMMON}" ) 
			main.dieGracefully "Common modules cannot be run this way!" ;;
			
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
			main.dieGracefully "Unknown module type $module_type" ;;

	esac
	
	# Increase global indent level
	main.increaseLogIndent

	# Check if we need any of them at all
	# If the user wants to run a specific module, we enter anyway
	if [[ ! ( "${enabled_modules}" == "" && "${disabled_modules}" == "${CXR_SKIP_ALL}" ) ]]
	then
	
		# We did not turn off everything
	
		# Loop through available input dirs
		for module_directory in $module_directories
		do
			main.log  "Loading $module_type modules from $module_directory..."
		
			for function_file in $(ls ${module_directory}/??_*.sh 2>/dev/null)
			do
				# Check if we are still happy if needed
				if [[ "${check_continue}" == true  ]]
				then
					common.state.doContinue? || main.dieGracefully "Continue file no longer present."
				fi
				
				FILE_NAME=$(basename "$function_file")
				
				# Before loading a new module, remove old meta variables
				unset ${!CXR_META_MODULE*}
				
				# Export the module name
				CXR_META_MODULE_NAME=$(main.getModuleName $function_file)
				
				# First source the file to get the meta info
				source $function_file
				
				# Check if we must run this
				# if the module name is in the enabled list, run it,no matter what
				if [[ "$(common.string.isSubstringPresent? "$enabled_modules" "$CXR_META_MODULE_NAME")" == true ]]
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
					main.log -v "Step $FILE_NAME is disabled, skipped"
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
						# Loop through all invocations
						nInvocations=$(common.module.getNumInvocations "$CXR_META_MODULE_NAME")
						for iInvocation in $(seq 1 $nInvocations )
						do
							# RUNNING IT
							main.log -v "Starting Module $CXR_META_MODULE_NAME"
							"$CXR_META_MODULE_NAME" $iInvocation || ret_val=$CXR_RET_ERROR
						done # invocations
					else
						main.log  "Version check for $CXR_META_MODULE_NAME failed. Either change the values in the head of the module or manipulate the revision numbers of either CAMxRunner.sh or the configuration.\nModule skipped."
					fi

					# Take note that this module was already announced
					CXR_ANNOUNCED_MODULES="${CXR_ANNOUNCED_MODULES} ${CXR_META_MODULE_NAME}"
						
					# Decrease global indent level
					main.decreaseLogIndent
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
	
	if [[ ${CXR_RUN_PRE_ONCE} == true ]]
	then
		common.module.runType ${CXR_TYPE_PREPROCESS_ONCE} || ret_val=$CXR_RET_ERROR
	else
		main.log -a "We do not run ${CXR_TYPE_PREPROCESS_ONCE} modules."
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
				common.module.runType ${CXR_TYPE_PREPROCESS_DAILY} || ret_val=$CXR_RET_ERROR
			else
				main.log -a "We do not run ${CXR_TYPE_PREPROCESS_DAILY} modules."
			fi
			
			if [[ ${CXR_RUN_MODEL} == true  ]]
			then
				common.module.runType ${CXR_TYPE_MODEL} || ret_val=$CXR_RET_ERROR
			else
				main.log -a "We do not run ${CXR_TYPE_MODEL} modules."
			fi
			
			if [[ ${CXR_RUN_POST_DAILY} == true  ]]
			then
				common.module.runType ${CXR_TYPE_POSTPROCESS_DAILY} || ret_val=$CXR_RET_ERROR
			else
				main.log -a "We do not run ${CXR_TYPE_POSTPROCESS_DAILY} modules."
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
		common.module.runType ${CXR_TYPE_POSTPROCESS_ONCE} || ret_val=$CXR_RET_ERROR
	else
		main.log -a "We do not run ${CXR_TYPE_POSTPROCESS_ONCE} modules."
	fi
	
	return $ret_val
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
	# TODO: How do model and installer modules fit in here?
	
	local module_type="$1"
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
		pattern="${module_directory}/??_*.sh"
		
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
	
		main.log -i "\n  These $module_type modules are disabled physically (if any) - to run them, remove the .${CXR_DISABLED_EXT} in the filename:\n"
		
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
		main.log -a -B "CAMxRunner.sh" "\n  These modules are available for $CXR_MODEL $CXR_MODEL_VERSION.\n  All of these can be combined in one single -r"" statement."
		
		main.log -a "CAMxRunner.sh" "\n  One-Time pre-processing:\n"
		
		# module_functions.sh
		common.module.listModuleType ${CXR_TYPE_PREPROCESS_ONCE}
		
		main.log -a "CAMxRunner.sh" "\n  Daily pre-processing:\n"
		
		# module_functions.sh
		common.module.listModuleType ${CXR_TYPE_PREPROCESS_DAILY}
		
		main.log -a "CAMxRunner.sh" "\n  Model:\n"
		
		# module_functions.sh
		common.module.listModuleType ${CXR_TYPE_MODEL}
		
		main.log -a "CAMxRunner.sh" "\n  Daily post-processing:\n"
		
		# module_functions.sh
		common.module.listModuleType ${CXR_TYPE_POSTPROCESS_DAILY}
		
		main.log -a "CAMxRunner.sh" "\n  One-Time post-processing steps:\n"
		
		# module_functions.sh
		common.module.listModuleType ${CXR_TYPE_POSTPROCESS_ONCE}
		
		main.log -a "CAMxRunner.sh" "\n  To disable single modules, you can add the name of a module you do *not* want to run to either of the lists\n CXR_DISABLED_DAILY_PREPROC,\n CXR_DISABLED_ONCE_PREPROC,\n CXR_DISABLED_DAILY_POSTPROC or\n CXR_DISABLED_ONCE_POSTPROC\nin your configuration file. Setting any of these strings to \"${CXR_SKIP_ALL}\" disables all modules of this class."    
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
	is "$_module_name" convert_emissions "common.module.parseIdentifier only module - name"
	is "$_day_offset" "" "common.module.parseIdentifier only module - day offset"
	is "$_invocation" "" "common.module.parseIdentifier only module - invocation"
	
	common.module.parseIdentifier some_module0
	is "$_module_name" some_module "common.module.parseIdentifier module and day offset - name"
	is "$_day_offset" 0 "common.module.parseIdentifier module and day offset - day offset"
	is "$_invocation" "" "common.module.parseIdentifier module and day offset - invocation"
	
	common.module.parseIdentifier differentcase@2
	is "$_module_name" differentcase "common.module.parseIdentifier module and invocation - name"
	is "$_day_offset" "" "common.module.parseIdentifier module and invocation - day offset"
	is "$_invocation" 2 "common.module.parseIdentifier module and invocation - invocation"
	
	common.module.parseIdentifier ThatsNowAll3@1
	is "$_module_name" ThatsNowAll "common.module.parseIdentifier complete - name"
	is "$_day_offset" 3 "common.module.parseIdentifier complete - day offset"
	is "$_invocation" 1 "common.module.parseIdentifier complete - invocation"
	
	common.module.parseIdentifier ThatsNowAll123@24
	is "$_module_name" ThatsNowAll "common.module.parseIdentifier multidigit - name"
	is "$_day_offset" 123 "common.module.parseIdentifier multidigit - day offset"
	is "$_invocation" 24 "common.module.parseIdentifier multidigit - invocation"
	
	is $(common.module.getType boundary_conditions) ${CXR_TYPE_PREPROCESS_DAILY} "common.module.getType boundary_conditions"
	is $(common.module.getExclusive model) true "common.module.getExclusive model"
	
	# Testing the resolvers for dependencies
	is "$(common.module.resolveSingleDependency "model-" 0)" "" "common.module.resolveSingleDependency minus on first day"
	is "$(common.module.resolveSingleDependency "model-" 1)" "model0" "common.module.resolveSingleDependency minus on second day"
	
	is "$(common.module.resolveAllDependencies "model- create_emissions" 1)" "model0 create_emissions1@1 create_emissions1@2 create_emissions1@3" "common.module.resolveAllDependencies on second day"
	
	
	########################################
	# teardown tests if needed
	########################################
	
}
