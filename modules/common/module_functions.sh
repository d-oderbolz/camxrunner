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
CXR_META_MODULE_NUM_TESTS=1

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
# Function: common.module.areDependenciesOk?
#
# Checks if all dependencies of a given task dependencies are fullfilled. 
# If any dependency has failed, the run is destroyed, 
# any depdendency was not yet started, false is returned,
# all are CXR_STATUS_SUCCESS, true is returned.
#
# Parameters:
# $1 - module name for which to check
# $2 - day_offset for which to check
################################################################################
function common.module.areDependenciesOk?()
################################################################################
{
	if [[ "$CXR_IGNORE_ANY_DEPENDENCIES" == true ]]
	then
		main.log  "You set CXR_IGNORE_ANY_DEPENDENCIES to true. We will not check dependencies (pretty dangerous...)"
		echo true
		return $CXR_RET_OK
	fi
	
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a module name and a day offset as input, got $*"
	fi
	
	local module
	local day_offset
	local disabled_modules
	local unfinishedCount
	local failedCount
	
	module="$1"
	day_offset="${2}"
	
	
	main.log -v "Evaluating if all tasks $module depends on for offset $day_offset are done."
	
	# Find out if any of the dependencies is disabled
	
	disabled_modules=$(common.db.getResultSet "$CXR_STATE_DB_FILE" - <<-EOT
	
	SELECT d.independent_module
	FROM dependencies d, modules m
	WHERE 
	      m.module = d.independent_module
	AND   m.active='false'
	AND   d.dependent_module='$module'
	AND   d.dependent_day_offset=$day_offset;
	
	EOT
	)
	
	if [[ "$disabled_modules" && "$CXR_IGNORE_DISABLED_DEPENDENCIES" == false ]]
	then
		main.dieGracefully "There are dependencies to disabled modules: $disabled_modules"
	fi
	
	# If any dependency has failed, we stop the run
	failedCount=$(common.db.getResultSet "$CXR_STATE_DB_FILE" - <<-EOT
	
	SELECT COUNT(*)
	FROM dependencies d, tasks t
	WHERE 
	      t.module = d.independent_module
	AND   t.status='$CXR_STATUS_FAILURE'
	AND   d.dependent_module='$module'
	AND   d.dependent_day_offset=$day_offset;
	
	EOT
	)
	
	if [[ $failedCount -gt 0 ]]
	then
		main.dieGracefully "$failedCount tasks that $* depends on have failed!"
	fi
	
	# If more that 0 taks are non-successful, we return false
	unfinishedCount=$(common.db.getResultSet "$CXR_STATE_DB_FILE" - <<-EOT
	
	SELECT COUNT(*)
	FROM dependencies d, tasks t
	WHERE 
	      t.module = d.independent_module
	AND   t.status IS NOT '$CXR_STATUS_SUCCESS'
	AND   d.dependent_module='$module'
	AND   d.dependent_day_offset=$day_offset;
	
	EOT
	)
	
	if [[ $unfinishedCount -gt 0 ]]
	then
		# Still unfinished stuff
		echo false
	else
		# all OK
		echo true
	fi
}

################################################################################
# Function: common.module.getNumInvocations
# 
# For a given module name, returns the value of getNumInvocations
# Caveat: This function modifies the environment - always call like this:
# > b=$(common.module.getNumInvocations "$module")
# The $() construct opens a subshell.
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
	path=$(common.db.getResultSet "$CXR_STATE_DB_FILE" "SELECT path FROM modules WHERE module='$1'")
	echo "$path"
}

################################################################################
# Function: common.module.getType
# 
# For a given module name, returns the type of the module.
# Needs a fully updated CXR_STATE_DB_FILE!
#
# Parameters:
# $1 - name of a module
################################################################################
function common.module.getType()
################################################################################
{
	type=$(common.db.getResultSet "$CXR_STATE_DB_FILE" "SELECT type FROM modules WHERE module='$1'")
	echo "$type"
}

################################################################################
# Function: common.module.getTypeSlow
# 
# For a given module name, returns the type of the module. Slower than 
# <common.module.getType> because the file itself is queried, but no need
# for a DB. Similar code is used in <common.state.updateInfo>.
# Only use this function if needed.
#
# Parameters:
# $1 - name of a module
################################################################################
function common.module.getTypeSlow()
################################################################################
{
	local metafield
	local value
	local file
	
	# We ASSUME that each instance of this file is of the same type
	file="$(find $CXR_RUN_DIR -noleaf -name ${1}.sh | head -n1)"
	
	metafield=$(grep '^[[:space:]]\{0,\}CXR_META_MODULE_TYPE=.*' $file)
	value="$(expr match "$metafield" '.*=\(.*\)')" || :
	# Do expansion
	value="$(eval "echo $(echo "$value")")"
	
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
	count=$(common.db.getResultSet "$CXR_STATE_DB_FILE" "SELECT COUNT(*) FROM modules WHERE module='$1'")
	
	if [[ $count -eq 1 ]]
	then
		echo true
	else
		echo false
	fi
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
	
	is $(common.module.getType boundary_conditions) ${CXR_TYPE_PREPROCESS_DAILY} "common.module.getType boundary_conditions"

	########################################
	# teardown tests if needed
	########################################
	
}
