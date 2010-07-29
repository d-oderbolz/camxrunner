# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
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
# TODO: rewrite using sqlite
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=2

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

################################################################################
# Function: common.state.isRepeatedRun?
#
# Returns true, if this run was already started earlier (basically a check if the 
# state directory is empty)
# 
################################################################################
function common.state.isRepeatedRun?()
################################################################################
{
	local count
	
	count=$(find ${CXR_STATE_DIR} -maxdepth 1 -noleaf -type f 2>/dev/null | wc -l)
	
	main.log -v  "File count in state directory: $count"
	
	if [[ "$count" -gt 0  ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.state.getLastDayModelled
#
# If the run was already done, returns the last day done in ISO Format. If it was not done, 
# returns the empty string.
# 
################################################################################
function common.state.getLastDayModelled()
################################################################################
{
	local max_day
	local date
	
	if [[ $(common.state.isRepeatedRun?) == true  ]]
	then
		# A similar expression is used in common.state.cleanup
		max_day=$(find ${CXR_STATE_DIR} -noleaf -type f 2>/dev/null | xargs -i basename \{\} |  grep -o '^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]' - | sort | uniq | tail -n1)
		
		if [[ "$max_day" ]]
		then
			# Convert to ISO
			date=$(common.date.toISO "$max_day")
		
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
# Function: common.state.getFirstDayModelled
#
# If the run was already done, returns the first day done in ISO Format. If it was not done, 
# returns the empty string.
# 
################################################################################
function common.state.getFirstDayModelled()
################################################################################
{
	local min_day
	local date
	
	if [[ $(common.state.isRepeatedRun?) == true  ]]
	then
		# A similar expression is used in common.state.cleanup
		max_day=$(find ${CXR_STATE_DIR} -noleaf -type f 2>/dev/null | xargs -i basename \{\} |  grep -o '^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]' - | sort | uniq | head -n1)
		
		if [[ "$min_day" ]]
		then
			# Convert to ISO
			date=$(common.date.toISO "$min_day")
		
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
# Function: common.state.getStageName
#
# Generates the name of the current stage from the parameters.
# Names are dependent on the type of module.
# Names have the form
# ${CXR_DATE_RAW}@${CXR_META_MODULE_TYPE}@${CXR_META_MODULE_NAME}@${CXR_INVOCATION}
# We choose names that sort more or less nicely.
# 
# Parameters:
# $1 - module type
# $2 - module name
# $3 - raw date (may be empty, depending on module type)
# $4 - invocation (may be empty)
################################################################################
function common.state.getStageName()
################################################################################
{
	local module_type
	local module
	local date
	local invocation
	
	if [[ $# -lt 3 ]]
	then
		# Use the environment
		module_type="${CXR_META_MODULE_TYPE}"
		module="${CXR_META_MODULE_NAME}"
		date="${CXR_DATE_RAW:-date_not_set}"
		invocation="${CXR_INVOCATION:-1}"
	else
		# Use parameters
		module_type="${1}"
		module="${2}"
		date="${3}"
		invocation="${4:-1}"
	fi
	
	case "${module_type}" in
		${CXR_TYPE_COMMON}) 
			# A common module normally does not need this, but who knows?
			echo ${date}@${module_type}@${module}@${invocation} ;; 
		
		${CXR_TYPE_PREPROCESS_DAILY}) 
			echo ${date}@${module_type}@${module}@${invocation} ;;
			
		${CXR_TYPE_PREPROCESS_ONCE}) 
			echo _@${module_type}@${module}@${invocation} ;;
			
		${CXR_TYPE_POSTPROCESS_DAILY}) 
			echo ${date}@${module_type}@${module}@${invocation} ;;
		
		${CXR_TYPE_POSTPROCESS_ONCE}) 
			echo ZZ_@${module_type}@${module}@${invocation} ;;
			
		${CXR_TYPE_MODEL} ) 
			echo ${date}@${module_type}@${module}@${invocation} ;;
			
		${CXR_TYPE_INSTALLER}) 
			echo ${module_type}@${module}@${invocation} ;;
			
	 *) main.dieGracefully "Unknown module type ${module_type}";;
	esac
}

################################################################################
# Function: common.state.deleteContinueFiles
#
# Deletes the continue files of all instances
################################################################################
function common.state.deleteContinueFiles()
################################################################################
{
	main.log -w  "The continue files of all instances of this run will be deleted now!"
	
	find ${CXR_ALL_INSTANCES_DIR} -noleaf -name ${CXR_CONTINUE} -exec rm -f {} \;
}

################################################################################
# Function: common.state.updateInfo
#
# Goes through all available modules for the current model and version, and collects 
# vital information in the state DB.
# 
# tables that are persistent:
# - tasks
# - days 
# - installed
# - types (constant)
#
# tables that contain ony whats relevant currently ("active")
# - modules
# - metadata
# (also some others, but they are taken care of in <common.task.init>)
################################################################################
function common.state.updateInfo()
################################################################################
{
	main.log -a "Collecting module information, might take a while..."
	
	# Increase global indent level
	main.increaseLogIndent
	
	local iIteration
	local dirs
	local dir
	local types
	local type
	local files
	local file
	local module
	local run_it
	local metafiled
	local list
	local field
	local value
	
	# Only update info if we are not a slave
	if [[	${CXR_ALLOW_MULTIPLE} == true && \
			"$(common.state.countInstances)" -gt 1 ]]
	then
		# We are in a non-master multiple runner
		main.log -a -b "This is a slave process - we will not collect new information"
	else
		
		main.log -a "Cleaning Non-Persistent tables..."
		
		${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" <<-EOT
		
			DELETE FROM modules;
			DELETE FROM metadata;
		
		EOT
	
		# Create a few working arrays we will go through
		types=($CXR_TYPE_PREPROCESS_ONCE \
		       $CXR_TYPE_PREPROCESS_DAILY \
		       $CXR_TYPE_MODEL \
		       $CXR_TYPE_POSTPROCESS_DAILY \
		       $CXR_TYPE_POSTPROCESS_ONCE)
		       
		dirs=($CXR_PREPROCESSOR_ONCE_INPUT_DIR \
		      $CXR_PREPROCESSOR_DAILY_INPUT_DIR \
		      $CXR_MODEL_INPUT_DIR \
		      $CXR_POSTPROCESSOR_DAILY_INPUT_DIR \
		      $CXR_POSTPROCESSOR_ONCE_INPUT_DIR)
		
		for iIteration in $(seq 0 $(( ${#dirs[@]} - 1 )) )
		do
			type=${types[$iIteration]}
			dir=${dirs[$iIteration]}
			
			# The enabled and disabled strings are lists
			case "$type" in
				"${CXR_TYPE_PREPROCESS_ONCE}" )
					enabled_modules="$CXR_ENABLED_ONCE_PREPROC"
					disabled_modules="$CXR_DISABLED_ONCE_PREPROC"
					;;
					
				"${CXR_TYPE_PREPROCESS_DAILY}" )
					enabled_modules="$CXR_ENABLED_DAILY_PREPROC"
					disabled_modules="$CXR_DISABLED_DAILY_PREPROC"
					;;
					
				"${CXR_TYPE_MODEL}" )
					enabled_modules="$CXR_ENABLED_MODEL"
					disabled_modules="$CXR_DISABLED_MODEL"
					;;
					
				"${CXR_TYPE_POSTPROCESS_DAILY}" )
					enabled_modules="$CXR_ENABLED_DAILY_POSTPROC"
					disabled_modules="$CXR_DISABLED_DAILY_POSTPROC"
					;;
					
				"${CXR_TYPE_POSTPROCESS_ONCE}" )
					enabled_modules="$CXR_ENABLED_ONCE_POSTPROC"
					disabled_modules="$CXR_DISABLED_ONCE_POSTPROC"
					;;
					
	
				* ) 
					main.dieGracefully "The module type $type is not relevant here" ;;
			esac
			
			main.log -v "Adding $type modules..."
			
			if [[ -d "$dir" ]]
			then
				# Find all *.sh files in this dir (no descent!)
				files="$(find "$dir" -noleaf -maxdepth 1 -name '*.sh')"
	
				for file in $files
				do
					# We are still alive...
					common.user.showProgress
					
					module="$(main.getModuleName $file)"
					
					# Is this module active? (Enabled wins over disabled)
					# if the module name is in the enabled list, run it,no matter what
					if [[ "$(main.isSubstringPresent? "$enabled_modules" "$module")" == true ]]
					then
						# Module was explicitly enabled
						run_it=true
					elif [[  "$(main.isSubstringPresent? "$disabled_modules" "$module")" == false && "${disabled_modules}" != "${CXR_SKIP_ALL}" ]]
					then
						# Module was not explicitly disabled and we did not disable all
						run_it=true
					else
						# If the name of the module is in the disabled list, this should not be run (except if it is in the enabled list)
						run_it=false
						main.log -a "Module $module is disabled, skipped"
					fi
					
					if [[ "$run_it" == true ]]
					then
						# Like mentioned above, only active stuff is added.
						# Other things are not needed.
						# Add $file, $module and $type to DB
						${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "INSERT INTO modules (module,type,path) VALUES ('$module','$type','$file')"
					
						# Add metadata
						# grep the CXR_META_ vars that are not commented out
						list=$(grep '^[[:space:]]\{0,\}CXR_META_[_A-Z]\{1,\}=.*' $file)
						
						oIFS="$IFS"
						# Set IFS to newline
						IFS='
	'
						for metafield in $list
						do
							# Parse this
							# Field is to the left of the = sign
							field="$(expr match "$metafield" '\([_A-Z]\{1,\}\)=')"
							# the value is to the right (test quoting!!)
							value="$(expr match "$metafield" '.*=\(.*\)')"
							
							${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "INSERT INTO metadata (module,field,value) VALUES ('$module','$field','$value')"
						done
						
						IFS="$oIFS"
						
					fi
				done # Loop over files
			else
				main.dieGracefully "Tried to add modules in $dir - directory not found."
			fi # Directory exists?
		done # loop over type-index
		
		# decrease global indent level
		main.decreaseLogIndent
	
	fi
	
	exit
}

################################################################################
# Function: common.state.init
#
# Creates the state database for the current run
################################################################################
function common.state.init()
################################################################################
{
	if [[ -z "${CXR_STATE_DIR}" ]]
	then
		main.dieGracefully  "CXR_STATE_DIR not set!"
	fi
	
	# Not started yet - create state dir first
	if [[ ! -d "${CXR_STATE_DIR}" ]]
	then
		mkdir -p "${CXR_STATE_DIR}"
	fi
	
	# Create the global dirs
	mkdir -p "${CXR_GLOBAL_DIR}"
	
	# Create any instance dirs
	mkdir -p "${CXR_INSTANCE_DIR}"
	
	##################
	# Init a few Hashes
	##################
	# Contains the cache for MD5 hashes, it is shared among all runs in this installation
	common.hash.init MD5 $CXR_HASH_TYPE_UNIVERSAL
	
	# Stores Timing information
	common.hash.init Timing $CXR_HASH_TYPE_UNIVERSAL
	
	# In this hash, we store files that where decompressed (for all instances)
	# Implies that all instances see the same CXR_TEMP_DIR
	common.hash.init $CXR_GLOBAL_HASH_DECOMPRESSED_FILES $CXR_HASH_TYPE_GLOBAL
	
	# In this hash, we store all output files that have been generated
	common.hash.init $CXR_INSTANCE_HASH_OUTPUT_FILES $CXR_HASH_TYPE_INSTANCE
	
	# In this hash, we store dummy files of a dry run.
	common.hash.init $CXR_INSTANCE_HASH_DUMMY_FILES $CXR_HASH_TYPE_INSTANCE
	
	# In this hash, we store temporay files of a run. 
	common.hash.init $CXR_INSTANCE_HASH_TEMP_FILES $CXR_HASH_TYPE_INSTANCE
	
	# Creating .continue file
	echo "Creating the file ${CXR_CONTINUE_FILE}. If this file is deleted, the process  stops at the next possible stage" 1>&2
	echo "If you remove this file, the instance $$ on $(uname -n) will stop" > ${CXR_CONTINUE_FILE}
	
	main.log -v "Creating database schema..."
	
	${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" <<-EOT
	-- Get exclusive access
	PRAGMA main.locking_mode=EXCLUSIVE; 
	
	-- Here we store all installed stuff (just for installer)
	CREATE TABLE IF NOT EXISTS installed (item,
	                                      model,
	                                      model_version);
	
	-- Here we store all known simulation days
	CREATE TABLE IF NOT EXISTS days (day_offset,
	                                 day_iso);
	
	-- These are the days we currently plan to run on
	CREATE TABLE IF NOT EXISTS plan_days (day_offset,
	                                      day_iso);

	-- All modules go here
	CREATE TABLE IF NOT EXISTS modules (module, 
	                                    type,
	                                    path);
	CREATE UNIQUE INDEX IF NOT EXISTS module_idx ON modules(module);

	-- List of module types
	CREATE TABLE IF NOT EXISTS types (type);
	CREATE UNIQUE INDEX IF NOT EXISTS type_idx ON types(type);
	
	-- Storage of module metadata
	CREATE TABLE IF NOT EXISTS metadata (module,
	                                     field,
	                                     value);

	-- Table to store dependencies
	CREATE TABLE IF NOT EXISTS dependencies (independent_module, 
	                                         independent_day_offset, 
	                                         independent_invocation, 
	                                         dependent_module, 
	                                         dependent_day_offset, 
	                                         dependent_invocation);
	
	-- Table for all tasks comprising a run (static)
	CREATE TABLE IF NOT EXISTS tasks (module,
	                                 module_type,
	                                 exclusive,
	                                 day_offset,
	                                 invocation,
	                                 status,
	                                 epoch_m);
	                                 
	-- Table for all steps we need to take, ordered
	CREATE TABLE IF NOT EXISTS plan (id,
	                                 module,
	                                 module_type,
	                                 exclusive,
	                                 day_offset,
	                                 invocation,
	                                 status,
	                                 epoch_m);
	
	-- Table for workers
	CREATE TABLE IF NOT EXISTS workers (pid,
	                                    hostname,
	                                    status,
	                                    current_task,
	                                    epoch_m);
	
	PRAGMA main.locking_mode=NORMAL; 
	
	EOT
	
	# Update the module path hash and form the lists of active modules
	common.state.updateInfo
}

################################################################################
# Function: common.state.storeState
#
# Stores the current state of a run, can be either $CXR_STATE_START, $CXR_STATE_STOP or $CXR_STATE_ERROR.
# In the case of ERROR, we will take note of the fact.
#
# If a stage starts, checks if this stage was already executed.
# The name of the stage is determined via <common.state.getStageName> if it is not passed in.
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
function common.state.storeState()
################################################################################
{
	
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully "needs a state like $CXR_STATE_ERROR as Input"   
	fi
	
	local state
	local stage
	
	state=$1
	# stage is either passed or set using common.state.getStageName
	stage="${2:-$(common.state.getStageName)}"
	
	# Do we care at all?
	# Set CXR_ENABLE_STATE_DB to false in tests etc.
	if [[ "$CXR_ENABLE_STATE_DB" == false  ]]
	then
		main.log -v   "You disabled the state DB (CXR_ENABLE_STATE_DB=false), new state will not be stored."
		echo true
		return $CXR_RET_OK
	fi
	
	case "$state" in
	
		"$CXR_STATE_START") 
			# Check if this was already started
			if [[ $(common.state.hasFinished? "$stage") == true  ]]
			then
				
				if [[ "$CXR_RUN_LIMITED_PROCESSING" == true  ]]
				then
					# Ran already, but user wants to run specifically this
					main.log -w  "stage $stage was already started, but since you requested this specific module, we run it. If it fails try to run \n \t ${CXR_CALL} -F \n to remove existing output files."
					echo true
				else
					# Oops, this stage was already started	
					main.log -w  "stage $stage was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
					
					# false means already run
					echo false
					return $CXR_RET_ALREADY_RUN
				fi 
			else
				echo true
			fi
			;;
	
			"$CXR_STATE_STOP")
				main.log "stage $stage successfully completed."
				echo true
				;;
	
			"$CXR_STATE_ERROR")
				CXR_STATUS=$CXR_STATUS_FAILURE
				main.log -e "An error has occured during the execution of $stage!"
				echo false
			;;
			
			*)
				CXR_STATUS=$CXR_STATUS_FAILURE
				main.dieGracefully "Unknown state $state given"
				echo false
			;;
	esac
	
	# Touch your state file
	touch "$(_common.state.getStateFileName "$state" "$stage")" 
	return $CXR_RET_OK
}

################################################################################
# Function: _common.state.getStateFileName
#
# Returns:
# The start/stop/error filename of a stage
#
# Parameters:
# $1 - state
# $2 - stage 
################################################################################
function _common.state.getStateFileName()
################################################################################
{
	if [[ $# -ne 2  ]]
	then
		main.dieGracefully "needs a state and a stage as Input" 
	fi
	
	local state
	local stage
	
	state=$1
	stage=$2
	
	echo ${CXR_STATE_DIR}/${stage}.${state}	
}

################################################################################
# Function: common.state.countInstances
#
# Counts living CAMxRunners by checking the continue files
# TODO: Local - check ps, Remote - check .continue files (mtime)
#
################################################################################
function common.state.countInstances()
################################################################################
{
	find "${CXR_ALL_INSTANCES_DIR}" -noleaf -name ${CXR_CONTINUE} 2>/dev/null | wc -l
}

################################################################################
# Function: common.state.detectInstances
#
# Check if there are still living processes by checking the continue files
# TODO: Local - check ps, Remote - check .continue files (mtime)
#
################################################################################
function common.state.detectInstances()
################################################################################
{
	local process_count
	process_count=$(common.state.countInstances) 
	
	if [[ ${process_count} -ne 0 && ${CXR_ALLOW_MULTIPLE} == false ]]
	then
		# There are other processes running and this is not allowed
		main.log -e   "Found other instances - maybe these processes died or they are still running:\n(Check their age!)"
		
		find "${CXR_ALL_INSTANCES_DIR}" -noleaf -type d -maxdepth 1 2>/dev/null | tee -a ${CXR_LOG}
		
		main.log -e "Check manually if the processes still run, if not clean the state db by runnig \n\t ${CXR_CALL} -c \n or (experts only) you can run your instance anyway using \n \t ${CXR_RUN} -m [options]"    
		
		main.dieGracefully "Process stopped"
	fi
}

################################################################################
# Function: common.state.hasFinished?
#	
# Check if a specific stage has finished.
#
# Parameters:	
# $1 - stage to test
################################################################################
function common.state.hasFinished?()
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "needs a state and a stage as Input" 
		echo false
	fi
	
	local stage
	local start_file
	local stop_file
	
	stage="$1"
	start_file=$(_common.state.getStateFileName "${CXR_STATE_START}" "${stage}")
	stop_file=$(_common.state.getStateFileName "${CXR_STATE_STOP}" "${stage}")
	
	main.log -v "Testing stage ${stage}, looking for ${start_file} and ${stop_file}"
	
	if [[ -f "$stop_file"  ]]
	then
	
		if [[ -f "$start_file"  ]]
		then
			main.log -v  "Found a START file for ${stage}"
		fi
		
		echo true
	else
	
		# There is no stop file. Still there might be a start file:
		if [[ -f "$start_file"  ]]
		then
			main.log -w  "The stage ${stage} was started, but did not (yet) finish."
		fi
	
		echo false
	fi
}

################################################################################
# Function: common.state.hasFailed?
#	
# Check if a specific stage has failed. We do not consider if we have a start file,
# we check if we have an error file.
#
# Parameters:	
# $1 - stage to test
################################################################################
function common.state.hasFailed?()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully "needs a state and a stage as Input" 
		echo false
	fi
	
	local stage
	local error_file
	
	stage="$1"
	error_file=$(_common.state.getStateFileName "${CXR_STATE_ERROR}" "${stage}")
	
	if [[ -f "$error_file"  ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.state.cleanup
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
function common.state.cleanup()
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
	local message
	local start_offset
	local stop_offset
	local iOffset
	local current_date
	
	message="Do you want to change the state database?"
	
	while [[ "$(common.user.getOK "$message" )" == true ]]
	do
		# Fix the message
		message="Do you want to further change the state database?"
		
		# what do you want?
		what=$(common.user.getMenuChoice "Which part of the state database do you want to clean (none exits this function)?\nNote that you might need to delete output files in order to repeat a run, or run with ${CXR_CALL} -F (overwrite existing files)" "all-non-tasks existing-instances specific tasks none" "none")
		
		case "$what" in 
		
			all-non-tasks)
				main.log -w  "The following files will be deleted:"
					
				find ${CXR_STATE_DIR} -noleaf -maxdepth 1 -type f | xargs -i basename \{\}
		
				# Do we do this?
				if [[ "$(common.user.getOK "Do you really want to delete these files?" )" == false  ]]
				then
					# No 
					main.log -a "Will not delete any state information"
					return 0
				else
					# Yes
					rm -rf ${CXR_STATE_DIR}/* 2>/dev/null
					main.log -a "Done."
				fi #Delete?
				
				;; # all-non-tasks
			
			existing-instances)
			
				main.log -w  "The following directories and files therein will be deleted:"
					
				find ${CXR_ALL_INSTANCES_DIR} -noleaf -type d | xargs -i basename \{\}
		
				# Do we do this?
				if [[ "$(common.user.getOK "Do you really want to delete these files?" )" == false  ]]
				then
					# No 
					main.log -a   "Will not delete any state information"
					return 0
				else
					# Yes
					rm -rf ${CXR_ALL_INSTANCES_DIR}/* 2>/dev/null
					main.log -a "Done."
				fi #Delete?
			
				;; # existing-instances
					
			specific)
			
				# Possibilities:
				# one day 
				# one step
				# both
				
				what_detail="$(common.user.getMenuChoice "You have 3 options: (1) we delete all state information about one specific day , (2) about one specific step or (3) a combination thereof (none exits this function)?" "day step both none" "none")"
				
				case "$what_detail" in
				
					day)
						
						# To enumerate all days, we use a little grep-magic, also we add none at the end
						# The basename is needed to strip off the path, because the pattern starts with ^
						days="$(find ${CXR_STATE_DIR} -noleaf -type f | xargs -i basename \{\} |  grep -o '^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]' - | sort | uniq ) none"
						
						which_day="$(common.user.getMenuChoice "Which days state information should be deleted  (none exits this function)?" "$days" "none" )"
						
						# If this is true, we delete until the end
						following_days="$(common.user.getOK "Do you want to delete also data of days following this one?" )"
						
						if [[ $which_day == none ]]
						then
							main.log -w "Will not delete any state information" 
							return 0
						fi
						
						start_offset=$(common.date.toOffset $(common.date.toISO ${which_day}))
								
						if [[ "$following_days" == true ]]
						then
							stop_offset=$((${CXR_NUMBER_OF_SIM_DAYS} -1))
						else
							stop_offset=$start_offset
						fi
						
						for iOffset in $(seq $start_offset $stop_offset)
						do
							# determine raw date from iOffset
							current_date=$(common.date.toRaw $(common.date.OffsetToDate $iOffset))
						
							main.log -w  "The following files will be deleted:"
					
							ls ${CXR_STATE_DIR}/${current_date}@*@* | xargs -i basename \{\}
								
							if [[ "$(common.user.getOK "Do you really want to delete these files?" )" == false ]]
							then
								# No 
								main.log -a "Will not delete this information"
								continue
							else
								#Yes
								rm -f ${CXR_STATE_DIR}/${current_date}@*@* 2>/dev/null
							fi #Delete?
						done
								
						;; # specific.day
						
					step)
						# To enumerate all steps, we use a little grep-magic
						# We can tell between module types and steps
						module_types="$(find ${CXR_STATE_DIR} -noleaf -type f | grep -o '@.*@' - | sort | uniq) "
						
						# Count the module types
						num_module_types=$(echo "$module_types" | wc -l)
						
						steps="$(find ${CXR_STATE_DIR} -noleaf -type f | grep -o '@.*@.*\.' - | sort | uniq) none"
						
						which_step="$(common.user.getMenuChoice "Which module types (the first $num_module_types entries in the list) or steps state information should be deleted \n(none exits this function)?" "${module_types}${steps}" "none" )"
						
						if [[ $which_step == none ]]
						then
							main.log -a "Will not delete any state information" 
							return 0
						fi
						
						main.log -w  "The following files will be deleted:"
								
						ls ${CXR_STATE_DIR}/*${which_step}* | xargs -i basename \{\}
						
						if [[ "$(common.user.getOK "Do you really want to delete these files?" )" == false  ]]
						then
							# No 
							main.log -a  "Will not delete any state information"
							return 0
						else
							#Yes
							rm -f ${CXR_STATE_DIR}/*${which_step}* 2>/dev/null
							main.log -a "Done."
						fi #Delete?

						;; # specific.step
						
					both)
						# First get the step, then the day(s)
						# To enumerate all steps, we use a little grep-magic
						# We can tell between module types and steps
						module_types="$(find ${CXR_STATE_DIR} -noleaf -type f | grep -o '@.*@' - | sort | uniq) "
						
						# Count the module types
						num_module_types=$(echo "$module_types" | wc -l)
						
						steps="$(find ${CXR_STATE_DIR} -noleaf -type f | grep -o '@.*@.*\.' - | sort | uniq) none"
						
						which_step="$(common.user.getMenuChoice "Which module types (the first $num_module_types entries in the list) or steps state information should be deleted \n(none exits this function)?" "${module_types}${steps}" "none" )"
						
						if [[ $which_step == none ]]
						then
							main.log -w "Will not delete any state information" 
							return 0
						fi
						
						# To enumerate all days, we use a little grep-magic, also we add none at the end
						# The basename is needed to strip off the path, because the pattern starts with ^
						days="$(find ${CXR_STATE_DIR} -noleaf -type f | xargs -i basename \{\} |  grep -o '^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]' - | sort | uniq ) none"
						
						which_day="$(common.user.getMenuChoice "Which days state information should be deleted (none exits this function)?" "$days" "none" )"

						if [[ $which_day == none ]]
						then
							main.log -w "Will not delete any state information" 
							return 0
						fi

						# If this is true, we delete until the end
						following_days="$(common.user.getOK "Do you want to delete also all days following this one?" )"
						
						start_offset=$(common.date.toOffset $(common.date.toISO ${which_day}))

						if [[ "$following_days" == true ]]
						then
							stop_offset=$(( ${CXR_NUMBER_OF_SIM_DAYS} - 1 ))
						else
							stop_offset=$start_offset
						fi
						
						for iOffset in $(seq $start_offset $stop_offset)
						do
							# determine raw date from iOffset
							current_date=$(common.date.toRaw $(common.date.OffsetToDate $iOffset))
							
							main.log -w "The following files will be deleted:"
					
							ls ${CXR_STATE_DIR}/${current_date}${which_step}* | xargs -i basename \{\}
							
							if [[ "$(common.user.getOK "Do you really want to delete these files?" )" == false ]]
							then
								# No 
								main.log -a "Will not delete this information"
								continue
							else
								#Yes
								rm -f ${CXR_STATE_DIR}/${current_date}${which_step}* 2>/dev/null
							fi
							main.log -a "Done."
						done
						;; # specific.both
				
					none) 
						main.log -w "Will not delete any state information" 
						return 0
						;; # specific.none
				esac
				;; # specific
				
			tasks)
				# Do we do this?
				if [[ "$(common.user.getOK "Do you really want to clean all tasks?" )" == false  ]]
				then
					# No 
					main.log -w "Will not delete any state information"
					return 0
				else

					# Yes
					common.task.cleanTasks

					main.log -a "Done."
				fi
				;; # Tasks
			
			none)
				main.log -w "Will not delete any state information" 
				return 0
				;; # none
		
		esac # The big one...
	
	done # Loop where user can repeatedly delete data
}

################################################################################
# Function: common.state.doContinue?
#	
# Checks if the .continue file still exists,
# if not, CXR_RET_CONTINUE_MISSING is returned. Also checks the error threshold
# ends run if we are too high and toches the alive file
################################################################################
function common.state.doContinue?()
################################################################################
{
	local error_count
	error_count=$(main.countErrors)
	
	
	# Report error count
	main.log -v -b "Current Error Count: $error_count"

	# Check error threshold, but only if the value of
	# of CXR_ERROR_THRESHOLD is not -1
	if [[  ( ${CXR_ERROR_THRESHOLD} != ${CXR_NO_ERROR_THRESHOLD} ) && ( ${error_count} -gt ${CXR_ERROR_THRESHOLD} )   ]]
	then
		main.dieGracefully "The number of errors occured (${error_count}) exceeds the threshold (${CXR_ERROR_THRESHOLD})"
	fi
	
	# Do we care at all?
	# Set this in tests etc.
	if [[ "$CXR_ENABLE_STATE_DB" == false  ]]
	then
		main.log -v   "You disabled the state DB, cannot determine presence of the continue file!"
		return $CXR_RET_OK
	fi
	
	# If the variable is not defined, nothing will happen
	if [[ "${CXR_CONTINUE_FILE}"  ]]
	then
		if [[ ! -f ${CXR_CONTINUE_FILE}  ]]
		then
			main.log -w  "The Continue file no longer exists, exiting."
			return $CXR_RET_CONTINUE_MISSING
		else
			# We touch the continue file
			touch ${CXR_CONTINUE_FILE}
		fi
	fi
}

################################################################################
# Function: common.state.reportEta
# 
#  Reports the estimated time of arrival.
################################################################################
function common.state.reportEta()
################################################################################
{
	local percentDone
	local estimatedTimeSeconds
	
	percentDone=$(common.math.FloatOperation "($CXR_TASKS_DONE / $CXR_TASKS_TOTAL) * 100" -1 false )
	estimatedTimeSeconds=$(common.math.FloatOperation "( (100 - $percentDone) / 100) * $CXR_TIME_TOTAL_ESTIMATED" -1 false)
	
	
	# Only goes to stderr
	echo "Estimated remaining time of this run: $(common.date.humanSeconds $estimatedTimeSeconds)" 1>&2
	common.user.showProgressBar $percentDone
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
	
	main.log -a  "Initialising state DB in ${CXR_STATE_DIR}"
	common.state.init
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.state.isRepeatedRun?) false "common.state.isRepeatedRun?"
	is "$(common.state.countInstances)" 1 "common.state.countInstances in $CXR_ALL_INSTANCES_DIR"

	########################################
	# teardown tests if needed
	########################################
	
}
