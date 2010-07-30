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
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: common.state.isRepeatedRun?
#
# Returns true, if this run was already started earlier (If there are any Non-Todo Tasks)
# 
################################################################################
function common.state.isRepeatedRun?()
################################################################################
{
	local count
	
	count=$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT COUNT(*) FROM tasks WHERE status NOT IN ('$CXR_STATUS_TODO'))
	main.log -v  "Counting $count tasks that where already touched"
	
	if [[ "$count" -gt 0 ]]
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
	if [[ $(common.state.isRepeatedRun?) == true  ]]
	then
		# Let the database tell it
		${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT MAX(day_iso) FROM tasks WHERE status='$CXR_STATUS_SUCCESS'"
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
	if [[ $(common.state.isRepeatedRun?) == true  ]]
	then
		# Let the database tell it
		${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT MIN(day_iso) FROM tasks WHERE status='$CXR_STATUS_SUCCESS'"
	else
		echo ""
	fi
	
	return $CXR_RET_OK
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
		      
		main.log -a "Collecting module information, might take a while..."
		
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
					common.check.reportMD5 $file
					
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
							field="$(expr match "$metafield" '\([_A-Z]\{1,\}\)=')" || :
							# the value is to the right
							value="$(expr match "$metafield" '.*=\(.*\)')" || :
							
							# OK, we want all quoting gone and variables expanded
							value="$(eval "echo $(echo "$value")")"
							
							${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "INSERT INTO metadata (module,field,value) VALUES ('$module','$field','$value')"
						done
						
						IFS="$oIFS"
					fi
				done # Loop over files
			else
				main.dieGracefully "Tried to add modules in $dir - directory not found."
			fi # Directory exists?
		done # loop over type-index
		
		# Adding any new module types
		${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "INSERT OR IGNORE INTO types (type) SELECT DISTINCT value FROM metadata where field='CXR_META_MODULE_TYPE'"
		
		main.log -a "Module data successfully collected."
		
		# decrease global indent level
		main.decreaseLogIndent
	
	fi
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
	echo "Creating the file ${CXR_CONTINUE_FILE}. If this file is deleted, the process  stops at the next possible task" 1>&2
	echo "If you remove this file, the instance $$ on $(uname -n) will stop" > ${CXR_CONTINUE_FILE}
	
	main.log -v "Creating database schema..."
	
	${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" <<-EOT
	-- Get exclusive access
	PRAGMA main.locking_mode=EXCLUSIVE; 
	
	-- This is a "Oracle like" Dummy table 
	CREATE TABLE dual (dummy);
	DELETE FROM dual;
	INSERT INTO dual (dummy) VALUES ('X');
	
	-- Here we store all installed stuff (just for installer)
	CREATE TABLE IF NOT EXISTS installed (item,
	                                      model,
	                                      model_version);
	
	-- Here we store all known simulation days
	CREATE TABLE IF NOT EXISTS days (day_offset,
	                                 day_iso,
	                                 active);

	-- All modules go here
	CREATE TABLE IF NOT EXISTS modules (module, 
	                                    type,
	                                    path,
	                                    active);
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
	CREATE TABLE IF NOT EXISTS tasks (task_id,
	                                 rank,
	                                 module,
	                                 module_type,
	                                 exclusive,
	                                 day_offset,
	                                 invocation,
	                                 status,
	                                 seconds_estimated,
	                                 seconds_real,
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
# Function: common.state.storeStatus
#
# Stores the current status of a run, can be either $CXR_STATUS_RUNNING, $CXR_STATUS_SUCCESS or $CXR_STATUS_FAILURE.
# In the case of ERROR, we will take note of the fact.
#
# If a task starts, checks if this task was already executed.
# The name of the task is determined via <common.task.getId> if it is not passed in.
# The ability to pass a task name is used by installers and tests alike, they use a slightly different approach.
#
# If the user wants to run a specific module (CXR_RUN_LIMITED_PROCESSING=true), we will disregard the fact that
# the step already ran, but we advise -F
#
# Returns:
# 	$CXR_RET_OK if OK (true)
# 	$CXR_RET_ALREADY_RUN if this task was already started (false)
#
# Parameters:
# $1 - status ($CXR_STATUS_RUNNING, $CXR_STATUS_SUCCESS or $CXR_STATUS_FAILURE.)
################################################################################
function common.state.storeStatus()
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "needs a state like $CXR_STATUS_RUNNING, $CXR_STATUS_SUCCESS or $CXR_STATUS_FAILURE as Input"   
	fi
	
	local status
	local task
	
	status=$1
	
	# Get a nice string describing where we are
	task=$(common.task.getId)
	
	
	# Set the current state data
	module="${CXR_META_MODULE_NAME}"
	day_offset="${CXR_DAY_OFFSET}"
	invocation="${CXR_INVOCATION:-1}"
	
	# Do we care at all?
	# Set CXR_ENABLE_STATE_DB to false in tests etc.
	if [[ "$CXR_ENABLE_STATE_DB" == false ]]
	then
		main.log -v "You disabled the state DB (CXR_ENABLE_STATE_DB=false), new status will not be stored."
		echo true
		return $CXR_RET_OK
	fi
	
	case "$status" in
	
		"$CXR_STATUS_RUNNING") 
			# Check if this was already started
			if [[ $(common.state.hasFinished? "$module" "$day_offset" "$invocation") == true ]]
			then
				if [[ "$CXR_RUN_LIMITED_PROCESSING" == true ]]
				then
					# Ran already, but user wants to run specifically this
					main.log -w  "Task $task was already started, but since you requested this specific module, we run it. If it fails try to run \n \t ${CXR_CALL} -F \n to remove existing output files."
					echo true
				else
					# Oops, this task was already started	
					main.log -w  "Task $task was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
					
					# false means already run
					echo false
					return $CXR_RET_ALREADY_RUN
				fi 
			else
				echo true
			fi
			;;
	
			"$CXR_STATUS_SUCCESS")
				main.log "Task $task successfully completed."
				echo true
				;;
	
			"$CXR_STATUS_FAILURE")
				CXR_STATUS=$CXR_STATUS_FAILURE
				main.log -e "An error has occured during the execution of task $task!"
				echo false
			;;
			
			*)
				CXR_STATUS=$CXR_STATUS_FAILURE
				main.dieGracefully "Unknown state $status given"
				echo false
				return $CXR_RET_ERROR
			;;
	esac
	
	# Update the database
	${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "UPDATE tasks set status='$status' WHERE module='$module' AND day_offset=$day_offset AND invocation=$invocation"
	
	return $CXR_RET_OK
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
		main.log -e "Found other instances - maybe these processes died or they are still running:\n(Check their age!)"
		
		find "${CXR_ALL_INSTANCES_DIR}" -noleaf -type d -maxdepth 1 2>/dev/null | tee -a ${CXR_LOG}
		
		main.log -e "Check manually if the processes still run, if not clean the state db by runnig \n\t ${CXR_CALL} -c \n or (experts only) you can run your instance anyway using \n \t ${CXR_RUN} -m [options]"    
		
		main.dieGracefully "Process stopped"
	fi
}

################################################################################
# Function: common.state.hasFinished?
#	
# Checks if a specific task has finished.
#
# Parameters:	
# $1 - module name
# $2 - day_offset
# $3 - invocation
################################################################################
function common.state.hasFinished?()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a module, a day_offset and a invocation as input" 
		echo false
	fi
	
	local module
	local day_offset
	local invocation
	local task
	local status
	
	module="$1"
	day_offset="$2"
	invocation="$3"
	task="$(common.task.getId $module $day_offset $invocation)"
	
	main.log -v "Testing if task ${task} is done..."
	
	status=$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT status FROM tasks WHERE module='$module' AND day_offset=$day_offset AND invocation=$invocation")
	
	case $status in
	
		$CXR_STATUS_SUCCESS) 
			echo true 
			;;
			
		$CXR_STATUS_FAILURE) 
			main.log -w "Task $task failed."
			echo false
			;;
		
		$CXR_STATUS_RUNNING)
			main.log -v "Task $task is marked as still running."
			echo false
			;;
			
		$CXR_STATUS_TODO)
			main.log -v "Task $task has not started yet."
			echo false
			;;
		
		*) 
			main.log -e "Task $task has unknown status: $status"
			echo false
			;;
	esac
}

################################################################################
# Function: common.state.hasFailed?
#	
# Check if a specific task has failed (similar to the inverse of <common.state.hasFinished?>
# but with subtle differences.
#
# Parameters:	
# $1 - module name
# $2 - day_offset
# $3 - invocation
################################################################################
function common.state.hasFailed?()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a module, a day_offset and a invocation as input" 
		echo false
	fi
	
	local module
	local day_offset
	local invocation
	local task
	local status
	
	module="$1"
	day_offset="$2"
	invocation="$3"
	task="$(common.task.getId $module $day_offset $invocation)"
	
	main.log -v "Testing if task ${task} is done..."
	
	status=$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT status FROM tasks WHERE module='$module' AND day_offset=$day_offset AND invocation=$invocation")
	
	case $status in
	
		$CXR_STATUS_SUCCESS) 
			echo false 
			;;
			
		$CXR_STATUS_FAILURE) 
			main.log -w "Task $task failed."
			echo true
			;;
		
		$CXR_STATUS_RUNNING)
			main.log -v "Task $task is marked as still running."
			echo false
			;;
			
		$CXR_STATUS_TODO)
			main.log -v "Task $task has not started yet."
			echo false
			;;
		
		*) 
			# Unknown := failed
			main.log -e "Task $task has unknown status: $status"
			echo true
			;;
	esac
}

################################################################################
# Function: common.state.cleanup
#	
# Depending on user selection:
# - Deletes all state information
# - Deletes only part of the state information
# All is in a endless loop so one can quickly delete a lot of stuff
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
		what=$(common.user.getMenuChoice "Which part of the state database do you want to clean (none exits this function)?\nNote that you might need to delete output files in order to repeat a run, or run with ${CXR_CALL} -F (overwrite existing files)" "all-tasks specific-tasks old-instances none" "none")
		
		case "$what" in 
		
			all-tasks)
				# Do we do this?
				if [[ "$(common.user.getOK "Do you really want to delete the whole state database ${CXR_STATE_DB_FILE}?" )" == false  ]]
				then
					# No 
					main.log -a "Will not delete any state information"
					return 0
				else
					# Yes
					rm -f ${CXR_STATE_DB_FILE}
					main.log -a "Done."
				fi #Delete?
				
				;; # all-tasks
			
			old-instances)
			
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
			
				;; # old-instances
					
			specific-tasks)
			
				# First, we build a select statement,
				# then we execute it (SELECT & DELETE)
				
				# First select the module type or module name
				if [[ "$(common.user.getOK "Do you want to delete specific module types (otherwise, you get a list of modules)?" )" == true  ]]
				then
					# Module types it is.
					# We add the value "all" to the result
					steps="$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT type FROM types UNION SELECT 'all' FROM dual")"
					
					oIFS="$IFS"
					# set IFS to newline that select parses correctly
					IFS='
'
					which_step="$(common.user.getMenuChoice "Which module types state information should be deleted?" "${steps}" "none" )"
					# Reset IFS
					IFS="$oIFS"
					
					if [[ $which_step == all ]]
					then
						main.log -a "You pre-selected all module types for deletion"
						where_module=""
					else
						where_module="module_type='$which_step'"
					fi
					
				else
					# Module names
					
					# We add the value "all" to the result
					steps="$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT module FROM modules UNION SELECT 'all' FROM dual")"
					
					oIFS="$IFS"
					# set IFS to newline that select parses correctly
					IFS='
'
					which_step="$(common.user.getMenuChoice "Which modules state information should be deleted?" "${steps}" "none" )"
					# Reset IFS
					IFS="$oIFS"
					
					if [[ $which_step == all ]]
					then
						main.log -a "You pre-selected all modules for deletion"
						where_module=""
					else
						where_module="module='$which_step'"
					fi
				fi
				
				# Get all days and add all as above
				days="$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT day_iso FROM days UNION SELECT 'all' FROM dual")"
				
				oIFS="$IFS"
				# set IFS to newline that select parses correctly
				IFS='
'
				which_day="$(common.user.getMenuChoice "Which days state information should be deleted?" "$days" "none" )"
				
				# Reset IFS
				IFS="$oIFS"
				
				if [[ $which_day == all ]]
				then
					main.log -a "You pre-selected all days for deletion"
					all_days=true
				else
					offset=$(common.date.toOffset $which_day)
					all_days=false
				fi

				# If this is true, we delete until the end
				following_days="$(common.user.getOK "Do you want to delete also all days following this one?" )"
				
				if [[ "$following_days" == true ]]
				then
					stop_offset=$(( ${CXR_NUMBER_OF_SIM_DAYS} - 1 ))
				else
					stop_offset=$offset
				fi
				
				for iOffset in $(seq $offset $stop_offset)
				do
					main.log -w "Planning to delete these tasks:"
					
					if [[ $all_days == true ]]
					then
						# We delete all days
						where="$where_module"
					else
						# Just the current one
						where="$where_module AND day_offset=$iOffset"
					fi
					
					${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT id FROM tasks WHERE $where"

					if [[ "$(common.user.getOK "Do you really want to do this?" )" == false ]]
					then
						# No 
						main.log -a "Will not delete this information"
						continue
					else
						#Yes
						${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "DELETE FROM tasks WHERE $where"
					fi
					
					if [[ -z "$where_day" ]]
					then
						# all days need to be deleted only once
						break
					fi
				done
					
				main.log -a "Done."
				
				;; # specific
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
#
################################################################################
function common.state.doContinue?()
################################################################################
{
	local error_count
	error_count=$(main.countErrors)
	
	# Report error count and ReaLoad
	main.log -a -b "Current Error Count: $error_count\nCurrent ReaLoad: $(common.performance.getReaLoadPercent) %"

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
		main.log -v "You disabled the state DB, cannot determine presence of the continue file!"
		return $CXR_RET_OK
	fi
	
	# If the variable is not defined, nothing will happen
	if [[ "${CXR_CONTINUE_FILE}" ]]
	then
		if [[ ! -f ${CXR_CONTINUE_FILE} ]]
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
# Function: common.state.getPercentDone
# 
# Calculates the % of tasks done
################################################################################
function common.state.getPercentDone()
################################################################################
{
	local percentDone
	local done
	local total
	
	done=$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT COUNT(*) FROM tasks WHERE status NOT IN('$CXR_STATUS_TODO')")
	total=$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT COUNT(*) FROM tasks")
	
	if [[ $total -gt 0 ]]
	then
		percentDone=$(common.math.FloatOperation "($done / $total) * 100" -1 false )
	else
		percentDone=0
	fi
	
	echo $percentDone
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
	
	percentDone=$(common.state.getPercentDone)
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
