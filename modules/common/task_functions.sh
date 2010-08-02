# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Functions to manage execution of modules.
# The most important aspect of this is the management of the varius dependencies 
# between modules.
#
# Prepares a pool of tasks, which are then harvested by Worker threads. 
# This pool is implemented as a <http://www.sqlite.org> DB.
# 
# The worker processes may run in parallel - even on different machines.
#
# Approach:
# First, a list of tasks is generated.
# This list is sorted according to the dependencies (topological sorting), so that tasks with no or few
# dependencies appear first -> execution plan.
# Then, we create a number of workers, which each get a unique entry of this list. 
# They check first if the dependencies are already fulfilled 
# if not they wait, otherwise the task starts.
# After successful execution, the worker gets the next task from the list.
# A task is identified by its module name, a day offset, and a invocation id. 
# The invocation id is useful for tasks that can be splitted further, like albedo_haze_ozone.
# This allows us to parallelize parts of a task.
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

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=4

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|dot|optional exec|tsort"

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the functions to manage parallel task execution"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: common.task.getId
#
# Creates a task Id out of several inputs. Can be used to identify a task,
# be it as a dependency or otherwise.
#
# Format: $date@$module@$invocation
#
# Example:
# > echo "${dependency} $(common.task.getId "$module" "$day_offset" "$iInvocation" )" >> $output_file
# 
# Parameters:
# $1 - module name
# $2 - day offset
# $3 - iInvocation
################################################################################
function common.task.getId()
################################################################################
{
	local module
	local invocation
	local date

	if [[ $# -lt 3 ]]
	then
		# Use the environment
		module="${CXR_META_MODULE_NAME}"
		# Use CXR_DATE or the start date
		date="${CXR_DATE:${CXR_STRT_DATE}}"
		invocation="${CXR_INVOCATION:-1}"
	else
		# Use parameters
		module="${1}"
		date="$(common.date.OffsetToDate "${2:-0}")"
		invocation="${3:-1}"
	fi
	
	echo "${date}@${module}@${invocation}" 
}

################################################################################
# Function: common.task.parseId
# 
# Parses an identifier string of the form date@Module@Invocation,
# e. g. 2007-01-01@prepare_output_dir@0
#
# Output variables:
# _module
# _day_offset
# _invocation
#
# Parameters:
# $1 - a task identifier
################################################################################
function common.task.parseId()
################################################################################
{
	if [[ $# -ne 1 || -z "$1" ]]
	then
		main.dieGracefully "Needs a non-empty identifier as Input"
	fi
	
	local identifier
	local -a id_arr
	
	identifier="$1"

	
	main.log -v "Parsing $identifier"
	
	oIFS="$IFS"
	IFS="@"
	
	id_arr=($identifier)
	IFS="$oIFS"
	
	if [[ ${#id_arr[@]} -ne 3 ]]
	then
		main.dieGracefully "Malformed task id $identifier"
	else
		_date=${id_arr[0]}
		_module=${id_arr[1]}
		_invocation=${id_arr[2]}
		
		_day_offset=$(common.date.toOffset $_date)
		main.log -v "module: $_module date: $_date day_offset: $_day_offset invocation: $_invocation"
	fi
}

################################################################################
# Function: common.task.createDependencyList
# 
# Collects module dependencies of the modules to be executed in a file of the form
# independent_module dependent_module.
# 
# If a module has no dependencies, then the line is
# independent_module independent_module
# (see man tsort for details).
#
# Parameters:
# $1 - output_file to write list of dependencies for tsort to sort to
# $2 - optional where clause including leading logical oerator
# $3 - optional boolean flag (default false) to ignore - dependencies
# $4 - optional day_offset (dependent)
################################################################################
function common.task.createDependencyList()
################################################################################
{
	local output_file
	local tempfile
	local where
	local ignore_last_day
	local day_offset
	local day_where

	output_file="$1"
	where=${2:-}
	ignore_last_day=${3:-false}
	day_offset="${4:-}"
	
	
	if [[ $ignore_last_day == true ]]
	then
		# Add additional where statement to suppress - dependencies
		where="$where AND di.day_iso = dd.day_iso"
	fi
	
	if [[ "$day_offset" ]]
	then
		day_where=" AND dependent_day_offset = $day_offset "
	fi
	
	
	${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" <<-EOT
	
	-- Prepare proper output
	.output $output_file
	.separator ' '
	
	------------------------------------
	-- First, add all tasks, no matter what
	-- Duplicates are removed later
	------------------------------------
	
	SELECT di.day_iso || '@' || t.module || '@' || t.invocation,
	       dd.day_iso || '@' || t.module || '@' || t.invocation
	FROM tasks t, days di, days dd, modules m
	WHERE m.module = t.module
	AND   di.day_offset = t.day_offset
	AND   m.active='true'
	AND   di.day_iso = dd.day_iso
	$where ;
	
	------------------------------------
	-- Then add all the dependencies
	------------------------------------
	
	SELECT di.day_iso || '@' || independent_module || '@' || independent_invocation,
	       dd.day_iso || '@' || dependent_module || '@' || dependent_invocation
	FROM dependencies, days di, days dd, modules m
	WHERE m.module = independent_module
	AND   di.day_offset = independent_day_offset
	AND   dd.day_offset = dependent_day_offset
	AND   m.active='true'
	$where $day_where ;
	
	EOT
	
	main.log -v "Removing duplicates..."
	tempfile="$(common.runner.createTempFile $FUNCNAME)"
	
	sort "$output_file" | uniq > "$tempfile"
	mv "$tempfile" "$output_file"
}

################################################################################
# Function: common.task.drawDependencyGraph
# 
# Creates an image of the dependency graphy using dot (graphviz)
# 
# Parameters:
# $1 - a file describing the dependencies in tsort format
# [$2] - an output file (extension must be any suported Graphviz filetype like pdf, ps, svg) 
# see also <http://www.graphviz.org/doc/info/output.html>
################################################################################
function common.task.drawDependencyGraph()
################################################################################
{
	local input_file
	local output_file
	local dot_file
	local elements
	
	# Extract the filetype (lowercase)
	local extension
	
	input_file="$1"
	output_file="${2:-$CXR_RUN_DIR/${CXR_RUN}_dep_$(date +"%Y_%m_%d_%H_%M").pdf}"
	dot_file=$(common.runner.createTempFile $FUNCNAME)
	extension="$(common.string.toLower "${output_file##*.}")"
	
	echo "digraph dependencies" > "$dot_file"
	echo "{" >> "$dot_file"
	
	# Now go through each entry of the file, the form is
	# independent_mod dependent_mod
	# if the two names are the same, ignore them
	# always exchange the order.
	# Dot has issues with the @ sign, we replace it by i for invocation
	while read line
	do
		# IFS is ok with space
		elements=($line)
		
		# Replace @
		indep=${elements[0]//@/_i}
		dep=${elements[1]//@/_i}
		
		if [[ $indep != $dep ]]
		then
			echo "    ${dep} -> ${indep} ;" >> "$dot_file"
		else
			main.log -v  "$indep equals $dep"
		fi
	
	done < "${input_file}"
	
	echo "}" >> "$dot_file"
	
	# Now call dot
	${CXR_DOT_EXEC} -T${extension} "${dot_file}" -o "${output_file}" 2>&1 | tee -a $CXR_LOG
	
	if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
	then
		main.log -e  "Could not visualize the dependencies."
	else	
		main.log -a  "You find a visualisation of the modules dependencies in the file ${output_file}"
	fi
}

################################################################################
# Function: common.task.countAllTasks
#
# Returns the number of tasks known.
# 
#
################################################################################
function common.task.countAllTasks()
################################################################################
{
	# Find all entries in the table
	task_count="$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT COUNT(*) FROM tasks")"
	
	main.log -v "Found $task_count tasks in total"
	
	echo "$task_count"
}


################################################################################
# Function: common.task.countOpenTasks
#
# Returns the number of open tasks. 
# Make sure you call this in a critical section (lock acquired), otherwise 
# a race condition might occur
# 
#
################################################################################
function common.task.countOpenTasks()
################################################################################
{
	# Find only "TODO" entries
	task_count="$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT COUNT(*) FROM tasks WHERE STATUS='${CXR_STATUS_TODO}'")"
	
	main.log -v "Found $task_count open tasks"
	
	echo "$task_count"
}

################################################################################
# Function: common.task.detectLockup
#
# Tests if all workers of a run are in a waiting state. This means that some dependency
# is not fullfilled but has not failed, so all Worker will have to wait forever
# (or until they waited CXR_DEPENDECY_TIMEOUT_SEC seconds).
# Since it is possible that this happens be coincidence, we keep a counter in
# a hash that we increase when all workers are idle and decrease when they are not.
# If a threshold is reached, we stop the run.
#
# Variables:
# CXR_MAX_LOCKUP_COUNT - the maximal number of consecutive idling allowed. 
#
# Hashes:
# Lockup (Global)
#
################################################################################
function common.task.detectLockup()
################################################################################
{
	local count
	local numRunning
	
	common.hash.has? Lockup $CXR_HASH_TYPE_GLOBAL LockupCount
	if [[ $_has == true ]]
	then
		count=$_value
	else
		count=0
	fi
	
	# Count the running workers
	numRunning="$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT COUNT(*) FROM workers WHERE STATUS='${CXR_STATUS_RUNNING}'")"
	
	if [[ $numRunning -gt 0 ]]
	then
		# OK, at least one is running, decrease if above 0
		if [[ $count -gt 0 ]]
		then
			count=$(( $count - 1 ))
		fi
	else
		# Too bad, they are all waiting, increase
		count=$(( $count + 1 ))
	fi
	
	if [[ $count -gt ${CXR_MAX_LOCKUP_COUNT:-10} ]]
	then
		main.dieGracefully "It seems that all workers of this run wait for a dependency to be resolved - we will stop now!"
	fi
	
	# Store new count
	common.hash.put Lockup $CXR_HASH_TYPE_GLOBAL LockupCount $count
}


################################################################################
# Function: common.task.setNextTask
#
# Returns the id of the next task to execute and all its data in environment vars.
# Even though sqlite does locking, we protect this critical function with a lock.
# 
# If there are no more tasks, the empty string is returned.
# If all tasks are executed, deletes the continue file.
#
# We set these _vars:
#
# Output variables:
# _id
# _exclusive
# _module
# _day_offset
# _invocation
#
################################################################################
function common.task.setNextTask()
################################################################################
{
	# Acquire lock
	if [[ $(common.runner.getLock NextTask "$CXR_HASH_TYPE_INSTANCE") == false ]]
	then
		main.dieGracefully "Waiting for NextTask lock took too long"
	fi
	
	local task_count
	local potential_task_data
	
	task_count=$(common.task.countOpenTasks)

	# Are there open tasks at all?
	if [[ "$task_count" -eq 0 ]]
	then
		main.log  "All tasks have been processed, notifying system after security pause..."
		
		# there are no more tasks, remove all continue files after some waiting
		# The waiting should ensure that all tasks are past their check for do_we_continue
		sleep $CXR_WAITING_SLEEP_SECONDS
		
		common.state.deleteContinueFiles
		common.runner.releaseLock NextTask "$CXR_HASH_TYPE_INSTANCE"
		echo ""
		return $CXR_RET_OK
		
	else
		main.log -v "There are $task_count unfinished tasks - we choose the top one."
	fi
	
	# get first relevant entry in the DB
	potential_task_data="$(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT id,module,module_type,exclusive,day_offset,invocation FROM tasks WHERE STATUS='${CXR_STATUS_TODO}' ORDER BY rank ASC LIMIT 1")"
	
	# Check status
	if [[ $? -ne 0 ]]
	then
		main.dieGracefully "could not find next task!"
	fi
	
	# we might not get a string
	if [[ -z "$potential_task_data" ]]
	then
		# No task!
		main.dieGracefully "could not find next task!"
	else
		# We got a task
		# parse
		
		oIFS="$IFS"
		IFS="$CXR_DELIMITER"
		set $potential_task_data
		IFS="$oIFS"
		
		_id="$1"
		_module="$2"
		_module_type="$3"
		_exclusive="$4"
		_day_offset="$5"
		_invocation="$6"
		
		# Assign it by an update
		${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "UPDATE tasks set  STATUS='${CXR_STATUS_RUNNING}' WHERE id=$id"
		
		main.log -v "New task has id $id"
	fi
	
	# Release lock
	common.runner.releaseLock NextTask "$CXR_HASH_TYPE_INSTANCE"
}

################################################################################
# Function: common.task.changeTaskStatus
#
# Just updates the task db.
# As a precaution, we also notify the state DB on error (all modules should do this!)
#
# Parameters:
# $1 - id of task
# $2 - status (SUCCESS/FAILURE)
################################################################################
function common.task.changeTaskStatus()
################################################################################
{
	local id
	local status
	
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a task descriptor and a status as input"
	fi
	
	id="$1"
	status="$2"
	
	main.log -v "Changing status of $id to $status"
	
	case $status in
	
		$CXR_STATUS_SUCCESS|$CXR_STATUS_FAILURE) 
			${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "UPDATE tasks set status='${status}' WHERE id=$id"
			;;

		*)
			main.dieGracefully "status $status not supported!"
	
	esac
}

################################################################################
# Function: common.task.waitingWorker
#
# Udates the status of a worker to waiting
#
# Parameters:
# $1 - pid of common.task.Worker
################################################################################
function common.task.waitingWorker()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully "needs a pid as input"
	fi
	
	local pid
	pid=$1
	 
	${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "UPDATE workers set status='${CXR_STATUS_WAITING}' WHERE pid=$pid AND hostname='$CXR_MACHINE'"
	
	main.log -v   "common.task.Worker (pid: $pid) changed its state to waiting"
}

################################################################################
# Function: common.task.runningWorker
#
# Udates the status of a worker to running
#
# Parameters:
# $1 - pid of common.task.Worker
################################################################################
function common.task.runningWorker()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully "needs a pid as input"
	fi
	
	local pid
	pid=$1
	 
	${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "UPDATE workers set status='${CXR_STATUS_RUNNING}' WHERE pid=$pid AND hostname='$CXR_MACHINE'"
	
	main.log -v "common.task.Worker (pid: $pid) changed its state to running"
}


################################################################################
# Function: common.task.removeWorker
#
# kills the common.task.Worker of the given task_pid and alse removes it from the process list.
# 
# Parameters:
# $1 - the workers pid
################################################################################
function common.task.removeWorker()
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "needs a task_pid as input"
	fi
	
	local pid
	
	pid=$1
	
	# Kill the process
	kill $pid 2>/dev/null
	
	# Remove from DB
	${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "DELETE FROM workers WHERE pid=$pid AND hostname='$CXR_MACHINE'"
}

################################################################################
# Function: common.task.Worker
#
# This function is the workhorse of the parallel CAMxRunner. The Runner spawns one 
# or more of this functions to operate on the existing tasks.
# This can even be done from more than one machine.
#
# TODO: a worker that finishes must restore a consistent state, that is the current
# task must be "put back"
#
# The worker gets a new task via <common.task.setNextTask>
# then waits (using <common.module.areDependenciesOk?>)
# until the dependencies of this task are fullfilled. 
#
# Parameters:
# $1 - the worker id (internal number, just to tell log output on screen apart)
################################################################################
function common.task.Worker()
################################################################################
{
	# The ID is global
	CXR_WORKER_ID=${1}
	
	local tmp
	local task_pid
	local new_task_descriptor
	local new_task
	local oIFS
	local descriptor
	local task
	local exclusive
	local raw_dependencies
	local invocation
	local module
	local day_offset
	local start_epoch
	
	#Getting the pid is not easy, we do not want to create unnecessary processes...
	tmp=$(common.runner.createTempFile $FUNCNAME)
	
	# The pid is the parent of the awk process
	# and the 4th field of /proc/self/stat is the Parent PID
	awk '{print $4}' /proc/self/stat > $tmp
	# We add the machine name so that it is unique among all machines
	pid=$(cat $tmp)
	
	# Insert this worker
	${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "INSERT OR REPLACE workers (pid, hostname,status,epoch_m) VALUES ($pid,'$CXR_MACHINE','$CXR_STATUS_WAITING',$(date "+%s"))"
	
	main.log -a -B  "parallel worker (pid ${pid}, id ${CXR_WORKER_ID} ) starts on $CXR_MACHINE..."

	# Do we have more than 1 process?
	# If so, define process-specific stuff
	if [[ -f "$CXR_LOG" && "$CXR_MAX_PARALLEL_PROCS" -gt 1 ]]
	then
		# Set task_pid-dependent logfile to disentangle things
		CXR_LOG=${CXR_LOG%.log}_${CXR_MACHINE}_${pid}.log
		
		main.log -a "This common.task.Worker will use its own logfile: ${CXR_LOG}"
	fi

	# We stay in this loop as long as the continue file exists
	while [[ -f "$CXR_CONTINUE_FILE" ]]
	do
		# Do we stop here?
		common.state.doContinue? || main.dieGracefully "Continue file no longer present."
		
		# We are not yet busy
		common.task.waitingWorker $pid
		
		# Is there enough free memory?
		if [[ "$(common.performance.getMemFreePercent)" -gt ${CXR_MEM_FREE_PERCENT:-0} ]]
		then
			# Enough Memory
			
			# common.task.setNextTask provides tasks in an atomic fashion
			# already moves the task descriptor into "running" position
			# Sets a couple of "background" variables
			# This is a blocking call (we wait until we get a task)
			common.task.setNextTask
			
			id=$_id
			
			# The task id might be empty due to errors
			if [[ "$id" ]]
			then
				main.log -v "New task received: $id"
				
				######################
				# task was already parsed by common.task.setNextTask
				######################
				
				module="$_module"
				day_offset="$_day_offset"
				invocation="$_invocation"
				exclusive="$_exclusive"
				
				main.log -v "module: $module day_offset: $day_offset invocation: $invocation"
				
				if [[ "$exclusive" == true ]]
				then
					# If exclusive, try to get lock
					if [[ $(common.runner.getLock Exclusive "$CXR_HASH_TYPE_GLOBAL") == false ]]
					then
						main.dieGracefully "There seeems to be another exclusive task running that takes too long."
					fi
				else
					# If not, just check if it is set
					common.runner.waitForLock Exclusive "$CXR_HASH_TYPE_GLOBAL"
					
					if [[ $_retval == false ]]
					then
						main.dieGracefully "There seeems to be another exclusive task running that takes too long."
					fi
				fi
				
				module_path="$(common.module.getPath "$module")"

				raw_dependencies="$(common.module.getMetaField $module "CXR_META_MODULE_DEPENDS_ON")"
				
				start_epoch=$CXR_EPOCH
				
				# We need to wait until all dependencies are ok
				until [[ "$(common.module.areDependenciesOk? "$module" "$day_offset" "$invocation" )" == true ]]
				do
					main.log -v "Waiting for dependencies of $module to be done for day $day_offset"
					
					# Tell the system we wait, then sleep
					common.task.waitingWorker $task_pid
					sleep $CXR_WAITING_SLEEP_SECONDS
					
					if [[ $(( $(date "+%s") - $start_epoch )) -gt $CXR_DEPENDECY_TIMEOUT_SEC ]]
					then
						main.dieGracefully "It took longer than CXR_DEPENDECY_TIMEOUT_SEC ($CXR_DEPENDECY_TIMEOUT_SEC) seconds to fullfill the dependencies of $module for day $day_offset"
					fi
				done
				
				# Time to work
				common.task.runningWorker $task_pid
				
				main.log -v "module: $module day_offset: $day_offset invocation: $invocation exclusive: $_exclusive"
				
				# Setup environment
				common.date.setVars "$CXR_START_DATE" "${day_offset:-0}"
				
				main.log -a -B "common.task.Worker $task_pid assigned to $module for $CXR_DATE"
				
				# Before loading a new module, remove old meta variables
				unset ${!CXR_META_MODULE*}
				
				# Export the module name
				CXR_META_MODULE_NAME=${module}
				
				# source the file to get the rest of the metadata
				source $module_path
				
				# Start Timing 
				common.performance.startTiming $CXR_META_MODULE_NAME
				
				# Now start the work.
				# The function we use is the Module name
				# We use the return status to determine if it was successful
				
				# we pass the invocation as argument
				# most modules simply ignore this.
				# If invocation is unset, we pass 1
				$CXR_META_MODULE_NAME ${invocation:-1} \
				&& common.task.changeTaskStatus $new_task_descriptor $CXR_STATUS_SUCCESS \
				|| common.task.changeTaskStatus $new_task_descriptor $CXR_STATUS_FAILURE
							
				# Stop Timing 
				common.performance.stopTiming $CXR_META_MODULE_NAME
				
				#Release resources if needed
				if [[ "$_exclusive" == true ]]
				then
					main.log  "Activating the assignment of new tasks again."
					common.runner.releaseLock Exclusive "$CXR_HASH_TYPE_GLOBAL"
				fi
			else
				main.log -v  "Worker $pid did not receive an assignment - maybe there are too many workers around"
				
				# This means that someone wants exclusive access
				# Tell the system we wait, then sleep
				common.task.waitingWorker $task_pid
				sleep $CXR_WAITING_SLEEP_SECONDS
				
			fi # Got a task?
		
		else
			# Not enough memory
			main.log -w "Worker $task_pid detected that we have less than ${CXR_MEM_FREE_PERCENT} % of free memory.\nReaLoad: $(common.performance.getReaLoadPercent) %. We wait..."
			
			# Tell the system we wait, then sleep
			common.task.waitingWorker $pid
			sleep $CXR_WAITING_SLEEP_SECONDS
		fi # Enough Memory?
			
	done
	
	# We have done our duty
	common.task.removeWorker $pid

	exit $CXR_RET_OK
}

################################################################################
# Function: common.task.spawnWorkers
#
# Parameters:
# $1 - number of workers to spawn
################################################################################
function common.task.spawnWorkers()
################################################################################
{
	local iWorker
	
	# The control thread is "Worker 0"
	CXR_WORKER_ID=0
	
	main.log  "We now create $1 worker threads"
	
	for iWorker in $(seq 1 $1)
	do
		# Create a worker and send it to background
		common.task.Worker $iWorker &
		
		# Wait a bit to avoid congestion
		main.log -a "We wait 60 seconds until we launch the next worker to see the memory demand"
		sleep 60
	done
}

################################################################################
# Function: common.task.removeAllWorkers
# 
# Removes all workers
# 
################################################################################
function common.task.removeAllWorkers()
################################################################################
{
	main.log  "We remove all workers on $CXR_MACHINE."
	
	for pid in $(${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE" "SELECT pid FROM workers WHERE hostname='$CXR_MACHINE'")
	do
		common.task.removeWorker "$pid"
	done
}

################################################################################
# Function: common.task.waitForWorkers
#
# Basically a sleep function: we loop and check if the continue file is there.
#
################################################################################
function common.task.waitForWorkers()
################################################################################
{
	main.log  "Entering a wait loop (the work is carried out by background processes. I check every $CXR_WAITING_SLEEP_SECONDS seconds if all is done.)"
	
	while [ -f "$CXR_CONTINUE_FILE" ]
	do
		sleep $CXR_WAITING_SLEEP_SECONDS
		common.state.reportEta
	done
	
	main.log -B   "The Continue file is gone, all workers will stop asap."
	
	# OK, remove the workers now
	common.task.removeAllWorkers
}

################################################################################
# Function: common.task.init
# 
# Modifies the state database by assingning a rank to each task depending on the 
# ordering scheme in use.
# If we run in parallel, the sorting is done differently that if we run non-parallel
# 
# DBs:
# CXR_STATE_DB_FILE
################################################################################
function common.task.init()
################################################################################
{
	local running_tasks
	local running_task
	local task_id
	local tasksTodo
	local task
	local taskCount
	local task_file
	local line
	local module
	local day_offset
	local invocation
	local module_type
	local my_stage
	local dep_file
	local sorted_file
	local iOffset
	
	main.log -a "Initializing task subsystem, might take a while, depending on number of tasks...\n"
	
	# Reset the ID counter
	local current_id
	local task_file
	
	current_id=1
	
	# Init
	CXR_TIME_TOTAL_ESTIMATED=0
	
	# Check if we already have tasks 
	# Iff we have this and allow multiple, we use them.
	taskCount=$(common.task.countAllTasks)
	
	if [[	$taskCount -ne 0 && \
			${CXR_ALLOW_MULTIPLE} == true && \
			"$(common.state.countInstances)" -gt 1 ]]
	then
		# We are in a non-master multiple runner
		main.log -a -b "This is a slave process - we use the pre-existing task infrastructure"
	else
		# Redo everything
		
		# Some tempfiles we need
		dep_file="$(common.runner.createTempFile dependencies)"
		sorted_file="$(common.runner.createTempFile tsort-out)"
		mixed_file="$(common.runner.createTempFile mixed_tasks)"
		
		main.log -a "\nCreating the list of dependencies...\n"
		
		if [[ $CXR_PARALLEL_PROCESSING == true ]]
		then
			# In parallel mode, we order all modules together
			common.task.createDependencyList "$dep_file"
		
			main.log -a "\nOrdering tasks...\n"
			${CXR_TSORT_EXEC} "$dep_file" > "$sorted_file" || main.dieGracefully "I could not figure out the correct order to execute the tasks. Most probably there is a cycle (Module A depends on B which in turn depends on A)"

		else
			# In sequential mode, we first sort the One-Time preprocessors,
			# then each day 
			# then the One-Time postprocossors

			# In all of these, we ignore - dependencies
			
			# OT-PRE
			common.task.createDependencyList "$dep_file" " AND m.type='$CXR_TYPE_PREPROCESS_ONCE'" true
			main.log -a "\nOrdering $CXR_TYPE_PREPROCESS_ONCE tasks...\n"
			${CXR_TSORT_EXEC} "$dep_file" >> "$sorted_file" || main.dieGracefully "I could not figure out the correct order to execute the tasks.\nMost probably there is a cycle (Module A depends on B which in turn depends on A)"
			
			# DAILY
			# This is not very elegant...
			main.log -a "\nOrdering daily tasks...\n"
			for iOffset in $(seq 0 $(( ${CXR_NUMBER_OF_SIM_DAYS} - 1 )) )
			do
				common.task.createDependencyList "$dep_file" " AND m.type NOT IN ('$CXR_TYPE_PREPROCESS_ONCE','$CXR_TYPE_POSTPROCESS_ONCE')" true $iOffset
				${CXR_TSORT_EXEC} "$dep_file" >> "$sorted_file" || main.dieGracefully "I could not figure out the correct order to execute the tasks.\nMost probably there is a cycle (Module A depends on B which in turn depends on A)"
			done
			
			# OT-POST
			common.task.createDependencyList "$dep_file" " AND m.type='$CXR_TYPE_POSTPROCESS_ONCE'" true
			${CXR_TSORT_EXEC} "$dep_file" >> "$sorted_file" || main.dieGracefully "I could not figure out the correct order to execute the tasks.\nMost probably there is a cycle (Module A depends on B which in turn depends on A)"
			
		fi
		
		main.log -a -B "We will execute the tasks in this order:"
		cat "$sorted_file" | tee -a "$CXR_LOG" 
		
		main.log -a "Updating ranks in tasks DB $CXR_STATE_DB_FILE...\n"
		
		tempfile="$(common.runner.createTempFile $FUNCNAME)"
		echo "BEGIN TRANSACTION;" > $tempfile
		
		while read line 
		do
			# We need to parse the line
			# this sets a couple of _variables
			common.task.parseId "$line"

			# Write Update statement to file

			echo "UPDATE tasks SET rank=$current_id WHERE status IS NOT '$CXR_STATUS_SUCCESS';" >> $tempfile

			# Increase ID
			current_id=$(( $current_id + 1 ))

		done < "$sorted_file"
		
		echo "COMMIT TRANSACTION" > $tempfile
		
		# Execute all statements at once
		${CXR_SQLITE_EXEC} "$CXR_STATE_DB_FILE"  < $tempfile || dieGracefully "Could not update ranks properly"
		
		main.log -v  "This run consists of $(( $current_id -1 )) tasks."
		
		# pdf_file=$CXR_RUN_DIR/${CXR_RUN}_dep_$(date +"%Y_%m_%d_%H_%M").pdf
		# common.task.drawDependencyGraph "$dep_file" "$pdf_file"

	fi # Multiple mode?
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

	common.task.init
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is "$(common.task.getId test 0 2)" "${CXR_START_DATE}@test@2" "common.task.getId normal"
	
	# Testing parser. 
	common.task.parseId ${CXR_START_DATE}@convert_emissions@2
	is "$_module" convert_emissions "common.task.parseId - module name"
	is "$_day_offset" 0 "common.task.parseId only module - day offset"
	is "$_invocation" 2 "common.task.parseId only module - invocation"
	

	########################################
	# teardown tests if needed
	########################################
	
}