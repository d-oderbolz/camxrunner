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
# Prepares a pool of tasks, which are then harvested by common.parallel.Worker threads. 
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
# Function: common.parallel.createDependencyList
# 
# Collects all dependencies of the modules to be executed in a file of the form
# independent_module dependent_module
# If a module has no dependencies, then the line is
# independent_module independent_module
# (see man tsort for details)
#
# Hashes:
# CXR_MODULE_TYPE_HASH ($CXR_HASH_TYPE_UNIVERSAL) - maps module names to their type
#
# CXR_ACTIVE_ALL_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active modules (dummy value)
# CXR_ACTIVE_ONCE_PRE_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active One-Time preprocessing modules (dummy value)
# CXR_ACTIVE_DAILY_PRE_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active daily preprocessing modules (dummy value)
# CXR_ACTIVE_MODEL_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active model modules (dummy value)
# CXR_ACTIVE_DAILY_POST_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active daily postprocessing modules (dummy value)
# CXR_ACTIVE_ONCE_POST_HASH ($CXR_HASH_TYPE_GLOBAL) - contains all active One-Time postprocessing modules (dummy value)
#
# Parameters:
# $1 - output_file will contain a list of dependencies for tsort to sort
################################################################################
function common.parallel.createDependencyList()
################################################################################
{
	local output_file="$1"
	local module
	local module_type
	local active_hashes
	local active_hash
	local raw_dependencies
	local dependency
	local day_offset
	
	# Add the different module types
	if [[ "$CXR_RUN_PRE_ONCE" == true ]]
	then
		active_hashes="${active_hashes} ${CXR_ACTIVE_ONCE_PRE_HASH}"
	fi
	
	if [[ "$CXR_RUN_PRE_DAILY" == true ]]
	then
		active_hashes="${active_hashes} ${CXR_ACTIVE_DAILY_PRE_HASH}"
	fi
	
	if [[ "$CXR_RUN_MODEL" == true ]]
	then
		active_hashes="${active_hashes} ${CXR_ACTIVE_MODEL_HASH}"
	fi
	
	if [[ "$CXR_RUN_POST_DAILY" == true ]]
	then
		active_hashes="${active_hashes} ${CXR_ACTIVE_DAILY_POST_HASH}"
	fi
	
	if [[ "$CXR_RUN_POST_ONCE" == true ]]
	then
		active_hashes="${active_hashes} ${CXR_ACTIVE_ONCE_POST_HASH}"
	fi
	
	# Loop through the module types in order
	for active_hash in $active_hashes
	do
		# Get all active modules of the current type
		local active_modules="$(common.hash.getKeys $active_hash $CXR_HASH_TYPE_GLOBAL)"
		
			# Loop through days
			for day_offset in $(seq 0 $((${CXR_NUMBER_OF_SIM_DAYS} -1 )) )
			do
				for module in $active_modules
				do
					
					# get type (its not efficient here - will fix this later WIHT)
					module_type=$(common.hash.get $CXR_MODULE_TYPE_HASH $CXR_HASH_TYPE_UNIVERSAL $module)
					
					# Get the raw dependencies
					raw_dependencies=$(common.module.getRawDependencies $module)
					
					# resolve them
					resolved_dependencies=$(common.module.resolveAllDependencies $module )
					
					# Are there any?
					if [[ "$resolved_dependencies" ]]
					then
						# Loop 
						for dependency in $resolved_dependencies
						do
							echo "$dependency $module" >> "$output_file"
						done
					else
						# Add the module twice (see header)
						echo "$module $module" >> "$output_file"
					fi
				done # current active modules
				
				if [[ "$module_type" == "$CXR_TYPE_PREPROCESS_ONCE" || "$module_type" == "$CXR_TYPE_POSTPROCESS_ONCE" ]]
				then
					# For these module types, time is not important, so we can break after one iteration
					break
				fi
				
			done # days
	done # hashes of active modules
}

################################################################################
# Function: common.parallel.drawDependencyGraph
# 
# Creates an image of the dependency graphy using dot (graphviz)
# 
# Parameters:
# $1 - a file describing the dependencies in tsort format
# [$2] - an output file (PDF)
################################################################################
function common.parallel.drawDependencyGraph()
################################################################################
{
	local input_file="$1"
	local output_file="${2:-$CXR_RUN_DIR/${CXR_RUN}_dep_$(date +"%Y_%m_%d_%H_%M").pdf}"
	local dot_file=$(common.runner.createTempFile $FUNCNAME)
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
			main.log -v  "$indep equals $dep"
		fi
	
	done < "${input_file}"
	
	echo "}" >> "$dot_file"
	
	# Now call dot
	${CXR_DOT_EXEC} -Tpdf "${dot_file}" -o "${output_file}" 2>&1 | tee -a $CXR_LOG
	
	if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
	then
		main.log -e  "Could not visualize the dependencies."
	else	
		main.log -a  "You find a visualisation of the modules dependencies in the file ${output_file}"
	fi
}

################################################################################
# Function: common.parallel.countOpenTasks
#
# Returns the number of open tasks. 
# Make sure you call this in a critical section (lock acquired), otherwise 
# a race condition might occur
# 
#
################################################################################
function common.parallel.countOpenTasks()
################################################################################
{
	# Find all links below CXR_TASK_TODO_DIR
	task_count="$(find "$CXR_TASK_TODO_DIR" -noleaf -type l 2>/dev/null | wc -l)"
	
	main.log -v   "Found $task_count open tasks"
	
	echo "$task_count"
}


################################################################################
# Function: common.parallel.getNextTask
#
# Returns the content of the task descriptor for the next task to execute.
# If there are no more tasks or a module runs exclusively, the empty string is returned.
# If all tasks are executed, deletes the continue file.
#
################################################################################
function common.parallel.getNextTask()
################################################################################
{
	local task_count
	local potential_task
	local skip_it
	local descriptor
	local new_descriptor_name
	
	# If the system wants exclusive access to all processors,
	# (e. g. to run the model in parallel mode) do not give out new assignments
	if [[ "$CXR_BLOCK_ASSIGNMENTS" == true  ]]
	then
		main.log -v   "Currently, no new tasks are being assigned. Probably the model is running"
		
		echo ""
		return $CXR_RET_OK
	fi 

	# Entering critical section...
	common.runner.getLock common.parallel.getNextTask
	
	task_count=$(common.parallel.countOpenTasks)
	
	# Are there open tasks at all?
	if [[ "$task_count" -eq 0  ]]
	then
		main.log  "All tasks have been processed, notifing system..."
		# there are no more tasks, remove all continue file
		common.state.deleteContinueFiles
		echo ""
		
		# Release lock
		common.runner.releaseLock common.parallel.getNextTask
		return $CXR_RET_OK
	else
		main.log -v   "There are $task_count unfinished tasks - we choose the top one."
	fi
	
	# get first file
	potential_task="$(find "$CXR_TASK_TODO_DIR" -noleaf -type l 2>/dev/null | head -n1)"
	
	# Check status
	if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
	then
		main.die_gracefully "could not find next task!"
	fi
	
	# It can be that we did not get a string
	if [[ -z "$potential_task" ]]
	then
		# No task!
		main.log -v  "Did not find any task..."
		echo ""
		
		# Release lock
		common.runner.releaseLock common.parallel.getNextTask
		
		return $CXR_RET_OK
	else
		# We got a task
		# Immediately assign it, so that we can leave the critical section
		new_descriptor_name=$CXR_TASK_RUNNING_DIR/${task_pid}_$(basename $potential_task)
		mv $potential_task $new_descriptor_name
		
		# Release lock
		common.runner.releaseLock common.parallel.getNextTask
		
		main.log -v   "New descriptor is $new_descriptor_name"
		
		# Return content
		cat "$new_descriptor_name"
		return $CXR_RET_OK
	fi
}

################################################################################
# Function: cxr_common_task_change_status
#
# Just moves the task descriptor (used only for the users reference).
# As a precaution, we also notify the state DB on error (all modules should do this!)
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
		main.die_gracefully "needs a task descriptor and a status as input"
	fi
	
	task_descriptor_path="$1"
	task_descriptor="$(basename "$task_descriptor_path")"
	status="$2"
	
	case $status in
	
		$CXR_STATUS_SUCCESS) 
			DIRECTORY="$CXR_TASK_SUCCESSFUL_DIR";;
			
		$CXR_STATUS_FAILURE) 
			DIRECTORY="$CXR_TASK_FAILED_DIR"
			
			# Notify state DB
			common.state.storeState ${CXR_STATE_ERROR}
			;;
			
		*)
			main.die_gracefully "status $status not supported!"
	
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
# $1 - task_pid of common.parallel.Worker
################################################################################
function cxr_common_parallel_worker_waiting()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		main.die_gracefully "needs a task_pid as input"
	fi
	
	local task_pid=$1
	
	rm -f $CXR_RUNNING_WORKER_DIR/$task_pid >/dev/null 2>&1
	
	touch $CXR_WAITING_WORKER_DIR/$task_pid
	
	main.log -v   "common.parallel.Worker (task_pid: $task_pid) changed its state to waiting"
	
	
}

################################################################################
# Function: common.parallel.workingWorker
#
# Add a task_pid file in the CXR_RUNNING_WORKER_DIR
# Remove it from CXR_WAITING_WORKER_DIR
#
# Parameters:
# $1 - task_pid of common.parallel.Worker
################################################################################
function common.parallel.workingWorker()
################################################################################
{
	
	
	if [[ $# -ne 1  ]]
	then
		main.die_gracefully "needs a task_pid as input"
	fi
	
	local task_pid=$1
	
	rm -f $CXR_WAITING_WORKER_DIR/$task_pid >/dev/null 2>&1
	
	touch $CXR_RUNNING_WORKER_DIR/$task_pid
	
	main.log -v   "common.parallel.Worker (task_pid: $task_pid) changed its state to working"
	
	
}

################################################################################
# Function: common.parallel.removeWorker
#
# kills the common.parallel.Worker of the given task_pid and alse removes it from the process list.
# For this the task_pid is parsed
################################################################################
function common.parallel.removeWorker()
################################################################################
{
	
	
	local task_pid
	local pid
	local node
	
	if [[ $# -ne 1  ]]
	then
		main.die_gracefully "needs a task_pid as input"
	fi
	
	# Remove identifier
	rm -f $CXR_WORKER_DIR/$task_pid
	
	task_pid=$1
	
	pid=$(echo $task_pid | cut -d_ -f1)
	node=$(echo $task_pid | cut -d_ -f2)
	
	if [[ "$node" != "$(uname -n)"  ]]
	then
		main.log  "Strange: $task_pid seems to run on $node rather than on $(uname -n)"
		return $CXR_RET_ERROR
	fi
	
	# Add check if the process is running at all!
	kill $pid 2>/dev/null
	
	# We do not care if the process was waiting or running
	rm -f $CXR_WAITING_WORKER_DIR/$task_pid >/dev/null 2>&1
	rm -f $CXR_RUNNING_WORKER_DIR/$task_pid >/dev/null 2>&1
}

################################################################################
# Function: common.parallel.Worker
#
# This function is the workhorse of the parallel CAMxRunner. The Runner spawns one 
# or more of this functions to operate on the existing tasks.
# This can even be done from more than one machine.
#
# The worker gets a new task via <common.parallel.getNextTask>
# then waits (using <cxr_common_parallel_raw_dependencies_ok?>)
# until the dependencies of this task are fullfilled. 
################################################################################
function common.parallel.Worker()
################################################################################
{
	local tmp
	local task_pid
	local new_task
	local oIFS
	local descriptor
	local task
	local exclusive
	local raw_dependencies
	
	#Getting the pid is not easy, we do not want to many processes...
	tmp=$(common.runner.createTempFile $FUNCNAME)
	
	# The pid is the parent of the awk process
	# and the 4th field of /proc/self/stat is the PPID
	awk '{print $4}' /proc/self/stat > $tmp
	# We add the machine name so that it is unique among all machines
	task_pid=$(cat $tmp)_$(uname -n)
	
	# Create a file identifying the common.parallel.Worker in the common.parallel.Worker dir
	touch $CXR_WORKER_DIR/$task_pid
	
	main.log -a -B  "parallel worker (task_pid $task_pid) starts..."

	# Do we have more than 1 process?
	# If so, define process-specific stuff
	if [[  "$CXR_MAX_PARALLEL_PROCS" -gt 1 && "$CXR_DO_FILE_LOGGING" == true ]]
	then
		# Set task_pid-dependent logfile to disentangle things
		CXR_LOG=${CXR_LOG%.log}_${task_pid}.log
		
		main.log  "This common.parallel.Worker will use its own logfile: ${CXR_LOG}"
	fi

	# We are not yet busy
	cxr_common_parallel_worker_waiting $task_pid

	# We stay in this loop as long as the continue file exists
	while [[ -f "$CXR_CONTINUE_FILE" ]]
	do
		# Do we stop here?
		common.state.doContinue? || main.die_gracefully "Continue file no longer present."
	
		# common.parallel.getNextTask must provide tasks in an atomic fashion (locking needed)
		# already moves the task descriptor into "running" position
		# we get something like "create_emissions0"
		new_task="$(common.parallel.getNextTask)"
		
		# If we are on wait state, we get the empty string back
		if [[ "$new_task_descriptor" ]]
		then
		
			main.log -a  "New task received: $new_task_descriptor"
			
			######################
			# Parse the task
			######################
			
			# Remove potential digits
			module_name=$(expr match "$line" '\(\<[_a-z]\{1,\}\)')
			# get only the digits at the end
			day_offset=$(expr match "$line" '.*\([0-9]\{1,\}\>\)')
	
	
			if [[ "$(common.hash.has? $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module)" == true ]]
			then
				module_path="$(common.hash.get $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module)"
			else
				main.die_gracefully "cannot find path of $module"
			fi
			
			exclusive="$(common.module.getExclusive $module_name)"
			raw_dependencies="$(common.module.getRawDependencies $module_name)"
			
			# We need to wait until all dependencies are ok
			cxr_common_parallel_worker_waiting $task_pid
			
			until [[ "$(cxr_common_parallel_raw_dependencies_ok? "$raw_dependencies" "$day_offset" )" == true ]]
			do
				sleep $CXR_WAITING_SLEEP_SECONDS
			done
			
			# Time to work
			common.parallel.workingWorker $task_pid
			
			main.log -v   "task: $task\nDAY_OFFSET: $day_offset\nEXCLUSIVE: $exclusive"
			
			#Reserve resources if needed
			if [[ "$exclusive" == true  ]]
			then
				main.log  "This task needs exclusive access, we suspend the assignment of new tasks temporarily"
				CXR_BLOCK_ASSIGNMENTS=true
			fi
		
			# Setup environment
			common.date.setVars "$CXR_START_DATE" "$day_offset"
			
			main.log -B   "common.parallel.Worker $task_pid assigned to $task for $CXR_DATE"
			
			# Before loading a new module, remove old meta variables
			unset ${!CXR_META_MODULE*}
			
			# Export the module name
			CXR_META_MODULE_NAME=$(main.getModuleName $task)
			
			# source the file to get the rest of the metadata
			source $module_path
			
			# Now start the work.
			# The function we use is the Module name (derived from the file name)
			# We use the return status to determine if it was successful
			$CXR_META_MODULE_NAME 
			
			# Change status according to retval
			if [[ $? -eq 0 ]]
			then
				# success
				cxr_common_task_change_status $new_task_descriptor $CXR_STATUS_SUCCESS
			else
				# failure
				cxr_common_task_change_status $new_task_descriptor $CXR_STATUS_FAILURE
			fi

			#Release resources if needed
			if [[ "$exclusive" == true  ]]
			then
				main.log  "Activating the assignment of new tasks again."
				CXR_BLOCK_ASSIGNMENTS=false
			fi
		else
			# If we don't get anything, but we should, we terminate
			if [[ $CXR_BLOCK_ASSIGNMENTS == false ]]
			then
				main.log -v  "common.parallel.Worker $task_pid did not receive an assignment - there seem to be too many workers (starvation)"
				common.parallel.removeWorker "$task_pid"
			fi
		
			# This means that someone wants exclusive access, so we sleep
			cxr_common_parallel_worker_waiting $task_pid
			# Sleep a bit
			sleep $CXR_WAITING_SLEEP_SECONDS
		fi
	done
	
	# We have done our duty
	common.parallel.removeWorker $task_pid

	exit $CXR_RET_OK
}

################################################################################
# Function: common.parallel.spawnWorkers
#
# Parameters:
# $1 - number of workers to spawn
################################################################################
function common.parallel.spawnWorkers()
################################################################################
{
	main.log  "We create now $1 common.parallel.Worker threads"
	
	for i in $(seq 1 $1)
	do
		# Create a common.parallel.Worker and send it to background		
		common.parallel.Worker &
	done
}

################################################################################
# Function: common.parallel.removeAllWorkers
# 
# Removes all workers
# 
################################################################################
function common.parallel.removeAllWorkers()
################################################################################
{
	main.log  "We remove all workers now."
	
	for task_pid in $(find "$CXR_WORKER_DIR" -noleaf -name \*_$(uname -n) )
	do
		common.parallel.removeWorker "$(basename $task_pid)"
	done
}

################################################################################
# Function: common.parallel.WaitForWorkers
#
# Basically a sleep function: we loop and check if the continue file is there.
#
################################################################################
function common.parallel.WaitForWorkers()
################################################################################
{
		main.log  "Entering a wait loop (the work is carried out by backgound processes. I check every $CXR_WAITING_SLEEP_SECONDS seconds if all is done.)"
		
		while [ -f "$CXR_CONTINUE_FILE" ]
		do
			sleep $CXR_WAITING_SLEEP_SECONDS
		done
		
		main.log -B   "The Continue file is gone, all workers will stop asap."
		
		# OK, remove the workers now
		common.parallel.removeAllWorkers
}

################################################################################
# Function: common.parallel.init
# 
# Creates a sorted lists of files in the CXR_TASK_POOL_DIR directory and
# links to these in the CXR_TASK_TODO_DIR.
# 
# Hashes:
# CXR_MODULE_PATH_HASH ($CXR_HASH_TYPE_UNIVERSAL) - maps module names to their path
# 
################################################################################
function common.parallel.init()
################################################################################
{
	# Check if we already have tasks - fail if this is the case
	if [[ $(find "$CXR_TASK_POOL_DIR" -noleaf -maxdepth 1 -type f 2>/dev/null | wc -l ) -ne 0  ]]
	then
		main.log  "There is already a tasklist - we will use it.\nIf you want to start from scratch, delete all state info using\n ${CXR_CALL} -c\n"
		return 0
	fi
	
	## Create the todo dir if needed
	if [[ ! -d "$CXR_TASK_TODO_DIR" ]]
	then
		mkdir -p "$CXR_TASK_TODO_DIR"
	fi
	
	# Some tempfiles we need
	local dep_file="$(common.runner.createTempFile dependencies)"
	local sorted_file="$(common.runner.createTempFile tsort-out)"
	
	# Reset the ID counter
	local current_id=1
	local task_file
	
	main.log -a  "Creating a list of dependencies..."
		
	common.parallel.createDependencyList "$dep_file"
	
	main.log -a  "Ordering tasks..."
	
	${CXR_TSORT_EXEC} "$dep_file" > "$sorted_file"
	
	if [[ $? -ne 0 ]]
	then
		main.die_gracefully "I could not figure out the correct order to execute the tasks. Most probably there is a cycle (Module A depends on B which in turn depends on A)"
	fi
	
	main.log -a  "Creating todo-structure"
	
	while read line 
	do
		task_file=$CXR_TASK_POOL_DIR/$(printf "%0${CXR_TASK_ID_DIGITS}d" $current_id)
	
		# Is this a unique number?
		if [[ -f "$task_file" ]]
		then
			main.die_gracefully "The task Id $current_id is already taken"
		fi
		
		# each line contains something like "create_emissions0" or "initial_conditions"
		# we just put it in a file
		
		echo $line > "$task_file"
		
		# Create link in todo dir
		(
			cd $CXR_TASK_TODO_DIR || main.die_gracefully "Could not change to dir $CXR_TASK_TODO_DIR"
	
			# General
			ln -s $task_file
		)
		
		# Increase ID
		current_id=$(( $current_id + 1 ))
	
	done < "$sorted_file"
	
	main.log -a  "This run consists of $(( $current_id -1 )) tasks."
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
	local dep_file=$(common.runner.createTempFile $FUNCNAME)
	
	# This is just to play
	echo "create_emissions_0 model_0" >> "${dep_file}"
	echo "convert_emissions_0 model_0" >> "${dep_file}"
	echo "convert_input_0 model_0" >> "${dep_file}"
	echo "initial_conditions model_0" >> "${dep_file}"
	echo "boundary_conditions_0 model_0" >> "${dep_file}"
	
	pdf_file=$CXR_RUN_DIR/${CXR_RUN}_dep_$(date +"%Y_%m_%d_%H_%M").pdf
	common.parallel.drawDependencyGraph "$dep_file" "$pdf_file"
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is "$(common.fs.isNotEmpty? "$pdf_file")" true "common.parallel.drawDependencyGraph simple existence check. Look at $pdf_file "

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
	