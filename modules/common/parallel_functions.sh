# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Functions to manage parallel execution of modules.
# The most important aspect of this is the management of the varius dependencies between modules.
#
# Prepares a pool of tasks, which are then harvested by Worker threads. 
# These threads can run parallel - even on different machines.
#
# First, a list of tasks is generated.
# This list is sorted according to the dependencies (topological sorting), so that tasks with no or few
# dependencies appear first -> execution plan.
# Then, we create a number of workers, which each get a unique entry of this list. 
# They check first if the dependencies are already fulfilled 
# if not they wait otherwise, the task starts.
# After successful execution, the worker gets the next task from the list.
# A task is identified by its module name, a day offset, and a invocation id like create_emissions1@1. 
# One-Time Tasks have no day offset, like initial_conditions@1. 
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

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=3

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

################################################################################
# Function: common.parallel.formatDependency
#
# Creates a proper dependency string out of several inputs.
#
# Example:
# > echo "${dependency} $(common.parallel.formatDependency "$module_name" "$module_type" "$day_offset" "$iInvocation" "$nInvocations")" >> $output_file
# 
# Parameters:
# $1 - module name
# $2 - module type
# $3 - day offset
# $4 - iInvocation
# $5 - nInvocations
################################################################################
function common.parallel.formatDependency()
################################################################################
{
	local module_name="${1}"
	local module_type="${2}"
	local day_offset="${3}"
	local iInvocation="${4}"
	local nInvocations="${5}"

	# Create dependency string
	if [[ "$module_type" == "${CXR_TYPE_MODEL}" ||\
	      "$module_type" == "${CXR_TYPE_PREPROCESS_DAILY}" ||\
	      "$module_type" == "${CXR_TYPE_POSTPROCESS_DAILY}" ]]
	then
		# With day offset
		
		# Add invocation only if there is more than one
		if [[ $nInvocations -gt 1 ]]
		then
			echo "${module_name}${day_offset}@${iInvocation}" 
		else
			echo "${module_name}${day_offset}"
		fi
	else
		# Without day offset
		
		# Add invocation only if there is more than one
		if [[ $nInvocations -gt 1 ]]
		then
			echo "${module_name}@${iInvocation}"
		else
			echo "${module_name}"
		fi
	fi
}


################################################################################
# Function: common.parallel.createDependencyList
# 
# Collects all dependencies (resolved) of the modules to be executed in a file of the form
# independent_module dependent_module.
# The actual resolving is done in <common.module.resolveAllDependencies>.
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
# $1 - output_file to write list of dependencies for tsort to sort to
################################################################################
function common.parallel.createDependencyList()
################################################################################
{
	local output_file="$1"
	local tempfile
	local module
	local module_type
	local active_hashes
	local active_hash
	local raw_dependencies
	local dependency
	local day_offset
	local iInvocation
	local nInvocation
	
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
		oIFS="$IFS"
		local keyString="$(common.hash.getKeys $active_hash $CXR_HASH_TYPE_GLOBAL)"
		IFS="$CXR_DELIMITER"
		# Turn string into array (we cannot call <common.hash.getKeys> directly here!)
		local activeModuleKeys=( $keyString )
		# Reset Internal Field separator
		IFS="$oIFS"
		
		local active_modules=""
		
		# Re-determine module type
		case $active_hash in
			${CXR_ACTIVE_ONCE_PRE_HASH}) 	module_type="${CXR_TYPE_PREPROCESS_ONCE}";;
			${CXR_ACTIVE_DAILY_PRE_HASH}) 	module_type="${CXR_TYPE_PREPROCESS_DAILY}";; 
			${CXR_ACTIVE_MODEL_HASH}) 		module_type="${CXR_TYPE_MODEL}";;
			${CXR_ACTIVE_DAILY_POST_HASH}) 	module_type="${CXR_TYPE_POSTPROCESS_DAILY}";;
			${CXR_ACTIVE_ONCE_POST_HASH}) 	module_type="${CXR_TYPE_POSTPROCESS_ONCE}";;
		esac
		
		# Loop through all active modules of this type
		for iKey in $( seq 0 $(( ${#activeModuleKeys[@]} - 1)) )
		do
			module="${activeModuleKeys[$iKey]}"

			# Get the raw dependencies
			raw_dependencies="$(common.module.getRawDependencies $module)"
			
			main.log -v "$module depedends on ${raw_dependencies:--}"
		
			# Loop through days
			for day_offset in $(seq 0 $((${CXR_NUMBER_OF_SIM_DAYS} -1 )) )
			do
				# Give some visual feedback
				common.user.showProgress
				
				# We must resolve for each invocation
				nInvocations=$(common.module.getNumInvocations "$module")
				
				# resolve the dependencies
				resolved_dependencies="$(common.module.resolveAllDependencies "$raw_dependencies" $day_offset )"
				
				for iInvocation in $(seq 1 $nInvocations )
				do
					# Are there any?
					if [[ "$resolved_dependencies" ]]
					then
						# Loop 
						for dependency in $resolved_dependencies
						do
							echo "${dependency} $(common.parallel.formatDependency "$module" "$module_type" "$day_offset" "$iInvocation" "$nInvocations")" >> "$output_file"
						done # Dependencies
					else
						# Add the module twice (see header), including all invocations
						local dep_string="$(common.parallel.formatDependency "$module" "$module_type" "$day_offset" "$iInvocation" "$nInvocations")"
						echo $dep_string $dep_string >> "$output_file"
					fi
				done # invocations
				
			done # days
				
		done # current active modules

	done # hashes of active modules
	
	main.log -v "Removing duplicates..."
	tempfile="$(common.runner.createTempFile $FUNCNAME)"
	
	sort "$output_file" | uniq > "$tempfile"
	mv "$tempfile" "$output_file"
}

################################################################################
# Function: common.parallel.drawDependencyGraph
# 
# Creates an image of the dependency graphy using dot (graphviz)
# 
# Parameters:
# $1 - a file describing the dependencies in tsort format
# [$2] - an output file (extension must be any suported Graphviz filetype like pdf, ps, svg) 
# see also <http://www.graphviz.org/doc/info/output.html>
################################################################################
function common.parallel.drawDependencyGraph()
################################################################################
{
	local input_file="$1"
	local output_file="${2:-$CXR_RUN_DIR/${CXR_RUN}_dep_$(date +"%Y_%m_%d_%H_%M").pdf}"
	local dot_file=$(common.runner.createTempFile $FUNCNAME)
	local elements
	
	# Extract the filetype (lowercase)
	local extension="$(common.string.toLower "${output_file##*.}")"
	
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
	
	main.log -v "Found $task_count open tasks"
	
	echo "$task_count"
}

################################################################################
# Function: common.parallel.detectLockup
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
function common.parallel.detectLockup()
################################################################################
{
	local count
	local numRunning
	
	if [[ $(common.hash.has? Lockup $CXR_HASH_TYPE_GLOBAL LockupCount) == true ]]
	then
		count=$(common.hash.get Lockup $CXR_HASH_TYPE_GLOBAL LockupCount)
	else
		count=0
	fi
	
	# Count the running workers
	numRunning=$(find $CXR_RUNNING_WORKER_DIR -noleaf -maxdepth 1 -type f 2>/dev/null | wc -l)
	
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
# Function: common.parallel.setNextTask
#
# Returns the full path of the task descriptor for the next task to execute in an environment variable.
# We protect this form concurrent access using locking (instance level).
# If there are no more tasks, the empty string is returned.
# If all tasks are executed, deletes the continue file.
#
# The task is parsed in here, and we set these _vars:
#
# Output variables:
# _new_task_descriptor
# _exclusive
# _module_name
# _day_offset
# _invocation
#
################################################################################
function common.parallel.setNextTask()
################################################################################
{
	# Acquire lock
	if [[ $(common.runner.getLock NextTask "$CXR_HASH_TYPE_INSTANCE") == false ]]
	then
		main.dieGracefully "Waiting for NextTask lock took too long"
	fi
	
	local task_count
	local potential_task
	local skip_it
	local descriptor
	local new_descriptor_name
	

	task_count=$(common.parallel.countOpenTasks)

	# Are there open tasks at all?
	if [[ "$task_count" -eq 0  ]]
	then
		main.log  "All tasks have been processed, notifying system after security pause..."
		
		# there are no more tasks, remove all continue files after some waiting
		# The waiting should ensure that all tasks aro past their check for do_we_continue
		sleep $CXR_WAITING_SLEEP_SECONDS
		
		common.state.deleteContinueFiles
		common.runner.releaseLock NextTask "$CXR_HASH_TYPE_INSTANCE"
		echo ""
		return $CXR_RET_OK
		
	else
		main.log -v "There are $task_count unfinished tasks - we choose the top one."
	fi
	
	# get first file in the TODO directory
	potential_task="$(find "$CXR_TASK_TODO_DIR" -noleaf -type l 2>/dev/null | head -n1)"
	
	# Check status
	if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
	then
		main.dieGracefully "could not find next task!"
	fi
	
	# we might not get a string
	if [[ -z "$potential_task" ]]
	then
		# No task!
		main.dieGracefully "could not find next task!"
	else
		# We got a task
		# Assign it by moving
		new_descriptor_name=$CXR_TASK_RUNNING_DIR/${task_pid}_$(basename $potential_task)
		mv $potential_task $new_descriptor_name
		
		main.log -v "New descriptor is $new_descriptor_name"
		
		# We need to parse the task
		# this sets a couple of _variables
		common.module.parseIdentifier "$(cat $new_descriptor_name)"
		
		# Test if it needs exclusive access
		_exclusive="$(common.module.getExclusive "$_module_name")"
		
		# Return content
		_new_task_descriptor="$new_descriptor_name"
	fi

	# Release lock
	common.runner.releaseLock NextTask "$CXR_HASH_TYPE_INSTANCE"
}

################################################################################
# Function: common.parallel.changeTaskStatus
#
# Just moves the task descriptor (used only for the users reference).
# As a precaution, we also notify the state DB on error (all modules should do this!)
#
# Parameters:
# $1 - descriptor file of task
# $2 - status (SUCCESS/FAILURE)
################################################################################
function common.parallel.changeTaskStatus()
################################################################################
{
	local task_descriptor_path
	local task_descriptor
	local status
	
	if [[ $# -ne 2  ]]
	then
		main.dieGracefully "needs a task descriptor and a status as input"
	fi
	
	task_descriptor_path="$1"
	task_descriptor="$(basename "$task_descriptor_path")"
	status="$2"
	
	main.log -v "Changing status of $task_descriptor_path to $status"
	
	case $status in
	
		$CXR_STATUS_SUCCESS) 
			DIRECTORY="$CXR_TASK_SUCCESSFUL_DIR";;
			
		$CXR_STATUS_FAILURE) 
			DIRECTORY="$CXR_TASK_FAILED_DIR";;
			
		*)
			main.dieGracefully "status $status not supported!"
	
	esac
	
	mv "$task_descriptor_path" "$DIRECTORY/$task_descriptor"
}

################################################################################
# Function: common.parallel.waitingWorker
#
# Add a task_pid file in the CXR_WAITING_WORKER_DIR
# Remove it from CXR_RUNNING_WORKER_DIR.
#
# Parameters:
# $1 - task_pid of common.parallel.Worker
################################################################################
function common.parallel.waitingWorker()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully "needs a task_pid as input"
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
# Remove it from CXR_WAITING_WORKER_DIR if it exists.
# Tests if we are locked up.
#
# Parameters:
# $1 - task_pid of common.parallel.Worker
################################################################################
function common.parallel.workingWorker()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully "needs a task_pid as input"
	fi
	
	local task_pid=$1
	
	# Detect lockup
	common.parallel.detectLockup
	
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
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully "needs a task_pid as input"
	fi
	
	local task_pid=$1
	local pid
	local node
	
	# Remove identifier
	rm -f $CXR_WORKER_DIR/$task_pid

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
# TODO: a worker that finishes must restore a consistent state, that is the current
# task must be "put back"
#
# The worker gets a new task via <common.parallel.setNextTask>
# then waits (using <common.module.areDependenciesOk?>)
# until the dependencies of this task are fullfilled. 
#
# Parameters:
# $1 - the worker id (internal number, just to tell log output on screen apart)
################################################################################
function common.parallel.Worker()
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
	local module_name
	local day_offset
	local start_epoch
	
	#Getting the pid is not easy, we do not want to create unnecessary processes...
	tmp=$(common.runner.createTempFile $FUNCNAME)
	
	# The pid is the parent of the awk process
	# and the 4th field of /proc/self/stat is the Parent PID
	awk '{print $4}' /proc/self/stat > $tmp
	# We add the machine name so that it is unique among all machines
	task_pid=$(cat $tmp)_$(uname -n)
	
	# Create a file identifying the Worker in the $CXR_WORKER_DIR
	touch $CXR_WORKER_DIR/$task_pid
	
	main.log -a -B  "parallel worker (task_pid ${task_pid}, id ${CXR_WORKER_ID} ) starts..."

	# Do we have more than 1 process?
	# If so, define process-specific stuff
	if [[ -f "$CXR_LOG" && "$CXR_MAX_PARALLEL_PROCS" -gt 1 ]]
	then
		# Set task_pid-dependent logfile to disentangle things
		CXR_LOG=${CXR_LOG%.log}_${task_pid}.log
		
		main.log -a "This common.parallel.Worker will use its own logfile: ${CXR_LOG}"
	fi

	# We stay in this loop as long as the continue file exists
	while [[ -f "$CXR_CONTINUE_FILE" ]]
	do
		# Do we stop here?
		common.state.doContinue? || main.dieGracefully "Continue file no longer present."
		
		# We are not yet busy
		common.parallel.waitingWorker $task_pid
		
		# Is there enough free memory?
		if [[ "$(common.memory.getFreePercent)" -gt ${CXR_MEM_FREE_PERCENT:-0} ]]
		then
			# Enough Memory
			
			# common.parallel.setNextTask provides tasks in an atomic fashion
			# already moves the task descriptor into "running" position
			# Sets a couple of "background" variables
			# This is a blocking call (we wait until we get a task)
			common.parallel.setNextTask
			
			new_task_descriptor=$_new_task_descriptor
			
			# here we get something like "create_emissions0@1" (no path!)
			# as a side effect, _exclusive is set
			
			if [[ -f $new_task_descriptor ]]
			then
				new_task="$(cat $new_task_descriptor)"
			else
				main.log -w "File $new_task_descriptor (new task descriptor) not found"
				new_task=
			fi
			
			# The task name might be empty due to errors
			if [[ "$new_task" ]]
			then
				main.log -v "New task received: $new_task"
				
				######################
				# task was already parsed by common.parallel.setNextTask
				######################
				
				module_name="$_module_name"
				day_offset="$_day_offset"
				invocation="$_invocation"
				exclusive="$_exclusive"
				
				main.log -v "module: $module_name day_offset: $day_offset invocation: $invocation"
				
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
				
				
				if [[ "$(common.hash.has? $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module_name)" == true ]]
				then
					module_path="$(common.hash.get $CXR_MODULE_PATH_HASH $CXR_HASH_TYPE_UNIVERSAL $module_name)"
				else
					main.dieGracefully "cannot find path of $module_name"
				fi
				
				raw_dependencies="$(common.module.getRawDependencies $module_name)"
				
				start_epoch=$CXR_EPOCH
				
				# We need to wait until all dependencies are ok
				until [[ "$(common.module.areDependenciesOk? "$raw_dependencies" "$day_offset" )" == true ]]
				do
					main.log -v "Waiting for dependencies of $module_name to be done for day $day_offset"
					
					# Tell the system we wait, then sleep
					common.parallel.waitingWorker $task_pid
					sleep $CXR_WAITING_SLEEP_SECONDS
					
					if [[ $(( $(date "+%s") - $start_epoch )) -gt $CXR_DEPENDECY_TIMEOUT_SEC ]]
					then
						main.dieGracefully "It took longer than CXR_DEPENDECY_TIMEOUT_SEC ($CXR_DEPENDECY_TIMEOUT_SEC) seconds to fullfill the dependencies of $module_name for day $day_offset"
					fi
				done
				
				# Time to work
				common.parallel.workingWorker $task_pid
				
				main.log -v "module: $module_name day_offset: $day_offset invocation: $invocation exclusive: $_exclusive"
				
				# Setup environment
				common.date.setVars "$CXR_START_DATE" "${day_offset:-0}"
				
				main.log -a -B "common.parallel.Worker $task_pid assigned to $module_name for $CXR_DATE"
				
				# Before loading a new module, remove old meta variables
				unset ${!CXR_META_MODULE*}
				
				# Export the module name
				CXR_META_MODULE_NAME=${module_name}
				
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
				&& common.parallel.changeTaskStatus $new_task_descriptor $CXR_STATUS_SUCCESS \
				|| common.parallel.changeTaskStatus $new_task_descriptor $CXR_STATUS_FAILURE
							
				# Stop Timing 
				common.performance.stopTiming $CXR_META_MODULE_NAME
				
				# There is a potential race-condition here...
				# It is not critical, since we use this only for ETA estimations
				CXR_TASKS_DONE=$(( $CXR_TASKS_DONE + 1 ))
	
				#Release resources if needed
				if [[ "$_exclusive" == true ]]
				then
					main.log  "Activating the assignment of new tasks again."
					common.runner.releaseLock Exclusive "$CXR_HASH_TYPE_GLOBAL"
				fi
			else
				main.log -v  "Worker $task_pid did not receive an assignment - maybe there are too many workers around"
				
				# This means that someone wants exclusive access
				# Tell the system we wait, then sleep
				common.parallel.waitingWorker $task_pid
				sleep $CXR_WAITING_SLEEP_SECONDS
				
			fi # Got a task?
		
		else
			# Not enough memory
			main.log -w "Worker $task_pid detected that we have less than ${CXR_MEM_FREE_PERCENT} % of free memory. We wait..."
			
			# Tell the system we wait, then sleep
			common.parallel.waitingWorker $task_pid
			sleep $CXR_WAITING_SLEEP_SECONDS
		fi # Enough Memory?
			
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
	local iWorker
	
	# The control thread is "Worker 0"
	CXR_WORKER_ID=0
	
	main.log  "We now create $1 worker threads"
	
	for iWorker in $(seq 1 $1)
	do
		# Create a worker and send it to background
		common.parallel.Worker $iWorker &
		
		# Wait a bit to avoid congestion
		main.log -a "We wait 60 seconds until we launch the next worker to see the memory demand"
		sleep 60
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
# Function: common.parallel.cleanTasks
#
# Deletes the contents of the task pool and the workers directories.
#
#
################################################################################
function common.parallel.cleanTasks()
################################################################################
{
	main.log -v "Removing files in ${CXR_TASK_POOL_DIR} and ${CXR_WORKER_DIR}"
	
	find ${CXR_TASK_POOL_DIR} -noleaf -exec rm -f \{\} \; 2>/dev/null
	find ${CXR_WORKER_DIR} -noleaf -exec rm -f \{\} \; 2>/dev/null
}
################################################################################
# Function: common.parallel.waitForWorkers
#
# Basically a sleep function: we loop and check if the continue file is there.
#
################################################################################
function common.parallel.waitForWorkers()
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
		common.parallel.removeAllWorkers
}

################################################################################
# Function: common.parallel.init
# 
# Creates a sorted lists of files in the CXR_TASK_POOL_DIR directory and
# links to these in the CXR_TASK_TODO_DIR.
# Tasks that already finished successfully are not added to the Todo dir.
# Unless we allow multiple CAMxRunner instances and we are not the master, 
# we recreate the task infrastructure every run. 
# This works because the information on what did run is in the state db.
# 
# Hashes:
# CXR_MODULE_PATH_HASH ($CXR_HASH_TYPE_UNIVERSAL) - maps module names to their path
# 
################################################################################
function common.parallel.init()
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
	local module_name
	local day_offset
	local invocation
	local module_type
	local raw_date
	local my_stage
	local dep_file
	local sorted_file
	
	main.log -a "Initializing parallel subsystem, might take a while, depending on number of tasks...\n"
	
	# Reset the ID counter
	local current_id=1
	local task_file
	
	# Init
	CXR_TIME_TOTAL_ESTIMATED=0
	
	# Check if we already have tasks 
	# Iff we have this and allow multiple, we use them.
	taskCount=$(find "$CXR_TASK_POOL_DIR" -noleaf -maxdepth 1 -type f 2>/dev/null | wc -l )
	
	if [[	$taskCount -ne 0 && \
			${CXR_ALLOW_MULTIPLE} == true && \
			"$(common.state.countInstances)" -gt 1 ]]
	then
		# We are in a non-master multiple runner
		main.log -a -b "There is already a tasklist - we will use it.\nIf you want to start from scratch, delete all state info using\n ${CXR_CALL} -c\n"
	else
		# Redo everything
		
		# Delete contents, if any
		common.parallel.cleanTasks
		
		## Create the todo dir if needed
		if [[ ! -d "$CXR_TASK_TODO_DIR" ]]
		then
			mkdir -p "$CXR_TASK_TODO_DIR"
		fi
		
		# Some tempfiles we need
		dep_file="$(common.runner.createTempFile dependencies)"
		sorted_file="$(common.runner.createTempFile tsort-out)"
		mixed_file="$(common.runner.createTempFile mixed_tasks)"
		
		main.log -a "\nCreating the list of dependencies...\n"
		
		main.log -a $(date)
		common.parallel.createDependencyList "$dep_file"
		main.log -a $(date)
		
		main.log -a  "\nOrdering tasks...\n"
		${CXR_TSORT_EXEC} "$dep_file" > "$sorted_file"
		
		if [[ $? -ne 0 ]]
		then
			main.dieGracefully "I could not figure out the correct order to execute the tasks. Most probably there is a cycle (Module A depends on B which in turn depends on A)"
		fi
		
		main.log -a -B "We will execute the tasks in this order:"
		cat "$sorted_file" | tee -a "$CXR_LOG" 
		
		main.log -a "\nCreating todo-structure...\n"
		
		while read line 
		do
			# Visual feedback
			common.user.showProgress
			
			task_file=$CXR_TASK_POOL_DIR/$(printf "%0${CXR_TASK_ID_DIGITS}d" $current_id)
		
			# Is this a unique number?
			if [[ -f "$task_file" ]]
			then
				main.dieGracefully "The task Id $current_id is already taken"
			fi
			
			# each line contains something like "create_emissions0@1" or "initial_conditions"
			
			# We need to parse the line
			# this sets a couple of _variables
			common.module.parseIdentifier "$line"
					
			module_name="$_module_name"
			day_offset="$_day_offset"
			invocation="$_invocation"
			
			module_type="$(common.module.getType "$module_name")"
			
			# Convert date
			raw_date="$(common.date.toRaw $(common.date.OffsetToDate "${day_offset:-0}"))"
			
			my_stage="$(common.state.getStageName "$module_type" "$module_name" "$raw_date" "$invocation" )"
			
			# Is this known to have worked?
			if [[ "$(common.state.hasFinished? "$my_stage")" == false ]]
			then
			
				# estimate the runtime
				CXR_TIME_TOTAL_ESTIMATED=$(common.math.FloatOperation "$CXR_TIME_TOTAL_ESTIMATED + $(common.performance.estimateRuntime $module_name)" -1 false)
				
				# we just put it in a file
				echo $line > "$task_file"
				
				# Create link in todo dir
				(
					cd $CXR_TASK_TODO_DIR || main.dieGracefully "Could not change to dir $CXR_TASK_TODO_DIR"
			
					# General
					ln -s $task_file
				)
				
				# Increase ID
				current_id=$(( $current_id + 1 ))
				
			else
				main.log -v "Task $my_stage already finished."
			fi
		
		done < "$sorted_file"
		
		# Set the total number of tasks
		CXR_TASKS_TOTAL=$(( $current_id -1 ))
		CXR_TASKS_DONE=0
		
		main.log -v  "This run consists of $CXR_TASKS_TOTAL tasks."
		
		# pdf_file=$CXR_RUN_DIR/${CXR_RUN}_dep_$(date +"%Y_%m_%d_%H_%M").pdf
		# common.parallel.drawDependencyGraph "$dep_file" "$pdf_file"

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

	common.parallel.init
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is "$(common.parallel.formatDependency test ${CXR_TYPE_MODEL} 1 2 12)" "test1@2" "common.parallel.formatDependency normal"
	is "$(common.parallel.formatDependency test ${CXR_TYPE_MODEL} 1 2 1)" "test1" "common.parallel.formatDependency only one invocation in total"
	is "$(common.parallel.formatDependency test ${CXR_TYPE_MODEL} "" "" "")" "test" "common.parallel.formatDependency empty parameters"
	
	########################################
	# teardown tests if needed
	########################################
	
}