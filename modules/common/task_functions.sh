# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Title: Functions to manage (parallel and sequential) execution of modules.
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
# TODO: 
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
CXR_META_MODULE_DESCRIPTION="Contains the functions to manage task execution"

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
# The format of this ID is sacrosanct: for examlpe we depend on the day_iso to be 
# present. Changing this may cause lots of trouble.
# Must be consistent with <common.state.buildTasksAndDeps>
#
# Format: $date@$module@$invocation
#
# Example:
# > echo "${dependency} $(common.task.getId "$module" "$day_iso" "$iInvocation" )" >> $output_file
# 
# Parameters:
# $1 - module name
# $2 - iso day
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
		date="${CXR_DATE:-${CXR_START_DATE}}"
		invocation="${CXR_INVOCATION:-1}"
	else
		# Use parameters
		module="${1}"
		date=${2:-${CXR_DATE}}
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
# [$2] - no_invocation optional boolean (default false), if true, we accept strings withoin invocation 
################################################################################
function common.task.parseId()
################################################################################
{
	if [[ $# -lt 1 || $# -gt 2 || -z "$1" ]]
	then
		main.dieGracefully "Needs a non-empty identifier and the optional flag no_invocation as Input, got $*"
	fi
	
	local identifier
	local no_invocation
	local -a id_arr
	
	identifier="$1"
	no_invocation="${2:-false}"
	
	main.log -v "Parsing $identifier"
	
	oIFS="$IFS"
	IFS="@"
	
	id_arr=($identifier)
	IFS="$oIFS"
	
	if [[ $no_invocation == true ]]
	then
		if [[ ${#id_arr[@]} -ne 2 ]]
		then
			main.dieGracefully "Malformed task id $identifier"
		else
			_date=${id_arr[0]}
			_module=${id_arr[1]}
			
			_day_offset=$(common.date.toOffset $_date)
			main.log -v "module: $_module date: $_date day_offset: $_day_offset (no invocation)"
		fi
	else
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
	fi
}

################################################################################
# Function: common.task.createSequentialDependencyList
# 
# Performs topological sorting on the dependencies so that the resulting list is
# suitable for sequential harvesting by a single worker. This means that days are
# sorted individually, so that no task of day n+1 is executed before all tasks of
# day n have been executed.
# 
# The output is a file that contains a list of task ids.
#
# Parameters:
# $1 - output_file to write list of task ids to
################################################################################
function common.task.createSequentialDependencyList()
################################################################################
{
	local output_file
	local dep_file
	local nodup_file

	output_file="$1"
	
	main.log -a "Ordering tasks for sequential execution..."
	
	dep_file="$(common.runner.createTempFile dependencies)"
	day_file="$(common.runner.createTempFile dependencies-daily)"
	nodup_file="$(common.runner.createTempFile nodup)"

	# Reset file
	: > "$output_file"

	# In sequential mode, we first sort the One-Time preprocessors,
	# then each day 
	# then the One-Time postprocossors
	
	# In all of these, we ignore - dependencies
	
	######################################
	main.log -v "Ordering $CXR_TYPE_PREPROCESS_ONCE tasks..."
	######################################

	common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" - <<-EOT
	
	-- Prepare proper output
	.output $dep_file
	.separator ' '
	
	------------------------------------
	-- First, add all OT-Pre Tasks 
	-- otherwise tasks without deps would
	-- be skipped
	------------------------------------
	SELECT '$CXR_START_DATE' || '@' || t.module ,
	       '$CXR_START_DATE' || '@' || t.module 
	FROM tasks t, modules m
	WHERE m.module = t.module
	AND   m.type = '$CXR_TYPE_PREPROCESS_ONCE';
	
	------------------------------------
	-- Then add all dependencies.
	------------------------------------
	SELECT '$CXR_START_DATE' || '@' || d.independent_module ,
	       '$CXR_START_DATE' || '@' || d.dependent_module 
	FROM  dependencies d, 
	      modules m
	WHERE m.module = d.dependent_module
	AND   d.independent_day_offset = d.dependent_day_offset
	AND   m.type IN ('$CXR_TYPE_PREPROCESS_ONCE') ;
	
	EOT
	
	main.log -v "Removing duplicates..."
	
	sort "$dep_file" | uniq > "$nodup_file"

	main.log -a "Running tsort on $nodup_file and appending output to $output_file..."
	
	${CXR_TSORT_EXEC} "$nodup_file" >> "$output_file" 
	
	if [[ $? -ne 0 ]]
	then
		main.dieGracefully "I could not figure out the correct order to execute the tasks.\nMost probably there is a cycle (Module A depends on B which in turn depends on A)"
	fi

	######################################
	main.log -v "Ordering daily tasks. First we create the order for day 0..."
	######################################
	
		common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" - <<-EOT
	
	-- Prepare proper output
	.output $dep_file
	.separator ' '
	
	------------------------------------
	-- First, add all Daily Tasks 
	-- otherwise tasks without deps would
	-- be skipped
	------------------------------------
	SELECT  t.module ,
	        t.module 
	FROM tasks t, modules m
	WHERE m.module = t.module
	AND   m.type IN ('$CXR_TYPE_PREPROCESS_DAILY',
	                 '$CXR_TYPE_MODEL',
	                 '$CXR_TYPE_POSTPROCESS_DAILY');
	
	------------------------------------
	-- Then add all the dependencies
	-- we need to restrict the dependencies to
	-- the daily modules.
	------------------------------------
	SELECT  independent_module,
	        dependent_module
	FROM dependencies, modules m, modules im
	WHERE m.module = dependent_module
	AND im.module = independent_module
	AND   independent_day_offset = dependent_day_offset
	AND   m.type IN ('$CXR_TYPE_PREPROCESS_DAILY',
	                 '$CXR_TYPE_MODEL',
	                 '$CXR_TYPE_POSTPROCESS_DAILY')
	AND   im.type IN ('$CXR_TYPE_PREPROCESS_DAILY',
	                 '$CXR_TYPE_MODEL',
	                 '$CXR_TYPE_POSTPROCESS_DAILY');
	
	EOT
	
	main.log -v "Removing duplicates..."
	
	sort "$dep_file" | uniq > "$nodup_file"

	main.log -v "Running tsort on $nodup_file > $day_file"
	
	${CXR_TSORT_EXEC} "$nodup_file" > "$day_file" 
	
	if [[ $? -ne 0 ]]
	then
		main.dieGracefully "I could not figure out the correct order to execute the tasks.\nMost probably there is a cycle (Module A depends on B which in turn depends on A)"
	fi
	
	main.log -v "Now filling in data for all other days..."
	# the day_file now contains a tsorted list of module entries.
	
	# Import data
	# .Import seems to do at implicit (undocumented!) commit...
	common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" -  false <<-EOT
	DROP TABLE IF EXISTS day_t;
	CREATE TABLE day_t (module);
	.import $day_file day_t
	EOT
	
	# now create the permutation
	day_list="$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" - <<-EOT
	SELECT d.day_iso || '@' || t.module 
	FROM 	days d,day_t t;
	EOT)"
	
	# Drop temp table
	common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "DROP TABLE IF EXISTS day_t;"
	
	main.log -v "Appending day list..."
	echo "$day_list" >> "$output_file"
	
	######################################
	main.log -v "Ordering $CXR_TYPE_POSTPROCESS_ONCE tasks..."
	######################################
	
	common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" - <<-EOT
	
	-- Prepare proper output
	.output $dep_file
	.separator ' '
	
	------------------------------------
	-- First, add all OT-Pre Tasks 
	-- otherwise tasks without deps would
	-- be skipped
	------------------------------------
	SELECT '$CXR_STOP_DATE' || '@' || t.module,
	       '$CXR_STOP_DATE' || '@' || t.module
	FROM tasks t, modules m
	WHERE m.module = t.module
	AND   m.type IN ('$CXR_TYPE_POSTPROCESS_ONCE');
	
	------------------------------------
	-- Then add all dependencies
	-- we need to restrict the dependencies to
	-- the daily modules.
	------------------------------------
	SELECT '$CXR_STOP_DATE' || '@' || d.independent_module,
	       '$CXR_STOP_DATE' || '@' || d.dependent_module
	FROM  dependencies d, modules m, modules im
	WHERE m.module = d.dependent_module
	AND   im.module = d.independent_module
	AND   d.independent_day_offset = d.dependent_day_offset
	AND   m.type IN ('$CXR_TYPE_POSTPROCESS_ONCE')
	AND   im.type IN ('$CXR_TYPE_POSTPROCESS_ONCE') ;
	
	EOT
	
	main.log -v "Removing duplicates..."
	
	sort "$dep_file" | uniq > "$nodup_file"

	main.log -v "Running tsort on $nodup_file and appending output to $output_file..."
	
	${CXR_TSORT_EXEC} "$nodup_file" >> "$output_file" 
	
	if [[ $? -ne 0 ]]
	then
		main.dieGracefully "I could not figure out the correct order to execute the tasks.\nMost probably there is a cycle (Module A depends on B which in turn depends on A)"
	fi
}

################################################################################
# Function: common.task.createParallelDependencyList
# 
# Performs topological sorting on the dependencies so that the resulting list is
# suitable for parallel harvesting by a many workers. This means that all tasks are
# ordered by increasing dependency.
# TODO: We need to order stuff with predicates smarter. If a -7 predicate is given,
# the resulting order is 7-6-5-... Ascending order would be better.
#
# The outputfile contains an ordered list of task ids.
#
# Parameters:
# $1 - output_file to write list of task ids to
################################################################################
function common.task.createParallelDependencyList()
################################################################################
{
	local output_file
	
	local dep_file
	local nodup_file

	main.log -a "Ordering tasks for parallel execution..."
	
	output_file="$1"
	
	dep_file="$(common.runner.createTempFile dependencies)"
	nodup_file="$(common.runner.createTempFile nodup)"
	
	# Reset file
	: > "$output_file"
	
	common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" - <<-EOT
	
	-- Prepare proper output
	.output $dep_file
	.separator ' '
	
	------------------------------------
	-- First, add all tasks, no matter what
	------------------------------------
	
	SELECT d.day_iso || '@' || t.module,
	       d.day_iso || '@' || t.module
	FROM tasks t, days d, modules m
	WHERE m.module = t.module
	AND   d.day_iso = substr(t.id,1,10);

	------------------------------------
	-- Then add all the dependencies. 
	------------------------------------
	
	SELECT di.day_iso || '@' || independent_module,
	       dd.day_iso || '@' || dependent_module
	FROM dependencies, days di, days dd, modules m
	WHERE m.module = independent_module
	AND   di.day_offset = independent_day_offset
	AND   dd.day_offset = dependent_day_offset;

	EOT
	
	main.log -v "Removing duplicates..."
	
	sort "$dep_file" | uniq > "$nodup_file"

	main.log -v "Running tsort on $nodup_file and appending output to $output_file..."
	
	${CXR_TSORT_EXEC} "$nodup_file" >> "$output_file" 
	
	if [[ $? -ne 0 ]]
	then
		main.dieGracefully "I could not figure out the correct order to execute the tasks.\nMost probably there is a cycle (Module A depends on B which in turn depends on A)"
	fi
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
# Returns the number of tasks known we are working on.
# 
#
################################################################################
function common.task.countAllTasks()
################################################################################
{
	# Find all entries in the table
	task_count="$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT COUNT(*) FROM tasks t, instance_tasks it WHERE (t.id = it.id AND it.instance = '$CXR_INSTANCE' ) AND t.rank IS NOT NULL;")"
	
	main.log -v "Found $task_count tasks in total"
	
	echo "$task_count"
}

################################################################################
# Function: common.task.countSuccessfulTasks
#
# Returns the number of successful tasks known.
# 
#
################################################################################
function common.task.countSuccessfulTasks()
################################################################################
{
	task_count="$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT COUNT(*) FROM tasks WHERE status='$CXR_STATUS_SUCCESS' AND rank IS NOT NULL;")"
	
	main.log -v "Found $task_count successful tasks"
	
	echo "$task_count"
}

################################################################################
# Function: common.task.countFailedTasks
#
# Returns the number of failed tasks known.
# 
#
################################################################################
function common.task.countFailedTasks()
################################################################################
{
	task_count="$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT COUNT(*) FROM tasks WHERE status='$CXR_STATUS_FAILURE' AND rank IS NOT NULL;")"
	
	main.log -v "Found $task_count failed tasks"
	
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
	task_count="$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT COUNT(*) FROM tasks t, instance_tasks it WHERE (t.id = it.id AND it.instance = '$CXR_INSTANCE' ) AND t.status='${CXR_STATUS_TODO}' AND t.rank IS NOT NULL;")"
	
	main.log -v "Found $task_count open tasks"
	
	echo "$task_count"
}

################################################################################
# Function: common.task.countAllWorkers
#
# Returns the number of all workers (on any instace).
#
################################################################################
function common.task.countAllWorkers()
################################################################################
{
	local worker_count
	
	# Find only "RUNNING" entries
	worker_count="$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT COUNT(*) FROM workers;")"
	
	# Set to 0 if empty
	if [[ -z "$worker_count" ]]
	then
		worker_count=0
	fi
	
	main.log -v "Found $worker_count workers"
	
	echo "$worker_count"
}

################################################################################
# Function: common.task.countMyRunningWorkers
#
# Returns the number of running workers (running from the POV of the Operating system) 
# on this instance.
#
################################################################################
function common.task.countMyRunningWorkers()
################################################################################
{
	local running_pids
	local pid
	local count
	
	count=0
	
	# Find only my "RUNNING" entries
	running_pids="$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT pid FROM workers WHERE instance='${CXR_INSTANCE}';")"
	
	oIFS="$IFS"
	IFS='
'
	
	for pid in $running_pids
	do
		# kill returns non-zero if process is gone, 0 is pseudo signal 
		# we avoid termination with || :
		kill -0 $pid &> /dev/null || :
		
		if [[ $? -eq 0 ]]
		then
			count=$(( $count + 1 ))
		fi
		
	done
	
	IFS="$oIFS"
	
	main.log -v "Found $count active pid(s)"
	echo $count
	
}

################################################################################
# Function: common.task.countRunningWorkers
#
# Returns the number of running workers (on any instance).
#
################################################################################
function common.task.countRunningWorkers()
################################################################################
{
	local worker_count
	
	# Find only "RUNNING" entries
	worker_count="$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT COUNT(*) FROM workers WHERE STATUS='${CXR_STATUS_RUNNING}';")"
	
	# Set to 0 if empty
	if [[ -z "$worker_count" ]]
	then
		worker_count=0
	fi
	
	main.log -v "Found $worker_count running workers"
	
	echo "$worker_count"
}

################################################################################
# Function: common.task.detectLockup
#
# Tests if all workers of a run are in a waiting state. This means that some dependency
# is not fullfilled but has not failed, so all Worker will have to wait forever
# (or until they waited CXR_DEPENDENCY_TIMEOUT_SEC seconds).
# Since it is possible that this happens by coincidence, we keep a counter in
# an instance hash that we increase when all workers are idle and decrease when they are not.
# If a threshold is reached, we stop the run, but only if there are still open tasks.
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
	
	if [[ "$CXR_PARALLEL_PROCESSING" == false || "$(common.task.countOpenTasks)" -eq 0 ]]
	then
		# it seems that we are done or alone, OK
		return $CXR_RET_OK
	fi
	
	common.hash.has? Lockup $CXR_LEVEL_INSTANCE LockupCount > /dev/null
	if [[ $_has == true ]]
	then
		count=$_value
	else
		count=0
	fi
	
	# Count the running workers
	numRunning="$(common.task.countRunningWorkers)"
	
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
	common.hash.put Lockup $CXR_LEVEL_INSTANCE LockupCount $count
}


################################################################################
# Function: common.task.setNextTask
#
# Returns the id of the next task to execute and all its data in environment vars.
# Even though sqlite does locking, we protect this critical function with a lock.
# If no parameter is given, we choose from all tasks is the pool, otherwie we restrict.
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
	common.runner.getLock NextTask "$CXR_LEVEL_GLOBAL"
	
	local task_count
	local potential_task_data
	
	task_count=$(common.task.countOpenTasks)
	
	# Are there open tasks at all?
	if [[ "$task_count" -eq 0 ]]
	then
		# Release Lock
		common.runner.releaseLock NextTask "$CXR_LEVEL_GLOBAL"
		
		main.log -a "All tasks have been assigned."
		
		# there are no more tasks, remove all continue files after some waiting
		# The waiting should ensure that all workers are past their check for do_we_continue

		sleep $(( 2 * $CXR_WAITING_SLEEP_SECONDS ))
		
		# It is safe to do this because the test for the continue file comes 
		# very early in the worker 
		common.state.deleteMyContinueFile
		
		# Remove this worker
		common.task.removeWorker $CXR_WORKER_PID
		
		echo ""
		return $CXR_RET_OK
		
	else
		main.log -v "There are $task_count unfinished tasks - we choose the top one."
	fi
	
	# get first relevant entry in the DB
	# We join with instance_tasks to get only tasks we are interested in
	potential_task_data="$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT t.id,t.module,t.type,t.exclusive,d.day_offset,t.invocation FROM tasks t, instance_tasks it, days d WHERE (d.day_iso=substr(t.id,1,10)) AND (t.id = it.id AND it.instance = '$CXR_INSTANCE' ) AND t.status='${CXR_STATUS_TODO}' AND t.rank NOT NULL ORDER BY rank ASC LIMIT 1")"
	
	# Check status
	if [[ $? -ne 0 ]]
	then
		main.dieGracefully "An error ocurred when selecting the next task!" 
	fi
	
	# we might not get a string
	if [[ -z "$potential_task_data" ]]
	then
		# No task!
		main.log -a "It seems that all tasks are done..."
		
		# Release lock
		common.runner.releaseLock NextTask "$CXR_LEVEL_GLOBAL"
		
		return $CXR_RET_OK
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
		common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "UPDATE tasks set  STATUS='${CXR_STATUS_RUNNING}' WHERE id='$_id'"
		
		main.log -v "New task has id $_id"
	fi
	
	# Release lock
	common.runner.releaseLock NextTask "$CXR_LEVEL_GLOBAL"
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
			common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "UPDATE tasks set status='${status}' WHERE id='$id'"
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
	 
	common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "UPDATE workers set status='${CXR_STATUS_WAITING}' WHERE pid=$pid AND instance='$CXR_INSTANCE'"
	
	main.log -v  "common.task.Worker (pid: $pid) changed its state to waiting"
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
		main.dieGracefully "needs a pid as input, got $*"
	fi
	
	local pid
	pid=$1
	 
	common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "UPDATE workers set status='${CXR_STATUS_RUNNING}' WHERE pid=$pid AND instance='$CXR_INSTANCE'"
	
	main.log -v "common.task.Worker (pid: $pid) changed its state to running"
}


###############################################################################
# Function: common.task.removeWorker
#
# kills the common.task.Worker of the given pid and alse removes it from the process list.
# 
# Parameters:
# $1 - the workers pid
################################################################################
function common.task.removeWorker()
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "needs a pid as input"
	fi
	
	local pid
	
	pid=$1
	
	# Remove from DB
	common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "DELETE FROM workers WHERE pid=$pid AND instance='$CXR_INSTANCE'"
	
	# Kill the process
	kill $pid 2>/dev/null
}

################################################################################
# Function: common.task.Worker
#
# This function is the workhorse of the parallel CAMxRunner. The Runner spawns one 
# or more of this functions to operate on the existing tasks.
# This can even be done from more than one machine.
#
# The worker gets a new task via <common.task.setNextTask>
# then waits (using <common.module.areDependenciesOk?>)
# until the dependencies of this task are fullfilled. 
#
# Parameters:
# $1 - the worker id (internal number, just to tell log output on screen apart)
# $2 - logfile to use
################################################################################
function common.task.Worker()
################################################################################
{
	# The ID is global, but it is not unique across servers
	CXR_WORKER_ID=${1}
	
	# Logfile is global across this workers processes
	CXR_LOG="$2"
	
	local tmp
	local new_task
	local oIFS
	local descriptor
	local task
	local exclusive
	local invocation
	local module
	local day_offset
	local day_iso
	local shown
	local start_epoch
	local waited_seconds
	
	waited_seconds=0
	
	#Getting the pid is not easy, we do not want to create unnecessary processes...
	tmp=$(common.runner.createTempFile $FUNCNAME)
	
	# The pid is the parent of the awk process
	# and the 4th field of /proc/self/stat is the Parent PID
	awk '{print $4}' /proc/self/stat > $tmp
	# read pid from file
	CXR_WORKER_PID=$(cat $tmp)
	
	# Test other approach
	MY_PID=$( /bin/ps -p $$ -o pid= )
	
	main.log -a "PIDs: $CXR_WORKER_PID $MY_PID $$"
	
	# Insert this worker
	common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "INSERT OR REPLACE INTO workers (pid, instance,status,epoch_m) VALUES ($CXR_WORKER_PID,'$CXR_INSTANCE','$CXR_STATUS_WAITING',$(date "+%s"))"
	
	main.log -a -B  "Worker (pid ${CXR_WORKER_PID}, id ${CXR_WORKER_ID}) starts on $CXR_MACHINE..."

	# We stay in this loop as long as the continue file exists
	# or until no more tasks are around
	while [[ -f "$CXR_CONTINUE_FILE" ]]
	do
		# Do we stop here?
		common.state.doContinue? || common.task.removeWorker $CXR_WORKER_PID
		
		# We are not yet busy
		common.task.waitingWorker $CXR_WORKER_PID
		
		# Is there enough free memory?
		if [[ "$CXR_CHECK_MEMORY_USAGE" == false || "$(common.performance.getMemFreePercent)" -gt ${CXR_MEM_FREE_PERCENT:-0} ]]
		then
			# Enough Memory
			
			# Init id
			_id=""
			
			# common.task.setNextTask provides tasks in an atomic fashion
			# Sets a couple of "background" variables
			# This is a blocking call (we wait until we get a task)
			common.task.setNextTask
			
			id="${_id:-}"
			
			# The task id might be empty
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
				
				day_iso="$(common.date.OffsetToDate $day_offset)"
				
				main.log -v "module: $module day: $day_iso invocation: $invocation"
				
				if [[ ${CXR_ALLOW_MODEL_CONCURRENCY:-false} == false ]]
				then
					if [[ "$exclusive" == true ]]
					then
						# If exclusive, try to get lock
						common.runner.getLock Exclusive "$CXR_LEVEL_GLOBAL"
						
					else
						# If not, just check if it is set 
						common.runner.waitForLock Exclusive "$CXR_LEVEL_GLOBAL"
						
						if [[ $_retval == false ]]
						then
							main.dieGracefully "There seeems to be another exclusive task running that takes too long."
						fi

					fi # do we allow other processes while CAMx runs?
				fi
				
				module_path="$(common.module.getPath "$module")"
				
				shown=false
				# Store befor-wait epoch to test timeout
				start_epoch="$(date "+%s")"
				
				# We need to wait until all dependencies are ok
				until [[ "$(common.module.areDependenciesOk? "$module" "$day_offset" "$day_iso" )" == true ]]
				do
					# At least show once that we wait
					if [[ $shown == false  ]]
					then
						shown=true
						main.log -a "Waiting for dependencies of $module to be done for day $day_iso"
					else
						main.log -v "Waiting for dependencies of $module to be done for day $day_iso"
					fi
					
					# Tell the system we wait, then sleep
					common.task.waitingWorker $CXR_WORKER_PID
					sleep $CXR_WAITING_SLEEP_SECONDS
					
					waited_seconds=$(( $(date "+%s") - $start_epoch ))
					
					# Test of we waited to long (not if CXR_DEPENDENCY_TIMEOUT_SEC is -1)
					if [[ $CXR_DEPENDENCY_TIMEOUT_SEC -ne -1 && $waited_seconds -gt $CXR_DEPENDENCY_TIMEOUT_SEC ]]
					then
						main.dieGracefully "It took longer than CXR_DEPENDENCY_TIMEOUT_SEC ($CXR_DEPENDENCY_TIMEOUT_SEC) seconds to fullfill the dependencies of $module for day $day_iso"
					fi
					
					# It's possible that we have been "shot" in the meantime
					if [[ "$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT status FROM workers WHERE pid=$CXR_WORKER_PID AND instance='$CXR_INSTANCE'" )" == $CXR_STATUS_KILLED ]]
					then
						# We have done our duty
						common.task.removeWorker $CXR_WORKER_PID
						return $CXR_RET_OK
					fi
					
				done
				
				# Time to work
				common.task.runningWorker $CXR_WORKER_PID
				
				main.log -v "module: $module\nday: $day_iso\ninvocation: $invocation\nexclusive: $_exclusive\nwait time for dependencies: $waited_seconds s"
				
				# Setup environment
				common.date.setVars "$CXR_START_DATE" "${day_offset:-0}"
				
				if [[ $CXR_RELOAD_CONF == true ]]
				then
					main.log -v "CXR_RELOAD_CONF is true, we reload the config..."
					
					main.readConfig "${CXR_RUN}" "${CXR_MODEL}" "${CXR_MODEL_VERSION}" "${CXR_RUN_DIR}"
				fi
				
				main.log -a -B "Worker $CXR_WORKER_ID (PID: $CXR_WORKER_PID on $CXR_MACHINE) assigned to $module (invocation $invocation) for $CXR_DATE"
				
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
				# If invocation is unset, we pass 1
				$CXR_META_MODULE_NAME ${invocation:-1} \
				&& common.task.changeTaskStatus $id $CXR_STATUS_SUCCESS \
				|| common.task.changeTaskStatus $id $CXR_STATUS_FAILURE
							
				# Stop Timing 
				# getProblemSize must be implemented by all modules
				common.performance.stopTiming $CXR_META_MODULE_NAME $(getProblemSize ${invocation:-1})
				
				#Release resources if needed
				if [[ "$exclusive" == true && ${CXR_ALLOW_MODEL_CONCURRENCY:-false} == false ]]
				then
					main.log  "Activating the assignment of new tasks again."
					common.runner.releaseLock Exclusive "$CXR_LEVEL_GLOBAL"
				fi
			else
				main.log -v "Worker $CXR_WORKER_PID did not receive an assignment - it seems that we are done."
				# Get out of loop
				break

			fi # Got a task?
		
		else
			# Not enough memory
			main.log -w "Worker $CXR_WORKER_PID detected that we have less than ${CXR_MEM_FREE_PERCENT} % of free memory.\nReaLoad: $(common.performance.getReaLoadPercent) %. We wait..."
			
			# Tell the system we wait, then sleep
			common.task.waitingWorker $CXR_WORKER_PID
			sleep $CXR_WAITING_SLEEP_SECONDS
			
			# It's possible that we have been "shot" in the meantime
			if [[ "$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT status FROM workers WHERE pid=$CXR_WORKER_PID AND instance='$CXR_INSTANCE'" )" == $CXR_STATUS_KILLED ]]
			then
				# We have done our duty
				common.task.removeWorker $CXR_WORKER_PID
				return $CXR_RET_OK
			fi
				
		fi # Enough Memory?
			
	done
	
	# We have done our duty
	common.task.removeWorker $CXR_WORKER_PID
	return $CXR_RET_OK
}

################################################################################
# Function: common.task.spawnWorkers
#
# Creates $1 workers with teir logfiles. We fill the global array CXR_LOGS
#
# Parameters:
# $1 - number of workers to spawn
################################################################################
function common.task.spawnWorkers()
################################################################################
{
	local iWorker
	local nWorkers
	local task_count
	
	if [[ "${CXR_CHECK_MODEL_SPACE_REQUIRED}" == true  ]]
	then
		mb_needed=$(common.check.PredictModelOutputMb)
	
		common.check.MbNeeded "${CXR_OUTPUT_DIR}" "${mb_needed}"
		
		# We assume that we need 5% of this space in CXR_TMP_DIR if we do not decompress in place
		if [[ "${CXR_DECOMPRESS_IN_PLACE}" == false  ]]
		then
			common.check.MbNeeded "${CXR_TMP_DIR}" $(common.math.FloatOperation "${CXR_TMP_SPACE_FACTOR:-0.05} * ${mb_needed}" 0)
		fi
	else
		main.log -w "CXR_CHECK_MODEL_SPACE_REQUIRED is false, I will not check if sufficient diskspace is available"
	fi
	
	
	nWorkers="$1"
	
	# The control thread is "Worker 0"
	CXR_WORKER_ID=0
	
	main.log  "We now create $1 worker threads"
	
	for iWorker in $(seq 1 $nWorkers)
	do
	
		task_count=$(common.task.countOpenTasks)
	
		# Are there open tasks at all?
		if [[ "$task_count" -ne 0 ]]
		then
			# Determine logfile for this worker.
			# We use a different naming convention if we have more than 1 worker only
			if [[ $nWorkers -gt 1 ]]
			then
				CXR_LOGS[$iWorker]=${CXR_LOG%.log}_${CXR_MACHINE}_${iWorker}.log
			else
				CXR_LOGS[$iWorker]=${CXR_LOG}
			fi
			
			# Create a worker and send it to background
			common.task.Worker $iWorker ${CXR_LOGS[$iWorker]} &
			
			if [[ "$1" -gt 1 ]]
			then
				# Wait a bit to avoid congestion
				main.log -a "We wait 60 seconds until we launch the next worker to see the memory demand"
				sleep 60
			fi
		else
			main.log -a "There are no more tasks, will not spawn more workers."
		fi
		
	done
}

################################################################################
# Function: common.task.removeAllWorkers
# 
# Removes all workers of this instance.
# 
################################################################################
function common.task.removeAllWorkers()
################################################################################
{
	main.log  "We remove all workers on $CXR_MACHINE."
	
	for pid in $(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT pid FROM workers WHERE instance='$CXR_INSTANCE'")
	do
		common.task.removeWorker "$pid"
	done
}

################################################################################
# Function: common.task.controller
#
# The main process that started the workers goes into this function to
# watch the progress periodically and detect issues.
#
################################################################################
function common.task.controller()
################################################################################
{
	local ReaLoad
	# Counter needed to find out when to show ETA
	local i
	local task_count
	
	i=0
	
	main.log "Controller: Entering controller loop (the work is carried out by background processes. I check every $CXR_WAITING_SLEEP_SECONDS seconds if all is swell.)"
	
	while [[ -f "$CXR_CONTINUE_FILE" ]]
	do
		# Still alive
		touch $CXR_INSTANCE_FILE_ALIVE
		
		sleep $CXR_WAITING_SLEEP_SECONDS
		
		# Detect Lockup (all workers are waiting)
		common.task.detectLockup

		if [[ $(common.task.countMyRunningWorkers) -lt $CXR_MAX_PARALLEL_PROCS ]]
		then
			main.log -w "Controller: Somehow, less than $CXR_MAX_PARALLEL_PROCS workers are alive! (maybe some have not started yet)"
		fi
		
		# Report the Estimated Time of arrival every now and then
		i=$(( $i + 1 ))
		i=$(( $i % $CXR_REPORT_INTERVAL ))
		
		if [[ $i -eq 0 ]]
		then
			common.performance.reportEta
			
			# Look at system load
			ReaLoad=$(common.performance.getReaLoadPercent)
		
			if [[ $ReaLoad -gt $CXR_LOAD_WARN_THRESHOLD ]]
			then
				# TODO: Safely remove a sleeping worker before it wakes up
				# when a worker gets status CXR_STATUS_KILLED, it will remove itself
				main.log -w "Controller: ReaLoad exceeds $CXR_LOAD_WARN_THRESHOLD %!"
			fi
		fi
		
		task_count=$(common.task.countOpenTasks)
		
		if [[ "$task_count" -eq 0 ]]
		then
			main.log -a "Controller: This was the last task to be processed, notifying system after security pause...\nController: Do not be alarmed: Running processes will have time to finish."
			
			# there are no more tasks, remove our file after some waiting
			# The waiting should ensure that all workers are past their check for do_we_continue
			sleep $(( 2 * $CXR_WAITING_SLEEP_SECONDS ))
			
			# It is safe to do this because the test for the continue file comes 
			# very early in the workers
			common.state.deleteMyContinueFile
		fi
		
	done
	
	if [[ ! -e "$CXR_GLOBAL_ABNORMAL_TERMINATION_FILE" ]]
	then
		# We now wait for the last workers to finish
		main.log -a "Controller: Waiting until running workers are done..."
		while [[ $(common.task.countMyRunningWorkers) -gt 0 ]]
		do
			sleep $CXR_WAITING_SLEEP_SECONDS
		done
		
	else
		main.log -w "Controller: Abnormal termination - all workers will be taken down now."
	fi
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
	local task_file
	local line
	local module
	local day_offset
	local invocation
	local module_type
	local my_stage
	local dep_file
	local iOffset
	local tempfile
	
	main.log -a -B "Initializing task subsystem, might take a while, depending on number of tasks..."
	
	# Reset the ID counter
	local current_id
	local task_file
	
	current_id=1
	
	# Init
	CXR_TIME_TOTAL_ESTIMATED=0
	
	if [[	$CXR_FIRST_INSTANCE == false ]]
	then
		# We are in a non-master runner
		main.log -a -b "We are not the first instance - we use the pre-existing task infrastructure"
	else
		# Redo
		
		# The list of tasks is stred in this file
		task_file=$(common.runner.createTempFile task_id_list)
		tempfile=$(common.runner.createTempFile sql-rank)
		
		# Clean tempfile
		: > $tempfile
		
		if [[ $CXR_PARALLEL_PROCESSING == true ]]
		then
			common.task.createParallelDependencyList $task_file
		else
			common.task.createSequentialDependencyList $task_file
		fi
		
		main.log -v "Got this taskfile:\n$(cat $task_file)"
		
		# Generate SQL file to update the ranks of the tasks
		# db_functions will put all in the same TRX
		while read line 
		do
			if [[ -z "$line" ]]
			then
				main.log -w "Detected empty line in $task_file: $(cat $task_file)"
				continue
			fi
			
			# We need to parse the line
			# this sets a couple of _variables
			# we set the flag that there is no invocation
			common.task.parseId "${line}" true

			# Write Update statement to file
			# We only give ranks to stuff that was not yet sucessfully done
			# note that all invocations of a given (module, day) pair get the same rank.
			# This is by design and correct.
			echo "UPDATE tasks SET rank=$current_id WHERE module='$_module' AND substr(id,1,10)=(SELECT day_iso FROM days WHERE day_offset=$_day_offset) AND status IS NOT '$CXR_STATUS_SUCCESS' AND rank IS NULL AND module in (SELECT module FROM modules);" >> $tempfile

			# Increase ID
			current_id=$(( $current_id + 1 ))

		done < "$task_file"
		
		# Execute all statements at once
		common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" $tempfile || main.dieGracefully "Could not update ranks properly"
		
		main.log -v  "This run consists of $(common.task.countOpenTasks) tasks."
		
		# pdf_file=$CXR_RUN_DIR/${CXR_RUN}_dep_$(date +"%Y_%m_%d_%H_%M").pdf
		# common.task.drawDependencyGraph "$dep_file" "$pdf_file"

	fi # Multiple mode?
	
	# Get a time estimate
	CXR_TIME_TOTAL_ESTIMATED=$(common.performance.estimateTotalRuntimeSeconds)
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
	
	is "$(common.task.getId test ${CXR_START_DATE} 2)" "${CXR_START_DATE}@test@2" "common.task.getId normal"
	
	# Testing parser. 
	common.task.parseId ${CXR_START_DATE}@convert_emissions@2
	is "$_module" convert_emissions "common.task.parseId - module name"
	is "$_day_offset" 0 "common.task.parseId only module - day offset"
	is "$_invocation" 2 "common.task.parseId only module - invocation"
	

	########################################
	# teardown tests if needed
	########################################
	
}