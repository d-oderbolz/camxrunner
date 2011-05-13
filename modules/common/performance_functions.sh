# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Title: Functions to assess performance.
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################

################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=1

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL=""

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains functions to measure performance and machine usage for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'


################################################################################
# Function: common.performance.startTiming
# 
# Initializes the most simple wall-clock timer for a given function.
# Stores this value in CXR_TEMP_START_TIME_${module}
#
# Parameters:
# $1 - module name (can be any string)
################################################################################
function common.performance.startTiming()
################################################################################
{
	module=${1}
	currrent_epoch="$(date "+%s")"

	# Store the current epoch in Env
	# let is needed, otherwise the syntax is incorrect
	let CXR_TEMP_START_TIME_${module}="$currrent_epoch"
	
	main.log -v "$module started at $currrent_epoch"
}

################################################################################
# Function: common.performance.stopTiming
# 
# Measures the time difference in seconds, adds it to the universal timing db. 
#
#
# Parameters:
# $1 - module name (can be any string)
# $2 - the problem size
################################################################################
function common.performance.stopTiming()
################################################################################
{
	local module
	local time_norm
	local start_time
	local stop_time
	local load
	local problem_size
	
	# Get the current epoch
	stop_time="$(date "+%s")"

	module=${1}
	problem_size=${2}
	
	var=CXR_TEMP_START_TIME_${module}
	
	# Get value via indirection
	start_time=${!var:-}
	
	if [[ "$start_time" ]]
	then
		# Calculate difference
		diff=$(( $stop_time - $start_time ))
		
		# Get current load
		load="$(common.performance.getReaLoadPercent)"
		
		if [[ -z "$load" ]]
		then
			load=0
		fi
		
		# Store data
		common.db.change "$CXR_UNIVERSAL_TIMING_DB" "$CXR_LEVEL_UNIVERSAL" - <<-EOT
		
			INSERT INTO timing 	(model,
													version,
													module,
													problem_size,
													machine,
													ReaLoad,
													elapsed_seconds,
													epoch_m,
													run)
							VALUES			(
														'$CXR_MODEL',
														'$CXR_MODEL_VERSION',
														'$module',
														'$problem_size',
														'$CXR_MACHINE',
														$load,
														$diff,
														$(date "+%s")
														,'$CXR_RUN'
													);
		
		EOT
		
	else
		# No start value found
		main.log -e "Module $module has no start time information. Maybe common.performance.startTiming was not run?"
	fi
}

################################################################################
# Function: common.performance.estimateTotalRuntimeSeconds
# 
# Estimates the runtime in seconds of a run. Currently very simple and very incorrect.
#
################################################################################
function common.performance.estimateTotalRuntimeSeconds()
################################################################################
{
	local n_tasks
	local mean
	local result
	
	n_tasks=$(common.task.countOpenTasks)
	mean=$(common.db.getResultSet "$CXR_UNIVERSAL_TIMING_DB" "$CXR_LEVEL_UNIVERSAL" "SELECT AVG(elapsed_seconds) FROM timing;")
	
	result=$(common.math.FloatOperation "$n_tasks * $mean" 0)
	
	echo $result
}

################################################################################
# Function: common.performance.estimateModuleRuntimeSeconds
# 
# Estimates the runtime in seconds of a given module for the current problem size.
# This estimation is based on the mean. TODO: estimate STDEV and add 1 sigma.
#
# If this module has no performance data yet, we use the a mean over all data.
#
# Parameters:
# $1 - module name
################################################################################
function common.performance.estimateModuleRuntimeSeconds()
################################################################################
{
	local module
	local mean
	local estimate
	
	module=${1}
	
	mean=$(common.db.getResultSet "$CXR_UNIVERSAL_TIMING_DB" "$CXR_LEVEL_UNIVERSAL" "SELECT AVG(elapsed_seconds) FROM timing WHERE model='$CXR_MODEL' AND version='$CXR_MODEL_VERSION' AND module='$module' AND machine='$CXR_MACHINE';")

	if [[ -z "$mean" || $(common.math.FloatOperation "$mean == 0" 0) == 1 ]]
	then
		# No data yet, try an average over more
		mean=$(common.db.getResultSet "$CXR_UNIVERSAL_TIMING_DB" "$CXR_LEVEL_UNIVERSAL" "SELECT AVG(elapsed_seconds) FROM timing WHERE model='$CXR_MODEL' AND version='$CXR_MODEL_VERSION' AND module='$module';")
		
		if [[ -z "$mean" || $(common.math.FloatOperation "$mean == 0" 0) == 1 ]]
		then
			# Still nothing, get avg over all
			mean=$(common.db.getResultSet "$CXR_UNIVERSAL_TIMING_DB" "$CXR_LEVEL_UNIVERSAL" "SELECT AVG(elapsed_seconds) FROM timing;")
		
			if [[ -z "$mean" || $(common.math.FloatOperation "$mean == 0" 0) == 1 ]]
			then
				# Still nothing, use default
				mean=600
			fi
		fi
	fi

	# we need to multply with normalisation factor to get it right
	estimate=$(common.math.FloatOperation "$mean * $CXR_TIME_NORM_FACTOR" -1)
	
	echo $estimate
}

################################################################################
# Function: common.performance.reportEta
# 
# Reports the estimated time of arrival together with the number
# of workers working/total
################################################################################
function common.performance.reportEta()
################################################################################
{
	local percentDone
	local estimatedTimeSeconds
	local elapsed
	local left
	
	# How many seconds have elapsed?
	elapsed=$(( $(date "+%s") - $CXR_START_EPOCH ))
	left=$(( $CXR_TIME_TOTAL_ESTIMATED - $elapsed ))
	
	if [[ $left -gt 0 ]]
	then
		# OK, we are within estimation
		percentDone=$(common.math.FloatOperation "(100 * $elapsed) / $CXR_TIME_TOTAL_ESTIMATED" 0)
	
		# Only goes to stderr
		echo -e "\nWorkers (Running/Total): $(common.task.countRunningWorkers)/$(common.task.countAllWorkers)" 1>&2
		echo "Tasks (Successful/Failed/Total): $(common.task.countSuccessfulTasks)/$(common.task.countFailedTasks)/$(common.task.countAllTasks)" 1>&2
		echo "Runtime so far: $(common.date.humanSeconds $elapsed)" 1>&2
		echo "Estimated remaining time of this run: $(common.date.humanSeconds $left)" 1>&2
		common.user.showProgressBar $percentDone
	else
		# Duh, our time is up...
		:
	fi
	
	
}


################################################################################
# Function: common.performance.getMemUsedPercent
#
# Estimates the percentage of used memory from the output of top by sampling
# CXR_CHECK_MEMORY_SAMPLES times
#
################################################################################
function common.performance.getMemUsedPercent()
################################################################################
{
	local usedPercent
	local iColumn
	local MemColumn
	local found
	local used
	local iIter
	
	usedPercent=0
	iColumn=1
	found=false
	
	if [[ $CXR_CHECK_MEMORY_SAMPLES -lt 1 ]]
	then
		main.log -w "CXR_CHECK_MEMORY_SAMPLES has an invalid value of $CXR_CHECK_MEMORY_SAMPLES. Using 1"
		CXR_CHECK_MEMORY_SAMPLES=1
	fi
	
	headers=$(top -b -n1 | head -n7 | tail -n1)
	for item in $headers
	do
		# Memory percent may be in different columns
		if [[ $item == %MEM ]]
		then
			found=true
			MemColumn=$iColumn
			break
		fi
		
		iColumn=$(( $iColumn + 1 ))
	done
	
	if [[ $found == true ]]
	then
		# Sample. We cannot use -n$CXR_CHECK_MEMORY_SAMPLES because of the headers
		for iIter in $(seq 1 $CXR_CHECK_MEMORY_SAMPLES)
		do
			# The first 7 lines are header
			for used in $(top -b -n1 | sed '1,7d' | awk "{ print \$$MemColumn }")
			#                                                                            ^ Escape awk $ for shell
			do
				if [[ $used =~ $CXR_PATTERN_NUMERIC ]]
				then
					usedPercent="$(common.math.FloatOperation "$usedPercent + $used" 1)"
				else
					main.log -w "Non-numerig output of top: $used"
				fi
			done
			
			# We wait 1 second before resampling
			sleep 1
		
		done # Iterate over samples
		
		# Divide by sample size
		usedPercent="$(common.math.FloatOperation "$usedPercent / $CXR_CHECK_MEMORY_SAMPLES" 1)"
		
		main.log -v "Currently $usedPercent % of memory are in use"
		
		echo $usedPercent
	else
		main.log -w "Could not find column %MEM of top - cannot determine amount of used memory"
		echo 0
	fi
	
}

################################################################################
# Function: common.performance.getMemFreePercent
#
# Gets free memory using <common.performance.getMemUsedPercent>
#
################################################################################
function common.performance.getMemFreePercent()
################################################################################
{
	local usedPercent
	
	usedPercent=$(common.performance.getMemUsedPercent)
	free="$(common.math.FloatOperation "100 - $usedPercent" 0)"
	
	main.log -v "Found $free % free memory"
	
	echo $free

}

################################################################################
# Function: common.performance.getSystemLoadPercent
#
# Gets the CPU-corrected systemload in percent from top (15 min average). 
# If this number is higher than 100, the system is heavily loaded.
#
#
################################################################################
function common.performance.getSystemLoadPercent()
################################################################################
{
	local rawLoad
	
	# The last field ($NF in awk) contains the 15min average of the load
	rawLoad=$(top -b -n1 | head -n1 | awk '{print $NF}')
	
	# Divide this number by number of cores and multply by 100
	Load=$(common.math.FloatOperation "($rawLoad * 100) / $CXR_NUMBER_OF_CORES" 2)
	
	echo $Load
	
}

################################################################################
# Function: common.performance.getReaLoadPercent
#
# A performance metric that takes both Memory and CPU into account.
# We calculate the length of the vector determined by memory and CPU use.
# This idea could be extended by an arbitrary number of variables.
# Note that this is a relatively expensive operation.
#
#
################################################################################
function common.performance.getReaLoadPercent()
################################################################################
{
	# In the fast case, we assume an idle machine
	if [[ "${CXR_FAST}" == true ]]
	then
		echo 0
		return $CXR_RET_OK
	fi
	
	local mem
	local cpu
	local load
	
	mem=$(common.performance.getMemUsedPercent)
	cpu=$(common.performance.getSystemLoadPercent)
	
	load=$(common.math.FloatOperation "sqrt(${mem}^2 + ${cpu}^2)" 0)
	
	echo $load
}

################################################################################
# Function: test_module
#
# Runs the predefined tests for this module
# 
################################################################################
function test_module()
################################################################################
{
	########################################
	# Setup tests if needed
	########################################
	
	local nSeconds
	local time
	local arr
	local difference
	local epsilon
	
	
	# We accept one second difference
	epsilon=1
	nSeconds=5
	
	main.log -a "We will now wait $nSeconds seconds to test timing..."
	
	common.performance.startTiming test
	sleep $nSeconds
	common.performance.stopTiming test 1
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	main.log "Free memory: $(common.performance.getMemFreePercent) %"
	main.log "System Load: $(common.performance.getSystemLoadPercent) %"
	main.log "ReaLoad: $(common.performance.getReaLoadPercent) %"
	
	# Measured time is in the DB,
	# we get the row that was added last
	time=$(common.db.getResultSet "$CXR_UNIVERSAL_TIMING_DB" "$CXR_LEVEL_UNIVERSAL" "SELECT elapsed_seconds FROM timing WHERE model='$CXR_MODEL' AND version='$CXR_MODEL_VERSION' AND module='test' ORDER BY epoch_m DESC LIMIT 1;" )

	difference=$(common.math.abs $(common.math.FloatOperation "$nSeconds - $time" 0))
	
	# We test for difference
	is_less_or_equal $difference $epsilon "common.performance Timing of sleep"

	########################################
	# teardown tests if needed
	########################################
	
}

