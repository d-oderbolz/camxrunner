# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Filesystem functions of CAMxRunner.
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
# At the moment, we use a standardized problem size.
#
# DB: $CXR_UNIVERSAL_TIMING_DB
#
# Parameters:
# $1 - module name (can be any string)
################################################################################
function common.performance.stopTiming()
################################################################################
{
	local module
	local time_norm
	local start_time
	local stop_time

	module=${1}
	
	# Get the current epoch
	stop_time="$(date "+%s")"
	
	var=CXR_TEMP_START_TIME_${module}
	
	# Get value via indirection
	start_time=${!var:-}
	
	if [[ "$start_time" ]]
	then
		# Calculate difference
		diff=$(( $stop_time - $start_time ))
		
		# Normalize by cells
		time_norm=$(common.math.FloatOperation "$diff / $CXR_TIME_NORM_FACTOR" 1 false)
		
		# Is the normalized time very small?
		if [[ $(common.math.FloatOperation "$time_norm < 1.0" 0 false) == 1 ]]
		then
			main.log -v "Normalized difference $time_norm is small. That might be a hint that CXR_TIME_PER_CELLS ($CXR_TIME_PER_CELLS) is too small"
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
													epoch_m)
							VALUES			(
														'$CXR_MODEL',
														'$CXR_MODEL_VERSION',
														'$module',
														'$CXR_TIME_PER_CELLS',
														'$CXR_MACHINE',
														$(common.performance.getReaLoadPercent),
														$time_norm,
														$(date "+%s")
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
# DB: $CXR_UNIVERSAL_TIMING_DB
#
################################################################################
function common.performance.estimateTotalRuntimeSeconds()
################################################################################
{
	local n_tasks
	local mean
	local result
	
	n_tasks=$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT COUNT(*) FROM tasks WHERE rank IS NOT NULL;")
	mean=$(common.db.getResultSet "$CXR_UNIVERSAL_TIMING_DB" "$CXR_LEVEL_UNIVERSAL" "SELECT AVG(elapsed_seconds) FROM timing;")
	
	result=$(common.math.FloatOperation "$n_tasks * $mean" -1 false)
	
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
# DB: $CXR_UNIVERSAL_TIMING_DB
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

	if [[ -z "$mean" || $(common.math.FloatOperation "$mean == 0" 0 false) == 1 ]]
	then
		# No data yet, try an average over more
		mean=$(common.db.getResultSet "$CXR_UNIVERSAL_TIMING_DB" "$CXR_LEVEL_UNIVERSAL" "SELECT AVG(elapsed_seconds) FROM timing WHERE model='$CXR_MODEL' AND version='$CXR_MODEL_VERSION' AND module='$module';")
		
		if [[ -z "$mean" || $(common.math.FloatOperation "$mean == 0" 0 false) == 1 ]]
		then
			# Still nothing, get avg over all
			mean=$(common.db.getResultSet "$CXR_UNIVERSAL_TIMING_DB" "$CXR_LEVEL_UNIVERSAL" "SELECT AVG(elapsed_seconds) FROM timing;")
		
			if [[ -z "$mean" || $(common.math.FloatOperation "$mean == 0" 0 false) == 1 ]]
			then
				# Still nothing, use default
				mean=600
			fi
		fi
	fi

	# we need to multply with normalisation factor to get it right
	estimate=$(common.math.FloatOperation "$mean * $CXR_TIME_NORM_FACTOR" -1 false)
	
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
	elapsed=$(( $(date "+%s") - $CXR_EPOCH ))
	left=$(( $CXR_TIME_TOTAL_ESTIMATED - $elapsed ))
	
	if [[ $left -gt 0 ]]
	then
		# OK, we are within estimation
		percentDone=$(common.math.FloatOperation "(100 * $elapsed) / $CXR_TIME_TOTAL_ESTIMATED" -1 false)
	
		# Only goes to stderr
		echo "Workers (Running/Total): $(common.task.countRunningWorkers)/$(common.task.countAllWorkers)" 1>&2
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
# Estimates the percentage of used memory from the output of top.
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
	
	usedPercent=0
	iColumn=1
	found=false
	
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
		# The first 7 lines are header
		for used in $(top -b -n1 | sed '1,7d' | awk "{ print \$$MemColumn }")
		#                                                    ^ Escape awk $ for shell
		do
			usedPercent="$(common.math.FloatOperation "$usedPercent + $used" 1 0)"
		done
		
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
	free="$(common.math.FloatOperation "100 - $usedPercent" -1 0)"
	
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
	Load=$(common.math.FloatOperation "($rawLoad * 100) / $CXR_NUM_CORES" 2 false)
	
	echo $Load
	
}

################################################################################
# Function: common.performance.getReaLoadPercent
#
# A performance metric that takes both Memory and CPU into account.
# We calculate the length of the vector determined by memory and CPU use.
# This metrich could be extended by an arbitrary number of metrics :-)
#
#
################################################################################
function common.performance.getReaLoadPercent()
################################################################################
{
	local mem
	local cpu
	local load
	
	mem=$(common.performance.getMemUsedPercent)
	cpu=$(common.performance.getSystemLoadPercent)
	
	load=$(common.math.FloatOperation "sqrt(${mem}^2 + ${cpu}^2)" -1 false)
	
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
	common.performance.stopTiming test
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	main.log "Free memory: $(common.performance.getMemFreePercent) %"
	main.log "System Load: $(common.performance.getSystemLoadPercent) %"
	main.log "ReaLoad: $(common.performance.getReaLoadPercent) %"
	
	# Measured time is in the DB,
	# we get the row that was added last
	time=$(common.db.getResultSet "$CXR_UNIVERSAL_TIMING_DB" "$CXR_LEVEL_UNIVERSAL" "SELECT elapsed_seconds FROM timing WHERE model='$CXR_MODEL' AND version='$CXR_MODEL_VERSION' AND module='test' ORDER BY epoch_m DESC LIMIT 1;" )

	difference=$(common.math.abs $(common.math.FloatOperation "$nSeconds - $time" 0 false))
	
	# We test for difference
	is_less_or_equal $difference $epsilon "common.performance Timing of sleep"

	########################################
	# teardown tests if needed
	########################################
	
}

