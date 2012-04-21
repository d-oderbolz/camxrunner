# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
# 
# Postprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id: 50_concatenate_station_data.sh 911 2009-04-29 13:08:42Z oderbolz $ 
#
# Concatenates NABEL or other station data to one file per station
# This is done once at the end of the simulation
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

# This is a very important field needed for the scheduling of parallel processes.
# It contains a comma-separated list of modules (just their names without numbers or extensions)
# from whose output this module DIRECTLY depends on.
#
# A process can only start if its dependencies have finished. Only list direct dependencies.
# There are some special dependencies:
# ${CXR_TYPE_PREPROCESS_ONCE} - all pre_start_preprocessors must have finished
# ${CXR_TYPE_PREPROCESS_DAILY} - all daily_preprocessors must have finished
# ${CXR_TYPE_MODEL} - all model modules must have finished
# ${CXR_TYPE_POSTPROCESS_DAILY} - all daily_postprocessors must have finished
# ${CXR_TYPE_POSTPROCESS_ONCE} - all finish_postprocessors must have finished


# the predicate "-<n>" refers to some previous model day, so ${CXR_TYPE_MODEL}-1 means that all model modules of the previous day must be successful before this module may run. 
CXR_META_MODULE_DEPENDS_ON="${CXR_TYPE_POSTPROCESS_DAILY}"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Concatenates station data."

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_POSTPROCESS_ONCE}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=0

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id: 50_concatenate_station_data.sh 911 2009-04-29 13:08:42Z oderbolz $'

################################################################################
# Function: getNumInvocations
#
# Needs to be changed only if your module can be called more than once per step independently.
# For example your module might be run for each grid separately. Then, CAMxRunner
# can might be able to start these in parallel, but it needs to know how many
# of these "invocations" per step are needed.
# 
################################################################################
function getNumInvocations()
################################################################################
{
	# This module needs one invocation per step
	echo 1
}

################################################################################
# Function: getProblemSize
#
# Returns the problem size of a given invocation.
# If the problem size is constant, return 1.
# 
# Parameters:
# $1 - invocation
################################################################################
function getProblemSize()
################################################################################
{
	# Problem size is constant
	echo 1
}

################################################################################
# Function: set_variables
#	
# Sets the appropriate variables for <concatenate_station_data>
################################################################################
function set_variables() 
################################################################################
{
	# First of all, reset checks.
	# We will later continuously add entries to these 2 lists.
	# CAREFUL: If you add files to CXR_CHECK_THESE_OUTPUT_FILES,
	# these are deleted if he user runs the -F option. Do not mix up with input files!
	CXR_CHECK_THESE_INPUT_FILES=
	CXR_CHECK_THESE_OUTPUT_FILES=
	
	local iStation
	local day_offset
	local index
	local cpa_out
	local cpa_in
	
	index=0
	
	########################################################################
	# Set variables
	########################################################################
	
	main.log -a "Setting variables..."
	
	common.date.setVars "$CXR_START_DATE" 0
	
	# There is one output file per station
	for iStation in $(seq 0 $(($CXR_NUMBER_OF_STATIONS-1)) );
	do
		# Needed to expand the file rule
		station=${CXR_STATION[${iStation}]}
	
		# Output files must not be decompressed!
		CXR_STATION_OUTPUT_ARR_FILES[${iStation}]=$(common.runner.evaluateRule "$CXR_CUMULATIVE_STATION_FILE_RULE" false CXR_CUMULATIVE_STATION_FILE_RULE false)
	
		#Define Output check
		CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES ${CXR_STATION_OUTPUT_ARR_FILES[${iStation}]}"
	
		# Prepare CPA file
		if [[ $CXR_PROBING_TOOL == PA ]]
		then
			cpa_out=$(dirname ${CXR_STATION_OUTPUT_ARR_FILES[${iStation}]})/cpa_$(basename ${CXR_STATION_OUTPUT_ARR_FILES[${iStation}]})
			
			# There might be also a CPA file (cannot check it becasue we do not yet know if we have the input data)
			CXR_STATION_OUTPUT_ARR_FILES_CPA[${iStation}]=$cpa_out
		fi
		
	done
	
	# There is one input file per day and station
	# later we need to determine the station from the running index
	for day_offset in $(seq 0 $((${CXR_NUMBER_OF_SIMULATION_DAYS} -1 )) )
	do
		common.date.setVars "$CXR_START_DATE" "$day_offset"
		
		common.user.showProgress

		# Station dependent data
		for iStation in $(seq 0 $(($CXR_NUMBER_OF_STATIONS-1)) );
		do
			# Needed to expand the file rule
			station=${CXR_STATION[${iStation}]}
			
			CXR_STATION_INPUT_ARR_FILES[${index}]=$(common.runner.evaluateRule "$CXR_STATION_FILE_RULE" false CXR_STATION_FILE_RULE)
			
			#Checks
			CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_STATION_INPUT_ARR_FILES[${index}]}"
			
			if [[ $CXR_PROBING_TOOL == PA ]]
			then
				cpa_in=$(dirname ${CXR_STATION_INPUT_ARR_FILES[${index}]})/cpa_$(basename ${CXR_STATION_INPUT_ARR_FILES[${index}]})
				
				# file might not exist
				if [[ -e "$cpa_in" ]]
				then
					# Do not check these (we know its there now)
					CXR_STATION_INPUT_ARR_FILES_CPA[${index}]="$cpa_in"
				fi
			fi
			
			#increment index
			index=$(($index + 1))
		done # stations
	done # days
}

################################################################################
# Function: concatenate_station_data
#	
# Concatenates the data that was extracted by <extract_station_data>
# By Looping over the generated files
#
################################################################################	
function concatenate_station_data
################################################################################
{
	# We do not need this variable here (exept implicit for the stage name)
	CXR_INVOCATION=${1:-1}
	
	local iStation
	local index
	local iFile
	local oFile
	local skip
	local skip_cpa
	local do_cpa

	#Was this stage already completed?
	if [[ $(common.state.storeStatus ${CXR_STATUS_RUNNING}) == true ]]
	then
		set_variables
		
		# Check if CXR_STATION_INPUT_ARR_FILES_CPA is set
		# http://stackoverflow.com/questions/874389/bash-test-for-a-variable-unset-using-a-function
		if [[ ${!CXR_STATION_INPUT_ARR_FILES_CPA[@]} ]]
		then
			main.log -a "Found CPA files, will also concatenate these"
			do_cpa=true
		else
			if [[ $CXR_PROBING_TOOL == PA ]]
			then
				main.log -w "Probing tool is PA, but I found no CPA files. Maybe you did not use the correct extraction script (CXR_STATION_PROC_INPUT_FILE)?"
			fi
			do_cpa=false
		fi
	
		#  --- Check Settings
		if [[ $(common.check.preconditions) == false  ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
			common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
		
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		# Test if we need to skip any file
		for iStation in $(seq 0 $(($CXR_NUMBER_OF_STATIONS-1)) );
		do
			if [[ -e ${CXR_STATION_OUTPUT_ARR_FILES[${iStation}]} ]]
			then
				if [[ $CXR_SKIP_EXISTING == true ]]
				then
					main.log -w "File ${CXR_STATION_OUTPUT_ARR_FILES[${iStation}]} exists - because of CXR_SKIP_EXISTING, file will skipped."
					# Skip this file
					skip[${iStation}]=true
				else
					main.dieGracefully "File ${CXR_STATION_OUTPUT_ARR_FILES[${iStation}]} exists, CXR_SKIP_EXISTING is false!"
				fi
			else
				# Don't skip
				skip[${iStation}]=false
			fi
			
			# Same for cpa
			if  [[ $do_cpa == true ]]
			then
				if [[ -e ${CXR_STATION_OUTPUT_ARR_FILES_CPA[${iStation}]} ]]
				then
					if [[ $CXR_SKIP_EXISTING == true ]]
					then
						main.log -w "File ${CXR_STATION_OUTPUT_ARR_FILES_CPA[${iStation}]} exists - because of CXR_SKIP_EXISTING, file will skipped."
						# Skip this file
						skip_cpa[${iStation}]=true
					else
						main.dieGracefully "File ${CXR_STATION_OUTPUT_ARR_FILES_CPA[${iStation}]} exists, CXR_SKIP_EXISTING is false!"
					fi
				else
					# Don't skip
					skip_cpa[${iStation}]=false
				fi
			fi
		
		done
		
		# Processing normal files (preventing seq from using engineering notation)
		for index in $(seq -f"%.0f" 0 $(( ${#CXR_STATION_INPUT_ARR_FILES[@]} - 1)) )
		do
				# Input file
				iFile="${CXR_STATION_INPUT_ARR_FILES[$index]}"
				# For the output, we need to calculate the modulus with respect to the number of stations
				iStation=$(( $index % ${#CXR_STATION[@]} ))
				
				if [[ ${skip[${iStation}]} == true ]]
				then
					# Skip this station file if needed
					continue
				fi
				
				main.log -a "Adding ${CXR_STATION_INPUT_ARR_FILES[${index}]} to ${CXR_STATION_OUTPUT_ARR_FILES[${iStation}]}..."
				
				#Dry?
				if [[ "$CXR_DRY" == false  ]]
				then
					cat "${CXR_STATION_INPUT_ARR_FILES[${index}]}" >> "${CXR_STATION_OUTPUT_ARR_FILES[${iStation}]}"
				else
					main.log -a "This is a dry-run, no action required"
				fi
		done

		if  [[ $do_cpa == true ]]
		then
			# Processing CPA files
			for index in $(seq -f"%.0f" 0 $(( ${#CXR_STATION_INPUT_ARR_FILES_CPA[@]} - 1)) )
			do
					# Input file
					iFile="${CXR_STATION_INPUT_ARR_FILES_CPA[$index]}"
					# For the output, we need to calculate the modulus with respect to the number of stations
					iStation=$(( $index % ${#CXR_STATION[@]} ))
					
					if [[ ${skip_cpa[${iStation}]} == true ]]
					then
						# Skip this station file if needed
						continue
					fi
					
					main.log -a "Adding ${CXR_STATION_INPUT_ARR_FILES_CPA[${index}]} to ${CXR_STATION_OUTPUT_ARR_FILES_CPA[${iStation}]}..."
					
					#Dry?
					if [[ "$CXR_DRY" == false  ]]
					then
						cat "${CXR_STATION_INPUT_ARR_FILES_CPA[${index}]}" >> "${CXR_STATION_OUTPUT_ARR_FILES_CPA[${iStation}]}"
					else
						main.log -a "This is a dry-run, no action required"
					fi
			done
		fi # CPA?
		
		# Check if all went well
		# Postprocessor: we only terminate the module
		if [[ "$(common.check.postconditions)" == false  ]]
		then
			main.log -a "Postconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_POSTCONDITIONS
		fi
		
		common.state.storeStatus ${CXR_STATUS_SUCCESS} > /dev/null
	else
		main.log  "Stage $(common.task.getId) was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
	fi
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
	
	# None yet.
	:

	########################################
	# teardown tests if needed
	########################################
	
}