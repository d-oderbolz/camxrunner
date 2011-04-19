# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Postprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Extracts NABEL or other station data
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

CXR_META_MODULE_DEPENDS_ON="${CXR_TYPE_MODEL}"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Extracts data for defined measurement stations"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_POSTPROCESS_DAILY}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=0

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|idl"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'


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
	# This module needs one invocation per grid the user wants to precoss per step
	echo  $(main.countDelimitedElements "$CXR_RUN_EXTRACTION_ON_GRID" " ")
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
	local invocation
	local grid
	local x
	local y
	local z
	local -a arr
	
	# The Problem size here is a function of the invocation
	invocation=$1
	
	# The grid is the i-1 element of the list CXR_RUN_EXTRACTION_ON_GRID
	arr=($CXR_RUN_EXTRACTION_ON_GRID)
	grid=${arr[$(( $invocation - 1 )) ]}
	
	x=$(common.runner.getX ${grid})
	y=$(common.runner.getY ${grid})
	z=$(common.runner.getZ ${grid})
	
	echo $(( $x * $y * $z ))
}

################################################################################
# Function: set_variables
#	
# Sets the appropriate variables needed for <extract_station_data>
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
	
	########################################################################
	# Set variables
	########################################################################
	
	# IDL-based extraction of NABEL data needs just the 
	# data from this specific grid in ASCII data
	
	# Grid specific - we need to define CXR_IGRID
	# Read it from the variable
	run_on=($CXR_RUN_EXTRACTION_ON_GRID)
	CXR_IGRID=${run_on[$(( $CXR_INVOCATION - 1 ))]}
	
	CXR_MODEL_INPUT_FILE=$(common.runner.evaluateRule "$CXR_AVG_FILE_RULE" false CXR_AVG_FILE_RULE)

	# the temp and pressure files to use (binary)
	CXR_ZP_GRID_FILE="$(common.runner.evaluateRule "$CXR_PRESSURE_FILE_RULE" false CXR_PRESSURE_FILE_RULE)"
	CXR_TEMP_GRID_FILE="$(common.runner.evaluateRule "$CXR_TEMPERATURE_FILE_RULE" false CXR_TEMPERATURE_FILE_RULE)"
	
	
	#Checks
	CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_MODEL_INPUT_FILE $CXR_STATION_PROC_INPUT_FILE $CXR_ZP_GRID_FILE $CXR_TEMP_GRID_FILE"

	main.log -a "Configuring station data:"

	# Station dependent data
	for iStation in $(seq 0 $(($CXR_NUMBER_OF_STATIONS-1)) );
	do
		common.user.showProgress
		
		# Needed to expand the file rule
		station=${CXR_STATION[${iStation}]}
		
		# Output files must not be decompressed!
		CXR_STATION_OUTPUT_ARR_FILES[${iStation}]=$(common.runner.evaluateRule "$CXR_STATION_FILE_RULE" false CXR_STATION_FILE_RULE false)
		
		# Checks
		CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES ${CXR_STATION_OUTPUT_ARR_FILES[${iStation}]}"
	done

}

################################################################################
# Function: extract_station_data
#	
# Extracts the model data (innermost domain) from the locations of the stations
# given in CXR_STATION_X and CXR_STATION_Y.
# If you want to extract a certain cell, fill CXR_STATION_X and CXR_STATION_Y
# with data obtained from $(common.map.indexesToModelCoordinates)
#
# Calls IDL Procedure <extract_nabel_stations>
################################################################################	
function extract_station_data
################################################################################
{
	# In this module, CXR_INVOCATION corresponds to the entry in CXR_RUN_EXTRACTION_ON_GRID
	CXR_INVOCATION=${1}
	
	local exec_tmp_file
	local stations_array
	local iStation
	local station_file
	local x
	local y
	local xy
	local station
	local species_array
	local species
	local write_header
	local iSpec
	
	#Was this stage already completed?
	if [[ $(common.state.storeStatus ${CXR_STATUS_RUNNING}) == true ]]
	then

		#  --- Setup the Environment of the current day
		set_variables 
		
		#  --- Check Settings
		# Postprocessor: we only terminate the module
		if [[ $(common.check.preconditions) == false  ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		# Generate Temp file name
		exec_tmp_file=$(common.runner.createJobFile $FUNCNAME)
		
		# We have to prepare the stations array [x,y,filename]
		# and the species to extract array [speciesname]

		# Stations

		# Open brackets
		stations_array="["
		
		for iStation in $(seq 0 $(($CXR_NUMBER_OF_STATIONS-1)) )
		do
			
			# The output file of a given station
			# We remove the path from it to save space in the call
			# (easier to read)
			station_file="$(basename ${CXR_STATION_OUTPUT_ARR_FILES[${iStation}]})"
			
			# In case of a dry-run, create a dummy file
			if [[ "$CXR_DRY" == true  ]]
			then
				common.runner.createDummyFile ${CXR_STATION_OUTPUT_DIR}/${station_file}
			fi
			
			# The position of this station in grid indexes
			# Must be calculated here.
			# In the inner bracket, we convert to LonLat, in the outer to indexes.
			# It is possible that the coordinates are outside this grid. If so,
			# "-1 -1" is returned
			xy="$(common.map.ProjectionToIndexes ${CXR_STATION_X[${iStation}]} ${CXR_STATION_Y[${iStation}]} $CXR_IGRID $CXR_STATION_PROJECTION)"
			
			main.log -a "Station $(basename $station_file) has indexes $xy in domain $CXR_IGRID (Input: ${CXR_STATION_X[${iStation}]} ${CXR_STATION_Y[${iStation}]})"
			
			if [[ $xy == "-1 -1" ]]
			then
				main.log -a "It seems that station $(basename $station_file) is outside grid $CXR_IGRID. This station is skipped."
				continue
			fi
			
			# Parse the result
			x="$(echo "$xy" | awk '{ print $1 }')"
			y="$(echo "$xy" | awk '{ print $2 }')"
			
			# We pass all in a string array, IDL then does the conversion
			station="['${x}','${y}','${station_file}']"
			
			# Here we need not single quotes, because we have a 2D array
			stations_array="${stations_array}${station},"
		done
		
		# If the stations array is empty (eg. because all indexes are beyond the given domain) we quit
		if [[ "$stations_array" == "[" ]]
		then
			main.log -w "It seems that all indexes where outside domain $CXR_IGRID!"
			
			# Still we count this as success
			common.state.storeStatus ${CXR_STATUS_SUCCESS} > /dev/null
			
			# Exit
			return $CXR_RET_OK
		fi
		
		# Close brackets and remove last ","
		stations_array="${stations_array%,}]"
		
		# We also need information on the species to extract
		
		# Open brackets
		species_array="["
		
		if [[ "${CXR_STATION_SPECIES}" == "${CXR_ALL}"  ]]
		then
			# Loop through the general list
			for iSpec in $( seq 1 $CXR_NUMBER_OF_OUTPUT_SPECIES )
			do
				species=${CXR_OUTPUT_SPECIES_NAMES[${iSpec}]}
			
				species_array="${species_array}'${species}',"
			done
		else
		
			# Read it from the string
			for species in ${CXR_STATION_SPECIES}
			do
				species_array="${species_array}'${species}',"
			done

		fi
		# Close brackets and remove last ","
		species_array="${species_array%,}]"

		# Change to directory of IDL procedures
		cd $(dirname ${CXR_STATION_PROC_INPUT_FILE}) || return $CXR_RET_ERROR
		
		# We instruct the extractors to print a header for the first day
		if [[ $(common.date.isFirstDayOfSimulation?) == true  ]]
		then
			write_header=1
		else
			write_header=0
		fi
		
		# Then create the file to run IDL
		# Depending on the file to call we need a different interface.
		# later, this could be simplified using a rule...
		
		case $(basename ${CXR_STATION_PROC_INPUT_FILE} .pro) in

			extract_arpa_stations)
				# Here, we also need our ZP and Temp file (for ppb conversion)
				# also, we can specify the method of normalisation
				cat <<-EOF > $exec_tmp_file
				.run $(basename ${CXR_STATION_PROC_INPUT_FILE})
				$(basename ${CXR_STATION_PROC_INPUT_FILE} .pro),'${CXR_MODEL_INPUT_FILE}','${CXR_STATION_OUTPUT_DIR}',${write_header},${CXR_DAY},${CXR_MONTH},${CXR_YEAR},${stations_array},'${CXR_TEMP_GRID_FILE}','${CXR_ZP_GRID_FILE}',norm_method='${CXR_NORM_METHOD}',is_binary=1
				exit
				EOF
				;;
			extract_nabel_stations)
				
				cat <<-EOF > $exec_tmp_file
				.run $(basename ${CXR_STATION_PROC_INPUT_FILE})
				$(basename ${CXR_STATION_PROC_INPUT_FILE} .pro),'${CXR_MODEL_INPUT_FILE}','${CXR_STATION_OUTPUT_DIR}',${CXR_DAY},${CXR_MONTH},${CXR_YEAR},${CXR_MODEL_HOUR},${species_array},${stations_array},is_binary=1
				exit
				EOF
				;;
			*) main.log -w "I do not know how to start ${CXR_STATION_PROC_INPUT_FILE}!"
				;;
		
		esac

		if [[ "$CXR_DRY" == false  ]]
		then
			
			# Then we run it, while preserving the output
			${CXR_IDL_EXEC} < ${exec_tmp_file} 2>&1 | tee -a $CXR_LOG
			
		else
			main.log "This is a dry-run, no action required"
		fi
		
		# Get back
		cd ${CXR_RUN_DIR} || return $CXR_RET_ERROR
		
		# Check if all went well
		# Postprocessor: we only terminate the module
		if [[ $(common.check.postconditions) == false  ]]
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
