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
# ${CXR_DEP_ALL_ONCE_PRE} - all pre_start_preprocessors must have finished
# ${CXR_DEP_ALL_DAILY_PRE} - all daily_preprocessors must have finished
# ${CXR_DEP_ALL_MODEL} - all model modules must have finished
# ${CXR_DEP_ALL_DAILY_POST} - all daily_postprocessors must have finished
# ${CXR_DEP_ALL_ONCE_POST} - all finish_postprocessors must have finished

# the predicate "-"refers to the previous model day, so ${CXR_DEP_ALL_MODEL}- means that all model modules of the previous day must be successful. The predicate "+" means that this module must have run for all days, so extract_station_data+ means that extract_station_data ran for all days. (Usually only useful in One-Time Postprocessors)

CXR_META_MODULE_DEPENDS_ON="convert_output"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Extracts data for defined measurement stations"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_POSTPROCESS_DAILY}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=0

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|idl"

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=94

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=94

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2009), CAMxRunner@psi.ch"

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
	# This module needs one invocation per step
	# TODO: We want to run this on any grid later (similar to aqmfad)
	# One approach would be to convert the IDs of the fine domain to the others
	# (otherwise we again need to specify the coordinates for each domain)
	echo 1
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
	CXR_IGRID=${CXR_STATION_DOMAIN}
	CXR_STATION_INPUT_FILE=$(common.runner.evaluateRule "$CXR_AVG_ASC_FILE_RULE" false CXR_AVG_ASC_FILE_RULE)

	# the MM5 file to use
	# only used for ARPA stuff
	CXR_ZP_GRID_ASC_FILE="$(common.runner.evaluateRule "$CXR_PRESSURE_ASC_FILE_RULE" false CXR_PRESSURE_ASC_FILE_RULE)"
	CXR_TEMP_GRID_ASC_FILE="$(common.runner.evaluateRule "$CXR_TEMPERATURE_ASC_FILE_RULE" false CXR_TEMPERATURE_ASC_FILE_RULE)"
	
	#Checks
	CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_STATION_INPUT_FILE $CXR_STATION_PROC_INPUT_FILE $CXR_ZP_GRID_ASC_FILE $CXR_TEMP_GRID_ASC_FILE"

	# Station dependent data
	for iStation in $(seq 0 $(($CXR_NUMBER_OF_STATIONS-1)) );
	do
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
#
# Calls IDL Procedure <extract_nabel_stations>
################################################################################	
function extract_station_data
################################################################################
{
	# We do not need this variable here (exept implicit for the stage name)
	CXR_INVOCATION=${1:-1}
	
	local exec_tmp_file
	local xdim
	local ydim
	local zdim
	local stations_array
	local iStation
	local station_file
	local x
	local y
	local station
	local species_array
	local species
	local write_header
	local iSpec
	
	#Was this stage already completed?
	if [[ $(common.state.storeState ${CXR_STATE_START}) == true  ]]
	then

		#  --- Setup the Environment of the current day
		set_variables 
		
		#  --- Check Settings
		# Postprocessor: we only terminate the module
		if [[ $(common.check.preconditions) == false  ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			common.state.storeState ${CXR_STATE_ERROR}
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		# Generate Temp file name
		exec_tmp_file=$(common.runner.createTempFile $FUNCNAME)
		
		# Calculate extension of grid to extract
		xdim=$(common.runner.getX $CXR_STATION_DOMAIN)
		ydim=$(common.runner.getY $CXR_STATION_DOMAIN)
		
		# The Z dim depends on wether we use 3D output
		if [[ "${CXR_AVERAGE_OUTPUT_3D}" == true  ]]
		then
			# 3D
			zdim=$(common.runner.getZ $CXR_STATION_DOMAIN)
		else
			# Only 1 layer
			zdim=1
		fi
		
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
			
			# The position of this station
			x=${CXR_STATION_X[${iStation}]}
			y=${CXR_STATION_Y[${iStation}]}
			
			# We pass all in a string array, IDL then does the conversion
			station="['${x}','${y}','${station_file}']"
			
			# Here we need not single quotes, because we have a 2D array
			stations_array="${stations_array}${station},"
		done
		
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

			extract_stations)
				# Here, we also need an MM5 file for the pressure (for ppb conversion and potential coordinate conversion), 
				# as well as a flag to indicate if we look at the master domain or not (for coordinate transformation)
				# We set this fag to 0 because currently we only run on the innermost domain
				cat <<-EOF > $exec_tmp_file
				.run $(basename ${CXR_STATION_PROC_INPUT_FILE})
				$(basename ${CXR_STATION_PROC_INPUT_FILE} .pro),'${CXR_STATION_INPUT_FILE}','${CXR_STATION_OUTPUT_DIR}',${CXR_DAY},${CXR_MONTH},${CXR_YEAR},${species_array},${xdim},${ydim},${zdim},${stations_array},'${CXR_METEO_INPUT_FILE}',0
				exit
				EOF
				;;
				
			extract_arpa_stations)
				# Here, we also need an MM5 file for the pressure (for ppb conversion), 
				# as well as a flag to indicate if we look at the master domain or not (for coordinate transformation)
				# We set this fag to 0 because currently we only run on the innermost domain
				cat <<-EOF > $exec_tmp_file
				.run $(basename ${CXR_STATION_PROC_INPUT_FILE})
				$(basename ${CXR_STATION_PROC_INPUT_FILE} .pro),'${CXR_STATION_INPUT_FILE}','${CXR_STATION_OUTPUT_DIR}',${write_header},${CXR_DAY},${CXR_MONTH},${CXR_YEAR},${species_array},${xdim},${ydim},${zdim},${stations_array},'${CXR_TEMP_GRID_ASC_FILE}','${CXR_ZP_GRID_ASC_FILE}'
				exit
				EOF
				;;
			extract_nabel_stations)
				
				cat <<-EOF > $exec_tmp_file
				.run $(basename ${CXR_STATION_PROC_INPUT_FILE})
				$(basename ${CXR_STATION_PROC_INPUT_FILE} .pro),'${CXR_STATION_INPUT_FILE}','${CXR_STATION_OUTPUT_DIR}',${CXR_DAY},${CXR_MONTH},${CXR_YEAR},${CXR_MODEL_HOUR},${species_array},${xdim},${ydim},${zdim},${stations_array}
				exit
				EOF
				;;
			*) 
				;;
		
		esac
		
		# Get a copy of the call
		cat ${exec_tmp_file} | tee -a $CXR_LOG
			
		if [[ "$CXR_DRY" == false  ]]
		then
			
			# Then we run it, while preserving the output
			${CXR_IDL_EXEC} < ${exec_tmp_file} 2>&1 | tee -a $CXR_LOG
			
		else
			main.log   "This is a dry-run, no action required"    
		fi
		
		# Check if all went well
		# Postprocessor: we only terminate the module
		if [[ $(common.check.postconditions) == false  ]]
		then
			main.log -a "Postconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			common.state.storeState ${CXR_STATE_ERROR}
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_POSTCONDITIONS
		fi
		
		# Get back
		cd ${CXR_RUN_DIR}  || return $CXR_RET_ERROR

		common.state.storeState ${CXR_STATE_STOP} > /dev/null
	else
		main.log  "Stage $(common.state.getStageName) was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
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
