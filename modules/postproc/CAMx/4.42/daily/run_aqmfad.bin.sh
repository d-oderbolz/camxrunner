# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Postprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id: run_aqmfad.sh 6777 2011-08-17 10:58:07Z oderbolz $ 
#
# Runs aqmfad, the script which creates the direct access files
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

CXR_META_MODULE_DEPENDS_ON="convert_output"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Runs the program aqmfad, which creates direct access files for aqm, a PSI/LAC visualisation tool\nNot portable!"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_POSTPROCESS_DAILY}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=0

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id: run_aqmfad.sh 6777 2011-08-17 10:58:07Z oderbolz $'

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
	echo  $(main.countDelimitedElements "$CXR_RUN_AQMFAD_ON_GRID" " ")
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
	
	# The grid is the i-1 element of the list CXR_RUN_AQMFAD_ON_GRID
	arr=($CXR_RUN_AQMFAD_ON_GRID)
	grid=${arr[$(( $invocation - 1 )) ]}
	
	x=$(common.runner.getX ${grid})
	y=$(common.runner.getY ${grid})
	z=$(common.runner.getZ ${grid})
	
	echo $(( $x * $y * $z ))
}

################################################################################
# Function:set_variables
#	
# Sets the appropriate variables needed for <run_aqmfad>
################################################################################	
function set_variables() 
################################################################################
{
	local hour
	
	# First of all, reset checks.
	# We will later continuously add entries to these 2 lists.
	# CAREFUL: If you add files to CXR_CHECK_THESE_OUTPUT_FILES,
	# these are deleted if he user runs the -F option. Do not mix up with input files!
	CXR_CHECK_THESE_INPUT_FILES=
	CXR_CHECK_THESE_OUTPUT_FILES=
	
	########################################################################
	# Set variables
	########################################################################
	
	########################################################################
	# per day-per grid settings
	########################################################################
	
	# Grid specific - we need to define CXR_IGRID
	# Read it from the variable
	run_on=($CXR_RUN_AQMFAD_ON_GRID)
	CXR_IGRID=${run_on[$(( $CXR_INVOCATION - 1 ))]}
	
	#aqmfad needs ASCII Input
	CXR_AVG_INPUT_FILE=$(common.runner.evaluateRule "$CXR_AVG_FILE_RULE" false CXR_AVG_FILE_RULE)

	# TERRAIN is still read ASCII
	CXR_TERRAIN_GRID_ASC_INPUT_FILE=$(common.runner.evaluateRule "$CXR_TERRAIN_ASC_FILE_RULE" false CXR_TERRAIN_ASC_FILE_RULE)

	# Pressure
	CXR_ZP_GRID_INPUT_FILE=$(common.runner.evaluateRule "$CXR_PRESSURE_FILE_RULE" false CXR_PRESSURE_FILE_RULE)
	
	# Wind
	CXR_WIND_GRID_INPUT_FILE=$(common.runner.evaluateRule "$CXR_WIND_FILE_RULE" false CXR_WIND_FILE_RULE)

	# Temperature
	CXR_TEMP_GRID_INPUT_FILE=$(common.runner.evaluateRule "$CXR_TEMPERATURE_FILE_RULE" false CXR_TEMPERATURE_FILE_RULE)

	# Vapor
	CXR_VAPOR_INPUT_FILE=$(common.runner.evaluateRule "$CXR_VAPOR_FILE_RULE" false CXR_VAPOR_FILE_RULE)

	# No Cloud
	# Vertical K
	CXR_KV_GRID_INPUT_FILE=$(common.runner.evaluateRule "$CXR_K_FILE_RULE" false CXR_K_FILE_RULE)
	
	# NO Emissions
	
	#Checks
	CXR_CHECK_THESE_INPUT_FILES="${CXR_AVG_INPUT_FILE} \
 ${CXR_TERRAIN_GRID_ASC_INPUT_FILE} \
 ${CXR_ZP_GRID_INPUT_FILE} \
 ${CXR_WIND_GRID_INPUT_FILE} \
 ${CXR_TEMP_GRID_INPUT_FILE} \
 ${CXR_VAPOR_INPUT_FILE} \
 ${CXR_KV_GRID_INPUT_FILE}"

	# The outputfile checks cannot be formulated using rules yet...
	# results in something like
	# ~/@direct/camx-v4.51-bafu3-june-2006-s147-sem302-1only.20060621.avrg.grd01.asc_01 ...
	for hour in $(seq 1 24)
	do
		CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES $CXR_DIRECT_OUTPUT_DIR/$(common.string.toLower $(basename ${CXR_AVG_INPUT_FILE})).asc_$(common.string.leftPadZero $hour 2)"
	done

}

################################################################################
# Function: run_aqmfad
#	
# Runs the PSI postprocessor aqmfad for the current day (all grids mentioned in CXR_RUN_AQMFAD_ON_GRID)
# This preprocessor creates Fortran direct access files needed for the PSI visualisation tool aqm.
#
# Both aqmfad and aqm where written by Michel Tinguely.
################################################################################	
function run_aqmfad() 
################################################################################
{
	# In this module, CXR_INVOCATION corresponds to the entry in CXR_RUN_AQMFAD_ON_GRID
	CXR_INVOCATION=${1}
	
	local ofile

	#Was this stage already completed?
	if [[ $(common.state.storeStatus ${CXR_STATUS_RUNNING}) == true  ]]
	then
		# --- Setup the Environment of the current day
		set_variables 
		
		# --- Check Settings
		# Postprocessor: we only terminate the module
		if [[ $(common.check.preconditions) == false ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi

		# Test if any of the output file pre-exists
		for ofile in $CXR_CHECK_THESE_OUTPUT_FILES;
		do
			if [[ -e "${ofile}" ]]
			then
				if [[ "${CXR_SKIP_EXISTING}" == true ]]
				then
					# Ups, we skip this one
					main.log -w "File ${ofile} exists, run_aqmfad will not be run"
					common.state.storeStatus ${CXR_STATUS_SUCCESS}  > /dev/null
					return $CXR_RET_OK
				else
					main.log -e  "File ${${ofile}} exists - to force the re-creation run ${CXR_CALL} -F"
					common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
					return $CXR_RET_ERROR
				fi
			fi
		done
		
		# Do it.
		if [[ "$CXR_DRY" == false ]]
		then
			main.log -a  "Running aqmfad on grid ${CXR_IGRID}..."
			main.log -a  "${CXR_AQMFAD_EXEC} fi_aqm=${CXR_AVG_INPUT_FILE} fi_terrain=${CXR_TERRAIN_GRID_ASC_INPUT_FILE} fi_zp=${CXR_ZP_GRID_INPUT_FILE} fi_t=${CXR_TEMP_GRID_INPUT_FILE} fi_q=${CXR_VAPOR_INPUT_FILE} fi_kv=${CXR_KV_GRID_INPUT_FILE} fi_uv=${CXR_WIND_GRID_INPUT_FILE}"    

			# Call aqmfad while collecting stderr only
			${CXR_AQMFAD_EXEC} fi_aqm=${CXR_AVG_INPUT_FILE} fi_terrain=${CXR_TERRAIN_GRID_ASC_INPUT_FILE} fi_zp=${CXR_ZP_GRID_INPUT_FILE} fi_t=${CXR_TEMP_GRID_INPUT_FILE} fi_q=${CXR_VAPOR_INPUT_FILE} fi_kv=${CXR_KV_GRID_INPUT_FILE} fi_uv=${CXR_WIND_GRID_INPUT_FILE} 2>> $CXR_LOG
		else
			main.log "This is a dryrun, no action required"
		fi

		
		cd ${CXR_RUN_DIR} || return $CXR_RET_ERROR
		
		# Check if all went well
		# Postprocessor: we only terminate the module
		if [[ $(common.check.postconditions) == false ]]
		then
			main.log -a "Postconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			common.state.storeStatus ${CXR_STATUS_FAILURE} > /dev/null
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_POSTCONDITIONS
		fi
		
		common.state.storeStatus ${CXR_STATUS_SUCCESS} > /dev/null
	else
		main.log "Stage $(common.task.getId) was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
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


