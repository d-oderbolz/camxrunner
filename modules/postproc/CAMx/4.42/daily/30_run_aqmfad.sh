# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Postprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
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
CXR_META_MODULE_DESCRIPTION="Runs the program aqmfad, which creates direct access files for aqm, a PSI/LAC visualisation tool\nNot portable!"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_POSTPROCESS_DAILY}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=0

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

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
	# This module needs one invocation per grid the user wants to precoss per step
	echo  $(main.countDelimitedElements "$CXR_RUN_AQMFAD_ON_GRID" " ")
}

################################################################################
# Function:set_variables
#	
# Sets the appropriate variables needed for <run_aqmfad>
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
	
	########################################################################
	# Set variables
	########################################################################
	
	########################################################################
	# per day-per grid settings
	########################################################################
	
	# Grid specific - we need to define CXR_IGRID
	CXR_IGRID=$CXR_INVOCATION
	
	#aqmfad needs ASCII Input
	CXR_AVG_ASC_INPUT_FILE=$(common.runner.evaluateRule "$CXR_AVG_ASC_FILE_RULE" false CXR_AVG_ASC_FILE_RULE)
	
	# TERRAIN
	CXR_TERRAIN_GRID_ASC_INPUT_FILE=$(common.runner.evaluateRule "$CXR_TERRAIN_ASC_FILE_RULE" false CXR_TERRAIN_ASC_FILE_RULE)
	# Pressure
	CXR_ZP_GRID_ASC_INPUT_FILE=$(common.runner.evaluateRule "$CXR_PRESSURE_ASC_FILE_RULE" false CXR_PRESSURE_ASC_FILE_RULE)
	# Wind
	CXR_WIND_GRID_ASC_INPUT_FILE=$(common.runner.evaluateRule "$CXR_WIND_ASC_FILE_RULE" false CXR_WIND_ASC_FILE_RULE)
	# Temperature
	CXR_TEMP_GRID_ASC_INPUT_FILE=$(common.runner.evaluateRule "$CXR_TEMPERATURE_ASC_FILE_RULE" false CXR_TEMPERATURE_ASC_FILE_RULE)
	# Vapor
	CXR_VAPOR_ASC_INPUT_FILE=$(common.runner.evaluateRule "$CXR_VAPOR_ASC_FILE_RULE" false CXR_VAPOR_ASC_FILE_RULE)
	# No Cloud
	# Vertical K
	CXR_KV_GRID_ASC_INPUT_FILE=$(common.runner.evaluateRule "$CXR_K_ASC_FILE_RULE" false CXR_K_ASC_FILE_RULE)
	# NO Emissions
	
	#Checks
	CXR_CHECK_THESE_INPUT_FILES="${CXR_AVG_ASC_INPUT_FILE} \
								${CXR_TERRAIN_GRID_ASC_INPUT_FILE} \
								${CXR_ZP_GRID_ASC_INPUT_FILE} \
								${CXR_WIND_GRID_ASC_INPUT_FILE} \
								${CXR_TEMP_GRID_ASC_INPUT_FILE} \
								${CXR_VAPOR_ASC_INPUT_FILE} \
								${CXR_KV_GRID_ASC_INPUT_FILE}"

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
	# In this module, CXR_INVOCATION corresponds to the grid number.
	CXR_INVOCATION=${1}

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
		
		cd $CXR_AQMFAD_OUTPUT_DIR || return $CXR_RET_ERROR
		
		# Do it.
		if [[ "$CXR_DRY" == false  ]]
		then
			main.log   "Running aqmfad on grid ${CXR_INVOCATION}..."
			main.log   "${CXR_AQMFAD_EXEC} fi_aqm=$(basename ${CXR_AVG_ASC_INPUT_FILE}) fi_terrain=$(basename ${CXR_TERRAIN_GRID_ASC_INPUT_FILE}) fi_zp=$(basename ${CXR_ZP_GRID_ASC_INPUT_FILE}) fi_t=$(basename ${CXR_TEMP_GRID_ASC_INPUT_FILE}) fi_q=$(basename ${CXR_VAPOR_ASC_INPUT_FILE}) fi_kv=$(basename ${CXR_KV_GRID_ASC_INPUT_FILE}) fi_uv=$(basename ${CXR_WIND_GRID_ASC_INPUT_FILE})"    

			# Call aqmfad while collecting stderr only
			${CXR_AQMFAD_EXEC} fi_aqm=$(basename ${CXR_AVG_ASC_INPUT_FILE}) fi_terrain=$(basename ${CXR_TERRAIN_GRID_ASC_INPUT_FILE}) fi_zp=$(basename ${CXR_ZP_GRID_ASC_INPUT_FILE}) fi_t=$(basename ${CXR_TEMP_GRID_ASC_INPUT_FILE}) fi_q=$(basename ${CXR_VAPOR_ASC_INPUT_FILE}) fi_kv=$(basename ${CXR_KV_GRID_ASC_INPUT_FILE}) fi_uv=$(basename ${CXR_WIND_GRID_ASC_INPUT_FILE}) 2>> $CXR_LOG
		else
			main.log   "This is a dryrun, no action required"
		fi

		
		cd ${CXR_RUN_DIR}  || return $CXR_RET_ERROR
		
		# Check if all went well
		# Postprocessor: we only terminate the module
		if [[ $(common.check.postconditions) == false  ]]
		then
			main.log  "Postconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			common.state.storeState ${CXR_STATE_ERROR}
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_POSTCONDITIONS
		fi
		
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


