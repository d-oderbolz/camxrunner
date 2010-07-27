# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Postprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Runs emifad, the script which creates the direct access files for emissions
# We run this as a preprocessor right after emissions have been created.
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

CXR_META_MODULE_DEPENDS_ON="create_emissions"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Runs the program emifad, which creates emission direct access files for aqm, a PSI/LAC visualisation tool\nNot portable!"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_DAILY}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=0

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
	echo  $(main.countDelimitedElements "$CXR_RUN_EMIFAD_ON_GRID" " ")
}


################################################################################
# Function:set_variables
#	
# Sets the appropriate variables needed for <run_emifad>
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
	
	# Grid specific - we need to define CXR_IGRID
	CXR_IGRID=$CXR_INVOCATION
	
	########################################################################
	# per day-per grid settings
	########################################################################

	#emifad needs ASCII Input

	# TERRAIN
	CXR_TERRAIN_GRID_ASC_INPUT_FILE=$(common.runner.evaluateRule "$CXR_TERRAIN_ASC_FILE_RULE" false CXR_TERRAIN_ASC_FILE_RULE)

	# Emissions
	CXR_EMISSION_GRID_ASC_INPUT_FILE=$(common.runner.evaluateRule "$CXR_EMISSION_ASC_FILE_RULE" false CXR_EMISSION_ASC_FILE_RULE)
	
	#Checks
	CXR_CHECK_THESE_INPUT_FILES="${CXR_TERRAIN_GRID_ASC_INPUT_FILE} \
								 ${CXR_EMISSION_GRID_ASC_INPUT_FILE}"

}

################################################################################
# Function: run_emifad
#	
# Runs the PSI postprocessor emifad for the current day (all grids mentioned in CXR_RUN_EMIFAD_ON_GRID)
# This preprocessor creates Fortran direct access files needed for the PSI visualisation tool aqm.
#
# Both emifad and aqm where written by Michel Tinguely.
################################################################################	
function run_emifad() 
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
		
		# Emifad uses the aqmfad directory
		cd $CXR_AQMFAD_OUTPUT_DIR || return $CXR_RET_ERROR
		
		# We loop through all the grids we need
		main.log "Running emifad on grid $CXR_INVOCATION"

		# First we need to create links (if not existing)
		main.log "Creating links in the $CXR_AQMFAD_OUTPUT_DIR directory..."
		
		# Link to emissions
		CURRENT_EMISSION_BASE=$(basename ${CXR_EMISSION_GRID_ASC_INPUT_FILE})
		if [[ ! -L "${CURRENT_EMISSION_BASE}"  ]]
		then
			if [[ -f "${CXR_EMISSION_GRID_ASC_INPUT_FILE}" ]]
			then
				ln -s ${CXR_EMISSION_GRID_ASC_INPUT_FILE}
			else
				main.log -e  "Cannot find the emission file ${CXR_EMISSION_GRID_ASC_INPUT_FILE}"
			fi
		fi
		
		#Link to terrain
		CURRENT_TERRAIN_BASE=$(basename ${CXR_TERRAIN_GRID_ASC_INPUT_FILE})
		if [[ ! ( -L ${CURRENT_TERRAIN_BASE} || -f ${CURRENT_TERRAIN_BASE} ) ]]
		then
			ln -s ${CXR_TERRAIN_GRID_ASC_INPUT_FILE}
		fi
		
		main.log "Running emifad on grid ${CXR_INVOCATION}..."
		main.log  "${CXR_EMIFAD_EXEC} fi_emi=$(basename ${CXR_EMISSION_GRID_ASC_INPUT_FILE}) fi_terrain=$(basename ${CXR_TERRAIN_GRID_ASC_INPUT_FILE})"

		if [[ "$CXR_DRY" == false  ]]
		then
			# Call emifad while collecting only stderr
			${CXR_EMIFAD_EXEC} fi_emi=$(basename ${CXR_EMISSION_GRID_ASC_INPUT_FILE}) fi_terrain=$(basename ${CXR_TERRAIN_GRID_ASC_INPUT_FILE}) 2>> $CXR_LOG
		else
			main.log "This is a dryrun, no action required"
		fi

		# go back
		cd ${CXR_RUN_DIR} || main.dieGracefully "Could not change back to ${CXR_RUN_DIR}"

		# Check if all went well
		# Postprocessor: we only terminate the module
		if [[ $(common.check.postconditions) == false  ]]
		then
			main.log -a "Postconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
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