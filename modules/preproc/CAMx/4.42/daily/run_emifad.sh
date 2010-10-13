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
# ${CXR_TYPE_PREPROCESS_ONCE} - all pre_start_preprocessors must have finished
# ${CXR_TYPE_PREPROCESS_DAILY} - all daily_preprocessors must have finished
# ${CXR_TYPE_MODEL} - all model modules must have finished
# ${CXR_TYPE_POSTPROCESS_DAILY} - all daily_postprocessors must have finished
# ${CXR_TYPE_POSTPROCESS_ONCE} - all finish_postprocessors must have finished

# the predicate "-<n>" refers to some previous model day, so ${CXR_TYPE_MODEL}-1 means that all model modules of the previous day must be successful before this module may run. 

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
	echo  $(main.countDelimitedElements "$CXR_RUN_EMIFAD_ON_GRID" " ")
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
	
	# The grid is the i-1 element of the list CXR_RUN_EMIFAD_ON_GRID
	arr=($CXR_RUN_EMIFAD_ON_GRID)
	grid=${arr[$(( $invocation - 1 )) ]}
	
	x=$(common.runner.getX ${grid})
	y=$(common.runner.getY ${grid})
	z=$(common.runner.getZ ${grid})
	
	echo $(( $x * $y * $z ))
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
	
	# We work in a temp dir. Here we store links to all the input files,
	# wherever they are.
	emifad_dir="$(common.runner.createTempDir emifad)"
	
	########################################################################
	# Set variables
	########################################################################
	
	# Grid specific - we need to define CXR_IGRID
	# Read it from the variable
	run_on=($CXR_RUN_EMIFAD_ON_GRID)
	CXR_IGRID=${run_on[$(( $CXR_INVOCATION - 1 ))]}
	
	########################################################################
	# per day-per grid settings
	########################################################################

	#emifad needs ASCII Input

	# TERRAIN
	CXR_TERRAIN_GRID_ASC_INPUT_FILE=$(common.runner.evaluateRule "$CXR_TERRAIN_ASC_FILE_RULE" false CXR_TERRAIN_ASC_FILE_RULE)
	# Create link
	ln -s -t $emifad_dir $CXR_TERRAIN_GRID_ASC_INPUT_FILE 
	
	# Emissions
	CXR_EMISSION_GRID_ASC_INPUT_FILE=$(common.runner.evaluateRule "$CXR_EMISSION_ASC_FILE_RULE" false CXR_EMISSION_ASC_FILE_RULE)
	# Create link
	ln -s -t $emifad_dir $CXR_EMISSION_GRID_ASC_INPUT_FILE 
	
	#Checks
	CXR_CHECK_THESE_INPUT_FILES="$emifad_dir/$(basename ${CXR_TERRAIN_GRID_ASC_INPUT_FILE}) \
							$emifad_dir/$(basename ${CXR_EMISSION_GRID_ASC_INPUT_FILE})"

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
	# In this module, CXR_INVOCATION corresponds corresponds to the entry in CXR_RUN_EMIFAD_ON_GRID
	CXR_INVOCATION=${1}
	
	#Was this stage already completed?
	if [[ $(common.state.storeStatus ${CXR_STATUS_RUNNING}) == true  ]]
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
		
		# Emifad uses the aqmfad directory
		cd $emifad_dir || return $CXR_RET_ERROR
		
		# We loop through all the grids we need
		main.log "Running emifad on grid $CXR_IGRID"

		main.log "Running emifad on grid ${CXR_IGRID}..."
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