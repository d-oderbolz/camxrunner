# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Preprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
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

CXR_META_MODULE_DEPENDS_ON=""

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Converts meteorological files from ASCII to Binary"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_ONCE}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=1

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
	# This module needs one invocation per step
	echo 1
}

################################################################################
# Function: getProblemSize
#
# Returns the problem size of a given invocation.
# If the problem size is constant, return 1.
#
################################################################################
function getProblemSize()
################################################################################
{
	# The Problem size here is not a function of the invocation
	echo $(common.runner.countAllCells3D)
}

################################################################################
# Function: set_variables
#	
# Sets the appropriate variables needed for <convert_meteo>.
#
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
	
	########################################
	# For the ZP files we need all the first domain values
	# but for all days, so one file per day
	########################################	

	# Set CXR_IGRID to one, we only want the master domain
	CXR_IGRID=1

	# Set date vars using invocation as day offset
	common.date.setVars "$CXR_START_DATE" "$CXR_INVOCATION"

	# The input (binary)
	CXR_INPUT_FILE="$(common.runner.evaluateRule "$CXR_PRESSURE_FILE_RULE" false CXR_PRESSURE_FILE_RULE)"
	
	# The output (ascii)
	CXR_OUTPUT_FILE="$(common.runner.evaluateRule "$CXR_PRESSURE_ASC_FILE_RULE" false CXR_PRESSURE_ASC_FILE_RULE false)"
	
	#Add checks
	CXR_CHECK_THESE_INPUT_FILES="${CXR_CHECK_THESE_INPUT_FILES} ${CXR_INPUT_FILE}"
	CXR_CHECK_THESE_OUTPUT_FILES="${CXR_CHECK_THESE_OUTPUT_FILES} ${CXR_OUTPUT_FILE}"
}

################################################################################
# Function: convert_meteo
#	
# Converts the meteo zp files (for initial and boundary conditions and aqmfad)
# to ascii
#
# Parameters:
# $1 - the invocation (1..n) - here directly used as day offset
################################################################################
function convert_meteo() 
################################################################################
{
	# In this module, CXR_INVOCATION is the day offset
	CXR_INVOCATION=${1}
	
	local DayOffset
	
	#Was this stage already completed?
	if [[ $(common.state.storeStatus ${CXR_STATUS_RUNNING}) == true  ]]
	then
		#  --- Setup the Environment
		set_variables 
		
		#  --- Check Settings
		if [[ $(common.check.preconditions) == false  ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
			common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		if [[ "$CXR_DRY" == false  ]]
		then

			# Does the output already exist?
			if [[ -s "${CXR_OUTPUT_FILE}" ]]
			then
				main.log -w "File ${CXR_OUTPUT_FILE} exists - file will skipped."
			else
				# Call the converter, collect sterr and stout
				# 0 indicates stdout for logging
				main.log "Calling ${CXR_UAMVASCII_EXEC} ${CXR_INPUT_FILE} ${CXR_OUTPUT_FILE} HEIGHT $(common.runner.getX ${CXR_IGRID}) $(common.runner.getY ${CXR_IGRID}) $(common.runner.getZ ${CXR_IGRID}) /dev/null 2>&1 | tee -a $CXR_LOG"
				${CXR_UAMVASCII_EXEC} ${CXR_INPUT_FILE} ${CXR_OUTPUT_FILE} HEIGHT $(common.runner.getX ${CXR_IGRID}) $(common.runner.getY ${CXR_IGRID}) $(common.runner.getZ ${CXR_IGRID}) /dev/null 2>&1 | tee -a $CXR_LOG
			fi
		else
			main.log  "This is a dry-run, no action required"
		fi
	
		# Decrease global indent level
		main.decreaseLogIndent
	
		# Check if all went well
		if [[ $(common.check.postconditions) == false  ]]
		then
			main.log -a "Postconditions for ${CXR_META_MODULE_NAME} are not met!"
			common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_POSTCONDITIONS
		fi

		# Store the state
		common.state.storeStatus ${CXR_STATUS_SUCCESS} > /dev/null
	else
		main.log  "Stage $(common.task.getId) was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
	fi
}

################################################################################
# Function: test_module
#
# Runs the predefined tests for this module
# 
# Parameters:
################################################################################	
function test_module()
################################################################################
{
	########################################
	# Setup tests if needed
	########################################
	
	# For this module, testing is harder 
	# compared to date_functions because we cannot just compare
	# Expected with actual results
	convert_meteo 1
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.fs.isNotEmpty? ${CXR_OUTPUT_FILES[0]}) true "convert_meteo simple existence check, inspect ${CXR_OUTPUT_FILES[0]}"
	
	
	########################################
	# teardown tests if needed
	########################################
}
