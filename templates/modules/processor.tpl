# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Processor module for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
#
# Version: $Id$ 
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

CXR_META_MODULE_DEPENDS_ON=""

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="This is an interesting module \n totally untested.\nAlso specify portability"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_DAILY}"

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
CXR_META_MODULE_DOC_URL=-

# Who wrote this module?
CXR_META_MODULE_AUTHOR=-

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE=-

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" NAME" 
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
	# By default, this function returns 1 
	# (If this module needs only one invocation per step)
	# echo 1
	
	# as an example, we now return the number of grids.
	# (If this module needs one invocation per grid per step)
	# This makes only sense if one invocation takes substantial time to execute.
	
	echo $CXR_NUMBER_OF_GRIDS
}

################################################################################
# Function: set_variables
#
# Sets the appropriate variables. Check inhowfar this depends on CXR_INVOCATION.
#
# Parameters:
# $1 - ...
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
	# Define checks
	########################################################################
	
	# If you need special checks on the first or last day,
	# use the functions common.date.isFirstDayOfSimulation? and common.date.isLastDayOfSimulation?
	
	########################################################################
	# Set variables
	########################################################################
	
	# Grid specific - we need to define CXR_IGRID
	CXR_IGRID=$CXR_INVOCATION
	
	# Emission Source File
	INPUT_FILE=$(common.runner.evaluateRule "$CXR_EMISSION_ASC_FILE_RULE" false CXR_EMISSION_ASC_FILE_RULE)

	# Emission Target file
	# Output files must not be decompressed!
	OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_EMISSION_BIN_FILE_RULE" false CXR_EMISSION_BIN_FILE_RULE false)

	# Update Lists of files to be checked
	CXR_CHECK_THESE_INPUT_FILES="$INPUT_FILE"
	CXR_CHECK_THESE_OUTPUT_FILES="$OUTPUT_FILE"

}

################################################################################
# Function: name
#
# Does the actual work. This function gets called from autside and its name must
# be the same as the name of the module file without number and extension.
# So if this file i called 10_my_module.sh this function MUST be called my_module.
#
# The parameter $1 allows the module to infer which part of the work must be done.
# If the module is atomic, this parameter can be ignored.
#
# Parameters:
# $1 - the invocation (1..n) - here directly used as grid number
################################################################################
function name() 
################################################################################
{
	# We set the invocation (default 1)
	# In this module, CXR_INVOCATION corresponds to the grid number.
	# We die, if this parameter is missing
	CXR_INVOCATION=${1}
	
	# We check if this stage was already excuted before
	if [[ $(common.state.storeState ${CXR_STATE_START}) == true  ]]
	then
	
		#  --- Setup the Environment of the current day
		set_variables 
		
		#  --- Check Settings. 
		# Decide if you crash if errors are found or not.
		# We do not stop the run here if the module failed, this is decided by the 
		# task management. We only stop the run, if a task depends on
		# this failed one - otherwise we can go on!
		if [[ $(common.check.preconditions) == false  ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			common.state.storeState ${CXR_STATE_ERROR}
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		main.log   "Running ${FUNCNAME}..."
		
		# Often, the actual work is done once for each grid.
		# as an example, the grid number is the invocation.
		# In a more complicated example, you might need to determine
		# the value of some variables as a function of CXR_INVOCATION
		
		if [[ -f "$OUTPUT_FILE" ]]
		then
			if [[ "$CXR_SKIP_EXISTING" == true ]]
			then
				# Skip it
				main.log   "File ${OUTPUT_FILE} exists - because of CXR_SKIP_EXISTING, file will skipped."
				common.state.storeState ${CXR_STATE_STOP} > /dev/null
				return $CXR_RET_OK
			else
				# Fail
				main.log -e  "File ${OUTPUT_FILE} exists - to force the re-creation run ${CXR_CALL} -F"
				common.state.storeState ${CXR_STATE_ERROR}
				return $CXR_RET_ERROR
			fi
		fi

		# Increase global indent level
		main.increaseLogIndent

		main.log  "Converting ${INPUT_FILE} to ${OUTPUT_FILE}"

		if [[ "$CXR_DRY" == false  ]]
		then
			# Do what needs to be done
			# Normally, there is a call of this form
			# ${CXR_SOME_EXEC}  ${INPUT_FILE} ${OUTPUT_FILE} 2>&1 | tee -a $CXR_LOG
			# Call an exec stored in a variable and redirect output and error to file and stdout.

		else
			main.log -w   "Dryrun - no action performed"
		fi

		# Decrease global indent level
		main.decreaseLogIndent

		# Check if all went well
		# We do not stop the run here if the module failed, this is decided by the 
		# task management. We only stop the run, if a task depends on
		# this failed one - otherwise we can go on!
		if [[ $(common.check.postconditions) == false  ]]
		then
			main.log  "Postconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			common.state.storeState ${CXR_STATE_ERROR}
		
			# We notify the caller of the problem
			return $CXR_RET_ERR_POSTCONDITIONS
		fi
		
		
		# -v option shows message only if the log level is high enough
		main.log -v   "Some verbose message here"
	
		# Store the state
		common.state.storeState ${CXR_STATE_STOP} > /dev/null ${STAGE}
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
	
	# Initialise the date variables for first day
	day_offset=0
	common.date.setVars "$CXR_START_DATE" "$day_offset"
	set_variables
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	# Example test
	# is $(common.fs.isNotEmpty? ${OUTPUT_FILE}) true "simple existence check, inspect ${OUTPUT_FILE}"

	########################################
	# teardown tests if needed
	########################################

}