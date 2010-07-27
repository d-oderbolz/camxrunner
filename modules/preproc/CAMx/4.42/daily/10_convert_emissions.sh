# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Preprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Conversion of the emission data
#
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
CXR_META_MODULE_DESCRIPTION="Converts the emission data from ASCII to BIN.\nRequires the conversion programs in bin/conv/exec"

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
	# By default, this function returns 1 
	# this module needs one invocation per grid per step
	echo $CXR_NUMBER_OF_GRIDS
}

################################################################################
# Function: set_variables
#	
# Sets the appropriate variables needed for <convert_emissions>.
# Its the first module to use the decmopression capability
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
	
	# Grid specific - we need to define CXR_IGRID
	CXR_IGRID=$CXR_INVOCATION
	
	# Grid specific

	# Emission Source Files
	CXR_EMISSION_INPUT_FILE="$(common.runner.evaluateRule "$CXR_EMISSION_ASC_FILE_RULE" false CXR_EMISSION_ASC_FILE_RULE)"
	
	# Emission Target files
	# Output files must not be decompressed!
	CXR_EMISSION_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_EMISSION_BIN_FILE_RULE" false CXR_EMISSION_BIN_FILE_RULE false)

	# Update Lists of files to be checked
	CXR_CHECK_THESE_INPUT_FILES="${CXR_EMISSION_INPUT_FILE}"
	CXR_CHECK_THESE_OUTPUT_FILES="${CXR_EMISSION_OUTPUT_FILE}"
}

################################################################################
# Function: convert_emissions
#	
# Converts emissions for a given day
################################################################################
function convert_emissions() 
################################################################################
{
	# In this module, CXR_INVOCATION corresponds to the grid number.
	# We die if this parameter is missing
	CXR_INVOCATION=${1}
	
	#Was this stage already completed?
	if [[ $(common.state.storeState ${CXR_STATE_START}) == true  ]]
	then	
		#  --- Setup the Environment of the current day
		set_variables 
		
		#  --- Check Settings
		if [[ $(common.check.preconditions) == false  ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
			common.state.storeState ${CXR_STATE_ERROR}
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		# Increase global indent level
		main.increaseLogIndent

		main.log   "Converting Emission data for domain $CXR_INVOCATION..."
		

		INPUT_FILE=${CXR_EMISSION_INPUT_FILE}
		OUTPUT_FILE=${CXR_EMISSION_OUTPUT_FILE}
	
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

		main.log   "Converting ${INPUT_FILE} to ${OUTPUT_FILE}"     

		if [[ "$CXR_DRY" == false  ]]
		then
			# Call Converter
			${CXR_AIRCONV_EXEC}  ${INPUT_FILE} ${OUTPUT_FILE} EMISSIONS 0 2>&1 | tee -a $CXR_LOG
		else
			main.log   "Dryrun - no conversion performed"
		fi

		# Decrease global indent level
		main.decreaseLogIndent	

		# Decrease global indent level
		main.decreaseLogIndent

		# Check if all went well
		if [[ $(common.check.postconditions) == false  ]]
		then
			main.log -a "Postconditions for ${CXR_META_MODULE_NAME} are not met!"
			common.state.storeState ${CXR_STATE_ERROR}
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_POSTCONDITIONS
		fi

		# Store the state
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