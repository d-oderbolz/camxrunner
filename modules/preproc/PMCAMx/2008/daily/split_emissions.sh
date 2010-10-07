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

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Runs the split emissions preproc to create splitted particulate emissions"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_DAILY}"

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
	# this module needs one invocation per grid per step
	
	echo $CXR_NUMBER_OF_GRIDS
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
	local x
	local y
	local z
	
	invocation=$1
	
	x=$(common.runner.getX ${invocation})
	y=$(common.runner.getY ${invocation})
	z=$(common.runner.getZ ${invocation})
	
	# The Problem size here is a function of the invocation
	echo $(( $x * $y * $z ))
}

################################################################################
# Function: set_variables
#	
# Sets the appropriate variables needed for <convert_emissions>
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
	
	# Evaluate some rules
	CXR_SPLIT_EMISSIONS_INPUT_FILE="$(common.runner.evaluateRule "$CXR_SPLIT_EMISSIONS_INPUT_FILE_RULE" false CXR_SPLIT_EMISSIONS_INPUT_FILE_RULE)"

	# Output files must not be decompressed!
	CXR_SPLIT_EMISSIONS_OUTPUT_FILE="$(common.runner.evaluateRule "$CXR_SPLIT_EMISSIONS_OUTPUT_FILE_RULE" false CXR_SPLIT_EMISSIONS_OUTPUT_FILE_RULE false)"

	# CXR_CHECK_THESE_INPUT_FILES is a space separated list of output files to check
	CXR_CHECK_THESE_INPUT_FILES="$CXR_SPLIT_EMISSIONS_INPUT_FILE"

	# CXR_CHECK_THESE_OUTPUT_FILES is a space separated list of output files to check
	CXR_CHECK_THESE_OUTPUT_FILES="$CXR_SPLIT_EMISSIONS_OUTPUT_FILE"
}

################################################################################
# Function: split_emissions
#	
# Splits emissions for a given day
#
################################################################################
function split_emissions() 
################################################################################
{
	# In this module, CXR_INVOCATION corresponds to the grid number.
	# We die, if this parameter is missing
	CXR_INVOCATION=${1}
	
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
		
		# Increase global indent level
		main.increaseLogIndent

		main.log   "Splitting Emission data"
		
		# Is the output there?
		if [[ ! -f "$CXR_SPLIT_EMISSIONS_OUTPUT_FILE"  ]]
		then
			# File not yet there
		
			if [[ "$CXR_DRY" == false  ]]
			then
			
					main.log  "Calling split_emissions_area - be patient...\n"
			
					# Call Processor 
					${CXR_SPLIT_EMISSIONS_AREA_EXEC} <<-EOT 2>&1 | tee -a $CXR_LOG
					# of input files  | 1
					Input File        | $CXR_SPLIT_EMISSIONS_INPUT_FILE
					Output File       | $CXR_SPLIT_EMISSIONS_OUTPUT_FILE
					EOT
	
			else
				main.log   "Dryrun - splitting not performed"
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
		else
			# File exists. That is generally bad,
			# unless user wants to skip
			if [[ "$CXR_SKIP_EXISTING" == true ]]
			then
				# Skip it
				main.log -w  "File $CXR_SPLIT_EMISSIONS_OUTPUT_FILE exists, because of CXR_SKIP_EXISTING, file will skipped."
				# Store the state
				common.state.storeStatus ${CXR_STATUS_SUCCESS} > /dev/null
				return 0
			else
				# Fail!
				main.log -e  "File $CXR_SPLIT_EMISSIONS_OUTPUT_FILE exists - to force the re-creation run ${CXR_CALL} -F"
				return $CXR_RET_ERROR
			fi
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
	
	# Initialise the date variables for first day
	day_offset=0
	common.date.setVars "$CXR_START_DATE" "$day_offset"
	set_variables
	
	# For this module, testing is harder 
	# compared to date_functions because we cannot just compare
	# Expected with actual results
	
	split_emissions 1
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.fs.isNotEmpty? ${CXR_SPLIT_EMISSIONS_OUTPUT_FILE}) true "split_emissions simple existence check, inspect ${CXR_SPLIT_EMISSIONS_OUTPUT_FILE}"
	
	########################################
	# teardown tests if needed
	########################################
	
}
