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

# the predicate "-"refers to the previous model day, so ${CXR_TYPE_MODEL}- means that all model modules of the previous day must be successful. 

CXR_META_MODULE_DEPENDS_ON=""

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Generates the emissions for the current day by calling Hannes IDL proc"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_DAILY}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=1

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
	# This module needs one invocation per grid per step
	echo $CXR_NUMBER_OF_GRIDS
}

################################################################################
# Function: set_variables
#	
# Sets the appropriate variables needed for <create_emissions>
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
	
	# We really need the IDL script
	CXR_CHECK_THESE_INPUT_FILES="$CXR_IDL_EMISSION_GENERATOR"
	
	########################################################################
	# Set variables
	########################################################################
	
	# Evaluate some rules
	# Output files must not be decompressed!
	
	# Grid specific - we need to define CXR_IGRID
	CXR_IGRID=$CXR_INVOCATION
	
	# Grid specific

	# This is a bit faked, we just use it to check preconditions
	# the IDL procedures set the names themselves
	CXR_EMISSION_OUTPUT_FILE="$(common.runner.evaluateRule "$CXR_EMISSION_ASC_FILE_RULE" false CXR_EMISSION_ASC_FILE_RULE false)"

	# CXR_CHECK_THESE_OUTPUT_FILES is a space separated list of output files to check
	CXR_CHECK_THESE_OUTPUT_FILES="$CXR_EMISSION_OUTPUT_FILE"

}

################################################################################
# Function: create_emissions
#	
# Generates all emissions needed
################################################################################
function create_emissions() 
################################################################################
{
	# In this module, CXR_INVOCATION corresponds to the grid number.
	CXR_INVOCATION=${1}
	
	# Define & Initialize local vars
	local exec_tmp_file
	local stop_h
	local start_h
	local bionly
	
	#Was this stage already completed?
	if [[ $(common.state.storeStatus ${CXR_STATUS_RUNNING}) == true  ]]
	then
		#  --- Setup the Environment
		set_variables 
		
		#  --- Check Settings
		if [[ $(common.check.preconditions) == false  ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
			common.state.storeStatus ${CXR_STATUS_FAILURE}
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		if [[ ! -f "${CXR_EMISSION_OUTPUT_FILE}"  ]]
		then
			#File does not exist
		
			# Increase global indent level
			main.increaseLogIndent

			# We will write the IDL call into a temporary file
			exec_tmp_file=$(common.runner.createJobFile $FUNCNAME)
			
			main.log  "Creating a temporary IDL command file in $exec_tmp_file"
			
			# Go there
			cd $(dirname ${CXR_IDL_EMISSION_GENERATOR}) || return $CXR_RET_ERROR
			
			# We create the emissions 

			main.log  "Creating Emissions for grid $CXR_INVOCATION..."
			
			# Create the file to run IDL
			# Emissions stop at 23, while the model stops at 24
			# we ned to cut off 2 digits
			if [[ "${CXR_STOP_HOUR}" == 2400 ]]
			then
				stop_h=23
			else
				stop_h=${CXR_STOP_HOUR:0:2}
			fi
			
			start_h=${CXR_START_HOUR:0:2}
			
			################################################################################
			# Determine value of bionly
			################################################################################
			
			#	bionly=-1: only calculate anthropogenic Emissions
			#	bionly=0 : calculate all anthropogenic and biogenic Emissions - we use this the first 7 days
			#	bionly=1 : calculate only biogenic Emissions, ignore anthropogenic
			#	bionly=2 : calculate only biogenic Emissions, use existing anthropogenic for the current weekday - we use this after offset 6
			if [[ "${CXR_DAY_OFFSET}" -gt 6 ]]
			then
				main.log -a "We already have 7 days worth of emission data, anthropoginc data will be taken corresponding to tho current weekday..."
				bionly=2
			else
				main.log -a "We have less than 7 days worth of emission data, will calculate anthropogenic data..."
				bionly=0
			fi
			
			cat <<-EOF > $exec_tmp_file
			.run $(basename ${CXR_IDL_EMISSION_GENERATOR})
			$(basename ${CXR_IDL_EMISSION_GENERATOR} .pro),${CXR_YEAR},${CXR_MONTH},${CXR_DAY},${start_h},${CXR_YEAR},${CXR_MONTH},${CXR_DAY},${stop_h},${CXR_INVOCATION},'${CXR_MET_PROJECT}','${CXR_EMMISS_SCENARIO}','${CXR_MET_SCENARIO}',${bionly},'${CXR_EMISSION_SOURCE_DIR}'
			exit
			EOF
			
			# Only run if we are not in a dry run
			if [[ "$CXR_DRY" == false  ]]
			then
				# Then we run it, while preserving the output
				${CXR_IDL_EXEC} < ${exec_tmp_file} 2>&1 | tee -a $CXR_LOG
			else
				main.log "This is a dry-run, no action required"
			fi

			# go back
			cd ${CXR_RUN_DIR} || main.dieGracefully "Could not change back to ${CXR_RUN_DIR}"
	
			# Decrease global indent level
			main.decreaseLogIndent
	
			# Check if all went well
			if [[ $(common.check.postconditions) == false  ]]
			then
				main.log -a "Postconditions for ${CXR_META_MODULE_NAME} are not met!"
				common.state.storeStatus ${CXR_STATUS_FAILURE}
			
				# We notify the caller of the problem
				return $CXR_RET_ERR_POSTCONDITIONS
			fi
			
		else
			# File exists. That is generally bad,
			# unless user wants to skip
			if [[ "$CXR_SKIP_EXISTING" == true ]]
			then
				# Skip it
				main.log -w   "File $CXR_EMISSION_OUTPUT_FILE exists - because -S option was supplied, file will skipped."
			else
				# Fail!
				main.log -e  "File $CXR_EMISSION_OUTPUT_FILE exists - to force the re-creation run ${CXR_CALL} -F"
				common.state.storeStatus ${CXR_STATUS_FAILURE}
				return $CXR_RET_ERROR
			fi
		fi
		
		# We are fine.
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
	create_emissions
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.fs.isNotEmpty? ${CXR_EMISSION_OUTPUT_FILE}) true "create_emissions simple existence check, inspect ${CXR_EMISSION_OUTPUT_FILE}"
	
	########################################
	# teardown tests if needed
	########################################
	
}
