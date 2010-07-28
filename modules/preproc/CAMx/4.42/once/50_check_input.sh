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
# ${CXR_DEP_ALL_ONCE_PRE} - all pre_start_preprocessors must have finished
# ${CXR_DEP_ALL_DAILY_PRE} - all daily_preprocessors must have finished
# ${CXR_DEP_ALL_MODEL} - all model modules must have finished
# ${CXR_DEP_ALL_DAILY_POST} - all daily_postprocessors must have finished
# ${CXR_DEP_ALL_ONCE_POST} - all finish_postprocessors must have finished

# the predicate "-"refers to the previous model day, so ${CXR_DEP_ALL_MODEL}- means that all model modules of the previous day must be successful. 

CXR_META_MODULE_DEPENDS_ON="convert_landuse"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Checks if landuse file is OK"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_ONCE}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=1

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
	# We do this since the check is fast and
	# if is "atomic" (either all is good or all is bad)
	echo 1
}

################################################################################
# Function: set_variables
#	
# Sets the appropriate variables needed for <check_input>.
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
	# The Landuse is time independent, we have one file per domain
	########################################	
	for CXR_IGRID in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
	do
		# The binary landuse files
		CXR_LANDUSE_FILES[$CXR_IGRID]="$(common.runner.evaluateRule "$CXR_LANDUSE_FILE_RULE" false CXR_LANDUSE_FILE_RULE)"
		
		#Add checks
		CXR_CHECK_THESE_INPUT_FILES="${CXR_CHECK_THESE_INPUT_FILES} ${CXR_LANDUSE_FILES[$CXR_IGRID]}"
	done
}

################################################################################
# Function: check_input
#
# Runs checks on inputs. Currently only landuse is checked, but more might follow later.
################################################################################
function check_input() 
################################################################################
{
	# We do net need this variable here (exept implicit for the stage name)
	CXR_INVOCATION=${1:-1}
	
	# Define & Initialize local vars
	local iGrid
	

	#Was this stage already completed?
	if [[ $(common.state.storeState ${CXR_STATE_START}) == true  ]]
	then
		#  --- Setup the Environment
		set_variables 
		
		#  --- Check Settings
		if [[ $(common.check.preconditions) == false  ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
			common.state.storeState ${CXR_STATE_ERROR}
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		#Run this even in dry run

		### Go through all grids
		for iGrid in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
		do
			
			# Put call into this file
			EXEC_TMP_FILE=$(common.runner.createTempFile $FUNCNAME)
			
			# Build tempfile
			# The 3rd argument is just the filename without extension
			cat <<-EOF > $EXEC_TMP_FILE
			$(common.runner.getX $iGrid),$(common.runner.getY $iGrid),$(common.runner.getZ $iGrid),${CXR_MASTER_ORIGIN_XCOORD},${CXR_MASTER_ORIGIN_YCOORD}
			${CXR_LANDUSE_FILES[$iGrid]}
			${CXR_LANDUSE_FILES[$iGrid]%\.*}
			EOF
			
			main.log "Checking Landuse files using this script..."
			cat ${EXEC_TMP_FILE} | tee -a $CXR_LOG
			
			# Run SRFLND
			if [[ "$CXR_DRY" == false  ]]
			then
				${CXR_SRFLND_EXEC} < $EXEC_TMP_FILE
			else
				main.log   "This is a dry-run, no action required"    
			fi
		
			# Check the return value (0 means OK)
			if [[ $? -ne 0  ]]
			then
				main.log -a "Postconditions for ${CXR_META_MODULE_NAME} are not met!"
				common.state.storeState ${CXR_STATE_ERROR}
			
				# We notify the caller of the problem
				return $CXR_RET_ERR_POSTCONDITIONS
			fi

		done

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
	check_input
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $? 0 "check_input simple return value check"
	
	
	########################################
	# teardown tests if needed
	########################################
	
	
}
