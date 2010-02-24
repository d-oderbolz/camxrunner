#!/usr/bin/env bash
#
# Postprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Compares the model output of the current run to another run.
# Normally this processor is not needed - it is targeted towards 
# the ENVIRON Testcase
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
# all_once_preprocessors - all pre_start_preprocessors must have finished
# all_daily_preprocessors - all daily_preprocessors must have finished
# all_model - all model modules must have finished
# all_daily_postprocessors - all daily_postprocessors must have finished
# all_once_postprocessors - all finish_postprocessors must have finished

# the special predicate - refers to the previous model day, so all_model- means that all model modules of the previous day must be successful

CXR_META_MODULE_DEPENDS_ON="all_model"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Compares the model output of the current run to another run.\nNormally this processor is not needed - it is targeted towards the ENVIRON Testcase"

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

# just needed for stand-alone usage help
progname=$(basename $0)
################################################################################

################################################################################
# Function: usage
#
# Shows that this script can only be used from within the CAMxRunner
# For common scripts, remove the reference to CAMxRunner options
#
################################################################################
function usage() 
################################################################################
{
	# At least in theory compatible with help2man
	cat <<EOF

	$progname - A part of the CAMxRunner tool chain.

	Can ONLY be called by the CAMxRunner.
	
	If you want to run just this part of the processing,
	look at the options 
	-D (to process one day),
	-i (a step of the input prep) and 
	-o (a part of the output prep) of the CAMxRunner
	
	Written by $CXR_META_MODULE_AUTHOR
	License: $CXR_META_MODULE_LICENSE
	
	Find more info here:
	$CXR_META_MODULE_DOC_URL
EOF
exit 1
}

################################################################################
# Function: set_variables
#	
# Sets the appropriate variables for <avgdif>
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
	
	# Output files must not be decompressed!
	CXR_AVGDIF_OUTPUT_FILE=$(cxr_common_evaluate_rule "$CXR_AVGDIF_OUTPUT_FILE_RULE" false CXR_AVGDIF_OUTPUT_FILE_RULE false)
	
	#Checks
	CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES $CXR_AVGDIF_OUTPUT_FILE"
	
	# Station dependent data
	for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
	do
		# The files produced in this run
		CXR_TEST_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_AVG_FILE_RULE" false CXR_AVG_FILE_RULE)
		
		# The reference files
		CXR_REFERENCE_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_REFERENCE_FILE_RULE" false CXR_REFERENCE_FILE_RULE)
	
		#Checks
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_TEST_INPUT_ARR_FILES[${i}]} ${CXR_REFERENCE_INPUT_ARR_FILES[${i}]}"
	done
	
}

################################################################################
# Function: avgdif
#	
# Compares the model output of the current run to another run using the avgdif postprocessor
################################################################################	
function avgdif
################################################################################
{
	#Was this stage already completed?
	if [ $(cxr_common_store_state ${CXR_STATE_START}) == true ]
	then	
		#  --- Setup the Environment of the current day
		set_variables 
		
		#  --- Check Settings
		# Postprocessor: we only terminate the module
		if [ $(cxr_common_check_preconditions) == false ]
		then
			cxr_main_logger "${FUNCNAME}" "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		cxr_main_logger "${FUNCNAME}" "Comparing Model output to existing model data..."    
		
		# Loop over grids
		for i in $(seq 1 $(($CXR_NUMBER_OF_GRIDS)) );
		do
			cxr_main_logger "${FUNCNAME}" "Comparing ${CXR_REFERENCE_INPUT_ARR_FILES[${i}]} and ${CXR_TEST_INPUT_ARR_FILES[${i}]}\nOutput will be in $CXR_AVGDIF_OUTPUT_FILE"
			
			# Put call into this file
			EXEC_TMP_FILE=$(cxr_common_create_tempfile $FUNCNAME)
			
			# Build tempfile
			cat <<-EOF > $EXEC_TMP_FILE
			$CXR_AVGDIF_OUTPUT_FILE
			${CXR_REFERENCE_INPUT_ARR_FILES[${i}]}
			${CXR_TEST_INPUT_ARR_FILES[${i}]}
			${CXR_AVGDIF_MIN_LAYER} ${CXR_AVGDIF_MAX_LAYER}
			EOF
			
			# Get a copy of the call
			cxr_main_logger "${FUNCNAME}" "Calling AVGDIF - using this jobfile (be patient)...\n"
			cat ${EXEC_TMP_FILE} | tee -a $CXR_LOG
			
			#Dry?
			if [ "$CXR_DRY" == false ]
			then
				# Call AVGDIF (never mind the strange calling convention...)
				$CXR_AVGDIF_EXEC < $EXEC_TMP_FILE
			else
				cxr_main_logger "${FUNCNAME}"  "This is a dry-run, no action required"    
			fi
		
		done
		
		# Check if all went well
		# Postprocessor: we only terminate the module
		if [ $(cxr_common_check_result) == false ]
		then
			cxr_main_logger "${FUNCNAME}" "Postconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			# We notify the caller of the problem
			return $CXR_RET_ERR_POSTCONDITIONS
		fi
		
		cxr_common_store_state ${CXR_STATE_STOP} > /dev/null
	else
		cxr_main_logger "${FUNCNAME}" "${FUNCNAME}:${LINENO} - Stage $(cxr_common_get_stage_name) was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
	fi
}

################################################################################
# Are we running stand-alone? - Can only show help
################################################################################

# If the CXR_META_MODULE_NAME  is not set,
# somebody started this script alone
if [ -z "${CXR_META_MODULE_NAME:-}"  ]
then
	usage
fi

################################################################################
# Code beyond this point is not executed in stand-alone operation
################################################################################




