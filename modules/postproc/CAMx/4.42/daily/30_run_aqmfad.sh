#!/usr/bin/env bash
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
# all_once_preprocessors - all pre_start_preprocessors must have finished
# all_daily_preprocessors - all daily_preprocessors must have finished
# all_model - all model modules must have finished
# all_daily_postprocessors - all daily_postprocessors must have finished
# all_once_postprocessors - all finish_postprocessors must have finished

# the special predicate - refers to the previous model day, so all_model- means that all model modules of the previous day must be successful

CXR_META_MODULE_DEPENDS_ON="prepare_output_dir convert_output"

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
	# we loop
	#	expand the file name rule
	#	then export the name and the value
	########################################################################
	for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
	do
		#aqmfad needs ASCII Input
		
		CXR_AVG_ASC_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_AVG_ASC_FILE_RULE" false CXR_AVG_ASC_FILE_RULE)
		
		# TERRAIN
		CXR_TERRAIN_GRID_ASC_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_TERRAIN_ASC_FILE_RULE" false CXR_TERRAIN_ASC_FILE_RULE)
		# Pressure
		CXR_ZP_GRID_ASC_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_PRESSURE_ASC_FILE_RULE" false CXR_PRESSURE_ASC_FILE_RULE)
		# Wind
		CXR_WIND_GRID_ASC_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_WIND_ASC_FILE_RULE" false CXR_WIND_ASC_FILE_RULE)
		# Temperature
		CXR_TEMP_GRID_ASC_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_TEMPERATURE_ASC_FILE_RULE" false CXR_TEMPERATURE_ASC_FILE_RULE)
		# Vapor
		CXR_VAPOR_ASC_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_VAPOR_ASC_FILE_RULE" false CXR_VAPOR_ASC_FILE_RULE)
		# No Cloud
		# Vertical K
		CXR_KV_GRID_ASC_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_K_ASC_FILE_RULE" false CXR_K_ASC_FILE_RULE)
		# NO Emissions
		
		#Checks
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_AVG_ASC_INPUT_ARR_FILES[${i}]} ${CXR_TERRAIN_GRID_ASC_INPUT_ARR_FILES[${i}]} ${CXR_ZP_GRID_ASC_INPUT_ARR_FILES[${i}]} ${CXR_WIND_GRID_ASC_INPUT_ARR_FILES[${i}]} ${CXR_TEMP_GRID_ASC_INPUT_ARR_FILES[${i}]} ${CXR_VAPOR_ASC_INPUT_ARR_FILES[${i}]} ${CXR_KV_GRID_ASC_INPUT_ARR_FILES[${i}]}"
	done
	

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
	local i
	
	#Was this stage already completed?
	if [[ $(cxr_common_store_state ${CXR_STATE_START}) == true  ]]
	then
		#  --- Setup the Environment of the current day
		set_variables 
		
		#  --- Check Settings
		# Postprocessor: we only terminate the module
		if [[ $(cxr_common_check_preconditions) == false  ]]
		then
			main.log "${FUNCNAME}" "Preconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		cd $CXR_AQMFAD_OUTPUT_DIR || return $CXR_RET_ERROR
		
		# We loop through all the grids we need
		for i in ${CXR_RUN_AQMFAD_ON_GRID};
		do
			if [[ "$CXR_DRY" == false  ]]
			then
				main.log "${FUNCNAME}"  "Running aqmfad on grid ${i}..."
				main.log "${FUNCNAME}"  "${CXR_AQMFAD_EXEC} fi_aqm=$(basename ${CXR_AVG_ASC_INPUT_ARR_FILES[${i}]}) fi_terrain=$(basename ${CXR_TERRAIN_GRID_ASC_INPUT_ARR_FILES[${i}]}) fi_zp=$(basename ${CXR_ZP_GRID_ASC_INPUT_ARR_FILES[${i}]}) fi_t=$(basename ${CXR_TEMP_GRID_ASC_INPUT_ARR_FILES[${i}]}) fi_q=$(basename ${CXR_VAPOR_ASC_INPUT_ARR_FILES[${i}]}) fi_kv=$(basename ${CXR_KV_GRID_ASC_INPUT_ARR_FILES[${i}]}) fi_uv=$(basename ${CXR_WIND_GRID_ASC_INPUT_ARR_FILES[${i}]})"    

				# Call aqmfad while collecting stderr only
				${CXR_AQMFAD_EXEC} fi_aqm=$(basename ${CXR_AVG_ASC_INPUT_ARR_FILES[${i}]}) fi_terrain=$(basename ${CXR_TERRAIN_GRID_ASC_INPUT_ARR_FILES[${i}]}) fi_zp=$(basename ${CXR_ZP_GRID_ASC_INPUT_ARR_FILES[${i}]}) fi_t=$(basename ${CXR_TEMP_GRID_ASC_INPUT_ARR_FILES[${i}]}) fi_q=$(basename ${CXR_VAPOR_ASC_INPUT_ARR_FILES[${i}]}) fi_kv=$(basename ${CXR_KV_GRID_ASC_INPUT_ARR_FILES[${i}]}) fi_uv=$(basename ${CXR_WIND_GRID_ASC_INPUT_ARR_FILES[${i}]}) 2>> $CXR_LOG
			else
				main.log "${FUNCNAME}"  "This is a dryrun, no action required"
			fi
		done
		
		cd ${CXR_RUN_DIR}  || return $CXR_RET_ERROR
		
		# Check if all went well
		# Postprocessor: we only terminate the module
		if [[ $(cxr_common_check_result) == false  ]]
		then
			main.log "${FUNCNAME}" "Postconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			# We notify the caller of the problem
			return $CXR_RET_ERR_POSTCONDITIONS
		fi
		
		cxr_common_store_state ${CXR_STATE_STOP} > /dev/null
	else
		main.log "${FUNCNAME}" "${FUNCNAME}:${LINENO} - Stage $(cxr_common_get_stage_name) was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
	fi
}

################################################################################
# Are we running stand-alone? - Can only show help
################################################################################

# If the CXR_META_MODULE_NAME  is not set,
# somebody started this script alone
if [[ -z "${CXR_META_MODULE_NAME:-}"   ]]
then
	usage
fi

################################################################################
# Code beyond this point is not executed in stand-alone operation
################################################################################




