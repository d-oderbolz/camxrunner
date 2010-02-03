#!/usr/bin/env bash
#
# Postprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Creation of the output postprocessing directory by linking 
# available files into the aqmfad directory
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
CXR_META_MODULE_DESCRIPTION="Creates a directory full of links for the postprocessors (mostly aqmfad)"

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
# Function: set_prepare_output_dir_variables
#	
# Sets the needed variables needed for <prepare_output_dir>
################################################################################
function set_prepare_output_dir_variables() 
################################################################################
{
	# First of all, reset checks.
	# We will later continuously add entries to these 2 lists.
	# CAREFUL: If you add files to CXR_CHECK_THESE_OUTPUT_FILES,
	# these are deleted if he user runs the -F option. Do note mik up with input files!
	CXR_CHECK_THESE_INPUT_FILES=
	CXR_CHECK_THESE_OUTPUT_FILES=
	
	########################################################################
	# Set variables
	########################################################################
	
	# File names here are generated using file name rules (strings containig variables)
	# which are expanded run time. See http://people.web.psi.ch/oderbolz/CAMxRunner#FileRules
	# The expansion is done using $(cxr_common_evaluate_rule "$VAR")

	########################################################################
	# Per-day settings
	########################################################################
	
	########################################################################
	# per day-per grid settings
	# we loop
	#	expand the file name rule
	#	then export the name and the value
	########################################################################
	for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
	do
		# Because the input might be compressed, we use two sets af arrays,
		# the second one contains just the real basenames (these are the link-names)
		# Note the last argument (false) in every even call to <cxr_common_evaluate_rule>
	
		# Terrain
		CXR_TERRAIN_GRID_ASC_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_TERRAIN_ASC_FILE_RULE" false CXR_TERRAIN_ASC_FILE_RULE)
		CXR_TERRAIN_GRID_NAME[${i}]=$(basename $(cxr_common_evaluate_rule "$CXR_TERRAIN_ASC_FILE_RULE" false CXR_TERRAIN_ASC_FILE_RULE false))
		
		# Pressure
		CXR_ZP_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_PRESSURE_FILE_RULE" false CXR_PRESSURE_FILE_RULE)
		CXR_ZP_GRID_NAME[${i}]=$(basename $(cxr_common_evaluate_rule "$CXR_PRESSURE_FILE_RULE" false CXR_PRESSURE_FILE_RULE false))
		
		# Wind
		CXR_WIND_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_WIND_FILE_RULE" false CXR_WIND_FILE_RULE)
		CXR_WIND_GRID_NAME[${i}]=$(basename $(cxr_common_evaluate_rule "$CXR_WIND_FILE_RULE" false CXR_WIND_FILE_RULE false))
		
		# Temperature
		CXR_TEMP_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_TEMPERATURE_FILE_RULE" false CXR_TEMPERATURE_FILE_RULE)
		CXR_TEMP_GRID_NAME[${i}]=$(basename $(cxr_common_evaluate_rule "$CXR_TEMPERATURE_FILE_RULE" false CXR_TEMPERATURE_FILE_RULE false))
		
		# Vapor
		CXR_VAPOR_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_VAPOR_FILE_RULE" false CXR_VAPOR_FILE_RULE)
		CXR_VAPOR_GRID_NAME[${i}]=$(basename $(cxr_common_evaluate_rule "$CXR_VAPOR_FILE_RULE" false CXR_VAPOR_FILE_RULE false))
		
		# Vertical K
		CXR_KV_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_K_FILE_RULE" false CXR_K_FILE_RULE)
		CXR_KV_GRID_NAME[${i}]=$(basename $(cxr_common_evaluate_rule "$CXR_K_FILE_RULE" false CXR_K_FILE_RULE false))
		
		# Emissions done in run_emifad 
		
		# These are used for the creation of the aqmfad directory
		# Despite the name an input here
		CXR_AVG_OUTPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_AVG_FILE_RULE" false CXR_AVG_FILE_RULE)
		CXR_AVG_OUTPUT_NAME[${i}]=$(basename $(cxr_common_evaluate_rule "$CXR_AVG_FILE_RULE" false CXR_AVG_FILE_RULE false))
	
		#Checks
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_TERRAIN_GRID_ASC_INPUT_ARR_FILES[${i}]} ${CXR_ZP_GRID_INPUT_ARR_FILES[${i}]} ${CXR_WIND_GRID_INPUT_ARR_FILES[${i}]} ${CXR_TEMP_GRID_INPUT_ARR_FILES[${i}]} ${CXR_VAPOR_INPUT_ARR_FILES[${i}]} ${CXR_KV_GRID_INPUT_ARR_FILES[${i}]} ${CXR_AVG_OUTPUT_ARR_FILES[${i}]}"
	
	done
	
	# We will later loop over these arrays
	CXR_INPUT_ARRAYS=(CXR_TERRAIN_GRID_ASC_INPUT_ARR_FILES CXR_ZP_GRID_INPUT_ARR_FILES CXR_WIND_GRID_INPUT_ARR_FILES CXR_TEMP_GRID_INPUT_ARR_FILES CXR_VAPOR_INPUT_ARR_FILES CXR_KV_GRID_INPUT_ARR_FILES CXR_AVG_OUTPUT_ARR_FILES)
	CXR_NAME_ARRAYS=(CXR_TERRAIN_GRID_NAME CXR_ZP_GRID_NAME CXR_WIND_GRID_NAME CXR_TEMP_GRID_NAME CXR_VAPOR_GRID_NAME CXR_KV_GRID_NAME CXR_AVG_OUTPUT_NAME)
	
}

################################################################################
# Function: prepare_output_dir
#	
# Creates a directory for aqmfad containing softlinks to the input files
# By going through the Arrays containig the filenames
################################################################################
function prepare_output_dir() 
################################################################################
{

	if [ "${CXR_RUN_LIMITED_PROCESSING}" == true -a "${CXR_USER_TEMP_REMOVE_TEMP_FILES}" == true ]
	then
		cxr_main_logger -w "${FUNCNAME}"  "This module is susceptible to limited processing, because it creates links rather than files.\nIf you use compressed input files, the temporary files into which we decompress wil be deleted.\nUse \n \t ${CXR_CALL} -L \nto avoid this."
	fi

	#Was this stage already completed?
	if [ $(cxr_common_store_state ${CXR_STATE_START}) == true ]
	then
		#  --- Setup the Environment of the current day
		set_prepare_output_dir_variables 
		
		#  --- Check Settings
		# Postprocessor: we only terminate the module
		if [ $(cxr_common_check_preconditions) == false ]
		then
			cxr_main_logger "${FUNCNAME}" "Preconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		cd ${CXR_AQMFAD_OUTPUT_DIR} || return $CXR_RET_ERROR
		
		# We loop through all the grids
		# Therefore we let seq create the numbers from 1 to ${CXR_NUMBER_OF_GRIDS}
		for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
		do
			# Loo through the name of all the input arrays
			for iArr in $(seq 1 $(( ${#SA_REGIONS_DOMAIN_NUMBERS[@]} - 1 )))
			do
				VAR=${CXR_INPUT_ARRAYS[$iArr]}
				VAR_NAME=${CXR_NAME_ARRAYS[$iArr]}
			
				# Only if the link is not there, create a new one
				CURRENT_FILE=$(eval "echo \${${VAR}[${i}]}")
				CURRENT_BASE=$(eval "echo \${${VAR_NAME}[${i}]}")
				
				# If the Link or file does not yet exist
				if [ ! \( -L ${CURRENT_BASE} -o -f ${CURRENT_BASE} \) ]
				then
					if [ "$CXR_DRY" == "false" ]
					then
						ln -s ${CURRENT_FILE} ${CURRENT_BASE}
					else
						cxr_main_logger "${FUNCNAME}"  "This is a dry run, do not create a link for output processing"
					fi
				fi
			done
		done
		
		cd ${CXR_RUN_DIR}  || return $CXR_RET_ERROR

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




