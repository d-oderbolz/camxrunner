#!/usr/bin/env bash
#
# Postprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Extracts NABEL or other station data
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
CXR_META_MODULE_DESCRIPTION="Extracts data for defined measurement stations"

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
# Function: set_extract_station_data_variables
#	
# Sets the appropriate variables needed for <extract_station_data>
################################################################################
function set_extract_station_data_variables() 
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
	
	# IDL-based extraction of NABEL data needs just the 
	# data from this specific grid in ASCII data
	i=${CXR_STATION_DOMAIN}
	CXR_STATION_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_AVG_ASC_FILE_RULE" false CXR_AVG_ASC_FILE_RULE)

	# the MM5 file to use
	# only used for ARPA stuff
	CXR_METEO_INPUT_FILE="$(cxr_common_evaluate_rule "$CXR_MMOUT_FILE_RULE" false CXR_MMOUT_FILE_RULE)"

	#Checks
	CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_STATION_INPUT_FILE $CXR_STATION_PROC_INPUT_FILE $CXR_METEO_INPUT_FILE"

	# Station dependent data
	for i in $(seq 0 $(($CXR_NUMBER_OF_STATIONS-1)) );
	do
		# Needed to expand the file rule
		station=${CXR_STATION[${i}]}
		
		# Output files must not be decompressed!
		CXR_STATION_OUTPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_STATION_FILE_RULE" false CXR_STATION_FILE_RULE false)
		
		# Checks
		CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES ${CXR_STATION_OUTPUT_ARR_FILES[${i}]}"
	done

}

################################################################################
# Function: extract_station_data
#	
# Extracts the model data (innermost domain) from the locations of the stations
# given in CXR_STATION_X and CXR_STATION_Y.
#
# Calls IDL Procedure <extract_nabel_stations>
################################################################################	
function extract_station_data
################################################################################
{
	#Was this stage already completed?
	if [ $(cxr_common_store_state ${CXR_STATE_START}) == true ]
	then

		#  --- Setup the Environment of the current day
		set_extract_station_data_variables 
		
		#  --- Check Settings
		# Postprocessor: we only terminate the module
		if [ $(cxr_common_check_preconditions) == false ]
		then
			cxr_main_logger "${FUNCNAME}" "Preconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		# Generate Temp file name
		EXEC_TMP_FILE=$(cxr_common_create_tempfile $FUNCNAME)
		
		# Calculate extension of grid to extract
		XDIM=$(cxr_common_get_x_dim $CXR_STATION_DOMAIN)
		YDIM=$(cxr_common_get_y_dim $CXR_STATION_DOMAIN)
		
		# The Z dim depends on wether we use 3D output
		if [ "${CXR_AVERAGE_OUTPUT_3D}" == true ]
		then
			# 3D
			ZDIM=$(cxr_common_get_z_dim $CXR_STATION_DOMAIN)
		else
			# Only 1 layer
			ZDIM=1
		fi
		
		# We have to prepare the stations array [x,y,filename]
		# and the species to extract array [speciesname]

		# Stations

		# Open brackets
		STATIONS_ARRAY="["
		
		for i in $(seq 0 $(($CXR_NUMBER_OF_STATIONS-1)) )
		do
			
			# The output file of a given station
			# We remove the path from it to save space in the call
			# (easier to read)
			station_file="$(basename ${CXR_STATION_OUTPUT_ARR_FILES[${i}]})"
			
			# In case of a dry-run, create a dummy file
			if [ "$CXR_DRY" == true ]
			then
				cxr_common_create_dummyfile ${CXR_STATION_OUTPUT_DIR}/${station_file}
			fi
			
			# The position of this station
			x=${CXR_STATION_X[${i}]}
			y=${CXR_STATION_Y[${i}]}
			
			# We pass all in a string array, IDL then does the conversion
			STATION="['${x}','${y}','${station_file}']"
			
			# Here we need not single quotes, because we have a 2D array
			STATIONS_ARRAY="${STATIONS_ARRAY}${STATION},"
		done
		
		# Close brackets and remove last ","
		STATIONS_ARRAY="${STATIONS_ARRAY%,}]"
		
		# We also need information on the species to extract
		
		# Open brackets
		SPECIES_ARRAY="["
		
		if [ "${CXR_STATION_SPECIES}" == "${CXR_ALL}" ]
		then
			# Loop through the general list
			for i in $( seq 1 $CXR_NUMBER_OF_OUTPUT_SPECIES )
			do
				species=${CXR_OUTPUT_SPECIES_NAMES[${i}]}
			
				SPECIES_ARRAY="${SPECIES_ARRAY}'${species}',"
			done
		else
		
			# Read it from the string
			for species in ${CXR_STATION_SPECIES}
			do
				SPECIES_ARRAY="${SPECIES_ARRAY}'${species}',"
			done

		fi
		# Close brackets and remove last ","
		SPECIES_ARRAY="${SPECIES_ARRAY%,}]"

		# Change to directory of IDL procedures
		cd $(dirname ${CXR_STATION_PROC_INPUT_FILE}) || return $CXR_RET_ERROR
		
		# We instruct the extractors to print a header for the first day
		if [ $(cxr_common_is_first_day) == true ]
		then
			WRITE_HEADER=1
		else
			WRITE_HEADER=0
		fi
		
		# Then create the file to run IDL
		# Depending on the file to call we need a different interface.
		# later, this could be simplified using a rule...
		
		case $(basename ${CXR_STATION_PROC_INPUT_FILE} .pro) in

			extract_stations)
				# Here, we also need an MM5 file for the pressure (for ppb conversion and potential coordinate conversion), 
				# as well as a flag to indicate if we look at the master domain or not (for coordinate transformation)
				# We set this fag to 0 because currently we only run on the innermost domain.
				echo ".run $(basename ${CXR_STATION_PROC_INPUT_FILE})" >> ${EXEC_TMP_FILE}
				echo "$(basename ${CXR_STATION_PROC_INPUT_FILE} .pro),'${CXR_STATION_INPUT_FILE}','${CXR_STATION_OUTPUT_DIR}',${CXR_DAY},${CXR_MONTH},${CXR_YEAR},${SPECIES_ARRAY},${XDIM},${YDIM},${ZDIM},${STATIONS_ARRAY},'${CXR_METEO_INPUT_FILE}',0" >> ${EXEC_TMP_FILE}
				echo "exit" >> ${EXEC_TMP_FILE}
				;;
				
			extract_arpa_stations)
				# Here, we also need an MM5 file for the pressure (for ppb conversion), 
				# as well as a flag to indicate if we look at the master domain or not (for coordinate transformation)
				# We set this fag to 0 because currently we only run on the innermost domain.
				echo ".run $(basename ${CXR_STATION_PROC_INPUT_FILE})" >> ${EXEC_TMP_FILE}
				echo "$(basename ${CXR_STATION_PROC_INPUT_FILE} .pro),'${CXR_STATION_INPUT_FILE}','${CXR_STATION_OUTPUT_DIR}',${WRITE_HEADER},${CXR_DAY},${CXR_MONTH},${CXR_YEAR},${SPECIES_ARRAY},${XDIM},${YDIM},${ZDIM},${STATIONS_ARRAY},'${CXR_METEO_INPUT_FILE}',0" >> ${EXEC_TMP_FILE}
				echo "exit" >> ${EXEC_TMP_FILE}
				;;
			extract_nabel_stations)

				echo ".run $(basename ${CXR_STATION_PROC_INPUT_FILE})" >> ${EXEC_TMP_FILE}
				echo "$(basename ${CXR_STATION_PROC_INPUT_FILE} .pro),'${CXR_STATION_INPUT_FILE}','${CXR_STATION_OUTPUT_DIR}',${CXR_DAY},${CXR_MONTH},${CXR_YEAR},${CXR_MODEL_HOUR},${SPECIES_ARRAY},${XDIM},${YDIM},${ZDIM},${STATIONS_ARRAY}" >> ${EXEC_TMP_FILE}
				echo "exit" >> ${EXEC_TMP_FILE}
				;;
			*) 
				;;
		
		esac
		
		# Get a copy of the call
		cat ${EXEC_TMP_FILE} | tee -a $CXR_LOG
			
		if [ "$CXR_DRY" == false ]
		then
			
			# Then we run it, while preserving the output
			${CXR_IDL_EXEC} < ${EXEC_TMP_FILE} 2>&1 | tee -a $CXR_LOG
			
		else
			cxr_main_logger "${FUNCNAME}"  "This is a dry-run, no action required"    
		fi
		
		# Check if all went well
		# Postprocessor: we only terminate the module
		if [ $(cxr_common_check_result) == false ]
		then
			cxr_main_logger "${FUNCNAME}" "Postconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			# We notify the caller of the problem
			return $CXR_RET_ERR_POSTCONDITIONS
		fi
		
		# Get back
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



