#!/usr/bin/env bash 
#
# Postprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Converts the Model output to ASCII. Is tolerant with existing files (this is needed
# because the IC/BC preprecessors already need some ASCII files and we do not know if they ran or not)
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# TODO: Properly implement cloud files, Add output checks
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

CXR_META_MODULE_DEPENDS_ON="all_model prepare_output_dir"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Converts the binary output to ASCII for aqmfad"

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
# Function: set_model_output_variables
#	
# Sets the appropriate variables for convert_output
################################################################################	
function set_convert_output_variables()
################################################################################
{
	# This is true if zp was already converted 
	ZP_ONE_OK=false
	
	# First of all, reset checks.
	# We will later continuously add entries to these 2 lists.
	# CAREFUL: If you add files to CXR_CHECK_THESE_OUTPUT_FILES,
	# these are deleted if he user runs the -F option. Do note mik up with input files!
	CXR_CHECK_THESE_INPUT_FILES=
	CXR_CHECK_THESE_OUTPUT_FILES=
	
	########################################################################
	# Set variables
	########################################################################
	
	# The converter scripts
	CXR_CONVERTERS="${CXR_AIRASCII_EXEC} ${CXR_UAMVASCII_EXEC} ${CXR_UAMVASCII_EXEC} ${CXR_UAMVASCII_EXEC} ${CXR_UAMVASCII_EXEC} ${CXR_UAMVASCII_EXEC}"
	
	# The options to the converter scripts
	CXR_CONVERTER_OPTIONS="AVERAGE HEIGHT WIND TEMPERATUR H2O VDIFFUSION"
	
	# Input Arrays to convert
	CXR_INPUT_ARRAYS="CXR_AVG_INPUT_ARR_FILES CXR_ZP_GRID_INPUT_ARR_FILES CXR_WIND_GRID_INPUT_ARR_FILES CXR_TEMP_GRID_INPUT_ARR_FILES CXR_VAPOR_INPUT_ARR_FILES CXR_KV_GRID_INPUT_ARR_FILES"
	
	# Output Arrays to convert to
	# Here, the name of each corresponding output array MUST be ..._ASC_OUTPUT_ARR_FILES
	# because the name of the output array is generated from the name of the input array
	CXR_OUTPUT_ARRAYS="CXR_AVG_ASC_OUTPUT_ARR_FILES CXR_ZP_GRID_ASC_OUTPUT_ARR_FILES CXR_WIND_GRID_ASC_OUTPUT_ARR_FILES CXR_TEMP_GRID_ASC_OUTPUT_ARR_FILES CXR_VAPOR_ASC_OUTPUT_ARR_FILES CXR_KV_GRID_ASC_OUTPUT_ARR_FILES"
	
	########################################################################
	# per day-per grid settings
	# we loop
	#	expand the file name rule
	#	then export the name and the value
	########################################################################
	for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
	do
		# Here, we ASSUME that we work on a directory prepared by <prepare_output_dir>
		# in particular, we assume that the linknames are proper, that is, they have
		# no temporary names
		# Note the last argument (false) in every call to <cxr_common_evaluate_rule>
		
		
		# The ASC Filenames are the output
		# Output files must not be decompressed!
		CXR_AVG_ASC_OUTPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_AVG_ASC_FILE_RULE" false CXR_AVG_ASC_FILE_RULE false)
		
		# TERRAIN must not be converted
		
		# Pressure - convert_input should have done the first grid
		CXR_ZP_GRID_ASC_OUTPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_PRESSURE_ASC_FILE_RULE" false CXR_PRESSURE_ASC_FILE_RULE false)
		
		if [ $i -eq 1 ]
		then
			if [ -s ${CXR_ZP_GRID_ASC_OUTPUT_ARR_FILES[${i}]} ]
			then
				ZP_ONE_OK=true
			fi
		fi
		
		# Wind
		CXR_WIND_GRID_ASC_OUTPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_WIND_ASC_FILE_RULE" false CXR_WIND_ASC_FILE_RULE false)
		# Temperature
		CXR_TEMP_GRID_ASC_OUTPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_TEMPERATURE_ASC_FILE_RULE" false CXR_TEMPERATURE_ASC_FILE_RULE false)
		# Vapor
		CXR_VAPOR_ASC_OUTPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_VAPOR_ASC_FILE_RULE" false CXR_VAPOR_ASC_FILE_RULE false)
		# No Cloud
		# Vertical K
		CXR_KV_GRID_ASC_OUTPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_K_ASC_FILE_RULE" false CXR_K_ASC_FILE_RULE false)
		# NO Emissions
	
	
		# These files are INPUT Files - we need to modify it, becaus we read a link in the 
		# ascii dir instead of the real file
		CXR_AVG_INPUT_ARR_FILES[${i}]=$CXR_AQMFAD_OUTPUT_DIR/$(basename $(cxr_common_evaluate_rule "$CXR_AVG_FILE_RULE" false CXR_AVG_FILE_RULE false))

		# TERRAIN must not be converted, it is already there.
		
		# Pressure. 
		CXR_ZP_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_PRESSURE_FILE_RULE" false CXR_PRESSURE_FILE_RULE false)
		# Wind
		CXR_WIND_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_WIND_FILE_RULE" false CXR_WIND_FILE_RULE false)
		# Temperature
		CXR_TEMP_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_TEMPERATURE_FILE_RULE" false CXR_TEMPERATURE_FILE_RULE false)
		# Vapor
		CXR_VAPOR_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_VAPOR_FILE_RULE" false CXR_VAPOR_FILE_RULE false)
		# No Cloud
		# Vertical K
		CXR_KV_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_K_FILE_RULE" false CXR_K_FILE_RULE false)
		# NO Emissions
		
		# Checks for the input
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_AVG_INPUT_ARR_FILES[${i}]} ${CXR_ZP_GRID_INPUT_ARR_FILES[${i}]} ${CXR_WIND_GRID_INPUT_ARR_FILES[${i}]} ${CXR_TEMP_GRID_INPUT_ARR_FILES[${i}]} ${CXR_VAPOR_INPUT_ARR_FILES[${i}]} ${CXR_KV_GRID_INPUT_ARR_FILES[${i}]} "
		
	done
}

################################################################################
# Function: convert_output
#	
# Converts the files of the current day to .asc
################################################################################	
function convert_output() 
################################################################################
{
	#Was this stage already completed?
	if [ $(cxr_common_store_state ${CXR_STATE_START}) == true ]
	then
		#  --- Setup the Environment of the current day
		set_convert_output_variables 
		
		#  --- Check Settings
		# Postprocessor: we only terminate the module
		if [ $(cxr_common_check_preconditions) == false ]
		then
			cxr_main_logger "${FUNCNAME}" "Preconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		
		# The Fortran programs can only handle parameters shorter than 255 Chars
		# Therefore we change to the directory and work with the basenames
		#
		# We convert the whole input of aqmfad
		#
		########################################################################
		# Requires the bin2asc and the airascii patches 	
		########################################################################
		
		cd $CXR_AQMFAD_OUTPUT_DIR || return $CXR_RET_ERROR

		cxr_main_logger "${FUNCNAME}" "Working in $CXR_AQMFAD_OUTPUT_DIR"

		# We loop through all the grids
		# Therefore we let seq create the numbers from 1 to ${CXR_NUMBER_OF_GRIDS}
		
		# Because there is yet no single converter for CAMx autput and 
		# MM5 output, this code looks hairy. One day it might look so:
		for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
		do
			# We have 4 Arrays, containing
			#	CXR_INPUT_ARRAYS	: the names of the input arrays
			#	CXR_OUTPUT_ARRAYS	: the names of the output arrays
			#	CXR_CONVERTERS		: the converter scripts
			#	CXR_CONVERTER_OPTIONS	: the optins to the converter scripts
			#
			#	They all have equal length and corresponding elements are at the same index
			#
			#	The uamvascii script also needs x-dim, y-dim and z-dim, as well as a logfile as input. 
			# 	We provide these no matter what program we are running (the logfile is 0=Stdout)
			
			# Determine dimensions
			XDIM=$(cxr_common_get_x_dim ${i})
			YDIM=$(cxr_common_get_y_dim ${i})
			ZDIM=$(cxr_common_get_z_dim ${i})
			
			cxr_main_logger -v "${FUNCNAME}" "Grid $i\nXDIM: $XDIM\nYDIM: $YDIM\nZDIM: $ZDIM"

			# Use the deserialisation code again
			eval "CXR_INPUT_ARRAYS_ARR=( ${CXR_INPUT_ARRAYS[*]} )"
			eval "CXR_OUTPUT_ARRAYS_ARR=( ${CXR_OUTPUT_ARRAYS[*]} )"
			eval "CXR_CONVERTERS_ARR=( ${CXR_CONVERTERS[*]} )"
			eval "CXR_CONVERTER_OPTIONS_ARR=( ${CXR_CONVERTER_OPTIONS[*]} )"
			
			# Maximum of iterator
			MAX=$(( ${#CXR_INPUT_ARRAYS_ARR[*]} - 1))
			
			### Go trough all input arrays
			for j in $(seq 0 ${MAX})
			do
				INPUT_FILE=$(basename $(eval "echo \${${CXR_INPUT_ARRAYS_ARR[${j}]}[${i}]}"))
				OUTPUT_FILE=$(basename $(eval "echo \${${CXR_OUTPUT_ARRAYS_ARR[${j}]}[${i}]}"))
				
				CONVERTER=${CXR_CONVERTERS_ARR[${j}]}
				OPTIONS=${CXR_CONVERTER_OPTIONS_ARR[${j}]}
				
				cxr_main_logger "${FUNCNAME}" "Converting ${INPUT_FILE} to ${OUTPUT_FILE} using ${CONVERTER}..."
				
				# Any existing file will be skipped (see comment in header)
				if [ -s "$OUTPUT_FILE" ]
				then
				
					if [ "${CXR_FORCE}" == true ] 
					then
						# Delete it
						cxr_main_logger "${FUNCNAME}"  "File ${OUTPUT_FILE} exists - since you run with the -f option, if will be deleted now"
						rm -f "$OUTPUT_FILE"
					else
						# Skip it
						cxr_main_logger "${FUNCNAME}"  "File ${OUTPUT_FILE} exists - file will skipped."
					continue
					fi
				fi
				
				cxr_main_logger "${FUNCNAME}"  "Converting ${INPUT_FILE} to ${OUTPUT_FILE} ..."
				
				cxr_main_logger -v "${FUNCNAME}" "${CONVERTER} ${INPUT_FILE} ${OUTPUT_FILE} ${OPTIONS} ${XDIM} ${YDIM} ${ZDIM} 0 "

				if [ "$CXR_DRY" == false ]
				then
					#Call the converter, collect sterr and stout
					${CONVERTER} ${INPUT_FILE} ${OUTPUT_FILE} ${OPTIONS} ${XDIM} ${YDIM} ${ZDIM} 0 2>&1 | tee -a $CXR_LOG
				else
						cxr_main_logger "${FUNCNAME}"  "Dryrun, no conversion performed"
				fi
			done
		done 

		# Check if all went well
		# Postprocessor: we only terminate the module
		if [ $(cxr_common_check_result) == false ]
		then
			cxr_main_logger "${FUNCNAME}" "Postconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			# We notify the caller of the problem
			return $CXR_RET_ERR_POSTCONDITIONS
		fi
		
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



