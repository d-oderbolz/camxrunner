#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the test harness of CAMxRunner.
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

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=0

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains functions to simplify user interaction"

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

	Written by $CXR_META_MODULE_AUTHOR
	License: $CXR_META_MODULE_LICENSE
	
	Find more info here:
	$CXR_META_MODULE_DOC_URL
EOF
exit 1
}


################################################################################
# Function: cxr_common_load_test_data
#	
# Loads testdata for a testcase from a compressed file if the file is available.
# Reads CXR_TEST_DATA_INPUT_FILE and CXR_TEST_DATA_OUTPUT_DIR
#
################################################################################
function cxr_common_load_test_data()
################################################################################
{
	local filetype
	
	# Need to load test data
	cxr_main_logger -B "${FUNCNAME}" " Loading test data (CXR_LOAD_TEST_DATA is true)... "

	if [[   "${CXR_TEST_DATA_OUTPUT_DIR:-}" && -d "${CXR_TEST_DATA_OUTPUT_DIR:-}" && -f "${CXR_TEST_DATA_INPUT_FILE:-}"    ]]
	then
	
		cd "${CXR_TEST_DATA_OUTPUT_DIR}" || cxr_main_die_gracefully "Could not change to ${CXR_TEST_DATA_OUTPUT_DIR}"
		
		# Query filetype
		filetype=$(cxr_common_get_file_type "${CXR_TEST_DATA_INPUT_FILE}")
		
		# We support gzip and bzip compression
		case $filetype in
		
			bzip2)
				tar --use-compress-program=$CXR_BUNZIP2_EXEC -xf "${CXR_TEST_DATA_INPUT_FILE}" .
				;;
				
			gzip)
				tar --use-compress-program=$CXR_GUNZIP_EXEC -xf "${CXR_TEST_DATA_INPUT_FILE}" .
				;;
		
			*)
				cxr_main_logger -e "${FUNCNAME}" "File type $FILETYPE not supported"
				;;
		esac
		
		cd $CXR_RUN_DIR || cxr_main_die_gracefully "Could not change back to ${CXR_RUN_DIR}"
	
	else
		cxr_main_logger -e "${FUNCNAME}" "Cannot load test data, either CXR_TEST_DATA_OUTPUT_DIR or CXR_TEST_DATA_INPUT_FILE not set correctly or permission problem!"
	fi
}


################################################################################
# Function: cxr_common_test_all_modules
#	
# Test all testable modules, generates a .tap file that contains the output of the tests.
#
# Parameters:
# [$1] - The Model Version to use
# [$2] - The Modelname to use
################################################################################
function cxr_common_test_all_modules()
################################################################################
{
	local iput_model="${1:-}"
	local input_version="${2:-}"
	local model_id
	local supported
	local array
	local default_version
	local total_tests
	local message
	
	
	# This function runs all tests that are availabe (asking the user before each new test if wanted).
	# For this, all modules are enumerated and then tested if marked as testable.
	# To do this, we need to know the model name and version to test for (otherwise we might repeat many tests)

	message="Do you want to run the test suite of CAMxRunner?"
	
	while [ "$(cxr_common_get_consent "$message" )" == true ]
	do
	
		# Fix the message
		message="Do you want to further run the test suite of CAMxRunner (for other models/versions)?"
		
		################################################################################
		# Determine model and version
		################################################################################	
		
		if [[ ! "${iput_model}"  ]]
		then
			# Model was not passed
			model=$(cxr_common_get_menu_choice "Which model should the tests be run for?\nIf your desired model is not in this list, adjust CXR_SUPPORTED_MODELS \n(Currently $CXR_SUPPORTED_MODELS)" "$CXR_SUPPORTED_MODELS" "CAMx")
		else
			model=$iput_model
		fi
		
		model_id=$(cxr_common_get_model_id "$model") || cxr_main_die_gracefully "Model $model is not known."
		
		
		if [[ ! "${input_version}"  ]]
		then
			# version was not passed
		
			# Extract the list of supported versions
			supported="${CXR_SUPPORTED_MODEL_VERSIONS[${model_id}]}"
			
			# Set the default to the first entry
			# Save old IFS
			oIFS="$IFS"
		
			IFS="$CXR_SPACE"
			
			# Suck line into array
			array=($supported)
			
			# Reset IFS
			IFS="$oIFS"
			
			default_version=${array[0]}
		
			#Generate a menu automatically
			version=$(cxr_common_get_menu_choice "Which version of $model should be used?\nIf your desired version is not in this list, adjust CXR_SUPPORTED_MODEL_VERSIONS \n(Currently $supported)" "$supported" "$default_version")
		else
			version=$input_version
		fi
		
		cxr_common_is_version_supported $version $model
		
		cxr_main_logger "${FUNCNAME}" "Testing system using modules for $model $version..."
		
		# This is a marker that tells the modules they do not need init anymore
		CXR_TESTING_FROM_HARNESS=true
		
		# Pre-load testing interface
		CXR_RUN=base
	
		source inc/init_test.inc
	
		########################################
		#  Count all tests
		########################################
		
		total_tests=0
		
		# save stdin and redirect it from an in-line file
		exec 9<&0 <<-EOF
		# Here we list all directories and a comment on each
		# The order might need to change later
		$CXR_COMMON_INPUT_DIR                 "Common Modules"
		$CXR_COMMON_MODEL_INPUT_DIR           "Model specific common Modules"
		$CXR_COMMON_VERSION_INPUT_DIR         "Version specific  common Modules"
		$CXR_PREPROCESSOR_DAILY_INPUT_DIR     "Daily preprocessor Modules"
		$CXR_PREPROCESSOR_ONCE_INPUT_DIR      "One-Time preprocessor Modules"
		$CXR_POSTPROCESSOR_DAILY_INPUT_DIR    "Daily postprocessor Modules"
		$CXR_POSTPROCESSOR_ONCE_INPUT_DIR     "One-Time postprocessor Modules"
		$CXR_MODEL_INPUT_DIR                  "Model Modules"
		$CXR_INSTALLER_INPUT_DIR              "Common installer Modules"
		$CXR_INSTALLER_MODEL_INPUT_DIR        "Model specific installer Modules"
		$CXR_INSTALLER_VERSION_INPUT_DIR      "Version specific installer Modules"
		EOF
		while read CURRENT_DIR COMMENT
		do
			# ignore comment and blank lines
			echo "${CURRENT_DIR}" |egrep -v "^(#|$)" >/dev/null || continue
	
			cxr_main_logger "$FUNCNAME" "Counting tests in ${CURRENT_DIR} (${COMMENT})..."
			
			for FUNCTION_FILE in $(ls ${CURRENT_DIR}/??_*.sh 2>/dev/null)
			do
				# Create the "bare" name (analogous to extract_module_name, but hen&egg)
				BASE_FUNCTION_NAME=$(basename $FUNCTION_FILE)
				RAW_FUNCTION_NAME=${BASE_FUNCTION_NAME%.sh}
				
				# Remove XX_
				INDEX_US=$(expr index "$RAW_FUNCTION_NAME" _)
				RAW_FUNCTION_NAME=${RAW_FUNCTION_NAME:$INDEX_US}
				
				# Load it
				CXR_META_MODULE_NAME=$RAW_FUNCTION_NAME
					
				source $FUNCTION_FILE
				
				cxr_main_logger -v "$FUNCNAME" "Found $CXR_META_MODULE_NUM_TESTS in $CXR_META_MODULE_NAME"
				
				total_tests=$(( $total_tests + $CXR_META_MODULE_NUM_TESTS ))
				
			done
		done
		# Restore stdin and close fd 9
		exec 0<&9 9<&-
		
		########################################
		#  Plan these tests
		########################################
		cxr_main_logger -v "$FUNCNAME" "Planning to run $total_tests tests..."
		
		# Plan them
		plan_tests $total_tests
	
		########################################
		#  Do these tests
		########################################
		
		# This is to remember the last loaded config
		LAST_LOADED_CONFIG=base
		
		# save stdin and redirect it from an in-line file
		exec 9<&0 <<-EOF
		# Here we list all directories and a comment on each
		# The order might need to change later
		$CXR_COMMON_INPUT_DIR                 "Common Modules"
		$CXR_COMMON_MODEL_INPUT_DIR           "Model specific common Modules"
		$CXR_COMMON_VERSION_INPUT_DIR         "Version specific  common Modules"
		$CXR_PREPROCESSOR_DAILY_INPUT_DIR     "Daily preprocessor Modules"
		$CXR_PREPROCESSOR_ONCE_INPUT_DIR      "One-Time preprocessor Modules"
		$CXR_POSTPROCESSOR_DAILY_INPUT_DIR    "Daily postprocessor Modules"
		$CXR_POSTPROCESSOR_ONCE_INPUT_DIR     "One-Time postprocessor Modules"
		$CXR_MODEL_INPUT_DIR                  "Model Modules"
		$CXR_INSTALLER_INPUT_DIR              "Common installer Modules"
		$CXR_INSTALLER_MODEL_INPUT_DIR        "Model specific installer Modules"
		$CXR_INSTALLER_VERSION_INPUT_DIR      "Version specific installer Modules"
		EOF
		while read CURRENT_DIR COMMENT
		do
			# ignore comment and blank lines
			echo "${CURRENT_DIR}" |egrep -v "^(#|$)" >/dev/null || continue
	
			cxr_main_logger "$FUNCNAME" "Executing ${COMMENT} tests..."
			
			for FUNCTION_FILE in $(ls ${CURRENT_DIR}/??_*.sh 2>/dev/null)
			do
				# Create the "bare" name (analogous to extract_module_name, but hen&egg)
				BASE_FUNCTION_NAME=$(basename $FUNCTION_FILE)
				RAW_FUNCTION_NAME=${BASE_FUNCTION_NAME%.sh}
				
				# Remove XX_
				INDEX_US=$(expr index "$RAW_FUNCTION_NAME" _)
				RAW_FUNCTION_NAME=${RAW_FUNCTION_NAME:$INDEX_US}
				
				# Load it
				CXR_META_MODULE_NAME=$RAW_FUNCTION_NAME
					
				source $FUNCTION_FILE
				
				if [[ ${CXR_META_MODULE_NUM_TESTS:-0} -gt 0  ]]
				then
					
					# We must state the run name properly
					CXR_RUN=${CXR_META_MODULE_TEST_RUN:-base}
					
					# If we did not just load this config, do it now
					if [[ "$CXR_RUN" != "$LAST_LOADED_CONFIG"  ]]
					then
						# We prepare the tests
						source inc/init_test.inc
						LAST_LOADED_CONFIG=$CXR_RUN
					fi
					
					cxr_main_logger -b "$FUNCNAME" "Testing $CXR_META_MODULE_NAME ($CXR_META_MODULE_NUM_TESTS tests)..."
					
					test_module
					
				else
					cxr_main_logger -v "$FUNCNAME" "There are no tests in $CXR_META_MODULE_NAME yet."
				fi
				
			done
		done
		# Restore stdin and close fd 9
		exec 0<&9 9<&-
		
		########################################
		#  Get stats
		########################################
		summarize_tests

	done
}

################################################################################
# Are we running stand-alone? - Can only show help
################################################################################

# If the CXR_META_MODULE_NAME  is not set,
# somebody started this script alone
if [[ -z "${CXR_META_MODULE_NAME:-}" ]]
then
	usage
fi

################################################################################
# Code beyond this point is not executed in stand-alone operation
################################################################################


