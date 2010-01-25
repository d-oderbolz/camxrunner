#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Check Functions
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
CXR_META_MODULE_NUM_TESTS=1

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains most of the check functions for the CAMxRunner"

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
# Function: cxr_common_check_bash_version
#	
# Checks if the bash version is ok for what we do
################################################################################
function cxr_common_check_bash_version() 
################################################################################
{
	if [ ${BASH_VERSINFO[0]} -lt 3 ]
	then
		cxr_main_die_gracefully "We need at least Bash Version 3.x - please upgrade."
	fi
}

################################################################################
# Function: cxr_common_predict_file_size_megabytes
# 
# Gives a rough estimate  on the number of megabytes needed for a given output file.
# This function is NOT used by <cxr_common_predict_model_output_megabytes>, the purpose of
# this function here is to check if a file has about the right size.
#
# Parameters:
# $1 - the filename in question
# $2 - the type of file (AIRQUALITY, BOUNDARY, EMISSIONS...)
# $3 - the storage type (ASCII, BINARY, FDA, HDF)
#
################################################################################
function cxr_common_predict_file_size_megabytes ()
################################################################################
{
	# TODO Should be implemented
	echo 400
}

################################################################################
# Function: cxr_common_predict_model_output_megabytes
# 
# Gives a rough (hopefully over-)estimate  on the number of megabytes needed for the output.
#
# Formula: sum (dx_i * dy_i * dz_i) * ( t / t_res) * nspec * C(Options) * f_margin
# Takes into account if 3D output is requested or not.
# The factor C(Options) is currently a constant, it might take other options
# like HDF, source apportionment etc. into account later.
#
################################################################################
function cxr_common_predict_model_output_megabytes ()
################################################################################
{
	# Determine size of output field
	if [ ${CXR_AVERAGE_OUTPUT_3D} == true ]; then
		CELLS=$(cxr_common_get_num_cells_3d)
	else
		CELLS=$(cxr_common_get_num_cells_2d)
	fi
	
	# Our constant is designed for 10^5 cells
	CELLS=$(cxr_common_fp_calculate "$CELLS / 100000")
	
	TIME_STEPS=$(cxr_common_fp_calculate "(60 * 24 * ${CXR_NUMBER_OF_SIM_DAYS}) / ${CXR_OUTPUT_FREQUENCY}" "0" )
	
	SIZE=$(cxr_common_fp_calculate "${CELLS} * ${TIME_STEPS} * ${CXR_NUMBER_OF_OUTPUT_SPECIES} * ${CXR_C_SPACE} * ${CXR_F_MARGIN}" "0" false)
	
	echo "$SIZE"
}

################################################################################
# Function: cxr_common_check_mb_needed
#	
# Checks if space in target directory is sufficient.
# Aborts if not sufficient.
# Internally uses <cxr_common_free_megabytes>
#
# Parameters:
# $1 - Directory to check
# $2 - Space needed (in megabytes)
################################################################################
function cxr_common_check_mb_needed() 
################################################################################
{
	if [ $# -ne 2 ]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs 2 parameters: a path and a number (megabytes needed)"
	fi
	
	DIR="$1"
	MB="$2"
	
	cxr_main_logger -v "${FUNCNAME}" "Checking if space in $DIR is sufficient..."
		
	# Nonexistent Directory?
	if [ ! -d $DIR ]
	then
		# Create it!
		cxr_main_logger -w "${FUNCNAME}" "Directory $DIR is missing, I create it now."
		mkdir -p $DIR
	fi
	
	if [ "$(cxr_common_free_megabytes "$DIR")" -ge "$MB" ]
	then
		cxr_main_logger -i "${FUNCNAME}" "Space in $DIR is sufficient."
	else
		cxr_main_die_gracefully "Space in $DIR is not sufficient, need at least $MB Megabytes!"
	fi
	
}


################################################################################
# Function: cxr_common_check_datataype
#	
# Checks if a value has a certain datatype - needed for installation
# to make sure the user entered sensible values
#
# Datatypes:
#	S (String - allowed to be empty)
# I (Integer)
#	F (Float)
#	B (Boolean - either true or false)
# D (Directory - a high level string that is checked)
#
# Parameters:	
# $1 - Value to check
# $2 - Expected datatype (1 char)
################################################################################
function cxr_common_check_datataype()
################################################################################
{
	if [ $# -ne 2 ]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - needs 2 strings as input"
	fi
	
	VALUE=$1
	DATATYPE=$2
	
	cxr_main_logger -d "${FUNCNAME}"  "$FUNCNAME\nVALUE: *$VALUE*\nDATATYPE: *$DATATYPE*"

	case $DATATYPE in
	S) # String - everything is ok, even an empty string
		echo true
		;;
	I) # Integer
		echo $(cxr_main_is_numeric "$VALUE")
		;;
	F) # Floating point number

		#Check for FP
		# If this returns 0, we look at a FP.
		# We look for any ocurrences of numbers,
		# mandatorily followed by a dot
		# optionally followed by any ocurrences of numbers
		
		# We neet the retval,
		# turn off strict checks
		set +e
		
		echo "$VALUE" | grep "[0-9]*\.[0-9]*" >/dev/null
		
		if [ $? -eq 0 ]
		then
			echo true
		else
			echo false
		fi
		
		#Turn strict checks back on
		set -e
		
		;;
	B) # Boolean - either true or false
		if [ "$VALUE" == true -o "$VALUE" == false ]
		then
			echo true
		else
			echo false
		fi
		;;
	D) # Directory - in principle we accept anything
		if [ ! -d "$VALUE" ]
		then
			cxr_main_logger "${FUNCNAME}" -s "The directory $VALUE was net found"
			echo true
		fi
			
		;;
	*) cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - illegal Datatype $DATATPE" ;;
	esac
}

################################################################################
# Function: cxr_common_check_runner_executables
#	
# Loop through all *.sh scripts in the ${CXR_RUN_DIR} and check if they are executable
#
################################################################################
function cxr_common_check_runner_executables ()
################################################################################
{
	# We use a bash 3.x structure, the so-called "here-variable"
	while read FILE
	do
		if [ ! -x $FILE ]
		then
			cxr_main_logger -w "${FUNCNAME}" "File $FILE is not executable,I try to correct this"
			# File is not executable, try to correct
			chmod +x $FILE || cxr_main_die_gracefully "Could not change permissions on file $FILE - exiting"
			
		fi
	done<<<"$(find ${CXR_RUN_DIR} -noleaf -type f -name \*.sh )"
}

################################################################################
# Function: cxr_common_check_environment_executables
#	
# Loop through all CXR_*.*_EXEC enviranment variables and check if they are 
# * Set
# * Present
# * Executable
#
# This function is only visual, does not terminate
################################################################################
function cxr_common_check_environment_executables ()
################################################################################
{
	for EXEC in $(set | grep -e ^CXR_*.*_EXEC= | cut -d= -f1)
	do
	
		cxr_main_logger -v "${FUNCNAME}"  "Variable $EXEC has value: ${!EXEC}\n"
			
		# is it set?
		if [ "${!EXEC}" ]
		then
			# Does it exist?
			if [ -f "${!EXEC}" ]
			then
				# is it executable 
				if [ ! -x ${!EXEC} ]
				then
					cxr_main_logger -w "${FUNCNAME}"  "Executable ${!EXEC}, Parameter $EXEC not executable - I try to correct this ..."     
					
					chmod +x ${!EXEC} || cxr_main_logger "${FUNCNAME}" "Could not change permissions on file $FILE"

					# Do not increase error count here - maybe we do not need this one
				fi
			else
			  # Not present!
			  cxr_main_logger -w "${FUNCNAME}"  "Executable ${!EXEC}, Parameter $EXEC does not exist (might not be a problem, though, CAMx e. g. is not needed for postprocessing and vice-versa)!"
			fi
			
		else
			cxr_main_logger -w "${FUNCNAME}"  "Variable $EXEC is not set (might not be a problem, though)"
		fi
	done
}

################################################################################
# Function: cxr_common_check_preconditions
#	
# Checks if all input files listed in CXR_CHECK_THESE_INPUT_FILES are available.
# Also sees that output files listed in CXR_CHECK_THESE_OUTPUT_FILES are not present 
# - if -F is given, existing output files are deleted here. 
# If we detect empty output files, they are always removed.
#
# Files are checked for length if CXR_CHECK_MAX_PATH=true
#
# This function does not terminate the runner if errors are found,
# but it returns false.
#
# You can restrict the checks if wanted, currently, the one-time checks are always executed.
#
# Parameters:
# -i - check input files (Default: Check all)
# -o - check output files (Default: Check all)
################################################################################
function cxr_common_check_preconditions() 
################################################################################
{
	# Does the user want to limit the checks?
	local LIMITED=false
	
	# First, all is switched off
	local DO_INPUT=false
	local DO_OUTPUT=false

	while getopts ":io" opt
	do
		case $opt in
		i) LIMITED=true ; DO_INPUT=true  ;;
		o) LIMITED=true ; DO_OUTPUT=true ;;
		esac
	done
	
	# Fix switches if user did not restrict
	if [ "${LIMITED}" == false ]
	then
		DO_INPUT=true
		DO_OUTPUT=true
	fi
	
	# This is not strictly needed, but it allows to read 
	# non-named command line options
	shift $(($OPTIND - 1))
	
	# Make getopts ready again
	unset OPTSTRING
	unset OPTIND

	# In here, we disable strict checking of variables
	# because we want to fail in a controlled manner if anything is wrong
	set +u

	#Mark if there were errors
	ERRORS_FOUND=false
	
	##########################################
	#### this is only checked once
	##########################################
	if [ "$CXR_CHECKED_ONCE" == false ]
	then
	
		cxr_main_logger -v "${FUNCNAME}"  "Performing one-time checks..."
		
		# Increase global indent level
		cxr_main_increase_log_indent

		cxr_main_logger -v "${FUNCNAME}"  "Checking Output directories ..."
		
		# Increase global indent level
		cxr_main_increase_log_indent

		# Create Output dirs if needed
		# cut extracts the variable name
		for OUTPUT_DIR in $(set | grep -e ^CXR_*.*_OUTPUT_DIR= | cut -d= -f1)
		do
			# is it set?
			if [ "${!OUTPUT_DIR}" ]
			then
				cxr_main_logger -v "${FUNCNAME}"  "Variable $OUTPUT_DIR has value: ${!OUTPUT_DIR}\n"
				
				# Test length
				if [ "${CXR_CHECK_MAX_PATH}" == true ]
				then
					if [ $(cxr_common_len "${!OUTPUT_DIR}") -gt "${CXR_MAX_PATH}" ]
					then
						cxr_main_logger -e "${FUNCNAME}" "Path to $OUTPUT_DIR longer than ${CXR_MAX_PATH}. Either disable this check (CXR_CHECK_MAX_PATH=false) or increase CXR_MAX_PATH.\nCheck if all binaries are ready for paths of this size!"
					fi
				fi
				
				# Does it exist
				if [ ! -d ${!OUTPUT_DIR} ]
				then
					cxr_main_logger -w "${FUNCNAME}" "Directory ${!OUTPUT_DIR} Parameter $OUTPUT_DIR does not exist - will create it"
					
					mkdir -p ${!OUTPUT_DIR} || cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - could not create output dir ${!OUTPUT_DIR}"
					
				# is it writeable?
				elif [ ! -w ${!OUTPUT_DIR} ]
				then
					cxr_main_logger -e "${FUNCNAME}:${LINENO} - Output Directory ${!OUTPUT_DIR}, \nParameter $OUTPUT_DIR not writeable!"
				fi
			else
				cxr_main_logger -w "${FUNCNAME}"  "Variable $OUTPUT_DIR is not set (might not be a problem, though)"
			fi
		done
		
		# Decrease global indent level
		cxr_main_decrease_log_indent
		
		cxr_main_logger -v "${FUNCNAME}"  "Checking Executables ..."
		
		# Increase global indent level
		cxr_main_increase_log_indent

		# EXECUTABLES
		cxr_common_check_environment_executables
		
		# Decrease global indent level
		cxr_main_decrease_log_indent
		
		cxr_main_logger -v "${FUNCNAME}"  "Checking Directories ..."
		
		# Increase global indent level
		cxr_main_increase_log_indent

		
		# DIRECTORIES
		for DIR in $(set | grep -e ^CXR_*.*_DIR= | cut -d= -f1)
		do
			cxr_main_logger -v "${FUNCNAME}"  "Variable $DIR has value: ${!DIR}\n"

			# is it set?
			if [ "${!DIR}" ]
			then
			
				# does it exist?
				if [ -d "${!DIR}" ]
				then
					# is it executable (dir: means accessible)?
					if [ ! -x ${!DIR} ]
					then
						cxr_main_logger -e "${FUNCNAME}:${LINENO} - Directory ${!DIR}, \nParameter $DIR not accessible!"
					fi
				else
					# Does not exist, create it.
					if [ $(cxr_common_is_absolute_path ${!DIR}) == true ]
					then
						cxr_main_logger -w "${FUNCNAME}"  "Directory ${!DIR}, \nParameter $DIR does not exist - creating it"
						mkdir -p ${!DIR}
					else
						cxr_main_logger -v "${FUNCNAME}" "${!DIR} does not exist, but is a relative path - no action taken"
					fi
				fi
				
			else
				cxr_main_logger -w "${FUNCNAME}"  "Variable $DIR is not set (might not be a problem, though)"
			fi
		done 
		
		# Decrease global indent level
		cxr_main_decrease_log_indent
		
		# Check done
		CXR_CHECKED_ONCE=true
		
		##########################################
		#### END this is only checked once
		##########################################
		
	fi
	
	##########################################
	### From here, checks are run for each day
	##########################################
	
	cxr_main_logger -v "${FUNCNAME}"  "Performing per-day checks..."
	
	
	########################################
	# Input Check
	########################################
	if [ "${DO_INPUT}" == true ]
	then
		# INPUT FILES - are they there?
		cxr_main_logger -v "${FUNCNAME}"  "Checking Input Files ..."
	
		# Increase global indent level
		cxr_main_increase_log_indent
	
		for INPUT_FILE in ${CXR_CHECK_THESE_INPUT_FILES}
		do
			
			# Test length
			if [ "${CXR_CHECK_MAX_PATH}" == true ]
			then
				if [ $(cxr_common_len "${INPUT_FILE}") -gt "${CXR_MAX_PATH}" ]
				then
					cxr_main_logger -e "${FUNCNAME}" "Path to $INPUT_FILE longer than ${CXR_MAX_PATH}. Either disable this check (CXR_CHECK_MAX_PATH=false) or increase CXR_MAX_PATH.\nCheck if all binaries are ready for paths of this size!"
					ERRORS_FOUND=true
				fi
			fi
			
			# is it readable?
			if [ ! -r "${INPUT_FILE}" ]
			then
				# Not readable!
				cxr_main_logger -e "${FUNCNAME}:${LINENO} - File ${INPUT_FILE} not readable!"
				ERRORS_FOUND=true
			else
				# Readable
			
				# is it larger than 0 bytes?
				if [ ! -s "${INPUT_FILE}" ]
				then
					# Empty File!
					cxr_main_logger -e "${FUNCNAME}:${LINENO} - File ${INPUT_FILE} is empty!"
					ERRORS_FOUND=true
				fi
			fi
		done
	else
		cxr_main_logger -v "${FUNCNAME}" "Will not check input"
	fi
	
	########################################
	# Output Check
	########################################
	if [ "${DO_OUTPUT}" == true ]
	then
		# OUTPUT FILES - are they absent?
		cxr_main_logger -v "${FUNCNAME}"  "Checking if Output Files are absent..."
	
		# Increase global indent level
		cxr_main_increase_log_indent
	
		for OUTPUT_FILE in ${CXR_CHECK_THESE_OUTPUT_FILES}
		do
			# Test length
			if [ "${CXR_CHECK_MAX_PATH}" == true ]
			then
				if [ $(cxr_common_len "${OUTPUT_FILE}") -gt "${CXR_MAX_PATH}" ]
				then
					cxr_main_logger -e "${FUNCNAME}" "Path to $OUTPUT_FILE longer than ${CXR_MAX_PATH}. Either disable this check (CXR_CHECK_MAX_PATH=false) or increase CXR_MAX_PATH.\nCheck if all binaries are ready for paths of this size!"
					ERRORS_FOUND=true
				fi
			fi
			
			# is it present?
			if [ -f "${OUTPUT_FILE}" ]
			then
				#############################
				# File Present!
				#############################
				
				# is it empty or do we force?
				if [ ! -s "${OUTPUT_FILE}" ]
				then
					# Empty
					cxr_main_logger -w "${FUNCNAME}" "File ${OUTPUT_FILE} already exists, but is empty. I will delete it now..."
					rm -f "${OUTPUT_FILE}"
				elif [ "$CXR_FORCE" == true ]
				then
					# Force overwrite
					cxr_main_logger -w "${FUNCNAME}" "File ${OUTPUT_FILE} already exists. You chose the -F option, so we delete it now..."
					
					if [ "$CXR_DRY" == false ]
					then
						rm -f "${OUTPUT_FILE}"
					else
						cxr_main_logger -w "${FUNCNAME}" "Dryrun, file ${OUTPUT_FILE} not removed. A real run removes the file if -F is given!"
					fi
				else
					#############################
					# No overwrite, no empty file
					#############################
					
					# Do we skip?
					if [ "${CXR_SKIP_EXISTING}" == true ]
					then
						# Ok, skipping
						cxr_main_logger -w "${FUNCNAME}" "File ${OUTPUT_FILE} already exists. You chose to skip over this file..."
					else
						cxr_main_logger -e  "File ${OUTPUT_FILE} already exists! You can force the deletion of existing files by calling \n ${CXR_CALL} -F"
						ERRORS_FOUND=true
					fi
				fi
			else
				# Not there, OK
				cxr_main_logger -v "${FUNCNAME}"  "File $OUTPUT_FILE does not yet exist - Good."
			fi
		done
		
		# Decrease global indent level
		cxr_main_decrease_log_indent
	else
		cxr_main_logger -v "${FUNCNAME}" "Will not check output"
	fi
	
	# re-enable checking of variables
	set -u
	
	# Inverting
	if [ "${ERRORS_FOUND:-false}" == false ]
	then
		# No errors
		echo true
	else
		# Detected errors
		echo false
	fi
	
	return 0
}

################################################################################
# Function: cxr_common_check_module_version
#	
# Checks if the currently loaded module requests a CAMxRunner and configuration
# Version less or equal than current one.
#
# Returns true on success, fals otherwise
################################################################################
function cxr_common_check_module_version() 
################################################################################
{
	# Test if Module was already announced (for efficiency and log-file size reasons)
	# The problem is that we live in a subprocess (function), so we cannot
	# add the name of this module to the string in here. this mst be done outside
	# (by the caller)
	FOUND=$(cxr_common_is_substring_present "$CXR_ANNOUNCED_MODULES" "$CXR_META_MODULE_NAME")
	
	# We announce only if it was not found
	if [ "$FOUND" == false ]
	then
	
		cxr_main_logger -v "${FUNCNAME}"  "\nLoading ${CXR_META_MODULE_NAME} - Version ${CXR_META_MODULE_VERSION}\n"    
	
		# Increase global indent level
		cxr_main_increase_log_indent
		
		cxr_main_logger -v "${FUNCNAME}"  "==========================================================\nName of the Module: *${CXR_META_MODULE_NAME}*\n==========================================================\nType: *${CXR_META_MODULE_TYPE}*\nRequires at least CAMxRunner Revision: *${CXR_META_MODULE_REQ_RUNNER_VERSION}*\n"
		cxr_main_logger -v "${FUNCNAME}"  "Description:\n$CXR_META_MODULE_DESCRIPTION\n"     
		cxr_main_logger -v "${FUNCNAME}"  "More Information can be found here:\n$CXR_META_MODULE_DOC_URL\n"     
		cxr_main_logger -v "${FUNCNAME}"  "The Module was written by: $CXR_META_MODULE_AUTHOR\nAnd is released under these terms:\n$CXR_META_MODULE_LICENSE\n"

		if [ "${CXR_META_MODULE_NUM_TESTS:-0}" -gt 0 ]
		then
			cxr_main_logger -v "${FUNCNAME}"  "This module contains a test suite."
		fi

		# Decrease global indent level
		cxr_main_decrease_log_indent
	fi
	
	################################################################################
	# Perform version check
	################################################################################	
	
	# Increase global indent level
	cxr_main_increase_log_indent
	
	# check if the stuff is set properly
	# Check CAMxRunner revision
	if [ "$CXR_META_MODULE_REQ_RUNNER_VERSION" -a \(  "$CXR_META_MODULE_REQ_RUNNER_VERSION" != "-" \) ]
	then
		
		# Increase global indent level
		cxr_main_increase_log_indent
		
		# CAMxRunner Version
		RUNNER_REVISION=$(cxr_common_get_svn_revision $CXR_RUN_DIR/CAMxRunner.sh)
		
		if [ "$RUNNER_REVISION" -ge "$CXR_META_MODULE_REQ_RUNNER_VERSION" ] 
		then
			cxr_main_logger -v "${FUNCNAME}"  "CAMxRunner Version OK"
		else
			cxr_main_logger -w "${FUNCNAME}" "Module $CXR_META_MODULE_NAME needs at least revision $CXR_META_MODULE_REQ_RUNNER_VERSION of CAMxRunner.\nCurrently running is revision $RUNNER_REVISION"
			echo false
			return 0
		fi
		
		# Decrease global indent level
		cxr_main_decrease_log_indent
		
	else
			cxr_main_logger -w "${FUNCNAME}" "Consider to set the required CAMxRunner Version in Module ${CXR_META_MODULE_NAME} using variable CXR_META_MODULE_REQ_RUNNER_VERSION"
	fi
	
	# check if the stuff is set properly
	# Check config revision
	if [ "$CXR_META_MODULE_REQ_CONF_VERSION" -a \(  "$CXR_META_MODULE_REQ_CONF_VERSION" != "-" \) ]
	then
		
		# Increase global indent level
		cxr_main_increase_log_indent
		
		# Config Revision
		CONFIG_REVISION=$(cxr_common_get_svn_revision $CXR_CONFIG)

		if [ "$CONFIG_REVISION" -ge "$CXR_META_MODULE_REQ_CONF_VERSION" ] 
		then
			cxr_main_logger -v "${FUNCNAME}"  "Config Version OK"
		else
			cxr_main_logger -w "${FUNCNAME}" "Module $CXR_META_MODULE_NAME needs at least revision $CXR_META_MODULE_REQ_CONF_VERSION of the config file $CXR_CONFIG.\nCurrently loaded is revision $CONFIG_REVISION"
			echo false
			return 0
		fi
		
		# Decrease global indent level
		cxr_main_decrease_log_indent
		
	else
			cxr_main_logger -w "${FUNCNAME}" "Consider to set the required Config Version in Module ${CXR_META_MODULE_NAME} using variable CXR_META_MODULE_REQ_CONF_VERSION"
	fi
	
	# Decrease global indent level
	cxr_main_decrease_log_indent


	# If we arrive here, it is fine
	echo true
	return 0

}

################################################################################
# Function: cxr_common_check_result
#	
# Checks if the output was generated
# Later, we can think about a real size check
#
# We basically just loop through a list of files (this is a radical break with the 
# old approach where we looped over an array of arrays of files).
# This list is called CXR_CHECK_THESE_OUTPUT_FILES and must be set befor calling this function
#
# Additionally, each created file is added to the file CXR_OUTPUT_FILE_LIST
#
# This function does not terminate the runner if errors are found,
# but it returns false
################################################################################
function cxr_common_check_result() 
################################################################################
{
	ERRORS_FOUND=false
	
	# No output check for dryruns
	if [ $CXR_DRY == true ]
	then
		
		cxr_main_logger -w "${FUNCNAME}" "Output check is not carried out for dryruns - no output was generated.\nStill we generate dummy files now..."
		
		# generate dummy files if needed
		for OUTPUT_FILE in ${CXR_CHECK_THESE_OUTPUT_FILES}
		do
			# does it not exist?
			if [ ! -f "${OUTPUT_FILE}" ]
			then
				cxr_common_create_dummyfile ${OUTPUT_FILE}
			else
				cxr_main_logger -w "${FUNCNAME}" "File ${OUTPUT_FILE} seems to exist, we do not touch it."
			fi
		done
		
		echo true
		return 0
	fi

	cxr_main_logger -v "${FUNCNAME}"  "Checking Result Files..."

	# Increase global indent level
	cxr_main_increase_log_indent

	# We do word splitting
	for OUTPUT_FILE in ${CXR_CHECK_THESE_OUTPUT_FILES}
	do
		# does it exist and is it larger than 0 bytes?
		if [ -s "${OUTPUT_FILE}" ]
		then
			cxr_main_logger -v "${FUNCNAME}" "Output File ${OUTPUT_FILE} has non-zero size, OK."
			
			# Add the file to CXR_OUTPUT_FILE_LIST if the file is not yet listed
			grep "${OUTPUT_FILE}${CXR_DELIMITER}${CXR_META_MODULE_NAME}" "${CXR_OUTPUT_FILE_LIST}"
			
			if [ "$?" -ne 0 ]
			then
				# File structure is 
				# filename|module_that_created it
				echo "${OUTPUT_FILE}${CXR_DELIMITER}${CXR_META_MODULE_NAME}" >> "${CXR_OUTPUT_FILE_LIST}"
			fi
			
		else
			cxr_main_logger -e "${FUNCNAME}:${LINENO} - File $(basename ${OUTPUT_FILE}) was not created properly"
			ERRORS_FOUND=true
		fi
	done
	# Decrease global indent level
	cxr_main_decrease_log_indent
	
	if [ "$ERRORS_FOUND" == false ]
	then
		# No errors
		echo true
	else
		# Detected errors
		echo false
	fi
}

################################################################################
# Function: cxr_common_is_version_supported
#	
# Checks if a version number is supported by a given model.
#
# Returns:
# 0 if ok, 1 otherwise
#
# Parameters:	
# $1 - The Version to check
# $2 - The Model name as indicated in CXR_SUPPORTED_MODELS
################################################################################
function cxr_common_is_version_supported()
################################################################################
{
	if [ -z "${1:-}" ]
	then
		# Forget it - must be larger than ""
		return 1
	fi
	
	MODEL_ID=$(cxr_common_get_model_id "$MODEL") || cxr_main_die_gracefully "Model $MODEL is not known."
	SUPPORTED="${CXR_SUPPORTED_MODEL_VERSIONS[${MODEL_ID}]}"
	
	# Check the Version
	FOUND=$(cxr_common_is_substring_present "$SUPPORTED" "$1")
	
	if [ $FOUND == true ]
	then
		# Found, ok
		return 0
	else
		cxr_main_logger -e "${FUNCNAME}"  "Version $1 of $2 is not supported by CAMxRunner. Adjust CXR_SUPPORTED_MODEL_VERSIONS\n(Currently $SUPPORTED)"    
		return 1
	fi
}

################################################################################
# Function: cxr_common_check_run_name
#	
# Checks if run name has correct form and length (max 60 characters)
# e. g. 
# CAMx-v4.51-ENVIRON_testcase or
# PMCAMx-v3.01-test
#
# So we basically split using "-"
#
# Much of the code here is repeated from <cxr_main_determine_model_and_version>
#
# Returns:
# Retruns 0 if ok, 1 otherwise.
#
# Parameters:
# $1 - String
################################################################################
function cxr_common_check_run_name()
################################################################################
{
	if [ $# -ne 1 ]
	then
		echo 0
		cxr_main_logger -e "${FUNCNAME}"  "${FUNCNAME}:${LINENO} - needs a string as input"
	fi
	
	# Length must not exceed 60 because we use it as
	# "note" field in all files
	if [ $(cxr_common_len $1) -gt 60 ]
	then
		echo 0
		cxr_main_logger -e "${FUNCNAME}"  "A run name must not be longer than 60 characters!"
	fi
	
	# Split it
	oIFS="$IFS"

	IFS=-

	# Suck line into array
	RUN_ARRAY=($1)

	# Reset IFS
	IFS="$oIFS"
	
	# First, there is the Model
	MODEL=${RUN_ARRAY[0]}
	
	# Then there is vVersion, remove v to the left:
	VERSION=${RUN_ARRAY[1]#v}
	
	#This returns non-0 if its not ok
	cxr_common_is_version_supported $VERSION $MODEL
		
	# We return the return value
	return $?
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
	if [ "${CXR_TESTING_FROM_HARNESS:-false}" == false ]
	then
		# We need to do initialisation
	
		# This is the run we use to test this
		CXR_RUN=$CXR_META_MODULE_TEST_RUN
	
		# Safety measure if script is not called from .
		MY_DIR=$(dirname $0) && cd $MY_DIR
	
		# We step down the directory tree until we either find CAMxRunner.sh
		# or hit the root directory /
		while [ $(pwd) != / ]
		do
			cd ..
			# If we find CAMxRunner, we are there
			ls CAMxRunner.sh >/dev/null 2>&1 && break
			
			# If we are in root, we have gone too far
			if [ $(pwd) == / ]
			then
				echo "Could not find CAMxRunner.sh!"
				exit 1
			fi
		done
		
		# Save the number of tests, as other modules
		# will overwrite this (major design issue...)
		MY_META_MODULE_NUM_TESTS=$CXR_META_MODULE_NUM_TESTS
		
		# Include the init code
		source inc/init_test.inc
		
		# Plan the number of tests
		plan_tests $MY_META_MODULE_NUM_TESTS
		
	fi
	
	########################################
	# Setup tests if needed
	########################################
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(cxr_common_check_datataype 1 I) true "cxr_common_check_datataype 1 I"

	########################################
	# teardown tests if needed
	########################################

}

################################################################################
# Are we running stand-alone? 
################################################################################


# If the CXR_META_MODULE_NAME  is not set
# somebody started this script alone
# Normlly this is not allowed, except to test using -t
if [ -z "${CXR_META_MODULE_NAME:-}" ]
then

	# When using getopts, never directly call a function inside the case,
	# otherwise getopts does not process any parametres that come later
	while getopts ":dvFST" opt
	do
		case "${opt}" in
		
			d) CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_LOG_EXT="-dry" ;;
			v) CXR_USER_TEMP_VERBOSE=true ; echo "Enabling VERBOSE (-v) output. All lines starting with % would not be present otherwise" ;;
			F) CXR_USER_TEMP_FORCE=true ;;
			S) CXR_USER_TEMP_SKIP_EXISTING=true ;;
			
			T) TEST_IT=true;;
			
		esac
	done
	
	# This is not strictly needed, but it allows to read 
	# non-named command line options
	shift $((${OPTIND} - 1))

	# Make getopts ready again
	unset OPTSTRING
	unset OPTIND
	
	# This is needed so that getopts surely processes all parameters
	if [ "${TEST_IT:-false}" == true ]
	then
		test_module
	else
		usage
	fi

fi
