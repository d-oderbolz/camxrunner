#!/usr/bin/env bash
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
# TODO: Check which date to pass (probably the center day...)
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

CXR_META_MODULE_DEPENDS_ON="albedo_haze_ozone"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Runs AHOMAP to generate the Albedo/haze/ozone input file"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_ONCE}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=0

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=CAMx-v4.51-test

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

	Is designed to be called by the CAMxRunner.
	
	You can, however, call it like this:
	
	$ $progname -T
	
	this starts the self-test of the module.
	
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
# Function: set_convert_emissions_variables
#	
# Sets the appropriate variables needed for <convert_emissions>
################################################################################	
function set_photolysis_rates_variables() 
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
	
	# Evaluate some rules
	CXR_AHOMAP_INPUT_FILE="$(cxr_common_evaluate_rule "$CXR_ALBEDO_HAZE_OZONE_FILE_RULE" false CXR_ALBEDO_HAZE_OZONE_FILE_RULE)"
	# Output files must not be decompressed!
	CXR_TUV_OUTPUT_FILE="$(cxr_common_evaluate_rule "$CXR_PHOTOLYIS_RATES_FILE_RULE" false CXR_PHOTOLYIS_RATES_FILE_RULE false)"
	
	#Checks
	CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_AHOMAP_INPUT_FILE"
	CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES $CXR_TUV_OUTPUT_FILE"

}


################################################################################
# Function: create_tuv_control_file
#	
# Creates a TUV Control file that can be fed to the program.
#
# Parameters:
# $1 - filename of AHOMAP file
#
# Returns:
# a filename of the created control file
#
# See:
# <write_control_file>
################################################################################
function create_tuv_control_file ()
################################################################################
{
	# Create a file name
	TUV_CONTROL_FILE=$(cxr_common_create_tempfile $FUNCNAME)

	# Here, we need only > (overwrite)
	echo "output file name   |$CXR_TUV_OUTPUT_FILE" > $TUV_CONTROL_FILE
	
	# From here one, please >> (append)
	echo "ahomap file name   |$CXR_AHOMAP_INPUT_FILE" >> $TUV_CONTROL_FILE
	echo "ozone, albedo, haze|$CXR_NOZN, $CXR_NALB, $CXR_NHAZ" >> $TUV_CONTROL_FILE
	echo "# of vert levels   |$CXR_NHGHT" >> $TUV_CONTROL_FILE
	echo "levels, km agl     |$CXR_TUV_HEIGHTS" >> $TUV_CONTROL_FILE
	echo "date (YYMMDD)      |${CXR_YEAR_S}${CXR_MONTH}${CXR_DAY}" >> $TUV_CONTROL_FILE
	echo "rad transfer scheme|$CXR_TUV_RADIATIVE_TRANSFER_SCHEME " >> $TUV_CONTROL_FILE
	echo "# of phot reactions|$CXR_TUV_NO_OF_REACTIONS" >> $TUV_CONTROL_FILE
	echo "TUV reaction #s    |$CXR_TUV_REACTION_NUMBERS" >> $TUV_CONTROL_FILE
	
	# Return the file name
	echo $TUV_CONTROL_FILE
}

################################################################################
# Function: photolysis_rates
#	
# Converts emissions for a given day
#
# See:
# <create_tuv_control_file>
################################################################################
function photolysis_rates() 
################################################################################
{
	#Was this stage already completed?
	if [ $(cxr_common_store_state ${CXR_STATE_START}) == true ]
	then
	
		# Reset stored variables
		LAST_WEEK=
		LAST_MONTH=
	
		for DAY_OFFSET in $(seq 0 $((${CXR_NUMBER_OF_SIM_DAYS} -1 )) )
		do
		
			cxr_common_set_date_variables "$CXR_START_DATE" "$DAY_OFFSET"
		
			# Check if we need another file
			case "${CXR_RUN_AHOMAP_TUV_INTERVAL:-once}" in
			
				once )
					cxr_main_logger -b ${FUNCNAME} "Running TUV for whole period..."
					SUBSTAGE=once
					;;
					
				daily )
					cxr_main_logger -b ${FUNCNAME} "Running TUV for $CXR_DATE..."
					SUBSTAGE=$CXR_DATE
					;;
					
				weekly )
					# Are we in a new week?
					if [ "$LAST_WEEK" != "$CXR_WOY" ]
					then
						cxr_main_logger -b ${FUNCNAME} "Running TUV for week $CXR_WOY..."
						SUBSTAGE=$CXR_WOY
					else
						# No new week
						LAST_WEEK=$CXR_WOY
						LAST_MONTH=$CXR_MONTH
						continue
					fi
					;;
				
				monthly )
					# Are we in a new month?
					if [ "$LAST_MONTH" != "$CXR_MONTH" ]
					then
						cxr_main_logger -b ${FUNCNAME} "Running TUV for month $CXR_MONTH..."
						SUBSTAGE=$CXR_MONTH
					else
						# No new week
						LAST_WEEK=$CXR_WOY
						LAST_MONTH=$CXR_MONTH
						continue
					fi
					;;
			
				*)
					cxr_main_die_gracefully "Unknown interval for TUV! Exiting." ;;
			esac
			
			# Call to the state db to set a substage
			# So we can tell each single TUV run from each other
			cxr_common_set_substage $SUBSTAGE
			
			if [ $(cxr_common_store_state ${CXR_STATE_START}) == true ]
			then
				# Substage not yet run
			
				#  --- Setup the Environment
				set_photolysis_rates_variables 
				
				#  --- Check Settings
				if [ $(cxr_common_check_preconditions) == false ]
				then
					cxr_main_logger "${FUNCNAME}" "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
					# We notify the caller of the problem
					return $CXR_RET_ERR_PRECONDITIONS
				fi
				
				if [ ! -f "$CXR_TUV_OUTPUT_FILE" ]
				then
					# TUV File does not exist
				
					# Increase global indent level
					cxr_main_increase_log_indent
			
					cxr_main_logger "${FUNCNAME}" "Preparing photolysis lookup table for run ${CXR_RUN}..."
					
					if [ "$CXR_DRY" == false ]
					then
						# TUV needs an input file
						TUV_CONTROL_FILE=$(create_tuv_control_file)
						
						# Is the file there and not empty?)
						if [ -s "${TUV_CONTROL_FILE}" ]
						then
					
							# TUV is picky - it expects a file called
							# tuv.inp and also it looks for some files
							# That's why we chage to CXR_CAMX_DIR and create a link in the current place
							
							cd ${CXR_CAMX_DIR} || return $CXR_RET_ERROR
							
							# First remove it
							rm -f tuv.inp
							ln -s "$TUV_CONTROL_FILE" tuv.inp
			
							# Call TUV
							${CXR_TUV_EXEC}  2>&1 | tee -a $CXR_LOG
			
						else
							cxr_main_logger "${FUNCNAME}" "Could not create TUV control file - module failed."
							return $CXR_RET_ERROR
						fi
			
					else
						# Dryrun, create dummy
						TUV_CONTROL_FILE=$(cxr_common_create_tempfile $FUNCNAME)
						
						cxr_main_logger "${FUNCNAME}" "Dryrun - TUV not performed"
					fi
			
					# Decrease global indent level
					cxr_main_decrease_log_indent
			
					# Check if all went well
					if [ $(cxr_common_check_result) == false ]
					then
						cxr_main_logger "${FUNCNAME}" "Postconditions for ${CXR_META_MODULE_NAME} are not met!"
						# We notify the caller of the problem
						return $CXR_RET_ERR_POSTCONDITIONS
					fi
					
					# Removing unneeded files
					rm -f tuv.inp
					rm -f tuv.out
				else
					# File exists. That is generally bad,
					# unless user wants to skip
					if [ "$CXR_SKIP_EXISTING" == true ]
					then
						# Skip it
						cxr_main_logger -w "${FUNCNAME}"  "File $CXR_TUV_OUTPUT_FILE exists - because -S option was supplied, file will skipped."
						
						# next iteration
						LAST_WEEK=$CXR_WOY
						LAST_MONTH=$CXR_MONTH
						continue
					else
						# Fail!
						cxr_main_logger -e "${FUNCNAME}" "File $CXR_TUV_OUTPUT_FILE exists - to force the re-creation run ${CXR_CALL} -F"
						return $CXR_RET_ERROR
					fi
				fi
				
				# Store the state (substage)
				cxr_common_store_state ${CXR_STATE_STOP} > /dev/null
				
				# Unset substage
				cxr_common_unset_substage
				
			else
				cxr_main_logger -w "$FUNCNAME" "Substage $SUBSTAGE already run."
			fi
			
			# Do not repeat loop if we run it only once
			if [ "${CXR_RUN_AHOMAP_TUV_INTERVAL}" == once ]
			then
				break
			fi
			
			LAST_WEEK=$CXR_WOY
			LAST_MONTH=$CXR_MONTH
			
		done
		
		# Reset date variables for first day
		cxr_common_set_date_variables "$CXR_START_DATE" "0"

		# Return to where we were
		cd $CXR_RUN_DIR || return $CXR_RET_ERROR
		
		
		# All is fine
		CXR_STATUS=$CXR_STATUS_SUCCESS

		# Store the state
		cxr_common_store_state ${CXR_STATE_STOP} > /dev/null
	else
		cxr_main_logger "${FUNCNAME}" "${FUNCNAME}:${LINENO} - Stage $(cxr_common_get_stage_name) was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
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
	ERROR_COUNT=0
	TEST_COUNT=1
	
	# This is our test run for this module
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
	
	# Include the init code
	source inc/init_test.inc

	# For this module, testing is harder 
	# compared to date_functions because we cannot just compare
	# Expected with actual results
	photolysis_rates
	
	echo "For now, you need to inspect the results manually"
	
	exit 1
}


################################################################################
# Are we running stand-alone? 
################################################################################


# If the CXR_META_MODULE_NAME  is a subset of the progname,
# somebody started this script alone
# Normlly this is not allowed, exept to test using -t
if [ $(expr match "$progname" ".*$CXR_META_MODULE_NAME.*") -gt 0 ]
then

	# When using getopts, never directly call a function inside the case,
	# otherwise getopts does not process any parametres that come later
	while getopts ":dvFST" opt
	do
		case "${opt}" in
		
			d) CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_LOG_EXT="-dry" ;;
			v) CXR_USER_TEMP_VERBOSE=true ; echo "Enabling VERBOSE (-v) output. " ;;
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
	fi
	
	usage
	
fi

################################################################################
# Code beyond this point is not executed in stand-alone operation
################################################################################



