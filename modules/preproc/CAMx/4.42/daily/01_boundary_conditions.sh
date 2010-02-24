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

CXR_META_MODULE_DEPENDS_ON="convert_input"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Generates a file containing the boundary concentrations at the 4 faces of the coarse grid"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_DAILY}"

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
# Sets the appropriate variables needed for <boundary_conditions>
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
	CXR_CHECK_THESE_INPUT_FILES="$CXR_BC_PROC_INPUT_FILE"
	
	########################################################################
	# Set variables
	########################################################################
	
	NLEV=${CXR_NUMBER_OF_LAYERS[1]}
	
	# The date needed by this function is a bit strange
	# It needs a 2-digit yoer and a 3-digit DOY
	IBDATE="${CXR_YEAR_S}$(cxr_common_day_of_year ${CXR_DATE} 3 )"
	
	# Evaluate some rules
	
	# Final output files
	# Output files must not be decompressed!
	CXR_BC_OUTPUT_FILE="$(cxr_common_evaluate_rule "$CXR_BOUNDARY_CONDITIONS_FILE_RULE" false CXR_BOUNDARY_CONDITIONS_FILE_RULE false)"
	
	# The processor creates this intermediate file (must not be checked)
	CXR_BC_ASC_OUTPUT_FILE="${CXR_BC_OUTPUT_FILE}.${CXR_ASC_EXT}"
	
	# CXR_CHECK_THESE_OUTPUT_FILES is a space separated list of output files to check
	CXR_CHECK_THESE_OUTPUT_FILES="$CXR_BC_OUTPUT_FILE "
	
	# ICBCPREP needs no input files, except the frst BC file
	if [[ "${CXR_IC_BC_TC_METHOD}" != ICBCPREP  ]]
	then 
		# All MOZART-flavors need Input
		
		# We need a MOZART file as input
		CXR_MOZART_INPUT_FILE="$(cxr_common_evaluate_rule "$CXR_GLOBAL_CTM_FILE_RULE" false CXR_GLOBAL_CTM_FILE_RULE)"
	
		# Also, we need a domain 1 meteo file
		i=1
		CXR_METEO_INPUT_FILE="$(cxr_common_evaluate_rule "$CXR_MMOUT_FILE_RULE" false CXR_MMOUT_FILE_RULE)"
		
		# And finally the ZP file
		CXR_ZP_INPUT_FILE="$(cxr_common_evaluate_rule "$CXR_PRESSURE_ASC_FILE_RULE" false CXR_PRESSURE_ASC_FILE_RULE)"
	
		# a space separated list of input files to check
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_MOZART_INPUT_FILE $CXR_METEO_INPUT_FILE $CXR_ZP_INPUT_FILE"
	else
	
		# ICBCPREP needs to know the filename of the first BC file
		CXR_FIRST_BC_FILE="$(cxr_common_evaluate_rule_at_offset "$CXR_BOUNDARY_CONDITIONS_FILE_RULE" 0 false CXR_BOUNDARY_CONDITIONS_FILE_RULE)"
		
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_FIRST_BC_FILE"
	fi
}

################################################################################
# Function: boundary_conditions
#	
# Converts emissions for a given day
################################################################################
function boundary_conditions() 
################################################################################
{
	# Define & Initialize local vars
	local extra=
	local species=
	local conc=
	local spec_line=
	local exec_tmp_file=
	local i=0
	local mozart_array=
	local camx_array=
	local mozart_spec=
	local camx_spec=
	
	# These we set with the ternary (or conditional) operator
	local doplots=$(( "${CXR_IC_BC_TC_DO_PLOT:-false}" == true  ? 1 : 0 ))
	local dopng=$(( "${CXR_IC_BC_TC_DO_PNG:-false}" == true  ? 1 : 0 ))
	local deleteps=$(( "${CXR_IC_BC_TC_RM_PS:-false}" == true  ? 1 : 0 ))
	
	#Was this stage already completed?
	if [[ $(cxr_common_store_state ${CXR_STATE_START}) == true  ]]
	then
		#  --- Setup the Environment
		set_variables 
		
		#  --- Check Settings
		if [[ $(cxr_common_check_preconditions) == false  ]]
		then
			cxr_main_logger "${FUNCNAME}" "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		if [[ ! -f "${CXR_BC_OUTPUT_FILE}"  ]]
		then
			# File does not exist
		
			# Increase global indent level
			cxr_main_increase_log_indent
	
			cxr_main_logger "${FUNCNAME}"  "Preparing BOUNDARY CONDITIONS data..."
			
			# What method is wanted?
			case "${CXR_IC_BC_TC_METHOD}" in
			
				MOZART | MOZART_CONSTANT | MOZART_INCREMENT )
				
					# MOZART_CONSTANT or INCREMENT?
					if [[ "${CXR_IC_BC_TC_METHOD}" == MOZART_CONSTANT  ]]
					then
					
						# Open the bracket
						extra="{"
						
						for spec_line in ${CXR_IC_BC_TC_SPEC[@]}
						do
							# Each line looks something like
							# O3:0.074740447 
							
							if [[ "$spec_line"  ]]
							then
								# Make sure its uppercase
								species=$(cxr_common_to_upper $(echo $spec_line | cut -d: -f1))
								conc=$(echo $spec_line | cut -d: -f2)
								
								#Add to extra
								extra="${extra} c${species}:${conc},"
							fi
						done
						
						# Remove last comma
						extra=${extra%\,}
						
						# Close the bracket
						extra="${extra}}"
						
						# Add the rest of the syntax
						extra=",extra=${extra}"
						
					elif [[ "${CXR_IC_BC_TC_METHOD}" == MOZART_INCREMENT  ]]
					then
					
						# Open the bracket
						extra="{"
						
						for spec_line in ${CXR_IC_BC_TC_SPEC[@]}
						do
							# Each line looks something like
							# O3:0.074740447 
							
							if [[ "$spec_line"  ]]
							then
								# Make sure its uppercase
								species=$(cxr_common_to_upper $(echo $spec_line | cut -d: -f1))
								conc=$(echo $spec_line | cut -d: -f2)
								
								#Add to extra
								extra="${extra} i${species}:${conc},"
							fi
						done
						
						# Remove last comma
						extra=${extra%\,}
						
						# Close the bracket
						extra="${extra}}"
						
						# Add the rest of the syntax
						extra=",extra=${extra}"

					fi
				
					# We will write the IDL call into a temporary file
					exec_tmp_file=$(cxr_common_create_tempfile $FUNCNAME)
					
					# Go there
					cd $(dirname ${CXR_BC_PROC_INPUT_FILE}) || return $CXR_RET_ERROR
					
					
					# First of all, we need to create the 2 arrays or mozart and CAMx species
					# that we pass to the procedure
					
					# Open brackets
					mozart_array="["
					camx_array="["
					
					for i in $(seq 0 $(( $CXR_NUM_MOZART_SPECIES - 1 )))
					do
						
						mozart_spec=$(echo ${CXR_CAMX_MOZART_MAPPING[$i]} | cut -d: -f2)
						camx_spec=$(echo ${CXR_CAMX_MOZART_MAPPING[$i]} | cut -d: -f1)
						
						mozart_array="${mozart_array}'${mozart_spec}',"
						camx_array="${camx_array}'${camx_spec}',"
		
					done
					
					# Close brackets and remove last ","
					mozart_array="${mozart_array%,}]"
					camx_array="${camx_array%,}]"
		
					# Create the file to run IDL
					echo ".run $(basename ${CXR_BC_PROC_INPUT_FILE})" >> ${exec_tmp_file}
					# Interface:
					# pro camxbound $
					# ,fmoz $
					# ,fln $
					# ,mm5camxinfile $
					# ,outfile_bc $
					# ,nlevs $
					# ,mozart_specs $
					# ,camx_specs $
					# ,note $
					# ,xorg $
					# ,yorg $
					# ,delx $
					# ,dely $
					# ,ibdate $ 
					# ,doplots $
					# ,plot_base_dir $
					# ,MOZtime $
					# ,run_name $
					# ,dopng $
					# ,deleteps $
					# ,extra=extra
					# we need to multiply the resolution by 1000 (metre)
					
					echo "$(basename ${CXR_BC_PROC_INPUT_FILE} .pro),'${CXR_MOZART_INPUT_FILE}','${CXR_METEO_INPUT_FILE}','${CXR_ZP_INPUT_FILE}','${CXR_BC_ASC_OUTPUT_FILE}',$NLEV,$mozart_array,$camx_array,'${CXR_RUN}',$CXR_MASTER_ORIGIN_XCOORD,$CXR_MASTER_ORIGIN_YCOORD,$(cxr_common_fp_calculate "$CXR_MASTER_CELL_XSIZE * 1000"),$(cxr_common_fp_calculate "$CXR_MASTER_CELL_YSIZE * 1000"),'$IBDATE',${doplots},'$CXR_IC_BC_TC_PLOT_BASE_DIR',$CXR_IC_BC_TC_PLOT_TIME,'${CXR_RUN}',${dopng},${deleteps}${extra}" >> ${exec_tmp_file}
					echo "exit" >> ${exec_tmp_file}
					
					# Get a copy of the call
					cat ${exec_tmp_file} | tee -a $CXR_LOG
						
					if [[ "$CXR_DRY" == false  ]]
					then
						
						# Then we run it, while preserving the output
						${CXR_IDL_EXEC} < ${exec_tmp_file} 2>&1 | tee -a $CXR_LOG
						
						# Now we need to convert the file to binary format
						"${CXR_AIRCONV_EXEC}" ${CXR_BC_ASC_OUTPUT_FILE} ${CXR_BC_OUTPUT_FILE} BOUNDARY 0 2>&1 | tee -a $CXR_LOG
						
					else
						cxr_main_logger "${FUNCNAME}"  "This is a dry-run, will not run program"
					fi
			
					# Get back
					cd ${CXR_RUN_DIR}  || return $CXR_RET_ERROR
			
					# Decrease global indent level
					cxr_main_decrease_log_indent
			
				;;
				
				ICBCPREP )
				
					cxr_main_logger -w "${FUNCNAME}"  "Preparing BOUNDARY CONDITIONS data using CONSTANT data..."
					# We use the topconc file that was created by initial_conditions
					# And we link to the BC file that was created there.
				
					if [[ "$CXR_DRY" == false  ]]
					then
						# We only create a file the first day, all others we link
						if [[ "$(cxr_common_is_first_day)" == false  ]]
						then
							# Not the first day, just link
							ln -s "${CXR_FIRST_BC_FILE}" "${CXR_BC_OUTPUT_FILE}"
						fi
					else
						cxr_main_logger "${FUNCNAME}"  "This is a dry-run, no action required"
					fi
				;;
				
			esac
	
			# Check if all went well
			if [[ "$(cxr_common_check_result)" == false  ]]
			then
				cxr_main_logger "${FUNCNAME}" "Postconditions for ${CXR_META_MODULE_NAME} are not met!"
				# We notify the caller of the problem
				return $CXR_RET_ERR_POSTCONDITIONS
			fi
		
		else
			# File exists. That is generally bad,
			# unless user wants to skip
			if [[ "$CXR_SKIP_EXISTING" == true  ]]
			then
				# Skip it
				cxr_main_logger -w "${FUNCNAME}"  "File $CXR_BC_OUTPUT_FILE exists - because of CXR_SKIP_EXISTING, file will skipped."
				return 0
			else
				# Fail!
				cxr_main_logger -e "${FUNCNAME}" "File $CXR_BC_OUTPUT_FILE exists - to force the re-creation run ${CXR_CALL} -F"
				return $CXR_RET_ERROR
			fi
		fi

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
		if [[ $(pwd) == /  ]]
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
	boundary_conditions
	
	echo "For now, you need to inspect the results manually"
	
	exit 1
}


################################################################################
# Are we running stand-alone? 
################################################################################


# If the CXR_META_MODULE_NAME  is a subset of the progname,
# somebody started this script alone
# Normlly this is not allowed, exept to test using -t
if [[ $(expr match "$progname" ".*$CXR_META_MODULE_NAME.*") -gt 0  ]]
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
	if [[ "${TEST_IT:-false}" == true  ]]
	then
		test_module
	fi
	
	usage
	
fi

################################################################################
# Code beyond this point is not executed in stand-alone operation
################################################################################



