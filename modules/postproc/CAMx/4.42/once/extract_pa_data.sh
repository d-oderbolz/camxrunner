# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Postprocessor for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Runs the IRR/IPR/CPA extractors on existing PA data. Only active if 
# PA is active.
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# TODO: Implement CPA 
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# This is a very important field needed for the scheduling of parallel processes.
# It contains a comma-separated list of modules (just their names without numbers or extensions)
# from whose output this module DIRECTLY depends on.
#
# A process can only start if its dependencies have finished. Only list direct dependencies.
# There are some special dependencies:
# ${CXR_TYPE_PREPROCESS_ONCE} - all pre_start_preprocessors must have finished
# ${CXR_TYPE_PREPROCESS_DAILY} - all daily_preprocessors must have finished
# ${CXR_TYPE_MODEL} - all model modules must have finished
# ${CXR_TYPE_POSTPROCESS_DAILY} - all daily_postprocessors must have finished
# ${CXR_TYPE_POSTPROCESS_ONCE} - all finish_postprocessors must have finished

# the predicate "-<n>" refers to some previous model day, so ${CXR_TYPE_MODEL}-1 means that all model modules of the previous day must be successful before this module may run. 

CXR_META_MODULE_DEPENDS_ON="${CXR_TYPE_MODEL}"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Runs the IRR/IPR/CPA extractors on existing proces analysis data. Only active if PA is active."

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_POSTPROCESS_ONCE}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=0

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

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
	if [[ "$CXR_PROBING_TOOL" == "PA" || "$CXR_PROBING_TOOL" == "IPR" || "$CXR_PROBING_TOOL" == "IRR" ]]
	then
		# One call per PA domain
		echo $CXR_NUMBER_OF_PA_DOMAINS
	elif [[ "$CXR_PROBING_TOOL" == "CPA" ]]
	then
		# One call per model day
		echo $CXR_NUMBER_OF_SIM_DAYS
	else
		main.log -a "CXR_PROBING_TOOL is not PA, we do not run extract_pa_data."
		echo 0
	fi

}

################################################################################
# Function: getProblemSize
#
# Returns the problem size of a given invocation.
# If the problem size is constant, return 1.
# 
# Parameters:
# $1 - invocation
################################################################################
function getProblemSize()
################################################################################
{
	echo 1
}

################################################################################
# Function: set_variables
#	
# Sets the appropriate variables for extract_pa_data, depending on CXR_PROBING_TOOL
################################################################################	
function set_variables()
################################################################################
{
	local day_offset
	
	# First of all, reset checks.
	# We will later continuously add entries to these 2 lists.
	# CAREFUL: If you add files to CXR_CHECK_THESE_OUTPUT_FILES,
	# these are deleted if he user runs the -F option. Do not mix up with input files!
	CXR_CHECK_THESE_INPUT_FILES=
	CXR_CHECK_THESE_OUTPUT_FILES=
	CXR_PA_IPR_FILES=
	CXR_PA_IRR_FILES=
	

	########################################################################
	# Set variables
	########################################################################
	
	# PA domain specific - we need to define CXR_PA_IGRID
	CXR_PA_IGRID=$CXR_INVOCATION
	
	####################################
	# Input
	####################################

	# There is one input file per day 
	for day_offset in $(seq 0 $((${CXR_NUMBER_OF_SIM_DAYS} -1 )) )
	do
		common.date.setVars "$CXR_START_DATE" "$day_offset"
		
		common.user.showProgress
		
		# PA means "do all" - but CPA is not implemented yet
		if [[ $CXR_PROBING_TOOL == "IPR" || $CXR_PROBING_TOOL == "PA" ]]
		then
			# We allow decompression
			CXR_PA_IPR_FILES="$CXR_PA_IPR_FILES $(common.runner.evaluateRule "$CXR_PA_IPR_OUTPUT_FILE_RULE" false CXR_PA_IPR_OUTPUT_FILE_RULE)"
		fi
		
		if [[ $CXR_PROBING_TOOL == "IRR" || $CXR_PROBING_TOOL == "PA" ]]
		then
			# We allow decompression
			CXR_PA_IRR_FILES="$CXR_PA_IRR_FILES $(common.runner.evaluateRule "$CXR_PA_IRR_OUTPUT_FILE_RULE" false CXR_PA_IRR_OUTPUT_FILE_RULE)"
		fi
	
	done # days
	
	####################################
	# Output
	####################################
	
	if [[ $CXR_PROBING_TOOL == "IPR" || $CXR_PROBING_TOOL == "PA" ]]
	then
	
		# The ASC Filenames are the output
		# Output files must not be decompressed!
		CXR_PA_IPR_EXT_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_PA_IPR_EXT_OUTPUT_FILE_RULE" false CXR_PA_IPR_EXT_OUTPUT_FILE_RULE false)
		
	fi
	
	if [[ $CXR_PROBING_TOOL == "IRR" || $CXR_PROBING_TOOL == "PA" ]]
	then
	
		# IRR generates two files
		CXR_PA_IRR_EXT_ASC_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_PA_IRR_EXT_ASC_OUTPUT_FILE_RULE" false CXR_PA_IRR_EXT_ASC_OUTPUT_FILE_RULE false)
		CXR_PA_IRR_EXT_BIN_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_PA_IRR_EXT_BIN_OUTPUT_FILE_RULE" false CXR_PA_IRR_EXT_BIN_OUTPUT_FILE_RULE false)
	
	fi
	
	# Checks 
	if [[ $CXR_PROBING_TOOL == "PA" ]]
	then
	
		CXR_CHECK_THESE_INPUT_FILES="$CXR_PA_IPR_FILES $CXR_PA_IRR_FILES"
		CXR_CHECK_THESE_OUTPUT_FILES="$CXR_PA_IPR_EXT_OUTPUT_FILE $CXR_PA_IRR_EXT_ASC_OUTPUT_FILE $CXR_PA_IRR_EXT_BIN_OUTPUT_FILE"
		
	elif [[ $CXR_PROBING_TOOL == "IPR" ]]
	then
		
		CXR_CHECK_THESE_INPUT_FILES="$CXR_PA_IPR_FILES"
		CXR_CHECK_THESE_OUTPUT_FILES="$CXR_PA_IPR_EXT_OUTPUT_FILE"
	
	elif [[ $CXR_PROBING_TOOL == "IRR" ]]
	then
		
		CXR_CHECK_THESE_INPUT_FILES="$CXR_PA_IRR_FILES"
		CXR_CHECK_THESE_OUTPUT_FILES="$CXR_PA_IRR_EXT_ASC_OUTPUT_FILE $CXR_PA_IRR_EXT_BIN_OUTPUT_FILE"
	
	fi
	
}

################################################################################
# Function: create_ipr_control_file
#	
# Creates a Control file that can be fed to the ext_ipr program.
#
# Returns:
# a filename of the created control file
#
# See:
# <write_control_file>
################################################################################
function create_ipr_control_file ()
################################################################################
{
	# Create a file name
	local ipr_control_file
	local currFile
	
	ipr_control_file=$(common.runner.createJobFile EXT_IPR)
	
	# Clean file
	: > $ipr_control_file
	
	# Loop though all input files
	for currFile in $CXR_PA_IPR_FILES
	do
		echo "CAMx IPR output fil|$currFile" >  $ipr_control_file
	done
	# This marker is needed to show end of input files
	echo "End of files       |/END/" >> $ipr_control_file
	
	echo "ASCII output file  |$CXR_PA_IPR_EXT_OUTPUT_FILE" >> $ipr_control_file
	echo "Units for output   |${CXR_PA_EXT_UNITS:-PPB}" >> $ipr_control_file
	echo "domain to extract  |${CXR_PA_IGRID}" >> $ipr_control_file
	echo "Beg/End I-cell     |${CXR_PA_EXT_I_BEG[${CXR_PA_IGRID}]:-1} ${CXR_PA_EXT_I_END[${CXR_PA_IGRID}]:-9999}" >> $ipr_control_file
	echo "Beg/End J-cell     |${CXR_PA_EXT_J_BEG[${CXR_PA_IGRID}]:-1} ${CXR_PA_EXT_J_END[${CXR_PA_IGRID}]:-9999}" >> $ipr_control_file
	echo "Beg/End layer      |${CXR_PA_EXT_K_BEG[${CXR_PA_IGRID}]:-1} ${CXR_PA_EXT_K_END[${CXR_PA_IGRID}]:-9999}" >> $ipr_control_file
	
	# Return filename
	echo "$ipr_control_file"
}

################################################################################
# Function: create_irr_control_file
#	
# Creates a Control file that can be fed to the ext_irr program.
#
# Returns:
# a filename of the created control file
#
# See:
# <write_control_file>
################################################################################
function create_irr_control_file ()
################################################################################
{
	# Create a file name
	local irr_control_file
	local currFile
	
	irr_control_file=$(common.runner.createJobFile EXT_IRR)
	
	# Clean file
	: > $irr_control_file
	
	# Loop though all input files
	for currFile in $CXR_PA_IRR_FILES
	do
		echo "CAMx IPR output fil|$currFile" >  $irr_control_file
	done
	# This marker is needed to show end of input files
	echo "End of files       |/END/" >> $irr_control_file
	
	echo "ASCII output file  |$CXR_PA_IRR_EXT_ASC_OUTPUT_FILE" >> $irr_control_file
	echo "Binary output file |$CXR_PA_IRR_EXT_BIN_OUTPUT_FILE" >> $irr_control_file
	echo "domain to extract  |${CXR_PA_IGRID}" >> $irr_control_file
	echo "Beg/End I-cell     |${CXR_PA_EXT_I_BEG[${CXR_PA_IGRID}]:-1} ${CXR_PA_EXT_I_END[${CXR_PA_IGRID}]:-9999}" >> $irr_control_file
	echo "Beg/End J-cell     |${CXR_PA_EXT_J_BEG[${CXR_PA_IGRID}]:-1} ${CXR_PA_EXT_J_END[${CXR_PA_IGRID}]:-9999}" >> $irr_control_file
	echo "Beg/End layer      |${CXR_PA_EXT_K_BEG[${CXR_PA_IGRID}]:-1} ${CXR_PA_EXT_K_END[${CXR_PA_IGRID}]:-9999}" >> $irr_control_file
	echo "Hours of input data|$(common.date.getTotalModelHours)" >> $irr_control_file
	# Return filename
	echo "$irr_control_file"
}

################################################################################
# Function: create_cpa_control_file
#	
# Creates a Control file that can be fed to the vertavg program.
#
# Returns:
# a filename of the created control file
#
# See:
# <write_control_file>
################################################################################
function create_cpa_control_file ()
################################################################################
{
	# Create a file name
	local cpa_control_file
	local currFile
	
	cpa_control_file=$(common.runner.createJobFile EXT_IRR)
	
	# Clean file
	: > $cpa_control_file
	
	# Currently, we only support coarse grid output
	echo "COARSE or FINE x?  |COARSE" >> $cpa_control_file
	echo "CAMx Kv file       |" >> $cpa_control_file
	echo "CAMx ZP file       |" >> $cpa_control_file
	echo "CAMx TP file       |" >> $cpa_control_file
	echo "Coarse *.cpa file  |$CXR_PA_CPA_FILE" >> $cpa_control_file
	# echo "Fine *.cpa file    |" >> $cpa_control_file
	echo "Output file        |$CXR_PA_CPA_EXT_OUTPUT_FILE" >> $cpa_control_file
	
	# Return filename
	echo "$cpa_control_file"
}

################################################################################
# Function: run_pa
#	
# Performs PA extraction
# 
################################################################################	
function run_pa() 
################################################################################
{
		local exec
		local control_file
		
		exec="${1}"
		control_file="${2}"
		
		main.log -v  "Running ${exec}..."

		if [[ "$CXR_DRY" == false ]]
		then
			#Call the converter, collect sterr and stout
			${exec} < ${control_file} 2>&1 | tee -a $CXR_LOG
		else
				main.log   "Dryrun, no extraction performed"
		fi
}
################################################################################
# Function: extract_pa_data
#	
# Extracts the process analysis data from the CAMx output.
# Currently, only IPR is supported.
# 
# TODO: Implement other PA Postprocesors
################################################################################	
function extract_pa_data() 
################################################################################
{
	# Here, the invocation is the PA domain number
	CXR_INVOCATION=${1}
	
	local exec
	local control_file
	
	#Was this stage already completed?
	if [[ $(common.state.storeStatus ${CXR_STATUS_RUNNING}) == true ]]
	then
		#  --- Setup the Environment of the current day
		set_variables 
		
		#  --- Check Settings
		# Postprocessor: we only terminate the module
		if [[ $(common.check.preconditions) == false ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		# Any existing file will be skipped 
		for checkFile in $CXR_CHECK_THESE_OUTPUT_FILES
		do
			if [[ -s "$checkFile" ]]
			then
				if [[ "${CXR_SKIP_EXISTING}" == true ]]
				then
					# Skip it
					main.log "File ${checkFile} exists - file will skipped."
					common.state.storeStatus ${CXR_STATUS_SUCCESS}  > /dev/null
					return $CXR_RET_OK
				else
					main.log -e  "File ${checkFile} exists - to force the re-creation run ${CXR_CALL} -F"
					common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
					return $CXR_RET_ERROR
				fi
			fi
		done

		# Currently, we assume IPR
		case ${CXR_PROBING_TOOL} in

			PA)		main.log -a "Running extraction for IRR,IPR and CPA..."
						exec="$CXR_EXT_IPR_EXEC"
						# IPR
						control_file=$(create_ipr_control_file)
						run_pa "$exec" "$control_file"
						
						#IRR
						control_file=$(create_irr_control_file)
						run_pa "$exec" "$control_file"
						
						# CPA (not implemented)
						#control_file=$(create_cpa_control_file)
						#run_pa "$exec" "$control_file"
						;;
						
			IRR)	main.log -a "Running extraction for IRR..."
						exec="$CXR_EXT_IRR_EXEC"
						control_file=$(create_irr_control_file)
						run_pa "$exec" "$control_file";;
						
		 IPR)		main.log -a "Running extraction for IPR..."
						exec="$CXR_EXT_IPR_EXEC"
						control_file=$(create_ipr_control_file)
						run_pa "$exec" "$control_file";;

		 CPA)		main.log -a "Support for CPA currently not implemented."	
		 				common.state.storeStatus ${CXR_STATUS_SUCCESS} > /dev/null
		 				return $CXR_RET_OK
		 				
						#exec="$CXR_EXT_CPA_EXEC"
						#control_file=$(create_cpa_control_file)
						
						# Run it
						#run_pa "$exec" "$control_file"
						;;
						
			*)		main.log -e "Unsupported PA Tool $CXR_PA_TOOL!" 
						common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
						return $CXR_RET_ERROR;;

		esac

		
		
		# Check if all went well
		# Postprocessor: we only terminate the module
		if [[ $(common.check.postconditions) == false  ]]
		then
			main.log -a "Postconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_POSTCONDITIONS
		fi
		
		common.state.storeStatus ${CXR_STATUS_SUCCESS} > /dev/null
	else
		main.log  "Stage $(common.task.getId) was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
	fi
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

	########################################
	# Setup tests if needed
	########################################
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	# None yet.
	:

	########################################
	# teardown tests if needed
	########################################
	
}