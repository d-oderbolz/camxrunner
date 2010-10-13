# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
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
CXR_META_MODULE_DESCRIPTION="Creates a directory full of links for the postprocessors (for convert_output, run_emifad, run_aqmfad)"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_POSTPROCESS_DAILY}"

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
	# This module needs one invocation per step
	echo 1
}

################################################################################
# Function: getProblemSize
#
# Returns the problem size of a given invocation.
# If the problem size is constant, return 1.
#
################################################################################
function getProblemSize()
################################################################################
{
	# The Problem size here is not a function of the invocation
	echo 1
}

################################################################################
# Function: set_variables
#	
# Sets the needed variables needed for <prepare_output_dir>
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
	
	# File names here are generated using file name rules (strings containig variables)
	# which are expanded run time. See http://people.web.psi.ch/oderbolz/CAMxRunner#FileRules
	# The expansion is done using $(common.runner.evaluateRule "$VAR")

	########################################################################
	# Per-day settings
	########################################################################
	
	########################################################################
	# per day-per grid settings
	# we loop
	#	expand the file name rule
	#	then export the name and the value
	########################################################################
	for CXR_IGRID in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
	do
		# Because the input might be compressed, we use two sets af arrays,
		# the second one contains just the real basenames (these are the link-names)
		# Note the last argument (false) in every even call to <common.runner.evaluateRule>
	
		# Terrain
		CXR_TERRAIN_GRID_ASC_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_TERRAIN_ASC_FILE_RULE" false CXR_TERRAIN_ASC_FILE_RULE)
		CXR_TERRAIN_GRID_NAME[${CXR_IGRID}]=$(basename $(common.runner.evaluateRule "$CXR_TERRAIN_ASC_FILE_RULE" false CXR_TERRAIN_ASC_FILE_RULE false))
		
		# Pressure
		CXR_ZP_GRID_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_PRESSURE_FILE_RULE" false CXR_PRESSURE_FILE_RULE)
		CXR_ZP_GRID_NAME[${CXR_IGRID}]=$(basename $(common.runner.evaluateRule "$CXR_PRESSURE_FILE_RULE" false CXR_PRESSURE_FILE_RULE false))
		
		# Wind
		CXR_WIND_GRID_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_WIND_FILE_RULE" false CXR_WIND_FILE_RULE)
		CXR_WIND_GRID_NAME[${CXR_IGRID}]=$(basename $(common.runner.evaluateRule "$CXR_WIND_FILE_RULE" false CXR_WIND_FILE_RULE false))
		
		# Temperature
		CXR_TEMP_GRID_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_TEMPERATURE_FILE_RULE" false CXR_TEMPERATURE_FILE_RULE)
		CXR_TEMP_GRID_NAME[${CXR_IGRID}]=$(basename $(common.runner.evaluateRule "$CXR_TEMPERATURE_FILE_RULE" false CXR_TEMPERATURE_FILE_RULE false))
		
		# Vapor
		CXR_VAPOR_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_VAPOR_FILE_RULE" false CXR_VAPOR_FILE_RULE)
		CXR_VAPOR_GRID_NAME[${CXR_IGRID}]=$(basename $(common.runner.evaluateRule "$CXR_VAPOR_FILE_RULE" false CXR_VAPOR_FILE_RULE false))
		
		# Vertical K
		CXR_KV_GRID_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_K_FILE_RULE" false CXR_K_FILE_RULE)
		CXR_KV_GRID_NAME[${CXR_IGRID}]=$(basename $(common.runner.evaluateRule "$CXR_K_FILE_RULE" false CXR_K_FILE_RULE false))
		
		# Emissions done in run_emifad 
		
		# These are used for <convert_output>
		# Despite the name an input here
		CXR_AVG_OUTPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_AVG_FILE_RULE" false CXR_AVG_FILE_RULE)
		CXR_AVG_OUTPUT_NAME[${CXR_IGRID}]=$(basename $(common.runner.evaluateRule "$CXR_AVG_FILE_RULE" false CXR_AVG_FILE_RULE false))
	
		#Checks
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_TERRAIN_GRID_ASC_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_ZP_GRID_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_WIND_GRID_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_TEMP_GRID_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_VAPOR_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_KV_GRID_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_AVG_OUTPUT_ARR_FILES[${CXR_IGRID}]}"
	
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
	# We do not need this variable here (exept implicit for the stage name)
	CXR_INVOCATION=${1:-1}
	
	# Define & Initialize local vars
	local iGrid
	local i_input_arr
	local var
	local var_name
	local current_file
	local current_base

	if [[  "${CXR_RUN_LIMITED_PROCESSING}" == true && "${CXR_REMOVE_DECOMPRESSED_FILES}" == true   ]]
	then
		main.log -w   "This module is susceptible to limited processing, because it creates links rather than files.\nIf you use compressed input files, the temporary files into which we decompress wil be deleted.\nUse \n \t ${CXR_CALL} -L \nto avoid this."
	fi

	#Was this stage already completed?
	if [[ $(common.state.storeStatus ${CXR_STATUS_RUNNING}) == true  ]]
	then
		#  --- Setup the Environment of the current day
		set_variables 
		
		#  --- Check Settings
		# Postprocessor: we only terminate the module
		if [[ $(common.check.preconditions) == false  ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met, we exit this module."
			common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		cd ${CXR_ASCII_OUTPUT_DIR} || return $CXR_RET_ERROR
		
		# We loop through all the grids
		# Therefore we let seq create the numbers from 1 to ${CXR_NUMBER_OF_GRIDS}
		for iGrid in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
		do
			# Loop through the name of all the input arrays
			for i_input_arr in $(seq 0 $(( ${#CXR_INPUT_ARRAYS[@]} - 1 )))
			do
				var=${CXR_INPUT_ARRAYS[$i_input_arr]}
				var_name=${CXR_NAME_ARRAYS[$i_input_arr]}
			
				# Only if the link is not there, create a new one
				current_file=$(eval "echo \${${var}[${iGrid}]}")
				current_base=$(eval "echo \${${var_name}[${iGrid}]}")
				
				if [[ "$CXR_DRY" == "false"  ]]
				then
					
					main.log "Linking ${current_base} to ${current_file}..."
					
					# Test if there is already a link, f so force recreation
					# This must be done because of compressed files (temporary destinations change)
					if [[ -L ${current_base} ]]
					then
						# Link exists - force new link
						ln -s -f ${current_file} ${current_base}
					else
						# There is is no link yet, but there might already be a file
						if [[ ! -f ${current_base} ]]
						then
							# no file - go ahead
							ln -s ${current_file} ${current_base}
						else
							# There was a file, log it
							main.log -v "Will not create a link to ${current_base} - its a file"
						fi
					fi
				else
					main.log   "This is a dry run, do not create a link for output processing"
				fi

			done
		done
		
		cd ${CXR_RUN_DIR}  || return $CXR_RET_ERROR

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