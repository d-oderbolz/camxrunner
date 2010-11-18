# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
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
CXR_META_MODULE_DESCRIPTION="Converts the binary output to ASCII for aqmfad"

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
	# This module needs one invocation per grid per step
	echo $CXR_NUMBER_OF_GRIDS
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
	local invocation
	local x
	local y
	local z
	
	invocation=$1
	
	x=$(common.runner.getX ${invocation})
	y=$(common.runner.getY ${invocation})
	z=$(common.runner.getZ ${invocation})
	
	# The Problem size here is a function of the invocation
	echo $(( $x * $y * $z ))
}

################################################################################
# Function: set_variables
#	
# Sets the appropriate variables for convert_output
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
	
	# We work in a temp dir. Here we store links to all the input files,
	# wherever they are.
	convert_dir="$(common.runner.createTempDir convert)"
	
	########################################################################
	# Set variables
	########################################################################
	
	# The converter scripts
	CXR_CONVERTERS=(${CXR_AIRASCII_EXEC} ${CXR_UAMVASCII_EXEC} ${CXR_UAMVASCII_EXEC} ${CXR_UAMVASCII_EXEC} ${CXR_UAMVASCII_EXEC} ${CXR_UAMVASCII_EXEC})
	
	# The options to the converter scripts
	CXR_CONVERTER_OPTIONS=(AVERAGE HEIGHT WIND TEMPERATUR H2O VDIFFUSION)
	
	########################################################################
	# per day-per grid settings
	########################################################################
	
	# Grid specific - we need to define CXR_IGRID
	CXR_IGRID=$CXR_INVOCATION
	
	####################################
	# Input
	####################################

	# We allow decompression
	CXR_AVG_INPUT_FILE=$(common.runner.evaluateRule "$CXR_AVG_FILE_RULE" false CXR_AVG_FILE_RULE)
	# Create link
	ln -s -t $convert_dir $CXR_AVG_INPUT_FILE 

	# TERRAIN must not be converted, it is already ASCII.
	
	# Pressure.
	CXR_ZP_GRID_INPUT_FILE=$(common.runner.evaluateRule "$CXR_PRESSURE_FILE_RULE" false CXR_PRESSURE_FILE_RULE)
	# Create link
	ln -s -t $convert_dir $CXR_ZP_GRID_INPUT_FILE 
	
	# Wind
	CXR_WIND_GRID_INPUT_FILE=$(common.runner.evaluateRule "$CXR_WIND_FILE_RULE" false CXR_WIND_FILE_RULE)
	# Create link
	ln -s -t $convert_dir $CXR_WIND_GRID_INPUT_FILE 
	
	# Temperature
	CXR_TEMP_GRID_INPUT_FILE=$(common.runner.evaluateRule "$CXR_TEMPERATURE_FILE_RULE" false CXR_TEMPERATURE_FILE_RULE)
	# Create link
	ln -s -t $convert_dir $CXR_TEMP_GRID_INPUT_FILE 
	
	# Vapor
	CXR_VAPOR_INPUT_FILE=$(common.runner.evaluateRule "$CXR_VAPOR_FILE_RULE" false CXR_VAPOR_FILE_RULE)
	# Create link
	ln -s -t $convert_dir $CXR_VAPOR_INPUT_FILE 
	
	# No Cloud
	# Vertical K
	CXR_KV_GRID_INPUT_FILE=$(common.runner.evaluateRule "$CXR_K_FILE_RULE" false CXR_K_FILE_RULE)
	# Create link
	ln -s -t $convert_dir $CXR_KV_GRID_INPUT_FILE 
	
	# NO Emissions
	
	####################################
	# Output
	####################################
	
	# The ASC Filenames are the output
	# Output files must not be decompressed!
	CXR_AVG_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_AVG_ASC_FILE_RULE" false CXR_AVG_ASC_FILE_RULE false)
	
	# TERRAIN must not be converted
	
	# Pressure - convert_meteo should have done the first grid
	CXR_ZP_GRID_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_PRESSURE_ASC_FILE_RULE" false CXR_PRESSURE_ASC_FILE_RULE false)
	
	# Wind
	CXR_WIND_GRID_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_WIND_ASC_FILE_RULE" false CXR_WIND_ASC_FILE_RULE false)
	# Temperature
	CXR_TEMP_GRID_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_TEMPERATURE_ASC_FILE_RULE" false CXR_TEMPERATURE_ASC_FILE_RULE false)
	# Vapor
	CXR_VAPOR_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_VAPOR_ASC_FILE_RULE" false CXR_VAPOR_ASC_FILE_RULE false)
	# No Cloud
	# Vertical K
	CXR_KV_GRID_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_K_ASC_FILE_RULE" false CXR_K_ASC_FILE_RULE false)
	# NO Emissions

	# Checks for the input (the links we created)
	CXR_CHECK_THESE_INPUT_FILES="$convert_dir/$(basename ${CXR_AVG_INPUT_FILE}) \
								$convert_dir/$(basename ${CXR_ZP_GRID_INPUT_FILE}) \
								$convert_dir/$(basename ${CXR_WIND_GRID_INPUT_FILE}) \
								$convert_dir/$(basename ${CXR_TEMP_GRID_INPUT_FILE}) \
								$convert_dir/$(basename ${CXR_VAPOR_INPUT_FILE}) \
								$convert_dir/$(basename ${CXR_KV_GRID_INPUT_FILE}) "

	# Checks for the output
	CXR_CHECK_THESE_OUTPUT_FILES="${CXR_AVG_OUTPUT_FILE} \
								 ${CXR_ZP_GRID_OUTPUT_FILE} \
								 ${CXR_WIND_GRID_OUTPUT_FILE} \
								 ${CXR_TEMP_GRID_OUTPUT_FILE} \
								 ${CXR_VAPOR_OUTPUT_FILE} \
								 ${CXR_KV_GRID_OUTPUT_FILE} "
	
	# Create "real" arrays 
	CXR_INPUT_FILES=($CXR_CHECK_THESE_INPUT_FILES)
	CXR_OUTPUT_FILES=($CXR_CHECK_THESE_OUTPUT_FILES)
	
}

################################################################################
# Function: convert_output
#	
# Converts the files of the current day to .asc
################################################################################	
function convert_output() 
################################################################################
{
	# Here, the invocation is the grid number
	CXR_INVOCATION=${1}
	
	local iFile
	local xdim
	local ydim
	local zdim
	local input_file
	local output_file
	local converter
	local options
	
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
		
		# The Fortran programs can only handle parameters shorter than 255 Chars
		# Therefore we change to the directory and work with the basenames
		#
		# We convert the whole input of aqmfad
		#
		########################################################################
		# Requires the bin2asc and the airascii patches 	
		########################################################################
		
		cd $convert_dir || return $CXR_RET_ERROR

		main.log  "Working in $convert_dir"

		# We loop through all the grids
		# Therefore we let seq create the numbers from 1 to ${CXR_NUMBER_OF_GRIDS}
		
		# Because there is yet no single converter for CAMx output and 
		# MM5 output, we need to change the converters depending on filetype

		# We have these Arrays, containing
		#	CXR_INPUT_FILES
		#	CXR_OUTPUT_FILES		
		#	CXR_CONVERTERS			: the converter scripts
		#	CXR_CONVERTER_OPTIONS	: the options to the converter scripts
		#
		#	They all have equal length and corresponding elements are at the same index
		#
		#	The uamvascii script also needs x-dim, y-dim and z-dim, as well as a logfile as input. 
		# 	We provide these no matter what program we are running (the logfile is 0=Stdout)
		
		# Determine dimensions
		xdim=$(common.runner.getX ${CXR_INVOCATION})
		ydim=$(common.runner.getY ${CXR_INVOCATION})
		zdim=$(common.runner.getZ ${CXR_INVOCATION})
		
		main.log -v  "Grid $CXR_INVOCATION\nXDIM: $xdim\nYDIM: $ydim\nZDIM: $zdim"

		### Go through all input files
		for iFile in $( seq 0 $(( ${#CXR_INPUT_FILES[@]} - 1)) )
		do
			# Fetch the corresponding in and outfiles
			input_file="${CXR_INPUT_FILES[${iFile}]}"
			output_file="${CXR_OUTPUT_FILES[${iFile}]}"
			
			# and the converters with options
			converter=${CXR_CONVERTERS[${iFile}]}
			options=${CXR_CONVERTER_OPTIONS[${iFile}]}
			
			main.log  "Converting ${input_file} to ${output_file} using ${converter}..."
			
			# Any existing file will be skipped (see comment in header)
			if [[ -s "$output_file"  ]]
			then
				if [[ "${CXR_SKIP_EXISTING}" == true ]]
				then
					# Skip it
					main.log "File ${output_file} exists - file will skipped."
					continue
				else
					main.log -e  "File ${output_file} exists - to force the re-creation run ${CXR_CALL} -F"
					common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
					return $CXR_RET_ERROR
				fi
			fi
			
			main.log "Converting ${input_file} to ${output_file} ..."
		
			main.log -v  "${converter} ${input_file} ${output_file} ${options} ${xdim} ${ydim} ${zdim} /dev/null"

			if [[ "$CXR_DRY" == false ]]
			then
				#Call the converter, collect sterr and stout
				${converter} ${input_file} ${output_file} ${options} ${xdim} ${ydim} ${zdim} /dev/null 2>&1 | tee -a $CXR_LOG
			else
					main.log   "Dryrun, no conversion performed"
			fi
		done # File types

		# go back
		cd ${CXR_RUN_DIR} || main.dieGracefully "Could not change back to ${CXR_RUN_DIR}"

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