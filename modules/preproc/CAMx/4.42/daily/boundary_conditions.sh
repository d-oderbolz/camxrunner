# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
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
# ${CXR_TYPE_PREPROCESS_ONCE} - all pre_start_preprocessors must have finished
# ${CXR_TYPE_PREPROCESS_DAILY} - all daily_preprocessors must have finished
# ${CXR_TYPE_MODEL} - all model modules must have finished
# ${CXR_TYPE_POSTPROCESS_DAILY} - all daily_postprocessors must have finished
# ${CXR_TYPE_POSTPROCESS_ONCE} - all finish_postprocessors must have finished

# the predicate "-<n>" refers to some previous model day, so ${CXR_TYPE_MODEL}-1 means that all model modules of the previous day must be successful before this module may run. 

CXR_META_MODULE_DEPENDS_ON="convert_meteo"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Generates a file containing the boundary concentrations at the 4 faces of the coarse grid"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_DAILY}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=1

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|idl"

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
	local grid
	local x
	local y
	local z
	
	# The Problem size here is not a function of the invocation
	# It is the area of the 4 lateral boundaries
	grid=1
	
	x=$(common.runner.getX ${grid})
	y=$(common.runner.getY ${grid})
	z=$(common.runner.getZ ${grid})
	
	echo $(( (2 * $x * $z) + (2 * $y * $z) ))
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
	# It needs a 2-digit year and a 3-digit DOY
	IBDATE="${CXR_YEAR_S}$(common.date.DayOfYear ${CXR_DATE} 3 )"
	
	# Evaluate some rules
	
	# Final output files
	# Output files must not be decompressed!
	CXR_BC_OUTPUT_FILE="$(common.runner.evaluateRule "$CXR_BOUNDARY_CONDITIONS_FILE_RULE" false CXR_BOUNDARY_CONDITIONS_FILE_RULE false)"
	
	# The processor creates this intermediate file (must not be checked)
	CXR_BC_ASC_OUTPUT_FILE="${CXR_BC_OUTPUT_FILE}.${CXR_ASC_EXT}"
	
	# CXR_CHECK_THESE_OUTPUT_FILES is a space separated list of output files to check
	CXR_CHECK_THESE_OUTPUT_FILES="$CXR_BC_OUTPUT_FILE "
	
	# ICBCPREP needs no input files, except the frst BC file
	if [[ "${CXR_IC_BC_TC_METHOD}" != ICBCPREP ]]
	then 
		# All MOZART-flavors need Input
		
		# We need a MOZART file as input
		CXR_MOZART_INPUT_FILE="$(common.runner.evaluateRule "$CXR_GLOBAL_CTM_FILE_RULE" false CXR_GLOBAL_CTM_FILE_RULE)"
	
		# Also, we need a domain 1 meteo file
		CXR_IGRID=1
		CXR_METEO_INPUT_FILE="$(common.runner.evaluateRule "$CXR_MMOUT_FILE_RULE" false CXR_MMOUT_FILE_RULE)"
		
		# And finally the ZP file
		CXR_ZP_INPUT_FILE="$(common.runner.evaluateRule "$CXR_PRESSURE_ASC_FILE_RULE" false CXR_PRESSURE_ASC_FILE_RULE)"
	
		# a space separated list of input files to check
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_MOZART_INPUT_FILE $CXR_METEO_INPUT_FILE $CXR_ZP_INPUT_FILE"
	else
	
		# ICBCPREP needs to know the filename of the first BC file
		CXR_FIRST_BC_FILE="$(common.runner.evaluateRuleAtDayOffset "$CXR_BOUNDARY_CONDITIONS_FILE_RULE" 0 false CXR_BOUNDARY_CONDITIONS_FILE_RULE)"
		
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
	# We do not need this variable here (exept implicit for the stage name)
	CXR_INVOCATION=${1:-1}
	
	# Define & Initialize local vars
	local extra
	local species
	local conc
	local spec_line
	local exec_tmp_file
	local iSpec
	local mozart_array
	local camx_array
	local mozart_spec
	local camx_spec
	local dx
	local dy
	
	iSpec=0

	#Was this stage already completed?
	if [[ $(common.state.storeStatus ${CXR_STATUS_RUNNING}) == true  ]]
	then
		#  --- Setup the Environment
		set_variables 
		
		#  --- Check Settings
		if [[ $(common.check.preconditions) == false  ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
			common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		if [[ ! -s "${CXR_BC_OUTPUT_FILE}"  ]]
		then
			# File does not exist
		
			# Increase global indent level
			main.increaseLogIndent
	
			main.log   "Preparing BOUNDARY CONDITIONS data..."
			
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
							
							# This is for later:
							# or it may specify layers (1 entry per layer needed!)
							# O3:[0.0500, 0.0508, 0.0515, 0.0523, 0.0531, 0.0538, 0.0546, 0.0554, 0.0562, 0.0569, 0.0577, 0.0585, 0.0592, 0.0600]
							# or even specify layers for each face (WST,EST,STH,NTH)
							# O3:[[0.0500, 0.0508, 0.0515, 0.0523, 0.0531, 0.0538, 0.0546, 0.0554, 0.0562, 0.0569, 0.0577, 0.0585, 0.0592, 0.0600],[0.0500, 0.0508, 0.0515, 0.0523, 0.0531, 0.0538, 0.0546, 0.0554, 0.0562, 0.0569, 0.0577, 0.0585, 0.0592, 0.0600],[0.0500, 0.0508, 0.0515, 0.0523, 0.0531, 0.0538, 0.0546, 0.0554, 0.0562, 0.0569, 0.0577, 0.0585, 0.0592, 0.0600],[0.0500, 0.0508, 0.0515, 0.0523, 0.0531, 0.0538, 0.0546, 0.0554, 0.0562, 0.0569, 0.0577, 0.0585, 0.0592, 0.0600]]
							if [[ "$spec_line"  ]]
							then
								# Make sure its uppercase
								species=$(common.string.toUpper $(echo $spec_line | cut -d: -f1))
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
							
							if [[ "$spec_line" ]]
							then
								# Make sure its uppercase
								species=$(common.string.toUpper $(echo $spec_line | cut -d: -f1))
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
					exec_tmp_file=$(common.runner.createJobFile $FUNCNAME)
					
					# Go there
					cd $(dirname ${CXR_BC_PROC_INPUT_FILE}) || return $CXR_RET_ERROR
					
					
					# First of all, we need to create the 2 arrays or mozart and CAMx species
					# that we pass to the procedure
					
					# Open brackets
					mozart_array="["
					camx_array="["
					
					for iSpec in $(seq 0 $(( $CXR_NUMBER_OF_GCTM_SPECIES - 1 )))
					do
						
						mozart_spec=$(echo ${CXR_CAMX_MOZART_MAPPING[$iSpec]} | cut -d: -f2)
						camx_spec=$(echo ${CXR_CAMX_MOZART_MAPPING[$iSpec]} | cut -d: -f1)
						
						mozart_array="${mozart_array}'${mozart_spec}',"
						camx_array="${camx_array}'${camx_spec}',"
		
					done
					
					# Close brackets and remove last ","
					mozart_array="${mozart_array%,}]"
					camx_array="${camx_array%,}]"
		
					# Create the file to run IDL
					
					# we need to multiply the resolution by 1000 if its in km
					case $CXR_MAP_PROJECTION in
					
						LATLON) dx="$CXR_MASTER_CELL_XSIZE"
						        dy="$CXR_MASTER_CELL_YSIZE";;
						                   
						LAMBERT|POLAR|UTM) dx=$(common.math.FortranFloatOperationFloatOperation "$CXR_MASTER_CELL_XSIZE * 1000")
						                   dy=$(common.math.FortranFloatOperationFloatOperation "$CXR_MASTER_CELL_YSIZE * 1000");;
					
					esac
					
					cat <<-EOF > $exec_tmp_file
					.run $(basename ${CXR_BC_PROC_INPUT_FILE})
					$(basename ${CXR_BC_PROC_INPUT_FILE} .pro),'${CXR_MOZART_INPUT_FILE}','${CXR_METEO_INPUT_FILE}','${CXR_MET_MODEL}','${CXR_ZP_INPUT_FILE}','${CXR_BC_ASC_OUTPUT_FILE}',$NLEV,$mozart_array,$camx_array,'${CXR_RUN}',$CXR_MASTER_ORIGIN_XCOORD,$CXR_MASTER_ORIGIN_YCOORD,$dx,$dy,'$IBDATE',$(common.math.convertBoolean ${CXR_IC_BC_TC_DO_PLOT:-false}),'$CXR_IC_BC_TC_PLOT_BASE_DIR',$CXR_IC_BC_TC_PLOT_TIME,'${CXR_RUN}',$(common.math.convertBoolean ${CXR_IC_BC_TC_DO_PNG:-false}),$(common.math.convertBoolean ${CXR_IC_BC_TC_RM_PS:-false})${extra}
					exit
					EOF
						
					if [[ "$CXR_DRY" == false  ]]
					then
						
						# Then we run it, while preserving the output
						${CXR_IDL_EXEC} < ${exec_tmp_file} 2>&1 | tee -a $CXR_LOG
						
						# Now we need to convert the file to binary format
						main.log -a "${CXR_AIRCONV_EXEC} ${CXR_BC_ASC_OUTPUT_FILE} ${CXR_BC_OUTPUT_FILE} BOUNDARY 0 2>&1"
						"${CXR_AIRCONV_EXEC}" ${CXR_BC_ASC_OUTPUT_FILE} ${CXR_BC_OUTPUT_FILE} BOUNDARY 0 2>&1 | tee -a $CXR_LOG
						
					else
						main.log   "This is a dry-run, will not run program"
					fi
			
					# go back
					cd ${CXR_RUN_DIR} || main.dieGracefully "Could not change back to ${CXR_RUN_DIR}"
			
					# Decrease global indent level
					main.decreaseLogIndent
			
				;;
				
				ICBCPREP )
				
					main.log -w   "Preparing BOUNDARY CONDITIONS data using CONSTANT data..."
					# We use the topconc file that was created by initial_conditions
					# And we link to the BC file that was created there.
				
					# First cd there
					cd "$(dirname "${CXR_FIRST_BC_FILE}")" || common.state.storeStatus ${CXR_STATUS_FAILURE}
				
					if [[ "$CXR_DRY" == false  ]]
					then
						# We only create a file the first day, all others we link
						if [[ "$(common.date.isFirstDayOfSimulation?)" == false  ]]
						then
							# Not the first day, just link
							ln -s "$(basename "${CXR_FIRST_BC_FILE}")" "$(basename "${CXR_BC_OUTPUT_FILE}")"
						fi
					else
						main.log   "This is a dry-run, no action required"
					fi
					
					# go back
					cd ${CXR_RUN_DIR} || main.dieGracefully "Could not change back to ${CXR_RUN_DIR}"
				;;
				
			esac
	
			# Check if all went well
			if [[ "$(common.check.postconditions)" == false  ]]
			then
				main.log -a "Postconditions for ${CXR_META_MODULE_NAME} are not met!"
				common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
			
				# We notify the caller of the problem
				return $CXR_RET_ERR_POSTCONDITIONS
			fi

		else
			# File exists. That is generally bad,
			# unless user wants to skip
			if [[ "$CXR_SKIP_EXISTING" == true  ]]
			then
				# Skip it
				main.log -w   "File $CXR_BC_OUTPUT_FILE exists - because of CXR_SKIP_EXISTING, file will skipped."
			else
				# Fail!
				main.log -e  "File $CXR_BC_OUTPUT_FILE exists - to force the re-creation run ${CXR_CALL} -F"
				common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
				return $CXR_RET_ERROR
			fi
		fi
		
		# We are fine.
		common.state.storeStatus ${CXR_STATUS_SUCCESS} > /dev/null
		
	else
		main.log  "Stage $(common.task.getId) was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
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
	########################################
	# Setup tests if needed
	########################################
	
	# Initialise the date variables for first day
	day_offset=0
	common.date.setVars "$CXR_START_DATE" "$day_offset"
	set_variables

	# For this module, testing is harder 
	# compared to date_functions because we cannot just compare
	# Expected with actual results
	boundary_conditions
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.fs.isNotEmpty? ${CXR_BC_OUTPUT_FILE}) true "boundary_conditions simple existence check, inspect ${CXR_BC_OUTPUT_FILE}"
	
	########################################
	# teardown tests if needed
	########################################
	
}



