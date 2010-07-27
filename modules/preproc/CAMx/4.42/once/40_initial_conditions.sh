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
# ${CXR_DEP_ALL_ONCE_PRE} - all pre_start_preprocessors must have finished
# ${CXR_DEP_ALL_DAILY_PRE} - all daily_preprocessors must have finished
# ${CXR_DEP_ALL_MODEL} - all model modules must have finished
# ${CXR_DEP_ALL_DAILY_POST} - all daily_postprocessors must have finished
# ${CXR_DEP_ALL_ONCE_POST} - all finish_postprocessors must have finished

# the predicate "-"refers to the previous model day, so ${CXR_DEP_ALL_MODEL}- means that all model modules of the previous day must be successful. The predicate "+" means that this module must have run for all days, so extract_station_data+ means that extract_station_data ran for all days. (Usually only useful in One-Time Postprocessors)

CXR_META_MODULE_DEPENDS_ON="convert_meteo"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Generates a file containing the initial concentrations in the coarse grid"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_ONCE}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=1

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|idl"


# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2009), CAMxRunner@psi.ch"

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
# Function: set_variables
#	
# Sets the appropriate variables needed for <initial_conditions>
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
	CXR_CHECK_THESE_INPUT_FILES="$CXR_IC_PROC_INPUT_FILE"
	
	########################################################################
	# Set variables
	########################################################################
	
	NLEV=${CXR_NUMBER_OF_LAYERS[1]}
	
	# The date needed by this function is a bit strange
	# It needs a 2-digit yoer and a 3-digit DOY
	IBDATE="${CXR_YEAR_S}$(common.date.DayOfYear ${CXR_START_DATE} 3 )"
	
	# Evaluate some rules
	
	# Final output files
	# Output files must not be decompressed!
	CXR_TOPCONC_OUTPUT_FILE="$(common.runner.evaluateRule "$CXR_TOP_CONCENTRATIONS_FILE_RULE" false CXR_TOP_CONCENTRATIONS_FILE_RULE false)"
	CXR_IC_OUTPUT_FILE="$(common.runner.evaluateRule "$CXR_INITIAL_CONDITIONS_FILE_RULE" false CXR_INITIAL_CONDITIONS_FILE_RULE false)"
	
	# The processor creates this intermediate file (must not be checked)
	CXR_IC_ASC_OUTPUT_FILE="${CXR_IC_OUTPUT_FILE}.${CXR_ASC_EXT}"
	
	# CXR_CHECK_THESE_OUTPUT_FILES is a space separated list of output files to check
	CXR_CHECK_THESE_OUTPUT_FILES="$CXR_IC_OUTPUT_FILE $CXR_TOPCONC_OUTPUT_FILE"

	# ICBCPREP needs no input files
	if [[ "${CXR_IC_BC_TC_METHOD}" != ICBCPREP  ]]
	then 
		# All MOZART-flavors need Input
		
		# We need a MOZART file as input
		CXR_MOZART_INPUT_FILE="$(common.runner.evaluateRule "$CXR_GLOBAL_CTM_FILE_RULE" false CXR_GLOBAL_CTM_FILE_RULE)"
	
		# Also, we need a domain 1 meteo file
		CXR_IGRID=1
		CXR_METEO_INPUT_FILE="$(common.runner.evaluateRule "$CXR_MMOUT_FILE_RULE" false CXR_MMOUT_FILE_RULE)"
		
		# And finally the ZP file
		CXR_ZP_INPUT_FILE="$(common.runner.evaluateRule "$CXR_PRESSURE_ASC_FILE_RULE" false CXR_PRESSURE_ASC_FILE_RULE)"
	
		# space separated list of input files to check
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_METEO_INPUT_FILE $CXR_ZP_INPUT_FILE $CXR_MOZART_INPUT_FILE"
	
	else
		#ICBCPREP needs a filename for the BC file it will create (the one for the first day)
		# Output files must not be decompressed!
		CXR_BC_OUTPUT_FILE="$(common.runner.evaluateRule "$CXR_BOUNDARY_CONDITIONS_FILE_RULE" false CXR_BOUNDARY_CONDITIONS_FILE_RULE false)"
		CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES $CXR_BC_OUTPUT_FILE"
	fi
	
}

################################################################################
# Function: create_topconc_file
#
# If we are using constant values, we need to create a topcnc file first.
# This function does this by reading the array CXR_IC_BC_TC_SPEC
#
################################################################################
function create_topconc_file() 
################################################################################
{
	# Define & Initialize local vars
	local spec_line
	local species
	local conc

	# Clean file First
	: > "$CXR_TOPCONC_OUTPUT_FILE"

	for spec_line in ${CXR_IC_BC_TC_SPEC[@]}
	do
		# Each line looks something like
		# O3:0.074740447 
		
		if [[ "$spec_line"  ]]
		then
			# Make sure its uppercase
			species=$(common.string.toUpper $(echo $spec_line | cut -d: -f1))
			conc=$(echo $spec_line | cut -d: -f2)
			
			# Format should be (a10,f10.7), e. g. 
			# NO        .000000049
			printf '%-10s' $species >> "$CXR_TOPCONC_OUTPUT_FILE"
			printf '%10.7f' $conc >> "$CXR_TOPCONC_OUTPUT_FILE"
			
			# Next line
			printf "\n" >> "$CXR_TOPCONC_OUTPUT_FILE"
		fi
	done
}

################################################################################
# Function: initial_conditions
#	
# Converts emissions for a given day
################################################################################
function initial_conditions() 
################################################################################
{
	# We do not need this variable here (exept implicit for the stage name)
	CXR_INVOCATION=${1:-1}
	
	# Define & Initialize local vars
	local extra
	local spec_line
	local species
	local conc
	local exec_tmp_file
	local mozart_array
	local camx_array
	local mozart_spec
	local camx_spec
	local iMapping
	
	#Was this stage already completed?
	if [[ $(common.state.storeState ${CXR_STATE_START}) == true  ]]
	then
		#  --- Setup the Environment
		set_variables 
		
		#  --- Check Settings
		if [[ "$(common.check.preconditions)" == false ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
			common.state.storeState ${CXR_STATE_ERROR}
			
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		if [[ ! -f "${CXR_IC_OUTPUT_FILE}"  ]]
		then
			# Output File does not exist - good.
			
			# Increase global indent level
			main.increaseLogIndent
			
			main.log   "Preparing INITIAL CONDITIONS and TOPCONC data using method ${CXR_IC_BC_TC_METHOD}..."
	
			# What method is wanted?
			case "${CXR_IC_BC_TC_METHOD}" in
			
				MOZART | MOZART_CONSTANT | MOZART_INCREMENT )
				
					# By default, we pass no extra
					local extra
					extra=""
					
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
							
							if [[ "$spec_line"  ]]
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
					exec_tmp_file=$(common.runner.createTempFile $FUNCNAME)
					
					# Go there
					cd $(dirname ${CXR_IC_PROC_INPUT_FILE}) || return $CXR_RET_ERROR
					
					
					# First of all, we need to create the 2 arrays or mozart and CAMx species
					# that we pass to the procedure
					
					# Open brackets
					mozart_array="["
					camx_array="["
					
					for iMapping in $(seq 0 $(( $CXR_NUM_MOZART_SPECIES - 1 )))
					do
						
						mozart_spec=$(echo ${CXR_CAMX_MOZART_MAPPING[$iMapping]} | cut -d: -f2)
						camx_spec=$(echo ${CXR_CAMX_MOZART_MAPPING[$iMapping]} | cut -d: -f1)
						
						mozart_array="${mozart_array}'${mozart_spec}',"
						camx_array="${camx_array}'${camx_spec}',"
		
					done
					
					# Close brackets and remove last ","
					mozart_array="${mozart_array%,}]"
					camx_array="${camx_array%,}]"
					
					# interface:
					# fmoz,fln,mm5camxinfile,outfile_bc,nlevs,nspec,note,xorg,yorg,delx,dely,ibdate,extra
					# we need to multiply the resolution by 1000 (metre) 
					
					# Create the file to run IDL
					
					cat <<-EOF > $exec_tmp_file
					.run $(basename ${CXR_IC_PROC_INPUT_FILE})
			
					$(basename ${CXR_IC_PROC_INPUT_FILE} .pro),'${CXR_MOZART_INPUT_FILE}','${CXR_METEO_INPUT_FILE}','${CXR_ZP_INPUT_FILE}','${CXR_IC_ASC_OUTPUT_FILE}','${CXR_TOPCONC_OUTPUT_FILE}',$NLEV,$mozart_array,$camx_array,'${CXR_RUN}',$CXR_MASTER_ORIGIN_XCOORD,$CXR_MASTER_ORIGIN_YCOORD,$(common.math.FloatOperation "$CXR_MASTER_CELL_XSIZE * 1000"),$(common.math.FloatOperation "$CXR_MASTER_CELL_YSIZE * 1000"),'$IBDATE'$extra
					exit
					EOF
					
					# Get a copy of the call
					cat ${exec_tmp_file} | tee -a $CXR_LOG
		
					if [[ "$CXR_DRY" == false  ]]
					then
						
						# Then we run it, while preserving the output
						${CXR_IDL_EXEC} < ${exec_tmp_file} 2>&1 | tee -a $CXR_LOG
						
						# Now we need to convert the file to binary format
						"${CXR_AIRCONV_EXEC}" ${CXR_IC_ASC_OUTPUT_FILE} ${CXR_IC_OUTPUT_FILE} AIRQUALITY 0 2>&1 | tee -a $CXR_LOG
						
					else
						main.log   "This is a dry-run, will not run the program"
					fi
			
					# Get back
					cd ${CXR_RUN_DIR} || main.dieGracefully "Could not change back to ${CXR_RUN_DIR}"
			
					# Decrease global indent level
					main.decreaseLogIndent
				;;
				
				ICBCPREP )
				
					main.log -w   "Preparing INITIAL CONDITIONS and TOPCONC data using CONSTANT data..."
					
					if [[ "$CXR_DRY" == false  ]]
					then
						# We need a topconc file First
						create_topconc_file
						
						# We need the grid resolution in m
						local master_cell_dx_m
						local master_cell_dy_m
						
						master_cell_dx_m=$(common.math.FloatOperation "${CXR_MASTER_CELL_XSIZE} * 1000")
						master_cell_dy_m=$(common.math.FloatOperation "${CXR_MASTER_CELL_YSIZE} * 1000")
						
						# Is topconc non-empty?
						if [[ -s "${CXR_TOPCONC_OUTPUT_FILE}"  ]]
						then
							# OK, we can now call ICBCPREP
							# We create one file that is good until the end of the simulation
							# Therefore, we fix the enddate
							"$CXR_ICBCPREP_EXEC" <<-EOF
							topcon   |${CXR_TOPCONC_OUTPUT_FILE}
							ic file  |${CXR_IC_OUTPUT_FILE}
							ic messag|${CXR_RUN}-CONSTANT
							bc file  |${CXR_BC_OUTPUT_FILE}
							bc messag|${CXR_RUN}-CONSTANT
							nx,ny,nz |${CXR_MASTER_GRID_COLUMNS},${CXR_MASTER_GRID_ROWS},${CXR_NUMBER_OF_LAYERS[1]}
							x,y,dx,dy|${CXR_MASTER_ORIGIN_XCOORD},${CXR_MASTER_ORIGIN_YCOORD},${master_cell_dx_m},${master_cell_dy_m}
							iutm     |${CXR_UTM_ZONE}
							st date  |${CXR_YEAR_S}${CXR_DOY},0
							end date |${CXR_YEAR_S}$(( ${CXR_DOY} + ${CXR_NUMBER_OF_SIM_DAYS} - 1 )),24
							EOF
						else
							main.dieGracefully "could not create the topconc file ${CXR_TOPCONC_OUTPUT_FILE}"
						fi
					else
						main.log   "This is a dry-run, will not run the program"
					fi
					
				
				;;
				
			esac
	
			# Check if all went well
			if [[ $(common.check.postconditions) == false  ]]
			then
				main.log -a "Postconditions for ${CXR_META_MODULE_NAME} are not met!"
				common.state.storeState ${CXR_STATE_ERROR}
				
				# We notify the caller of the problem
				return $CXR_RET_ERR_POSTCONDITIONS
			fi
			
		else
			# File exists. That is generally bad,
			# unless user wants to skip
			if [[ "$CXR_SKIP_EXISTING" == true  ]]
			then
				# Skip it
				main.log -w   "File $CXR_IC_OUTPUT_FILE exists - because of CXR_SKIP_EXISTING, file will skipped."
				common.state.storeState ${CXR_STATE_STOP} > /dev/null
				return $CXR_RET_OK
			else
				# Fail!
				main.log -e  "File $CXR_IC_OUTPUT_FILE exists - to force the re-creation run ${CXR_CALL} -F"
				common.state.storeState ${CXR_STATE_ERROR}
				return $CXR_RET_ERROR
			fi
		fi

		# Store the state
		common.state.storeState ${CXR_STATE_STOP} > /dev/null
	else
		main.log  "Stage $(common.state.getStageName) was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
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
	initial_conditions
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.fs.isNotEmpty? ${CXR_IC_OUTPUT_FILE}) true "initial_conditions simple existence check, inspect ${CXR_IC_OUTPUT_FILE}"
	
	
	########################################
	# teardown tests if needed
	########################################
	
	
}

