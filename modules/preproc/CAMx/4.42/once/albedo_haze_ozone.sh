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

CXR_META_MODULE_DEPENDS_ON="convert_landuse"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Runs AHOMAP to generate the Albedo/haze/ozone input file"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_ONCE}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=1

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|wget"

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
	# This depends on the interval
	case "${CXR_RUN_AHOMAP_TUV_INTERVAL:-once}" in
	
		once )
			# Just once
			echo 1
			;;
			
		daily )
			# One for each day
			echo $CXR_NUMBER_OF_SIM_DAYS
			;;
			
		weekly )
			# One for each unique week
			common.date.WeeksBetween $CXR_START_DATE $CXR_STOP_DATE
			;;
			
		
		monthly )
			# One for each unique week
			common.date.MonthsBetween $CXR_START_DATE $CXR_STOP_DATE
			;;
	
		*)
			main.dieGracefully "Unknown interval for AHOMAP in variable CXR_RUN_AHOMAP_TUV_INTERVAL, we suport once,daily,weekly or monthly! Exiting." 
			;;
	esac
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
# Sets the appropriate variables needed for <albedo_haze_ozone>
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
	
	# Evaluate some rules
	# Output files must not be decompressed!
	CXR_AHOMAP_OUTPUT_FILE="$(common.runner.evaluateRule "$CXR_ALBEDO_HAZE_OZONE_FILE_RULE" false CXR_ALBEDO_HAZE_OZONE_FILE_RULE false)"

	# CXR_CHECK_THESE_OUTPUT_FILES is a space separated list of output files to check
	CXR_CHECK_THESE_OUTPUT_FILES="$CXR_AHOMAP_OUTPUT_FILE"

	# Grid specific
	for CXR_IGRID in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
	do
		# Landuse Files
		CXR_LANDUSE_INPUT_ARR_FILES[${CXR_IGRID}]="$(common.runner.evaluateRule "$CXR_LANDUSE_FILE_RULE" false CXR_LANDUSE_FILE_RULE)"
	
		#Check
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_LANDUSE_INPUT_ARR_FILES[${CXR_IGRID}]}"
	done
}


################################################################################
# Function: create_ahomap_control_file
#	
# Creates a AHOMAP Control file that can be fed to the program.
#
# Also downloads needed satellite data.
#
# Such a file has this structure (examplo from sample job file):
#> Coordinate project |LAMBERT
#> xorg,yrog,clon,clat|-2000,-1700,-100,40,60,30     (km,km,deg,deg,deg,deg)
#> true1,true2
#>dx,dy              |16.,16.
#>Output filename    |ahomap.test
#>Process for snow?  |.true.
#>Number of grids    |1
#>Landuse filename   |camx.landuse
#>nx,ny              |95,83
#>Numbr of TOMS files|28
#>Bday,Eday,TOMS file|930815,930815,ozcol_data/ga930815.m3t
#>Bday,Eday,TOMS file|930816,930816,ozcol_data/ga930816.m3t
#>...
#
# Explanation by ENVIRON:
#>Lines 1 & 2: Coordiniate projection and master/coarse grid location definition
#>Coordinate project |UTM
#>xorg,yorg,izone    |-316.,2808.,15                (km,km,unitless)
#>or
#>Coordinate project |LAMBERT
#>xorg,yrog,clon,clat|-2000,-1700,-100,40,60,30     (km,km,deg,deg,deg,deg)
#>true1,true2
#>or
#>Coordinate project |LATLON
#>xorg,yorg          |30,-120                       (deg,deg)
#>or 
#>Coordinate project |POLAR
#>xorg,yorg,plon,plat|-100,-200,-100,40             (km,km,deg,deg)
#>
#>Line 3: Master/coarse grid cell size (km)
#>dx,dy              |16,16
#>
#>Line 4: Output file name
#>Output filename    |ahomap.test
#>
#>Line 5: Snow flag (T=5th albedo bin set for snow, F=no snow albedo included)
#>Process for snow?  |.true.
#>
#>Line 6: Number of grids to process
#>Number of grids    |1
#>
#>Lines 7 and 8: Landuse filename and domain size (number of grid cells)
#>               List the master/coarse grid first, then any/all fine grids
#>               Repeat n times
#>Landuse file       |camx.landuse
#>nx, ny             |95,83
#>
#>Line 9: Number of TOMS input data files
#>Number of TOMS file|28
#>
#>Lines 10+: Begin date, End date,and the Ozone data file name.
#>           Requires begin date (YYMMDD) and end date (YYMMDD) for ozone 
#>           column data.
#>
#>Example: for August 15, 1993 daily ozone data, 
#>Bday,Eday,TOMS file|930815,930815,ozcol_data/ga930815.m3t
#
# Parameters:
# $1 - a filename of the created control file
# $2 - The start offset of the control file (0 based day offsets)
# $3 - The number of days in the control file
#
# See:
# <write_control_file>
################################################################################
function create_ahomap_control_file()
################################################################################
{
	# Define & Initialize local vars
	local ahomap_file
	local start_offset
	local num_days
	local iGrid
	
	local day_offset
	
	ahomap_file="$1"
	start_offset="$2"
	num_days="$3"
	
	# Write data line by line (analogous to write_control_file)
	
	echo "Coordinate project |$CXR_MAP_PROJECTION" > ${ahomap_file}
	
	# This is dependent on the projection, currently only LAMBERT is tested.
	case $CXR_MAP_PROJECTION in
	
		LAMBERT)
			echo "xorg,yorg,clon,clat|$CXR_MASTER_ORIGIN_XCOORD,$CXR_MASTER_ORIGIN_YCOORD,$CXR_LAMBERT_CENTER_LONGITUDE,$CXR_LAMBERT_CENTER_LATITUDE,$CXR_LAMBERT_TRUE_LATITUDE1,$CXR_LAMBERT_TRUE_LATITUDE2" >> ${ahomap_file}
			;;
		POLAR)
			main.log  "Note that POLAR support of CAMxRunner is limited, be careful!"
			echo "xorg,yorg,plon,plat|$CXR_MASTER_ORIGIN_XCOORD,$CXR_MASTER_ORIGIN_YCOORD,$CXR_POLAR_LONGITUDE_POLE,$CXR_POLAR_LATITUDE_POLE" >> ${ahomap_file}
			;;
		UTM)
			main.log  "Note that UTM support of CAMxRunner is limited, be careful!"
			echo "xorg,yorg,izone    |$CXR_MASTER_ORIGIN_XCOORD,$CXR_MASTER_ORIGIN_YCOORD,$CXR_UTM_ZONE" >> ${ahomap_file}
			;;
		LATLON)
			main.log  "Note that LATLON support of CAMxRunner is limited, be careful!"
			echo "xorg,yorg          |$CXR_MASTER_ORIGIN_XCOORD,$CXR_MASTER_ORIGIN_YCOORD" >> ${ahomap_file}
			;;
	
	esac
	
	echo "dx,dy              |$CXR_MASTER_CELL_XSIZE,$CXR_MASTER_CELL_YSIZE" >> ${ahomap_file}
	echo "Output filename    |$CXR_AHOMAP_OUTPUT_FILE" >> ${ahomap_file}
	echo "Process for snow?  |.$CXR_AHOMAP_SNOW." >> ${ahomap_file}
	echo "Number of grids    |$CXR_NUMBER_OF_GRIDS" >> ${ahomap_file}
	
	# Loop through grids for landuse files
	for iGrid in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
	do
		# Landuse File
		echo "Landuse filename   |${CXR_LANDUSE_INPUT_ARR_FILES[${iGrid}]}" >> ${ahomap_file}
		
		#The corresponding cell dimensions
		X_CURRENT=$(common.runner.getX $iGrid)
		Y_CURRENT=$(common.runner.getY $iGrid)

		echo "nx,ny              |$X_CURRENT,$Y_CURRENT" >> ${ahomap_file}
	done
	
	# We assume that we have always satellite data for all simulated days
	# The spelling is taken literally from ENVIRON (important because column length matters)
	echo "Numbr of TOMS files|$num_days" >> ${ahomap_file}
	
	# Loop through the day offsets of the simulation (0=Start day)
	for day_offset in $(seq ${start_offset} $(( ${start_offset} + ${num_days} -1 )) )
	do
		# Exports all relevant date variables
		# like CXR_YEAR, CXR_MONTH...
		common.date.setVars "$CXR_START_DATE" "$day_offset"

		# expand rule
		CXR_AHOMAP_OZONE_COLUMN_FILE="$(common.runner.evaluateRule "$CXR_AHOMAP_OZONE_COLUMN_FILE_RULE" false CXR_AHOMAP_OZONE_COLUMN_FILE_RULE)"
		CURRENT_URL="$(common.runner.evaluateRule "$CXR_AHOMAP_OZONE_COLUMN_URL_RULE" false CXR_AHOMAP_OZONE_COLUMN_URL_RULE)"
		
		if [[ ! -s $CXR_AHOMAP_OZONE_COLUMN_DIR/${CXR_AHOMAP_OZONE_COLUMN_FILE}  ]]
		then
			# File does not exist or is empty
		
			if [[ $CXR_DRY == false  ]]
			then
				# Download
				common.install.downloadFile ${CURRENT_URL} $CXR_AHOMAP_OZONE_COLUMN_DIR/${CXR_AHOMAP_OZONE_COLUMN_FILE}
			else
				main.log  "Dryrun, file ${CXR_AHOMAP_OZONE_COLUMN_FILE} not downloaded"
			fi
		else
			main.log  "File ${CXR_AHOMAP_OZONE_COLUMN_FILE} is already in the cache."
		fi
		
		# Write data to file
		echo "Bday,Eday,TOMS file|${CXR_YEAR_S}${CXR_MONTH}${CXR_DAY},${CXR_YEAR_S}${CXR_MONTH}${CXR_DAY},$CXR_AHOMAP_OZONE_COLUMN_DIR/${CXR_AHOMAP_OZONE_COLUMN_FILE}" >> ${ahomap_file}
	done
	
	# Reset date variables for first day
	common.date.setVars "$CXR_START_DATE" "0"
	
	main.log  "I just wrote a control file for AHOMAP to ${ahomap_file}."

	# Return the file name
	echo ${ahomap_file}
}

################################################################################
# Function: albedo_haze_ozone
#	
# Creates the AHOMAP input file(s). Depending on the invocation number,
# the correct amount of days are grouped and then AHOMAP is run over them.
#
# See:
# <create_ahomap_control_file>
################################################################################
function albedo_haze_ozone() 
################################################################################
{
	# We set the invocation (we die if not passed)
	# In this module, CXR_INVOCATION must be mapped to a part of the workload
	CXR_INVOCATION=${1}
	
	# Define & Initialize local vars
	local day_offset
	local num_days
	local iMonth
	
	day_offset=0
	num_days=0
	
	local ahomap_control_file
	
	#Was this stage already completed?
	if [[ "$(common.state.storeStatus ${CXR_STATUS_RUNNING})" == true  ]]
	then
	
		# Initialize data for this file
		case "${CXR_RUN_AHOMAP_TUV_INTERVAL:-once}" in
		
			once )
				# This is independent of the invocation
				day_offset=0
				common.date.setVars "$CXR_START_DATE" "$day_offset"
				num_days=${CXR_NUMBER_OF_SIM_DAYS}
				
				main.log -b "Running AHOMAP for whole period..."
				;;
				
			daily )
				# In this case, the invocation is the day-offset plus 1
				day_offset=$(( $CXR_INVOCATION - 1 ))
				common.date.setVars "$CXR_START_DATE" "$day_offset"
				num_days=1
				main.log -b "Running AHOMAP for $CXR_DATE..."
				;;
				
			weekly )
				# Here, the invocation is the week offset since start
				# We need to find the start (Monday) of the current offset week
				if [[ $CXR_INVOCATION -eq 1 ]]
				then
					# we are at the first day
					day_offset=0
				else
					# we are at some later day
					# note that adding 7 to a day_offset moves us into the next week.
					# Therefore we subtract 2 and not 1 from the CXR_INVOCATION
					day_offset=$(( $(common.date.DaysLeftInWeek $CXR_START_DATE true) + 7 * ( $CXR_INVOCATION - 2 ) ))
				fi
				
				# We cannot go beyond the last day
				if [[ $day_offset -gt $(( ${CXR_NUMBER_OF_SIM_DAYS} -1 )) ]]
				then
					day_offset=$(( ${CXR_NUMBER_OF_SIM_DAYS} -1 ))
				fi
				
				common.date.setVars "$CXR_START_DATE" "$day_offset"
				
				# That many days remain in the current week (considering end of simulation)
				num_days=$(common.date.DaysLeftInWeek $CXR_DATE true)
				
				main.log -b "Running AHOMAP for week $CXR_WOY ( $num_days days starting at offset $day_offset )..."

				;;
			
			monthly )
			
				# Here, the invocation is the month offset since start
				# We need to find the 1st of the current offset month
				if [[ $CXR_INVOCATION -eq 1 ]]
				then
					# we are at the first day
					day_offset=0
				else
					# We start at day 0
					day_offset=0
				
					# we are at some later moth. iMonth is the month offset compared to the start month
					# Note that adding the lenght of a month to a day_offset moves us into the next month.
					# Therefore we subtract 2
					for iMonth in $(seq 0 $(( $CXR_INVOCATION - 2)) )
					do
						common.date.setVars "$CXR_START_DATE" "$day_offset"
						day_offset=$(( $day_offset + $(common.date.DaysLeftInMonth $CXR_DATE) ))
					done
					
				fi
				
				# We cannot go beyond the last day
				if [[ $day_offset -gt $(( ${CXR_NUMBER_OF_SIM_DAYS} -1 )) ]]
				then
					day_offset=$(( ${CXR_NUMBER_OF_SIM_DAYS} -1 ))
				fi
				
				common.date.setVars "$CXR_START_DATE" "$day_offset"
				
				num_days=$(common.date.DaysLeftInMonth $CXR_DATE true)
				
				main.log -b "Running AHOMAP for month $CXR_MONTH ( $num_days days  starting at offset $day_offset )..."
				
				;;
		
			*)
				main.dieGracefully "Unknown interval for AHOMAP in variable CXR_RUN_AHOMAP_TUV_INTERVAL, we suport once,daily,weekly or monthly! Exiting." ;;
		esac

		#  --- Setup the Environment
		set_variables 
		
		#  --- Check Settings
		if [[ "$(common.check.preconditions)" == false ]]
		then
			main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
			common.state.storeStatus ${CXR_STATUS_FAILURE} > /dev/null
		
			# We notify the caller of the problem
			return $CXR_RET_ERR_PRECONDITIONS
		fi
		
		# Increase global indent level
		main.increaseLogIndent

		main.log  "Preparing Albedo/Haze/Ozone data for run ${CXR_RUN}..."
		
		# Is the output there?
		if [[ ! -s "$CXR_AHOMAP_OUTPUT_FILE"  ]]
		then
			# File not yet there
		
			# The options of AHOMAP are a bit 
			# weird, better produce a job file
			ahomap_control_file=$(common.runner.createJobFile AHOMAP)
			
			if [[ "$CXR_DRY" == false  ]]
			then
			
				# This function (I know, one should avoid side effects!) 
				# also downloads/caches satellite data
				create_ahomap_control_file "$ahomap_control_file" $day_offset $num_days
					
				# Is the file there and not empty?)
				if [[ -s "${ahomap_control_file}"  ]]
				then
				
					main.log  "Calling AHOMAP - using jobfile ${ahomap_control_file} (be patient)...\n"

					# Call AHOMAP 
					cat ${ahomap_control_file} | tee -a ${CXR_LOG}
					
					${CXR_AHOMAP_EXEC} < ${ahomap_control_file} 2>&1 | tee -a $CXR_LOG
				else
					main.log  "Could not create AHOMAP control file - exiting."
					return $CXR_RET_ERROR
				fi
	
			else
				main.log   "Dryrun - AHOMAP not performed"
			fi
	
			# Decrease global indent level
			main.decreaseLogIndent
	
			# Check if all went well
			if [[ $(common.check.postconditions) == false  ]]
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
				main.log -w  "File $CXR_AHOMAP_OUTPUT_FILE exists - because of CXR_SKIP_EXISTING, file will skipped."
				
				# next iteration
			else
				# Fail!
				main.log -e  "File $CXR_AHOMAP_OUTPUT_FILE exists - to force the re-creation run ${CXR_CALL} -F"
				common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
				return $CXR_RET_ERROR
			fi
		fi
			
		# Store the state
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
	
	albedo_haze_ozone
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.fs.isNotEmpty? ${CXR_AHOMAP_OUTPUT_FILE}) true "albedo_haze_ozone simple existence check, inspect ${CXR_AHOMAP_OUTPUT_FILE}"
	
}
