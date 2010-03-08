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

CXR_META_MODULE_DEPENDS_ON=""

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=false

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Runs AHOMAP to generate the Albedo/haze/ozone input file"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_PREPROCESS_ONCE}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=1

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=CAMx-v4.51-co5-s160-sem063-run1

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|wget"

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
	for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
	do
		# Landuse Files
		CXR_LANDUSE_INPUT_ARR_FILES[${i}]="$(common.runner.evaluateRule "$CXR_LANDUSE_FILE_RULE" false CXR_LANDUSE_FILE_RULE)"
	
		#Check
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_LANDUSE_INPUT_ARR_FILES[${i}]}"
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
	local ahomap_file="$1"
	local start_offset="$2"
	local num_days="$3"
	
	local day_offset
	
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
	for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
	do
		# Landuse Files
		echo "Landuse filename   |${CXR_LANDUSE_INPUT_ARR_FILES[${i}]}" >> ${ahomap_file}
		
		#The corresponding cell dimensions
		if [[ $i -eq 1  ]]
		then
			# Master Grid (Strange order!)
			echo "nx,ny              |$CXR_MASTER_GRID_COLUMNS,$CXR_MASTER_GRID_ROWS" >> ${ahomap_file}
		else
			# Calculate other dimensions using scaling factor
			# This code is also used in xx_convert_output.sh
			
			X_CURRENT=$(( (((${CXR_NEST_END_I_INDEX[$i]} - ${CXR_NEST_BEG_I_INDEX[$i]}) + 1) * ${CXR_NEST_MESHING_FACTOR[$i]}) + 2))
			Y_CURRENT=$(( (((${CXR_NEST_END_J_INDEX[$i]} - ${CXR_NEST_BEG_J_INDEX[$i]}) + 1) * ${CXR_NEST_MESHING_FACTOR[$i]}) + 2))

			echo "nx,ny              |$X_CURRENT,$Y_CURRENT" >> ${ahomap_file}
			
		fi
		
	done
	
	# We assume that we have always satellite data for all simulated days
	# The spelling is taken literally from ENVIRON.
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
				${CXR_WGET_EXEC} ${CURRENT_URL} -O $CXR_AHOMAP_OZONE_COLUMN_DIR/${CXR_AHOMAP_OZONE_COLUMN_FILE} || return $CXR_RET_ERROR
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
# Creates the AHOMAP input file(s). Loops through all modelling days, and determines if
# a new file is needed. If so, creates one.
#
# See:
# <create_ahomap_control_file>
################################################################################
function albedo_haze_ozone() 
################################################################################
{
	# Define & Initialize local vars
	local last_week
	local last_month
	local day_offset=0
	
	local substage
	
	local start_offset=0
	local num_days=0
	local days_left=0
	local month_length=0
	
	local ahomap_control_file
	
	#Was this stage already completed?
	if [[ "$(common.state.storeState ${CXR_STATE_START})" == true  ]]
	then
	
		for day_offset in $(seq 0 $((${CXR_NUMBER_OF_SIM_DAYS} -1 )) )
		do
			common.date.setVars "$CXR_START_DATE" "$day_offset"
			
			# Check if we need another file
			# We need to know how long a week or month still lasts
			# as we do not always begin at boundaries
			case "${CXR_RUN_AHOMAP_TUV_INTERVAL:-once}" in
			
				once )
					start_offset=0
					num_days=${CXR_NUMBER_OF_SIM_DAYS}
					
					main.log -b "Running AHOMAP for whole period..."
					;;
					
				daily )
				
					start_offset=$day_offset
					num_days=1
					main.log -b "Running AHOMAP for $CXR_DATE..."
					;;
					
				weekly )
					# Are we in a new week?
					if [[ "$last_week" != "$CXR_WOY"  ]]
					then
						# Yep, new week.
						# Today is the first day (either of the week or the simulation)
						start_offset=$day_offset
						
						days_left=$(common.date.DaysLeftInWeek $CXR_DATE)
						
						# The number of days depends on the number of days left in the simulation
						if [[ $(( ${CXR_NUMBER_OF_SIM_DAYS} - ${day_offset} + 1 )) -lt ${days_left}  ]]
						then
							# less days left in simulation than in week
							num_days=$(( ${CXR_NUMBER_OF_SIM_DAYS} - ${day_offset} + 1 ))
						else
							#Plenty of days left
							num_days=${days_left}
						fi
						
						main.log -b "Running AHOMAP for week $CXR_WOY ( $num_days days starting at offset $start_offset )..."
						
					else
						# No new week, next iteration
						continue
					fi
					;;
				
				monthly )
					# Are we in a new month?
					if [[ "$last_month" != "$CXR_MONTH"  ]]
					then
						month_length=$(common.date.DaysInMonth $CXR_MONTH $CXR_YEAR)
					
						start_offset=$day_offset
						
						days_left=$(common.date.DaysLeftInMonth $CXR_DATE)
						
						# The number of days depends on the number of days left
						if [[ $(( ${CXR_NUMBER_OF_SIM_DAYS} - ${day_offset} + 1 )) -lt ${days_left}  ]]
						then
							num_days=$(( ${CXR_NUMBER_OF_SIM_DAYS} - ${day_offset} + 1 ))
						else
							#Plenty of days left
							num_days=${days_left}
						fi
						
						main.log -b "Running AHOMAP for month $CXR_MONTH ( $num_days days  starting at offset $start_offset )..."
						substage=$CXR_MONTH
						
					else
						continue
					fi
					;;
			
				*)
					main.die_gracefully "Unknown interval for AHOMAP in variable CXR_RUN_AHOMAP_TUV_INTERVAL, we suport once,daily,weekly or monthly! Exiting." ;;
			esac

			#  --- Setup the Environment
			set_variables 
			
			#  --- Check Settings
			if [[ "$(common.check.preconditions)" == false  ]]
			then
				main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
				# We notify the caller of the problem
				return $CXR_RET_ERR_PRECONDITIONS
			fi
			
			# Increase global indent level
			main.increaseLogIndent
	
			main.log  "Preparing Albedo/Haze/Ozone data for run ${CXR_RUN}..."
			
			# Is the output there?
			if [[ ! -f "$CXR_AHOMAP_OUTPUT_FILE"  ]]
			then
				# File not yet there
			
				# The options of AHOMAP are a bit 
				# weird, better produce a file
				# This function (I know, one should avoid side effects!) 
				# also downloads/caches satellite data
				ahomap_control_file=$(common.runner.createTempFile $FUNCNAME)
				
				if [[ "$CXR_DRY" == false  ]]
				then
				
					create_ahomap_control_file "$ahomap_control_file" $start_offset $num_days
						
					# Is the file there and not empty?)
					if [[ -s "${ahomap_control_file}"  ]]
					then
					
						main.log  "Calling AHOMAP - using this jobfile (be patient)...\n"
				
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
					main.log  "Postconditions for ${CXR_META_MODULE_NAME} are not met!"
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
					return $CXR_RET_ERROR
				fi
			fi
				
			# Do not repeat loop if we run it only once
			if [[ "${CXR_RUN_AHOMAP_TUV_INTERVAL}" == once  ]]
			then
				break
			fi

			last_week=$CXR_WOY
			last_month=$CXR_MONTH
			
		done

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
	if [[ "${CXR_TESTING_FROM_HARNESS:-false}" == false  ]]
	then
		# We need to do initialisation
	
		# This is the run we use to test this
		CXR_RUN=$CXR_META_MODULE_TEST_RUN
	
		# Safety measure if script is not called from .
		MY_DIR=$(dirname $0) && cd $MY_DIR
	
		# We step down the directory tree until we either find CAMxRunner.sh
		# or hit the root directory /
		while [[ $(pwd) != / ]]
		do
			# If we find CAMxRunner, we are there
			ls CAMxRunner.sh >/dev/null 2>&1 && break
			
			# If we are in root, we have gone too far
			if [[ $(pwd) == / ]]
			then
				echo "Could not find CAMxRunner.sh!"
				exit 1
			fi
			
			cd ..
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
	
	if [[ "${CXR_TESTING_FROM_HARNESS:-false}" == false ]]
	then
		# We where called stand-alone, cleanupo is needed
		main.doCleanup
	fi
	
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



