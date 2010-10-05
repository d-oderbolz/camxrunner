# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# This module runs the model.
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################

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

CXR_META_MODULE_DEPENDS_ON="${CXR_TYPE_MODEL}-1 ${CXR_TYPE_PREPROCESS_ONCE} ${CXR_TYPE_PREPROCESS_DAILY}"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=true

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_MODEL}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=1

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the functions to call PMCAMx"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2009), CAMxRunner@psi.ch"

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
# Sets the variables (on a daily basis)
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
	
	# If we do not run the first day, its a restart
	if [[ "$(common.date.isFirstDayOfSimulation?)" == false ]]
	then
		# This must be a restart!
		CXR_RESTART=true
	else
		# Nope.
		CXR_RESTART=false
	fi
	
	########################################################################
	# Per-day settings
	########################################################################
	
	# Empty expansion is not allowed
	
	# Name of the CAMx.in file
	CXR_MODEL_CTRL_FILE=$(common.runner.evaluateRule "$CXR_MODEL_CTRL_FILE_RULE" false CXR_MODEL_CTRL_FILE_RULE)
	
	########################################################################
	# Dry and real need the same variables set
	if [[ "$CXR_HOLLOW" == false || "$CXR_DRY" == true ]]
	then
		# Real or dry run
		########################################################################
		# Define checks
		########################################################################
		

		########################################################################
		# Set variables
		########################################################################
		
		CXR_PHOTOLYIS_RATES_INPUT_FILE=$(common.runner.evaluateRule "$CXR_PHOTOLYIS_RATES_FILE_RULE" false CXR_PHOTOLYIS_RATES_FILE_RULE)
		CXR_INITIAL_CONDITIONS_INPUT_FILE=$(common.runner.evaluateRule "$CXR_INITIAL_CONDITIONS_FILE_RULE" false CXR_INITIAL_CONDITIONS_FILE_RULE)
		CXR_BOUNDARY_CONDITIONS_INPUT_FILE=$(common.runner.evaluateRule "$CXR_BOUNDARY_CONDITIONS_FILE_RULE" false CXR_BOUNDARY_CONDITIONS_FILE_RULE)
		CXR_TOP_CONCENTRATIONS_INPUT_FILE=$(common.runner.evaluateRule "$CXR_TOP_CONCENTRATIONS_FILE_RULE" false CXR_TOP_CONCENTRATIONS_FILE_RULE)
		CXR_ALBEDO_HAZE_OZONE_INPUT_FILE=$(common.runner.evaluateRule "$CXR_ALBEDO_HAZE_OZONE_FILE_RULE" false CXR_ALBEDO_HAZE_OZONE_FILE_RULE)

		
		if [[ "$(common.date.isFirstDayOfSimulation?)" == true  ]]
		then
			# Stuff that we need only the first day
			CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_INITIAL_CONDITIONS_INPUT_FILE"
		else
			## Only needed after the first day
			CXR_MASTER_GRID_RESTART_INPUT_FILE=$(common.runner.evaluateRule "$CXR_MASTER_GRID_RESTART_FILE_RULE" false CXR_MASTER_GRID_RESTART_FILE_RULE)
			
			if [[ $CXR_NUMBER_OF_GRIDS -gt 1 ]]
			then
				CXR_NESTED_GRID_RESTART_INPUT_FILE=$(common.runner.evaluateRule "$CXR_NESTED_GRID_RESTART_FILE_RULE" false CXR_NESTED_GRID_RESTART_FILE_RULE)
			else
				CXR_NESTED_GRID_RESTART_INPUT_FILE=
			fi
			
			#Checks
			CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_MASTER_GRID_RESTART_INPUT_FILE $CXR_NESTED_GRID_RESTART_INPUT_FILE"
		fi

		#Checks
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_PHOTOLYIS_RATES_INPUT_FILE $CXR_INITIAL_CONDITIONS_INPUT_FILE $CXR_BOUNDARY_CONDITIONS_INPUT_FILE $CXR_TOP_CONCENTRATIONS_INPUT_FILE $CXR_ALBEDO_HAZE_OZONE_INPUT_FILE"

		
		# PiG
		if [[ "$CXR_PLUME_IN_GRID" == true  ]]
		then
			CXR_POINT_SOURCES_INPUT_FILE=$(common.runner.evaluateRule "$CXR_POINT_SOURCES_FILE_RULE" false CXR_POINT_SOURCES_FILE_RULE)
			CXR_PIG_RESTART_INPUT_FILE=$(common.runner.evaluateRule "$CXR_PIG_RESTART_FILE_RULE" false CXR_PIG_RESTART_FILE_RULE)
		
			#Checks
			CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_POINT_SOURCES_INPUT_FILE $CXR_PIG_RESTART_INPUT_FILE"
		fi
		
		################################################################################
		# Probing settings
		################################################################################
		
		################################################################
		# OSAT, PSAT, GOAT or APCA
		################################################################
		if [[    "$CXR_PROBING_TOOL" == "OSAT" || "$CXR_PROBING_TOOL" == "PSAT" || "$CXR_PROBING_TOOL" == "GOAT" || "$CXR_PROBING_TOOL" == "APCA"     ]] 
		then
			CXR_SA_MASTER_RESTART_INPUT_FILE=$(common.runner.evaluateRule "$CXR_SA_MASTER_RESTART_FILE_RULE" false CXR_SA_MASTER_RESTART_FILE_RULE)
			
			if [[ $CXR_NUMBER_OF_GRIDS -gt 1 ]]
			then
				CXR_SA_NESTED_RESTART_INPUT_FILE=$(common.runner.evaluateRule "$CXR_SA_NESTED_RESTART_FILE_RULE" false CXR_SA_NESTED_RESTART_FILE_RULE)
			else
				CXR_SA_NESTED_RESTART_INPUT_FILE=
			fi
			
			#Grid specific
			for CXR_IGRID in $(seq 1 $CXR_NUMBER_OF_GRIDS);
			do
				CXR_SA_SOURCE_AREA_MAP_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_SA_SOURCE_AREA_MAP_FILE_RULE" false CXR_SA_SOURCE_AREA_MAP_FILE_RULE) 
			
				#Checks
				CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_SA_SOURCE_AREA_MAP_INPUT_ARR_FILES[${CXR_IGRID}]}"
			done
			
			# Source group specific
 			for CXR_ISRCGROUP in $(seq 1 $CXR_SA_NUMBER_OF_SOURCE_GROUPS);
			do
 				CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${CXR_ISRCGROUP}]=$(common.runner.evaluateRule "$CXR_SA_POINTS_GROUP_FILE_RULE" false CXR_SA_POINTS_GROUP_FILE_RULE)
 				
 				#Checks
				CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${CXR_ISRCGROUP}]}"
 			done
			
		
		################################################################
		# Must we run direct decoupled sensitivity analysis?
		# DDM
		################################################################
		elif [[ "$CXR_PROBING_TOOL" == "DDM"  ]] 
		then
			# This is not a file (hence no _FILE at the end of the name)
			CXR_DDM_ROOT_OUTPUT=$(common.runner.evaluateRule "$CXR_DDM_ROOT_OUTPUT_FILE_RULE" false CXR_DDM_ROOT_OUTPUT_FILE_RULE)
			
			CXR_DDM_MASTER_RESTART_INPUT_FILE=$(common.runner.evaluateRule "$CXR_DDM_MASTER_RESTART_FILE_RULE" false CXR_DDM_MASTER_RESTART_FILE_RULE)
			
			if [[ $CXR_NUMBER_OF_GRIDS -gt 1 ]]
			then
				CXR_DDM_NESTED_RESTART_INPUT_FILE=$(common.runner.evaluateRule "$CXR_DDM_NESTED_RESTART_FILE_RULE" false CXR_DDM_NESTED_RESTART_FILE_RULE)
			else
				CXR_DDM_NESTED_RESTART_INPUT_FILE=
			fi
		
			#Grid specific
			for CXR_IGRID in $(seq 1 $CXR_NUMBER_OF_GRIDS);
			do
				CXR_DDM_SOURCE_AREA_MAP_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_DDM_SOURCE_AREA_MAP_FILE_RULE" false CXR_DDM_SOURCE_AREA_MAP_FILE_RULE) 
			
				#Checks
				CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_DDM_SOURCE_AREA_MAP_INPUT_ARR_FILES[${CXR_IGRID}]}"
			done
			
			#By soure group
			for CXR_IGRID in $(seq 1 $CXR_DDM_NUMBER_OF_SOURCE_GROUPS);
 			do
 				CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_SA_POINTS_GROUP_INPUT_FILE_RULE" false CXR_SA_POINTS_GROUP_INPUT_FILE_RULE) 
 			
 				#Checks
				CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${CXR_IGRID}]}"
 			done
			
			
			# By source group, by grid
 			for CXR_ISRCGROUP in $(seq 1 $CXR_DDM_NUMBER_OF_SOURCE_GROUPS);
 			do
 				
 				# and grid
				for CXR_IGRID in $(seq 1 $CXR_NUMBER_OF_GRIDS);
				do
 					# This is not elegant, but it simulates a 2D Array
 					ELEMENT_NAME=CXR_DDM_EMISS_GROUP_GRID_${CXR_ISRCGROUP}_${CXR_IGRID}_FILE_RULE
 					CXR_DDM_EMISS_GROUP_GRID_${j}_${CXR_IGRID}_INPUT_FILE=$(common.runner.evaluateRule "${!ELEMENT_NAME}" false $ELEMENT_NAME) 
 				
 					#Checks
					CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_DDM_EMISS_GROUP_GRID_${CXR_ISRCGROUP}_${CXR_IGRID}_INPUT_FILE"
 				done
 			done
		
		################################################################
		# Must we run Reactive Tracer Source Apportionment?
		# RTRAC (RT)
		################################################################
		elif [[ "$CXR_PROBING_TOOL" == "RTRAC"  ]] 
		then
			CXR_RT_MASTER_RESTART_INPUT_FILE=$(common.runner.evaluateRule "$CXR_RT_MASTER_RESTART_FILE_RULE" false CXR_RT_MASTER_RESTART_FILE_RULE)
			
			if [[ $CXR_NUMBER_OF_GRIDS -gt 1 ]]
			then
				CXR_RT_NESTED_RESTART_INPUT_FILE=$(common.runner.evaluateRule "$CXR_RT_NESTED_RESTART_FILE_RULE" false CXR_RT_NESTED_RESTART_FILE_RULE)
			else
				CXR_RT_NESTED_RESTART_INPUT_FILE=
			fi
		fi
		
		################################################################
		# General Probing support
		################################################################
		if [[ "$CXR_PROBING_TOOL" != "None"  ]]
		then
			# This is not a file (hence no _FILE at the end of the name)
			CXR_PA_ROOT_OUTPUT=$(common.runner.evaluateRule "$CXR_PA_ROOT_OUTPUT_FILE_RULE" false CXR_PA_ROOT_OUTPUT_FILE_RULE)
		fi
		
		# These are used to prevent overwriting of existing files
		# Output files must not be decompressed!
		CXR_DIAG_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_DIAG_FILE_RULE" false CXR_DIAG_FILE_RULE false)
		
		if [[ $CXR_NUMBER_OF_GRIDS -gt 1 ]]
		then
			CXR_FINST_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_FINST_FILE_RULE" false CXR_FINST_FILE_RULE false)
		else
			CXR_FINST_OUTPUT_FILE=
		fi
		
		CXR_INST_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_INST_FILE_RULE" false CXR_INST_FILE_RULE false)
		CXR_MASS_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_MASS_FILE_RULE" false CXR_MASS_FILE_RULE false)
		CXR_OUT_OUTPUT_FILE=$(common.runner.evaluateRule "$CXR_OUT_FILE_RULE" false CXR_OUT_FILE_RULE false)

		#Checks (this time output)
		CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES $CXR_DIAG_OUTPUT_FILE $CXR_FINST_OUTPUT_FILE $CXR_INST_OUTPUT_FILE $CXR_MASS_OUTPUT_FILE $CXR_OUT_OUTPUT_FILE "

		# THIS IS A WORKAROUND!
		CXR_POINT_SOURCES_INPUT_FILE=$(common.runner.evaluateRule "$CXR_POINT_SOURCES_FILE_RULE" false CXR_POINT_SOURCES_FILE_RULE)


		########################################################################
		# per day-per grid settings
		# we loop
		#	expand the file name rule
		#	then export the name and the value
		########################################################################
		for CXR_IGRID in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
		do
			# Landuse
			CXR_LANDUSE_GRID_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_LANDUSE_FILE_RULE" false CXR_LANDUSE_FILE_RULE)
			# Pressure
			CXR_ZP_GRID_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_PRESSURE_FILE_RULE" false CXR_PRESSURE_FILE_RULE)
			# Wind
			CXR_WIND_GRID_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_WIND_FILE_RULE" false CXR_WIND_FILE_RULE)
			# Temperature
			CXR_TEMP_GRID_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_TEMPERATURE_FILE_RULE" false CXR_TEMPERATURE_FILE_RULE)
			# Vapor
			CXR_VAPOR_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_VAPOR_FILE_RULE" false CXR_VAPOR_FILE_RULE)
			# Cloud
			CXR_CLOUD_GRID_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_CLOUD_FILE_RULE" false CXR_CLOUD_FILE_RULE)
			# Vertical K
			CXR_KV_GRID_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_K_FILE_RULE" false CXR_K_FILE_RULE)
			# Emissions
			CXR_EMISS_INPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_EMISSION_FILE_RULE" false CXR_EMISSION_FILE_RULE)
			
			#Checks
			CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_LANDUSE_GRID_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_ZP_GRID_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_WIND_GRID_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_TEMP_GRID_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_VAPOR_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_CLOUD_GRID_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_KV_GRID_INPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_EMISS_INPUT_ARR_FILES[${CXR_IGRID}]}"

			# These are used to prevent overwriting of existing files 
			# Output files must not be decompressed!
			CXR_AVG_OUTPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_AVG_FILE_RULE" false CXR_AVG_FILE_RULE false)
			CXR_DEPN_OUTPUT_ARR_FILES[${CXR_IGRID}]=$(common.runner.evaluateRule "$CXR_DEPN_FILE_RULE" false CXR_DEPN_FILE_RULE false)

			#Checks (this time output)
			CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES ${CXR_AVG_OUTPUT_ARR_FILES[${CXR_IGRID}]} ${CXR_DEPN_OUTPUT_ARR_FILES[${CXR_IGRID}]}"
		done

	fi
}

################################################################################
# Function: write_model_control_file
#
# Writes a CAMx.in file using current settings.
# Is wildly incomplete (No probing tools) and inflexible (species list is fixed)
#
# NOTE: By design, we currently cannot run more than one model in parallel
# Because the location of CAMx.in is not unique. (This is not a problem,
# since CAMx itself is parallel and it is better not to run more than one instance)
#
# This function can be overwritten for another version
################################################################################
function write_model_control_file() 
################################################################################
{
	# This only supports PMCAMx 2008  (CAMx 4.0)
	
	# Define & Initialize local vars
	local iSpec
	local iGrid
	local str
	local len
	local num_spaces
	local y
	
	# Clean the file away first
	: > ${CXR_MODEL_CTRL_FILE} 
	
	echo "CAMx Version       |VERSION4.0" > ${CXR_MODEL_CTRL_FILE}
	echo "Run Message        |${CXR_RUN}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Root output name   |$CXR_ROOT_OUTPUT" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Start yr/mo/dy/hr  |${CXR_YEAR_S} ${CXR_MONTH} ${CXR_DAY} ${CXR_START_HOUR}." >> ${CXR_MODEL_CTRL_FILE} 
	echo "End   yr/mo/dy/hr  |${CXR_YEAR_S} ${CXR_MONTH} ${CXR_DAY} ${CXR_STOP_HOUR}." >> ${CXR_MODEL_CTRL_FILE} 
	echo "DT:max,met,ems,out |${CXR_MAXIMUM_TIMESTEP} ${CXR_MET_INPUT_FREQUENCY} ${CXR_EMS_INPUT_FREQUENCY} ${CXR_OUTPUT_FREQUENCY}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "nx,ny,nz           |${CXR_MASTER_GRID_COLUMNS}   ${CXR_MASTER_GRID_ROWS}   ${CXR_NUMBER_OF_LAYERS[1]}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "sCoordinate ID     |${CXR_MAP_PROJECTION}" >> ${CXR_MODEL_CTRL_FILE}
	
	case ${CXR_MAP_PROJECTION} in
	
		LAMBERT) echo "xorg,yorg,dx,dy... |${CXR_MASTER_ORIGIN_XCOORD} ${CXR_MASTER_ORIGIN_YCOORD}  ${CXR_MASTER_CELL_XSIZE}  ${CXR_MASTER_CELL_YSIZE} ${CXR_LAMBERT_CENTER_LONGITUDE} ${CXR_LAMBERT_CENTER_LATITUDE} ${CXR_LAMBERT_TRUE_LATITUDE1} ${CXR_LAMBERT_TRUE_LATITUDE2}" >> ${CXR_MODEL_CTRL_FILE} ;;
		LATLON)  echo "long,lat,dx,dy     |${CXR_MASTER_ORIGIN_XCOORD} ${CXR_MASTER_ORIGIN_YCOORD}  ${CXR_MASTER_CELL_XSIZE}  ${CXR_MASTER_CELL_YSIZE} " >> ${CXR_MODEL_CTRL_FILE} ;;
		UTM)     echo "x,y,dx,dy,zone     |${CXR_MASTER_ORIGIN_XCOORD} ${CXR_MASTER_ORIGIN_YCOORD}  ${CXR_MASTER_CELL_XSIZE}  ${CXR_MASTER_CELL_YSIZE} ${CXR_UTM_ZONE} " >> ${CXR_MODEL_CTRL_FILE} ;;
		POLAR)   echo "x,y,dx,dy,lo_p,la_p|${CXR_MASTER_ORIGIN_XCOORD} ${CXR_MASTER_ORIGIN_YCOORD}  ${CXR_MASTER_CELL_XSIZE}  ${CXR_MASTER_CELL_YSIZE} ${CXR_POLAR_LONGITUDE_POLE} ${CXR_POLAR_LATITUDE_POLE}" >> ${CXR_MODEL_CTRL_FILE} ;;
		*) main.dieGracefully "Map projection ${CXR_MAP_PROJECTION} currently not supported!"
	
	esac
	
	echo "time zone          |${CXR_TIME_ZONE}" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "PiG parameters     |${CXR_PIG_MAX_PUFF_LENGTH} ${CXR_PIG_MAX_PUFF_AGE}" >> ${CXR_MODEL_CTRL_FILE} # Maxiumum puff length [m], maximum puff age [h]
	echo "Avg output species |${CXR_NUMBER_OF_OUTPUT_SPECIES}" >> ${CXR_MODEL_CTRL_FILE} 
	
	# PMCAMx wants the species to be listed in grops of 6 each, where the distance between each column is 10 bytes
	# Looks like this:
	# "                   |NO        NO2       O3        PAN       PAN2      MPAN"
	for iSpec in $(seq 1 $CXR_NUMBER_OF_OUTPUT_SPECIES);
	do
	
		# If we are in the first round, add apropriate intro
		
		if [[ $iSpec -eq 1  ]]
		then
			echo -n "                   |"  >> ${CXR_MODEL_CTRL_FILE}
		fi
	
		# string lenght, sorry for the weird detour
		str=${CXR_OUTPUT_SPECIES_NAMES[${iSpec}]}
		len=${#str}
		
		# What's left of the 10 Bytes?
		num_spaces=$(( ${CXR_SPECIES_COLUMN_WIDTH:-10} - $len ))
		
		# Correct if we chopped all away
		if [[ $num_spaces -lt 0  ]]
		then
			main.log  "Attention: Either your species names are to long or the column spacing is to small!"
			num_spaces=0
		fi
		
		# Generate that many spaces
		y=0 
		SPACES=
		while [ "$y" -lt $num_spaces ]; do
			SPACES=" $SPACES"
			y=$(($y + 1))
		done 

		# Do we need to start a new line?
		if [[ $(expr ${iSpec} % ${CXR_SPECIES_COLUMNS:-6}) -eq 0  ]]
		then
			# Start new Line, no spaces at the end
			echo ${CXR_OUTPUT_SPECIES_NAMES[${iSpec}]} >> ${CXR_MODEL_CTRL_FILE}
			
			# Already add new column if there are species left
			if [[ $iSpec -lt $CXR_NUMBER_OF_OUTPUT_SPECIES  ]]
			then
				echo -n "                   |"  >> ${CXR_MODEL_CTRL_FILE}
			fi
			
		else
			# No new Line, spaces if it is not last Element
			if [[ $iSpec -lt $CXR_NUMBER_OF_OUTPUT_SPECIES  ]]
			then
				echo -n ${CXR_OUTPUT_SPECIES_NAMES[${iSpec}]}"${SPACES}" >> ${CXR_MODEL_CTRL_FILE}
			else
				# But: if this was the last element, we need a newline and no spaces
				echo ${CXR_OUTPUT_SPECIES_NAMES[${iSpec}]} >> ${CXR_MODEL_CTRL_FILE}
			fi
		fi
	done
	
	if [[ ${CXR_NUMBER_OF_GRIDS} -lt 2  ]]
	then
		#PMCAMx expects a 0 insted of 1 if there is no nesting (that might be true for CAMx as well?)
		echo "# nested grids     |0" >> ${CXR_MODEL_CTRL_FILE}
	else
		echo "# nested grids     |${CXR_NUMBER_OF_GRIDS}" >> ${CXR_MODEL_CTRL_FILE}
	fi	
	
	# Set up Nested grid data
	for iGrid in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "i1,i2,j1,j2,nz,mesh|${CXR_NEST_BEG_I_INDEX[${iGrid}]} ${CXR_NEST_END_I_INDEX[${iGrid}]} ${CXR_NEST_BEG_J_INDEX[${iGrid}]} ${CXR_NEST_END_J_INDEX[${iGrid}]} ${CXR_NUMBER_OF_LAYERS[${iGrid}]} ${CXR_NEST_MESHING_FACTOR[${iGrid}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	echo "SMOLAR,BOTT, PPM?  |${CXR_ADVECTION_SOLVER}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Chemistry solver   |${CXR_CHEMISTRY_SOLVER}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Restart            |${CXR_RESTART}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Chemistry          |${CXR_CHEMISTRY}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Dry dep            |${CXR_DRY_DEPOSITION}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Wet dep            |${CXR_WET_DEPOSITION}" >> ${CXR_MODEL_CTRL_FILE} 
	
	# In this version, this is a flag
	if [[ "${CXR_PIG_SUBMODEL:-None}" == None  ]]
	then
		echo "PiG submodel       |false" >> ${CXR_MODEL_CTRL_FILE} 
	else
		echo "PiG submodel       |true" >> ${CXR_MODEL_CTRL_FILE} 
	fi
	
	echo "Staggered winds    |${CXR_STAGGERED_WINDS}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Treat area emiss   |${CXR_GRIDDED_EMISSIONS}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Treat point emiss  |${CXR_POINT_EMISSIONS}" >> ${CXR_MODEL_CTRL_FILE}
	
	# Is this the same idea?
	echo "1-day emiss inputs |${CXR_IGNORE_EMISSION_DATES}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "3-D average file   |${CXR_AVERAGE_OUTPUT_3D}" >> ${CXR_MODEL_CTRL_FILE}
	
	# In this version, this is a flag
	if [[ "${CXR_PROBING_TOOL:-None}" == None  ]]
	then
		echo "Probing tools      |false" >> ${CXR_MODEL_CTRL_FILE} 
	else
		echo "Probing tools      |true" >> ${CXR_MODEL_CTRL_FILE} 
	fi
	
	# Put specific info here (see sections 5-7 in manual)
	
	echo "Chemparam          |${CXR_CHEMPARAM_INPUT_FILE}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Photolysis rates   |${CXR_PHOTOLYIS_RATES_INPUT_FILE}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Landuse            |${CXR_LANDUSE_GRID_INPUT_ARR_FILES[1]}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Height/pressure    |${CXR_ZP_GRID_INPUT_ARR_FILES[1]}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Wind               |${CXR_WIND_GRID_INPUT_ARR_FILES[1]}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Temperature        |${CXR_TEMP_GRID_INPUT_ARR_FILES[1]}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Water vapor        |${CXR_VAPOR_INPUT_ARR_FILES[1]}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Cloud/rain         |${CXR_CLOUD_GRID_INPUT_ARR_FILES[1]}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Vertical diffsvty  |${CXR_KV_GRID_INPUT_ARR_FILES[1]}" >> ${CXR_MODEL_CTRL_FILE} 
	
		# If we do a restart, do not supply IC
	if [[ "${CXR_RESTART}" == false ]]
	then
		# First day
		echo "Initial conditions |${CXR_INITIAL_CONDITIONS_INPUT_FILE}" >> ${CXR_MODEL_CTRL_FILE} 
	else
		# Another day (restart) - nothing
		echo "Initial conditions |" >> ${CXR_MODEL_CTRL_FILE} 
	fi
	
	
	echo "Boundary conditions|${CXR_BOUNDARY_CONDITIONS_INPUT_FILE}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Top concentration  |${CXR_TOP_CONCENTRATIONS_INPUT_FILE}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Albedo/haze/ozone  |${CXR_ALBEDO_HAZE_OZONE_INPUT_FILE}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Point emiss        |${CXR_POINT_SOURCES_INPUT_FILE}" >> ${CXR_MODEL_CTRL_FILE} 
	echo "Area emiss         |${CXR_EMISS_INPUT_ARR_FILES[1]}" >> ${CXR_MODEL_CTRL_FILE}
	
	# Now we provide the data for the nested grids. The structure of the file
	# requires to repeat the loop over the grids (this was improved in CAMx 4.x!)
	
	for iGrid in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Landuse            |${CXR_LANDUSE_GRID_INPUT_ARR_FILES[${iGrid}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	for iGrid in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Height/pressure    |${CXR_ZP_GRID_INPUT_ARR_FILES[${iGrid}]]}" >> ${CXR_MODEL_CTRL_FILE}
	done
	
	for iGrid in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Wind               |${CXR_WIND_GRID_INPUT_ARR_FILES[${iGrid}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	for iGrid in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Temperature        |${CXR_TEMP_GRID_INPUT_ARR_FILES[${iGrid}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	for iGrid in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Water vapor        |${CXR_VAPOR_INPUT_ARR_FILES[${iGrid}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	for iGrid in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Cloud/rain         |${CXR_CLOUD_GRID_INPUT_ARR_FILES[${iGrid}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	for iGrid in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Vertical diffsvty  |${CXR_KV_GRID_INPUT_ARR_FILES[${iGrid}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	for iGrid in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Area emiss         |${CXR_EMISS_INPUT_ARR_FILES[${iGrid}]}" >> ${CXR_MODEL_CTRL_FILE}
	done

	echo "Master restart file|${CXR_MASTER_GRID_RESTART_INPUT_FILE:-}" >> ${CXR_MODEL_CTRL_FILE}
	
	if [[ ${CXR_NUMBER_OF_GRIDS} -gt 1  ]]
	then
		echo "Nested restart file|${CXR_NESTED_GRID_RESTART_INPUT_FILE:-}" >> ${CXR_MODEL_CTRL_FILE}
	fi
	
	if [[ "$CXR_PLUME_IN_GRID" == true  ]]
	then
		echo "PiG restart file   |${CXR_PIG_RESTART_INPUT_FILE:-}" >> ${CXR_MODEL_CTRL_FILE}
	fi
	
	################################################################
	# OSAT, PSAT,  GOAT or APCA ia yet to be implemented here...
	################################################################

	##########################################################################
	# Now we need to link this file to the name "CAMx.in" in the state directory
	##########################################################################
	(
		cd ${CXR_STATE_DIR}  || return $CXR_RET_ERROR
		
		# Remove old link - does not matter if this fails
		rm -f CAMx.in
		
		# New one
		ln -s $(basename ${CXR_MODEL_CTRL_FILE}) CAMx.in || return $CXR_RET_ERROR
	)

}

################################################################################
# Function: execute_model
#
# This central function at last calls the model
################################################################################
function execute_model()
################################################################################
{
	local retval
	
	# The CAMx.in file is in the state dir
	cd ${CXR_STATE_DIR}
	
	# Call the executable while collecting stderr and stdout
	$CXR_MODEL_EXEC 2>&1 | tee -a $CXR_LOG
	
	# Checking pipestatus...
	if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
	then
		# Failed.
		retval=1
	else
		# OK
		retval=0
	fi
	
	# go back
	cd ${CXR_RUN_DIR} || main.dieGracefully "Could not change back to ${CXR_RUN_DIR}"
	
	if [[ $retval -ne 0 ]]
	then
		# common.state.storeStatus ${CXR_STATUS_FAILURE}
		main.log -w "CAMx has returned a non-zero status for $CXR_DATE"
	fi
	

}

################################################################################
# Function: model
#
# High level function to call this model
################################################################################
function model()
################################################################################
{
	# We do not need this variable here (exept implicit for the stage name)
	CXR_INVOCATION=${1:-1}
		
	# Do we run the model?
	if [[ "$CXR_RUN_MODEL" == true  ]]
	then
	
		# common.state.storeStatus checks if we have finished this and if we need to continue
		if [[ ! $(common.state.storeStatus ${CXR_STATUS_RUNNING}) == true  ]]
		then
		
			main.log -B  "Running $CXR_MODEL_EXEC for day $CXR_DATE"
			
			#  --- Execute the model and write stderr and stdout to CXR_LOG ---
			set_variables
			
			#  --- Create the input file - will be stored in the state directory 
			#      but a link called CAMx.in wil be created where the CAMx binary is located
			write_model_control_file				
			
			if [[ $(common.check.preconditions) == false  ]]
			then
				main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
				common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
				
				# We notify the caller of the problem
				return $CXR_RET_ERR_PRECONDITIONS
			fi
			
			# Test if any of the average file pre-exists
			for iGrid in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
			do
				if [[ -e "${CXR_AVG_OUTPUT_ARR_FILES[${CXR_IGRID}]}" ]]
				then
					if [[ "${CXR_SKIP_EXISTING}" == true ]]
					then
						# Ups, we skip this one
						main.log -w "File ${CXR_AVG_OUTPUT_ARR_FILES[${CXR_IGRID}]} exists, model will not be run"
						common.state.storeStatus ${CXR_STATUS_SUCCESS}  > /dev/null
						return $CXR_RET_OK
					else
						main.log -e  "File ${CXR_AVG_OUTPUT_ARR_FILES[${CXR_IGRID}]} exists - to force the re-creation run ${CXR_CALL} -F"
						common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
						return $CXR_RET_ERROR
					fi
				fi
			done
			
			if [[ "$CXR_DRY" == false  ]]
			then
				execute_model
			else
				main.log  "This is a dry run, $CXR_MODEL is not run"
			fi
		
			# Did we run properly?
			if [[ $(common.check.postconditions) == false  ]]
			then
				main.log  "$CXR_MODEL Run was not successful!"
				common.state.storeStatus ${CXR_STATUS_FAILURE}  > /dev/null
				
				# We notify the caller of the problem
				return $CXR_RET_ERR_POSTCONDITIONS
			fi
			
			# We store the fact model run was completed
			common.state.storeStatus ${CXR_STATUS_SUCCESS} > /dev/null
			
		else
			main.log -a "Stage was already started, therefore we do not run it. I assume this is a restart - we try to catch up!"
		fi
		
		else
		main.log  "Model disabled (either in the config using CXR_RUN_MODEL=false or with the option -N)"
	fi
}

################################################################################
# Function: test_module
#
# Runs the predefined tests for this module
# 
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
	
	write_model_control_file
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.fs.isNotEmpty? ${CXR_MODEL_CTRL_FILE}) true "write_model_control_file simple existence check, inspect ${CXR_MODEL_CTRL_FILE}"
	
	
	########################################
	# teardown tests if needed
	########################################
	
	# Reset date variables for first day
	common.date.setVars "$CXR_START_DATE" "0"
}
