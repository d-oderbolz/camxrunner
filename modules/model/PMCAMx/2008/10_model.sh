#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Date functions
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
# all_once_preprocessors - all pre_start_preprocessors must have finished
# all_daily_preprocessors - all daily_preprocessors must have finished
# all_model - all model modules must have finished
# all_daily_postprocessors - all daily_postprocessors must have finished
# all_once_postprocessors - all finish_postprocessors must have finished

# the special predicate - refers to the previous model day, so all_model- means that all model modules of the previous day must be successful

CXR_META_MODULE_DEPENDS_ON="all_once_preprocessors all_daily_preprocessors"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=true

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_MODEL}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=0

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=PMCAMx-v2008-test

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=400

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=400
# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the functions to call CAMx"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2009), CAMxRunner@psi.ch"

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
	
	########################################################################
	# Per-day settings
	########################################################################
	
	# Empty expansion is not allowed
	
	# Name of the CAMx.in file
	CXR_MODEL_CTRL_FILE=$(cxr_common_evaluate_rule "$CXR_MODEL_CTRL_FILE_RULE" false CXR_MODEL_CTRL_FILE_RULE)
	
	# Not directly checkable, Start of all output file names without extension
	CXR_ROOT_OUTPUT=$(cxr_common_evaluate_rule "$CXR_ROOT_OUTPUT_FILE_RULE" false CXR_ROOT_OUTPUT_FILE_RULE)
	
	# This is not a file (hence no _FILE at the end of the name)
	CXR_RT_ROOT_OUTPUT=$(cxr_common_evaluate_rule "$CXR_RT_ROOT_OUTPUT_FILE_RULE" false CXR_RT_ROOT_OUTPUT_FILE_RULE)
	
	# This is not a file (hence no _FILE at the end of the name)
	CXR_SA_ROOT_OUTPUT=$(cxr_common_evaluate_rule "$CXR_SA_ROOT_OUTPUT_FILE_RULE" false CXR_SA_ROOT_OUTPUT_FILE_RULE)
	
	########################################################################
	# Dry and real need the same variables set
	if [[  "$CXR_HOLLOW" == false || "$CXR_DRY" == true   ]]
	then
		# Real or dry run
		########################################################################
		# Define checks
		########################################################################
		

		########################################################################
		# Set variables
		########################################################################
		
		CXR_PHOTOLYIS_RATES_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_PHOTOLYIS_RATES_FILE_RULE" false CXR_PHOTOLYIS_RATES_FILE_RULE)
		CXR_INITIAL_CONDITIONS_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_INITIAL_CONDITIONS_FILE_RULE" false CXR_INITIAL_CONDITIONS_FILE_RULE)
		CXR_BOUNDARY_CONDITIONS_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_BOUNDARY_CONDITIONS_FILE_RULE" false CXR_BOUNDARY_CONDITIONS_FILE_RULE)
		CXR_TOP_CONCENTRATIONS_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_TOP_CONCENTRATIONS_FILE_RULE" false CXR_TOP_CONCENTRATIONS_FILE_RULE)
		CXR_ALBEDO_HAZE_OZONE_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_ALBEDO_HAZE_OZONE_FILE_RULE" false CXR_ALBEDO_HAZE_OZONE_FILE_RULE)

		
		if [[ "$(common.date.isFirstDayOfSimulation?)" == true  ]]
		then
			# Stuff that we need only the first day
			CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_INITIAL_CONDITIONS_INPUT_FILE"
		else
			## Only needed after the first day
			CXR_MASTER_GRID_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_MASTER_GRID_RESTART_FILE_RULE" false CXR_MASTER_GRID_RESTART_FILE_RULE)
			CXR_NESTED_GRID_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_NESTED_GRID_RESTART_FILE_RULE" false CXR_NESTED_GRID_RESTART_FILE_RULE)
		
			#Checks
			CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_MASTER_GRID_RESTART_INPUT_FILE $CXR_NESTED_GRID_RESTART_INPUT_FILE"
		fi

		#Checks
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_PHOTOLYIS_RATES_INPUT_FILE $CXR_INITIAL_CONDITIONS_INPUT_FILE $CXR_BOUNDARY_CONDITIONS_INPUT_FILE $CXR_TOP_CONCENTRATIONS_INPUT_FILE $CXR_ALBEDO_HAZE_OZONE_INPUT_FILE"

		
		# PiG
		if [[ "$CXR_PLUME_IN_GRID" == true  ]]
		then
			CXR_POINT_SOURCES_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_POINT_SOURCES_FILE_RULE" false CXR_POINT_SOURCES_FILE_RULE)
			CXR_PIG_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_PIG_RESTART_FILE_RULE" false CXR_PIG_RESTART_FILE_RULE)
		
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
			CXR_SA_MASTER_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_SA_MASTER_RESTART_FILE_RULE" false CXR_SA_MASTER_RESTART_FILE_RULE)
			CXR_SA_NESTED_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_SA_NESTED_RESTART_FILE_RULE" false CXR_SA_NESTED_RESTART_FILE_RULE)
		
			#Grid specific
			for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
			do
				CXR_SA_SOURCE_AREA_MAP_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_SA_SOURCE_AREA_MAP_FILE_RULE" false CXR_SA_SOURCE_AREA_MAP_FILE_RULE) 
			
				#Checks
				CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_SA_SOURCE_AREA_MAP_INPUT_ARR_FILES[${i}]}"
			done
			
			# Source group specific
 			for j in $(seq 1 $CXR_SA_NUMBER_OF_SOURCE_GROUPS);
			do
 				CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${j}]=$(cxr_common_evaluate_rule "$CXR_SA_POINTS_GROUP_FILE_RULE" false CXR_SA_POINTS_GROUP_FILE_RULE)
 				
 				#Checks
				CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${j}]}"
 			done
			
		
		################################################################
		# Must we run direct decoupled sensitivity analysis?
		# DDM
		################################################################
		elif [[ "$CXR_PROBING_TOOL" == "DDM"  ]] 
		then
			# This is not a file (hence no _FILE at the end of the name)
			CXR_DDM_ROOT_OUTPUT=$(cxr_common_evaluate_rule "$CXR_DDM_ROOT_OUTPUT_FILE_RULE" false CXR_DDM_ROOT_OUTPUT_FILE_RULE)
			
			CXR_DDM_MASTER_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_DDM_MASTER_RESTART_FILE_RULE" false CXR_DDM_MASTER_RESTART_FILE_RULE)
			CXR_DDM_NESTED_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_DDM_NESTED_RESTART_FILE_RULE" false CXR_DDM_NESTED_RESTART_FILE_RULE)
		
			#Grid specific
			for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
			do
				CXR_DDM_SOURCE_AREA_MAP_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_DDM_SOURCE_AREA_MAP_FILE_RULE" false CXR_DDM_SOURCE_AREA_MAP_FILE_RULE) 
			
				#Checks
				CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_DDM_SOURCE_AREA_MAP_INPUT_ARR_FILES[${i}]}"
			done
			
			#By soure group
			for j in $(seq 1 $CXR_DDM_NUMBER_OF_SOURCE_GROUPS);
 			do
 				CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_SA_POINTS_GROUP_INPUT_FILE_RULE" false CXR_SA_POINTS_GROUP_INPUT_FILE_RULE) 
 			
 				#Checks
				CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${i}]}"
 			done
			
			
			# By source group, by grid
 			for j in $(seq 1 $CXR_DDM_NUMBER_OF_SOURCE_GROUPS);
 			do
 				
 				# and grid
				for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
				do
 					# This is not elegant, but it simulates a 2D Array
 					ELEMENT_NAME=CXR_DDM_EMISS_GROUP_GRID_${j}_${i}_FILE_RULE
 					CXR_DDM_EMISS_GROUP_GRID_${j}_${i}_INPUT_FILE=$(cxr_common_evaluate_rule "${!ELEMENT_NAME}" false $ELEMENT_NAME) 
 				
 					#Checks
					CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_DDM_EMISS_GROUP_GRID_${j}_${i}_INPUT_FILE"
 				done
 			done
		
		################################################################
		# Must we run Reactive Tracer Source Apportionment?
		# RTRAC (RT)
		################################################################
		elif [[ "$CXR_PROBING_TOOL" == "RTRAC"  ]] 
		then
			CXR_RT_MASTER_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_RT_MASTER_RESTART_FILE_RULE" false CXR_RT_MASTER_RESTART_FILE_RULE)
			CXR_RT_NESTED_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_RT_NESTED_RESTART_FILE_RULE" false CXR_RT_NESTED_RESTART_FILE_RULE)
		fi
		
		################################################################
		# General Probing support
		################################################################
		if [[ "$CXR_PROBING_TOOL" != "None"  ]]
		then
			# This is not a file (hence no _FILE at the end of the name)
			CXR_PA_ROOT_OUTPUT=$(cxr_common_evaluate_rule "$CXR_PA_ROOT_OUTPUT_FILE_RULE" false CXR_PA_ROOT_OUTPUT_FILE_RULE)
		fi
		
		# These are used to prevent overwriting of existing files
		# Output files must not be decompressed!
		CXR_DIAG_OUTPUT_FILE=$(cxr_common_evaluate_rule "$CXR_DIAG_FILE_RULE" false CXR_DIAG_FILE_RULE false)
		CXR_FINST_OUTPUT_FILE=$(cxr_common_evaluate_rule "$CXR_FINST_FILE_RULE" false CXR_FINST_FILE_RULE false)
		CXR_INST_OUTPUT_FILE=$(cxr_common_evaluate_rule "$CXR_INST_FILE_RULE" false CXR_INST_FILE_RULE false)
		CXR_MASS_OUTPUT_FILE=$(cxr_common_evaluate_rule "$CXR_MASS_FILE_RULE" false CXR_MASS_FILE_RULE false)
		CXR_OUT_OUTPUT_FILE=$(cxr_common_evaluate_rule "$CXR_OUT_FILE_RULE" false CXR_OUT_FILE_RULE false)

		#Checks (this time output)
		CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES $CXR_DIAG_OUTPUT_FILE $CXR_FINST_OUTPUT_FILE $CXR_INST_OUTPUT_FILE $CXR_MASS_OUTPUT_FILE $CXR_OUT_OUTPUT_FILE "

		# THIS IS A WORKAROUND!
		CXR_POINT_SOURCES_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_POINT_SOURCES_FILE_RULE" false CXR_POINT_SOURCES_FILE_RULE)


		########################################################################
		# per day-per grid settings
		# we loop
		#	expand the file name rule
		#	then export the name and the value
		########################################################################
		for i in $(seq 1 ${CXR_NUMBER_OF_GRIDS});
		do
			# Landuse
			CXR_LANDUSE_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_LANDUSE_FILE_RULE" false CXR_LANDUSE_FILE_RULE)
			# Pressure
			CXR_ZP_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_PRESSURE_FILE_RULE" false CXR_PRESSURE_FILE_RULE)
			# Wind
			CXR_WIND_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_WIND_FILE_RULE" false CXR_WIND_FILE_RULE)
			# Temperature
			CXR_TEMP_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_TEMPERATURE_FILE_RULE" false CXR_TEMPERATURE_FILE_RULE)
			# Vapor
			CXR_VAPOR_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_VAPOR_FILE_RULE" false CXR_VAPOR_FILE_RULE)
			# Cloud
			CXR_CLOUD_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_CLOUD_FILE_RULE" false CXR_CLOUD_FILE_RULE)
			# Vertical K
			CXR_KV_GRID_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_K_FILE_RULE" false CXR_K_FILE_RULE)
			# Emissions
			CXR_EMISS_INPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_EMISSION_FILE_RULE" false CXR_EMISSION_FILE_RULE)
			
			#Checks
			CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_LANDUSE_GRID_INPUT_ARR_FILES[${i}]} ${CXR_ZP_GRID_INPUT_ARR_FILES[${i}]} ${CXR_WIND_GRID_INPUT_ARR_FILES[${i}]} ${CXR_TEMP_GRID_INPUT_ARR_FILES[${i}]} ${CXR_VAPOR_INPUT_ARR_FILES[${i}]} ${CXR_CLOUD_GRID_INPUT_ARR_FILES[${i}]} ${CXR_KV_GRID_INPUT_ARR_FILES[${i}]} ${CXR_EMISS_INPUT_ARR_FILES[${i}]}"

			# These are used to prevent overwriting of existing files 
			# Output files must not be decompressed!
			CXR_AVG_OUTPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_AVG_FILE_RULE" false CXR_AVG_FILE_RULE false)
			CXR_DEPN_OUTPUT_ARR_FILES[${i}]=$(cxr_common_evaluate_rule "$CXR_DEPN_FILE_RULE" false CXR_DEPN_FILE_RULE false)

			#Checks (this time output)
			CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES ${CXR_AVG_OUTPUT_ARR_FILES[${i}]} ${CXR_DEPN_OUTPUT_ARR_FILES[${i}]}"
		done

	fi
}

################################################################################
# Function: write_model_control_file
#
# Writes a CAMx.in file using current settings.
# Is wildly incomplete (No probing tools) and inflexible (species list is fixed)
#
# This function can be overwritten for another version
################################################################################
function write_model_control_file() 
################################################################################
{
	# This only supports PMCAMx 2008  (CAMx 4.0)
	
	# Define & Initialize local vars
	local i
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
		*) main.die_gracefully "Map projection ${CXR_MAP_PROJECTION} currently not supported!"
	
	esac
	
	echo "time zone          |${CXR_TIME_ZONE}" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "PiG parameters     |${CXR_PIG_MAX_PUFF_LENGTH} ${CXR_PIG_MAX_PUFF_AGE}" >> ${CXR_MODEL_CTRL_FILE} # Maxiumum puff length [m], maximum puff age [h]
	echo "Avg output species |${CXR_NUMBER_OF_OUTPUT_SPECIES}" >> ${CXR_MODEL_CTRL_FILE} 
	
	# PMCAMx wants the species to be listed in grops of 6 each, where the distance between each column is 10 bytes
	# Looks like this:
	# "                   |NO        NO2       O3        PAN       PAN2      MPAN"
	for i in $(seq 1 $CXR_NUMBER_OF_OUTPUT_SPECIES);
	do
	
		# If we are in the first round, add apropriate intro
		
		if [[ $i -eq 1  ]]
		then
			echo -n "                   |"  >> ${CXR_MODEL_CTRL_FILE}
		fi
	
		# string lenght, sorry for the weird detour
		str=${CXR_OUTPUT_SPECIES_NAMES[${i}]}
		len=${#str}
		
		# What's left of the 10 Bytes?
		num_spaces=$(( ${CXR_SPECIES_COLUMN_WIDTH:-10} - $len ))
		
		# Correct if we chopped all away
		if [[ $num_spaces -lt 0  ]]
		then
			main.log  "Attention: Either your species names are to long or the column spacing is to small!"
			num_spaces=0
		fi
		
		# Generate that many spaces (the printf approach in main.log does not work with spaces!!)
		y=0 
		SPACES=
		while [ "$y" -lt $num_spaces ]; do
			SPACES=" $SPACES"
			y=$(($y + 1))
		done 

		# Do we need to start a new line?
		if [[ $(expr ${i} % ${CXR_SPECIES_COLUMNS:-6}) -eq 0  ]]
		then
			# Start new Line, no spaces at the end
			echo ${CXR_OUTPUT_SPECIES_NAMES[${i}]} >> ${CXR_MODEL_CTRL_FILE}
			
			# Already add new column if there are species left
			if [[ $i -lt $CXR_NUMBER_OF_OUTPUT_SPECIES  ]]
			then
				echo -n "                   |"  >> ${CXR_MODEL_CTRL_FILE}
			fi
			
		else
			# No new Line, spaces if it is not last Element
			if [[ $i -lt $CXR_NUMBER_OF_OUTPUT_SPECIES  ]]
			then
				echo -n ${CXR_OUTPUT_SPECIES_NAMES[${i}]}"${SPACES}" >> ${CXR_MODEL_CTRL_FILE}
			else
				# But: if this was the last element, we need a newline and no spaces
				echo ${CXR_OUTPUT_SPECIES_NAMES[${i}]} >> ${CXR_MODEL_CTRL_FILE}
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
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "i1,i2,j1,j2,nz,mesh|${CXR_NEST_BEG_I_INDEX[${i}]} ${CXR_NEST_END_I_INDEX[${i}]} ${CXR_NEST_BEG_J_INDEX[${i}]} ${CXR_NEST_END_J_INDEX[${i}]} ${CXR_NUMBER_OF_LAYERS[${i}]} ${CXR_NEST_MESHING_FACTOR[${i}]}" >> ${CXR_MODEL_CTRL_FILE} 
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
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Landuse            |${CXR_LANDUSE_GRID_INPUT_ARR_FILES[${i}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Height/pressure    |${CXR_ZP_GRID_INPUT_ARR_FILES[${i}]]}" >> ${CXR_MODEL_CTRL_FILE}
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Wind               |${CXR_WIND_GRID_INPUT_ARR_FILES[${i}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Temperature        |${CXR_TEMP_GRID_INPUT_ARR_FILES[${i}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Water vapor        |${CXR_VAPOR_INPUT_ARR_FILES[${i}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Cloud/rain         |${CXR_CLOUD_GRID_INPUT_ARR_FILES[${i}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Vertical diffsvty  |${CXR_KV_GRID_INPUT_ARR_FILES[${i}]}" >> ${CXR_MODEL_CTRL_FILE} 
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Area emiss         |${CXR_EMISS_INPUT_ARR_FILES[${i}]}" >> ${CXR_MODEL_CTRL_FILE}
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
		ln -s ${CXR_MODEL_CTRL_FILE} CAMx.in || return $CXR_RET_ERROR
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
	local outfile
	
	# The CAMx.in file is in the state dir
	cd ${CXR_STATE_DIR}
	
	# Call the executable while collecting stderr and stdout
	$CXR_MODEL_EXEC 2>&1 | tee -a $CXR_LOG
	
	retval=$?

	outfile=$(cxr_common_evaluate_rule "$CXR_OUT_FILE_RULE")
	
	main.log  "This is the content of the outfile:"
	
	cat $outfile 2>&1 | tee -a $CXR_LOG
	
	if [[ $retval -ne 0 ]]
	then
		main.die_gracefully "CAMx has returned a non-zero status for $CXR_DATE"
	fi
	
	# go back
	cd ${CXR_RUN_DIR}
}

################################################################################
# Function: model
#
# High level function to call this model
################################################################################
function model()
################################################################################
{
		# Do we run the model?
		if [[ "$CXR_RUN_MODEL" == true  ]]
		then
		
			# common.state.storeState checks if we have finished this and if we need to continue
			if [[ ! $(common.state.storeState ${CXR_STATE_START}) == true  ]]
			then
			
				# If we do not run the first day, its a restart
				if [[ "$(common.date.isFirstDayOfSimulation?)" == false  ]]
				then
					# This must be a restart!
					CXR_RESTART=true
				else
					# Nope.
					CXR_RESTART=false
				fi
			
				main.log -B  "Running $CXR_MODEL_EXEC for day $CXR_DATE"
				
				#  --- Execute the model and write stderr and stdout to CXR_LOG ---
				set_variables
				
				#  --- Create the input file - will be stored in the state directory 
				#      but a link called CAMx.in wil be created where the CAMx binary is located
				write_model_control_file				
				
				if [[ $(common.check.preconditions) == false  ]]
				then
					main.log  "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
					# We notify the caller of the problem
					return $CXR_RET_ERR_PRECONDITIONS
				fi
	
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
					# We notify the caller of the problem
					return $CXR_RET_ERR_POSTCONDITIONS
				fi
				
				# We store the fact model run was completed
				common.state.storeState ${CXR_STATE_STOP} > /dev/null
				
			else
				main.log  "Stage was already started, therefore we do not run it. I assume this is a restart - we try to catch up!"
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

	for DAY_OFFSET in $(seq 0 $((${CXR_NUMBER_OF_SIM_DAYS} -1 )) )
	do
		# Initialise the date variables 
		common.date.setVars "$CXR_START_DATE" "$DAY_OFFSET"
		
		set_variables
		
		write_model_control_file
	done
	
	# Reset date variables for first day
	common.date.setVars "$CXR_START_DATE" "0"
	
	exit 0
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


