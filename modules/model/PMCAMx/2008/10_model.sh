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
# Function: set_model_variables
#
# Sets the variables (on a daily basis)
################################################################################	
function set_model_variables() 
################################################################################
{	
	# First of all, reset checks.
	# We will later continuously add entries to these 2 lists.
	# CAREFUL: If you add files to CXR_CHECK_THESE_OUTPUT_FILES,
	# these are deleted if he user runs the -F option. Do note mik up with input files!
	CXR_CHECK_THESE_INPUT_FILES=
	CXR_CHECK_THESE_OUTPUT_FILES=
	
	########################################################################
	# Per-day settings
	########################################################################
	
	# Empty expansion is not allowed
	
	# Name of the CAMx.in file
	CXR_CAMXIN=$(cxr_common_evaluate_rule "$CXR_CAMXIN_RULE" false CXR_CAMXIN_RULE)
	
	# Not directly checkable, Start of all output file names without extension
	CXR_ROOT_OUTPUT=$(cxr_common_evaluate_rule "$CXR_ROOT_OUTPUT_FILE_RULE" false CXR_ROOT_OUTPUT_FILE_RULE)
	
	# This is not a file (hence no _FILE at the end of the name)
	CXR_RT_ROOT_OUTPUT=$(cxr_common_evaluate_rule "$CXR_RT_ROOT_OUTPUT_FILE_RULE" false CXR_RT_ROOT_OUTPUT_FILE_RULE)
	
	# This is not a file (hence no _FILE at the end of the name)
	CXR_SA_ROOT_OUTPUT=$(cxr_common_evaluate_rule "$CXR_SA_ROOT_OUTPUT_FILE_RULE" false CXR_SA_ROOT_OUTPUT_FILE_RULE)
	
	########################################################################
	# Dry and real need the same variables set
	if [ "$CXR_HOLLOW" == false -o "$CXR_DRY" == true ]
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

		
		if [ "$(cxr_common_is_first_day)" == true ]
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
		if [ "$CXR_PLUME_IN_GRID" == true ]
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
		if [ "$CXR_PROBING_TOOL" == "OSAT" -o "$CXR_PROBING_TOOL" == "PSAT" -o "$CXR_PROBING_TOOL" == "GOAT" -o "$CXR_PROBING_TOOL" == "APCA" ] 
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
		elif [ "$CXR_PROBING_TOOL" == "DDM" ] 
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
		elif [ "$CXR_PROBING_TOOL" == "RTRAC" ] 
		then
			CXR_RT_MASTER_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_RT_MASTER_RESTART_FILE_RULE" false CXR_RT_MASTER_RESTART_FILE_RULE)
			CXR_RT_NESTED_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_RT_NESTED_RESTART_FILE_RULE" false CXR_RT_NESTED_RESTART_FILE_RULE)
		fi
		
		################################################################
		# General Probing support
		################################################################
		if [ "$CXR_PROBING_TOOL" != "None" ]
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
	
	echo "CAMx Version       |VERSION4.0" > ${CXR_CAMXIN}
	echo "Run Message        |${CXR_RUN}" >> ${CXR_CAMXIN} 
	echo "Root output name   |$CXR_ROOT_OUTPUT" >> ${CXR_CAMXIN} 
	echo "Start yr/mo/dy/hr  |${CXR_YEAR_S} ${CXR_MONTH} ${CXR_DAY} ${CXR_START_HOUR}." >> ${CXR_CAMXIN} 
	echo "End   yr/mo/dy/hr  |${CXR_YEAR_S} ${CXR_MONTH} ${CXR_DAY} ${CXR_STOP_HOUR}." >> ${CXR_CAMXIN} 
	echo "DT:max,met,ems,out |${CXR_MAXIMUM_TIMESTEP} ${CXR_MET_INPUT_FREQUENCY} ${CXR_EMS_INPUT_FREQUENCY} ${CXR_OUTPUT_FREQUENCY}" >> ${CXR_CAMXIN} 
	echo "nx,ny,nz           |${CXR_MASTER_GRID_COLUMNS}   ${CXR_MASTER_GRID_ROWS}   ${CXR_NUMBER_OF_LAYERS[1]}" >> ${CXR_CAMXIN} 
	echo "sCoordinate ID     |${CXR_MAP_PROJECTION}" >> ${CXR_CAMXIN}
	
	case ${CXR_MAP_PROJECTION} in
	
		LAMBERT) echo "xorg,yorg,dx,dy... |${CXR_MASTER_ORIGIN_XCOORD} ${CXR_MASTER_ORIGIN_YCOORD}  ${CXR_MASTER_CELL_XSIZE}  ${CXR_MASTER_CELL_YSIZE} ${CXR_LAMBERT_CENTER_LONGITUDE} ${CXR_LAMBERT_CENTER_LATITUDE} ${CXR_LAMBERT_TRUE_LATITUDE1} ${CXR_LAMBERT_TRUE_LATITUDE2}" >> ${CXR_CAMXIN} ;;
	  LATLON)  echo "long,lat,dx,dy     |${CXR_MASTER_ORIGIN_XCOORD} ${CXR_MASTER_ORIGIN_YCOORD}  ${CXR_MASTER_CELL_XSIZE}  ${CXR_MASTER_CELL_YSIZE} " >> ${CXR_CAMXIN} ;;
	  UTM)     echo "x,y,dx,dy,zone     |${CXR_MASTER_ORIGIN_XCOORD} ${CXR_MASTER_ORIGIN_YCOORD}  ${CXR_MASTER_CELL_XSIZE}  ${CXR_MASTER_CELL_YSIZE} ${CXR_UTM_ZONE} " >> ${CXR_CAMXIN} ;;
	  POLAR)   echo "x,y,dx,dy,lo_p,la_p|${CXR_MASTER_ORIGIN_XCOORD} ${CXR_MASTER_ORIGIN_YCOORD}  ${CXR_MASTER_CELL_XSIZE}  ${CXR_MASTER_CELL_YSIZE} ${CXR_POLAR_LONGITUDE_POLE} ${CXR_POLAR_LATITUDE_POLE}" >> ${CXR_CAMXIN} ;;
	  *) cxr_main_die_gracefully "Map projection ${CXR_MAP_PROJECTION} currently not supported!"
	
	esac
	
	echo "time zone          |${CXR_TIME_ZONE}" >> ${CXR_CAMXIN} 
	
	echo "PiG parameters     |${CXR_PIG_MAX_PUFF_LENGTH} ${CXR_PIG_MAX_PUFF_AGE}" >> ${CXR_CAMXIN} # Maxiumum puff length [m], maximum puff age [h]
	echo "Avg output species |${CXR_NUMBER_OF_OUTPUT_SPECIES}" >> ${CXR_CAMXIN} 
	
#	echo "                   |NO        NO2       O3        PAN       PAN2      MPAN" >> ${CXR_CAMXIN}
#	echo "                   |PBZN      NPHE      RNO3      CRES      DCB2      DCB3" >> ${CXR_CAMXIN}
#	echo "                   |HNO4      BALD      HONO      XN        HCHO      CCHO" >> ${CXR_CAMXIN}
#	echo "                   |RCHO      BACL      PROD      DCB1      PHEN      ISOP" >> ${CXR_CAMXIN}
#	echo "                   |ISPD      MVK       METH      MGLY      GLY       TERP" >> ${CXR_CAMXIN}
#	echo "                   |BPIN      LIMO      MONO      SESQ      HNO3      HO2H" >> ${CXR_CAMXIN}
#	echo "                   |HC2H      CO2H      CO3H      RC2H      RC3H      ACET" >> ${CXR_CAMXIN}
#	echo "                   |MEK       MEOH      COOH      ROOH      CO        ETHE" >> ${CXR_CAMXIN}
#	echo "                   |ALK1      ALK2      ALK3      ALK4      ALK5      ARO1" >> ${CXR_CAMXIN}
#	echo "                   |ARO2      OLE1      OLE2      NXOY      SO2       SULF" >> ${CXR_CAMXIN}
#	echo "                   |NH3       CPO1      CPO2      CPO3      CPO4      CPO5" >> ${CXR_CAMXIN}
#	echo "                   |CPO6      CPO7      CPO8      CPO9      CPO10     COO1" >> ${CXR_CAMXIN}
#	echo "                   |COO2      COO3      COO4      COO5      COO6      COO7" >> ${CXR_CAMXIN}
#	echo "                   |COO8      COO9      COO10     CBS1      CBS2      CBS3" >> ${CXR_CAMXIN}
#	echo "                   |CBS4      CAS1      CAS2      CAS3      CAS4      APO1_1" >> ${CXR_CAMXIN}
#	echo "                   |APO1_2    APO1_3    APO1_4    APO1_5    APO1_6    APO1_7" >> ${CXR_CAMXIN}
#	echo "                   |APO1_8    APO1_9    APO1_10   APO2_1    APO2_2    APO2_3" >> ${CXR_CAMXIN}
#	echo "                   |APO2_4    APO2_5    APO2_6    APO2_7    APO2_8    APO2_9" >> ${CXR_CAMXIN}
#	echo "                   |APO2_10   APO3_1    APO3_2    APO3_3    APO3_4    APO3_5" >> ${CXR_CAMXIN}
#	echo "                   |APO3_6    APO3_7    APO3_8    APO3_9    APO3_10   APO4_1" >> ${CXR_CAMXIN}
#	echo "                   |APO4_2    APO4_3    APO4_4    APO4_5    APO4_6    APO4_7" >> ${CXR_CAMXIN}
#	echo "                   |APO4_8    APO4_9    APO4_10   APO5_1    APO5_2    APO5_3" >> ${CXR_CAMXIN}
#	echo "                   |APO5_4    APO5_5    APO5_6    APO5_7    APO5_8    APO5_9" >> ${CXR_CAMXIN}
#	echo "                   |APO5_10   APO6_1    APO6_2    APO6_3    APO6_4    APO6_5" >> ${CXR_CAMXIN}
#	echo "                   |APO6_6    APO6_7    APO6_8    APO6_9    APO6_10   APO7_1" >> ${CXR_CAMXIN}
#	echo "                   |APO7_2    APO7_3    APO7_4    APO7_5    APO7_6    APO7_7" >> ${CXR_CAMXIN}
#	echo "                   |APO7_8    APO7_9    APO7_10   APO8_1    APO8_2    APO8_3" >> ${CXR_CAMXIN}
#	echo "                   |APO8_4    APO8_5    APO8_6    APO8_7    APO8_8    APO8_9" >> ${CXR_CAMXIN}
#	echo "                   |APO8_10   APO9_1    APO9_2    APO9_3    APO9_4    APO9_5" >> ${CXR_CAMXIN}
#	echo "                   |APO9_6    APO9_7    APO9_8    APO9_9    APO9_10   APO10_1" >> ${CXR_CAMXIN}
#	echo "                   |APO10_2   APO10_3   APO10_4   APO10_5   APO10_6   APO10_7" >> ${CXR_CAMXIN}
#	echo "                   |APO10_8   APO10_9   APO10_10  AOO1_1    AOO1_2    AOO1_3" >> ${CXR_CAMXIN}
#	echo "                   |AOO1_4    AOO1_5    AOO1_6    AOO1_7    AOO1_8    AOO1_9" >> ${CXR_CAMXIN}
#	echo "                   |AOO1_10   AOO2_1    AOO2_2    AOO2_3    AOO2_4    AOO2_5" >> ${CXR_CAMXIN}
#	echo "                   |AOO2_6    AOO2_7    AOO2_8    AOO2_9    AOO2_10   AOO3_1" >> ${CXR_CAMXIN}
#	echo "                   |AOO3_2    AOO3_3    AOO3_4    AOO3_5    AOO3_6    AOO3_7" >> ${CXR_CAMXIN}
#	echo "                   |AOO3_8    AOO3_9    AOO3_10   AOO4_1    AOO4_2    AOO4_3" >> ${CXR_CAMXIN}
#	echo "                   |AOO4_4    AOO4_5    AOO4_6    AOO4_7    AOO4_8    AOO4_9" >> ${CXR_CAMXIN}
#	echo "                   |AOO4_10   AOO5_1    AOO5_2    AOO5_3    AOO5_4    AOO5_5" >> ${CXR_CAMXIN}
#	echo "                   |AOO5_6    AOO5_7    AOO5_8    AOO5_9    AOO5_10   AOO6_1" >> ${CXR_CAMXIN}
#	echo "                   |AOO6_2    AOO6_3    AOO6_4    AOO6_5    AOO6_6    AOO6_7" >> ${CXR_CAMXIN}
#	echo "                   |AOO6_8    AOO6_9    AOO6_10   AOO7_1    AOO7_2    AOO7_3" >> ${CXR_CAMXIN}
#	echo "                   |AOO7_4    AOO7_5    AOO7_6    AOO7_7    AOO7_8    AOO7_9" >> ${CXR_CAMXIN}
#	echo "                   |AOO7_10   AOO8_1    AOO8_2    AOO8_3    AOO8_4    AOO8_5" >> ${CXR_CAMXIN}
#	echo "                   |AOO8_6    AOO8_7    AOO8_8    AOO8_9    AOO8_10   AOO9_1" >> ${CXR_CAMXIN}
#	echo "                   |AOO9_2    AOO9_3    AOO9_4    AOO9_5    AOO9_6    AOO9_7" >> ${CXR_CAMXIN}
#	echo "                   |AOO9_8    AOO9_9    AOO9_10   AOO10_1   AOO10_2   AOO10_3" >> ${CXR_CAMXIN}
#	echo "                   |AOO10_4   AOO10_5   AOO10_6   AOO10_7   AOO10_8   AOO10_9" >> ${CXR_CAMXIN}
#	echo "                   |AOO10_10  ABS1_1    ABS1_2    ABS1_3    ABS1_4    ABS1_5" >> ${CXR_CAMXIN}
#	echo "                   |ABS1_6    ABS1_7    ABS1_8    ABS1_9    ABS1_10   ABS2_1" >> ${CXR_CAMXIN}
#	echo "                   |ABS2_2    ABS2_3    ABS2_4    ABS2_5    ABS2_6    ABS2_7" >> ${CXR_CAMXIN}
#	echo "                   |ABS2_8    ABS2_9    ABS2_10   ABS3_1    ABS3_2    ABS3_3" >> ${CXR_CAMXIN}
#	echo "                   |ABS3_4    ABS3_5    ABS3_6    ABS3_7    ABS3_8    ABS3_9" >> ${CXR_CAMXIN}
#	echo "                   |ABS3_10   ABS4_1    ABS4_2    ABS4_3    ABS4_4    ABS4_5" >> ${CXR_CAMXIN}
#	echo "                   |ABS4_6    ABS4_7    ABS4_8    ABS4_9    ABS4_10   AAS1_1" >> ${CXR_CAMXIN}
#	echo "                   |AAS1_2    AAS1_3    AAS1_4    AAS1_5    AAS1_6    AAS1_7" >> ${CXR_CAMXIN}
#	echo "                   |AAS1_8    AAS1_9    AAS1_10   AAS2_1    AAS2_2    AAS2_3" >> ${CXR_CAMXIN}
#	echo "                   |AAS2_4    AAS2_5    AAS2_6    AAS2_7    AAS2_8    AAS2_9" >> ${CXR_CAMXIN}
#	echo "                   |AAS2_10   AAS3_1    AAS3_2    AAS3_3    AAS3_4    AAS3_5" >> ${CXR_CAMXIN}
#	echo "                   |AAS3_6    AAS3_7    AAS3_8    AAS3_9    AAS3_10   AAS4_1" >> ${CXR_CAMXIN}
#	echo "                   |AAS4_2    AAS4_3    AAS4_4    AAS4_5    AAS4_6    AAS4_7" >> ${CXR_CAMXIN}
#	echo "                   |AAS4_8    AAS4_9    AAS4_10   POC_1     POC_2     POC_3" >> ${CXR_CAMXIN}
#	echo "                   |POC_4     POC_5     POC_6     POC_7     POC_8     POC_9" >> ${CXR_CAMXIN}
#	echo "                   |POC_10    PEC_1     PEC_2     PEC_3     PEC_4     PEC_5" >> ${CXR_CAMXIN}
#	echo "                   |PEC_6     PEC_7     PEC_8     PEC_9     PEC_10    CRST_1" >> ${CXR_CAMXIN}
#	echo "                   |CRST_2    CRST_3    CRST_4    CRST_5    CRST_6    CRST_7" >> ${CXR_CAMXIN}
#	echo "                   |CRST_8    CRST_9    CRST_10   PH2O_1    PH2O_2    PH2O_3" >> ${CXR_CAMXIN}
#	echo "                   |PH2O_4    PH2O_5    PH2O_6    PH2O_7    PH2O_8    PH2O_9" >> ${CXR_CAMXIN}
#	echo "                   |PH2O_10   PCL_1     PCL_2     PCL_3     PCL_4     PCL_5" >> ${CXR_CAMXIN}
#	echo "                   |PCL_6     PCL_7     PCL_8     PCL_9     PCL_10    NA_1" >> ${CXR_CAMXIN}
#	echo "                   |NA_2      NA_3      NA_4      NA_5      NA_6      NA_7" >> ${CXR_CAMXIN}
#	echo "                   |NA_8      NA_9      NA_10     PNH4_1    PNH4_2    PNH4_3" >> ${CXR_CAMXIN}
#	echo "                   |PNH4_4    PNH4_5    PNH4_6    PNH4_7    PNH4_8    PNH4_9" >> ${CXR_CAMXIN}
#	echo "                   |PNH4_10   PNO3_1    PNO3_2    PNO3_3    PNO3_4    PNO3_5" >> ${CXR_CAMXIN}
#	echo "                   |PNO3_6    PNO3_7    PNO3_8    PNO3_9    PNO3_10   PSO4_1" >> ${CXR_CAMXIN}
#	echo "                   |PSO4_2    PSO4_3    PSO4_4    PSO4_5    PSO4_6    PSO4_7" >> ${CXR_CAMXIN}
#	echo "                   |PSO4_8    PSO4_9    PSO4_10" >> ${CXR_CAMXIN}
	
	
	# PMCAMx wants the species to be listed in grops of 6 each, where the distance between each column is 10 bytes
	for i in $(seq 1 $CXR_NUMBER_OF_OUTPUT_SPECIES);
	do
	
		# If we are in the first round, add apropriate intro
		
		if [ $i -eq 1 ]
		then
			echo -n "                   |"  >> ${CXR_CAMXIN}
		fi
	
		# string lenght, sorry for the weird detour
		STR=${CXR_OUTPUT_SPECIES_NAMES[${i}]}
		cxr_common_len=${#STR}
		
		# What's left of the 10 Bytes?
		NUM_SPACES=$(( ${CXR_SPECIES_COLUMN_WIDTH:-10} - $cxr_common_len ))
		
		# Correct if we chopped all away
		if [ $NUM_SPACES -lt 0 ]
		then
			cxr_main_logger "${FUNCNAME}" "Attention: Either your species names are to long or the column spacing is to small!"
			NUM_SPACES=0
		fi
		
		# Generate that many spaces (the printf approach in cxr_main_logger does not work with spaces!!)
		y=0 
		SPACES=
		while [ "$y" -lt $NUM_SPACES ]; do
			SPACES=" $SPACES"
			y=$(($y + 1))
		done 

		# Do we need to start a new line?
		if [ $(expr ${i} % ${CXR_SPECIES_COLUMNS:-6}) -eq 0 ]
		then
			# Start new Line, no spaces at the end
			echo ${CXR_OUTPUT_SPECIES_NAMES[${i}]} >> ${CXR_CAMXIN}
			
			# Already add new column if there are species left
			if [ $i -lt $CXR_NUMBER_OF_OUTPUT_SPECIES ]
			then
				echo -n "                   |"  >> ${CXR_CAMXIN}
			fi
			
		else
			# No new Line, spaces if it is not last Element
			if [ $i -lt $CXR_NUMBER_OF_OUTPUT_SPECIES ]
			then
				echo -n ${CXR_OUTPUT_SPECIES_NAMES[${i}]}"${SPACES}" >> ${CXR_CAMXIN}
			else
				# But: if this was the last element, we need a newline and no spaces
				echo ${CXR_OUTPUT_SPECIES_NAMES[${i}]} >> ${CXR_CAMXIN}
			fi
		fi
	done
	
	if [ ${CXR_NUMBER_OF_GRIDS} -lt 2 ]
	then
		#PMCAMx expects a 0 insted of 1 if there is no nesting (that might be true for CAMx as well?)
		echo "# nested grids     |0" >> ${CXR_CAMXIN}
	else
		echo "# nested grids     |${CXR_NUMBER_OF_GRIDS}" >> ${CXR_CAMXIN}
	fi	
	
	# Set up Nested grid data
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "i1,i2,j1,j2,nz,mesh|${CXR_NEST_BEG_I_INDEX[${i}]} ${CXR_NEST_END_I_INDEX[${i}]} ${CXR_NEST_BEG_J_INDEX[${i}]} ${CXR_NEST_END_J_INDEX[${i}]} ${CXR_NUMBER_OF_LAYERS[${i}]} ${CXR_NEST_MESHING_FACTOR[${i}]}" >> ${CXR_CAMXIN} 
	done
	
	echo "SMOLAR,BOTT, PPM?  |${CXR_ADVECTION_SOLVER}" >> ${CXR_CAMXIN} 
	echo "Chemistry solver   |${CXR_CHEMISTRY_SOLVER}" >> ${CXR_CAMXIN} 
	echo "Restart            |${CXR_RESTART}" >> ${CXR_CAMXIN} 
	echo "Chemistry          |${CXR_CHEMISTRY}" >> ${CXR_CAMXIN} 
	echo "Dry dep            |${CXR_DRY_DEPOSITION}" >> ${CXR_CAMXIN} 
	echo "Wet dep            |${CXR_WET_DEPOSITION}" >> ${CXR_CAMXIN} 
	
	# In this version, this is a flag
	if [ "${CXR_PIG_SUBMODEL:-None}" == None ]
	then
		echo "PiG submodel       |false" >> ${CXR_CAMXIN} 
	else
		echo "PiG submodel       |true" >> ${CXR_CAMXIN} 
	fi
	
	echo "Staggered winds    |${CXR_STAGGERED_WINDS}" >> ${CXR_CAMXIN} 
	echo "Treat area emiss   |${CXR_GRIDDED_EMISSIONS}" >> ${CXR_CAMXIN} 
	echo "Treat point emiss  |${CXR_POINT_EMISSIONS}" >> ${CXR_CAMXIN}
	
	# Is this the same idea?
	echo "1-day emiss inputs |${CXR_IGNORE_EMISSION_DATES}" >> ${CXR_CAMXIN} 
	echo "3-D average file   |${CXR_AVERAGE_OUTPUT_3D}" >> ${CXR_CAMXIN}
	
	# In this version, this is a flag
	# In this version, this is a flag
	if [ "${CXR_PROBING_TOOL:-None}" == None ]
	then
		echo "Probing tools      |false" >> ${CXR_CAMXIN} 
	else
		echo "Probing tools      |true" >> ${CXR_CAMXIN} 
	fi
	
	# Put specific info here (see sections 5-7 in manual)
	
	echo "Chemparam          |${CXR_CHEMPARAM_INPUT_FILE}" >> ${CXR_CAMXIN} 
	echo "Photolysis rates   |${CXR_PHOTOLYIS_RATES_INPUT_FILE}" >> ${CXR_CAMXIN} 
	echo "Landuse            |${CXR_LANDUSE_GRID_INPUT_ARR_FILES[1]}" >> ${CXR_CAMXIN} 
	echo "Height/pressure    |${CXR_ZP_GRID_INPUT_ARR_FILES[1]}" >> ${CXR_CAMXIN} 
	echo "Wind               |${CXR_WIND_GRID_INPUT_ARR_FILES[1]}" >> ${CXR_CAMXIN} 
	echo "Temperature        |${CXR_TEMP_GRID_INPUT_ARR_FILES[1]}" >> ${CXR_CAMXIN} 
	echo "Water vapor        |${CXR_VAPOR_INPUT_ARR_FILES[1]}" >> ${CXR_CAMXIN} 
	echo "Cloud/rain         |${CXR_CLOUD_GRID_INPUT_ARR_FILES[1]}" >> ${CXR_CAMXIN} 
	echo "Vertical diffsvty  |${CXR_KV_GRID_INPUT_ARR_FILES[1]}" >> ${CXR_CAMXIN} 
	echo "Initial conditions |${CXR_INITIAL_CONDITIONS_INPUT_FILE}" >> ${CXR_CAMXIN} 
	echo "Boundary conditions|${CXR_BOUNDARY_CONDITIONS_INPUT_FILE}" >> ${CXR_CAMXIN} 
	echo "Top concentration  |${CXR_TOP_CONCENTRATIONS_INPUT_FILE}" >> ${CXR_CAMXIN} 
	echo "Albedo/haze/ozone  |${CXR_ALBEDO_HAZE_OZONE_INPUT_FILE}" >> ${CXR_CAMXIN} 
	echo "Point emiss        |${CXR_POINT_SOURCES_INPUT_FILE}" >> ${CXR_CAMXIN} 
	echo "Area emiss         |${CXR_EMISS_INPUT_ARR_FILES[1]}" >> ${CXR_CAMXIN}
	
	# Now we provide the data for the nested grids. The structure of the file
	# requires to repeat the loop over the grids (this was improved in CAMx 4.x!)
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Landuse            |${CXR_LANDUSE_GRID_INPUT_ARR_FILES[${i}]}" >> ${CXR_CAMXIN} 
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Height/pressure    |${CXR_ZP_GRID_INPUT_ARR_FILES[${i}]]}" >> ${CXR_CAMXIN}
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Wind               |${CXR_WIND_GRID_INPUT_ARR_FILES[${i}]}" >> ${CXR_CAMXIN} 
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Temperature        |${CXR_TEMP_GRID_INPUT_ARR_FILES[${i}]}" >> ${CXR_CAMXIN} 
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Water vapor        |${CXR_VAPOR_INPUT_ARR_FILES[${i}]}" >> ${CXR_CAMXIN} 
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Cloud/rain         |${CXR_CLOUD_GRID_INPUT_ARR_FILES[${i}]}" >> ${CXR_CAMXIN} 
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Vertical diffsvty  |${CXR_KV_GRID_INPUT_ARR_FILES[${i}]}" >> ${CXR_CAMXIN} 
	done
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
		echo "Area emiss         |${CXR_EMISS_INPUT_ARR_FILES[${i}]}" >> ${CXR_CAMXIN}
	done

	echo "Master restart file|${CXR_MASTER_GRID_RESTART_INPUT_FILE:-}" >> ${CXR_CAMXIN}
	
	if [ ${CXR_NUMBER_OF_GRIDS} -gt 1 ]
	then
		echo "Nested restart file|${CXR_NESTED_GRID_RESTART_INPUT_FILE:-}" >> ${CXR_CAMXIN}
	fi
	
	if [ "$CXR_PLUME_IN_GRID" == true ]
	then
		echo "PiG restart file   |${CXR_PIG_RESTART_INPUT_FILE:-}" >> ${CXR_CAMXIN}
	fi
	

#	################################################################
#	# OSAT, PSAT,  GOAT or APCA ia yet to be implemented here...
#	################################################################
#	if [ "$CXR_PROBING_TOOL" == "OSAT" -o "$CXR_PROBING_TOOL" == "PSAT" -o "$CXR_PROBING_TOOL" == "GOAT" -o "$CXR_PROBING_TOOL" == "APCA" ] 
#	then
#	
#		echo " !---${CXR_PROBING_TOOL}--------------------------------------------------------------------" >> ${CXR_CAMXIN} 
#		echo " &SA_Control" >> ${CXR_CAMXIN} 
#		
#		echo " SA_File_Root              = '${CXR_SA_ROOT_OUTPUT}'," >> ${CXR_CAMXIN} 
#		echo " SA_Summary_Output         = .${CXR_SA_SUMMARY_OUTPUT}.," >> ${CXR_CAMXIN} 
#		
#		echo " SA_Master_Sfc_Output         = .${CXR_SA_MASTER_SFC_OUTPUT}.," >> ${CXR_CAMXIN} 
#		echo " SA_Nested_Sfc_Output         = .${CXR_SA_NESTED_SFC_OUTPUT}.," >> ${CXR_CAMXIN} 
#		echo " SA_Stratify_Boundary         = .${CXR_SA_STRATIFY_BOUNDARY}.," >> ${CXR_CAMXIN} 
#		echo " SA_Number_of_Source_Regions  = ${CXR_SA_NUMBER_OF_SOURCE_REGIONS}," >> ${CXR_CAMXIN} 
#		echo " SA_Number_of_Source_Groups   = ${CXR_SA_NUMBER_OF_SOURCE_GROUPS}," >> ${CXR_CAMXIN} 
#		echo " Use_Leftover_Group           = .${CXR_USE_LEFTOVER_GROUP}.," >> ${CXR_CAMXIN} 
#		echo " Number_of_Timing_Releases    = ${CXR_NUMBER_OF_TIMING_RELEASES}, " >> ${CXR_CAMXIN} 
#		echo " PSAT_Treat_SULFATE_Class     = .${CXR_PSAT_TREAT_SULFATE_CLASS}.," >> ${CXR_CAMXIN} 
#		echo " PSAT_Treat_NITRATE_Class     = .${CXR_PSAT_TREAT_NITRATE_CLASS}.," >> ${CXR_CAMXIN} 
#		echo " PSAT_Treat_SOA_Class         = .${CXR_PSAT_TREAT_SOA_CLASS}.," >> ${CXR_CAMXIN} 
#		echo " PSAT_Treat_PRIMARY_Class     = .${CXR_PSAT_TREAT_PRIMARY_CLASS}.," >> ${CXR_CAMXIN} 
#		echo " PSAT_Treat_MERCURY_Class     = .${CXR_PSAT_TREAT_MERCURY_CLASS}.," >> ${CXR_CAMXIN} 
#		echo " PSAT_Treat_OZONE_Class       = .${CXR_PSAT_TREAT_OZONE_CLASS}.," >> ${CXR_CAMXIN} 
#		
#		echo " SA_Receptor_Definitions  = '${CXR_SA_RECEPTOR_DEFINITIONS_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		
#		echo " SA_Master_Restart        = '${CXR_SA_MASTER_RESTART_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		echo " SA_Nested_Restart        = '${CXR_SA_NESTED_RESTART_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		
#		# By grid
#		for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
#		do
#			echo " SA_Source_Area_Map(${i})    = '${CXR_SA_SOURCE_AREA_MAP_INPUT_ARR_FILES[${i}]}'," >> ${CXR_CAMXIN} 
#		done
#		
#		
#		if [ "${CXR_POINT_EMISSIONS}" == true ]
#		then
#			# By source group
#			for j in $(seq 1 $CXR_SA_NUMBER_OF_SOURCE_GROUPS);
#			do
#				echo " SA_Points_Group(${j})       = '${CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${j}]}'," >> ${CXR_CAMXIN} 
#			done
#		fi
#		
#		
#		if [ "${CXR_GRIDDED_EMISSIONS}" == true ]
#		then
#			# By source group
#			for j in $(seq 1 $CXR_SA_NUMBER_OF_SOURCE_GROUPS);
#			do
#				# and grid
#				for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
#				do
#					# This is not elegant, but it simulates a 2D Array
#					ELEMENT_NAME=CXR_SA_EMISS_GROUP_GRID_${j}_${i}_INPUT_FILE
#					echo " SA_Emiss_Group_Grid(${j},${i}) = '${!ELEMENT_NAME}'," >> ${CXR_CAMXIN} 
#			
#				done
#			done
#		fi
#		
#		echo " &END" >> ${CXR_CAMXIN} 
#	
#		
#	################################################################
#	# Must we run direct decoupled sensitivity analysis?
#	# DDM
#	################################################################
#	elif [ "$CXR_PROBING_TOOL" == "DDM" ] 
#	then
#
#		echo " !----Sensitivity Analysis (Direct Decoupled Method)-----------------------------" >> ${CXR_CAMXIN} 
#		echo " &DDM_Control" >> ${CXR_CAMXIN} 
#		
#		echo " DDM_File_Root                = '${CXR_DDM_ROOT_OUTPUT}'," >> ${CXR_CAMXIN} 
#		echo " DDM_Master_Sfc_Output        = .${CXR_DDM_MASTER_SFC_OUTPUT}.," >> ${CXR_CAMXIN} 
#		echo " DDM_Nested_Sfc_Output        = .${CXR_DDM_NESTED_SFC_OUTPUT}.," >> ${CXR_CAMXIN} 
#		echo " DDM_Stratify_Boundary        = .${CXR_DDM_STRATIFY_BOUNDARY}.," >> ${CXR_CAMXIN} 
#		echo " DDM_Number_of_Source_Regions = ${CXR_DDM_NUMBER_OF_SOURCE_REGIONS}," >> ${CXR_CAMXIN} 
#		echo " DDM_Number_of_Source_Groups  = ${CXR_DDM_NUMBER_OF_SOURCE_GROUPS}," >> ${CXR_CAMXIN} 
#		
#		echo " DDM_Initial_Conditions    = '${CXR_DDM_INITIAL_CONDITIONS_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		echo " DDM_Boundary_Conditions   = '${CXR_DDM_BOUNDARY_CONDITIONS_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		echo " DDM_Top_Concentrations    = '${CXR_DDM_TOP_CONCENTRATIONS_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		echo " DDM_Master_Restart        = '${CXR_DDM_MASTER_RESTART_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		echo " DDM_Nested_Restart        = '${CXR_DDM_NESTED_RESTART_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		
#		echo " DDM_Receptor_Definitions  = '${CXR_DDM_RECEPTOR_DEFINITIONS_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		
#		echo " Number_of_IC_Species_Groups = ${CXR_NUMBER_OF_IC_SPECIES_GROUPS}," >> ${CXR_CAMXIN} 
#		
#		# By initial conc group
#		for a in $(seq 1 "$CXR_NUMBER_OF_IC_SPECIES_GROUPS");
#		do
#			echo " IC_Species_Groups(${a})        = '${CXR_IC_SPECIES_GROUPS_INPUT_ARR_FILES[${a}]}'," >> ${CXR_CAMXIN} 
#		done
#		
#		echo " Number_of_BC_Species_Groups = ${CXR_NUMBER_OF_BC_SPECIES_GROUPS}," >> ${CXR_CAMXIN} 
#		
#		# By boundary condition group
#		for b in $(seq 1 "$CXR_NUMBER_OF_BC_SPECIES_GROUPS");
#		do
#			echo " BC_species_Groups(${b})        = '${CXR_BC_SPECIES_GROUPS_INPUT_ARR_FILES[${b}]}'," >> ${CXR_CAMXIN} 
#		done
#		
#		
#		echo " Number_of_EM_Species_Groups = ${CXR_NUMBER_OF_EM_SPECIES_GROUPS}," >> ${CXR_CAMXIN} 
#		
#		# By Emission group
#		for c in $(seq 1 "$CXR_NUMBER_OF_EM_SPECIES_GROUPS");
#		do
#			echo " Emis_Species_Groups(${c})      = '${CXR_EMIS_SPECIES_GROUPS[${c}]}'," >> ${CXR_CAMXIN} 
#		done
#		
#		# By grid
#		for i in $(seq 1 "$CXR_NUMBER_OF_GRIDS");
#		do
#			echo " DDM_Source_Area_Map(${i})    = '${CXR_DDM_SOURCE_AREA_MAP_INPUT_ARR_FILES[${i}]}'," >> ${CXR_CAMXIN}
#		done 
#		
#		if [ "${CXR_POINT_EMISSIONS}" == true ]
#		then
#			# By source group
#			for j in $(seq 1 "$CXR_DDM_NUMBER_OF_SOURCE_GROUPS");
#			do
#				echo " DDM_Points_Group(${j})       = '${CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${j}]}'," >> ${CXR_CAMXIN} 
#			done
#		fi
#		 
#		# By source group, by grid
#		if [ "${CXR_GRIDDED_EMISSIONS}" == true ]
#		then
#			# By source group
#			for j in $(seq 1 $CXR_DDM_NUMBER_OF_SOURCE_GROUPS);
#			do
#				
#				# and grid
#				for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
#				do
#					# This is not elegant, but it simulates a 2D Array
#					ELEMENT_NAME=CXR_DDM_EMISS_GROUP_GRID_${j}_${i}_INPUT_FILE
#					echo " DDM_Emiss_Group_Grid(${j},${i}) = '${!ELEMENT_NAME}'," >> ${CXR_CAMXIN} 
#				done
#			done
#		fi 
#		
#		echo " &END" >> ${CXR_CAMXIN}  
#
#	################################################################
#	# Must we run Reactive Tracer Source Apportionment?
#	# RTRAC (RT)
#	################################################################
#	elif [ "$CXR_PROBING_TOOL" == "RTRAC" ] 
#	then
#	
#		echo " !---RTRAC (Reactive Tracer Source Apportionment)-----------------------------------------------------------------" >> ${CXR_CAMXIN} 
#		echo " &RT_Control" >> ${CXR_CAMXIN} 
#		
#		echo " RT_File_Root            = '${CXR_RT_ROOT_OUTPUT}'," >> ${CXR_CAMXIN} 
#		
#		echo " RT_Initial_Conditions   = '${CXR_RT_INITIAL_CONDITIONS_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		echo " RT_Boundary_Conditions  = '${CXR_RT_BOUNDARY_CONDITIONS_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		echo " RT_Top_Concentrations   = '${CXR_RT_TOP_CONCENTRATIONS_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		
#		echo " RT_Master_Restart       = '${CXR_RT_MASTER_RESTART_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		echo " RT_Nested_Restart       = '${CXR_RT_NESTED_RESTART_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		
#		echo " RT_Chemistry_Parameters = '${CXR_RT_CHEMISTRY_PARAMETERS_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		echo " RT_Receptor_Definitions = '${CXR_RT_RECEPTOR_DEFINITIONS_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		echo " RT_Point_Sources        = '${CXR_RT_POINT_SOURCES_INPUT_FILE}'," >> ${CXR_CAMXIN} 
#		
#		if [ "$CXR_PLUME_IN_GRID" == true ]
#		then
#			echo " RT_PiG_Sample           = .${CXR_RT_PIG_SAMPLE}.,               ! Ignore if PiG = false" >> ${CXR_CAMXIN} 
#		fi
#		
#		# By grid
#		for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
#		do
#			echo " RT_Emiss_Grid(${i})    = '${CXR_RT_EMISS_GRID_INPUT_ARR_FILES[${i}]}'," >> ${CXR_CAMXIN}
#		done
#
#		
#		echo " &END" >> ${CXR_CAMXIN} 
#	
#	fi
#	
#	################################################################
#	# General Probing support
#	################################################################
#	if [ "$CXR_PROBING_TOOL" != "None" ]
#	then
#	
#		echo "!---------------Probing Tool General------------------------------------------" >> ${CXR_CAMXIN} 
#		echo " &PA_Control" >> ${CXR_CAMXIN} 
#		echo "" >> ${CXR_CAMXIN} 
#		echo " PA_File_Root         = '$CXR_PA_ROOT_OUTPUT'," >> ${CXR_CAMXIN} 
#		echo "" >> ${CXR_CAMXIN} 
#		echo " Number_of_PA_Domains = ${CXR_NUMBER_OF_PA_DOMAINS}," >> ${CXR_CAMXIN} 
#		
#		# Here we loop through the PA domains
#		for i in $(seq 1 $CXR_NUMBER_OF_PA_DOMAINS);
#		do
#			echo " Within_CAMx_Grid(${i})  = ${CXR_WITHIN_CAMX_GRID[${i}]},  ! Specify which CAMx grid that this PA domain is in" >> ${CXR_CAMXIN} 
#			echo " PA_Beg_I_Index(${i})    = ${CXR_PA_BEG_I_INDEX[${i}]}," >> ${CXR_CAMXIN} 
#			echo " PA_End_I_Index(${i})    = ${CXR_PA_END_I_INDEX[${i}]}," >> ${CXR_CAMXIN} 
#			echo " PA_Beg_J_Index(${i})    = ${CXR_PA_BEG_J_INDEX[${i}]}," >> ${CXR_CAMXIN} 
#			echo " PA_End_J_Index(${i})    = ${CXR_PA_END_J_INDEX[${i}]}," >> ${CXR_CAMXIN} 
#			echo " PA_Beg_K_Index(${i})    = ${CXR_PA_BEG_K_INDEX[${i}]}," >> ${CXR_CAMXIN} 
#			echo " PA_End_K_Index(${i})    = ${CXR_PA_END_K_INDEX[${i}]}," >> ${CXR_CAMXIN} 
#			echo "" >> ${CXR_CAMXIN} 
#		done
#		
#		echo "" >> ${CXR_CAMXIN} 
#		echo " &END" >> ${CXR_CAMXIN} 
#	fi
#	
#	echo "" >> ${CXR_CAMXIN} 
#	echo "!-------------------------------------------------------------------------------" >> ${CXR_CAMXIN} 
	
	##########################################################################
	# Now we need to link this file to the name "CAMx.in" in the state directory
	##########################################################################
	(
		cd ${CXR_STATE_DIR}  || return $CXR_RET_ERROR
		
		# Remove old link - does not matter if this fails
		rm -f CAMx.in
		
		# New one
		ln -s ${CXR_CAMXIN} CAMx.in || return $CXR_RET_ERROR
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
	# The CAMx.in file is in the state dir
	cd ${CXR_STATE_DIR}
	
	# Call the executable while collecting stderr and stdout
	$CXR_MODEL_EXEC 2>&1 | tee -a $CXR_LOG
	
	OUTFILE=$(cxr_common_evaluate_rule "$CXR_OUT_FILE_RULE")
	
	cxr_main_logger "${FUNCNAME}" "This is the content of the outfile:"
	
	cat $OUTFILE 2>&1 | tee -a $CXR_LOG
	
	# Here, we need to implement a sensible retval check!
	
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
		if [ "$CXR_RUN_MODEL" == true ]
		then
		
			# cxr_common_store_state checks if we have finished this and if we need to continue
			if [ ! $(cxr_common_store_state ${CXR_STATE_START}) == true ]
			then
			
				# If we do not run the first day, its a restart
				if [ "$(cxr_common_is_first_day)" == false ]
				then
					# This must be a restart!
					CXR_RESTART=true
				else
					# Nope.
					CXR_RESTART=false
				fi
			
				cxr_main_logger -B "${FUNCNAME}" "Running $CXR_MODEL_EXEC for day $CXR_DATE"
				
				#  --- Execute the model and write stderr and stdout to CXR_LOG ---
				set_model_variables
				
				#  --- Create the input file - will be stored in the state directory 
				#      but a link called CAMx.in wil be created where the CAMx binary is located
				write_model_control_file				
				
				if [ $(cxr_common_check_preconditions) == false ]
				then
					cxr_main_logger "${FUNCNAME}" "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
					# We notify the caller of the problem
					return $CXR_RET_ERR_PRECONDITIONS
				fi
	
				if [ "$CXR_DRY" == false ]
				then
					execute_model
				else
					cxr_main_logger "${FUNCNAME}" "This is a dry run, $CXR_MODEL is not run"
				fi
			
				# Did we run properly?
				if [ $(cxr_common_check_result) == false ]
				then
					cxr_main_logger "${FUNCNAME}" "$CXR_MODEL Run was not successful!"
					# We notify the caller of the problem
					return $CXR_RET_ERR_POSTCONDITIONS
				fi
				
				# We store the fact model run was completed
				cxr_common_store_state ${CXR_STATE_STOP} > /dev/null
				
			else
				cxr_main_logger "${FUNCNAME}" "Stage was already started, therefore we do not run it. I assume this is a restart - we try to catch up!"
			fi
			
			else
			cxr_main_logger "${FUNCNAME}" "Model disabled (either in the config using CXR_RUN_MODEL=false or with the option -N)"
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
		if [ $(pwd) == / ]
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
		cxr_common_set_date_variables "$CXR_START_DATE" "$DAY_OFFSET"
		
		set_model_variables
		
		write_model_control_file
	done
	
	# Reset date variables for first day
	cxr_common_set_date_variables "$CXR_START_DATE" "0"
	
	exit 0
}




################################################################################
# Are we running stand-alone? 
################################################################################


# If the CXR_META_MODULE_NAME  is a subset of the progname,
# somebody started this script alone
# Normlly this is not allowed, exept to test using -t
if [ $(expr match "$progname" ".*$CXR_META_MODULE_NAME.*") -gt 0 ]
then

	# When using getopts, never directly call a function inside the case,
	# otherwise getopts does not process any parametres that come later
	while getopts ":dvFST" opt
	do
		case "${opt}" in
		
			d) CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_LOG_EXT="-dry" ;;
			v) CXR_USER_TEMP_VERBOSE=true ; echo "Enabling VERBOSE (-v) output. All lines starting with % would not be present otherwise" ;;
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
	if [ "${TEST_IT:-false}" == true ]
	then
		test_module
	fi
	
	usage
	
fi



################################################################################
# Code beyond this point is not executed in stand-alone operation
################################################################################


