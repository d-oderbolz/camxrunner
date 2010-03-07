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
# TODO: Implement retval check in execute_model
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

CXR_META_MODULE_DEPENDS_ON="all_model- all_once_preprocessors all_daily_preprocessors"

# Also for the management of parallel tasks
# If this is true, no new tasks will be given out as long as this runs
# Normaly only used for a model or parallelized preprocessors
CXR_META_MODULE_RUN_EXCLUSIVELY=true

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_MODEL}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=0

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=CAMx-v4.51-test

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
	
	# This is not a file (hence no _FILE at the end of the name)
	CXR_SA_RECEPTORIN=$(cxr_common_evaluate_rule "$CXR_SA_RECEPTOR_DEFINITIONS_FILE_RULE" false CXR_SA_RECEPTOR_DEFINITIONS_FILE_RULE) # could be named better! sorry... might do it later...
	
	########################################################################
	# Dry and real need the same variables set
	if [[  "$CXR_HOLLOW" == false || "$CXR_DRY" == true   ]]
	then
		# Real or dry run


		########################################################################
		# Set variables
		########################################################################
		
		CXR_PHOTOLYIS_RATES_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_PHOTOLYIS_RATES_FILE_RULE" false CXR_PHOTOLYIS_RATES_FILE_RULE)
		CXR_INITIAL_CONDITIONS_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_INITIAL_CONDITIONS_FILE_RULE" false CXR_INITIAL_CONDITIONS_FILE_RULE)
		CXR_BOUNDARY_CONDITIONS_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_BOUNDARY_CONDITIONS_FILE_RULE" false CXR_BOUNDARY_CONDITIONS_FILE_RULE)
		CXR_TOP_CONCENTRATIONS_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_TOP_CONCENTRATIONS_FILE_RULE" false CXR_TOP_CONCENTRATIONS_FILE_RULE)
		CXR_ALBEDO_HAZE_OZONE_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_ALBEDO_HAZE_OZONE_FILE_RULE" false CXR_ALBEDO_HAZE_OZONE_FILE_RULE)

		########################################################################
		# Define checks
		########################################################################
		
		
		if [[ "$(date_isFirstDayOfSimulation?)" == true  ]]
		then
			# Stuff that we need only the first day
			CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_INITIAL_CONDITIONS_INPUT_FILE"
		else
			## Only needed after the first day
			CXR_MASTER_GRID_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_MASTER_GRID_RESTART_FILE_RULE" false CXR_MASTER_GRID_RESTART_FILE_RULE)
			CXR_NESTED_GRID_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_NESTED_GRID_RESTART_FILE_RULE" false CXR_NESTED_GRID_RESTART_FILE_RULE)
		
			#Checks
			CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_MASTER_GRID_RESTART_INPUT_FILE $CXR_NESTED_GRID_RESTART_INPUT_FILE"
		
			if [[    "$CXR_PROBING_TOOL" == "OSAT" || "$CXR_PROBING_TOOL" == "PSAT" || "$CXR_PROBING_TOOL" == "GOAT" || "$CXR_PROBING_TOOL" == "APCA"     ]] 
			then
				CXR_SA_MASTER_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_SA_MASTER_RESTART_FILE_RULE" false CXR_SA_MASTER_RESTART_FILE_RULE)
				CXR_SA_NESTED_RESTART_INPUT_FILE=$(cxr_common_evaluate_rule "$CXR_SA_NESTED_RESTART_FILE_RULE" false CXR_SA_NESTED_RESTART_FILE_RULE)

			fi
		
		fi

		# Normal Checks
		CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES $CXR_PHOTOLYIS_RATES_INPUT_FILE $CXR_BOUNDARY_CONDITIONS_INPUT_FILE $CXR_TOP_CONCENTRATIONS_INPUT_FILE $CXR_ALBEDO_HAZE_OZONE_INPUT_FILE"

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
		
			# Output files must not be decompressed
			# We only want to check them, otherwise we dont need these values
			CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES $(cxr_common_evaluate_rule "$CXR_SA_INST_FILE_RULE" false CXR_SA_INST_FILE_RULE false) $(cxr_common_evaluate_rule "$CXR_SA_FINST_FILE_RULE" false CXR_SA_FINST_FILE_RULE false)"	
	
			#Source area specific
			for k in $(seq 1 $(( ${#SA_REGIONS_DOMAIN_NUMBERS[@]} - 1 )));
			do
				##Source Area
				CXR_SA_SOURCE_AREA_MAP_INPUT_ARR_FILES[${k}]=$(cxr_common_evaluate_rule "$CXR_SA_SOURCE_AREA_MAP_FILE_RULE" false CXR_SA_SOURCE_AREA_MAP_FILE_RULE) 
			
				##Checks
				CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_SA_SOURCE_AREA_MAP_INPUT_ARR_FILES[${k}]}"
			done
			
			for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);	
			do
				# expected output files (grid specific)
				# We only want to check them, otherwise we dont need these values
				CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES $(cxr_common_evaluate_rule "$CXR_SA_AVG_FILE_RULE" false CXR_SA_AVG_FILE_RULE false)"
				
				##Source Group
				for j in $(seq 1 $CXR_SA_NUMBER_OF_SOURCE_GROUPS);
				do
					eval CXR_SA_EMISS_GROUP_GRID_INPUT_${i}_${j}=$(cxr_common_evaluate_rule "$CXR_SA_EMISS_GROUP_GRID_INPUT_FILE_RULE" false CXR_SA_EMISS_GROUP_GRID_INPUT_FILE_RULE)
					dummyvar="CXR_SA_EMISS_GROUP_GRID_INPUT_${i}_${j}"
					##Checks
					CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${!dummyvar}"
				done
			done
			
			# Source group specific
			if [[ $CXR_POINT_EMISSIONS == true  ]]
			then
				for j in $(seq 1 $CXR_SA_NUMBER_OF_SOURCE_GROUPS);
				do
					CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${j}]=$(cxr_common_evaluate_rule "$CXR_SA_POINTS_GROUP_FILE_RULE" false CXR_SA_POINTS_GROUP_FILE_RULE)

					#Checks
					CXR_CHECK_THESE_INPUT_FILES="$CXR_CHECK_THESE_INPUT_FILES ${CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${j}]}"
				done
			fi
			
			## Receptor file
			#if [[  "$CXR_SA_RECEPTOR" == "true" && "$CXR_SA_RECEPTOR_FILE_EXISTS" == "false"   ]]
			#then
			#	#Checks (this time output)
			#	CXR_CHECK_THESE_OUTPUT_FILES="$CXR_CHECK_THESE_OUTPUT_FILES $CXR_SA_RECEPTOR "
			#fi
		
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
# Function: write_sa_receptor_definitions_file
#
# Writes a file containing the receptor definitions
#
################################################################################
function write_sa_receptor_definitions_file() 
################################################################################
{
	: # dummy command
	# the sa_receptor_definitions_file is created here...
	
	# If we start this and no directories are yet created, we are in trouble,
	# so let us create it (workaround)
	
	mkdir -p $(dirname ${CXR_SA_RECEPTORIN})
	
	# write file
	: > ${CXR_SA_RECEPTORIN}
	
	if [[ "$SA_POINT_RECEPTOR" == "true"  ]]
	then
		for i in $(seq 1 $SA_POINT_NUMBER);
		do
			printf %-15s 'POINT' >> ${CXR_SA_RECEPTORIN}
			printf %5s >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_POINT_RECEPTOR_NAMES[$i]} >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_POINT_X_COORD[$i]} >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_POINT_Y_COORD[$i]} >> ${CXR_SA_RECEPTORIN}
			echo "" >> ${CXR_SA_RECEPTORIN}
		done
	fi
	
	if [[ "$SA_SINGLE_CELL_RECEPTOR" == "true"  ]]
	then
		for i in $(seq 1 $SA_SCELL_NUMBER);
		do
			printf %-15s 'SINGLE CELL' >> ${CXR_SA_RECEPTORIN}
			printf %5s >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_SCELL_RECEPTOR_NAMES[$i]} >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_SCELL_GRID_NUMBER[$i]} >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_SCELL_XNUMBER[$i]} >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_SCELL_YNUMBER[$i]} >> ${CXR_SA_RECEPTORIN}
			echo "" >> ${CXR_SA_RECEPTORIN}
		done
	fi
	
	if [[ "$SA_CELL_AVERAGE_RECEPTOR" == "true"  ]]
	then
			for i in $(seq 1 $SA_CELLAVG_NUMBER);
			do
				printf %-15s 'CELL AVERAGE' >> ${CXR_SA_RECEPTORIN}
				printf %5s >> ${CXR_SA_RECEPTORIN}
				printf %-10s ${SA_CELLAVG_RECEPTOR_NAMES[$i]} >> ${CXR_SA_RECEPTORIN}
				printf %-10s ${SA_CELLAVG_GRID_NUMBER[$i]} >> ${CXR_SA_RECEPTORIN}
				printf %-10s ${SA_CELLAVG_NUMBER_OF_CELLS[$i]} >> ${CXR_SA_RECEPTORIN}
				echo "" >> ${CXR_SA_RECEPTORIN}
				for h in $(seq $(( ${SA_CELLAVG_DUMMIES[$(( $i - 1 ))]} + 1 )) $(( ${SA_CELLAVG_NUMBER_OF_CELLS[$i]} + ${SA_CELLAVG_DUMMIES[$(( $i - 1 ))]} )))
				do
					printf %-10s ${SA_CELLAVG_XNUMBER[$h]} >> ${CXR_SA_RECEPTORIN}
					printf %-10s ${SA_CELLAVG_YNUMBER[$h]} >> ${CXR_SA_RECEPTORIN}
					echo "" >> ${CXR_SA_RECEPTORIN}
				done
			done
	fi
	
	if [[ "$SA_WALL_OF_CELLS_RECEPTOR" == "true"  ]]
	then
		for i in $(seq 1 $SA_CELLWALL_NUMBER);
		do
			printf %-15s 'WALL OF CELLS' >> ${CXR_SA_RECEPTORIN}
			printf %5s >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_CELLWALL_RECEPTOR_NAMES[$i]} >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_CELLWALL_GRID_NUMBER[$i]} >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_CELLWALL_XSTART[$i]} >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_CELLWALL_XEND[$i]} >> ${CXR_SA_RECEPTORIN}
			echo "" >> ${CXR_SA_RECEPTORIN}
			printf %40s >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_CELLWALL_YSTART[$i]} >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_CELLWALL_YEND[$i]} >> ${CXR_SA_RECEPTORIN}
			echo "" >> ${CXR_SA_RECEPTORIN}
			printf %40s >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_CELLWALL_ZSTART[$i]} >> ${CXR_SA_RECEPTORIN}
			printf %-10s ${SA_CELLWALL_ZEND[$i]} >> ${CXR_SA_RECEPTORIN}
			echo "" >> ${CXR_SA_RECEPTORIN}
		done
		
	fi
	
}



################################################################################
# Function: write_model_control_file
#
# Writes a CAMx.in file using current settings
#
################################################################################
function write_model_control_file() 
################################################################################
{
	# Define & Initialize local vars
	local i
	local j
	local k
	local a
	local b
	local c
	
	
	# Clean the file away first
	: > ${CXR_MODEL_CTRL_FILE} 
	
	# From here, append (>>)
	echo " &CAMx_Control" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Run_Message = '${CXR_RUN}'," >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "!--- Model clock control ---" >> ${CXR_MODEL_CTRL_FILE} 
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Time_Zone        = ${CXR_TIME_ZONE},                 ! (0=UTC,5=EST,6=CST,7=MST,8=PST)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Restart          = .${CXR_RESTART}.," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Start_Date_Hour  = ${CXR_YEAR},${CXR_MONTH},${CXR_DAY},${CXR_START_HOUR},   ! (YYYY,MM,DD,HHHH)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " End_Date_Hour    = ${CXR_YEAR},${CXR_MONTH},${CXR_DAY},${CXR_STOP_HOUR},   ! (YYYY,MM,DD,HHHH)" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo " Maximum_Timestep    = ${CXR_MAXIMUM_TIMESTEP},      ! minutes" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Met_Input_Frequency = ${CXR_MET_INPUT_FREQUENCY},   ! minutes" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Ems_Input_Frequency = ${CXR_EMS_INPUT_FREQUENCY},   ! minutes" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Output_Frequency    = ${CXR_OUTPUT_FREQUENCY},      ! minutes" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "!--- Map projection parameters ---" >> ${CXR_MODEL_CTRL_FILE} 
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Map_Projection           = '${CXR_MAP_PROJECTION}', ! (LAMBERT,POLAR,UTM,LATLON)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " UTM_Zone                 = ${CXR_UTM_ZONE}," >> ${CXR_MODEL_CTRL_FILE} 
	echo " POLAR_Longitude_Pole     = ${CXR_POLAR_LONGITUDE_POLE},        ! deg (west<0,south<0)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " POLAR_Latitude_Pole      = ${CXR_POLAR_LATITUDE_POLE},        ! deg (west<0,south<0)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " LAMBERT_Center_Longitude =  ${CXR_LAMBERT_CENTER_LONGITUDE},      ! deg (west<0,south<0)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " LAMBERT_Center_Latitude  =  ${CXR_LAMBERT_CENTER_LATITUDE},      ! deg (west<0,south<0)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " LAMBERT_True_Latitude1   =  ${CXR_LAMBERT_TRUE_LATITUDE1},      ! deg (west<0,south<0)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " LAMBERT_True_Latitude2   =  ${CXR_LAMBERT_TRUE_LATITUDE2},      ! deg (west<0,south<0)" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "!--- Parameters for the master (first) grid ---" >> ${CXR_MODEL_CTRL_FILE} 
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Number_of_Grids      = ${CXR_NUMBER_OF_GRIDS}," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Master_Origin_XCoord = ${CXR_MASTER_ORIGIN_XCOORD},       ! km or deg, SW corner of cell(1,1)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Master_Origin_YCoord = ${CXR_MASTER_ORIGIN_YCOORD},       ! km or deg, SW corner of cell (1,1)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Master_Cell_XSize    = ${CXR_MASTER_CELL_XSIZE},           ! km or deg" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Master_Cell_YSize    = ${CXR_MASTER_CELL_YSIZE},           ! km or deg" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Master_Grid_Columns  = ${CXR_MASTER_GRID_COLUMNS}," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Master_Grid_Rows     = ${CXR_MASTER_GRID_ROWS}," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Number_of_Layers(1)  = ${CXR_NUMBER_OF_LAYERS[1]}," >> ${CXR_MODEL_CTRL_FILE} 
	
	# Here we loop through the remaining grids
	
	for i in $(seq 2 $CXR_NUMBER_OF_GRIDS);
	do
 		echo "" >> ${CXR_MODEL_CTRL_FILE}
 		echo "!--- Parameters for grid ${i} ---" >> ${CXR_MODEL_CTRL_FILE}  
 			
 		echo " Nest_Meshing_Factor(${i}) = ${CXR_NEST_MESHING_FACTOR[${i}]}, ! Relative to master grid" >> ${CXR_MODEL_CTRL_FILE} 
		echo " Nest_Beg_I_Index(${i})    = ${CXR_NEST_BEG_I_INDEX[${i}]},    ! Relative to master grid" >> ${CXR_MODEL_CTRL_FILE} 
		echo " Nest_End_I_Index(${i})    = ${CXR_NEST_END_I_INDEX[${i}]},    ! Relative to master grid" >> ${CXR_MODEL_CTRL_FILE} 
		echo " Nest_Beg_J_Index(${i})    = ${CXR_NEST_BEG_J_INDEX[${i}]},    ! Relative to master grid" >> ${CXR_MODEL_CTRL_FILE} 
		echo " Nest_End_J_Index(${i})    = ${CXR_NEST_END_J_INDEX[${i}]},    ! Relative to master grid" >> ${CXR_MODEL_CTRL_FILE} 
		echo " Number_of_Layers(${i})    = ${CXR_NUMBER_OF_LAYERS[${i}]}," >> ${CXR_MODEL_CTRL_FILE} 
	done    
	
	echo "!--- Model options ---" >> ${CXR_MODEL_CTRL_FILE} 
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Diagnostic_Error_Check = .${CXR_DIAGNOSTIC_ERROR_CHECK}.,    ! True = will stop before 1st timestep" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Advection_Solver       = '${CXR_ADVECTION_SOLVER}',      ! (PPM,BOTT)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Chemistry_Solver       = '${CXR_CHEMISTRY_SOLVER}',      ! (CMC,IEH,LSODE)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " PiG_Submodel           = '${CXR_PIG_SUBMODEL}',     ! (None,GREASD,IRON)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Probing_Tool           = '${CXR_PROBING_TOOL}',     ! (None,OSAT,PSAT,GOAT,APCA,DDM,PA,RTRAC)" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Chemistry              = .${CXR_CHEMISTRY}.," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Dry_Deposition         = .${CXR_DRY_DEPOSITION}.," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Wet_Deposition         = .${CXR_WET_DEPOSITION}.," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Staggered_Winds        = .${CXR_STAGGERED_WINDS}.," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Gridded_Emissions      = .${CXR_GRIDDED_EMISSIONS}.," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Point_Emissions        = .${CXR_POINT_EMISSIONS}.," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Ignore_Emission_Dates  = .${CXR_IGNORE_EMISSION_DATES}.," >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "!--- Output specifications ---" >> ${CXR_MODEL_CTRL_FILE} 
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Root_Output_Name          = '${CXR_ROOT_OUTPUT}'," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Average_Output_3D         = .${CXR_AVERAGE_OUTPUT_3D}.," >> ${CXR_MODEL_CTRL_FILE} 
	echo " HDF_Format_Output         = .${CXR_HDF_FORMAT_OUTPUT}.," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Number_of_Output_Species  = ${CXR_NUMBER_OF_OUTPUT_SPECIES}," >> ${CXR_MODEL_CTRL_FILE}
	
	
	# Here we loop through the species
	for i in $(seq 1 $CXR_NUMBER_OF_OUTPUT_SPECIES);
	do
		echo " Output_Species_Names(${i})   = '${CXR_OUTPUT_SPECIES_NAMES[${i}]}'," >> ${CXR_MODEL_CTRL_FILE}
	done
	
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "!--- Input files ---" >> ${CXR_MODEL_CTRL_FILE} 
	echo " Chemistry_Parameters = '${CXR_CHEMPARAM_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Photolyis_Rates      = '${CXR_PHOTOLYIS_RATES_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
	
	# If we do a restart, do not supply IC
	if [[ "${CXR_RESTART}" == false ]]
	then
		# First day
		echo " Initial_Conditions   = '${CXR_INITIAL_CONDITIONS_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE}
	else
		# Another day (restart) - nothing
		echo " Initial_Conditions   = ''," >> ${CXR_MODEL_CTRL_FILE}
	fi
	
	echo " Boundary_Conditions  = '${CXR_BOUNDARY_CONDITIONS_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Top_Concentrations   = '${CXR_TOP_CONCENTRATIONS_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
	echo " Albedo_Haze_Ozone    = '${CXR_ALBEDO_HAZE_OZONE_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
	
	if [[ "$CXR_PLUME_IN_GRID" == true  ]]
	then
		echo " Point_Sources        = '${CXR_POINT_SOURCES_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
	fi
	
	# These two might not be set (if we are in the first day)
	echo " Master_Grid_Restart  = '${CXR_MASTER_GRID_RESTART_INPUT_FILE:-}'," >> ${CXR_MODEL_CTRL_FILE} 
	
	if [[ ${CXR_NUMBER_OF_GRIDS} -gt 1  ]]
	then
		echo " Nested_Grid_Restart  = '${CXR_NESTED_GRID_RESTART_INPUT_FILE:-}'," >> ${CXR_MODEL_CTRL_FILE} 
	fi
	
	if [[ "$CXR_PLUME_IN_GRID" == true  ]]
	then
		echo " PiG_Restart        = '${CXR_PIG_RESTART_INPUT_FILE:-}'," >> ${CXR_MODEL_CTRL_FILE} 
	fi
	
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	
	# Here, we loop through all grids
	# These variables get defined in the set_variables function
	# and not in the configuration
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
 			echo " Landuse_Grid(${i}) = '${CXR_LANDUSE_GRID_INPUT_ARR_FILES[${i}]}'," >> ${CXR_MODEL_CTRL_FILE} 
 			echo " ZP_Grid(${i})      = '${CXR_ZP_GRID_INPUT_ARR_FILES[${i}]}'," >> ${CXR_MODEL_CTRL_FILE} 
 			echo " Wind_Grid(${i})    = '${CXR_WIND_GRID_INPUT_ARR_FILES[${i}]}'," >> ${CXR_MODEL_CTRL_FILE} 
 			echo " Temp_Grid(${i})    = '${CXR_TEMP_GRID_INPUT_ARR_FILES[${i}]}'," >> ${CXR_MODEL_CTRL_FILE} 
 			echo " Vapor_Grid(${i})   = '${CXR_VAPOR_INPUT_ARR_FILES[${i}]}'," >> ${CXR_MODEL_CTRL_FILE} 
 			echo " Cloud_Grid(${i})   = '${CXR_CLOUD_GRID_INPUT_ARR_FILES[${i}]}'," >> ${CXR_MODEL_CTRL_FILE} 
 			echo " Kv_Grid(${i})      = '${CXR_KV_GRID_INPUT_ARR_FILES[${i}]}'," >> ${CXR_MODEL_CTRL_FILE} 
 			echo " Emiss_Grid(${i})   = '${CXR_EMISS_INPUT_ARR_FILES[${i}]}'," >> ${CXR_MODEL_CTRL_FILE}
	done
	
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo " /" >> ${CXR_MODEL_CTRL_FILE} 
	
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	
	
	################################################################
	# OSAT, PSAT,  GOAT or APCA
	################################################################
	if [[    "$CXR_PROBING_TOOL" == "OSAT" || "$CXR_PROBING_TOOL" == "PSAT" || "$CXR_PROBING_TOOL" == "GOAT" || "$CXR_PROBING_TOOL" == "APCA"     ]] 
	then
	
		echo " !---${CXR_PROBING_TOOL}--------------------------------------------------------------------" >> ${CXR_MODEL_CTRL_FILE} 
		echo " &SA_Control" >> ${CXR_MODEL_CTRL_FILE} 
		
		echo " SA_File_Root              = '${CXR_SA_ROOT_OUTPUT}'," >> ${CXR_MODEL_CTRL_FILE} 
		echo " SA_Summary_Output         = .${CXR_SA_SUMMARY_OUTPUT}.," >> ${CXR_MODEL_CTRL_FILE} 
		
		echo " SA_Master_Sfc_Output         = .${CXR_SA_MASTER_SFC_OUTPUT}.," >> ${CXR_MODEL_CTRL_FILE} 
		echo " SA_Nested_Sfc_Output         = .${CXR_SA_NESTED_SFC_OUTPUT}.," >> ${CXR_MODEL_CTRL_FILE} 
		echo " SA_Stratify_Boundary         = .${CXR_SA_STRATIFY_BOUNDARY}.," >> ${CXR_MODEL_CTRL_FILE} 
		echo " SA_Number_of_Source_Regions  = ${CXR_SA_NUMBER_OF_SOURCE_REGIONS}," >> ${CXR_MODEL_CTRL_FILE} 
		echo " SA_Number_of_Source_Groups   = ${CXR_SA_NUMBER_OF_SOURCE_GROUPS}," >> ${CXR_MODEL_CTRL_FILE} 
		echo " Use_Leftover_Group           = .${CXR_USE_LEFTOVER_GROUP}.," >> ${CXR_MODEL_CTRL_FILE} 
		echo " Number_of_Timing_Releases    = ${CXR_NUMBER_OF_TIMING_RELEASES}, " >> ${CXR_MODEL_CTRL_FILE} 
		echo " PSAT_Treat_SULFATE_Class     = .${CXR_PSAT_TREAT_SULFATE_CLASS}.," >> ${CXR_MODEL_CTRL_FILE} 
		echo " PSAT_Treat_NITRATE_Class     = .${CXR_PSAT_TREAT_NITRATE_CLASS}.," >> ${CXR_MODEL_CTRL_FILE} 
		echo " PSAT_Treat_SOA_Class         = .${CXR_PSAT_TREAT_SOA_CLASS}.," >> ${CXR_MODEL_CTRL_FILE} 
		echo " PSAT_Treat_PRIMARY_Class     = .${CXR_PSAT_TREAT_PRIMARY_CLASS}.," >> ${CXR_MODEL_CTRL_FILE} 
		echo " PSAT_Treat_MERCURY_Class     = .${CXR_PSAT_TREAT_MERCURY_CLASS}.," >> ${CXR_MODEL_CTRL_FILE} 
		echo " PSAT_Treat_OZONE_Class       = .${CXR_PSAT_TREAT_OZONE_CLASS}.," >> ${CXR_MODEL_CTRL_FILE} 
		
		echo " SA_Receptor_Definitions  = '${CXR_SA_RECEPTORIN:-}'," >> ${CXR_MODEL_CTRL_FILE} 
		
		echo " SA_Master_Restart        = '${CXR_SA_MASTER_RESTART_INPUT_FILE:-}'," >> ${CXR_MODEL_CTRL_FILE} 
		
		if [[ ${CXR_NUMBER_OF_GRIDS} -gt 1  ]]
		then
			echo " SA_Nested_Restart        = '${CXR_SA_NESTED_RESTART_INPUT_FILE:-}'," >> ${CXR_MODEL_CTRL_FILE} 
		fi
		
		# By grid
		for k in $(seq 1 $(( ${#SA_REGIONS_DOMAIN_NUMBERS[@]} - 1 )));
		do
			echo " SA_Source_Area_Map(${SA_REGIONS_DOMAIN_NUMBERS[$k]})    = '${CXR_SA_SOURCE_AREA_MAP_INPUT_ARR_FILES[${k}]}'," >> ${CXR_MODEL_CTRL_FILE} 
		done
		
		
		if [[ "${CXR_POINT_EMISSIONS}" == true  ]]
		then
			# By source group
			for j in $(seq 1 $CXR_SA_NUMBER_OF_SOURCE_GROUPS);
			do
				echo " SA_Points_Group(${j})       = '${CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${j}]}'," >> ${CXR_MODEL_CTRL_FILE} 
			done
		fi
		
		
		if [[ "${CXR_GRIDDED_EMISSIONS}" == true  ]]
		then
			# By source group
			for j in $(seq 1 $CXR_SA_NUMBER_OF_SOURCE_GROUPS);
			do
				# and grid
				for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
				do
					# This is not elegant, but it simulates a 2D Array
					ELEMENT_NAME="CXR_SA_EMISS_GROUP_GRID_INPUT_${i}_${j}"
					echo " SA_Emiss_Group_Grid(${j},${i}) = '${!ELEMENT_NAME}'," >> ${CXR_MODEL_CTRL_FILE} 
			
				done
			done
		fi
		
		echo " &END" >> ${CXR_MODEL_CTRL_FILE} 
	
		
	################################################################
	# Must we run direct decoupled sensitivity analysis?
	# DDM
	################################################################
	elif [[ "$CXR_PROBING_TOOL" == "DDM"  ]] 
	then

		echo " !----Sensitivity Analysis (Direct Decoupled Method)-----------------------------" >> ${CXR_MODEL_CTRL_FILE} 
		echo " &DDM_Control" >> ${CXR_MODEL_CTRL_FILE} 
		
		echo " DDM_File_Root                = '${CXR_DDM_ROOT_OUTPUT}'," >> ${CXR_MODEL_CTRL_FILE} 
		echo " DDM_Master_Sfc_Output        = .${CXR_DDM_MASTER_SFC_OUTPUT}.," >> ${CXR_MODEL_CTRL_FILE} 
		echo " DDM_Nested_Sfc_Output        = .${CXR_DDM_NESTED_SFC_OUTPUT}.," >> ${CXR_MODEL_CTRL_FILE} 
		echo " DDM_Stratify_Boundary        = .${CXR_DDM_STRATIFY_BOUNDARY}.," >> ${CXR_MODEL_CTRL_FILE} 
		echo " DDM_Number_of_Source_Regions = ${CXR_DDM_NUMBER_OF_SOURCE_REGIONS}," >> ${CXR_MODEL_CTRL_FILE} 
		echo " DDM_Number_of_Source_Groups  = ${CXR_DDM_NUMBER_OF_SOURCE_GROUPS}," >> ${CXR_MODEL_CTRL_FILE} 
		
		echo " DDM_Initial_Conditions    = '${CXR_DDM_INITIAL_CONDITIONS_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
		echo " DDM_Boundary_Conditions   = '${CXR_DDM_BOUNDARY_CONDITIONS_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
		echo " DDM_Top_Concentrations    = '${CXR_DDM_TOP_CONCENTRATIONS_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
		echo " DDM_Master_Restart        = '${CXR_DDM_MASTER_RESTART_INPUT_FILE:-}'," >> ${CXR_MODEL_CTRL_FILE} 
		echo " DDM_Nested_Restart        = '${CXR_DDM_NESTED_RESTART_INPUT_FILE:-}'," >> ${CXR_MODEL_CTRL_FILE} 
		
		echo " DDM_Receptor_Definitions  = '${CXR_DDM_RECEPTOR_DEFINITIONS_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
		
		echo " Number_of_IC_Species_Groups = ${CXR_NUMBER_OF_IC_SPECIES_GROUPS}," >> ${CXR_MODEL_CTRL_FILE} 
		
		# By initial conc group
		for a in $(seq 1 "$CXR_NUMBER_OF_IC_SPECIES_GROUPS");
		do
			echo " IC_Species_Groups(${a})        = '${CXR_IC_SPECIES_GROUPS_INPUT_ARR_FILES[${a}]}'," >> ${CXR_MODEL_CTRL_FILE} 
		done
		
		echo " Number_of_BC_Species_Groups = ${CXR_NUMBER_OF_BC_SPECIES_GROUPS}," >> ${CXR_MODEL_CTRL_FILE} 
		
		# By boundary condition group
		for b in $(seq 1 "$CXR_NUMBER_OF_BC_SPECIES_GROUPS");
		do
			echo " BC_species_Groups(${b})        = '${CXR_BC_SPECIES_GROUPS_INPUT_ARR_FILES[${b}]}'," >> ${CXR_MODEL_CTRL_FILE} 
		done
		
		
		echo " Number_of_EM_Species_Groups = ${CXR_NUMBER_OF_EM_SPECIES_GROUPS}," >> ${CXR_MODEL_CTRL_FILE} 
		
		# By Emission group
		for c in $(seq 1 "$CXR_NUMBER_OF_EM_SPECIES_GROUPS");
		do
			echo " Emis_Species_Groups(${c})      = '${CXR_EMIS_SPECIES_GROUPS[${c}]}'," >> ${CXR_MODEL_CTRL_FILE} 
		done
		
		# By grid
		for i in $(seq 1 "$CXR_NUMBER_OF_GRIDS");
		do
			echo " DDM_Source_Area_Map(${i})    = '${CXR_DDM_SOURCE_AREA_MAP_INPUT_ARR_FILES[${i}]}'," >> ${CXR_MODEL_CTRL_FILE}
		done 
		
		if [[ "${CXR_POINT_EMISSIONS}" == true  ]]
		then
			# By source group
			for j in $(seq 1 "$CXR_DDM_NUMBER_OF_SOURCE_GROUPS");
			do
				echo " DDM_Points_Group(${j})       = '${CXR_SA_POINTS_GROUP_INPUT_ARR_FILES[${j}]}'," >> ${CXR_MODEL_CTRL_FILE} 
			done
		fi
		 
		# By source group, by grid
		if [[ "${CXR_GRIDDED_EMISSIONS}" == true  ]]
		then
			# By source group
			for j in $(seq 1 $CXR_DDM_NUMBER_OF_SOURCE_GROUPS);
			do
				
				# and grid
				for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
				do
					# This is not elegant, but it simulates a 2D Array
					ELEMENT_NAME=CXR_DDM_EMISS_GROUP_GRID_${j}_${i}_INPUT_FILE
					echo " DDM_Emiss_Group_Grid(${j},${i}) = '${!ELEMENT_NAME}'," >> ${CXR_MODEL_CTRL_FILE} 
				done
			done
		fi 
		
		echo " &END" >> ${CXR_MODEL_CTRL_FILE}  

	################################################################
	# Must we run Reactive Tracer Source Apportionment?
	# RTRAC (RT)
	################################################################
	elif [[ "$CXR_PROBING_TOOL" == "RTRAC"  ]] 
	then
	
		echo " !---RTRAC (Reactive Tracer Source Apportionment)-----------------------------------------------------------------" >> ${CXR_MODEL_CTRL_FILE} 
		echo " &RT_Control" >> ${CXR_MODEL_CTRL_FILE} 
		
		echo " RT_File_Root            = '${CXR_RT_ROOT_OUTPUT}'," >> ${CXR_MODEL_CTRL_FILE} 
		
		echo " RT_Initial_Conditions   = '${CXR_RT_INITIAL_CONDITIONS_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
		echo " RT_Boundary_Conditions  = '${CXR_RT_BOUNDARY_CONDITIONS_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
		echo " RT_Top_Concentrations   = '${CXR_RT_TOP_CONCENTRATIONS_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
		
		echo " RT_Master_Restart       = '${CXR_RT_MASTER_RESTART_INPUT_FILE:-}'," >> ${CXR_MODEL_CTRL_FILE} 
		echo " RT_Nested_Restart       = '${CXR_RT_NESTED_RESTART_INPUT_FILE:-}'," >> ${CXR_MODEL_CTRL_FILE} 
		
		echo " RT_Chemistry_Parameters = '${CXR_RT_CHEMISTRY_PARAMETERS_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
		echo " RT_Receptor_Definitions = '${CXR_RT_RECEPTOR_DEFINITIONS_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
		echo " RT_Point_Sources        = '${CXR_RT_POINT_SOURCES_INPUT_FILE}'," >> ${CXR_MODEL_CTRL_FILE} 
		
		if [[ "$CXR_PLUME_IN_GRID" == true  ]]
		then
			echo " RT_PiG_Sample           = .${CXR_RT_PIG_SAMPLE}.,               ! Ignore if PiG = false" >> ${CXR_MODEL_CTRL_FILE} 
		fi
		
		# By grid
		for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
		do
			echo " RT_Emiss_Grid(${i})    = '${CXR_RT_EMISS_GRID_INPUT_ARR_FILES[${i}]}'," >> ${CXR_MODEL_CTRL_FILE}
		done

		
		echo " &END" >> ${CXR_MODEL_CTRL_FILE} 
	
	fi
	
	################################################################
	# General Probing support
	################################################################
	if [[ "$CXR_PROBING_TOOL" != "None"  ]]
	then
	
		echo "!---------------Probing Tool General------------------------------------------" >> ${CXR_MODEL_CTRL_FILE} 
		echo " &PA_Control" >> ${CXR_MODEL_CTRL_FILE} 
		echo "" >> ${CXR_MODEL_CTRL_FILE} 
		echo " PA_File_Root         = '$CXR_PA_ROOT_OUTPUT'," >> ${CXR_MODEL_CTRL_FILE} 
		echo "" >> ${CXR_MODEL_CTRL_FILE} 
		echo " Number_of_PA_Domains = ${CXR_NUMBER_OF_PA_DOMAINS}," >> ${CXR_MODEL_CTRL_FILE} 
		
		# Here we loop through the PA domains
		for i in $(seq 1 $CXR_NUMBER_OF_PA_DOMAINS);
		do
			echo " Within_CAMx_Grid(${i})  = ${CXR_WITHIN_CAMX_GRID[${i}]},  ! Specify which CAMx grid that this PA domain is in" >> ${CXR_MODEL_CTRL_FILE} 
			echo " PA_Beg_I_Index(${i})    = ${CXR_PA_BEG_I_INDEX[${i}]}," >> ${CXR_MODEL_CTRL_FILE} 
			echo " PA_End_I_Index(${i})    = ${CXR_PA_END_I_INDEX[${i}]}," >> ${CXR_MODEL_CTRL_FILE} 
			echo " PA_Beg_J_Index(${i})    = ${CXR_PA_BEG_J_INDEX[${i}]}," >> ${CXR_MODEL_CTRL_FILE} 
			echo " PA_End_J_Index(${i})    = ${CXR_PA_END_J_INDEX[${i}]}," >> ${CXR_MODEL_CTRL_FILE} 
			echo " PA_Beg_K_Index(${i})    = ${CXR_PA_BEG_K_INDEX[${i}]}," >> ${CXR_MODEL_CTRL_FILE} 
			echo " PA_End_K_Index(${i})    = ${CXR_PA_END_K_INDEX[${i}]}," >> ${CXR_MODEL_CTRL_FILE} 
			echo "" >> ${CXR_MODEL_CTRL_FILE} 
		done
		
		echo "" >> ${CXR_MODEL_CTRL_FILE} 
		echo " &END" >> ${CXR_MODEL_CTRL_FILE} 
	fi
	
	echo "" >> ${CXR_MODEL_CTRL_FILE} 
	echo "!-------------------------------------------------------------------------------" >> ${CXR_MODEL_CTRL_FILE} 
	
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
	
	main_log "$FUNCNAME" "This is the content of the outfile:"
	
	cat $outfile 2>&1 | tee -a $CXR_LOG
	
	if [[ $retval -ne 0 ]]
	then
		main_dieGracefully "$FUNCNAME - CAMx has returned a non-zero status for $CXR_DATE"
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
	
		# cxr_common_store_state checks if we have finished this and if we need to continue
		if [[ $(cxr_common_store_state ${CXR_STATE_START}) == true  ]]
		then
			main_log -B "$FUNCNAME" "Running $CXR_MODEL_EXEC for day $CXR_DATE"
			
			# If we do not run the first day, its a restart
			if [[ "$(date_isFirstDayOfSimulation?)" == false  ]]
			then
				# This must be a restart!
				CXR_RESTART=true
			else
				# Nope.
				CXR_RESTART=false
			fi
			
			#  --- Execute the model and write stderr and stdout to CXR_LOG ---
			set_variables
			
			if [[ "$CXR_DRY" == true ]]
			then
				main_log "$FUNCNAME" "This is a dry run, $CXR_MODEL is run, but only in diagnostic mode"
				CXR_DIAGNOSTIC_ERROR_CHECK=true
			fi
			
			
			#  --- Create the input file - will be stored in the state directory 
			#      but a link called CAMx.in wil be created where the CAMx binary is located
			write_model_control_file
			
			# If we do SA, write a receptor file
			if [[ "${CXR_SA_RECEPTOR:-}" == true  ]]
			then
				write_sa_receptor_definitions_file
			fi
			
			if [[ $(cxr_common_check_preconditions) == false  ]]
			then
				main_log "$FUNCNAME" "Preconditions for ${CXR_META_MODULE_NAME} are not met!"
				# We notify the caller of the problem
				return $CXR_RET_ERR_PRECONDITIONS
			fi
			
			# In case of a dry-run, we do run the model, but we turn on diagnostics
			execute_model
			
		
			# Did we run properly?
			if [[ $(cxr_common_check_result) == false  ]]
			then
				main_log "$FUNCNAME" "$CXR_MODEL Run was not successful!"
				# We notify the caller of the problem
				return $CXR_RET_ERR_POSTCONDITIONS
			fi
			
			# We store the fact model run was completed
			cxr_common_store_state ${CXR_STATE_STOP} > /dev/null
			
		else
			main_log "$FUNCNAME" "Stage was already started, therefore we do not run it. I assume this is a restart - we try to catch up!"
		fi
		
		else
		main_log "$FUNCNAME" "Model disabled (either in the config using CXR_RUN_MODEL=false or with the option -N)"
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
	local day_offset
	
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

	for day_offset in $(seq 0 $((${CXR_NUMBER_OF_SIM_DAYS} -1 )) )
	do
		# Initialise the date variables 
		date_setVars "$CXR_START_DATE" "$day_offset"
		
		set_variables
		
		write_model_control_file
	done
	
	# Reset date variables for first day
	date_setVars "$CXR_START_DATE" "0"
	
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


