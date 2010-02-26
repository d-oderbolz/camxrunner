#!/usr/bin/env bash
#
#
# Maintenance note: MUST BE KEPT UP TO DATE WITH installer/X.YZ/base.ask and conf/base.conf
#
# Runner script for CAMx 4.42/4.51 - Configuration. 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
# 
# Leave the next line intact, it is used for change detection
# Version: $Id$ 
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch),
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Who	When		What
# dco 	02.10.2008	Created
#
# All Variables (except system variables like OMP*) names start with CXR_
# Output File names end with OUTPUT_FILE
# Input File names end with INPUT_FILE
#
# Arrays of files are called OUTPUT_ARR_FILES AND INPUT_ARR_FILES
#
# Directories with _DIR
# Output directories with OUTPUT_DIR
# Executables with _EXEC
#
# Strings which are used as Floating point numbers, but are integer, need a trailing .
#
# Arrays must have an index 0 with a Dummy entry, we work in Fortran Land here.
#
# Also, entries in arrays which contain spaces must be protected by
# single (') quotes, because arrays are exported as space separated list
#
# The entries should be in a sensible order, starting from often changed ones to
# constant system stuff.

# The installer (CAMxRunner.sh -I) prepares a base.conf file form this.
# Template variables are marked by @@ and should be fully replaced.
# There are settings which the installer does not touch, search for 
# "These are settings the installer did not touch" in this file and fix it!

################################################################################
################################################################################
# These settings you are likely to change on a per-run basis
################################################################################
################################################################################

################################################################################
# Notification options
################################################################################
CXR_MAILADDR=@CXR_MAILADDR@
CXR_SMSNR=@CXR_SMSNR@
CXR_SMSPROVIDER=@CXR_SMSPROVIDER@
CXR_SMSADDR=${CXR_SMSNR}@${CXR_SMSPROVIDER}

#If this Variable is true, SMS will be sent
CXR_SEND_SMS=@CXR_SMSADDR@

################################################################################
# Time-span of simulation
################################################################################

# The Start of the simulation in YYYY-MM-DD notation
CXR_START_DATE=@CXR_START_DATE@

# The Stop of the simulation in YYYY-MM-DD notation
CXR_STOP_DATE=@CXR_STOP_DATE@

# Must be in HHHH Format
CXR_START_HOUR=0000
CXR_STOP_HOUR=2400

# Timezone (0=UTC,5=EST,6=CST,7=MST,8=PST)
CXR_TIME_ZONE=0

# The Simulation intervals in Minutes (Floats!)
CXR_MAXIMUM_TIMESTEP=15.
CXR_MET_INPUT_FREQUENCY=60.
CXR_EMS_INPUT_FREQUENCY=60.
CXR_OUTPUT_FREQUENCY=60.

################################################################################
# If you want to disable modules of the CAMxRunner, do it here
################################################################################

# Set this to false to disable CAMx in this run
CXR_RUN_MODEL=true

# This string contains the space-separated module names of
# unwanted daily preprocessors like "create_emissions"
# If "skip_all" is given, all single preprocessors are skipped

CXR_DISABLED_DAILY_PREPROC=""

# This string contains the space-separated  module names of
# unwanted one-time preprocessors.
# If "skip_all" is given, all one-time preprocessors are skipped

CXR_DISABLED_ONCE_PREPROC=""

# This string contains the space-separated module names of
# unwanted daily postprocessors like "prepare_output_dir convert_output"
# If "skip_all" is given, all single postprocessors are skipped
	
CXR_DISABLED_DAILY_POSTPROC="avgdif"

# This string contains the space-separated module names of
# unwanted one-time postprocessors.
# If "skip_all" is given, all one-time postprocessors are skipped

CXR_DISABLED_ONCE_POSTPROC=""

################################################################################
# Chemical Mechanism
################################################################################

# Here choose between these possibilities (taken from the CAMx Guide)
# 3 (Carbon Bond IV - Gery et al. 1989)
# 1 (same as above, but with reactive chlorine chemistry - Tanaka et al., 2000)
# 4 (Carbon Bond IV - CB4)
# 5 (SAPRC99, Carter, 2000)
# 6 (Carbon Bond version 2005 - CB05)
# 10 (User defined)

CXR_CHEMICAL_MECHANISM=4

# Choose between 
# CF for (Coarse fine) 
# CF+hg (Coarse fine with mercury)
# CMU (Carnegie Mellon sectional model) 
# NONE (No Aerosols)
# Aerosol Chemistry is currently only supported with mechanisms 4 and 6 - CAMxRunner will complain but try anyway
CXR_AEROSOL_MECHANISM=CF

# The CAMxRunner determines the name of the chemparam file automatically
# but you can overwrite this behavior by setting this parameter
# CXR_CHEMPARAM_INPUT_FILE=

################################################################################
# Labels
################################################################################
CXR_MET_PROJECT=uw3
CXR_MET_SCENARIO=s151

CXR_EMMISS_PROJECT=${CXR_MET_PROJECT}
CXR_EMMISS_SCENARIO=sem050

CXR_CAMX_SCENARIO=${CXR_MET_SCENARIO}
CXR_CAMX_PERIOD=winter07
CXR_CAMX_CUSTOMER=bafu3

################################################################################
# Directories
# Attention: Put no variables in directory names that are not yet set here!
# E. g. any variable containing dates (like simulation year) will fail to expand!
################################################################################

CXR_BASE_DIR=@CXR_BASE_DIR@/${CXR_CAMX_CUSTOMER}/${CXR_CAMX_PERIOD}
CXR_INPUT_DIR=${CXR_BASE_DIR}/Inputs
CXR_METEO_DIR=@CXR_METEO_DIR@
CXR_LANDUSE_DIR=@CXR_LANDUSE_DIR@
CXR_EMISSION_DIR=${CXR_BASE_DIR}/Emiss
CXR_PTSRCE_DIR=${CXR_BASE_DIR}/Ptsrce

# Input preparation directories
CXR_EMISSION_SOURCE_DIR=@CXR_EMISSION_SOURCE_DIR@

# This is the main output directory
CXR_OUTPUT_DIR=${CXR_BASE_DIR}/Outputs/$(uname -n)/${CXR_CAMX_SCENARIO}

# Probing output dirs
CXR_PA_OUTPUT_DIR=${CXR_OUTPUT_DIR}/PA
CXR_DDM_OUTPUT_DIR=${CXR_OUTPUT_DIR}/DDM
CXR_RT_OUTPUT_DIR=${CXR_OUTPUT_DIR}/RT
CXR_SA_OUTPUT_DIR=${CXR_OUTPUT_DIR}/SA

################################################################################
# Rules for filenames (See http://people.web.psi.ch/oderbolz/CAMxRunner#FileRules)
#
# There are more filerules in the Probing sections of this file
#
# These ABSOLUTELY need 'single quotes' around them, otherwise the shell expands them here!
################################################################################

################## Grid independent ############################################

# Input Preparation ############################################################


# Input ########################################################################

CXR_PHOTOLYIS_RATES_FILE_RULE='${CXR_INPUT_DIR}/tuv_${CXR_CAMX_CUSTOMER}_${CXR_CAMX_PERIOD}.out'
CXR_INITIAL_CONDITIONS_FILE_RULE='${CXR_INPUT_DIR}/ic_${CXR_CAMX_CUSTOMER}_moz_${CXR_DAY}${CXR_MONTH}${CXR_YEAR}.bin'
CXR_BOUNDARY_CONDITIONS_FILE_RULE='${CXR_INPUT_DIR}/bc_${CXR_CAMX_CUSTOMER}_moz_${CXR_YEAR_S}${CXR_MONTH}${CXR_DAY}.bin'
CXR_TOP_CONCENTRATIONS_FILE_RULE='${CXR_INPUT_DIR}/topconc_${CXR_CAMX_CUSTOMER}_${CXR_CAMX_PERIOD}.${CXR_ASC_EXT}'
CXR_ALBEDO_HAZE_OZONE_FILE_RULE='${CXR_INPUT_DIR}/ahomap_${CXR_CAMX_CUSTOMER}_3grids_${CXR_CAMX_PERIOD}.out'
CXR_POINT_SOURCES_FILE_RULE=''

CXR_MASTER_GRID_RESTART_FILE_RULE='${CXR_OUTPUT_DIR}/${CXR_RUN}.${CXR_DATE_RAW_YESTERDAY}.inst'
CXR_NESTED_GRID_RESTART_FILE_RULE='${CXR_OUTPUT_DIR}/${CXR_RUN}.${CXR_DATE_RAW_YESTERDAY}.finst'

# More restart rules in the probing sections later

CXR_PIG_RESTART_FILE_RULE=''

CXR_ROOT_OUTPUT_FILE_RULE='${CXR_OUTPUT_DIR}/${CXR_RUN}.${CXR_DATE_RAW}'

# File Roots for probing
CXR_PA_ROOT_OUTPUT_FILE_RULE='$CXR_PA_OUTPUT_DIR/${CXR_RUN}.${CXR_YEAR_S}${CXR_MONTH}${CXR_DAY}'

CXR_DDM_ROOT_OUTPUT_FILE_RULE='$CXR_DDM_OUTPUT_DIR/${CXR_RUN}.${CXR_YEAR_S}${CXR_MONTH}${CXR_DAY}'
CXR_RT_ROOT_OUTPUT_FILE_RULE='$CXR_RT_OUTPUT_DIR/${CXR_RUN}.${CXR_YEAR_S}${CXR_MONTH}${CXR_DAY}'
CXR_SA_ROOT_OUTPUT_FILE_RULE='$CXR_SA_OUTPUT_DIR/${CXR_RUN}.${CXR_YEAR_S}${CXR_MONTH}${CXR_DAY}'

# These are used 
#	* to prevent overwriting of existing files 
#	* the creation of the aqmfad directory
#	* to convert the binary output files

CXR_DIAG_FILE_RULE='${CXR_ROOT_OUTPUT}.diag'
CXR_FINST_FILE_RULE='${CXR_ROOT_OUTPUT}.finst'
CXR_INST_FILE_RULE='${CXR_ROOT_OUTPUT}.inst'
CXR_MASS_FILE_RULE='${CXR_ROOT_OUTPUT}.mass'
CXR_OUT_FILE_RULE='${CXR_ROOT_OUTPUT}.out'

################## Grid spcecific ##############################################

# These NEED the variable ${i} somewhere (the grid number)

# Input Preparation ############################################################

CXR_EMISSION_ASC_FILE_RULE='${CXR_EMISSION_SOURCE_DIR}/${CXR_DATE_RAW}/${CXR_EMMISS_SCENARIO}/camx_emiss_domain${i}_${CXR_MET_PROJECT}_${CXR_EMMISS_SCENARIO}_${CXR_DATE_RAW}.asc'


# Input ########################################################################

#Landuse
CXR_LANDUSE_FILE_RULE='${CXR_INPUT_DIR}/terrain_domain${i}_bx3_lucamx.bin'

# Pressure
CXR_PRESSURE_FILE_RULE='${CXR_METEO_DIR}/${CXR_DATE_RAW}/${CXR_MET_SCENARIO}/camx_zp_domain${i}_${CXR_MET_PROJECT}_${CXR_MET_SCENARIO}:${CXR_YEAR}-${CXR_MONTH}-${CXR_DAY_METEO}'

# Wind
CXR_WIND_FILE_RULE='${CXR_METEO_DIR}/${CXR_DATE_RAW}/${CXR_MET_SCENARIO}/camx_uv_domain${i}_${CXR_MET_PROJECT}_${CXR_MET_SCENARIO}:${CXR_YEAR}-${CXR_MONTH}-${CXR_DAY_METEO}'

# Temperature
CXR_TEMPERATURE_FILE_RULE='${CXR_METEO_DIR}/${CXR_DATE_RAW}/${CXR_MET_SCENARIO}/camx_tp_domain${i}_${CXR_MET_PROJECT}_${CXR_MET_SCENARIO}:${CXR_YEAR}-${CXR_MONTH}-${CXR_DAY_METEO}'

# Vapor
CXR_VAPOR_FILE_RULE='${CXR_METEO_DIR}/${CXR_DATE_RAW}/${CXR_MET_SCENARIO}/camx_qa_domain${i}_${CXR_MET_PROJECT}_${CXR_MET_SCENARIO}:${CXR_YEAR}-${CXR_MONTH}-${CXR_DAY_METEO}'

# Cloud
CXR_CLOUD_FILE_RULE='${CXR_METEO_DIR}/${CXR_DATE_RAW}/${CXR_MET_SCENARIO}/camx_cr_domain${i}_${CXR_MET_PROJECT}_${CXR_MET_SCENARIO}:${CXR_YEAR}-${CXR_MONTH}-${CXR_DAY_METEO}'

# Vertical K
CXR_K_FILE_RULE='${CXR_METEO_DIR}/${CXR_DATE_RAW}/${CXR_MET_SCENARIO}/camx_kv_CMAQ_domain${i}_${CXR_MET_PROJECT}_${CXR_MET_SCENARIO}:${CXR_YEAR}-${CXR_MONTH}-${CXR_DAY_METEO}'
 
# Emissions
CXR_EMISSION_BIN_FILE_RULE='${CXR_EMISSION_DIR}/camx_emiss_domain${i}_${CXR_EMMISS_PROJECT}_${CXR_EMMISS_SCENARIO}_${CXR_DATE_RAW}.bin'

# Output #######################################################################

#Deposition file
CXR_DEPN_FILE_RULE='${CXR_ROOT_OUTPUT}.depn.grd0${i}'

CXR_AVG_FILE_RULE='${CXR_ROOT_OUTPUT}.avrg.grd0${i}'  

# We create the ASCII version of thefiles in the aqmfad directory      
# All rules start with the CXR_AQMFAD_OUTPUT_DIR and end with ${CXR_ASC_EXT}
CXR_AVG_ASC_FILE_RULE='${CXR_AQMFAD_OUTPUT_DIR}/${CXR_RUN}.${CXR_DATE_RAW}.avrg.grd0${i}.${CXR_ASC_EXT}'

# Landuse ASCII File must not be created, it is used directly
CXR_LANDUSE_ASC_FILE_RULE='${CXR_LANDUSE_DIR}/terrain_domain${i}_bx3_terrcamx.${CXR_ASC_EXT}'
CXR_PRESSURE_ASC_FILE_RULE='${CXR_AQMFAD_OUTPUT_DIR}/camx_zp_domain${i}_${CXR_MET_PROJECT}_${CXR_MET_SCENARIO}:${CXR_YEAR}-${CXR_MONTH}${CXR_DAY_METEO}.${CXR_ASC_EXT}'
CXR_WIND_ASC_FILE_RULE='${CXR_AQMFAD_OUTPUT_DIR}/camx_uv_domain${i}_${CXR_MET_PROJECT}_${CXR_MET_SCENARIO}:${CXR_YEAR}-${CXR_MONTH}${CXR_DAY_METEO}.${CXR_ASC_EXT}'
CXR_TEMPERATURE_ASC_FILE_RULE='${CXR_AQMFAD_OUTPUT_DIR}/camx_tp_domain${i}_${CXR_MET_PROJECT}_${CXR_MET_SCENARIO}:${CXR_YEAR}-${CXR_MONTH}${CXR_DAY_METEO}.${CXR_ASC_EXT}'
CXR_VAPOR_ASC_FILE_RULE='${CXR_AQMFAD_OUTPUT_DIR}/camx_qa_domain${i}_${CXR_MET_PROJECT}_${CXR_MET_SCENARIO}:${CXR_YEAR}-${CXR_MONTH}${CXR_DAY_METEO}.${CXR_ASC_EXT}'
CXR_CLOUD_ASC_FILE_RULE='${CXR_AQMFAD_OUTPUT_DIR}/camx_cr_domain${i}_${CXR_MET_PROJECT}_${CXR_MET_SCENARIO}:${CXR_YEAR}-${CXR_MONTH}${CXR_DAY_METEO}.${CXR_ASC_EXT}'
CXR_K_ASC_FILE_RULE='${CXR_AQMFAD_OUTPUT_DIR}/camx_kv_CMAQ_domain${i}_${CXR_MET_PROJECT}_${CXR_MET_SCENARIO}:${CXR_YEAR}-${CXR_MONTH}${CXR_DAY_METEO}.${CXR_ASC_EXT}'		

################################################################################
# END RULES
################################################################################

################################################################################
# Geometry of simulation
################################################################################

CXR_MAP_PROJECTION=@CXR_MAP_PROJECTION@ # (LAMBERT,POLAR,UTM,LATLON)

CXR_UTM_ZONE=@CXR_UTM_ZONE@

# Floats
CXR_POLAR_LONGITUDE_POLE=@CXR_POLAR_LONGITUDE_POLE@        # deg (west<0south<0)
CXR_POLAR_LATITUDE_POLE=@CXR_POLAR_LATITUDE_POLE@        # deg (west<0south<0)
CXR_LAMBERT_CENTER_LONGITUDE=@CXR_LAMBERT_CENTER_LONGITUDE@   # deg (west<0south<0)
CXR_LAMBERT_CENTER_LATITUDE=@CXR_LAMBERT_CENTER_LATITUDE@    # deg (west<0south<0)
CXR_LAMBERT_TRUE_LATITUDE1=@CXR_LAMBERT_TRUE_LATITUDE1@     # deg (west<0south<0)
CXR_LAMBERT_TRUE_LATITUDE2=@CXR_LAMBERT_TRUE_LATITUDE2@     # deg (west<0south<0)

################################################################################
# Grid definition
################################################################################

# How many grids are used?
CXR_NUMBER_OF_GRIDS=@CXR_NUMBER_OF_GRIDS@

# Of course, more grids mean more settings down here.
# Check these, because the installer will not change these!

#Master grid (Floats)
CXR_MASTER_ORIGIN_XCOORD=@CXR_MASTER_ORIGIN_XCOORD@       # km or deg SW corner of cell(11)
CXR_MASTER_ORIGIN_YCOORD=@CXR_MASTER_ORIGIN_YCOORD@       # km or deg SW corner of cell (11)

# Floats
CXR_MASTER_CELL_XSIZE=@CXR_MASTER_CELL_XSIZE@               # km or deg
CXR_MASTER_CELL_YSIZE=@CXR_MASTER_CELL_YSIZE@               # km or deg


CXR_MASTER_GRID_COLUMNS=@CXR_MASTER_GRID_COLUMNS@
CXR_MASTER_GRID_ROWS=@CXR_MASTER_GRID_ROWS@

################################################################################
################################################################################
# These are settitgs the installer did not touch. You need to adjust these once
# in base.conf - then they can remain as they are
################################################################################
################################################################################

################################################################################
# Grid definition continued.
################################################################################

CXR_NUMBER_OF_LAYERS[1]=14

# Grid 0 and 1 are dummies
CXR_NEST_MESHING_FACTOR[0]=-
CXR_NEST_BEG_I_INDEX[0]=-
CXR_NEST_END_I_INDEX[0]=-
CXR_NEST_BEG_J_INDEX[0]=-
CXR_NEST_END_J_INDEX[0]=-
CXR_NUMBER_OF_LAYERS[0]=-

# This is a dummy because entries 1 are given by the MASTER settings
CXR_NEST_MESHING_FACTOR[1]=-
CXR_NEST_BEG_I_INDEX[1]=-
CXR_NEST_END_I_INDEX[1]=-
CXR_NEST_BEG_J_INDEX[1]=-
CXR_NEST_END_J_INDEX[1]=-

# Number of layers set above

# Second grid
CXR_NEST_MESHING_FACTOR[2]=3        # Relative to master grid
CXR_NEST_BEG_I_INDEX[2]=51          # Relative to master grid
CXR_NEST_END_I_INDEX[2]=74          # Relative to master grid
CXR_NEST_BEG_J_INDEX[2]=32          # Relative to master grid
CXR_NEST_END_J_INDEX[2]=49          # Relative to master grid
CXR_NUMBER_OF_LAYERS[2]=14
	
# Third grid
CXR_NEST_MESHING_FACTOR[3]=9        # Relative to master grid
CXR_NEST_BEG_I_INDEX[3]=55          # Relative to master grid
CXR_NEST_END_I_INDEX[3]=68          # Relative to master grid
CXR_NEST_BEG_J_INDEX[3]=35          # Relative to master grid
CXR_NEST_END_J_INDEX[3]=44          # Relative to master grid
CXR_NUMBER_OF_LAYERS[3]=14


################################################################################
# Chemical species
################################################################################

# Selection of output species
case "${CXR_AEROSOL_MECHANISM}" in

	CF)

		case "${CXR_CHEMICAL_MECHANISM}" in
		
			4)
				# CF-CBIV (as used before, extended by inert aerosol species at the end)
				# Entry 0 is a dummy, we want our arrays to start with index 1
				CXR_OUTPUT_SPECIES_NAMES[0]=-
				CXR_OUTPUT_SPECIES_NAMES[1]=NO
				CXR_OUTPUT_SPECIES_NAMES[2]=NO2
				CXR_OUTPUT_SPECIES_NAMES[3]=O3
				CXR_OUTPUT_SPECIES_NAMES[4]=TOL
				CXR_OUTPUT_SPECIES_NAMES[5]=XYL
				CXR_OUTPUT_SPECIES_NAMES[6]=FORM
				CXR_OUTPUT_SPECIES_NAMES[7]=PAN
				CXR_OUTPUT_SPECIES_NAMES[8]=CO
				CXR_OUTPUT_SPECIES_NAMES[9]=HONO
				CXR_OUTPUT_SPECIES_NAMES[10]=HNO3
				CXR_OUTPUT_SPECIES_NAMES[11]=H2O2
				CXR_OUTPUT_SPECIES_NAMES[12]=ISOP
				CXR_OUTPUT_SPECIES_NAMES[13]=PNA
				CXR_OUTPUT_SPECIES_NAMES[14]=SO2
				CXR_OUTPUT_SPECIES_NAMES[15]=NH3
				CXR_OUTPUT_SPECIES_NAMES[16]=PH2O
				CXR_OUTPUT_SPECIES_NAMES[17]=PNO3
				CXR_OUTPUT_SPECIES_NAMES[18]=PSO4
				CXR_OUTPUT_SPECIES_NAMES[19]=PNH4
				CXR_OUTPUT_SPECIES_NAMES[20]=POA
				CXR_OUTPUT_SPECIES_NAMES[21]=PEC
				CXR_OUTPUT_SPECIES_NAMES[22]=SOA1
				CXR_OUTPUT_SPECIES_NAMES[23]=SOA2
				CXR_OUTPUT_SPECIES_NAMES[24]=SOA3
				CXR_OUTPUT_SPECIES_NAMES[25]=SOA4
				CXR_OUTPUT_SPECIES_NAMES[26]=SOA5
				CXR_OUTPUT_SPECIES_NAMES[27]=NA
				CXR_OUTPUT_SPECIES_NAMES[28]=PCL
				CXR_OUTPUT_SPECIES_NAMES[29]=FPRM
				CXR_OUTPUT_SPECIES_NAMES[30]=FCRS
				CXR_OUTPUT_SPECIES_NAMES[31]=CPRM
				CXR_OUTPUT_SPECIES_NAMES[32]=CCRS
				;;
		
			6)
				# CF-CB05
				# Entry 0 is a dummy, we want our arrays to start with index 1
				CXR_OUTPUT_SPECIES_NAMES[0]=-
				CXR_OUTPUT_SPECIES_NAMES[1]=NO
				CXR_OUTPUT_SPECIES_NAMES[2]=NO2
				CXR_OUTPUT_SPECIES_NAMES[3]=O3
				CXR_OUTPUT_SPECIES_NAMES[4]=TOL
				CXR_OUTPUT_SPECIES_NAMES[5]=XYL
				CXR_OUTPUT_SPECIES_NAMES[6]=FORM
				CXR_OUTPUT_SPECIES_NAMES[7]=PAN
				CXR_OUTPUT_SPECIES_NAMES[8]=CO
				CXR_OUTPUT_SPECIES_NAMES[9]=HONO
				CXR_OUTPUT_SPECIES_NAMES[10]=HNO3
				CXR_OUTPUT_SPECIES_NAMES[11]=H2O2
				CXR_OUTPUT_SPECIES_NAMES[12]=ISOP
				CXR_OUTPUT_SPECIES_NAMES[13]=PNA
				CXR_OUTPUT_SPECIES_NAMES[14]=SO2
				CXR_OUTPUT_SPECIES_NAMES[15]=NH3
				CXR_OUTPUT_SPECIES_NAMES[16]=PH2O
				CXR_OUTPUT_SPECIES_NAMES[17]=PNO3
				CXR_OUTPUT_SPECIES_NAMES[18]=PSO4
				CXR_OUTPUT_SPECIES_NAMES[19]=PNH4
				CXR_OUTPUT_SPECIES_NAMES[20]=POA
				CXR_OUTPUT_SPECIES_NAMES[21]=PEC
				CXR_OUTPUT_SPECIES_NAMES[22]=SOA1
				CXR_OUTPUT_SPECIES_NAMES[23]=SOA2
				CXR_OUTPUT_SPECIES_NAMES[24]=SOA3
				CXR_OUTPUT_SPECIES_NAMES[25]=SOA4
				CXR_OUTPUT_SPECIES_NAMES[26]=SOA5
				CXR_OUTPUT_SPECIES_NAMES[27]=SOA6
				CXR_OUTPUT_SPECIES_NAMES[28]=SOA7
				CXR_OUTPUT_SPECIES_NAMES[29]=SOPA
				CXR_OUTPUT_SPECIES_NAMES[30]=SOPB
				CXR_OUTPUT_SPECIES_NAMES[31]=NA
				CXR_OUTPUT_SPECIES_NAMES[32]=PCL
				CXR_OUTPUT_SPECIES_NAMES[33]=FPRM
				CXR_OUTPUT_SPECIES_NAMES[34]=FCRS
				CXR_OUTPUT_SPECIES_NAMES[35]=CPRM
				CXR_OUTPUT_SPECIES_NAMES[36]=CCRS
				;;
			
			*)	cxr_main_die_gracefully "The chemical mechanism ${CXR_CHEMICAL_MECHANISM} is not supported by this configuration file"
				;;
			
		esac # Chemical Mechanism
		;;
		
	*) cxr_main_die_gracefully "The aerosol mechanism ${CXR_AEROSOL_MECHANISM} is not supported by this configuration file!"
	   ;;
		
		
esac # Aerosol Mechanism

# Automatically count # species (subtract 1 for dummy)
CXR_NUMBER_OF_OUTPUT_SPECIES=$(( ${#CXR_OUTPUT_SPECIES_NAMES[@]} - 1 ))


################################################################################
# Probing settings, Set this by hand - the installer does not touch it!
# If you do not want to use probing - ignore this and set CXR_PROBING_TOOL to "None"
################################################################################

# Set probing only if needed
if [[ "$CXR_PROBING_TOOL" != "None"  ]]
then

	CXR_NUMBER_OF_PA_DOMAINS=2
	
	# Entry 0 is a dummy, we want our arrays to start with index 1
	CXR_WITHIN_CAMX_GRID[0]=-
	CXR_PA_BEG_I_INDEX[0]=-
	CXR_PA_END_I_INDEX[0]=-
	CXR_PA_BEG_J_INDEX[0]=-
	CXR_PA_END_J_INDEX[0]=-
	CXR_PA_BEG_K_INDEX[0]=-
	CXR_PA_END_K_INDEX[0]=-
	
	CXR_WITHIN_CAMX_GRID[1]=3  # Specify which CAMx grid that this PA domain is in
	CXR_PA_BEG_I_INDEX[1]=62
	CXR_PA_END_I_INDEX[1]=76
	CXR_PA_BEG_J_INDEX[1]=63
	CXR_PA_END_J_INDEX[1]=74
	CXR_PA_BEG_K_INDEX[1]=1
	CXR_PA_END_K_INDEX[1]=5
	
	CXR_WITHIN_CAMX_GRID[2]=3  # Specify which CAMx grid that this PA domain is in
	CXR_PA_BEG_I_INDEX[2]=75
	CXR_PA_END_I_INDEX[2]=92
	CXR_PA_BEG_J_INDEX[2]=14
	CXR_PA_END_J_INDEX[2]=30
	CXR_PA_BEG_K_INDEX[2]=1
	CXR_PA_END_K_INDEX[2]=5
	
fi

################################################################################
### All following settings for OSAT, PSAT, DDM and RTRAC come from the       ###
### sample file of CAMx 4.51. They are untested.			                       ###
################################################################################

################################################################################
# Probing settings: OSAT/PSAT
################################################################################
if [[    "$CXR_PROBING_TOOL" == "OSAT" || "$CXR_PROBING_TOOL" == "PSAT" || "$CXR_PROBING_TOOL" == "GOAT" || "$CXR_PROBING_TOOL" == "APCA"     ]] 
then

	CXR_SA_SUMMARY_OUTPUT=true
	
	CXR_SA_MASTER_SFC_OUTPUT=true
	CXR_SA_NESTED_SFC_OUTPUT=true
	CXR_SA_STRATIFY_BOUNDARY=true
	CXR_SA_NUMBER_OF_SOURCE_REGIONS=10
	CXR_SA_NUMBER_OF_SOURCE_GROUPS=3
	CXR_USE_LEFTOVER_GROUP=false
	CXR_NUMBER_OF_TIMING_RELEASES=0
	CXR_PSAT_TREAT_SULFATE_CLASS=false
	CXR_PSAT_TREAT_NITRATE_CLASS=true
	CXR_PSAT_TREAT_SOA_CLASS=false
	CXR_PSAT_TREAT_PRIMARY_CLASS=false
	CXR_PSAT_TREAT_MERCURY_CLASS=false
	CXR_PSAT_TREAT_OZONE_CLASS=false
	
	# OSAT Input Files
	
	CXR_SA_RECEPTOR_DEFINITIONS_INPUT_FILE='osat.rcp.def.dat'
	
	##################Source group and grid specific########################
	
	# The first index is the source group, the second is the grid
	CXR_SA_EMISS_GROUP_GRID_1_1_INPUT_FILE=''
	
	# OSAT FILE RULES non-specific
	
	CXR_SA_MASTER_RESTART_FILE_RULE='${CXR_OUTPUT_DIR}/${CXR_RUN}.${CXR_DATE_RAW_YESTERDAY}.osat.inst'
	CXR_SA_NESTED_RESTART_FILE_RULE='${CXR_OUTPUT_DIR}/${CXR_RUN}.${CXR_DATE_RAW_YESTERDAY}.osat.finst'
	
	##################Grid spcecific########################################
	# These NEED the variable ${i} somewhere  (the grid number)
	
	CXR_SA_SOURCE_AREA_MAP_FILE_RULE='osat.src.area.map.g${i}'
	
	##################Source group specific#################################
	# These NEED the variable ${j} somewhere (the source group number)
	
	CXR_SA_POINTS_GROUP_FILE_RULE='ptsrc.${j}.${CXR_DATE_RAW}.bin'
	

################################################################################
# Probing settings:DDM
################################################################################
elif [[ "$CXR_PROBING_TOOL" == "DDM"  ]] 
then

	CXR_DDM_MASTER_SFC_OUTPUT=true
	CXR_DDM_NESTED_SFC_OUTPUT=true
	CXR_DDM_STRATIFY_BOUNDARY=true
	
	CXR_DDM_NUMBER_OF_SOURCE_REGIONS=10
	CXR_DDM_NUMBER_OF_SOURCE_GROUPS=1
	CXR_NUMBER_OF_BC_SPECIES_GROUPS=1
	CXR_NUMBER_OF_EM_SPECIES_GROUPS=2
	CXR_NUMBER_OF_IC_SPECIES_GROUPS=1
	
	# DDM Input Files
	
	CXR_DDM_INITIAL_CONDITIONS_INPUT_FILE='ic.generic.bin'
	CXR_DDM_BOUNDARY_CONDITIONS_INPUT_FILE='bc.generic.bin'
	CXR_DDM_TOP_CONCENTRATIONS_INPUT_FILE='tc.generic'
	CXR_DDM_RECEPTOR_DEFINITIONS_INPUT_FILE='ddm.rcp.def.dat'
	
	##################Source group and grid specific########################
	
	# The first index is the source group, the second is the grid
	# You need one file rule for each element for the source group/grid matrix
	CXR_DDM_EMISS_GROUP_GRID_1_1_FILE_RULE='emiss.utils_1.${CXR_DATE_RAW}.bin'
	CXR_DDM_EMISS_GROUP_GRID_1_2_FILE_RULE='emiss.utils_2.${CXR_DATE_RAW}.bin'
	CXR_DDM_EMISS_GROUP_GRID_1_3_FILE_RULE='emiss.utils_3.${CXR_DATE_RAW}.bin'
	
	# DDM FILE RULES non-specific
	CXR_DDM_MASTER_RESTART_FILE_RULE='${CXR_OUTPUT_DIR}/${CXR_RUN}.${CXR_DATE_RAW_YESTERDAY}.ddm.inst'
	CXR_DDM_NESTED_RESTART_FILE_RULE='${CXR_OUTPUT_DIR}/${CXR_RUN}.${CXR_DATE_RAW_YESTERDAY}.ddm.finst'
	
	CXR_EMIS_SPECIES_GROUPS[1]=O3
	CXR_EMIS_SPECIES_GROUPS[1]=NOX
	
	CXR_IC_SPECIES_GROUPS[1]=O3
	
	CXR_BC_SPECIES_GROUPS[1]=O3
	
	
	##################Grid spcecific########################################
	# These NEED the variable ${i} somewhere  (the grid number)
	
	CXR_DDM_SOURCE_AREA_MAP_FILE_RULE='ddm.src.area.map.g${i}'
	
	##################Source group specific#################################
	# These NEED the variable ${j} somewhere (the source group number)
	CXR_SA_POINTS_GROUP_INPUT_FILE_RULE='ptsrc.${j}.${CXR_DATE_RAW}.bin' 

################################################################################
# Probing settings:RTRAC
################################################################################
elif [[ "$CXR_PROBING_TOOL" == "RTRAC"  ]] 
then

	CXR_RT_PIG_SAMPLE=false
	
	# RTRAC Input Files
	CXR_RT_INITIAL_CONDITIONS_INPUT_FILE=''
	CXR_RT_BOUNDARY_CONDITIONS_INPUT_FILE=''
	CXR_RT_TOP_CONCENTRATIONS_INPUT_FILE=''
	CXR_RT_CHEMISTRY_PARAMETERS_INPUT_FILE='CAMx4.chemparam.rtrac_test'
	CXR_RT_RECEPTOR_DEFINITIONS_INPUT_FILE='receptor.rtrac.test'
	CXR_RT_POINT_SOURCES_INPUT_FILE='pt.rtrac.test'
	
	# RTRAC FILE RULES non-specific
	CXR_RT_MASTER_RESTART_RESTART_FILE_RULE='${CXR_OUTPUT_DIR}/${CXR_RUN}.${CXR_DATE_RAW_YESTERDAY}.rtrac.inst'
	CXR_RT_NESTED_RESTART_FILE_RULE='${CXR_OUTPUT_DIR}/${CXR_RUN}.${CXR_DATE_RAW_YESTERDAY}.rtrac.finst'
	
	##################Grid spcecific########################################
	# These NEED the variable ${i} somewhere  (the grid number)
	
fi

################################################################################
# END PROBING
################################################################################

################################################################################
# CAMxRunner specific settings
################################################################################

#######################################
# Config settings
#######################################

#Postprocessors
# This is the IDL procedure for the extraction
# Because it is not executable, we treat it as an input file
CXR_STATION_PROC_INPUT_FILE=${CXR_POSTPROCESSOR_DAILY_INPUT_DIR}/extract_nabel_stations.pro

################################################################################
# Model options (Solver etc.)
################################################################################
CXR_DIAGNOSTIC_ERROR_CHECK=@CXR_DIAGNOSTIC_ERROR_CHECK@    # True=will stop before 1st timestep
CXR_ADVECTION_SOLVER=@CXR_ADVECTION_SOLVER@      # (PPM,BOTT)
CXR_CHEMISTRY_SOLVER=@CXR_CHEMISTRY_SOLVER@      # (CMC,IEH,LSODE)

CXR_PROBING_TOOL=@CXR_PROBING_TOOL@     # (None,OSAT,PSAT,GOAT,APCA,DDM,PA,RTRAC)
CXR_CHEMISTRY=@CXR_CHEMISTRY@
CXR_DRY_DEPOSITION=@CXR_DRY_DEPOSITION@
CXR_WET_DEPOSITION=@CXR_WET_DEPOSITION@
CXR_STAGGERED_WINDS=@CXR_STAGGERED_WINDS@
CXR_GRIDDED_EMISSIONS=@CXR_GRIDDED_EMISSIONS@
CXR_POINT_EMISSIONS=@CXR_POINT_EMISSIONS@
CXR_IGNORE_EMISSION_DATES=@CXR_IGNORE_EMISSION_DATES@

# Generate PIG relevant code
CXR_PLUME_IN_GRID=@CXR_PLUME_IN_GRID@
CXR_PIG_SUBMODEL=@CXR_PIG_SUBMODEL@     # (None,GREASD,IRON)

################################################################################
# Output specifications
################################################################################

CXR_AVERAGE_OUTPUT_3D=@CXR_AVERAGE_OUTPUT_3D@
CXR_HDF_FORMAT_OUTPUT=@CXR_HDF_FORMAT_OUTPUT@

################################################################################
################################################################################
# Changes below this Comment are rather unusual
################################################################################
################################################################################


################################################################################
# Name/Path of the CAMx.in file
################################################################################

# Do not check this file for existence, hence name does not end in _FILE
# The basename must be "CAMx.in"
CXR_MODEL_CTRL_FILE=CAMx.in

################################################################################
# Machine name
################################################################################

CXR_MACHINE=$(uname -n)

################################################################################
# Executables
################################################################################

#Base path to find the CAMx executable is defined in the directory section!

# Lock manager
CXR_LOCK_MAN_EXEC=${CXR_BIN_DIR}/lock.sh

#### It is recommended to compile the binaries
#### into binaries that carry the machine type in their name
#### CAMxRunner -I can do this for you.
#### 
#### Also the function cxr_main_get_binary_name then selects the most appropriate one.
#### replace with literal for your own choice

# The binary to convert bin2asc
CXR_BIN2ASC_EXEC=$(cxr_main_get_binary_name bin2asc)

# The binary to convert asc2bin
CXR_ASC2IBN_EXEC=$(cxr_main_get_binary_name asc2bin)

# The binary to convert Emission ascii to Bin
CXR_AIRCONV_EXEC=$(cxr_main_get_binary_name airconv)

# The binary to convert MM5 Input
CXR_UAMVASCII_EXEC=$(cxr_main_get_binary_name uamvascii)

# Yet another binary to convert MM5 Input
CXR_AIRASCII_EXEC=$(cxr_main_get_binary_name airascii)

################################################################################
# bin2asc conversion
################################################################################
#No leading dot please
CXR_ASC_EXT=asc

################################################################################
# The OPM and EXEC Settings are machine-dependent.
################################################################################ 
# Are we on a multicore system?
CXR_NUM_CORES=$(cxr_main_count_cores)

# Maximal number of parallel processes in the CAMxRunner
if [[ "$CXR_PARALLEL_PROCESSING" == true  ]]
then
	# By default, use the number of cores
	CXR_MAX_PARALLEL_PROCS=$CXR_NUM_CORES
else
	# Non-Parallel use 1 process
	CXR_MAX_PARALLEL_PROCS=1
fi

# This can be either None, OMP or MPI (MPI is not currently implemented). OMP is recommended.
# the CAMx executable mifust provide this!
CXR_PARALLEL_PARADIGM=OMP

################################################################################
# OpenMP settings
################################################################################
# Switch on OMP
# If num_cores=1, OMP is turned off

# Number of CPUS
export NCPUS=$CXR_NUM_CORES

# Set number of OpenMP threads (usually equals CPU number)
export OMP_NUM_THREADS=$CXR_NUM_CORES

# Per Thread stack size. Can lead to problems if both to large or too small...
export MPSTKZ=128M

################################################################################
# Determine name of executable.
################################################################################

# If you need another executable name,
# replace this function call by a constant.
# The advantage of the function is that it makes sure the executable is present.
CXR_MODEL_EXEC=$(get_model_exec)

