#!/usr/bin/env bash
#
# Runner script for CAMx
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
# Version: $Id$ 
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# 
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Find the log of this file with "svn log CAMxRunner.sh"
################################################################################
# Important note on Variabe names
################################################################################
#
# About:
# The code here depends on a naming convention for variables because it reads
# variables dynamically from the environment. These are the rules:
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
################################################################################
# TODO:
################################################################################
# Define a few variables we need early, will be potentially overwritten by 
# base.conf and run-specific conf.

# First unset any CXR variables in the environment.
unset ${!CXR_*}

# Keep given arguments in mind
CXR_ARGUMENTS="${@}"

################################################################################
# Function: main_usage
#
# Shows the proper usage of CAMxRunner. 
# The rest of the main_* functions is defined in inc/main_functions.inc
#
################################################################################
function main_usage()
################################################################################
{
	# That we see the modules, we need at least verbose level
	CXR_LOG_LEVEL_SCREEN=$CXR_LOG_LEVEL_VRB
	
	# At least in theory compatible with help2man
	# We add - to remove leading tab
	less <<-EOF
	  ----------------------------------------------------------------------------
	  Usage: ${progname} [options]
	  ----------------------------------------------------------------------------

	  ${progname} - Modular runner for CAMx 
	  (Comprehensive Air quality Model with extensions) 
	  and other air quality models.

	  Find detailed information here: 
	  http://people.web.psi.ch/oderbolz/CAMxRunner

	  This is revision \$Rev$ of CAMxRunner

	  ----------------------------------------------------------------------------

	  Written by Daniel C. Oderbolz (dco)
	  CAMxRunner@psi.ch

	  ----------------------------------------------------------------------------
	  Options:
	  -h    shows this screen (quit by pressing 'q')

	  -I    Starts the installation of CAMxRunner (interactive)
	  
	  -T    Starts a test of the current Installation

	  -d    causes a dry run

	  -F    overwrites existing output files (force)

	  -w    wait for missing input files

	  -l    writes a logfile for dry-runs as well

	  -v    verbose screen: talkative script (if given twice you get even more information)

	  -V    verbose logfile: talkative script (if given twice you get even more information)

	  -c    cleanup: removes state information

	  -m    allow multiple instances of the runner on the same run 
	        (experts only - not recommended)

	  -t<n> set the threshold for allowed errors (Default ${CXR_ERROR_THRESHOLD}).
	        a threshold of ${CXR_NO_ERROR_THRESHOLD} ignores errors. 

	  -s    stop this run gracefully (stop all runners executing this run)
	  
	  -DYYYY-MM-DD execute a specific simulation day given in the form YYYY-MM-DD
	  
	  -L    Leaves tempfiles where they are. Useful for partial runs on compressed input.

	  -Pn   activates parallel execution of pre/postprocessing with 
	        max. n concurrent procs. n must be given!

	  -C    Create a new run, you are guided through the process.

	  -N    do NOT run CAMx, only input and output prep

	  In the following (limiting) options, a module name can be given 
	  if omitted, the whole sub-processing is run
	  If these options are not used, both pre- and postproc is run

	  -x<module>    only runs model modules
	  
	  -p<module>    only one-time preprocessor modules
	  
	  -i<module>    only daily preprocessors step module
	  -o<module>    only daily day postprocessors step module

	  -f<module>    only one-time postprocessor modules
	  
	  Available modules for ${CXR_MODEL} ${CXR_MODEL_VERSION}:
	  ----------------------------------------------------------------------------
	  
	  $(cxr_main_enumerate_all_modules 2>&1) 
	  
	  ----------------------------------------------------------------------------
	  Examples:
	  
	  To create a new run, start:

	  	\$  ./${progname} -C 

	  The script walks you through the creation of the new run.
	  
	  The script then creates link called "runname" (here we assume the run is called CAMx-v4.51-test_run) 
	  to $progname and a config template in 
	  config/CAMx-v4.51-test_run.conf
	  containing exports of all variables needed.
	  
	  Afterwards, the script is no longer called using  "$progname".
	  Instead, use the linkname "runname" (CAMx-v4.51-test_run)
	   
	  Afterwards, run a so called dryrun:
      
	  	\$ "runname" -d
	  	\$ ./CAMx-v4.51-test_run -d
      
	  This will tell you if the run can be successful at all.
      
	  Edit the configuration file if needed and run another dryrun, this time you
	  might want to produce a logfile:
      
	  	\$ "runname" -d -l
	  	\$ ./CAMx-v4.51-test_run -d -l
	  
	  If satisfied, run "properly" (automatically creates a logfile)
	  
	  Before this is possible, you need to clean the state database:

	  	\$ "runname" -c
	  	$ ./CAMx-v4.51-test_run -c
	  	
	  Then start the run:

	  	\$ "runname"
	  	\$ ./CAMx-v4.51-test_run

	  In some environments, you must use a special program to start a long-running job,
	  here is an example for a AFS/Kerberos environment:

	  	# Refresh token
	  	\$ klog
      
	  	# Run 
	  	\$ k5run -b ./CAMx-v4.51-test_run

	  ------------------------------------------------------------------------------
	  Report bugs to <CAMxRunner@psi.ch>.
	  ------------------------------------------------------------------------------
	  Find more info here:
	  http://people.web.psi.ch/oderbolz/CAMxRunner
EOF
exit 1
}

################################################################################
# Constants ####################################################################
################################################################################

# CXR_RUN contains the name of the current script
# CXR_RUN_DIR the directory where CAMxRunner resides
# This variable is so important that we export it
export CXR_RUN="$(basename $0)"
CXR_RUN_DIR="$(dirname $0)"

# Correct CXR_RUN_DIR to absolute
cd ${CXR_RUN_DIR} || ( echo "Could not change to $CXR_RUN_DIR" ; exit 1 )
CXR_RUN_DIR="$(pwd)"


################################################################################
# Source most important functions (rest is done later dynamically and version dependent)
################################################################################
source $CXR_RUN_DIR/inc/main_functions.inc

################################################################################
# Extract the CAMx Version from the Link Name and test it   ####################
################################################################################

# Determine Model name (CXR_MODEL) and version (CXR_MODEL_VERSION)
# from the run name and export it to the environment.
# Also set the directories where to look for commom modules and 
# processors
cxr_main_determine_model_and_version ${CXR_RUN}

# Load defaults
source $CXR_RUN_DIR/inc/defaults.inc



################################################################################
# Handle options ###############################################################
################################################################################
# Get the users input here (done this early to avoid loading of stuff we do not need -
# it makes no sense to load all modules if the users only wants to see the help screen)
#
# The downside of this approach is that we need to carry the users settings in 
# temporary variables, because we want the command line options to have precedence over 
# the configuration settings. All these variables have the name
# CXR_USER_TEMP_ to make automatic extraction easier.

# When using getopts, never directly call a function inside the case,
# otherwise getopts does not process any parametres that come later
while getopts ":dlvVFwmct:sD:LP:ITxi:o:CNp:f:h" opt
do
	case "${opt}" in
		d) CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_LOG_EXT="-dry" ;;
		l) CXR_USER_TEMP_FORCE_LOG=true ;;
		v) CXR_LOG_LEVEL_SCREEN=$(( 2 * $CXR_LOG_LEVEL_SCREEN )) ;;
		V) CXR_LOG_LEVEL_FILE=$(( 2 * $CXR_LOG_LEVEL_FILE )) ;;
		F) CXR_USER_TEMP_FORCE=true ;;
		w) CXR_USER_TEMP_WAIT_4_INPUT=true ;;
		m) CXR_USER_TEMP_ALLOW_MULTIPLE=true ;;
		c) CXR_HOLLOW=true; CXR_USER_TEMP_CLEANUP=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		t) CXR_USER_TEMP_ERROR_THRESHOLD=${OPTARG} ;;
		s) CXR_HOLLOW=true; CXR_USER_TEMP_STOP_RUN=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		D) CXR_USER_TEMP_ONE_DAY=${OPTARG} ;;
		L) CXR_USER_TEMP_REMOVE_DECOMPRESSED_FILES=false ;;
		P) CXR_USER_TEMP_PARALLEL_PROCESSING=true ; CXR_USER_TEMP_MAX_PARALLEL_PROCS=${OPTARG} ;;
	
		# Installer: We need to manipulate the CXR_RUN variable for now
		I) CXR_RUN=${CXR_INSTALLER}; CXR_HOLLOW=true; CXR_USER_TEMP_INSTALL=true; CXR_DO_FILE_LOGGING=false ;;
		
		# Testing
		T) CXR_HOLLOW=true; CXR_LOG_LEVEL_SCREEN=${CXR_LOG_LEVEL_INF} ; CXR_USER_TEMP_RUN_TESTS=true ;;
	
		C) CXR_HOLLOW=true; CXR_USER_TEMP_CREATE_NEW_RUN=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		
		N) CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_RUN_MODEL=false ;;
		
		p) CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_RUN_MODEL=false; CXR_USER_TEMP_RUN_PRE_ONCE=true; CXR_USER_TEMP_RUN_PRE_DAILY=false; CXR_USER_TEMP_RUN_POST_DAILY=false; CXR_USER_TEMP_RUN_POST_ONCE=false; CXR_USER_TEMP_RUN_PRE_ONCE_STEP="${OPTARG}" ;;
		i) CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_RUN_MODEL=false; CXR_USER_TEMP_RUN_PRE_ONCE=false; CXR_USER_TEMP_RUN_PRE_DAILY=true; CXR_USER_TEMP_RUN_POST_DAILY=false; CXR_USER_TEMP_RUN_POST_ONCE=false; CXR_USER_TEMP_RUN_PRE_DAILY_STEP="${OPTARG}"  ;;
		o) CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_RUN_MODEL=false; CXR_USER_TEMP_RUN_PRE_ONCE=false; CXR_USER_TEMP_RUN_PRE_DAILY=false; CXR_USER_TEMP_RUN_POST_DAILY=true; CXR_USER_TEMP_RUN_POST_ONCE=false; CXR_USER_TEMP_RUN_POST_DAILY_STEP="${OPTARG}" ;;
		f) CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_RUN_MODEL=false; CXR_USER_TEMP_RUN_PRE_ONCE=false; CXR_USER_TEMP_RUN_PRE_DAILY=false; CXR_USER_TEMP_RUN_POST_DAILY=false; CXR_USER_TEMP_RUN_POST_ONCE=true; CXR_USER_TEMP_RUN_POST_ONCE_STEP="${OPTARG}" ;;
		
		x) CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_RUN_MODEL=true; CXR_USER_TEMP_RUN_MODEL_SINGLE_STEP="${OPTARG}" ;;
		
		h) CXR_HOLLOW=true; main_usage ;;
		\?) CXR_HOLLOW=true; main_usage ;; 
		*) CXR_HOLLOW=true; main_usage ;;  # Show usage also for unrecognised options
	esac
done

# This is not strictly needed, but it allows to read 
# non-named command line options
shift $((${OPTIND} - 1))

# Make getopts ready again
unset OPTSTRING
unset OPTIND

################################################################################
# Include all external functions. We do this here to reduce clutter ############
################################################################################

source $CXR_RUN_DIR/inc/load_common_modules.inc

################################################################################
# Read Config ##################################################################
################################################################################

# The run determines the files to use
cxr_main_read_config "${CXR_RUN}" "${CXR_MODEL_VERSION}" "${CXR_MODEL}" "${CXR_RUN_DIR}"


################################################################################
# Correct user-supplied variables ##############################################
################################################################################

# Turn any CXR_USER_TEMP_ into a CXR_ variable
for var in $(set | sort | grep ^CXR_USER_TEMP.*= )
do
	# Now var contains a var=value string
	name="$(echo "${var}" | cut -d= -f1)"
	# Rewrite name
	new_name="${name//CXR_USER_TEMP/CXR}"
	value="$(echo "${var}" | cut -d= -f2)"

	# Export rewritten variable (here we need export so that its a command)
	export ${new_name}="$value"
done

#  correct LOG if needed

# Force wins
if [[ "${CXR_FORCE_LOG}" == true  ]]
then
	CXR_DO_FILE_LOGGING=true
fi

if [[ "${CXR_DO_FILE_LOGGING}" == false  ]]
then
	# No logging!
	# Log to the null device
	CXR_CXR_LOG=/dev/null
fi

# Adjust options to control pre- and postprocessors
if [[ "${CXR_RUN_LIMITED_PROCESSING}" == false  ]]
then
	#Run everything. This is needed because if no option is given, we want all.
	CXR_RUN_PRE_ONCE=true
	CXR_RUN_PRE_DAILY=true
	CXR_USER_TEMP_RUN_MODEL=true
	CXR_RUN_POST_DAILY=true
	CXR_RUN_POST_ONCE=true
else
	# Limited processing is not done in parallel
	CXR_MAX_PARALLEL_PROCS=1
	CXR_PARALLEL_PROCESSING=false
fi


#### Check for contradictions and take action (incomplete!)

# either -F or CXR_SKIP_EXISTING
if [[  "${CXR_FORCE}" == true && "${CXR_SKIP_EXISTING}" == true   ]]
then
	# Force wins (CXR_SKIP_EXISTING is default)
	CXR_SKIP_EXISTING=false
fi

# Hollow and non-hollow options should not be mixed
if [[ "${CXR_HOLLOW}" == true && "${CXR_RUN_LIMITED_PROCESSING}" == true ]]
then
	cxr_main_die_gracefully "You have chosen contradicting options. Refer to ${CXR_CALL} -h" false
fi

################################################################################

cxr_main_logger -v -B "CAMxRunner.sh" "Checking if selected options are valid..." 

if [[ $(cxr_main_is_numeric "${CXR_MAX_PARALLEL_PROCS}") == false ]]
then
	cxr_main_die_gracefully "CAMxRunner:${LINENO} - The argument of -P must be numeric!"
fi

if [[ $(cxr_main_is_numeric "${CXR_ERROR_THRESHOLD}") == false  ]]
then
	cxr_main_die_gracefully "CAMxRunner:${LINENO} - The argument of -t must be numeric!"
fi

# Ok, now less than 2 processes is not parallel
if [[ "${CXR_MAX_PARALLEL_PROCS}" -lt 1   ]]
then
	cxr_main_logger -w "CAMxRunner.sh" "You chose to use less than 1 cxr_common_parallel_worker, this will literally not work. I will use 1".
	CXR_MAX_PARALLEL_PROCS=1
	CXR_PARALLEL_PROCESSING=false
fi

cxr_main_logger -v -B "CAMxRunner.sh" "Selected options are valid." 


################################################################################
# Check name of script
################################################################################

# Fewer checks if we are hollow
if [[ "${CXR_HOLLOW}" == false  ]] 
then
	if [[ ${CXR_RUN} == ${CXR_RUNNER_NAME}  ]]
	then
		cxr_main_die_gracefully "CAMxRunner:${LINENO} - You are not using the system properly - use \n \t ${CXR_CALL} -C to create a new run and then call the run instead!"
	else
		
		# Check if the name of the script has changed
		# We look for the target of the link ->
		#                                     ^
		# Notice that the result has a space in front...
		
		link_target="$(ls ${CXR_RUN_DIR}/${CXR_RUN} -l |  cut -d">" -f2)"
		
		if [[ "$link_target" != " ${CXR_RUNNER_NAME}" ]]
		then
			cxr_main_die_gracefully "CAMxRunner:${LINENO} - Probably the ${CXR_RUNNER_NAME} was renamed. Update the variable CXR_RUNNER_NAME in ${CXR_BASECONFIG}"
		fi
	
		# Make sure Run name is ok (Model.vVersion...)
		# XX_check_functions.sh
		cxr_common_check_run_name ${CXR_RUN} || cxr_main_die_gracefully "CAMxRunner:${LINENO} - Sorry, but a Run name (the link you create) must start with the string MODEL.vX.YZ where MODEL is one of $CXR_SUPPORTED_MODELS and X.YZ is the CAMx Version number."

	fi
	
	################################################################################

	# Extend stacksize
	ulimit -s unlimited
	
	################################################################################
	# Revision control
	################################################################################
	
	# Get revisions of configuration and the CAMxRunner.sh
	# The other variables are set in cxr_main_read_config
	CXR_RUNNER_REV=$(cxr_common_get_svn_revision $0)
	
	cxr_main_logger -v -B "CAMxRunner.sh" "Runner (${CXR_RUN}) revision ${CXR_RUNNER_REV}" 
	
	if [[ "$CXR_BASECONFIG_REV" -ne "$CXR_CONFIG_REV"  ]]
	then
		cxr_main_logger -w "CAMxRunner.sh" "The Configuration file ${CXR_CONFIG} \n was derived from an older revision ($CXR_CONFIG_REV) of the $CXR_BASECONFIG file (current revision: ${CXR_BASECONFIG_REV}).\n this is not necessarily bad, but check if the two files agree logically (e. g. using diff) \n\n To recreate the config, consider to rename the existing configuration and do a dry-run: \n \t \$ mv ${CXR_CONFIG} ${CXR_CONFIG}.old \n \t \$ $0 -d\n"	 
	fi
	
fi

################################################################################
# Is the configuration OK?
################################################################################

cxr_main_logger -v -B "CAMxRunner.sh" "Checking CAMxRunner for consistency..." 

# Increase global indent level
cxr_main_increase_log_indent

# XX_check_functions
cxr_common_check_bash_version

# XX_camx_runner_functions.sh
cxr_common_check_runner_consistency

# Decrease global indent level
cxr_main_decrease_log_indent

cxr_main_logger -i -B "CAMxRunner.sh" "CAMxRunner is consistent as far as I can tell." 

################################################################################
# Setting up chemparam file (dependent on CAMx executable and mechanisms)
################################################################################

# Is the chemparam file already set (in the config?)
if [[ "${CXR_CHEMPARAM_INPUT_FILE:-}"  ]]
then
	#String is non-empty, check if it is sensible
	if [[ ! -f ${CXR_CHEMPARAM_INPUT_FILE:-}  ]]
	then
		cxr_main_logger -w "CAMxRunner.sh" "You set the parameter CXR_CHEMPARAM_INPUT_FILE in your configuration, however, the file $CXR_CHEMPARAM_INPUT_FILE cannot be found. I try to find the correct setting."
		# String is not properly set - try to get it
		CXR_CHEMPARAM_INPUT_FILE=$(get_chemparam_file ${CXR_CHEMICAL_MECHANISM} ${CXR_AEROSOL_MECHANISM} )
	fi
else
	CXR_CHEMPARAM_INPUT_FILE=$(get_chemparam_file ${CXR_CHEMICAL_MECHANISM} ${CXR_AEROSOL_MECHANISM} )
fi

################################################################################
################################################################################
#                                                                              #
# All loading done - Start of main program                                     #
#                                                                              #
################################################################################
################################################################################

################################################################################
# Start hollow functions if needed
################################################################################

if [[ "${CXR_HOLLOW}" == true  ]]
then
	#Hollow functions neeed init too
	cxr_common_initialize_state_db

	if [[ "${CXR_CLEANUP}" == true  ]]
	then
		# Delete info in the state DB
		cxr_common_cleanup_state
	elif [[ "${CXR_CREATE_NEW_RUN}" == true  ]]
	then
		# Create a new run
		cxr_common_create_new_run
	elif [[ "${CXR_STOP_RUN}" == true  ]]
	then
		#Delete .CONTINUE files of all instances
		if [[ "$(cxr_common_get_consent "You chose the option -s (stop run). Do you really want to stop the run ${CXR_RUN}?" )" == true  ]]
		then
			cxr_common_delete_continue_files
		fi
	elif [[ "${CXR_INSTALL}" == true  ]]
	then
		# Run the installation
		cxr_common_install
	elif [[ "${CXR_RUN_TESTS}" == true  ]]
	then
		# Run the tests
		cxr_common_test_all_modules
	fi
	
	# Do cleanup
	cxr_main_cleanup
	
	# We are happy
	CXR_STATUS=${CXR_STATUS_SUCCESS}
	exit
fi

### No hollow function gets further here

cxr_main_logger -H "CAMxRunner.sh" "$progname - running stage\nLoading external modules from ${CXR_COMMON_INPUT_DIR}..." 

if [[ "${CXR_ONE_DAY}"  ]]
then

	if [[ "$(cxr_common_is_yyyymmdd_format ${CXR_ONE_DAY})" == true  ]]
	then
		cxr_main_logger -b "CAMxRunner.sh" "We run only day ${CXR_ONE_DAY}!"
	else
		cxr_main_die_gracefully "the -D option needs a date in YYYY-MM-DD format as input!"
	fi

fi

################################################################################
# Check space requirements if we run a full simulation
################################################################################

# count simulation days
CXR_NUMBER_OF_SIM_DAYS=$(cxr_common_days_between "${CXR_START_DATE}" "${CXR_STOP_DATE}")

# Is this a repetition of an earlier run?
if [[ $(cxr_common_is_repeated_run) == true  ]]
then
	cxr_main_logger -i "CAMxRunner.sh" "This run has already been started earlier."
	
	last=$(cxr_common_get_last_day_modelled)
	
	# last could be empty
	if [[ "$last"  ]]
	then
		if [[ "$(cxr_common_get_last_day_modelled)" != ${CXR_STOP_DATE}  ]]
		then
			cxr_main_logger -i "CAMxRunner.sh" "It seems that the number of simulation days changed since the last run. Make sure you repeat all needed steps (e. g. AHOMAP/TUV)"
		fi
	fi
	
fi


mb_needed=$(cxr_common_predict_model_output_megabytes)

cxr_main_logger -i "CAMxRunner.sh" "I estimate that this simulation will take ${mb_needed} MB of space in ${CXR_OUTPUT_DIR}."

if [[ "${CXR_RUN_LIMITED_PROCESSING}" == false  ]]
then
	# Full simulation, do the space check if user has not disabled it
	if [[ "${CXR_CHECK_MODEL_SPACE_REQUIRED}" == true  ]]
	then
		cxr_common_check_mb_needed "${CXR_OUTPUT_DIR}" "${mb_needed}"
		
		# We assume that we need 5% of this space in CXR_TMP_DIR if we do not decompress in place
		if [[ "${CXR_DECOMPRESS_IN_PLACE}" == false  ]]
		then
			cxr_common_check_mb_needed "${CXR_TMP_DIR}" $(cxr_common_fp_calculate "${CXR_TMP_SPACE_FACTOR:-0.05} * ${mb_needed}" 0 false)
		fi
	else
		cxr_main_logger -w "CAMxRunner.sh" "CXR_CHECK_MODEL_SPACE_REQUIRED is false, I will not check if sufficient diskspace is available"
	fi
fi

# Show grid dimensions
cxr_common_report_dimensions

cxr_main_logger -B "CAMxRunner.sh" "Using $CXR_NUMBER_OF_OUTPUT_SPECIES output species"

# Check if the selected binary supports our settings
cxr_common_check_model_limits

################################################################################
# Print out the variables and their settings
################################################################################

INFO="\nThis CAMxRunner has process id $$ and is running on host $(uname -n)\nRun ${CXR_RUN}, we use the ${CXR_MODEL_EXEC} executable. \n It is now $(date)\n\n The script was called as \n \t \$ ${0} ${CXR_ARGUMENTS} \n\n" 

cxr_main_notify "Run $CXR_RUN starts on $CXR_MACHINE" "$INFO"
cxr_main_logger -i "CAMxRunner.sh" "$INFO"

if [[  "${CXR_HOLLOW}" == false || "${CXR_DRY}" == true   ]]
then
	cxr_main_logger -i "CAMxRunner.sh" "Output will be written to ${CXR_OUTPUT_DIR}\nWe run ${CXR_MODEL} ${CXR_MODEL_VERSION} using the chemparam File ${CXR_CHEMPARAM_INPUT_FILE}. We process ${CXR_TUV_NO_OF_REACTIONS} photolytic reactions\n" 
fi

if [[ ${CXR_ERROR_THRESHOLD} != ${CXR_NO_ERROR_THRESHOLD}  ]]
then
	cxr_main_logger -i "CAMxRunner.sh" "In this run, at most ${CXR_ERROR_THRESHOLD} errors will be tolerated before stopping.\n"
else
	cxr_main_logger -i "CAMxRunner.sh" "We ignore the number of errors occurring and keep going because the option -t${CXR_NO_ERROR_THRESHOLD} was used\n"
fi

if [[ ${CXR_SKIP_EXISTING} == true  ]]
then
	cxr_main_logger -w "CAMxRunner.sh" "Existing output files will be skipped."
fi

if [[ ${CXR_FORCE} == true  ]]
then
	cxr_main_logger -w "CAMxRunner.sh" "Existing output files will be deleted."
fi

if [[ "${CXR_LOG_LEVEL_SCREEN}" -ge "${CXR_LOG_LEVEL_VRB}"  ]]
then
	cxr_common_list_cxr_variables
	
	cxr_common_list_system_variables
	
	cxr_main_logger -v -b "CAMxRunner.sh"  "Bash stack size: $(ulimit -s)" 
fi


################################################################################
# Detect other instances #######################################################
################################################################################

# TODO: Rethink this strategy (this check should never be false!)
if [[ "$CXR_HOLLOW" == false  ]]
then

	cxr_main_logger -v -B "CAMxRunner.sh" "Checking if another instance is running on this run..." 
	 
	cxr_common_detect_running_instances
	
	cxr_main_logger -v -B "CAMxRunner.sh" "No other instance found." 


	################################################################################
	# Create a Continue File. If it is gone, the worker thread(s) will stop
	################################################################################

	cxr_common_initialize_state_db || cxr_main_die_gracefully "Could not initialize state DB"
	
	# If we do a dry run, we want to show the detected pre- and postprocessors
	if [[ "$CXR_DRY" == true  ]]
	then
		cxr_main_logger -v -b "CAMxRunner.sh" "Listing all detected pre- and postprocessors...\n"    
		
		# Increase global indent level
		cxr_main_increase_log_indent
		
		cxr_main_logger -v "CAMxRunner.sh" "If you just want to run just a part of either the input or the output preparation\nrun it like\n"    
	
		# Increase global indent level
		cxr_main_increase_log_indent
	
		cxr_main_logger -v "CAMxRunner.sh" "${CXR_CALL} -pXX for a one-time input step,\n"
		cxr_main_logger -v "CAMxRunner.sh" "${CXR_CALL} -iXX for a daily input step,\n"
		cxr_main_logger -v "CAMxRunner.sh" "${CXR_CALL} -oXX for a daily output step or\n"
		cxr_main_logger -v "CAMxRunner.sh" "${CXR_CALL} -fXX for a one-time output step.\n"
		cxr_main_logger -v "CAMxRunner.sh" "  These are the possible values for one-time input steps:\n"
		
		# Decrease global indent level
		cxr_main_decrease_log_indent
		
		# Show what can be done
		cxr_main_enumerate_all_modules
		
	fi
	
	#
	# Here we really start things. Note that the execution of tasks is no longer sequential
	# if not needed (Dryruns are always sequential, though)
	# 
	
	if [[  "$CXR_PARALLEL_PROCESSING" == true && "$CXR_DRY" == false   ]]
	then
		# Creates a process dependency tree
		# XX_task_functions.sh
		cxr_common_parallel_create_task_list
	
		# Creates CXR_MAX_PARALLEL_PROCS cxr_common_parallel_worker processes
		# The workers
		#		Get the next task
		#		Execute the task
		#		Send State DB Info about completion
		#		Ask for the next job
		# XX_task_functions.sh
		cxr_common_spawn_workers $CXR_MAX_PARALLEL_PROCS
	
		################################################################################
		# Make sure that all subprocesses are done!
		################################################################################
		# XX_task_functions.sh
		cxr_common_wait_for_workers
		
		# If we arrive here, we should be done.
		# We can add a good check later.
		
		# We need a way to find out if all workers returned happily to
		# manipulate CXR_STATUS if needed
		
		if [[ "$(cxr_common_parallel_count_open_tasks)" -ne 0  ]]
		then
			cxr_main_logger "CAMxRunner.sh" "The run $CXR_RUN stopped, but there are still $(cxr_common_parallel_count_open_tasks) open tasks!"
			# We are not happy
			CXR_STATUS=$CXR_STATUS_FAILURE
		else
			# We are happy
			CXR_STATUS=$CXR_STATUS_SUCCESS
		fi
		
	else
		# Optimistic by nature
		CXR_STATUS=$CXR_STATUS_SUCCESS
	
		# Sequential processing (module_functions)
		cxr_common_process_sequentially || CXR_STATUS=$CXR_STATUS_FAILURE
	fi
fi

# Do compression if needed
cxr_common_compress_output

# Echo the "Finish message"
cxr_main_logger -i "CAMxRunner.sh" "$(cxr_common_evaluate_rule "$CXR_FINISH_MESSAGE_RULE" true CXR_FINISH_MESSAGE_RULE)"


################################################################################
# Cleanup all locks etc...
################################################################################
cxr_main_cleanup

# Set exit status (too coarse)
case "$CXR_STATUS" in

	"$CXR_STATUS_SUCCESS")
		exit ${CXR_RET_OK} ;;
		
	*)
		exit ${CXR_RET_ERROR} ;;
esac
		
	



