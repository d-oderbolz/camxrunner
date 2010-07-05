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
# TODO: Rewrite detectRunningInstances: local via ps, remote via time of CONTINUE
################################################################################
# Define a few variables we need early, will be potentially overwritten by 
# base.conf and run-specific conf.

# First unset any CXR variables in the environment.
unset ${!CXR_*}

# Keep given arguments in mind
CXR_ARGUMENTS="${@}"

################################################################################
# Function: main.usage
#
# Shows the proper usage of CAMxRunner. 
# The rest of the main.* functions is defined in inc/main_functions.inc
#
################################################################################
function main.usage()
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
	  
	  -n    No removal of tempfiles. Useful for partial runs on compressed input.

	  -Pn   activates parallel execution of pre/postprocessing with 
	        max. n concurrent procs. n must be given!

	  -C    Create a new run, you are guided through the process.
	  
	  -R[name]  Creates a configuration to repeat an existing run. If no rn is given, asks user.

	  The following options allow to run a subset of the modules that make up a run.
	  One approach is to select all modules of a module type (these options can be combined):

	  -x   only runs enabled model modules
	
	  -p   only enabled one-time preprocessor modules
	
	  -i   only enabled daily preprocessors step module
	  -o   only enabled daily day postprocessors step module
	
	  -f   only enabled one-time postprocessor modules
	
	  Or one can enable just a list of specific modules (the order is unimportant):
	
	  -r"list of modules"
	  
	  -L   List all available modules. (this command is sensitive to the run name)
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
main.setModelAndVersion ${CXR_RUN}

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
# otherwise getopts does not process any parameters that come later
# (we are in a loop!)
while getopts ":dlvVFwmct:sD:nP:ITr:xioCRpfLh" opt
do
	case "${opt}" in
		d) 	CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_LOG_EXT="-dry" ;;
		l) 	CXR_USER_TEMP_FORCE_LOG=true ;;
		v) 	CXR_LOG_LEVEL_SCREEN=$(( 2 * $CXR_LOG_LEVEL_SCREEN )) ;;
		V) 	CXR_LOG_LEVEL_FILE=$(( 2 * $CXR_LOG_LEVEL_FILE )) ;;
		F) 	CXR_USER_TEMP_FORCE=true ;;
		w) 	CXR_USER_TEMP_WAIT_4_INPUT=true ;;
		m) 	CXR_USER_TEMP_ALLOW_MULTIPLE=true ;;
		c) 	CXR_HOLLOW=true; CXR_USER_TEMP_CLEANUP=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		t) CXR_USER_TEMP_ERROR_THRESHOLD=${OPTARG} ;;
		s) 	CXR_HOLLOW=true; CXR_USER_TEMP_STOP_RUN=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		D) 	CXR_USER_TEMP_ONE_DAY=${OPTARG} ;;
		n)	CXR_USER_TEMP_REMOVE_DECOMPRESSED_FILES=false ;;
		P)	CXR_USER_TEMP_PARALLEL_PROCESSING=true ; CXR_USER_TEMP_MAX_PARALLEL_PROCS=${OPTARG} ;;
	
		# Installer: We need to manipulate the CXR_RUN variable for now
		I) 	CXR_RUN=${CXR_INSTALLER}; CXR_HOLLOW=true; CXR_USER_TEMP_INSTALL=true; CXR_DO_FILE_LOGGING=false ;;
		
		# Testing
		T) 	CXR_HOLLOW=true; CXR_LOG_LEVEL_SCREEN=${CXR_LOG_LEVEL_INF} ; CXR_USER_TEMP_RUN_TESTS=true ;;
	
		# Creation or re-creation of configuration
		C) 	CXR_HOLLOW=true; CXR_USER_TEMP_CREATE_NEW_RUN=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		R) 	CXR_USER_TEMP_REPEAT_THIS_RUN=${OPTARG}; CXR_HOLLOW=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		
		r) 	CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_RUN_LIST="${OPTARG}" ;;
		p) 	CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_RUN_MODEL=false; CXR_USER_TEMP_RUN_PRE_ONCE=true; CXR_USER_TEMP_RUN_PRE_DAILY=false; CXR_USER_TEMP_RUN_POST_DAILY=false; CXR_USER_TEMP_RUN_POST_ONCE=false ;;
		i) 	CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_RUN_MODEL=false; CXR_USER_TEMP_RUN_PRE_ONCE=false; CXR_USER_TEMP_RUN_PRE_DAILY=true; CXR_USER_TEMP_RUN_POST_DAILY=false; CXR_USER_TEMP_RUN_POST_ONCE=false ;;
		o) 	CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_RUN_MODEL=false; CXR_USER_TEMP_RUN_PRE_ONCE=false; CXR_USER_TEMP_RUN_PRE_DAILY=false; CXR_USER_TEMP_RUN_POST_DAILY=true; CXR_USER_TEMP_RUN_POST_ONCE=false ;;
		f) 	CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_RUN_MODEL=false; CXR_USER_TEMP_RUN_PRE_ONCE=false; CXR_USER_TEMP_RUN_PRE_DAILY=false; CXR_USER_TEMP_RUN_POST_DAILY=false; CXR_USER_TEMP_RUN_POST_ONCE=true ;;
		x) 	CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_RUN_MODEL=true; CXR_USER_TEMP_RUN_MODEL_SINGLE_STEP="${OPTARG}" ;;
		L) 	CXR_HOLLOW=true; CXR_USER_TEMP_LIST_MODULES=true;;
		
		h) CXR_HOLLOW=true; main.usage ;;
		\?) CXR_HOLLOW=true; main.usage ;; 
		*) CXR_HOLLOW=true; main.usage ;;  # Show usage also for unrecognised options
	esac
done

# This is not strictly needed, but it allows to read 
# non-named command line options
shift $((${OPTIND} - 1))

# Make getopts ready again
unset OPTSTRING
unset OPTIND

################################################################################
# Read Config ##################################################################
################################################################################

# The run determines the files to use
main.readConfig "${CXR_RUN}" "${CXR_MODEL}" "${CXR_MODEL_VERSION}" "${CXR_RUN_DIR}"

################################################################################
# Include all external functions. We do this here to reduce clutter ############
################################################################################

source $CXR_RUN_DIR/inc/load_common_modules.inc

################################################################################
# Determine name of model exec                                      ############
################################################################################

# Set the model exec if not user-supplied
if [[ -z "${CXR_MODEL_EXEC:-}" ]]
then
	main.log -a "CXR_MODEL_EXEC was not set by the user, determining name..."
	CXR_MODEL_EXEC="$(get_model_exec)"
fi


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

# If user feels verbose, make it so
if [[ "$CXR_LOG_LEVEL_SCREEN" -ge "$CXR_LOG_LEVEL_DBG" ]]
then
	set -xv
fi

#  correct LOG if needed
# Force wins
if [[ "${CXR_FORCE_LOG}" == true  ]]
then
	CXR_DO_FILE_LOGGING=true
fi

if [[ "${CXR_DO_FILE_LOGGING}" == false ]]
then
	# No logging!
	# Log to the null device
	CXR_LOG=/dev/null
fi

# Adjust options to control pre- and postprocessors
if [[ "${CXR_RUN_LIMITED_PROCESSING}" == false  ]]
then
	#Run everything. This is needed because if no option is given, we want all.
	CXR_RUN_PRE_ONCE=true
	CXR_RUN_PRE_DAILY=true
	CXR_RUN_MODEL=true
	CXR_RUN_POST_DAILY=true
	CXR_RUN_POST_ONCE=true
fi


#### Check for contradictions and take action (incomplete!)

# either -F or CXR_SKIP_EXISTING
if [[  "${CXR_FORCE}" == true && "${CXR_SKIP_EXISTING}" == true ]]
then
	# Force wins (CXR_SKIP_EXISTING is default)
	CXR_SKIP_EXISTING=false
fi

# Hollow and non-hollow options should not be mixed
if [[ "${CXR_HOLLOW}" == true && "${CXR_RUN_LIMITED_PROCESSING}" == true ]]
then
	main.dieGracefully "You have chosen contradicting options. Refer to ${CXR_CALL} -h" false
fi

################################################################################

main.log -v -B "CAMxRunner.sh" "Checking if selected options are valid..." 

if [[ $(main.isNumeric? "${CXR_MAX_PARALLEL_PROCS}") == false ]]
then
	main.dieGracefully "CAMxRunner:${LINENO} - The argument of -P must be numeric!"
fi

if [[ $(main.isNumeric? "${CXR_ERROR_THRESHOLD}") == false  ]]
then
	main.dieGracefully "CAMxRunner:${LINENO} - The argument of -t must be numeric!"
fi

# Ok, now less than 2 processes is not parallel
if [[ "${CXR_MAX_PARALLEL_PROCS}" -lt 1 ]]
then
	main.log -w "CAMxRunner.sh" "You chose to use less than 1 common.parallel.Worker, this will literally not work. I will use 1".
	CXR_MAX_PARALLEL_PROCS=1
fi

main.log -v -B "CAMxRunner.sh" "Selected options are valid." 


################################################################################
# Check name of script
################################################################################

# Fewer checks if we are hollow
if [[ "${CXR_HOLLOW}" == false  ]] 
then
	if [[ ${CXR_RUN} == ${CXR_RUNNER_NAME}  ]]
	then
		main.dieGracefully "CAMxRunner:${LINENO} - You are not using the system properly - use \n \t ${CXR_CALL} -C to create a new run and then call the run instead!"
	else
		
		# Check if the name of the script has changed
		# We look for the target of the link ->
		#                                     ^
		# Notice that the result has a space in front...
		
		link_target="$(ls ${CXR_RUN_DIR}/${CXR_RUN} -l |  cut -d">" -f2)"
		
		if [[ "$link_target" != " ${CXR_RUNNER_NAME}" ]]
		then
			main.dieGracefully "CAMxRunner:${LINENO} - Probably the ${CXR_RUNNER_NAME} was renamed. Update the variable CXR_RUNNER_NAME in ${CXR_BASECONFIG}"
		fi
	
		# Make sure Run name is ok (Model.vVersion...)
		# check_functions.sh
		common.check.RunName ${CXR_RUN} || main.dieGracefully "CAMxRunner:${LINENO} - Sorry, but a Run name (the link you create) must start with the string MODEL.vX.YZ where MODEL is one of $CXR_SUPPORTED_MODELS and X.YZ is the CAMx Version number."

	fi
	
	################################################################################

	# Extend stacksize
	ulimit -s unlimited
	
	################################################################################
	# Revision control
	################################################################################
	
	# Get revisions of configuration and the CAMxRunner.sh
	# The other variables are set in main.readConfig
	CXR_RUNNER_REV=$(main.getRevision $0)
	
	main.log -v -B "CAMxRunner.sh" "Runner (${CXR_RUN}) revision ${CXR_RUNNER_REV}" 
	
	if [[ "$CXR_BASECONFIG_REV" -gt "$CXR_CONFIG_REV" ]]
	then
		main.log -w "CAMxRunner.sh" "The Configuration file $(basename ${CXR_CONFIG}) \n was derived from an older revision ($CXR_CONFIG_REV) of the $CXR_BASECONFIG file (current revision: ${CXR_BASECONFIG_REV}).\n this is not necessarily bad, but check if the two files agree logically (e. g. using diff) \n\n To recreate the config, consider to rename the existing configuration and do a dry-run: \n \t \$ mv ${CXR_CONFIG} ${CXR_CONFIG}.old \n \t \$ $0 -d\n"	 
	fi
	
fi

################################################################################
# Is the configuration OK?
################################################################################

main.log -v -B "CAMxRunner.sh" "Checking CAMxRunner for consistency..." 

# Increase global indent level
main.increaseLogIndent

# check_functions
common.check.BashVersion

# camx_runner_functions.sh
common.check.runner

# Decrease global indent level
main.decreaseLogIndent

main.log -i -B "CAMxRunner.sh" "CAMxRunner is consistent as far as I can tell." 

################################################################################
# Setting up chemparam file (dependent on CAMx executable and mechanisms)
################################################################################

# Is the chemparam file already set (in the config?)
if [[ "${CXR_CHEMPARAM_INPUT_FILE:-}"  ]]
then
	#String is non-empty, check if it is sensible
	if [[ ! -f ${CXR_CHEMPARAM_INPUT_FILE:-}  ]]
	then
		main.log -w "CAMxRunner.sh" "You set the parameter CXR_CHEMPARAM_INPUT_FILE in your configuration, however, the file $CXR_CHEMPARAM_INPUT_FILE cannot be found. I try to find the correct setting."
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

if [[ "${CXR_HOLLOW}" == true ]]
then
	#Hollow functions neeed init too
	common.state.init

	if [[ "${CXR_CLEANUP}" == true  ]]
	then
		# Delete info in the state DB
		common.state.cleanup
	elif [[ "${CXR_CREATE_NEW_RUN}" == true ]]
	then
		# Create a new run
		common.runner.createNewRun
	elif [[ ! -z "${CXR_REPEAT_THIS_RUN:-}" ]]
	then
		# Re-create existing run
		common.runner.recreateRun
	elif [[ "${CXR_STOP_RUN}" == true ]]
	then
		#Delete .CONTINUE files of all instances
		if [[ "$(common.user.getOK "You chose the option -s (stop run). Do you really want to stop the run ${CXR_RUN}?" )" == true  ]]
		then
			common.state.deleteContinueFiles
		fi
	elif [[ "${CXR_INSTALL}" == true ]]
	then
		# Run the installation
		common.install.do
	elif [[ "${CXR_LIST_MODULES}" == true ]]
	then
		# Show possible modules
		common.module.listAllModules
	elif [[ "${CXR_RUN_TESTS}" == true ]]
	then
		# Run the tests
		common.test.all
	fi
	
	# Do cleanup
	main.doCleanup
	
	# We are happy
	CXR_STATUS=${CXR_STATUS_SUCCESS}
	exit
fi

### No hollow function gets further here

main.log -H "CAMxRunner.sh" "$progname - running stage\nLoading external modules from ${CXR_COMMON_INPUT_DIR}..." 

if [[ "${CXR_ONE_DAY}"  ]]
then

	if [[ "$(common.date.isYYYYMMDD? ${CXR_ONE_DAY})" == true  ]]
	then
		main.log -b "CAMxRunner.sh" "We run only day ${CXR_ONE_DAY}!"
	else
		main.dieGracefully "the -D option needs a date in YYYY-MM-DD format as input!"
	fi

fi

################################################################################
# Check space requirements if we run a full simulation
################################################################################

# count simulation days
CXR_NUMBER_OF_SIM_DAYS=$(common.date.DaysBetween "${CXR_START_DATE}" "${CXR_STOP_DATE}")

# Is this a repetition of an earlier run?
if [[ $(common.state.isRepeatedRun?) == true ]]
then
	main.log -i "CAMxRunner.sh" "This run has already been started earlier."
	
	last="$(common.state.getLastDayModelled)"
	
	# last could be empty
	if [[ "$last" ]]
	then
		if [[ "$last" != ${CXR_STOP_DATE} ]]
		then
			main.log -i "CAMxRunner.sh" "It seems that the number of simulation days changed since the last run. Make sure you repeat all needed steps (e. g. AHOMAP/TUV)"
		fi
	fi
	
	# Its dangerous if a run has been extended at the beginning
	first="$(common.state.getFirstDayModelled)"
	
	# first could be empty
	if [[ "$first" ]]
	then
		if [[ "$first" != ${CXR_START_DATE} ]]
		then
			main.dieGracefully "It seems that this run was extended at the beginning. This implies that the existing mapping of simulation days and real dates is broken.\nClean the state DB by running the -c (all) option!"
		fi
	fi
	
fi

mb_needed=$(common.check.PredictModelOutputMb)

main.log -i "CAMxRunner.sh" "I estimate that this simulation will take ${mb_needed} MB of space in ${CXR_OUTPUT_DIR}."

if [[ "${CXR_RUN_LIMITED_PROCESSING}" == false ]]
then
	# Full simulation, do the space check if user has not disabled it
	if [[ "${CXR_CHECK_MODEL_SPACE_REQUIRED}" == true  ]]
	then
		common.check.MbNeeded "${CXR_OUTPUT_DIR}" "${mb_needed}"
		
		# We assume that we need 5% of this space in CXR_TMP_DIR if we do not decompress in place
		if [[ "${CXR_DECOMPRESS_IN_PLACE}" == false  ]]
		then
			common.check.MbNeeded "${CXR_TMP_DIR}" $(common.math.FloatOperation "${CXR_TMP_SPACE_FACTOR:-0.05} * ${mb_needed}" -1 false)
		fi
	else
		main.log -w "CAMxRunner.sh" "CXR_CHECK_MODEL_SPACE_REQUIRED is false, I will not check if sufficient diskspace is available"
	fi
	else
	
		# Limited Processing, we need to fill DISABLED and ENABLED cleverly.
		# When the user selects a certain type of modules, they should be executed as 
		# configured
		
		main.log -a "CXR_SKIP: ${CXR_SKIP_ALL}"
		
		# First we store all current (configured values)
		d_once_pre="$CXR_DISABLED_ONCE_PREPROC"
		d_d_pre="$CXR_DISABLED_DAILY_PREPROC"
		d_model="$CXR_DISABLED_MODEL"
		d_d_post="$CXR_DISABLED_DAILY_POSTPROC"
		d_once_post="$CXR_DISABLED_ONCE_POSTPROC"
		
		# The same for enabled
		e_once_pre="$CXR_ENABLED_ONCE_PREPROC"
		e_d_pre="$CXR_ENABLED_DAILY_PREPROC"
		e_model="$CXR_ENABLED_MODEL"
		e_d_post="$CXR_ENABLED_DAILY_POSTPROC"
		e_once_post="$CXR_ENABLED_ONCE_POSTPROC"
		
		# Then, we disable all (later, we enable selectively)
		CXR_DISABLED_ONCE_PREPROC="${CXR_SKIP_ALL}"
		CXR_DISABLED_DAILY_PREPROC="${CXR_SKIP_ALL}"
		CXR_DISABLED_MODEL="${CXR_SKIP_ALL}"
		CXR_DISABLED_DAILY_POSTPROC="${CXR_SKIP_ALL}"
		CXR_DISABLED_ONCE_POSTPROC="${CXR_SKIP_ALL}"
		
		CXR_ENABLED_ONCE_PREPROC=""
		CXR_ENABLED_DAILY_PREPROC=""
		CXR_ENABLED_MODEL=""
		CXR_ENABLED_DAILY_POSTPROC=""
		CXR_ENABLED_ONCE_POSTPROC=""
		
		
		if [[ ${CXR_RUN_PRE_ONCE} == true ]]
		then
			CXR_DISABLED_ONCE_PREPROC="$d_once_pre"
			CXR_ENABLED_ONCE_PREPROC="$e_once_pre"
		fi
		
		if [[ ${CXR_RUN_PRE_DAILY} == true ]]
		then
			CXR_DISABLED_DAILY_PREPROC="$d_d_pre"
			CXR_ENABLED_DAILY_PREPROC="$e_d_pre"
		fi		
		
		if [[ ${CXR_RUN_MODEL} == true ]]
		then
			CXR_DISABLED_MODEL="$d_model"
			CXR_ENABLED_MODEL="$e_model"
		fi

		if [[ ${CXR_RUN_POST_DAILY} == true ]]
		then
			CXR_DISABLED_DAILY_POSTPROC="$d_d_post"
			CXR_ENABLED_DAILY_POSTPROC="$e_d_post"
		fi
		
		if [[ ${CXR_RUN_POST_ONCE} == true ]]
		then
			CXR_DISABLED_ONCE_POSTPROC="$d_once_post"
			CXR_ENABLED_ONCE_POSTPROC="$e_once_post"
		fi
		
		# Now, we must deal with -r.
		if [[ "$(common.string.trim "$CXR_RUN_LIST")" ]]
		then
			# There are arguments
			# we reset all specifically enabled modules
			# (-r means "run ONLY these additional modules")
			CXR_ENABLED_ONCE_PREPROC=""
			CXR_ENABLED_DAILY_PREPROC=""
			CXR_ENABLED_MODEL=""
			CXR_ENABLED_DAILY_POSTPROC=""
			CXR_ENABLED_ONCE_POSTPROC=""
			
			# Decode arguments
			for module_name in $CXR_RUN_LIST
			do
				# Determine type
				module_type="$(common.module.getType "$module_name")"
				
				case "$module_type" in
				
					"${CXR_TYPE_PREPROCESS_ONCE}" ) 
						CXR_ENABLED_ONCE_PREPROC="$CXR_ENABLED_ONCE_PREPROC $module_name";;
						
					"${CXR_TYPE_PREPROCESS_DAILY}" ) 
						CXR_ENABLED_DAILY_PREPROC="$CXR_ENABLED_DAILY_PREPROC $module_name";;
						
					"${CXR_TYPE_MODEL}" ) 
						CXR_ENABLED_MODEL="$CXR_ENABLED_MODEL $module_name";;
						
					"${CXR_TYPE_POSTPROCESS_DAILY}" ) 
						CXR_ENABLED_DAILY_POSTPROC="$CXR_ENABLED_DAILY_POSTPROC $module_name";;
						
					"${CXR_TYPE_POSTPROCESS_ONCE}" ) 
						CXR_ENABLED_ONCE_POSTPROC="$CXR_ENABLED_ONCE_POSTPROC $module_name";;
						
					* ) 
						main.dieGracefully "Module type $module_type not supported to be used with -r" ;;
			
				esac
			
			done
			
		else
			main.log -v "No -r argument found"
		fi
		
	
fi # Limited Processing?

# Show grid dimensions
common.runner.reportDimensions

main.log -B "CAMxRunner.sh" "Using $CXR_NUMBER_OF_OUTPUT_SPECIES output species"

# Check if the selected binary supports our settings
common.check.ModelLimits

################################################################################
# Print out the variables and their settings
################################################################################

INFO="\nThis CAMxRunner has process id ${CXR_PID} and is running on host $(uname -n)\nRun ${CXR_RUN}, we use the ${CXR_MODEL_EXEC} executable. \n It is now $(date)\n\n The script was called as \n \t \$ ${0} ${CXR_ARGUMENTS} \n\n" 

main.sendMessage "Run $CXR_RUN starts on $CXR_MACHINE" "$INFO"
main.log -i "CAMxRunner.sh" "$INFO"

if [[  "${CXR_HOLLOW}" == false || "${CXR_DRY}" == true   ]]
then
	main.log -i "CAMxRunner.sh" "Output will be written to ${CXR_OUTPUT_DIR}\nWe run ${CXR_MODEL} ${CXR_MODEL_VERSION} using the chemparam File ${CXR_CHEMPARAM_INPUT_FILE}. We process ${CXR_TUV_NO_OF_REACTIONS} photolytic reactions\n" 
fi

if [[ ${CXR_ERROR_THRESHOLD} != ${CXR_NO_ERROR_THRESHOLD}  ]]
then
	main.log -i "CAMxRunner.sh" "In this run, at most ${CXR_ERROR_THRESHOLD} errors will be tolerated before stopping.\n"
else
	main.log -i "CAMxRunner.sh" "We ignore the number of errors occurring and keep going because the option -t${CXR_NO_ERROR_THRESHOLD} was used\n"
fi

if [[ ${CXR_SKIP_EXISTING} == true  ]]
then
	main.log -w "CAMxRunner.sh" "Existing output files will be skipped."
fi

if [[ ${CXR_FORCE} == true  ]]
then
	main.log -w "CAMxRunner.sh" "Existing output files will be deleted."
fi

if [[ "${CXR_LOG_LEVEL_SCREEN}" -ge "${CXR_LOG_LEVEL_VRB}"  ]]
then
	common.variables.list
	
	common.variables.listSystemVars
	
	main.log -v -b "CAMxRunner.sh"  "Bash stack size: $(ulimit -s)" 
fi


################################################################################
# Detect other instances #######################################################
################################################################################

# TODO: Rethink this strategy (this check should never be false!)
if [[ "$CXR_HOLLOW" == false  ]]
then

	main.log -v -B "CAMxRunner.sh" "Checking if another instance is running on this run..." 
	 
	common.state.detectInstances
	
	main.log -v -B "CAMxRunner.sh" "No other instance found." 


	################################################################################
	# Create a Continue File. If it is gone, the worker thread(s) will stop
	################################################################################

	common.state.init || main.dieGracefully "Could not initialize state DB"
	
	# If we do a dry run, we want to show the detected pre- and postprocessors
	if [[ "$CXR_DRY" == true  ]]
	then
		main.log -v -b "CAMxRunner.sh" "Listing all detected pre- and postprocessors...\n"
		
		# Increase global indent level
		main.increaseLogIndent
		
		main.log -v "CAMxRunner.sh" "If you just want to run just a part of either the input or the output preparation\nrun it like\n"
	
		# Increase global indent level
		main.increaseLogIndent
	
		main.log -v "CAMxRunner.sh" "${CXR_CALL} -r"module_name_list"\n"
		common.module.listAllModules

		
		# Decrease global indent level
		main.decreaseLogIndent
		
	fi
	
	# Here we really start things. Note that the execution of tasks is no longer sequential
	# if not needed 

	if [[ "$CXR_PARALLEL_PROCESSING" == true ]]
	then
		# Creates a process dependency tree
		# parallel_functions.sh
		common.parallel.init
	
		# Creates CXR_MAX_PARALLEL_PROCS common.parallel.Worker processes
		# The workers then carry out the tasks in parallel
		common.parallel.spawnWorkers $CXR_MAX_PARALLEL_PROCS
	
		################################################################################
		# Make sure that all subprocesses are done!
		################################################################################
		# parallel_functions.sh
		common.parallel.waitForWorkers
		
		# If we arrive here, we should be done.
		# We can add a good check later.
		
		# We need a way to find out if all workers returned happily to
		# manipulate CXR_STATUS if needed
		
		if [[ "$(common.parallel.countOpenTasks)" -ne 0  ]]
		then
			main.log "CAMxRunner.sh" "The run $CXR_RUN stopped, but there are still $(common.parallel.countOpenTasks) open tasks!"
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
		common.module.processSequentially || CXR_STATUS=$CXR_STATUS_FAILURE
	fi
fi

# Do compression if needed
common.fs.CompressOutput

# Echo the "Finish message"
main.log -i "CAMxRunner.sh" "$(common.runner.evaluateRule "$CXR_FINISH_MESSAGE_RULE" true CXR_FINISH_MESSAGE_RULE)"


################################################################################
# Cleanup all locks etc...
################################################################################
main.doCleanup

# Set exit status (too coarse)
case "$CXR_STATUS" in

	"$CXR_STATUS_SUCCESS")
		exit ${CXR_RET_OK} ;;
		
	*)
		exit ${CXR_RET_ERROR} ;;
esac
		
	



