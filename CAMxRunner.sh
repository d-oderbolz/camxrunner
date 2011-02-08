#!/usr/bin/env bash
#
# Title: This is the main script of CAMxRunner.
#
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
# TODO: each -eq -gt -lt -le and == must be made FP safe if numeric
################################################################################
# Define a few variables we need early, will be potentially overwritten by 
# base.conf and run-specific conf.

# First unset any CXR variables in the environment.
unset ${!CXR_*}

# Keep given arguments in mind
CXR_ARGUMENTS="${@}"

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

	  This is revision \$Rev: 4020 $ of CAMxRunner

	  ----------------------------------------------------------------------------

	  Written by Daniel C. Oderbolz (dco)
	  CAMxRunner@psi.ch

	  ----------------------------------------------------------------------------
	  Options:
	  -h    shows this screen (quit by pressing 'q')
	  
	  -S    Shows a Summary of this run

	  -I    Starts the installation of CAMxRunner (interactive)
	  
	  -T    Starts a test of the current Installation

	  -d    causes a dry run (always uses a single worker)
	  
	  -l    Log even if in dryrun

	  -F    overwrites existing output files (force)

	  -w    wait for missing input files

	  -v    verbose screen: talkative script (if given more than once you get even more information)

	  -V    verbose logfile: talkative script (if given more than once you get even more information)

	  -c    cleanup: removes state information

	  -m    allow multiple instances of the runner on the same run 
	        Also see -r

	  -t<n> set the threshold for allowed errors (Default ${CXR_ERROR_THRESHOLD}).
	        a threshold of ${CXR_NO_ERROR_THRESHOLD} ignores errors. 

	  -s    stop this run gracefully (stop all runners executing this run)
	  
	  -DYYYY-MM-DD execute a specific simulation day given in the form YYYY-MM-DD
	  
	  -n    No removal of tempfiles. Useful for partial runs on compressed input.

	  -Pn   activates parallel execution of pre/postprocessing with 
	        max. n concurrent procs. n must be given!

	  -C    Create a new run, you are guided through the process.
	  
	  -R    Creates a configuration to repeat this run. User may select another run later.

	  The following options allow to run a subset of the modules that make up a run.
	  One approach is to select all modules of a module type (these options can be combined):

	  -x   only runs enabled model modules
	
	  -p   only enabled one-time preprocessor modules
	
	  -i   only enabled daily preprocessors step module
	  -o   only enabled daily day postprocessors step module
	
	  -f   only enabled one-time postprocessor modules
	
	  Or one can enable just a list of specific modules (the order is unimportant):
	
	  -r"list of modules or types"
	      When using -r together with -m (allow multiple runners), this instance will only work on the modules given.
        This can be used to assign CPU intensive tasks like the model to strong machines.
	  
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
# Also set the directories where to look for common modules and 
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
while getopts ":dSlvVFwmct:sD:nP:ITr:xioCRpfLh" opt
do
	case "${opt}" in
		d) 	CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_PARALLEL_PROCESSING=false ; CXR_USER_TEMP_LOG_EXT="-dry" ;;
		S) 	CXR_HOLLOW=true; CXR_USER_TEMP_LIST_INFO=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		l) 	CXR_USER_TEMP_FORCE_LOG=true;;
		v) 	CXR_LOG_LEVEL_SCREEN=$(( 2 * $CXR_LOG_LEVEL_SCREEN )) ;;
		V) 	CXR_LOG_LEVEL_FILE=$(( 2 * $CXR_LOG_LEVEL_FILE )) ;;
		F) 	CXR_USER_TEMP_FORCE=true ;;
		w) 	CXR_USER_TEMP_WAIT_4_INPUT=true ;;
		m) 	CXR_USER_TEMP_ALLOW_MULTIPLE=true ;;
		c) 	CXR_HOLLOW=true; CXR_USER_TEMP_CLEANUP=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		t) 	CXR_USER_TEMP_ERROR_THRESHOLD=${OPTARG:-} ;;
		s) 	CXR_HOLLOW=true; CXR_USER_TEMP_STOP_RUN=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		D) 	CXR_USER_TEMP_SINGLE_DAYS=${OPTARG:-} ;;
		n)	CXR_USER_TEMP_REMOVE_DECOMPRESSED_FILES=false ;;
		P)	CXR_USER_TEMP_PARALLEL_PROCESSING=true ; CXR_USER_TEMP_MAX_PARALLEL_PROCS=${OPTARG:-} ;;
	
		# Installer: We need to manipulate the CXR_RUN variable for now
		I) 	CXR_RUN=${CXR_INSTALLER}; CXR_HOLLOW=true; CXR_USER_TEMP_INSTALL=true; CXR_DO_FILE_LOGGING=false ;;
		
		# Testing
		T) 	CXR_HOLLOW=true; CXR_LOG_LEVEL_SCREEN=${CXR_LOG_LEVEL_INF} ; CXR_USER_TEMP_RUN_TESTS=true; CXR_USER_TEMP_REPORT_MD5=false ;;
	
		# Creation or re-creation of configuration
		C) 	CXR_HOLLOW=true; CXR_USER_TEMP_CREATE_NEW_RUN=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		R) 	CXR_HOLLOW=true; CXR_USER_TEMP_REPEAT_RUN=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		
		r) 	CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_RUN_LIST="${OPTARG:-}" ;;
		p) 	CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_CLI_RUN_PRE_ONCE=true ;;
		i) 	CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_CLI_RUN_PRE_DAILY=true ;;
		o) 	CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_CLI_RUN_POST_DAILY=true ;;
		f) 	CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_CLI_RUN_POST_ONCE=true ;;
		x) 	CXR_USER_TEMP_RUN_LIMITED_PROCESSING=true; CXR_USER_TEMP_CLI_RUN_MODEL=true ;;
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
# Set version dependent variables ##############################################
################################################################################

main.setVersionDepVariables "${CXR_RUN}" "${CXR_MODEL}" "${CXR_MODEL_VERSION}" "${CXR_RUN_DIR}"

################################################################################
# Include all external functions. ##############################################
################################################################################

source $CXR_RUN_DIR/inc/load_common_modules.inc

################################################################################
# Read Config ##################################################################
################################################################################

# The run determines the files to use
main.readConfig "${CXR_RUN}" "${CXR_MODEL}" "${CXR_MODEL_VERSION}" "${CXR_RUN_DIR}"


# count simulation days
CXR_NUMBER_OF_SIM_DAYS=$(common.date.DaysBetween "${CXR_START_DATE}" "${CXR_STOP_DATE}")

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
if [[ "$CXR_LOG_LEVEL_SCREEN" -eq "$(( $CXR_LOG_LEVEL_VRB * 2 ))" ]]
then
	echo "You increased the screen log-level above verbose, we show additional bash inforamation."
	set -x
elif [[ "$CXR_LOG_LEVEL_SCREEN" -gt "$(( $CXR_LOG_LEVEL_VRB * 2 ))" ]]
then
	echo "You increased the screen log-level to the maximum, we show vorbose bash information."
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
	main.dieGracefully "You have chosen contradicting options. Refer to ${CXR_CALL} -h"
fi

################################################################################

main.log -v -B "Checking if selected options are valid..." 

if [[ ! ${CXR_MAX_PARALLEL_PROCS} =~ $CXR_PATTERN_NUMERIC ]]
then
	main.dieGracefully "The argument of -P must be numeric!"
fi

if [[ ! ${CXR_ERROR_THRESHOLD} =~ $CXR_PATTERN_NUMERIC ]]
then
	main.dieGracefully "The argument of -t must be numeric!"
fi

# Ok, now less than 2 processes is not parallel
if [[ "${CXR_MAX_PARALLEL_PROCS}" -lt 2 ]]
then
	main.log -w "You chose to use less than 2 processes, we will run sequentially".
	CXR_MAX_PARALLEL_PROCS=1
	CXR_PARALLEL_PROCESSING=false
fi

if [[ ${OMP_NUM_THREADS} -gt ${CXR_NUM_CORES} ]]
then
	main.log -e "It makes no sense to start more OMP threads than you have CPUS. Using ${CXR_NUM_CORES}"
	export OMP_NUM_THREADS=${CXR_NUM_CORES}
fi

# Test if we may run other processes while CAMx runs
if [[ ${CXR_MAX_PARALLEL_PROCS} -le $(( $CXR_NUM_CORES - $OMP_NUM_THREADS + 1 )) ]]
then

	if [[ $CXR_HOLLOW == false ]]
	then
		main.log -a "Since we have $CXR_NUM_CORES CPUS, OMP_NUM_THREADS is set to $OMP_NUM_THREADS and you want ${CXR_MAX_PARALLEL_PROCS} CAMxRunner processes, we allow other processes besides the model to be run"
	fi
	
	CXR_ALLOW_MODEL_CONCURRENCY=true
else
	CXR_ALLOW_MODEL_CONCURRENCY=false
fi

main.log -v -B "Selected options are valid." 


################################################################################
# Check name of script
################################################################################

# Fewer checks if we are hollow
if [[ "${CXR_HOLLOW}" == false ]] 
then

	if [[ ${CXR_RUN} == ${CXR_RUNNER_NAME}  ]]
	then
		main.dieGracefully "You are not using the system properly - use \n \t ${CXR_CALL} -C to create a new run and then call the run instead!"
	else
		
		# Check if the name of the script has changed
		link_target="$(common.fs.getLinkTarget ${CXR_RUN_DIR}/${CXR_RUN})"
		
		if [[ "$(basename $link_target)" != "${CXR_RUNNER_NAME}" ]]
		then
			main.dieGracefully "Probably the ${CXR_RUNNER_NAME} was renamed. Update the variable CXR_RUNNER_NAME in ${CXR_BASECONFIG}"
		fi
	
		# Make sure Run name is ok (Model.vVersion...)
		# check_functions.sh
		common.check.RunName ${CXR_RUN} || main.dieGracefully "Sorry, but a Run name (the link you create) must start with the string MODEL.vX.YZ where MODEL is one of $CXR_SUPPORTED_MODELS and X.YZ is the CAMx Version number."

	fi # Check of run name
	
	################################################################################

	# Extend stacksize
	ulimit -s unlimited

fi # Are we hollow?

################################################################################
# Is the configuration OK?
################################################################################

main.log -v -B "Checking CAMxRunner for consistency..." 

# Increase global indent level
main.increaseLogIndent

# check_functions
common.check.BashVersion

# check_functions.sh
common.check.runner

# Decrease global indent level
main.decreaseLogIndent

main.log -v -B "CAMxRunner is consistent as far as I can tell." 

################################################################################
# Setting up chemparam file (dependent on CAMx executable and mechanisms)
################################################################################

# Is the chemparam file already set (in the config?)
if [[ "${CXR_CHEMPARAM_INPUT_FILE:-}"  ]]
then
	#String is non-empty, check if it is sensible
	if [[ ! -f ${CXR_CHEMPARAM_INPUT_FILE:-}  ]]
	then
		main.log -w "You set the parameter CXR_CHEMPARAM_INPUT_FILE in your configuration, however, the file $CXR_CHEMPARAM_INPUT_FILE cannot be found. I try to find the correct setting."
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

	if [[ "${CXR_CLEANUP}" == true ]]
	then
		# Delete info in the state DB
		common.state.cleanup
	elif [[ "${CXR_CREATE_NEW_RUN}" == true ]]
	then
		#Initialize state DB
		common.state.init
		
		# Create a new run
		common.runner.createNewRun
	elif [[ "${CXR_REPEAT_RUN:-}" == true ]]
	then
		#Initialize state DB
		common.state.init
		
		# Re-create existing run
		common.runner.recreateRun "${CXR_RUN}"
	elif [[ "${CXR_STOP_RUN}" == true ]]
	then
		#Delete .CONTINUE files of all instances
		if [[ "$(common.user.getOK "You chose the option -s (stop run). Do you really want to stop the run ${CXR_RUN}?" )" == true  ]]
		then
			common.state.deleteContinueFiles
		fi
	elif [[ "${CXR_LIST_INFO}" == true ]]
	then
		# show summary
		common.runner.printSummary
	elif [[ "${CXR_INSTALL}" == true ]]
	then
		#Initialize state DB
		common.state.init
		
		# Run the installation
		common.install.init
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

main.log -H "$progname - running stage\nLoading external modules from ${CXR_COMMON_INPUT_DIR}..." 


################################################################################
# Show a summary of the configuration
################################################################################

common.runner.printSummary

################################################################################
# Check resource requirements if we run a full simulation
################################################################################

load=$(common.performance.getReaLoadPercent)
if [[ $load -gt $CXR_LOAD_EXIT_THRESHOLD ]]
then
	main.dieGracefully "The load of the system exceeds CXR_LOAD_EXIT_THRESHOLD (${CXR_LOAD_EXIT_THRESHOLD}%). We stop!"
elif [[ $load -gt $CXR_LOAD_WARN_THRESHOLD ]]
then
	main.log -w "The load is higher than CXR_LOAD_WARN_THRESHOLD (${CXR_LOAD_WARN_THRESHOLD}%). The run may be slow on this machine!"
fi

mb_needed=$(common.check.PredictModelOutputMb)
if [[ "${CXR_RUN_LIMITED_PROCESSING}" == false ]]
then
	# Full simulation, do the space check if user has not disabled it
	if [[ "${CXR_CHECK_MODEL_SPACE_REQUIRED}" == true  ]]
	then
		common.check.MbNeeded "${CXR_OUTPUT_DIR}" "${mb_needed}"
		
		# We assume that we need 5% of this space in CXR_TMP_DIR if we do not decompress in place
		if [[ "${CXR_DECOMPRESS_IN_PLACE}" == false  ]]
		then
			common.check.MbNeeded "${CXR_TMP_DIR}" $(common.math.FloatOperation "${CXR_TMP_SPACE_FACTOR:-0.05} * ${mb_needed}" 0)
		fi
	else
		main.log -w "CXR_CHECK_MODEL_SPACE_REQUIRED is false, I will not check if sufficient diskspace is available"
	fi
else
	# Limited Processing, we need to fill DISABLED and ENABLED cleverly.
	# When the user selects a certain type of modules, they should be executed as 
	# configured
	# In the case of CXR_ALLOW_MULTIPLE, we create a WHERE-clause that is used later by setNextTask

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
	
	# Since by default, we run everything, we need to
	# use these special CLI variables to tell if the user
	# actively chose this particular part.
	# If not, we disable it in the case of limited processing
	if [[ ${CXR_CLI_RUN_PRE_ONCE:-false} == true ]]
	then
		CXR_RUN_PRE_ONCE=true
		CXR_DISABLED_ONCE_PREPROC="$d_once_pre"
		CXR_ENABLED_ONCE_PREPROC="$e_once_pre"
	else
		CXR_RUN_PRE_ONCE=false
	fi
	
	if [[ ${CXR_CLI_RUN_PRE_DAILY:-false} == true ]]
	then
		CXR_RUN_PRE_DAILY=true
		CXR_DISABLED_DAILY_PREPROC="$d_d_pre"
		CXR_ENABLED_DAILY_PREPROC="$e_d_pre"
	else
		CXR_RUN_PRE_DAILY=false
	fi		
	
	if [[ ${CXR_CLI_RUN_MODEL:-false} == true ]]
	then
		CXR_RUN_MODEL=true
		CXR_DISABLED_MODEL="$d_model"
		CXR_ENABLED_MODEL="$e_model"
	else
		CXR_RUN_MODEL=false
	fi

	if [[ ${CXR_CLI_RUN_POST_DAILY:-false} == true ]]
	then
		CXR_RUN_POST_DAILY=true
		CXR_DISABLED_DAILY_POSTPROC="$d_d_post"
		CXR_ENABLED_DAILY_POSTPROC="$e_d_post"
	else
		CXR_RUN_POST_DAILY=false
	fi
	
	if [[ ${CXR_CLI_RUN_POST_ONCE:-false} == true ]]
	then
		CXR_RUN_POST_ONCE=true
		CXR_DISABLED_ONCE_POSTPROC="$d_once_post"
		CXR_ENABLED_ONCE_POSTPROC="$e_once_post"
	else
		CXR_RUN_POST_ONCE=false
	fi
	
	# Now, we must deal with -r.
	if [[ "$(common.string.trim "$CXR_RUN_LIST")" ]]
	then
		# There are arguments
		# we reset all explicitly enabled modules
		# (-r means "run ONLY these modules")
		CXR_ENABLED_ONCE_PREPROC=""
		CXR_ENABLED_DAILY_PREPROC=""
		CXR_ENABLED_MODEL=""
		CXR_ENABLED_DAILY_POSTPROC=""
		CXR_ENABLED_ONCE_POSTPROC=""
		
		# Decode arguments
		for module in $CXR_RUN_LIST
		do
			# It might be a type!
			if [[ "$(common.module.isType $module)" == true ]]
			then
				main.log -a "We will run all modules of type $module"
				# Yep, its a type
				# Exchange roles
				module_type=$module
				# The modules must be derived from the tpe
				module="$(common.module.resolveType $module_type)"
			else 
				# no, normal module name
				# Determine type. 
				# Due to CATCH-22 we cannot use <common.module.getType> here
				module_type="$(common.module.getTypeSlow "$module")"
			fi  # is it a type?
			
			if [[ "$CXR_ALLOW_MULTIPLE" == false ]]
			then
				# Normal case with one runner
				case $module_type in
				
					${CXR_TYPE_PREPROCESS_ONCE} ) 
						CXR_ENABLED_ONCE_PREPROC="$CXR_ENABLED_ONCE_PREPROC $module"
						CXR_RUN_PRE_ONCE=true;;
						
					${CXR_TYPE_PREPROCESS_DAILY} ) 
						CXR_ENABLED_DAILY_PREPROC="$CXR_ENABLED_DAILY_PREPROC $module"
						CXR_RUN_PRE_DAILY=true;;
						
					${CXR_TYPE_MODEL} ) 
						CXR_ENABLED_MODEL="$CXR_ENABLED_MODEL $module"
						CXR_RUN_MODEL=true;;
						
					${CXR_TYPE_POSTPROCESS_DAILY} ) 
						CXR_ENABLED_DAILY_POSTPROC="$CXR_ENABLED_DAILY_POSTPROC $module"
						CXR_RUN_POST_DAILY=true;;
						
					${CXR_TYPE_POSTPROCESS_ONCE} ) 
						CXR_ENABLED_ONCE_POSTPROC="$CXR_ENABLED_ONCE_POSTPROC $module"
						CXR_RUN_POST_ONCE=true;;
						
					* ) main.dieGracefully "Module type $module_type not supported to be used with -r" ;;
				esac
			
			else
			
				# Special case where we run multiple runners
				# We need to go over all modules set now
				
				for entry in $module
				do
					CXR_TASK_WHERE="'$entry',${CXR_TASK_WHERE:-}"
				done
			
			fi # CXR_ALLOW_MULTIPLE?

		done # entries in -r
		
		if [[ "$CXR_TASK_WHERE" ]]
		then
			# We fix the task where clause
			# remove last comma
			CXR_TASK_WHERE="${CXR_TASK_WHERE%,}]"
			CXR_TASK_WHERE=" module in (${CXR_TASK_WHERE}) "
		fi 
		
	else
		main.log -v "The option -r needs at least one module name or type as argument!"
	fi
fi # Limited Processing?

################################################################################
# Print out the variables and their settings
################################################################################

INFO="\nThis CAMxRunner has process id ${CXR_PID} and is running on host $(uname -n)\nRun ${CXR_RUN}, we use the ${CXR_MODEL_EXEC} executable. \n It is now $(date)\n\n The script was called as \n \t \$ ${0} ${CXR_ARGUMENTS} \n\n" 

main.sendMessage "Run $CXR_RUN starts on $CXR_MACHINE" "$INFO"
main.log "$INFO"

if [[ "${CXR_HOLLOW}" == false || "${CXR_DRY}" == true ]]
then
	main.log "Output will be written to ${CXR_OUTPUT_DIR}\nWe run ${CXR_MODEL} ${CXR_MODEL_VERSION} using the chemparam File ${CXR_CHEMPARAM_INPUT_FILE}. We process ${CXR_TUV_NO_OF_REACTIONS} photolytic reactions\n" 
fi

if [[ ${CXR_ERROR_THRESHOLD} != ${CXR_NO_ERROR_THRESHOLD} ]]
then
	main.log "In this run, at most ${CXR_ERROR_THRESHOLD} errors will be tolerated before stopping.\n"
else
	main.log "We ignore the number of errors occurring and keep going because the option -t${CXR_NO_ERROR_THRESHOLD} was used\n"
fi

if [[ ${CXR_SKIP_EXISTING} == true ]]
then
	main.log -w "Existing output files will be skipped."
fi

if [[ ${CXR_FORCE} == true ]]
then
	main.log -w "Existing output files will be deleted."
fi

################################################################################
# Detect other instances #######################################################
################################################################################

# TODO: Rethink this strategy (this check should never be false!)
if [[ "$CXR_HOLLOW" == false ]]
then

	main.log -v -B "Checking if another instance is running on this run..." 
	
	common.state.detectInstances
	
	main.log -v -B "No other instances found." 


	################################################################################
	# Create a Continue File. If it is gone, the worker thread(s) will stop
	################################################################################

	common.state.init || main.dieGracefully "Could not initialize state DB"
	
	# Here we really start things. Note that the execution of tasks is no longer sequential
	# if not needed 
	
	if [[ "$CXR_PARALLEL_PROCESSING" == false ]]
	then
		# Sequential is "Parallel with one process"
		CXR_MAX_PARALLEL_PROCS=1
	fi
	
	# Create a plan
	common.task.init
	
	# Do we need to work?
	if [[ $(common.task.countOpenTasks) -gt 0 ]]
	then
		# Creates CXR_MAX_PARALLEL_PROCS common.task.Worker processes
		# The workers then carry out the tasks 
		common.task.spawnWorkers $CXR_MAX_PARALLEL_PROCS
	
		################################################################################
		# Make sure that all subprocesses are done!
		################################################################################
		
		# control the workers
		common.task.controller
		
		# If we arrive here, we should be done.
		# We can add a good check later.
		
		# We need a way to find out if all workers returned happily to
		# manipulate CXR_STATUS if needed
		
		if [[ "$(common.task.countOpenTasks)" -ne 0 || -e "$CXR_GLOBAL_ABNORMAL_TERMINATION_FILE" ]]
		then
			main.log "The run $CXR_RUN stopped, but there are still $(common.task.countOpenTasks) open tasks!"
			# We are not happy
			CXR_STATUS=$CXR_STATUS_FAILURE
		else
			# We are happy
			CXR_STATUS=$CXR_STATUS_SUCCESS
			
			# Do compression if needed
			common.fs.CompressOutput
		fi # still open tasks?
		
	else
		main.log -a "All tasks where already processed. If you want to repeat processing, consider the -c and -F options"
	fi # are there tasks at all?
fi

elapsed_seconds=$(( $(date "+%s") - $CXR_START_EPOCH ))
main.log -a "$CXR_RUN finished at $(date), we ran $elapsed_seconds s - $(common.date.humanSeconds $elapsed_seconds)"

# Echo the "Finish message"
main.log -a "$(common.runner.evaluateRule "$CXR_FINISH_MESSAGE_RULE" true CXR_FINISH_MESSAGE_RULE)"

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
		
	



