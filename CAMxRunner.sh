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

	  -d    causes a dry run (always uses a single worker). Also displays expected input and output files of each step.
	  
	  -l    Log even if in dryrun
	  
	  -f    fast mode - disables most checks. Recommended only for quick-and-dirty jobs.

	  -F    overwrites existing output files (force)

	  -w    wait for missing input files

	  -v    verbose screen: talkative script (if given more than once you get even more information)

	  -V    verbose logfile: talkative script (if given more than once you get even more information)

	  -c    cleanup: removes state information

	  -t<n> set the threshold for allowed errors (Default ${CXR_ERROR_THRESHOLD}).
	        a threshold of ${CXR_NO_ERROR_THRESHOLD} ignores errors. 

	  -s    stop this run gracefully (stop all runners executing this run)
	  
	  -DYYYY-MM-DD execute a specific simulation day given in the form YYYY-MM-DD
	               Attention: if you want to run One-Time pre- or postprocessing,
	               you must include the first and the last simulation day, respectively.
	  
	  -n    No removal of tempfiles. Useful for partial runs on compressed input.

	  -Pn   activates parallel execution of pre/postprocessing with 
	        max. n concurrent procs. n must be given!

	  -C    Create a new run, you are guided through the process.
	  
	  -R    Creates a configuration to repeat this run. User may select another run later.

	  The list of modules to be run can be modified (the order is unimportant):
	
	  -r"list of modules or types"
	        When using -r together with more than one instance, 
	        this instance will only work on the modules given.
          This can be used to assign CPU intensive tasks like the model to strong machines.
        
	  -e    Prepares an external model run on an MPI machine. This just generates CAMx.in 
	        files and a script to run the job on the HPC system. 
	        Uses external.conf as second to last conf file, allowing you to inject any 
	        directories and/or rules (like a new base.conf)
	  
	  -L    List all available modules. (this command is sensitive to the run name)
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
while getopts ":dSlvVFfwct:sD:nP:ITr:CReLh" opt
do
	case "${opt}" in
		d) 	CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_PARALLEL_PROCESSING=false ; CXR_USER_TEMP_LOG_EXT="-dry" ;;
		S) 	CXR_HOLLOW=true; CXR_USER_TEMP_LIST_INFO=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		l) 	CXR_USER_TEMP_FORCE_LOG=true;;
		v) 	CXR_LOG_LEVEL_SCREEN=$(( 2 * $CXR_LOG_LEVEL_SCREEN )) ;;
		V) 	CXR_LOG_LEVEL_FILE=$(( 2 * $CXR_LOG_LEVEL_FILE )) ;;
		f) 	CXR_USER_TEMP_FAST=true ;;
		F) 	CXR_USER_TEMP_FORCE=true ;;
		w) 	CXR_USER_TEMP_WAIT_4_INPUT=true ;;
		c) 	CXR_HOLLOW=true; CXR_USER_TEMP_CLEANUP=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		t) 	CXR_USER_TEMP_ERROR_THRESHOLD=${OPTARG:-} ;;
		s) 	CXR_HOLLOW=true; CXR_USER_TEMP_STOP_RUN=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		D) 	CXR_SINGLE_DAYS="${OPTARG:-}" ;;
		n)	CXR_USER_TEMP_REMOVE_DECOMPRESSED_FILES=false ;;
		P)	CXR_USER_TEMP_PARALLEL_PROCESSING=true ; CXR_USER_TEMP_MAX_PARALLEL_PROCS=${OPTARG:-} ;;
	
		# Installer
		I) 	CXR_RUN=${CXR_INSTALLER}; CXR_HOLLOW=true; CXR_USER_TEMP_INSTALL=true; CXR_DO_FILE_LOGGING=false ;;
		
		# Testing
		T) 	CXR_HOLLOW=true; CXR_LOG_LEVEL_SCREEN=${CXR_LOG_LEVEL_INF} ; CXR_USER_TEMP_RUN_TESTS=true; CXR_USER_TEMP_REPORT_MD5=false ;;
	
		# Creation or re-creation of configuration
		C) 	CXR_HOLLOW=true; CXR_USER_TEMP_CREATE_NEW_RUN=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		R) 	CXR_HOLLOW=true; CXR_USER_TEMP_REPEAT_RUN=true; CXR_USER_TEMP_DO_FILE_LOGGING=false ;;
		
		r) 	CXR_RUN_LIST="${OPTARG:-}" ;;
		e) 	CXR_HOLLOW=true; CXR_EXTERNAL=true ;;
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

# Init Db subsystem
common.db.init

##################
# Init all Hashes
##################
common.hash.init $CXR_LEVEL_UNIVERSAL
common.hash.init $CXR_LEVEL_GLOBAL
common.hash.init $CXR_LEVEL_INSTANCE

# The run determines the files to use
main.readConfig "${CXR_RUN}" "${CXR_MODEL}" "${CXR_MODEL_VERSION}" "${CXR_RUN_DIR}"

# count simulation days
CXR_NUMBER_OF_SIMULATION_DAYS=$(common.date.DaysBetween "${CXR_START_DATE}" "${CXR_STOP_DATE}")

################################################################################
# Determine name of model exec                                      ############
################################################################################

# Set the model exec if not user-supplied
if [[ -z "${CXR_MODEL_EXEC:-}" ]]
then
	CXR_MODEL_EXEC="$(get_model_exec)"
	main.log -a "CXR_MODEL_EXEC was not set by the user,using $CXR_MODEL_EXEC"
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

if [[ "${CXR_FAST}" == true ]]
then
	main.log -a -B "With the -f(ast) option, you disabled most checks. Be careful to test the output!"

	if [[ "${CXR_DRY}" == true ]]
	then
		main.dieGracefully "It makes no sense to use -f with -d"
	fi
fi

#### Check for contradictions and take action (incomplete!)

# either -F or CXR_SKIP_EXISTING
if [[  "${CXR_FORCE}" == true && "${CXR_SKIP_EXISTING}" == true ]]
then
	# Force wins (CXR_SKIP_EXISTING is default)
	CXR_SKIP_EXISTING=false
fi

# Hollow and non-hollow options should not be mixed
if [[ "${CXR_HOLLOW}" == true && "${CXR_SINGLE_DAYS}" && "${CXR_RUN_LIST}" ]]
then
	main.dieGracefully "You have chosen contradicting options. Refer to ${CXR_CALL} -h"
fi

# Remove spaces from the runlist/single days
CXR_RUN_LIST="$(common.string.trim "$CXR_RUN_LIST")"
CXR_SINGLE_DAYS="$(common.string.trim "$CXR_SINGLE_DAYS")"

# Are we the first instance?
# (the first must collect all kind of data, for example compute the execution plan)
if [[ "$(common.state.countInstances)" -lt 2 ]]
then
	CXR_FIRST_INSTANCE=true
else
	CXR_FIRST_INSTANCE=false
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

if [[ ${OMP_NUM_THREADS} -gt ${CXR_NUMBER_OF_CORES} ]]
then
	main.log -e "It makes no sense to start more OMP threads than you have CPUS. Using ${CXR_NUMBER_OF_CORES}"
	export OMP_NUM_THREADS=${CXR_NUMBER_OF_CORES}
fi

# Test if we may run other processes while CAMx runs
if [[ ${CXR_MAX_PARALLEL_PROCS} -le $(( $CXR_NUMBER_OF_CORES - $OMP_NUM_THREADS + 1 )) ]]
then

	if [[ $CXR_HOLLOW == false && ${CXR_MAX_PARALLEL_PROCS} -gt 1 ]]
	then
		main.log -a "Since we have $CXR_NUMBER_OF_CORES CPUS, OMP_NUM_THREADS is set to $OMP_NUM_THREADS and you want ${CXR_MAX_PARALLEL_PROCS} CAMxRunner processes, we allow other processes besides the model to be run"
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
	if [[ $(common.check.RunName ${CXR_RUN}) == false ]]
		then
			main.dieGracefully "Sorry, but a Run name (the link you create) must start with the string MODEL.vX.YZ where MODEL is one of $CXR_SUPPORTED_MODELS and X.YZ is the CAMx Version number."
		fi
		
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
if [[ "${CXR_CHEMPARAM_INPUT_FILE:-}" ]]
then
	#String is non-empty, check if it is sensible
	if [[ ! -s ${CXR_CHEMPARAM_INPUT_FILE:-} && ${CXR_HOLLOW} == false ]]
	then
		main.log -w "You set the parameter CXR_CHEMPARAM_INPUT_FILE in your configuration, however, the file $CXR_CHEMPARAM_INPUT_FILE cannot be found or is empty. I try to find the correct setting."
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
			common.state.deleteAllContinueFiles
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
	elif [[ "${CXR_EXTERNAL}" == true ]]
	then
		# Prepare external run
		common.external.init
	elif [[ "${CXR_RUN_TESTS}" == true ]]
	then
		# Run the tests
		common.test.all
	fi
	
	# Delete entries in instance_tasks
	common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "DELETE FROM instance_tasks WHERE instance='$CXR_INSTANCE';"
	
	# Do cleanup
	main.doCleanup
	
	# We are happy
	CXR_STATUS=${CXR_STATUS_SUCCESS}
	exit
fi

### No hollow function gets further here

main.log -H "$progname - running stage\nLoading modules from ${CXR_COMMON_INPUT_DIR}..." 


################################################################################
# Show a summary of the configuration and wait 10 seconds
################################################################################

common.runner.printSummary
common.user.wait 10

# Check if the selected binary supports our settings
common.check.ModelLimits
common.check.ExecLimits

################################################################################
# Check load
################################################################################

load=$(common.performance.getReaLoadPercent)
if [[ $load -gt $CXR_LOAD_EXIT_THRESHOLD ]]
then
	main.dieGracefully "The load of the system exceeds CXR_LOAD_EXIT_THRESHOLD (${CXR_LOAD_EXIT_THRESHOLD}%). We stop!"
elif [[ $load -gt $CXR_LOAD_WARN_THRESHOLD ]]
then
	main.log -w "The load is higher than CXR_LOAD_WARN_THRESHOLD (${CXR_LOAD_WARN_THRESHOLD}%). The run may be slow on this machine!"
fi

################################################################################
# Print out the variables and their settings
################################################################################

INFO="\nThis CAMxRunner has process id ${CXR_PID} and is running on host $(uname -n)\nRun ${CXR_RUN}, we use the ${CXR_MODEL_EXEC} executable. \n It is now $(date)\n\n The script was called as \n \t \$ ${0} ${CXR_ARGUMENTS} \n\n" 

main.sendMessage "Run $CXR_RUN starts on $CXR_MACHINE" "$INFO"
main.log "$INFO"

if [[ ${CXR_ERROR_THRESHOLD} != ${CXR_NO_ERROR_THRESHOLD} ]]
then
	main.log "In this run, at most ${CXR_ERROR_THRESHOLD} errors will be tolerated before stopping.\n"
else
	main.log "We ignore the number of errors occurring and keep going because the option -t${CXR_NO_ERROR_THRESHOLD} was used\n"
fi

if [[ ${CXR_SKIP_EXISTING} == true ]]
then
	main.log -w "Existing output files will be skipped."
elif [[ ${CXR_FORCE} == true ]]
then
	main.log -w "Existing output files will be deleted."
fi

################################################################################
# Detect other instances #######################################################
################################################################################

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
		
	



