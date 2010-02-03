#
# This file contains defaults for CAMxRunner. Some values are readonly,
# to avoid them being overwritten later.
# 
# Leave the next line intact, it is used for change detection
# Version: $Id$ 
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch),
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################

# used to tell the user how to start again
CXR_CALL="\$ ${0}"

################################################################################
# Bash settings     ############################################################
################################################################################

# We want to live on the safe side of town:
# Exit if any command returns non-zero 
# (means that $? cannot be checked anymore. Instead use 
# command || return $CXR_RET_ERROR)
set -o errexit

# Never use an undefined variable.
# This is a very strict requirement, but its also safe...
set -o nounset

# Bash sets HOSTTYPE too short (the only thing where tcsh is better)
# we want something like x86_64-linux
HOSTTYPE=$(uname -i)-$(echo "$(uname -s)" | tr "[:upper:]" "[:lower:]")

################################################################################
# Default Behaviour     ########################################################
################################################################################

# Uncomment this for REALLY Verbose messages (directly from bash)
#set -x

# Set this to true to disable locking.
# Dangerous, especially in parallel mode; should only be used when debugging
CXR_NO_LOCKING=false

# Just for main_usage
progname=CAMxRunner.sh

# Define Statusses
CXR_STATUS_SUCCESS=SUCCESS
readonly CXR_STATUS_SUCCESS

CXR_STATUS_FAILURE=FAILURE
readonly CXR_STATUS_FAILURE

# If we exit unexpectedly, we will indicate failure
CXR_STATUS=$CXR_STATUS_FAILURE

# Special key
CXR_SKIP_ALL=skip_all
readonly CXR_SKIP_ALL

# The indentation of the logging (improves readability)
CXR_LOG_INDENT=0

# If the variable CXR_ERROR_THRESHOLD is set to thes value,
# we do not check the number of errors during runtime
CXR_NO_ERROR_THRESHOLD=-1

# Threshold of errors that can happen before we stop. Set to -1 to ignore any errors,
# Set to 0 to stop at first error.
CXR_ERROR_THRESHOLD=5

# Number of digits calculate should use in divisions
CXR_NUM_DIGITS=4

# Max allowed indent
CXR_MAX_LOG_INDENT=8

# Sign to use to draw boxes (in log)
CXR_STAR='*'

# We define 3 box sizes
# 20
CXR_BOX_SMALL="********************"

# 40
CXR_BOX_LARGE="****************************************"
	
# 80
CXR_BOX_HUGE="********************************************************************************"


# These are needed for the user functions
CXR_SINGLE_LINE="-----------------------------------------------------------------------------"
CXR_DOUBLE_LINE="============================================================================="

# Max Parallel Procs
CXR_MAX_PARALLEL_PROCS=1

# If this variable is true, we are not really running (help etc.)
CXR_HOLLOW=false

# This ariable allows to use the state DB for the installer
CXR_INSTALLER=installer

# Internal type constants for module handlers
CXR_TYPE_COMMON=common
readonly CXR_TYPE_COMMON

CXR_TYPE_PREPROCESS_ONCE=preprocess_once
readonly CXR_TYPE_PREPROCESS_ONCE

CXR_TYPE_PREPROCESS_DAILY=preprocess_daily
readonly CXR_TYPE_PREPROCESS_DAILY

CXR_TYPE_POSTPROCESS_DAILY=postprocess_daily
readonly CXR_TYPE_POSTPROCESS_DAILY

CXR_TYPE_POSTPROCESS_ONCE=postprocess_once
readonly CXR_TYPE_POSTPROCESS_ONCE

CXR_TYPE_MODEL=model
readonly CXR_TYPE_MODEL

CXR_TYPE_INSTALLER=${CXR_INSTALLER}
readonly CXR_TYPE_INSTALLER

# Exit codes (implemented as bitmap, but not used as such yet)
# You could add those to signal more than one condition
# I am not sure if a value of more than 128 is supported, though...
# This is a little underused...
CXR_RET_OK=0
CXR_RET_ERROR=4
CXR_RET_ALREADY_RUN=8
CXR_RET_PARAM_ERROR=16
CXR_RET_CONTINUE_MISSING=32
# These are used to signal an issue in the modules
CXR_RET_ERR_PRECONDITIONS=64
CXR_RET_ERR_POSTCONDITIONS=128


# Definitions of the log levels
CXR_LOG_LEVEL_DBG=16
readonly CXR_LOG_LEVEL_DBG

CXR_LOG_LEVEL_VRB=8
readonly CXR_LOG_LEVEL_VRB

CXR_LOG_LEVEL_INF=4
readonly CXR_LOG_LEVEL_INF

CXR_LOG_LEVEL_WRN=2
readonly CXR_LOG_LEVEL_WRN

CXR_LOG_LEVEL_ERR=1
readonly CXR_LOG_LEVEL_ERR

CXR_LOG_LEVEL_ALW=0
readonly CXR_LOG_LEVEL_ALW

# Here we set the default filter for screen and file messages
CXR_LOG_LEVEL_SCREEN=${CXR_LOG_LEVEL_WRN}
CXR_LOG_LEVEL_FILE=${CXR_LOG_LEVEL_INF}

# This is used for twitter, SMS and Mail
# Which channels are used are controlled by
# CXR_SEND_SMS, CXR_SEND_TWITTER and CXR_SEND_EMAIL
CXR_LOG_LEVEL_EXTERNAL=${CXR_LOG_LEVEL_ERR}

# Note that you can configure the log-level 
# CXR_LOG_LEVEL_EXTERNAL
# to decide what to see there.
# As a minimum, you get a mesage when the run starts and when the run ends.

# If this Variable is true,Email will be sent
CXR_SEND_EMAIL=true

#If this Variable is true, SMS will be sent
CXR_SEND_SMS=false

# If this Variable is true, twitter wil be used
# Twitter allows us to publish our logs to a whole group of people
# To use it, create a ~/.twitterrc file (readable only to you)
# that sets the variables ACCOUNT and PASS
CXR_SEND_TWITTER=true

# Delimiter for ask and playfiles and other "DB-like" files
CXR_DELIMITER="|"
readonly CXR_DELIMITER

# A space used to delimit list elements
CXR_SPACE=" "
readonly CXR_SPACE

#Normally, the state DB is in use
CXR_ENABLE_STATE_DB=true

# Define the prefix for disabled scripts (no dot please)
CXR_DISABLED_EXT=disabled
readonly CXR_DISABLED_EXT


# Stages that the stage controller knows
CXR_STATE_CONTINUE=CONTINUE
readonly CXR_STATE_CONTINUE

CXR_STATE_START=START
readonly CXR_STATE_START

CXR_STATE_STOP=STOP
readonly CXR_STATE_STOP

CXR_STATE_ERROR=ERROR
readonly CXR_STATE_ERROR

# Change this in case the name of the runner script changes
CXR_RUNNER_NAME=CAMxRunner.sh
readonly CXR_RUNNER_NAME

# This is a special value that is used in many contexts
CXR_ALL=all
readonly CXR_ALL

# Used to tell a single step from all steps
CXR_RUN_ALL=${CXR_ALL}
readonly CXR_RUN_ALL


################################################################################
# Set inital values of features ################################################
################################################################################

# Variables:  Parallel Task control
#
# CXR_PIN_SERVER - Allows to strictly assign (pin) certain modules to certain hosts. Attention: if the server does not show up, these modules will never be done!


# The syntax is: module_name1=server1:module_name2=server2
CXR_PIN_SERVER=

# If this is true, no new assignments will be made
# Normally set programatically - dangerous because
# no progress is possible when this is false
CXR_BLOCK_ASSIGNMENTS=false

# If this is false, modules that depend on disabled modules
# will never be able to run. 
CXR_IGNORE_DISABLED_DEPENDENCIES=true

# If this is true, we ignore dependecies altogether
CXR_IGNORE_ANY_DEPENDENCIES=false

# There probably won't be more than 99'999 tasks per run
CXR_TASK_ID_DIGITS=5

# The number of seconds a waiting task sleeps before retrying
CXR_WAITING_SLEEP_SECONDS=60

# These are the indexes of the task descriptor structure
CXR_TASK_DESCR_I_PATH=0					# Relative Path to module,
CXR_TASK_DESCR_I_TYPE=1					# Module type
CXR_TASK_DESCR_I_OFFSET=2				# Day offset
CXR_TASK_DESCR_I_EXCLUSIVE=3			# Run exclusively (boolean flag)
CXR_TASK_DESCR_I_DEPENDENCIES=4	# Dependencies

# These are special dependencies
CXR_DEP_ALL_ONCE_PRE=all_once_preprocessors
CXR_DEP_ALL_DAILY_PRE=all_daily_preprocessors
CXR_DEP_ALL_DAILY_POST=all_daily_postprocessors
CXR_DEP_ALL_ONCE_POST=all_once_postprocessors
CXR_DEP_ALL_MODEL=all_model

## End Task control

# Names of modules already checked
CXR_ANNOUNCED_MODULES=" "

# If this is false, the temporary files will not be deleted
# Debug only!
CXR_REMOVE_TEMP_FILES=true

# When set to true, the script logs even in a dry run
CXR_FORCE_LOG=false

# When set to true, the script logs
CXR_DO_FILE_LOGGING=true

# When set to true, CAMx is not run, only Input data is checked
CXR_DRY=false

# When set to true, CAMx is not run, only state information is cleared
CXR_CLEANUP=false

# When set to true, CAMx is not run, only a rew run is created
CXR_CREATE_NEW_RUN=false

# When set to true,CAMx is not run, only the installation is run
CXR_INSTALL=false

# When set to true, we only test the CAMxRunner
CXR_RUN_TESTS=false

#If non-empty, we will run this day (YYYY-MM-DD)
CXR_ONE_DAY=

# When set to true, CAMx is not run, only other runners on the same rn are stopped
CXR_STOP_RUN=false

# When set to true, existing Output files are overwritten (see also CXR_SKIP_EXISTING)
CXR_FORCE=false

# When set to false, existing Output files are not skipped (we stop if they exist, unless -F is given)
CXR_SKIP_EXISTING=true

# When set to false, disables check for disk space for model runs
CXR_CHECK_MODEL_SPACE_REQUIRED=true

# When set to false, we no longer check the lengths of the paths of filenames
# This check is important for programs that are compiled with a certain pathlenght
# Most often, these values are mugh lower than the maximum allowed e. g. ( grep MAX_PATH /usr/include/linux/limits.h)
CXR_CHECK_MAX_PATH=true

# Currently we assume this limit
CXR_MAX_PATH=240

# When set to true, muliple instances of CAMx Runner 
# on the same run are allowed (experts only)
CXR_ALLOW_MULTIPLE=false

# If true, run CAMx , otherwise don't
CXR_RUN_MODEL=true

#If true, compress the output files upon completion
# The compressor CXR_COMPRESSOR_EXEC used is selected later
CXR_COMPRESS_OUTPUT=false

# This is a pattern that can be used to limit compression either to 
# specific files or specific modules that created the files
# is tested using "expr match"
# If it is empty, it has no effect
CXR_COMPRESS_OUTPUT_PATTERN=

# If true, only specfied parts if pre- ar postprocessing are run
CXR_RUN_LIMITED_PROCESSING=false

# If one of these is false, the sub-system will not be run
# Must be false here, gets changed after option processing is done
CXR_RUN_PRE_DAILY=false
CXR_RUN_PRE_ONCE=false
CXR_RUN_POST_DAILY=false
CXR_RUN_POST_ONCE=false

# If set to a number, just run the respective input or output file
CXR_RUN_PRE_DAILY_STEP=${CXR_ALL}
CXR_RUN_POST_DAILY_STEP=${CXR_ALL}

# The same for one-time processing
CXR_RUN_PRE_ONCE_STEP=${CXR_ALL}
CXR_RUN_POST_ONCE_STEP=${CXR_ALL}

# This Check variable makes sure certain things are only checked once and not for each day
CXR_CHECKED_ONCE=false

# Contains an explanatory extension for the logfile name
# is set according to the options set
CXR_LOG_EXT=""

### Settings for testing

# If this is true, modules will not initialize the testing 
# environment
CXR_TESTING_FROM_HARNESS=false

################################################################################
################################################################################
# Now follows a default set of options that might be copied to a configuration file.
# The defaults given here should make sense most of the time.
# Note that this set is not complete, all model specific parameters like
# geometry etc. are defined in base.conf
################################################################################	
################################################################################

################################################################################
# Disabled Modules (space-separated module names, skip_all disables whole class)
################################################################################

CXR_DISABLED_DAILY_PREPROC=""

CXR_DISABLED_ONCE_PREPROC=""

CXR_DISABLED_DAILY_POSTPROC=""

CXR_DISABLED_ONCE_POSTPROC=""

CXR_DISABLED_MODEL=""

CXR_DISABLED_INSTALLER=""

################################################################################
# Enabled Modules (space-separated module names, stronger counterpart of the former variables)
################################################################################

CXR_ENABLED_DAILY_PREPROC=""

CXR_ENABLED_ONCE_PREPROC=""

CXR_ENABLED_DAILY_POSTPROC=""

CXR_ENABLED_ONCE_POSTPROC=""

CXR_ENABLED_MODEL=""

CXR_ENABLED_INSTALLER=""

################################################################################
# Directories 
# Attention: Put no variables in directory names that are not yet set here!
# E. g. any variable containing dates (like simulation year) will fail to expand!
################################################################################

# This is the directory where tempfiles go.
# We do not recommed to use /tmp because the space requirements
# might be significant (e. g. when decompressing files, we need a few GB per file)
CXR_TMP_DIR=~/tmp

# The directory where the executables lie
CXR_BIN_DIR=${CXR_RUN_DIR}/bin
readonly CXR_BIN_DIR

# The directory where the libraries lie
CXR_LIB_DIR=${CXR_RUN_DIR}/lib
readonly CXR_LIB_DIR

#Base path to find the CAMx executable
CXR_CAMX_DIR=${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}
readonly CXR_CAMX_DIR

# Here is the config
CXR_CONF_DIR=${CXR_RUN_DIR}/conf
readonly CXR_CONF_DIR

# Here are the templates
CXR_TEMPLATES_DIR=${CXR_RUN_DIR}/templates
readonly CXR_TEMPLATES_DIR

# Here are the modules
CXR_MODULES_DIR=${CXR_RUN_DIR}/modules
readonly CXR_MODULES_DIR

# External functions are loaded from here
CXR_COMMON_INPUT_DIR=${CXR_MODULES_DIR}/common
readonly CXR_COMMON_INPUT_DIR

# Common installers are here
CXR_INSTALLER_INPUT_DIR=${CXR_MODULES_DIR}/${CXR_INSTALLER}
readonly CXR_INSTALLER_INPUT_DIR

#######################################
# Model/Version specific Directories 
#######################################

# These are Model specific common modules, CAMx is the default model
CXR_COMMON_MODEL_INPUT_DIR=${CXR_COMMON_INPUT_DIR}/${CXR_MODEL}
readonly CXR_COMMON_MODEL_INPUT_DIR

# And these Version specific common modules
CXR_COMMON_VERSION_INPUT_DIR=${CXR_COMMON_INPUT_DIR}/${CXR_MODEL_VERSION}
readonly CXR_COMMON_VERSION_INPUT_DIR

#######################################
# Directories where to find installer modules
#######################################

# These are Model specific ones, CAMx is the default model
CXR_INSTALLER_MODEL_INPUT_DIR=${CXR_INSTALLER_INPUT_DIR}/${CXR_MODEL}
readonly CXR_INSTALLER_MODEL_INPUT_DIR

# And these Version specific
CXR_INSTALLER_VERSION_INPUT_DIR=${CXR_INSTALLER_MODEL_INPUT_DIR}/${CXR_MODEL_VERSION}
readonly CXR_INSTALLER_VERSION_INPUT_DIR

#######################################
# Directories where to find processors
#######################################

#Pre- Processors
# These are run for each simulation day
CXR_PREPROCESSOR_DAILY_INPUT_DIR=${CXR_MODULES_DIR}/preproc/${CXR_MODEL}/${CXR_MODEL_VERSION}/daily
readonly CXR_PREPROCESSOR_DAILY_INPUT_DIR

#These are run before the simulation starts
CXR_PREPROCESSOR_ONCE_INPUT_DIR=${CXR_MODULES_DIR}/preproc/${CXR_MODEL}/${CXR_MODEL_VERSION}/once
readonly CXR_PREPROCESSOR_ONCE_INPUT_DIR

# Post- Processors
# These are run for each simulation day
CXR_POSTPROCESSOR_DAILY_INPUT_DIR=${CXR_MODULES_DIR}/postproc/${CXR_MODEL}/${CXR_MODEL_VERSION}/daily
readonly CXR_POSTPROCESSOR_DAILY_INPUT_DIR

#These are run once the whole simulation has finished
CXR_POSTPROCESSOR_ONCE_INPUT_DIR=${CXR_MODULES_DIR}/postproc/${CXR_MODEL}/${CXR_MODEL_VERSION}/once
readonly CXR_POSTPROCESSOR_ONCE_INPUT_DIR

# Model
CXR_MODEL_INPUT_DIR=${CXR_MODULES_DIR}/model/${CXR_MODEL}/${CXR_MODEL_VERSION}
readonly CXR_MODEL_INPUT_DIR

################################################################################
# Parameters to estimate space needed
################################################################################

# MB per 10^5 cells/species/timestep
# If no source apportionment is selected
CXR_C_SPACE_NONE=2.2046 

# The actual selection must be based on options
CXR_C_SPACE=${CXR_C_SPACE_NONE}

# Safety factor
CXR_F_MARGIN=1.1

# Fraction of space (of total run) we need in the tmp dir
CXR_TMP_SPACE_FACTOR=0.05

################################################################################
# Installer - specific settings (find more in the CAMx version specific .conf files)
################################################################################

# A comma-separated list of Probing tools
# Note that support of CAMxRunner is still limited in this area
CXR_SUPPORTED_CAMX_PROBING_TOOLS="None OSAT PSAT DDM PA RTRAC"

# A comma-separated list of parallel paradigms.
# CAMx 5.x also supports MPI
CXR_SUPPORTED_PARALLEL_PARADIGMS="None OMP MPI"

# A list of supported platforms for CAMx (could be determined using uname - but this is configures job!)
# Taken from the Makefile
CXR_SUPPORTED_PLATFORMS="dec pg_linux pg_linuxomp i_linux i_linuxomp ab_linux sgi sgiomp sun hp ibm"

# Directory rules to find patches. CXR_CURRENT_BINARY contains the name of the current binary (e. g. 'CAMx'), CXR_CURRENT_PLATFORM the platform we compile for (e. g. 'i_linuxomp')
CXR_PATCH_ALL_DIR_RULE='${CXR_INSTALLER_INPUT_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/patches/${CXR_CURRENT_BINARY}/all_platforms'
CXR_PATCH_PLATFORM_DIR_RULE='${CXR_INSTALLER_INPUT_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/patches/${CXR_CURRENT_BINARY}/$CXR_CURRENT_PLATFORM'

################################################################################
# CAMxRunner specific settings
################################################################################

# Set this to false to disable CAMx in this run
CXR_RUN_MODEL=true

# Logs go here
CXR_LOG_OUTPUT_DIR=${CXR_RUN_DIR}/log
readonly CXR_LOG_OUTPUT_DIR

#State information is stored here
CXR_STATE_DIR=${CXR_RUN_DIR}/state/$CXR_RUN
readonly CXR_STATE_DIR

########################################
# Parallel Task execution. 
# This is global to all instances
########################################

# Task directories
CXR_TASK_POOL_DIR=$CXR_STATE_DIR/tasks
readonly CXR_TASK_POOL_DIR

# Contains links to the tasks that need to be done still
# The links have the form 0001
CXR_TASK_TODO_DIR=$CXR_TASK_POOL_DIR/todo
readonly CXR_TASK_TODO_DIR

# Contains links to tasks that are currently running
# the name is $(uname -n)$$ 
CXR_TASK_RUNNING_DIR=$CXR_TASK_POOL_DIR/running
readonly CXR_TASK_RUNNING_DIR

# Contains links to the successfully terminated tasks
# name is the day offset and the module name (like 1_convert_emissions)
CXR_TASK_SUCCESSFUL_DIR=$CXR_TASK_POOL_DIR/successful
readonly CXR_TASK_SUCCESSFUL_DIR

# Contains tasks that failed
# name is the day_offset and the module name (like 1_convert_emissions)
# if a subsequent module depends on a failed module, it will stop the run, otherwise it will continue
CXR_TASK_FAILED_DIR=$CXR_TASK_POOL_DIR/failed
readonly CXR_TASK_FAILED_DIR

# Workers
CXR_WORKER_DIR=$CXR_STATE_DIR/workers
readonly CXR_WORKER_DIR

CXR_RUNNING_WORKER_DIR=$CXR_WORKER_DIR/running
readonly CXR_RUNNING_WORKER_DIR

CXR_WAITING_WORKER_DIR=$CXR_WORKER_DIR/waiting
readonly CXR_WAITING_WORKER_DIR

# Files to store currently active modules (for simpler dependecies)
CXR_ACTIVE_ONCE_PRE_LIST=$CXR_STATE_DIR/active_once_pre_list
readonly CXR_ACTIVE_ONCE_PRE_LIST

CXR_ACTIVE_DAILY_PRE_LIST=$CXR_STATE_DIR/active_daily_pre_list
readonly CXR_ACTIVE_DAILY_PRE_LIST

CXR_ACTIVE_DAILY_POST_LIST=$CXR_STATE_DIR/active_daily_post_list
readonly CXR_ACTIVE_DAILY_POST_LIST

CXR_ACTIVE_ONCE_POST_LIST=$CXR_STATE_DIR/active_once_post_list
readonly CXR_ACTIVE_ONCE_POST_LIST

CXR_ACTIVE_MODEL_LIST=$CXR_STATE_DIR/active_model_list
readonly CXR_ACTIVE_MODEL_LIST

CXR_ACTIVE_ALL_LIST=$CXR_STATE_DIR/active_all_list
readonly CXR_ACTIVE_ALL_LIST

# This is where the locks go
# must be exported because lock manager reads from environment
CXR_LOCK_DIR=$CXR_STATE_DIR/locks
readonly CXR_LOCK_DIR

################################################################################
# CAMxRunner Internal stuff
################################################################################

# Important files
# Name of the Log file 
CXR_LOG=${CXR_LOG_OUTPUT_DIR}/${CXR_RUN}_$(date +"%Y_%m_%d_%H_%M")${CXR_LOG_EXT}.log

# These functions will report verbosely, no matter what
# Space separated list - for debugging only
CXR_LOG_FUNCTION_VERBOSE_LIST=""

################################################################################
# Per-Instance Information
################################################################################

########################################
# Machine name
########################################

CXR_MACHINE="$(uname -n)"

########################################
#  Instance name PID@HOST
########################################
CXR_INSTANCE=${$}@${CXR_MACHINE}
readonly CXR_INSTANCE

########################################
# This directory holds the instance-information
# The whole directory is removed after a run
# All files named CXR_INSTANCE_FILE_* are
# Created automatically in <cxr_common_initialize_state_db>
# The directories must be created explicitly there
########################################

CXR_ALL_INSTANCES_DIR=${CXR_STATE_DIR}/instances

CXR_INSTANCE_DIR=${CXR_ALL_INSTANCES_DIR}/${CXR_INSTANCE}
readonly CXR_INSTANCE_DIR

# Hashes are stored here
CXR_HASH_DIR=${CXR_INSTANCE_DIR}/hashes
readonly CXR_HASH_DIR

# Determine the name of the Continue file (must be this early because of lock manager)
# This is used to search for any continue files
CXR_CONTINUE=${CXR_RUNNER_NAME}.${CXR_STATE_CONTINUE}

# The whole file name -if it is deleted, this instance stops
# This file is created explictly in cxr_common_initialize_state_db
CXR_CONTINUE_FILE=${CXR_INSTANCE_DIR}/${CXR_CONTINUE}
readonly CXR_CONTINUE_FILE

# In this file, we store the error stack
CXR_INSTANCE_FILE_ERROR_STACK=${CXR_INSTANCE_DIR}/error.stack
readonly CXR_INSTANCE_FILE_ERROR_STACK

# In this file, we store the days where issues where detected
CXR_INSTANCE_FILE_FAILED_DAYS=${CXR_INSTANCE_DIR}/failed.days
readonly CXR_INSTANCE_FILE_FAILED_DAYS

# In this file, we store files that where decompressed
CXR_INSTANCE_FILE_DECOMPRESSED=${CXR_INSTANCE_DIR}/decompressed.files
readonly CXR_INSTANCE_FILE_DECOMPRESSED

########################################
# The next few files are SECURITY RELEVANT.
# 1. Use a random number in the name
# 2. Use chmod to deny access (done by cxr_common_initialize_state_db)
########################################

# In this file, we store all output files that have been generated
# This is used to compress output if needed or to delete output.
# We will not use a random number in the name (We might need this file 
# after a run completed), but we protect it using chmod
CXR_INSTANCE_FILE_OUTPUT_LIST=${CXR_INSTANCE_DIR}/output_files
readonly CXR_INSTANCE_FILE_OUTPUT_LIST

# In this file, we store dummy files of a dry run.
# These will be deleted after the dry run
CXR_INSTANCE_FILE_DUMMY_LIST=${CXR_INSTANCE_DIR}/dry_dummy_files.${RANDOM}
readonly CXR_INSTANCE_FILE_DUMMY_LIST

# In this file, we store temporay files of a run. 
# (e. g. AHOMAP Control files)
# These will be deleted after each model day
CXR_INSTANCE_FILE_TEMP_LIST=${CXR_INSTANCE_DIR}/temp_files.${RANDOM}
readonly CXR_INSTANCE_FILE_TEMP_LIST

# In this file, we store long-lived temporay files of a run. 
# These will be deleted after each model run
CXR_INSTANCE_FILE_TEMP_LIST_LONG=${CXR_INSTANCE_DIR}/temp_files_longterm.${RANDOM}
readonly CXR_INSTANCE_FILE_TEMP_LIST_LONG

# In this file, we store the locks we have aqcired
CXR_INSTANCE_FILE_LOCK_LIST=${CXR_INSTANCE_DIR}/lock_list.${RANDOM}
readonly CXR_INSTANCE_FILE_LOCK_LIST

################################################################################
# Supported model names.
################################################################################
CXR_SUPPORTED_MODELS="CAMx PMCAMx"
#                            |    | 
#                      Model 0    |
#                           Model 1 ...

# Supported CAMx Versions (space separated)
CXR_SUPPORTED_MODEL_VERSIONS[0]="4.42 4.51 5.10"
CXR_SUPPORTED_MODEL_VERSIONS[1]="2008"

# Comma separated list of subdirectories which must be in $CXR_RUN_DIR
CXR_RUN_SUBDIRS="bin conf doc log modules state templates"
readonly CXR_RUN_SUBDIRS

# Comma separated list of subdirectories containig version-specific driectories
CXR_RUN_VERSION_SUBDIRS="bin modules/common modules/installer modules/model modules/preproc modules/postproc"
readonly CXR_RUN_VERSION_SUBDIRS

################################################################################
# Information relevant for compilation
################################################################################

# These are the supported platforms of my pgf77
CXR_FORTRAN_PLATFORMS="amd64 amd64e athlon athlonxp barcelona barcelona-64 core2 core2-64 k7 k8-32 k8-64 k8-64e p5 p6 p7 p7-64 piii piv px px-64 x64"


################################################################################
# bin2asc conversion
################################################################################
#No leading dot please
CXR_ASC_EXT=asc
readonly CXR_ASC_EXT

################################################################################
# Executables
################################################################################

# Lock manager
CXR_LOCK_MAN_EXEC=${CXR_BIN_DIR}/lock.sh

# Decompression
CXR_GUNZIP_EXEC=$(which gunzip)
CXR_BUNZIP2_EXEC=$(which bunzip2)

# Compression
CXR_GZIP_EXEC=$(which gzip)
CXR_BZIP2_EXEC=$(which bzip2)

# The compressor we use to compress output (if any)
if [ "${CXR_COMPRESS_OUTPUT}" == true ]
then
	CXR_COMPRESSOR_EXEC="${CXR_BZIP2_EXEC}"
fi

# Fortran-77 Compiler (only needed when installing)
CXR_FORTRAN77_COMPILER_EXEC=$(which pgf77)

# Fortran-90 Compiler (only needed when installing)
CXR_FORTRAN90_COMPILER_EXEC=$(which pgf90)

# This is the default FORTRAN compiler
CXR_FORTRAN_COMPILER_EXEC="$CXR_FORTRAN77_COMPILER_EXEC"

# API GENERATOR
CXR_API_DOC_EXEC=${CXR_BIN_DIR}/generate-api-doc.sh

# wget to download stuff (only needed for installation  - save space)
CXR_WGET_EXEC=$(which wget)


#### It is recommended to compile the binaries
#### into binaries that carry the machine type in their name
#### CAMxRunner -I can do this for you.
#### 
#### Also the function cxr_main_get_binary_name then selects the most appropriate one.
#### replace with literal for your own choice

# Checker for landuse
CXR_SRFLND_EXEC=$(cxr_main_get_binary_name srflnd)

# The binary to convert bin2asc
CXR_BIN2ASC_EXEC=$(cxr_main_get_binary_name bin2asc)

# The binary to convert asc2bin
CXR_ASC2IBN_EXEC=$(cxr_main_get_binary_name asc2bin)

# The binary to convert Emission ascii to Bin
CXR_AIRCONV_EXEC=$(cxr_main_get_binary_name airconv)

# The binary to convert MM5 Input
CXR_UAMVASCII_EXEC=$(cxr_main_get_binary_name uamvascii)

# Binary to convert the other way
CXR_UAMVBINR_EXEC=$(cxr_main_get_binary_name uamvbinr)

# Yet another binary to convert MM5 Input
CXR_AIRASCII_EXEC=$(cxr_main_get_binary_name airascii)

# Needed to create constant IC/BC fields
CXR_ICBCPREP_EXEC=$(cxr_main_get_binary_name icbcprep)

# Avgdif
CXR_AVGDIF_EXEC=$(cxr_main_get_binary_name avgdif)

########################################
# Preprocessors
########################################	
CXR_AHOMAP_EXEC=$(cxr_main_get_binary_name ahomap)

CXR_TUV_EXEC=$(cxr_main_get_binary_name tuv)

################################################################################
# Advanced Input check
################################################################################

# If this is true, the we will look for compressed versions of missing files
CXR_DETECT_COMPRESSED_INPUT_FILES=true

# If this is true, files will be decompressed where they are.
# This is not recommended, because the user might not have write permission in all
# source directories.
# If it is false, make sure that the directory CXR_TMP_DIR has enough space free...
CXR_DECOMPRESS_IN_PLACE=false

# These extensions are searched with the automatic feature
# CAMxRunner does not determine the compression type from the suffix,
# this is done using "file"
CXR_COMPRESSED_EXT=".gz .bz2 .zip"


################################################################################
################################################################################
# Model details
################################################################################
################################################################################

################################################################################
# Name/Path of the CAMx.in file
################################################################################

# A rule that must be expanded
CXR_CAMXIN_RULE='${CXR_STATE_DIR}/CAMx.${CXR_DATE_RAW}.in'


########################################
# Options for the export of station-specific data
########################################

# Which species should be extracted (all does all, otherwise, provide a space separated list)
CXR_STATION_SPECIES=${CXR_ALL}

# Format can be either CSV or IDL
CXR_STATION_FILE_FORMAT=IDL

# The format of the numbers to use
CXR_STATION_NUMBER_FORMAT=(5e14.7)

# when true, concentrations will be normalized
CXR_STATION_NORMALIZE_CONCENTRATIONS=true

# Using these values for normalization
CXR_STATION_NORMALIZE_TEMPERATURE_K=293
CXR_STATION_NORMALIZE_PRESSURE_PA=101300

# When true, coordinates are logitude/latitude, when false coordinates are grid-indices (0-based)
CXR_STATION_LATLON_COORDINATES=false

# extracting from this grid (currently, only one is supported)
CXR_STATION_DOMAIN=3

########################################
# Preparation of IC/BC/TC
########################################

# Method that is used to determine ic/bc/tc
# Currently supported: MOZART, MOZART_INCREMENT, MOZART_CONSTANT or ICBCPREP
#
# MOZART uses the data from the MOZART global CTM
# MOZART_CONSTANT uses our preprocessor but replaces the MOZART data by constant values
# MOZART_INCREMENT allows to selectively increment certain species by a specified value (in PPM)
# ICBCPREP uses the ENVIRON preprocessor icbcprep (resulting in constant IC and BC data)

CXR_IC_BC_TC_METHOD=MOZART

if [ "$CXR_IC_BC_TC_METHOD" != MOZART ]
then
	# If we are not using MOZART, we need to define the constant values
	# or the increments.
	# If you are using MOZART_CONSTANT, you need to supply data for each element
	# of CXR_CAMX_MOZART_MAPPING because the preprocessors loops through that array

	# Here you need to define the species and the concentrations
	# (in ppm for gases, in ug/m**3 for aerosols) to use (format is "SPECIES:conc")
	# Depending on CXR_IC_BC_TC_METHOD, these are either increments or constant values.
	# Make sure the numbers are in Fortran 10.7 Format
	
	# Unset first - it's an array!
	unset CXR_IC_BC_TC_SPEC
	
	CXR_IC_BC_TC_SPEC[0]=O3:0.074740447 
	CXR_IC_BC_TC_SPEC[1]=CO:0.113577254 
	CXR_IC_BC_TC_SPEC[2]=FORM:0.000146525 
	CXR_IC_BC_TC_SPEC[3]=NO:0.000000436 
	CXR_IC_BC_TC_SPEC[4]=NO2:0.000035782 
	CXR_IC_BC_TC_SPEC[5]=HNO3:0.000045410 
	CXR_IC_BC_TC_SPEC[6]=PNA:0.000001336 
	CXR_IC_BC_TC_SPEC[7]=H2O2:0.001014395 
	CXR_IC_BC_TC_SPEC[8]=PAN:0.000176876 
	CXR_IC_BC_TC_SPEC[9]=CH4:1.843935728 
	CXR_IC_BC_TC_SPEC[10]=ALD2:0.000023972 
	CXR_IC_BC_TC_SPEC[11]=ETHA:0.000482804 
	CXR_IC_BC_TC_SPEC[12]=ISOP:0.000002357 
	CXR_IC_BC_TC_SPEC[13]=TOL:0.000009159 
fi

# This array describes the mapping of MOZART to CAMx species
# Prior to 2004, MOZART provides only 5 species
# The order is the same, so it is sufficient to change this parameter
# in the config file

# Unset first - it's an array!
unset CXR_CAMX_MOZART_MAPPING

# Syntax is CAMx:MOZART
CXR_CAMX_MOZART_MAPPING[0]=O3:O3_VMR_inst
CXR_CAMX_MOZART_MAPPING[1]=CO:CO_VMR_inst
CXR_CAMX_MOZART_MAPPING[2]=FORM:CH2O_VMR_inst
CXR_CAMX_MOZART_MAPPING[3]=NO:NO_VMR_inst
CXR_CAMX_MOZART_MAPPING[4]=NO2:NO2_VMR_inst
CXR_CAMX_MOZART_MAPPING[5]=HNO3:HNO3_VMR_inst
CXR_CAMX_MOZART_MAPPING[6]=PNA:HO2NO2_VMR_inst
CXR_CAMX_MOZART_MAPPING[7]=H2O2:H2O2_VMR_inst
CXR_CAMX_MOZART_MAPPING[8]=PAN:PAN_VMR_inst
CXR_CAMX_MOZART_MAPPING[9]=CH4:CH4_VMR_inst
CXR_CAMX_MOZART_MAPPING[10]=ALD2:CH3CHO_VMR_inst
CXR_CAMX_MOZART_MAPPING[11]=ETHA:C2H6_VMR_inst
CXR_CAMX_MOZART_MAPPING[12]=ISOP:ISOP_VMR_inst
CXR_CAMX_MOZART_MAPPING[13]=TOL:TOLUENE_VMR_inst

# Automatically count # species (no dummy)
CXR_NUM_MOZART_SPECIES=$(( ${#CXR_CAMX_MOZART_MAPPING[@]} ))


# This flag indicates that we already loaded the defaults
# this is needed bc. of the readonly stuff

CXR_DEFAULTS_LOADED=true
