#!/usr/bin/env bash
#
# Main functions for runner script for CAMx
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
# Version: $Id$ 
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# 
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Who		When				What
# dco		28.01.2009	Created by removal from CAMxRunner.sh

################################################################################
# Function: cxr_main_is_numeric
#
# A small, relatively robust function to test if the argument is numeric
#
# Parameters:
# $1 - String to test
################################################################################
function cxr_main_is_numeric()
################################################################################
{ 
	# If this returns 0, there is at least a non-numeric character
	# present. Negative values are OK
	
	# We neet the retval,
	# turn off strict checks
	set +e
	
	echo "${1:-}" | grep "[-]?[^0-9]" >/dev/null
	
	if [ $? -eq 0 -o -z "$1" ] 
	then
			echo false
	else
			echo true
	fi
	
	#Turn strict checks back on
	set -e
}

################################################################################
# Function: cxr_main_count_cores
#
# Counts the number of logical cores in a box. 
#
################################################################################
function cxr_main_count_cores()
################################################################################
{
	if [ ! -r /proc/cpuinfo ]
	then
		#Probably no linux box.
		# TODO: We need to find a POSIX approach!
		echo 1
	else
		# Aggregate processor entries in /proc/cpuinfo
		num_cores=$(grep 'processor' /proc/cpuinfo | sort -u | wc -l)
		echo $num_cores
	fi

	return 0
}

################################################################################
# Function: cxr_main_increase_log_indent
#
# Increases the intedation of the logfile. Can be used to structure 
# the log inte logical units.
# DO NOT CALL IN a $() construct (creates new process with new environment)
################################################################################
function cxr_main_increase_log_indent()
################################################################################
{
	#Make sure it is set
	CXR_LOG_INDENT=${CXR_LOG_INDENT:-0}
	
	NEXT=$(( $CXR_LOG_INDENT + 1 ))
	
	if [ "$NEXT" -gt "$CXR_MAX_LOG_INDENT" ]
	then
		# Maximum reached
		NEXT=$CXR_MAX_LOG_INDENT
	fi
	
	CXR_LOG_INDENT=$NEXT
	
}

################################################################################
# Function: cxr_main_decrease_log_indent
#
# Decreases the intedation of the logfile. Can be used to structure 
# the log inte logical units.
# DO NOT CALL IN a $() construct (creates new process with new environment)
################################################################################
function cxr_main_decrease_log_indent()
################################################################################
{
	#Make sure it is set
	CXR_LOG_INDENT=${CXR_LOG_INDENT:-0}
	
	NEXT=$(( $CXR_LOG_INDENT - 1 ))
	
	if [ $NEXT -lt 0 ]
	then
		NEXT=0
	fi
	
	CXR_LOG_INDENT=$NEXT
}

################################################################################
# Function: cxr_main_increase_error_count
#
# Adds an error to the ${CXR_INSTANCE_FILE_ERROR_STACK}. Each Message must only be one 
# line long (number of lines = number of errors)
#
# Parameters:
# $1 - Message for the error-stack file
################################################################################
function cxr_main_increase_error_count()
################################################################################
{

	# This is not beautiful, actually we need to make this sure 
	# earlier!
	if [ ! -d ${CXR_STATE_DIR:-} ]
	then
		mkdir -p ${CXR_STATE_DIR}
	fi
	
	# Make sure we have such a variable
	if [ "${CXR_INSTANCE_FILE_ERROR_STACK:-}" ]
	then

		# Add text to the stack (if empty a newline is written)
		# No -e (one line per error) - the lines are counted later
		# Could be that the state db is not yet initialized
		if [ -f "${CXR_INSTANCE_FILE_ERROR_STACK}" ]
		then
			echo "${1:-"No message"}" >> "${CXR_INSTANCE_FILE_ERROR_STACK}"
		fi	
	
	fi
	
	# Also show error to the user
	cxr_main_logger "${FUNCNAME}" "${1:-}"
}

################################################################################
# Function: cxr_main_get_error_count
#	
# Returns the current error count 
# by counting lines in  the ${CXR_INSTANCE_FILE_ERROR_STACK}).
################################################################################
function cxr_main_get_error_count()
################################################################################
{
	# Read from file - if not set, use /dev/null
	# Also redirect errors of cat to /dev/null
	NUMBER=$(cat ${CXR_INSTANCE_FILE_ERROR_STACK:-/dev/null} 2>/dev/null | wc -l )
	
	echo $NUMBER
}

################################################################################
# Function: cxr_main_logger
#
# Writes a message to the Log-file (if defined) and STDERR.
#
# As an important SIDE EFFECT IN CASE OF AN ERROR, this function adds the error to the error list.
# Also, the current day (CXR_DATE) is added to a special logfile which shows the failed days.
#
# We use STDERR instead of STDOUT in order not to interfere with returning values from functions.
# If you want a function to return log messages, rotate stderr and stdout:
# > $(my_function 2>&1)
#
# To determine if a message is wanted, we distinguish these log-levels (set by the corresponding command line option):
# -a showed always
# -d DEBUG (DBG) [16] : These are messages a user normally does not want to see
# -v VERBOSE (VRB) [8] : Messages that may help to solve problems or understand how CAMxRunner works
# -i INFO (INF) [4] - Default : Purely informational statements (like how many grid cells there are in a simulation)
# -w WARN (WRN) [2] : Indicates that something might be wrong
# -e ERROR (ERR) [1] : Something has failed (will always be shown)
#
# The user can manipulate Separate Loglevels for screen (CXR_LOG_LEVEL_SCREEN) and file (CXR_LOG_LEVEL_FILE),
# Usually the screen shows less and the file shows more.
# If the loglevel is greater or equal to the level of a message, the mesage is shown
#
# The function automatically indents its messages according to $CXR_LOG_INDENT
#
# For efficiency reasons (it's inefficient already...), call the function only once for multi-line messages
# use \n to separate lines.
# The use of continuation characters like
#> cxr_main_logger "$FUNCNAME" "This is \
#>              a message"
# is not recommended because the whitespace before the second part of the string will 
# be output "as is".
# 
# Parameters:
# [-a] - shown always, no prefix, cannot be turned off
# [-d] - debug message
# [-v] - verbose message
# [-i] - information message (Default)
# [-w] - warning
# [-e] - error (always shown)
# [-n] - no write to file (useful where CXR_LOG is defined but we still want no logfile)
# [-b] - surround with small box of stars
# [-B] - surround with large box of stars
# [-H] - surround with huge box of stars
# $1 - Function from which the message comes (can be used to filter)
# $2 - Message
################################################################################	
function cxr_main_logger()
################################################################################
{
	# By default, we write to STDERR
	# Note that CXR_DO_FILE_LOGGING is being set elsewhere
	local TO_STDERR=true
	local PREFIX=
	
	# A standard message is an info
	LOG_LEVEL=${CXR_LOG_LEVEL_INF}
	
	# A line of stars
	local STARS=""
	
	# One star
	local STAR=""
	
	while getopts ":ndviweabBH" opt
	do
		case $opt in
		b) STARS="${CXR_BOX_SMALL}"; STAR="${CXR_STAR} ";;
		B) STARS="${CXR_BOX_LARGE}"; STAR="${CXR_STAR} ";;
		H) STARS="${CXR_BOX_HUGE}"; STAR="${CXR_STAR} " ;;
		n) local CXR_DO_FILE_LOGGING=false;; #Local only!
		d) LOG_LEVEL=$CXR_LOG_LEVEL_DBG ; PREFIX=DBG: ;;
		v) LOG_LEVEL=$CXR_LOG_LEVEL_VRB ; PREFIX= ;;
		i) LOG_LEVEL=$CXR_LOG_LEVEL_INF ; PREFIX= ;;
		w) LOG_LEVEL=$CXR_LOG_LEVEL_WRN ; PREFIX=WRN: ;;
		e) LOG_LEVEL=$CXR_LOG_LEVEL_ERR ; PREFIX=ERR: ;;
		a) LOG_LEVEL=$CXR_LOG_LEVEL_ALW ; PREFIX= ;;
		esac
	done
	
	# This is not strictly needed, but it allows to read 
	# non-named command line options
	shift $(($OPTIND - 1))
	
	# Make getopts ready again
	unset OPTSTRING
	unset OPTIND
	
	# Just not to crash if $1 or $2 are not passed
	FUNCTION_NAME="${1:-}"
	BLANK_MESSAGE="${PREFIX}${2:-}"
	
	STDERR_MESSAGE="$BLANK_MESSAGE"
	
	### In case of error, write current day to failed days file
	### and increase error count
	if [ "${LOG_LEVEL}" == "${CXR_LOG_LEVEL_ERR}" ]
	then
		# Could be that the state db is not yet initialized
		if [ -f "${CXR_INSTANCE_FILE_FAILED_DAYS:-}" ]
		then
			echo "${CXR_DATE:-date_not_defined}" >> "${CXR_INSTANCE_FILE_FAILED_DAYS:-/dev/null}"
		fi
		
		cxr_main_increase_error_count "${FUNCTION_NAME} - ${BLANK_MESSAGE}"
	fi
	
	# We have to look at several things separately: 
	# - writing to STDERR
	# - writing to the logfile
	# - external channels like twitter/email/sms (treated together)
	
	# STDERR
	# Here we add no indentation
	if [ "${CXR_LOG_LEVEL_SCREEN:-${CXR_LOG_LEVEL_INF}}" -ge "${LOG_LEVEL}" ]
	then
	
		# If stars are set, we add a line of stars at the beginning and end
		if [ "${STARS}" ]
		then
			STDERR_MESSAGE="${STARS}\n${STDERR_MESSAGE}\n${STARS}\n"
		fi
		
		# Write a unformatted copy to sterr
		echo -e "${STDERR_MESSAGE}" 1>&2
		
	else
	
		# There is a second chance: the user might have added the function in question to 
		# CXR_LOG_FUNCTION_VERBOSE_LIST
		local FOUND=$(expr match " ${CXR_LOG_FUNCTION_VERBOSE_LIST:-}" ".*${FUNCTION_NAME}.*")
		
		if [ "${FOUND}" -ne 0 ]
		then
			# Function was added by user
			
			# If stars are set, we add a line of stars at the beginning and end
			if [ "${STARS}" ]
			then
				STDERR_MESSAGE"${STARS}\n${STDERR_MESSAGE}\n${STARS}\n"
			fi
			
			# Write a unformatted copy to sterr
			echo -e "${STDERR_MESSAGE}" 1>&2
		fi
		
	fi
	
	# file
	# Prepare and write to file only if we have a defined target
	# and this was not suppressed in the call
	if [ "${CXR_LOG_LEVEL_FILE:-${CXR_LOG_LEVEL_INF}}" -ge "${LOG_LEVEL}" -a -n "${CXR_LOG:-}" -a "$CXR_DO_FILE_LOGGING" == true ]
	then
		# Logfile is defined

		# Add date of form Aug 26 15:44:45
		# The printf prints CXR_LOG_INDENT tabs
		PREFIX="$(date +"%b %d %H:%M:%S"):$(printf '\t%.0s' {1..$CXR_LOG_INDENT})"
	
		# Put prefix in front of the Blank Message
		MESSAGE="${PREFIX}${BLANK_MESSAGE}"
	
		# Now, all the newline characters need a prefix as well:
		# \\n is what we are looking for, // request ALL \n to be replaced
		MESSAGE="${MESSAGE//\\n/\n${PREFIX}}"
		
		# If stars are set, we add a line of stars at the beginning and end
		if [ "${STARS}" ]
		then
			MESSAGE="${PREFIX}${STARS}\n${MESSAGE}\n${PREFIX}${STARS}\n"
		fi
		
		# Writing to file
		echo -e "$MESSAGE" >> ${CXR_LOG}
	fi
	
	# The external channels (motify decides who needs to get the message)
	# -a does not go there
	if [ "${LOG_LEVEL}" -gt "${CXR_LOG_LEVEL_ALW}" -a "${CXR_LOG_LEVEL_EXTERNAL:-${CXR_LOG_LEVEL_ERR}}" -ge "${LOG_LEVEL}" ]
	then
		cxr_main_notify "Log" "$BLANK_MESSAGE"
	fi
	

	return $CXR_RET_OK
}

################################################################################
# Function: cxr_main_notify
#	
# Sends out a notification to $CXR_MAILADDR, 
# $CXR_SMSADDR and to a Twitter account if needed
#
# Parameters:
# $1 - Subject
# $2 - Body
################################################################################
function cxr_main_notify()
################################################################################
{
	ADRESSES=
	
	#Add SMS Adress if needed
	if [ ${CXR_SEND_SMS} == true ]
	then
		ADRESSES="$ADRESSES $CXR_SMSADDR"
	fi
	
	# Add Email if needed
	if [ ${CXR_SEND_EMAIL} == true ]
	then
		ADRESSES="$ADRESSES $CXR_MAILADDR"
	fi
	
	if [ "${ADRESSES}" ]
	then
		# send it if we have any adresses
		echo -e $2 | mail -s "$1" $ADRESSES
	fi
	
	# Twitter
	if [ ${CXR_SEND_TWITTER} == true ]
	then
		
		# Get the data from .twitterrc
		if [ ! -r ~/.twitterrc ]
		then
			# no .twitterrc
			cxr_main_logger -e "$FUNCNAME" "To use Twitter, you need to create a ~/.twitterrc file, in there set ACCOUNT= and PASS=."
		else
			# .twitterrc there
			source ~/.twitterrc
			
			# Communicate w. Twitter (easy using curl)
			# Send any output to /dev/null
			curl --basic --user "${ACCOUNT}:${PASS}" --data-ascii "status=`perl -MURI::Escape -e \
			"print uri_escape('$(echo -e ${1}:${2})');"`"  http://twitter.com/statuses/update.json >/dev/null 2>&1
			
		fi
	fi
	
}

################################################################################
# Function: cxr_main_cleanup
#
# Cleans the state DB/Locks etc.
#
################################################################################
function cxr_main_cleanup()
################################################################################
{
	
	cxr_main_logger "$FUNCNAME" "Cleaning up - note that it can take more than $CXR_WAITING_SLEEP_SECONDS seconds until all processes are down!"
	
	# Remove any traps once called
	trap - INT TERM EXIT
	
	################################################################################
	# Error report - do in any Non-hollow case
	################################################################################	
	
	if [ "$CXR_HOLLOW" == false  ]
	then
		
		# All the non-hollow stuff goes here
		ERROR_COUNT=$(cxr_main_get_error_count)
		
		cxr_main_logger -B "$FUNCNAME"  "Error Report (may be repeated)"
		
		if [ "${ERROR_COUNT:-0}" -ne 0 ]
		then
			cxr_main_logger "$FUNCNAME"  "The system detected at least $ERROR_COUNT errors, fix them!\n\nHere is the error stack:\n"
			
			if [ "${CXR_LOG:-}" ]
			then
				# Write to logfile
				cat "$CXR_INSTANCE_FILE_ERROR_STACK" | tee -a ${CXR_LOG}
				
				cxr_main_logger -a "$FUNCNAME" "In these days, errors where detected:"
				# This file might contain dupes
				cat "$CXR_INSTANCE_FILE_FAILED_DAYS" | sort | uniq | tee -a ${CXR_LOG}
			else
				# There is no log
				cat "$CXR_INSTANCE_FILE_ERROR_STACK"
				
				cxr_main_logger -a "$FUNCNAME" "In these days, errors where detected:"
				cat "$CXR_INSTANCE_FILE_FAILED_DAYS" | sort | uniq 
			fi
			
		else	
			cxr_main_logger "$FUNCNAME"  "No errors where detected in this subprocess - exit status is $CXR_STATUS"
		fi
		cxr_main_logger "$FUNCNAME"  "**********************************************************"
	
		########################################
		# Do cleanup stuff - Run always when not hollow
		########################################
		
		if [ "$CXR_DRY" == true ]
		then
			cxr_main_logger "$FUNCNAME" "Dryrun: removing the dummy files..."
			
			# At least touch it
			touch "$CXR_INSTANCE_FILE_DUMMY_LIST" 2>/dev/null
			
			# Clean dummy files away
			for DUMMY_FILE in $(cat "$CXR_INSTANCE_FILE_DUMMY_LIST")
			do
				cxr_main_logger -v "$FUNCNAME"  "Deleting $DUMMY_FILE"
				
				rm -f "$DUMMY_FILE"
			done
		fi
		
		# Remove all tempfiles
		cxr_common_remove_tempfiles
		
		# Release Locks
		cxr_common_release_all_locks
		
		# Delete the instance files
		cxr_common_delete_instance_data

	fi
	
	if [ "$CXR_STATUS" == "$CXR_STATUS_SUCCESS" ]
	then
		# All went well
		
		# In a dryrun, alwyas print this if finished OK
		if [ "$CXR_DRY" == true ]
		then
		
			cxr_main_logger -H "$FUNCNAME" "Before you can run the simulation, you must clean the state database\nbecause a dryrun makes the same entries as a normal run.\nDo this using this command:\n \t ${CXR_CALL} -c \nIf you have adjusted the configuration in ${CXR_CONFIG}\nand are happy with the log, run the simulation like this (assuming an AFS/Kerberos environment):\n"     
			
			# Increase global indent level
			cxr_main_increase_log_indent
  	
			cxr_main_logger -b "$FUNCNAME" "\t \$ k5run -b ${0}"
			
			# Decrease global indent level
			cxr_main_decrease_log_indent
		
		else
			if [ "$CXR_HOLLOW" == false  ]
			then
				# print some infos if we are not hollow 
				cxr_main_logger "$FUNCNAME" "Ending ${CXR_RUN}, its now $(date)"
				cxr_main_notify "${CXR_RUN} finished on $CXR_MACHINE" "That is it, folks."
			fi
		fi
	fi
	
	# Change Back to rundir
	cd ${CXR_RUN_DIR}  || return $CXR_RET_ERROR
}

# Set the trap (needs function first)
trap "cxr_main_cleanup; exit" INT TERM EXIT

################################################################################
# Function: cxr_main_die_gracefully
#
# Prints a message and exits with value $2. must be in here because its important
# 
# Parameters:
# $1 - Last words
# [$2] - Run cleanup (if false, no cleanup is done)
# [$3] - Return code
################################################################################
function cxr_main_die_gracefully() 
################################################################################
{
	# Release any traps
	trap - INT TERM EXIT
	
	# Stop this run
	# Delete any .CONTINUE FILES
	
	if [ -d "${CXR_STATE_DIR:-}" ]
	then
		rm -f ${CXR_STATE_DIR}/*.${CXR_STATE_CONTINUE} 2>/dev/null
	fi
	
	cxr_main_logger -a "$FUNCNAME" "### CAMxRunner Crashes because of this reason:############"
	# This function also echoes the message
	cxr_main_increase_error_count "${1:-Unknown error.}"
	cxr_main_logger -a "$FUNCNAME" "##########################################################"
	
	cxr_main_cleanup
	
	# Use second parameter as exit value if set, otherwise use ${CXR_RET_ERROR}
	# If still not set, use 1
	RET_CODE=${3:-${CXR_RET_ERROR:-1}}

	exit $CXR_RET_ERROR
}

################################################################################
# Function: cxr_main_get_binary_name
#
# Returns the name and path of a binary name other than CAMx.
# The name depends on:
# - The Machine Architecture
# - (The machine name)
# 
# First it tries to find a machine dependent binary, if this is not found, 
# a general one is seeked.
#
# Notifies logger if no suitable executable could be found.
#
# Such a name looks like this:
#
# > ${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/$1-${HOSTTYPE}[-$(uname -n)] (e. g. ~/CAMxRunner/bin/4.51/airascii-x86_64-linux)
#
# Example:
# > CXR_BIN2ASC_EXEC=$(cxr_main_get_binary_name bin2asc)
#
#
# Parameters:
# $1 - name (without path!) of the binary in question
################################################################################
function cxr_main_get_binary_name()
################################################################################
{
	if [ $# -ne 1 ]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - needs the name of a binary as input"
	fi
	
	#Determine possible names
	GENERAL_EXEC=${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/$1-${HOSTTYPE}
	MACHINE_EXEC=${GENERAL_EXEC}-$(uname -n)
	
	# Check name - machine first
	if [ -x ${MACHINE_EXEC} ]
	then
		# Machine dependent exists
		cxr_main_logger "$FUNCNAME"  "Binary $1 is actually called ${MACHINE_EXEC}"
		echo "${MACHINE_EXEC}"
	elif [ -x ${GENERAL_EXEC} ]
	then
		# general exists
		cxr_main_logger "$FUNCNAME"  "Binary $1 is actually called ${GENERAL_EXEC}"
		echo "${GENERAL_EXEC}"
	else
		#None exists
		if [ "$1" == false ]
		then
			# optional paratemeter is false,
			# We do not care and return CXR_GENERAL_EXEC
			echo "$GENERAL_EXEC" 
		else
			# NOK!
			cxr_main_logger -e "$FUNCNAME" "$FUNCNAME:$LINENO Could not find a suitable executable for name $1, neither ${MACHINE_EXEC} nor ${GENERAL_EXEC} are present.\nRecompile these components using\n\t\$ CAMxRunner.sh -I"
		fi
	fi
	
	return 0
}

################################################################################
# Function: cxr_main_determine_model_and_version
#
# Extracts the Version of CAMx from the link name.
# We need this in order to load the common functions and defaults.inc, 
# so no reference to these allowed here!
#
# Parameters:
# $1 - Run name of the form Model-vVersion-Whatever (e. g. CAMx-v4.51-ENVIRON_testcase)
################################################################################
function cxr_main_determine_model_and_version()
################################################################################
{
	# We expect a name like CAMx-v4.42-bafu3_winter07.run3_lcsl5a
	# So we split using "-"
	
	if [ $1 == CAMxRunner.sh -o $1 == base -o $1 == installer ]
	then
		# If the user does CAMxRunner.sh -h this must work too!
		VERSION=4.51
		MODEL=CAMx
	else
		# Split it
		MODEL=$(echo "$1" | cut -d- -f1)
		VERSION=$(echo "$1" | cut -d- -f2)
		
		# Remove v
		VERSION="${VERSION#v}"
	fi
	
	#######################################
	# Basic variables
	#######################################
	CXR_MODEL_VERSION=$VERSION
	CXR_MODEL=$MODEL
}

################################################################################
# Function: cxr_main_read_config
#
# Determine relevant config files and load them.
# There are currently these:
# - site.conf
# - base.conf
# - CAMx version specific
# - run-specific (potentially expanded)
#
# The run name 'installer' is treated separately, there we load
# - site.conf
# - base.conf
# - CAMx version specific
# - installer.conf
# - ${CXR_MODEL}-${CXR_MODEL_VERSION}-installer.conf (if available)
#
# Parameters:
# $1 - The name of the run to use
# $2 - The Model Version to use
# $3 - The Modelname to use
# $4 - The Rundir to use
################################################################################
function cxr_main_read_config()
################################################################################
{
	export CXR_RUN=${1}
	
	CXR_MODEL=${3}
	
	# Model Version
	CXR_MODEL_VERSION=${2}
		
	CXR_RUN_DIR="$4"
	
	if [ -z "$CXR_MODEL_VERSION" ]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - Could not determine Model Version when loading config!"
	fi
	
	if [ -z "$CXR_RUN_DIR" ]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - Could not determine run dir when loading config!"
	fi
	
	# Change to run dir
	cd "$CXR_RUN_DIR" || cxr_main_die_gracefully "Could not change to $CXR_RUN_DIR"

	# Now redefine, we need an absolute path
	CXR_RUN_DIR="$(pwd)"

	cxr_main_logger -v "$FUNCNAME"  "CXR_RUN_DIR of ${CXR_RUN} is $CXR_RUN_DIR"
	cxr_main_logger "$FUNCNAME" "Loading configuration for $CXR_RUN, $CXR_MODEL Version $CXR_MODEL_VERSION..."

	########################################
	# Locations of configuration
	########################################

	# Site config
	CXR_SITECONFIG=${CXR_CONF_DIR}/site.conf
	
	# Base config file - from this all other config is derived
	CXR_BASECONFIG=${CXR_CONF_DIR}/base.conf
	
	# Version specific
	CXR_VERSIONCONFIG=${CXR_CONF_DIR}/${CXR_MODEL}-v${CXR_MODEL_VERSION}.conf
	
	# This is an expanded form (might not exist)
	CXR_EXPANDED_CONFIG=${CXR_CONF_DIR}/${CXR_RUN}.econf
	
	# This is a "normal" copy
	CXR_CONFIG=${CXR_CONF_DIR}/${CXR_RUN}.conf
	
	########################################
	# Try to load the site config
	########################################
	
	# No drama if it does not exist
	if [ -f ${CXR_SITECONFIG} ]
	then
		cxr_main_logger -v "$FUNCNAME"  "Loading ${CXR_SITECONFIG}..."
	
		source ${CXR_SITECONFIG}
	else
		cxr_main_logger "$FUNCNAME" "I noticed that you have no site-specific configuration file (${CXR_SITECONFIG}) - this is not needed, but you might want to put site-specific stuff there."
	fi

	########################################
	# Try to load the base config
	########################################
	
	cxr_main_logger -v "$FUNCNAME"  "Loading ${CXR_BASECONFIG}..."
	
	if [ ! -f ${CXR_BASECONFIG} ]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - The base configuration ${CXR_BASECONFIG} is not available - exiting"
	else	
		source ${CXR_BASECONFIG}
	fi
	
	########################################
	# Try to load CAMx version specific config
	########################################
	
	cxr_main_logger -v "$FUNCNAME"  "Loading ${CXR_VERSIONCONFIG}..."
	
	if [ ! -f "$CXR_VERSIONCONFIG" ]
	then
		cxr_main_logger "$FUNCNAME" "No CAMx version specific config ($CXR_VERSIONCONFIG) found.\nThis is only needed if you want to install and run different CAMx versions in parallel"
	else
		source ${CXR_VERSIONCONFIG}
	fi
	
	########################################
	# Loading config that is run-specific
	########################################
	
	cxr_main_logger -v "$FUNCNAME"  "Loading ${CXR_EXPANDED_CONFIG} or ${CXR_CONFIG}..."
	
	# Try to load an run-specific config that was expanded
	if [ -f ${CXR_EXPANDED_CONFIG} ]
	then
		# Found an expanded configuration (priority)
		cxr_main_logger -n "$FUNCNAME" "Found an expanded configuration for this run - loading ${CXR_EXPANDED_CONFIG}"
		source ${CXR_EXPANDED_CONFIG}
		
		# This makes sure all following code looks at the right file
		CXR_CONFIG=${CXR_EXPANDED_CONFIG}
	else
		#No expanded version found - see if we find a normal one
		# Try to load the run-specific config
		# Does it exist?
		if [ ! -f ${CXR_CONFIG} ]
		then
			# If the user types CAMxRunner.sh -h we land here
			if [ ${CXR_RUN} != ${CXR_RUNNER_NAME} ]
			then
				cxr_main_die_gracefully "$FUNCNAME:$LINENO - found no configuration for the run ${CXR_RUN}. Use \n CAMxRunner.sh -C to recreate the run!"
			fi
		else	
			# Source if
			source ${CXR_CONFIG}
		fi
	fi
	
	########################################
	# Installer only: Loading special version specific config
	########################################
	
	if [ "${CXR_RUN}" == "${CXR_INSTALLER}" ]
	then
		local INSTALLER_CONF="${CXR_CONF_DIR}/${CXR_MODEL}-v${CXR_MODEL_VERSION}-installer.conf"
		
		if [ -f "${INSTALLER_CONF}" ]
		then
			source "${INSTALLER_CONF}"
		else
			cxr_main_logger -w "$FUNCNAME" "No CAMx version specific config for installer ($INSTALLER_CONF) found.\nThis is only needed if you want to install and run different CAMx versions in parallel"
		fi
	fi
	
}

################################################################################
# Function: cxr_main_extract_module_name
#
# Extracts the module name from a path, removes the leading number and the trailing extension
#
# Parameters:
# $1 - Module name with or without path
################################################################################
function cxr_main_extract_module_name() 
################################################################################
{
	if [ $# -ne 1  ]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - needs a string (the long module name) as input."
	fi	
	
	FULL_NAME=$1
	
	# We just want the filename
	MODULE_NAME=$(basename $FULL_NAME)

	# Remove extension
	MODULE_NAME=${MODULE_NAME%.sh}
	
	# Remove XX_
	INDEX_US=$(expr index "$MODULE_NAME" _)
	MODULE_NAME=${MODULE_NAME:$INDEX_US}
	
	echo "$MODULE_NAME"
	return $CXR_RET_OK
}

################################################################################
# Function: cxr_main_enumerate_modules
#	
# Shows a list of recognised modules of the given type and displays how they can
# be called individually.
#
# Parameters:
# $1 - Type of modules to be shown
################################################################################
function cxr_main_enumerate_modules
################################################################################
{
	# TODO: How do model and installer modules fit in here?
	
	MODULE_TYPE="$1"
	
	# What kind of module?
	# - MODULE_DIRECOTRIES is a list of directories that will be used to search for modules
	# - DISABLED_MODULES is a list of disabled modules of the current type
	case "$MODULE_TYPE" in
	
		"${CXR_TYPE_COMMON}" ) 
			cxr_main_die_gracefully "Common modules cannot be run this way!" ;;
			
		"${CXR_TYPE_PREPROCESS_ONCE}" ) 
			CALL="-p"
			MODULE_DIRECTORIES="$CXR_PREPROCESSOR_ONCE_INPUT_DIR" 
			DISABLED_MODULES="${CXR_DISABLED_ONCE_PREPROC:-}";;
			
		"${CXR_TYPE_PREPROCESS_DAILY}" ) 
			CALL="-i"
			MODULE_DIRECTORIES="$CXR_PREPROCESSOR_DAILY_INPUT_DIR" 
			DISABLED_MODULES="${CXR_DISABLED_DAILY_PREPROC:-}";;
			
		"${CXR_TYPE_POSTPROCESS_DAILY}" ) 
			CALL="-o"
			MODULE_DIRECTORIES="$CXR_POSTPROCESSOR_DAILY_INPUT_DIR" 
			DISABLED_MODULES="${CXR_DISABLED_DAILY_POSTPROC:-}";;
			
		"${CXR_TYPE_POSTPROCESS_ONCE}" ) 
			CALL="-f"
			MODULE_DIRECTORIES="$CXR_POSTPROCESSOR_ONCE_INPUT_DIR" 
			DISABLED_MODULES="${CXR_DISABLED_ONCE_POSTPROC:-}";;
			
		* ) 
			cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Unknown module type $MODULE_TYPE" ;;

	esac
	
	# Increase global indent level
	cxr_main_increase_log_indent
	
	# Loop through available input dirs
	for MODULE_DIRECTORY in $MODULE_DIRECTORIES
	do
	
		# How do processors look like?
		PATTERN="${MODULE_DIRECTORY}/??_*.sh"
		
		#Count modules
		NUM_MODULES=$(ls ${PATTERN} 2> /dev/null | wc -l)
		
		if [ "$NUM_MODULES" -gt 0 ]
		then
			cxr_main_logger -v "${FUNCNAME}" "Name \t\t\t\t Call"
			
			# Active Processors
			for FUNCTION_FILE in $(ls ${PATTERN} 2> /dev/null)
			do
				FILE_NAME=$(basename $FUNCTION_FILE)
				cxr_main_logger -v "${FUNCNAME}" "$FILE_NAME \t ${CXR_CALL} ${CALL}$(cxr_main_extract_module_name $FUNCTION_FILE)"    
				FOUND=true
			done
		else
			# Increase global indent level
			cxr_main_increase_log_indent
	
			cxr_main_logger "${FUNCNAME}"  "No enabled modules of type $MODULE_TYPE where found. Is this intended?\n"
			
			# Decrease global indent level
			cxr_main_decrease_log_indent
		fi
	
		if [ "$DISABLED_MODULES" ]
		then
			cxr_main_logger -w "${FUNCNAME}" "You disabled these modules in the configuration: $DISABLED_MODULES"
		fi
	
		cxr_main_logger -i "${FUNCNAME}"  "\nThese $MODULE_TYPE modules are disabled physically (if any) - to run them, remove the .${CXR_DISABLED_EXT} in the filename:\n"    
		
		# How do disabled modules look like?
		PATTERN="${MODULE_DIRECTORY}/??_*.${CXR_DISABLED_EXT}"
		
		#Count processors
		NUM_MODULES=$(ls ${PATTERN} 2> /dev/null | wc -l)
		
		if [ "$NUM_MODULES" -gt 0 ]
		then
			# Disabled Processors
			for FUNCTION_FILE in $(ls ${PATTERN} 2> /dev/null)
			do
				FILE_NAME=$(basename $FUNCTION_FILE)
				cxr_main_logger -w "${FUNCNAME}"  "$FILE_NAME"
			done
		else
			# Increase global indent level
			cxr_main_increase_log_indent
	
			cxr_main_logger -v "${FUNCNAME}"  "No disabled modules of type $MODULE_TYPE where found.\n"
			
			# Decrease global indent level
			cxr_main_decrease_log_indent
		fi
		
	done # Loop through directories
	
	# Decrease global indent level
	cxr_main_decrease_log_indent
}

################################################################################
# Function: cxr_main_enumerate_all_modules
#	
# Shows a list of all recognised modules that can
# be called individually.
#
################################################################################
function cxr_main_enumerate_all_modules
################################################################################
{
		# Increase global indent level
		cxr_main_increase_log_indent

		cxr_main_logger -v "CAMxRunner.sh" "These are the possible values for One-Time pre processing steps:\n"
		
		# XX_module_functions.sh
		cxr_main_enumerate_modules ${CXR_TYPE_PREPROCESS_ONCE}
		
		cxr_main_logger -v "CAMxRunner.sh" "These are the possible values for daily pre processing steps:\n"
		
		# XX_module_functions.sh
		cxr_main_enumerate_modules ${CXR_TYPE_PREPROCESS_DAILY}
		
		cxr_main_logger -v "CAMxRunner.sh" "These are the possible values for daily post processing steps:\n"
		
		# XX_module_functions.sh
		cxr_main_enumerate_modules ${CXR_TYPE_POSTPROCESS_DAILY}
		
		cxr_main_logger -v "CAMxRunner.sh" "These are the possible values for One-Time post processing steps:\n"
		
		# XX_module_functions.sh
		cxr_main_enumerate_modules ${CXR_TYPE_POSTPROCESS_ONCE}
		
		cxr_main_logger -i "CAMxRunner.sh" "To disable single modules, you can add the name of a module you do *not* want to run to either of the lists\n CXR_DISABLED_DAILY_PREPROC,\n CXR_DISABLED_ONCE_PREPROC,\n CXR_DISABLED_DAILY_POSTPROC or\n CXR_DISABLED_ONCE_POSTPROC\nin your configuration file. Setting any of these strings to \"${CXR_SKIP_ALL}\" disables all modules of this class."    
	
		# Decrease global indent level
		cxr_main_decrease_log_indent
}