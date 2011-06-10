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

################################################################################
# Function: main.profiler
# 
# Very primitive profiling function that simply stores a function name and
# a timestamp in a DB table
#
# Parameters:
# $1 - caller name
################################################################################
function main.profiler()
################################################################################
{
	caller=${1}
	
	/afs/psi.ch/user/o/oderbolz/CAMxRunner/bin/sqlite3-x86_64-linux-lchpcx08 /afs/psi.ch/user/o/oderbolz/CAMxRunner/state/universal/profiling.sqlite "INSERT INTO profile (caller, time) VALUES ('$caller',strftime('%s','now')/100 + strftime('%f','now'));"
}

################################################################################
# Function: main.functionExists?
# 
# This function searches the output of "set" for a function name. Returns
# true if its there, false otherwise.
# This allows a primitive kind of reflection.
#
# Parameters:
# $1 - function name
################################################################################
function  main.functionExists?()
################################################################################
{
	local fct
	local count
	
	fct=${1}
	
	count=$(set | grep "${fct} ()" | wc -l )
	
	if [[ $count -gt 0 ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: main.isNumeric?
#
# A small, relatively robust function to test if the argument is numeric.
# Obsolete, use this construct now:
# > if [[ $var =~ $CXR_PATTERN_NUMERIC ]]
#
# Parameters:
# $1 - String to test
################################################################################
function main.isNumeric?()
################################################################################
{
	# If this returns 0, this is a number
	# This is harder than it seems...
	echo "${1:-}" | egrep -x '\-?[0-9]+(\.[0-9]*)?' >/dev/null
	#                     |
	#                  Very important option: only accept matches where the whole string matches!
	
	if [[ "$?" -ne 0 ]]
	then
			echo false
	else
			echo true
	fi
}

################################################################################
# Function: main.countCores
#
# Counts the number of logical cores in a box. 
#
################################################################################
function main.countCores()
################################################################################
{
	local num_cores
	
	if [[ ! -r /proc/cpuinfo  ]]
	then
		# Probably no linux box.
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
# Function: main.increaseLogIndent
#
# Increases the intedation of the logfile. Can be used to structure 
# the log inte logical units.
# DO NOT CALL IN a $() construct (creates new process with new environment)
################################################################################
function main.increaseLogIndent()
################################################################################
{
	#Make sure it is set
	CXR_LOG_INDENT=${CXR_LOG_INDENT:-0}
	local next
	
	next=$(( $CXR_LOG_INDENT + 1 ))
	
	if [[ "$next" -gt "$CXR_MAX_LOG_INDENT"  ]]
	then
		# Maximum reached
		next=$CXR_MAX_LOG_INDENT
	fi
	
	CXR_LOG_INDENT=$next
	
}

################################################################################
# Function: main.decreaseLogIndent
#
# Decreases the intedation of the logfile. Can be used to structure 
# the log inte logical units.
# DO NOT CALL IN a $() construct (creates new process with new environment)
################################################################################
function main.decreaseLogIndent()
################################################################################
{
	#Make sure it is set
	CXR_LOG_INDENT=${CXR_LOG_INDENT:-0}
	local next
	
	next=$(( $CXR_LOG_INDENT - 1 ))
	
	if [[ $next -lt 0 ]]
	then
		next=0
	fi
	
	CXR_LOG_INDENT=$next
}

################################################################################
# Function: main.addError
#
# Adds an error to the ${CXR_INSTANCE_FILE_ERROR_STACK}. Each Message must only be one 
# line long (number of lines = number of errors)
#
# Parameters:
# $1 - Message for the error-stack file
################################################################################
function main.addError()
################################################################################
{
	local error_count
	
	# This is not beautiful, actually we need to make this sure 
	# earlier!
	if [[ "${CXR_STATE_DIR:-}" && ! -d ${CXR_STATE_DIR:-} ]]
	then
		mkdir -p ${CXR_STATE_DIR}
	fi

	# Show message to user
	main.log -a "${1:-No message}"
	
	# Make sure we have such a variable and we can write to its directory
	if [[ -d "$(dirname ${CXR_INSTANCE_FILE_ERROR_STACK:-})"  ]]
	then

		# Add text to the stack (if empty a newline is written)
		# No -e (one line per error) - the lines are counted later
		# Could be that the state db is not yet initialized
		(echo "${1:-No message}" >> "${CXR_INSTANCE_FILE_ERROR_STACK}") &> /dev/null || :
	
	fi
	
	error_count=$(main.countErrors)
	
	# Report error count and ReaLoad
	main.log -a -b "Current Error Count: $error_count"

	# Check error threshold, but only if the value of
	# of CXR_ERROR_THRESHOLD is not -1
	if [[ ( ${CXR_ERROR_THRESHOLD} != ${CXR_NO_ERROR_THRESHOLD} ) && ( ${error_count} -gt ${CXR_ERROR_THRESHOLD} )   ]]
	then
		main.dieGracefully "The number of errors occured (${error_count}) exceeds the threshold (${CXR_ERROR_THRESHOLD})"
	fi
}

################################################################################
# Function: main.countErrors
#	
# Returns the current error count 
# by counting lines in  the ${CXR_INSTANCE_FILE_ERROR_STACK}).
################################################################################
function main.countErrors()
################################################################################
{
	local number
	
	# Read from file - if not set, use /dev/null
	# Also redirect errors of cat to /dev/null
	number=$(cat ${CXR_INSTANCE_FILE_ERROR_STACK:-/dev/null} 2>/dev/null | wc -l )
	
	echo $number
}

################################################################################
# Function: main.log
#
# Writes a message to the Log-file (if defined) and STDERR.
# This function is one of the most often called functions (right after common.math.FloatOperation)
# Therefore we want to check as early as possible if we need to do logging at all.
#
# As an important SIDE EFFECT IN CASE OF AN ERROR, this function adds the error to the error list.
# Also, the current day (CXR_DATE) is added to a special logfile which shows the failed days.
#
# If a test in running (CXR_TEST_IN_PROGRESS), we try to produce a TAP-compatible message.
#
# We use STDERR instead of STDOUT in order not to interfere with returning values from functions.
# If you want a function to return log messages, rotate stderr and stdout:
# > my_function 2>&1
#
# To determine if a message is wanted, we distinguish these log-levels (set by the corresponding command line option):
# -a showed always
# -d DEBUG (DBG) [16] : These are messages a user normally does not want to see
# -v VERBOSE (VRB) [8] : Messages that may help to solve problems or understand how CAMxRunner works
# -i INFO (INF) [4] - Default : Purely informational statements (like how many grid cells there are in a simulation)
# -w WARN (WRN) [2] : Indicates that something might be wrong
# -e ERROR (ERR) [1] : Something has failed (will always be shown, presents call-stack)
#
# The user can manipulate Separate Loglevels for screen (CXR_LOG_LEVEL_SCREEN) and file (CXR_LOG_LEVEL_FILE),
# Usually the screen shows less and the file shows more.
# If the loglevel is greater or equal to the level of a message, the message is shown
#
# The function automatically indents its messages according to $CXR_LOG_INDENT.
# It also infers its caller from the FUNCNAME array.
#
# For efficiency reasons (it's inefficient already...), call the function only once for multi-line messages
# use \n to separate lines.
# The use of continuation characters like
#> main.log  "This is \
#>                              a message"
# is not recommended because the whitespace before the second part of the string will 
# be output "as is".
#
# If not given, the log-level of a message is "CXR_LOG_LEVEL_INF"
# 
# Parameters:
# [-a] - shown always, no prefix, cannot be turned off
# [-v] - verbose message
# [-w] - warning
# [-e] - error (always shown)
# [-b] - surround with small box of stars
# [-B] - surround with large box of stars
# [-H] - surround with huge box of stars
# $1 - Message
################################################################################	
function main.log()
################################################################################
{
	local prefix
	local pre_stars
	local post_stars
	
	prefix=""
	pre_stars=""
	post_stars=""
	
	# A standard message is an info
	local log_level
	
	# A line of stars
	local stars
	
	local blank_message
	local stderr_message
	local message
	local call_stack
	local iFct
	local maxNests
	local do_log
	local prefix
	local last
	
	log_level=${CXR_LOG_LEVEL_INF}
	do_log=${CXR_DO_FILE_LOGGING:-true}
	
	stars=""
	
	while getopts ":vatwebBH" opt
	do
		case $opt in
		v) log_level=$CXR_LOG_LEVEL_VRB ; prefix=""   ;;
		a) log_level=$CXR_LOG_LEVEL_ALW ; prefix=""   ;;
		w) log_level=$CXR_LOG_LEVEL_WRN ; prefix=${CXR_LOG_PREFIX_WRN} ;;
		e) log_level=$CXR_LOG_LEVEL_ERR ; prefix=${CXR_LOG_PREFIX_ERR} ;;
		b) stars="${CXR_BOX_SMALL}";;
		B) stars="${CXR_BOX_LARGE}";;
		H) stars="${CXR_BOX_HUGE}";;
		esac
	done
	
	# This allows to read non-named command line options
	shift $(($OPTIND - 1))
	
	# Make getopts ready again
	unset OPTSTRING
	unset OPTIND
	
	# Triage - if we do not want to see this message, we return
	if [[ $log_level -ne $CXR_LOG_LEVEL_ERR && \
				$log_level -ne $CXR_LOG_LEVEL_ALW && \
	     ("${CXR_LOG_LEVEL_SCREEN:-${CXR_LOG_LEVEL_INF}}" -lt "${log_level}") && \
	     ("${CXR_LOG_LEVEL_FILE:-${CXR_LOG_LEVEL_INF}}" -lt "${log_level}") && \
	     ("${CXR_LOG_LEVEL_EXTERNAL:-${CXR_LOG_LEVEL_ERR}}" -lt "${log_level}") ]]
	then
		return $CXR_RET_OK
	fi
	
	# Add modifier if test is in progress 
	if [[ ${CXR_TEST_IN_PROGRESS:-false} == true ]]
	then
		prefix="#${prefix}"
		
		if [[ "$stars" ]]
		then
			pre_stars="${stars}\n"
			post_stars="\n${stars}\n"
		fi
	fi
	
	blank_message="${prefix}${1:-}"

	### In case of error, write current day to failed days file
	### and increase error count
	if [[ "${log_level}" -eq "${CXR_LOG_LEVEL_ERR}" ]]
	then
		# Could be that the state db is not yet initialized
		if [[ -d "$(dirname ${CXR_INSTANCE_FILE_FAILED_DAYS:-})" ]]
		then
			(echo "${CXR_DATE:-date_not_defined}" >> "${CXR_INSTANCE_FILE_FAILED_DAYS}" ) &> /dev/null || :
		fi
		
		# max index of the call-stack
		maxNests=$(( ${#FUNCNAME[@]} - 1 ))
		
		# Create callstack in case of error
		if [[ ${maxNests} -ge 1 ]]
		then
			call_stack="Call Stack:"
			for iFct in $(seq 1 ${maxNests})
			do
				# Index of last fct
				last=$(( $iFct - 1 ))
				call_stack="${call_stack} ${FUNCNAME[$iFct]}:${BASH_LINENO[$last]}"
			done
		fi # Callstack available?
		
		main.addError "${blank_message} - ${call_stack:-}"
		
	else
		# STDERR All other cases

		if [[ "${CXR_LOG_LEVEL_SCREEN:-${CXR_LOG_LEVEL_INF}}" -ge "${log_level}" ]]
		then
			# If set, we display the worker ID on screen
			if [[ "$CXR_MAX_PARALLEL_PROCS" -gt 1 && "${CXR_WORKER_ID:-}" ]]
			then
				stderr_message="${CXR_WORKER_ID}: $blank_message"
			else
				stderr_message="$blank_message"
			fi
			# adding stars (might be empty)
			stderr_message="${pre_stars}${stderr_message}${post_stars}"

			# Write a unformatted copy to sterr. 
			echo -e "${stderr_message}" 1>&2
		fi
	fi

	# file
	# Prepare and write to file only if we have a defined target
	# and this was not suppressed in the call
	if [[ "$do_log" == true && -n "${CXR_LOG:-}" && "${CXR_LOG_LEVEL_FILE:-${CXR_LOG_LEVEL_INF}}" -ge "${log_level}" ]]
	then
		# We want and can log to a file

		# Add date of form Aug 26 15:44:45
		# The printf prints CXR_LOG_INDENT tabs
		prefix="$(date +"%b %d %H:%M:%S"):$(printf '\t%.0s' {1..$CXR_LOG_INDENT})"
	
		# Put prefix in front of the Blank message.
		
		message="${prefix}${blank_message}"
	
		# Now, all the newline characters need a placeholder prefix
		# The prefix w/o tabs is 16 Bytes
		# \\n is what we are looking for, // request ALL \n to be replaced
		placeholder="                $(printf '\t%.0s' {1..$CXR_LOG_INDENT})"
		message="${message//\\n/\n${placeholder}}"
		
		# adding stars (might be empty)
		message="${pre_stars}${message}${post_stars}"
		
		# Writing to file, ignoring errors
		(echo -e "$message" >> ${CXR_LOG}) &> /dev/null || :
	fi
	
	# The external channels (sendMessage decides who needs to get the message)
	# only -e and -w may  even go there
	if [[  "${log_level}" -gt "${CXR_LOG_LEVEL_ALW}" && "${log_level}" -lt "${CXR_LOG_LEVEL_INF}" && "${CXR_LOG_LEVEL_EXTERNAL:-${CXR_LOG_LEVEL_ERR}}" -ge "${log_level}" ]]
	then
		# The sending is done in the background
		main.sendMessage "Log" "${blank_message} - ${call_stack}" &
	fi
	
	return $CXR_RET_OK
}

################################################################################
# Function: main.sendMessage
#	
# Sends out a notification to $CXR_MAILADDR, 
# $CXR_SMSADDR and to a Twitter account if needed
#
# Parameters:
# $1 - Subject
# $2 - Body
################################################################################
function main.sendMessage()
################################################################################
{
	local adresses
	local address
	
	
	#Add SMS Adress if needed
	if [[ ${CXR_SEND_SMS} == true ]]
	then
		adresses="$adresses $CXR_SMSADDR"
	fi
	
	# Add Email if needed
	if [[ ${CXR_SEND_EMAIL} == true  ]]
	then
		adresses="$adresses $CXR_MAILADDR"
	fi
	
	for address in $adresses
	do
		if [[ "$address" ]]
		then
			# send it if we have an adress
			echo -e $2 | mail -s "$1" $address
		fi
	done
	
	# Twitter
	if [[ ${CXR_SEND_TWITTER} == true ]]
	then
		# Get the data from .twitterrc
		if [[ ! -r ~/.twitterrc  ]]
		then
			# no .twitterrc
			main.log -e  "To use Twitter, you need to create a ~/.twitterrc file, in there set ACCOUNT= and PASS=."
		else
			# .twitterrc there
			source ~/.twitterrc
			
			# Communicate w. Twitter (easy using curl)
			# Send any output to /dev/null
			curl --basic --user "${ACCOUNT:-}:${PASS:-}" --data-ascii "status=`perl -MURI::Escape -e \
			"print uri_escape('$(echo -e ${1}:${2})');"`"  http://twitter.com/statuses/update.json &> /dev/null
			
		fi
	fi
	
}

################################################################################
# Function: main.doCleanup
#
# Cleans the state DB/Locks etc and kills all processes (master and workers)
#
################################################################################
function main.doCleanup()
################################################################################
{
	# Remove any traps once called
	trap - INT TERM EXIT
	
	local error_count
	local errors
	local dummy_file
	local arrKeys
	local keyString
	local currentLogFile
	
	#Relax
	set +e
	set +v
	set +x
	
	# Delete entries in instance_tasks, if possible
	if [[ -s "$CXR_STATE_DB_FILE" && $(main.functionExists? common.db.change) == true ]]
	then
		main.log -a "Deleting this instances task list..."
		common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "DELETE FROM instance_tasks WHERE instance='$CXR_INSTANCE';"
	fi
	
	# Pseudo-Singleton
	if [[ "${CXR_CLEANUP_IN_PROGRESS:-false}" == true ]]
	then
		return $CXR_RET_OK
	else
		CXR_CLEANUP_IN_PROGRESS=true
	fi
	
	main.log -a -B "Cleaning up $CXR_RUN and Terminating processes..."
	
	################################################################################
	# Error report - do in any Non-hollow case
	################################################################################	
	
	if [[ "$CXR_HOLLOW" == false ]]
	then
		
		# All the non-hollow stuff goes here
		error_count=$(main.countErrors)
		
		main.log -a -B "Error Report (may be repeated)"
		
		for currentLogFile in ${CXR_LOGS[@]}
		do
			errors="$(grep "${CXR_LOG_PREFIX_ERR}" $currentLogFile)"
			
			if [[ "$errors" ]]
			then
				echo "Error list from ${currentLogFile}:" 1>&2
				echo "$errors" 1>&2
			fi
		done
		
		if [[ "${error_count:-0}" -ne 0 ]]
		then
			main.log -a "The system detected at least $error_count errors, fix them!\n\nHere is the error stack:\n"
			
			if [[ "${CXR_LOG:-}" ]]
			then
				# Write to logfile
				cat "$CXR_INSTANCE_FILE_ERROR_STACK" | tee -a ${CXR_LOG}
				
				if [[ -f "$CXR_INSTANCE_FILE_FAILED_DAYS" ]]
				then
					main.log -a  "In these days, errors where detected:"
					# This file might contain dupes
					cat "$CXR_INSTANCE_FILE_FAILED_DAYS" | sort | uniq | tee -a ${CXR_LOG}
				fi
				
			else
				# There is no log
				cat "$CXR_INSTANCE_FILE_ERROR_STACK"
				
				if [[ -f "$CXR_INSTANCE_FILE_FAILED_DAYS" ]]
				then
					main.log -a  "In these days, errors where detected:"
					cat "$CXR_INSTANCE_FILE_FAILED_DAYS" | sort | uniq 
				fi
			fi
			
		else	
			main.log -a "No errors where detected in this subprocess - exit status is $CXR_STATUS"
		fi
		main.log -a "**********************************************************"
	
		########################################
		# Do cleanup stuff - do this always
		########################################
		
		if [[ "$CXR_DRY" == true ]]
		then
			main.log -a "Dryrun: removing the dummy files..."
			
			# common.hash.getKeys returns a newline delimited string
			oIFS="$IFS"
			IFS='
'
			# Clean files away
			for dummy_file in $(common.hash.getKeys $CXR_INSTANCE_HASH_DUMMY_FILES $CXR_LEVEL_INSTANCE)
			do
				# Reset IFS
				IFS="$oIFS"

				main.log -v "Deleting $dummy_file"
				
				rm -f "$dummy_file" &>/dev/null
				
				# Remove from hash
				common.hash.delete $CXR_INSTANCE_HASH_DUMMY_FILES $CXR_LEVEL_INSTANCE "$dummy_file"
			done
			
			IFS="$oIFS"
			
		fi # Dry?
		
	fi # Hollow?
	
	# Remove all tempfiles and redirect (function might not exist)
	common.runner.removeTempFiles 2> /dev/null
	
	main.log -a "Deleting this instances internal files..."
	rm -rf ${CXR_INSTANCE_DIR:-/dev/null} &> /dev/null

	if [[ "$CXR_STATUS" == "$CXR_STATUS_SUCCESS" ]]
	then
		# All went well
		
		# In a dryrun, always print this if finished OK
		if [[ "$CXR_DRY" == true  ]]
		then
		
			main.log -a -H  "Before you can run the simulation, you must clean the state database\nbecause a dryrun makes the same entries as a normal run.\nDo this using this command:\n \t ${CXR_CALL} -c \nIf you have adjusted the configuration in ${CXR_CONFIG}\nand are happy with the log, run the simulation like this (assuming an AFS/Kerberos environment):\n"     
			
			# Increase global indent level
			main.increaseLogIndent
  	
			main.log -a -b  "\t \$ k5run -b ${0}"
			
			# Decrease global indent level
			main.decreaseLogIndent
		
		else
			if [[ "$CXR_HOLLOW" == false ]]
			then
				# print some infos if we are not hollow 
				main.log -a "Ending ${CXR_RUN}, its now $(date)"
				main.sendMessage "${CXR_RUN} finished on $CXR_MACHINE" "That is it, folks."
			fi
		fi
	fi
	
	# Change Back to rundir
	cd ${CXR_RUN_DIR} 
	
	if [[ "${CXR_HOLLOW}" == false ]]
	then
		# OK, remove the workers now
		common.task.removeAllWorkers
	fi
	
	# What we do is kill the CAMxRunner processes (whole process group by giving negative PID)
	# See <http://stackoverflow.com/questions/392022/best-way-to-kill-all-child-processes>
	# Any child process can do this (which is good), however this means that all childs will re-execute this handler
	# because in their processes, the trap is still in place.
	kill -- -${CXR_PID}
	
}

# Set the trap (needs function first)
trap "main.doCleanup; exit" INT TERM EXIT

################################################################################
# Function: main.dieGracefully
#
# This is the function to call upon serious errors.
# It decides if it needs to do cleanup or if the controller does it.
# In any case, all CONTINUE files are deleted.
# Prints a message incl. call stack and performs cleanup.
# 
# Parameters:
# $1 - Last words
################################################################################
function main.dieGracefully()
################################################################################
{
	local iFct
	local maxNests
	local last
	
	# Pseudo Singleton
	if [[ -e "$CXR_GLOBAL_ABNORMAL_TERMINATION_FILE" ]]
	then
		return $CXR_RET_OK
	else
		# touching this file flags this as abnormal termination
		touch "$CXR_GLOBAL_ABNORMAL_TERMINATION_FILE"
	fi
	
	# Stop this run on all machines
	# Delete any .CONTINUE FILES
	common.state.deleteAllContinueFiles
	
	# Delete everything from the instance_tasks table
	if [[ -e "$CXR_STATE_DB_FILE" && $(main.functionExists? common.db.change) == true ]]
	then
		common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "DELETE FROM instance_tasks;"
	fi
	
	main.log -a "### CAMxRunner Crashes because of this reason:############"
	# This function also echoes the message
	main.addError "${1:-Unknown error.}"
	
	# max index of the call-stack
	maxNests=$(( ${#FUNCNAME[@]} - 1 ))
	
	if [[ ${maxNests} -ge 1 ]]
	then
		main.log -a "Call Stack:"
		
		for iFct in $(seq 1 ${maxNests})
		do
			last=$(( $iFct - 1 ))
			echo ${FUNCNAME[$iFct]}:${BASH_LINENO[$last]} 1>&2
		done
	fi # Callstack available?

	main.log -a  "Note that it may take a while for all processes to stop.\n##########################################################"
	
	# Cleanup kills the processes
	main.doCleanup
}

################################################################################
# Function: main.getBinaryName
#
# Returns the full path of a binary name other than the model (model name comes from <get_model_exec>)
#
# In CAMxRunner, *always* use this function instead of "which"
#
# The name depends on:
# - The Machine Architecture
# - (The machine name)
# 
# We try to find the most specialised binary.
# Notifies logger if no suitable executable could be found.
# We cannot use the has functions for caching, we use another, rather primitive approach.
#
# Such a name looks like this:
#
# > ${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/$1-${HOSTTYPE}[-$(uname -n)] (e. g. ~/CAMxRunner/bin/4.51/airascii-x86_64-linux)
#
# Example:
# > CXR_BIN2ASC_EXEC=$(main.getBinaryName bin2asc)
#
#
# Parameters:
# $1 - name (without path!) of the binary in question
# [$2] - allow_empty, an optional boolean, if true, we allow a binary to be missing (default: false)
################################################################################
function main.getBinaryName()
################################################################################
{
	if [[ $# -lt 1 || $# -gt 2 ]]
	then
		main.dieGracefully "Programming error: needs at least the name of a binary as input, optional parameter allow_empty, got $*"
	fi
	
	local exec_name
	local allow_empty
	
	exec_name="${1}"
	allow_empty="${2:-false}"
	
	local which_output
	local alias_output
	local current_special
	local current_general
	
	local directories
	local dir
	
	# We go through the whole hierarchy from special to general	
	directories="${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION} ${CXR_BIN_DIR}/${CXR_MODEL} ${CXR_BIN_DIR}"

	for dir in $directories
	do
		#Determine possible names
		current_general="${dir}/${exec_name}-${HOSTTYPE}"
		current_special="${current_general}-${CXR_MACHINE}"
		
		# In each dir, we check the machine-specific first, then the general
		if [[ -x "${current_special}" ]]
		then
			# Machine dependent exists
			main.log -v "Binary ${exec_name} is actually called ${current_special}"
			echo "${current_special}"
			return $CXR_RET_OK
		elif [[ -x "${current_general}" ]]
		then
			# general exists
			main.log -v "Binary ${exec_name} is actually called ${current_general}"
			echo "${current_general}"
			return $CXR_RET_OK
		fi
	done # Loop through directories.
	
	# If we arrive here, nothing was found yet.
	# Check alias and which
	
	# We must allow errors
	set +e
		alias_output="$(alias "${exec_name}" 2>/dev/null)"
	
		which_output="$(which "${exec_name}" 2>/dev/null)"
	set -e
	
	# Set output to "" if which failed
	if [[ $? -eq 0 ]]
	then
		# Ok, found it
		if [[ "$alias_output" ]]
		then
			main.log -v "${exec_name} is an alias to ${alias_output}"
		fi
		
		main.log -v "Binary ${exec_name} is actually called ${which_output}"
		echo "${which_output}"
	else
		# Still no match
		if [[ "$allow_empty" == true ]]
		then
			# optional param is true,
			# We do not care and return the empty string
			main.log -v "Binary $exec_name not found"
			echo "" 
		else
			# NOK-just issue a warning (if we are compiling e.g.)
			main.log -w "Could not find a suitable executable for binary $exec_name. The which command also did not find anything.\nRecompile this components using\n\t\$ CAMxRunner.sh -I"
		fi
	fi

	return $CXR_RET_OK
}

################################################################################
# Function: main.getLibraryName
#
# Returns the full path of a library name.
#
# The name depends on:
# - The Machine Architecture
# - (The machine name)
# 
# We try to find the most specialised library.
# Notifies logger if no suitable executable could be found.
#
# Such a name looks like this:
#
# > CXR_LIB_DIR/sqlite/$HOSTTYPE/libsqlitefunctions.so[-$(uname -n)] (e. g. ~/CAMxRunner/lib/sqlite3/x86_64-linux)
#
# Example:
# > CXR_SQLITE_LIBFUNCTIONS="$(main.getLibraryName libsqlitefunctions.so)"
#
#
# Parameters:
# $1 - name (without path, with .so if needed) of the binary in question
# $2 - name of corresponding executable (can also be a model name)
# [$3] - version of tho corresponding executable, if any (can also be a model version) 
# [$2] - allow_empty, an optional boolean, if true, we allow a binary to be missing (default: false)
################################################################################
function main.getLibraryName()
################################################################################
{
	if [[ $# -lt 1 || $# -gt 4 ]]
	then
		main.dieGracefully "Programming error: needs at least the name of a binary and the corresponding executable as input, optional parameter version and allow_empty, got $*"
	fi
	
	local lib_name
	local exec_name
	local version
	local allow_empty
	
	lib_name="${1}"
	exec_name="${2}"
	version="${3:-}"
	allow_empty="${2:-false}"
	
	local current_special
	local current_general
	
	local directories
	local dir
	
	# Define the directories to search
	# from special to general
	if [[ "$version" ]]
	then
		directories="${CXR_LIB_DIR}/${exec_name}/${version}/${HOSTTYPE} ${CXR_LIB_DIR}/${exec_name}/${HOSTTYPE} ${CXR_LIB_DIR}"
	else
		directories="${CXR_LIB_DIR}/${exec_name}/${HOSTTYPE} ${CXR_LIB_DIR}/${exec_name} ${CXR_LIB_DIR}"
	fi
	
	for dir in $directories
	do
		#Determine possible names
		current_general="${dir}/${lib_name}"
		current_special="${lib_name}-${CXR_MACHINE}"
		
		# In each dir, we check the machine-specific first, then the general
		if [[ -x "${current_special}" ]]
		then
			# Machine dependent exists
			main.log -v "Library ${lib_name} is actually called ${current_special}"
			echo "${current_special}"
			return $CXR_RET_OK
		elif [[ -x "${current_general}" ]]
		then
			# general exists
			main.log -v "Library ${lib_name} is actually called ${current_general}"
			echo "${current_general}"
			return $CXR_RET_OK
		fi
	done # Loop through directories.
	
	# no match yet!
	if [[ "$allow_empty" == true ]]
	then
		# optional param is true,
		# We do not care and return the empty string
		main.log -v "Library ${lib_name} not found"
		echo "" 
	else
		# NOK-just issue a warning (if we are compiling e.g.)
		main.log -w "Could not find library ${lib_name}!\nRecompile this components using\n\t\$ CAMxRunner.sh -I"
	fi

	return $CXR_RET_OK
}

################################################################################
# Function: main.setModelAndVersion
#
# Extracts the Version of CAMx from the link name.
# We need this in order to load the common functions and defaults.inc, 
# so no reference to these allowed here!
#
# Parameters:
# $1 - Run name of the form Model-vVersion-Whatever (e. g. CAMx-v4.51-ENVIRON_testcase)
################################################################################
function main.setModelAndVersion()
################################################################################
{
	local model
	local version

	# We expect a name like CAMx-v4.42-bafu3_winter07.run3_lcsl5a
	# So we split using "-"
	
	if [[ $1 == CAMxRunner.sh || $1 == base || $1 == installer ]]
	then
		# If the user does CAMxRunner.sh -h this must work too!
		version=4.51
		model=CAMx
	else
		# Split it
		model=$(echo "$1" | cut -d- -f1)
		version=$(echo "$1" | cut -d- -f2)
		
		# Remove v
		version="${version#v}"
	fi
	
	#######################################
	# Basic variables
	#######################################
	CXR_MODEL_VERSION=$version
	CXR_MODEL=$model
}

################################################################################
# Function: main.countDelimitedElements
#
# Counts the number of elements in a delimited string.
# Caution: Does not yet work well with newline as delimiter 
# NUM_MODULE_TYPES=$(main.countDelimitedElements "$MODULE_TYPES" "$'\n'")
# Yields a number that is off by one! 
# Use $(echo $String | wc -l) or something similar in these cases
#
# Parameters:
# $1 - String to parse
# [$2] - Optional Delimiter (Default $CXR_DELIMITER)
################################################################################
function main.countDelimitedElements ()
################################################################################
{
	local string
	local delimiter
	local array
	
	if [[ $# -ne 2  ]]
	then
		string="$1"
		delimiter="$CXR_DELIMITER"
	else
		string="$1"
		delimiter="$2"
	fi
	
	# Save old IFS
	oIFS="$IFS"

	IFS="$delimiter"
	
	# Suck line into array
	array=($string)
	
	# Reset IFS
	IFS="$oIFS"

	# Echo the count
	echo ${#array[@]}
}

################################################################################
# Function: main.getRevision
#
# Extracts the svn version of a file. 
# 
# Returns:
# If the revision is not set or the file does not exist, returns 0
#
# otherwise, return revision found
#
# Parameters:	
# $1 - Full filename
################################################################################
function main.getRevision()
################################################################################
{
	# Define & Initialize local vars
	local filename
	local version_string
	local revision
	
	if [[ $# -ne 1  ]]
	then
		# No filename supplied
		filename=/dev/null
	else
		filename=$1
	fi
	
	if [[ ! -s "$filename"  ]]
	then
		# File inexistent or empty
		revision=0
	else
		# File exists, get first version string
		# We want only the version string, nothing afterwards
		version_string="$(grep -o '\$Id.*\$' ${filename})"
		
		# We expect 7 fields:
		# "$Id$"
		
		if [[ $(main.countDelimitedElements "${version_string}" " ") -eq 7  ]]
		then
			# Get the lines with $Id, cut away $, get the 3rd field and make sure we get only one line
			revision=$( echo "${version_string}" | cut -d $ -f 2 | cut -d" " -f3 | head -n1)
		else
			# We do not have 6 fields, cannot garantee anything!
			main.log -w  "Version string of file $filename is broken. Fix using svn!"
			revision=0
		fi
		
		# Correct any garbage
		if [[ $(main.isNumeric? "$revision") == false   ]]
		then
			revision=0
		fi
	fi
	
	echo $revision
}

################################################################################
# Function: main.getMD5
#	
# Returns an MD5 Hash of a file. Returns the empty string if file does not exist.
#
# Parameters:
# $1 - file to Hash
################################################################################
function main.getMD5() 
################################################################################
{
		if [[ $# -ne 1 ]]
		then
			main.log -e "Programming error: no filename passed!"
		fi
		
		local file
		file="$1"
		
		if [[ -r "${file}" ]]
		then
			"${CXR_MD5_EXEC}" "${file}" | cut -d" " -f1
		else
			# Return the empty string
			main.log -e "File $file not readable."
			echo ""
		fi
}

################################################################################
# Function: main.isSubstringPresent?
#
# Returns true if a substring needle was found within haystack, false otherwise.
#
# Parameters:
# $1 - haystack, the string in which we search
# $2 - needle, the substring to be found
################################################################################
function main.isSubstringPresent?() 
################################################################################
{
	local haystack
	local needle
	local found
	
	haystack=$1
	needle=$2
	
	found=$(expr match " $haystack" ".*$needle.*")  || :
	# For safety, here        ^ is a space, so that things never start at 0
	
	if [[ $found -gt 0 ]]
	then
		main.log -v "Substring $needle matches (partially) with $haystack"
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: main.setVersionDepVariables
#
# Sets the variables that depend on the chosen version. For hen&egg reasons, this is called
# directly by CAMxRunner, as well as by <main.readConfig>
#
# Parameters:
# $1 - The name of the run to use
# $2 - The Modelname to use
# $3 - The Model Version to use
# $4 - The Rundir to use
################################################################################
function main.setVersionDepVariables()
################################################################################
{
	export CXR_RUN="${1}"
	
	CXR_MODEL="${2}"
	
	# Model Version
	CXR_MODEL_VERSION="${3}"
		
	CXR_RUN_DIR="${4}"
	
	main.log -v -b "Setting directory variables specific for ${CXR_MODEL} ${CXR_MODEL_VERSION}..."
	
	#Base path to find the CAMx executable
	CXR_MODEL_BIN_DIR=${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}
	
	# These are Model specific common modules, CAMx is the default model
	CXR_COMMON_MODEL_INPUT_DIR=${CXR_COMMON_INPUT_DIR}/${CXR_MODEL}
	
	# And these Version specific common modules
	CXR_COMMON_VERSION_INPUT_DIR=${CXR_COMMON_MODEL_INPUT_DIR}/${CXR_MODEL_VERSION}
	
	# Directories where to find installer modules
	
	# These are Model specific ones, CAMx is the default model
	CXR_INSTALLER_MODEL_INPUT_DIR=${CXR_INSTALLER_INPUT_DIR}/${CXR_MODEL}
	
	# And these Version specific
	CXR_INSTALLER_VERSION_INPUT_DIR=${CXR_INSTALLER_MODEL_INPUT_DIR}/${CXR_MODEL_VERSION}
	
	# Directories where to find processors
	
	#Pre- Processors
	# These are run for each simulation day
	CXR_PREPROCESSOR_DAILY_INPUT_DIR=${CXR_MODULES_DIR}/preproc/${CXR_MODEL}/${CXR_MODEL_VERSION}/daily
	
	#These are run before the simulation starts
	CXR_PREPROCESSOR_ONCE_INPUT_DIR=${CXR_MODULES_DIR}/preproc/${CXR_MODEL}/${CXR_MODEL_VERSION}/once
	
	# Post- Processors
	# These are run for each simulation day
	CXR_POSTPROCESSOR_DAILY_INPUT_DIR=${CXR_MODULES_DIR}/postproc/${CXR_MODEL}/${CXR_MODEL_VERSION}/daily
	
	#These are run once the whole simulation has finished
	CXR_POSTPROCESSOR_ONCE_INPUT_DIR=${CXR_MODULES_DIR}/postproc/${CXR_MODEL}/${CXR_MODEL_VERSION}/once
	
	# Model
	CXR_MODEL_INPUT_DIR=${CXR_MODULES_DIR}/model/${CXR_MODEL}/${CXR_MODEL_VERSION}
}

################################################################################
# Function: main.readConfig
#
# Determine relevant config files and load them, also report their names and versions.
# There are currently these:
# - site.conf
# - base.conf
# - Model version specific
# - run-specific (potentially expanded)
#
# The run names 'installer' and 'test' are treated separately, there we load
# - site.conf
# - base.conf
# - Model version specific
# - installer.conf
# - ${CXR_MODEL}-${CXR_MODEL_VERSION}-installer.conf resp. ${CXR_MODEL}-${CXR_MODEL_VERSION}-test.conf (if available)
#
# Also here, we set all variables that depend on model & version
# 
#
# Parameters:
# $1 - The name of the run to use
# $2 - The Modelname to use
# $3 - The Model Version to use
# $4 - The Rundir to use
################################################################################
function main.readConfig()
################################################################################
{
	# In this variable, we store the MD5 hashes of all config files loaded so far
	# it is for example possible, that a higher level config file is in reality a link
	# to a lower level one...
	# We do it only locally because sometimes we want to re-load config data.
	local loaded
	
	local iFile
	local file
	local rev
	local md5
	
	export CXR_RUN="${1}"
	
	CXR_MODEL="${2}"
	
	# Model Version
	CXR_MODEL_VERSION="${3}"
		
	CXR_RUN_DIR="${4}"
	
	if [[ -z "$CXR_MODEL_VERSION" ]]
	then
		main.dieGracefully "Could not determine Model Version when loading config!"
	fi
	
	if [[ -z "$CXR_RUN_DIR" ]]
	then
		main.dieGracefully "Could not determine run dir when loading config!"
	fi
	
	# Change to run dir
	cd "$CXR_RUN_DIR" || main.dieGracefully "Could not change to $CXR_RUN_DIR"

	# Now redefine, we need an absolute path
	CXR_RUN_DIR="$(pwd)"
	
	########################################
	# Setting important variables
	########################################

	main.setVersionDepVariables "${CXR_RUN}" "${CXR_MODEL}" "${CXR_MODEL_VERSION}" "${CXR_RUN_DIR}"
	
	main.log "Loading configuration for $CXR_RUN, $CXR_MODEL Version $CXR_MODEL_VERSION..."
	
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
	# Revisions - these are available as global vars
	########################################
	
	CXR_SITECONFIG_REV="$(main.getRevision $CXR_SITECONFIG)"
	CXR_BASECONFIG_REV="$(main.getRevision $CXR_BASECONFIG)"
	CXR_VERSIONCONFIG_REV="$(main.getRevision $CXR_VERSIONCONFIG)"
	CXR_CONFIG_REV="$(main.getRevision $CXR_CONFIG)"
	CXR_EXPANDED_CONFIG_REV="$(main.getRevision $CXR_EXPANDED_CONFIG)"
	
	# If there is an expanded specific config, use it
	if [[ -f "${CXR_EXPANDED_CONFIG}" ]]
	then
		CXR_CONFIG=$CXR_EXPANDED_CONFIG
		CXR_CONFIG_REV=$CXR_EXPANDED_CONFIG_REV
	fi
	
	########################################
	# Define Hierarchy
	########################################
	
	if [[ "${CXR_RUN}" == "${CXR_INSTALLER}" ]]
	then
		# Installer needs an additional file
		CXR_INSTALLER_CONF="${CXR_CONF_DIR}/${CXR_MODEL}-v${CXR_MODEL_VERSION}-${CXR_INSTALLER}.conf"
		CXR_INSTALLER_CONF_REV=$(main.getRevision $CXR_INSTALLER_CONF)
		
		CXR_CONF_HIERARCHY=($CXR_SITECONFIG $CXR_BASECONFIG $CXR_VERSIONCONFIG $CXR_CONFIG $CXR_INSTALLER_CONF)
		CXR_CONF_HIERARCHY_REV=($CXR_SITECONFIG_REV $CXR_BASECONFIG_REV $CXR_VERSIONCONFIG_REV $CXR_CONFIG_REV $CXR_INSTALLER_CONF_REV)
	elif [[ "${CXR_EXTERNAL}" == true ]]
	then
		# External also needs an additional file
		CXR_EXTERNAL_CONF="${CXR_CONF_DIR}/external.conf"
		CXR_EXTERNAL_CONF_REV=$(main.getRevision $CXR_EXTERNAL_CONF)
		
		CXR_CONF_HIERARCHY=($CXR_SITECONFIG $CXR_BASECONFIG $CXR_VERSIONCONFIG $CXR_CONFIG $CXR_EXTERNAL_CONF)
		CXR_CONF_HIERARCHY_REV=($CXR_SITECONFIG_REV $CXR_BASECONFIG_REV $CXR_VERSIONCONFIG_REV $CXR_CONFIG_REV $CXR_EXTERNAL_CONF_REV)
	else
		CXR_CONF_HIERARCHY=($CXR_SITECONFIG $CXR_BASECONFIG $CXR_VERSIONCONFIG $CXR_CONFIG)
		CXR_CONF_HIERARCHY_REV=($CXR_SITECONFIG_REV $CXR_BASECONFIG_REV $CXR_VERSIONCONFIG_REV $CXR_CONFIG_REV)
	fi
	
	########################################
	# Load the whole hierarchy
	########################################
	
	for iFile in $(seq 0 $(( ${#CXR_CONF_HIERARCHY[@]} - 1 )) )
	do
		file=${CXR_CONF_HIERARCHY[$iFile]}
		rev=${CXR_CONF_HIERARCHY_REV[$iFile]}
		
		if [[ ! -e "$file" ]]
		then
			if [[ $iFile -eq 0 ]]
			then
				# First file (site) may be missing
				main.log "I noticed that you have no site-specific configuration file (${CXR_SITECONFIG}) - this is not needed, but you might want to put site-specific stuff there."
				continue
			else
				# all other files must exist
				main.dieGracefully "The mandatory configuration file ${file} is not available - exiting"
			fi
		fi # File non-existent?
		
		md5="$(main.getMD5 "$file")"
		
		# Was this already loaded?
		if [[ $(main.isSubstringPresent? "$loaded" "$md5") == false ]]
		then
			main.log -a  "Loading ${file} rev ${rev}. MD5: ${md5}"
		
			source "${file}"
			
			# Add to loaded list
			loaded="${loaded} ${md5}"
		else
			main.log -v "A file with the same MD5 hash as ${file} ($md5) was already loaded!"
		fi

	done
}

################################################################################
# Function: main.getModuleName
#
# Extracts the module name from a path, removes the leading number (if any),
# the variant (if any) and the trailing extension
#
# Parameters:
# $1 - Module name with or without path
################################################################################
function main.getModuleName()
################################################################################
{
	local full_name
	local module
	
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "needs a string (the long module name with or without path) as input."
	fi	
	
	full_name=${1}
	
	# We just want the filename (without .sh)
	module=$(basename $full_name .sh)

	# Remove XX_ if present
	module="$(expr match "$module" '[0-9]\{0,\}_\{0,1\}\([_.a-zA-Z]\{1,\}\)')"  || :
	
	if [[ ! "$module" ]]
	then
		main.dieGracefully "Could not determine module name of $1"
	else
		# Now, we remove the variant by returning all before the first dot
		module=$(echo "$module" | cut -d'.' -f1)
	fi
	
	echo "$module"
	return $CXR_RET_OK
}

################################################################################
# Function: main.getModuleVariant
#
# Extracts the module variant from a path, removes the leading number (if any),
# the variant (if any) and the trailing extension.
# If a module is called 12_my_module.psi.ch.sh, the variant is psi.ch.
# For modules with no specific variant, the return value is the empty string.
#
# Parameters:
# $1 - Module name with or without path
################################################################################
function main.getModuleVariant()
################################################################################
{
	local full_name
	local variant
	
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "needs a string (the long module name with or without path) as input."
	fi	
	
	full_name=${1}
	
	# We just want the filename (without sh)
	module=$(basename $full_name sh)

	# Remove XX_ if present
	module="$(expr match "$module" '[0-9]\{0,\}_\{0,1\}\([_.a-zA-Z]\{1,\}\)')"  || :
	
	if [[ ! "$module" ]]
	then
		main.dieGracefully "Could not determine variant of $1"
	else
		# Now the variant is all between first dot and end of string
		variant=${module#*.}
		
		# Remove last dot
		variant=${variant%.}
	fi
	
	echo "$variant"
	return $CXR_RET_OK
}


################################################################################
# Function: main.CheckModuleRequirements
#	
# Checks if the currently loaded module requests a CAMxRunner and configuration
# Version less or equal than current one and if it needs anything special 
# (like special executables).
# The executable check is still a bit rough - the problem is that we do net know
# which variable name the module expects the binary to be in.
#
# Returns true on success, false otherwise
#
# Parameters:
# $1 - CXR_META_MODULE_NAME
# $2 - CXR_META_MODULE_DESCRIPTION
# $3 - CXR_META_MODULE_DOC_URL
# $4 - CXR_META_MODULE_AUTHOR
# $5 - CXR_META_MODULE_NUM_TESTS
# $6 - CXR_META_MODULE_REQ_SPECIAL
################################################################################
function main.CheckModuleRequirements() 
################################################################################
{
	local module_name
	local module_description
	local module_doc_url
	local module_author
	local module_num_tests
	local module_req_special
	
	module_name="${1}"
	module_description="${2}"
	module_doc_url="${3}"
	module_author="${4}"
	module_num_tests="${5}"
	module_req_special="${6}"

	# Disable checks for fast guys
	if [[ $CXR_FAST == true ]]
	then
		echo true
		return $CXR_RET_OK
	fi
	
	
	# Test if Module was already announced (for efficiency and log-file size reasons)
	local found
	local requirement
	local elements
	local n_elements
	local kind
	local what
	local need
	local executable
	local found
	local version
	local result
	
	result=true
	found=false
	
	found=$(main.isSubstringPresent? "$CXR_ANNOUNCED_MODULES" "$module_name")
	
	# We announce only if it was not found
	if [[ "$found" == false ]]
	then
	
		main.log -v "\nLoading ${module_name}\n"
	
		# Increase global indent level
		main.increaseLogIndent
		
		main.log -v   "==========================================================\nName of the Module: *${module_name}*\n==========================================================\n"
		main.log -v   "Description:\n$module_description\n"
		main.log -v   "More Information can be found here:\n$module_doc_url\n"
		main.log -v   "The Module was written by: $module_author\n"

		if [[ "${module_num_tests:-0}" -gt 0  ]]
		then
			main.log -v "This module contains a test suite."
		fi

		# Decrease global indent level
		main.decreaseLogIndent
	fi
	
	################################################################################
	# Perform exectuable check
	################################################################################
	
	if [[ "${module_req_special:-}" && ( "${module_req_special:-}" != "-" ) ]]
	then
		# Parsing something like "exec|dot|optional exec|wget"
		
		# Attention - we need tha standard IFS here!
		for requirement in $module_req_special
		do
			# Save old IFS
			oIFS="$IFS"
			IFS="$CXR_DELIMITER"
			
			# get requirement into array
			elements=($requirement)
			
			# Reset IFS
			IFS="$oIFS"
			
			n_elements=${#elements[@]}
			
			# do we have at least 2 elements?
			if [[ ${n_elements} -eq 2 ]]
			then
				# only stl. "exec|dot"
				kind=$(common.string.trim ${elements[0]})
				what=$(common.string.trim ${elements[1]})
				# The default is that we need it really
				need=mandatory
			elif [[ ${n_elements} -eq 3 ]]
			then
				# only stl. "exec|dot|optional"
				kind=$(common.string.trim ${elements[0]})
				what=$(common.string.trim ${elements[1]})
				need=$(common.string.trim ${elements[2]})
			else
				# this is wrong!
				main.log -e  "Requirement string $requirement contains an error. We need two or three pipe-separated fields like exec|idl or exec|idl|optional depending on the actual needs"
			fi
			
			case "$kind" in
			
				"exec") # Now we search the environment for this executable
							for executable in $(set | grep -e ^CXR_*.*_EXEC= | cut -d= -f1)
							do
								# We try to get the real name, but allow empty
								wanted_name="$(main.getBinaryName "$what" true)"
								
								if [[ "$wanted_name" && "${!executable}" ]]
								then
									# strip arguments
									if [[ "$(basename $( echo ${!executable} | cut -d" " -f1))" == "$(basename "$wanted_name")" && -x $( echo ${!executable} | cut -d" " -f1) ]]
									then
										main.log -v "${!executable} matches $what and is executable"
										found=true
										break
									fi
								fi
							done
							
							if [[ "$found" == false ]]
							then
								if [[ "$need" == mandatory ]]
								then
									main.log -e "Module $module_name mandatorily needs the executable $what which was not found."
									result=false
								else
									main.log -w "Module $module_name optinally needs the executable $what which was not found."
								fi
							fi
							;;
				*) main.log -e  "Currently, only exec requirements are supported." ;;
			esac
			
		done
	fi
	
	# Decrease global indent level
	main.decreaseLogIndent


	# If we arrive here, it is fine
	echo $result
	return $CXR_RET_OK
}
