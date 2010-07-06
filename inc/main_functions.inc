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
# TODO: Test all functions referring to common. for existence
################################################################################

################################################################################
# Function: main.isNumeric?
#
# A small, relatively robust function to test if the argument is numeric
#
# Parameters:
# $1 - String to test
################################################################################
function main.isNumeric?()
################################################################################
{ 
	# If this returns 0, this is a number
	echo "${1:-}" | grep '\<-\?\([0-9]\+\.\)\?[0-9]\+\>' >/dev/null
	
	if [[ -z "$1" || "$?" -ne 0 ]]
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
	
	if [[ $next -lt 0  ]]
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

	# This is not beautiful, actually we need to make this sure 
	# earlier!
	if [[ ! -d ${CXR_STATE_DIR:-}  ]]
	then
		mkdir -p ${CXR_STATE_DIR}
	fi

	# Show message to user
	main.log -a "${1:-No message}"
	
	# Make sure we have such a variable
	if [[ "${CXR_INSTANCE_FILE_ERROR_STACK:-}"  ]]
	then

		# Add text to the stack (if empty a newline is written)
		# No -e (one line per error) - the lines are counted later
		# Could be that the state db is not yet initialized
		if [[ -f "${CXR_INSTANCE_FILE_ERROR_STACK}"  ]]
		then
			echo "${1:-No message}" >> "${CXR_INSTANCE_FILE_ERROR_STACK}"
		fi	
	
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
# -e ERROR (ERR) [1] : Something has failed (will always be shown, presents call-stack)
#
# The user can manipulate Separate Loglevels for screen (CXR_LOG_LEVEL_SCREEN) and file (CXR_LOG_LEVEL_FILE),
# Usually the screen shows less and the file shows more.
# If the loglevel is greater or equal to the level of a message, the message is shown
#
# The function automatically indents its messages according to $CXR_LOG_INDENT.
# It also infers its caller from the FUNCNAME array
#
# For efficiency reasons (it's inefficient already...), call the function only once for multi-line messages
# use \n to separate lines.
# The use of continuation characters like
#> main.log  "This is \
#>                              a message"
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
# $1 - Message
################################################################################	
function main.log()
################################################################################
{
	# Note that CXR_DO_FILE_LOGGING is being set elsewhere
	local prefix=
	
	# A standard message is an info
	local log_level=${CXR_LOG_LEVEL_INF}
	
	# A line of stars
	local stars=""
	
	local function_name
	local blank_message
	local stderr_message
	local message
	local call_stack
	local iFct
	local maxNests
	local do_log=${CXR_DO_FILE_LOGGING:-true}
	
	while getopts ":ndviweabBH" opt
	do
		case $opt in
		b) stars="${CXR_BOX_SMALL}";;
		B) stars="${CXR_BOX_LARGE}";;
		H) stars="${CXR_BOX_HUGE}";;
		n) local do_log=false;; #Local only!
		d) log_level=$CXR_LOG_LEVEL_DBG ; prefix=DBG: ;;
		v) log_level=$CXR_LOG_LEVEL_VRB ; prefix= ;;
		i) log_level=$CXR_LOG_LEVEL_INF ; prefix= ;;
		w) log_level=$CXR_LOG_LEVEL_WRN ; prefix=WRN: ;;
		e) log_level=$CXR_LOG_LEVEL_ERR ; prefix=ERR: ;;
		a) log_level=$CXR_LOG_LEVEL_ALW ; prefix= ;;
		esac
	done
	
	# This is not strictly needed, but it allows to read 
	# non-named command line options
	shift $(($OPTIND - 1))
	
	# Make getopts ready again
	unset OPTSTRING
	unset OPTIND
	
	# max index of the call-stack
	maxNests=$(( ${#FUNCNAME[@]} - 1 ))
	
	# Often the case the first parameter is forgotten,
	# which leads to a lost message
	if [[ $# -eq 2 ]]
	then
		# Non-Standard case
		function_name="${1:-}"
		blank_message="${prefix}${2:-}"
	else
		# The caller is in ${FUNCNAME[1]}
		if [[ ${maxNests} -ge 1 ]]
		then
			function_name="${FUNCNAME[1]}"
		else
			function_name="CAMxRunner"
		fi
		
		blank_message="${prefix}${1:-}"
	fi
	
	# If set, we display the worker ID on screen
	if [[ "$CXR_MAX_PARALLEL_PROCS" -gt 1 && "${CXR_WORKER_ID:-}" ]]
	then
		stderr_message="${CXR_WORKER_ID}: $blank_message"
	else
		stderr_message="$blank_message"
	fi
	
	### In case of error, write current day to failed days file
	### and increase error count
	if [[ "${log_level}" -eq "${CXR_LOG_LEVEL_ERR}" ]]
	then
		# Could be that the state db is not yet initialized
		if [[ -f "${CXR_INSTANCE_FILE_FAILED_DAYS:-}"  ]]
		then
			echo "${CXR_DATE:-date_not_defined}" >> "${CXR_INSTANCE_FILE_FAILED_DAYS:-/dev/null}"
		fi
		
		# Create callstack
		if [[ ${maxNests} -ge 1 ]]
		then
			call_stack="Call Stack:"
			for iFct in $(seq 1 ${maxNests})
			do
				# Index of last fct
				local last=$(( $iFct - 1 ))
				call_stack="${call_stack} ${FUNCNAME[$iFct]}:${BASH_LINENO[$last]}"
			done
		fi # Callstack available?
		
		main.addError "${blank_message} - ${call_stack:-}"
	fi
	
	# We have to look at several things separately: 
	# - writing to STDERR
	# - writing to the logfile
	# - external channels like twitter/email/sms (treated together)
	
	# STDERR
	# Here we add no indentation
	if [[ "${CXR_LOG_LEVEL_SCREEN:-${CXR_LOG_LEVEL_INF}}" -ge "${log_level}"  ]]
	then
		# stderr not needed in case of error (already done)
		if [[ "${log_level}" != "${CXR_LOG_LEVEL_ERR}" ]]
		then
			# If stars are set, we add a line of stars at the beginning and end
			if [[ "${stars}" ]]
			then
				stderr_message="${stars}\n${stderr_message}\n${stars}\n"
			fi
			
			# Write a unformatted copy to sterr. 
			echo -e "${stderr_message}" 1>&2
		fi
	else
	
		if [[ $CXR_EVALUATE_FUNCTION_VERBOSE_LIST == true ]]
		then
			# There is a second chance: the user might have added the function in question to 
			# CXR_LOG_FUNCTION_VERBOSE_LIST
			local FOUND=$(expr match " ${CXR_LOG_FUNCTION_VERBOSE_LIST:-}" ".*${function_name}.*")
			
			if [[ "${FOUND}" -ne 0 ]]
			then
				# Function was added by user
				
				# If stars are set, we add a line of stars at the beginning and end
				if [[ "${stars}" ]]
				then
					stderr_message="${stars}\n${stderr_message}\n${stars}\n"
				fi
				
				# Write a unformatted copy to sterr
				echo -e "${stderr_message}" 1>&2
			fi
		
		fi
		
	fi
	
	# file
	# Prepare and write to file only if we have a defined target
	# and this was not suppressed in the call
	if [[   "${CXR_LOG_LEVEL_FILE:-${CXR_LOG_LEVEL_INF}}" -ge "${log_level}" && -n "${CXR_LOG:-}" && "$do_log" == true    ]]
	then
		# Logfile is defined

		# Add date of form Aug 26 15:44:45
		# The printf prints CXR_LOG_INDENT tabs
		prefix="$(date +"%b %d %H:%M:%S"):$(printf '\t%.0s' {1..$CXR_LOG_INDENT})"
	
		# Put prefix in front of the Blank message
		message="${prefix}${blank_message}"
	
		# Now, all the newline characters need a prefix as well:
		# \\n is what we are looking for, // request ALL \n to be replaced
		message="${message//\\n/\n${prefix}}"
		
		# If stars are set, we add a line of stars at the beginning and end
		if [[ "${stars}"  ]]
		then
			message="${prefix}${stars}\n${message}\n${prefix}${stars}\n"
		fi
		
		# Writing to file
		echo -e "$message" >> ${CXR_LOG}
	fi
	
	# The external channels (motify decides who needs to get the message)
	# -a does not go there
	if [[  "${log_level}" -gt "${CXR_LOG_LEVEL_ALW}" && "${CXR_LOG_LEVEL_EXTERNAL:-${CXR_LOG_LEVEL_ERR}}" -ge "${log_level}"   ]]
	then
		main.sendMessage "Log" "${blank_message} - ${call_stack}"
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
	local adresses=
	
	#Add SMS Adress if needed
	if [[ ${CXR_SEND_SMS} == true  ]]
	then
		adresses="$adresses $CXR_SMSADDR"
	fi
	
	# Add Email if needed
	if [[ ${CXR_SEND_EMAIL} == true  ]]
	then
		adresses="$adresses $CXR_MAILADDR"
	fi
	
	if [[ "${adresses}"  ]]
	then
		# send it if we have any adresses
		echo -e $2 | mail -s "$1" $adresses
	fi
	
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
			"print uri_escape('$(echo -e ${1}:${2})');"`"  http://twitter.com/statuses/update.json >/dev/null 2>&1
			
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
	local error_count
	local dummy_file
	
	main.log -a -B "Cleaning up $CXR_RUN and Terminating processes..."
	
	# Remove any traps once called
	trap - INT TERM EXIT
	
	################################################################################
	# Error report - do in any Non-hollow case
	################################################################################	
	
	if [[ "$CXR_HOLLOW" == false ]]
	then
		
		# All the non-hollow stuff goes here
		error_count=$(main.countErrors)
		
		main.log -B "Error Report (may be repeated)"
		
		if [[ "${error_count:-0}" -ne 0 ]]
		then
			main.log -a "The system detected at least $error_count errors, fix them!\n\nHere is the error stack:\n"
			
			if [[ "${CXR_LOG:-}" ]]
			then
				# Write to logfile
				cat "$CXR_INSTANCE_FILE_ERROR_STACK" | tee -a ${CXR_LOG}
				
				main.log -a  "In these days, errors where detected:"
				# This file might contain dupes
				cat "$CXR_INSTANCE_FILE_FAILED_DAYS" | sort | uniq | tee -a ${CXR_LOG}
			else
				# There is no log
				cat "$CXR_INSTANCE_FILE_ERROR_STACK"
				
				main.log -a  "In these days, errors where detected:"
				cat "$CXR_INSTANCE_FILE_FAILED_DAYS" | sort | uniq 
			fi
			
		else	
			main.log   "No errors where detected in this subprocess - exit status is $CXR_STATUS"
		fi
		main.log   "**********************************************************"
	
		########################################
		# Do cleanup stuff - Run always when not hollow
		########################################
		
		if [[ "$CXR_DRY" == true  ]]
		then
			main.log "Dryrun: removing the dummy files..."
			
			# common.hash.getKeys returns a CXR_DELIMITER delimited string
			oIFS="$IFS"
			local keyString="$(common.hash.getKeys $CXR_INSTANCE_HASH_DUMMY_FILES $CXR_HASH_TYPE_INSTANCE)"
			IFS="$CXR_DELIMITER"
			
			 # Turn string into array (we cannot call <common.hash.getKeys> directly here!)
			local arrKeys=( $keyString )
			
			# Reset Internal Field separator
			IFS="$oIFS"
			
			# Clean files away
			for iKey in $( seq 0 $(( ${#arrKeys[@]} - 1)) )
			do
				dummy_file=${arrKeys[$iKey]}

				main.log -v "Deleting $dummy_file"
				
				rm -f "$dummy_file" >/dev/null 2>&1
				
				# Remove from hash
				common.hash.delete $CXR_INSTANCE_HASH_DUMMY_FILES $CXR_HASH_TYPE_INSTANCE "$dummy_file"
			done
			
			IFS="$oIFS"
			
		fi
		
		# Remove all tempfiles
		common.runner.removeTempFiles
		
		# Release Locks
		common.runner.releaseAllLocks
		
		# Delete the instance files
		common.state.deleteInstanceData

	fi
	
	if [[ "$CXR_STATUS" == "$CXR_STATUS_SUCCESS" ]]
	then
		# All went well
		
		# In a dryrun, alwyas print this if finished OK
		if [[ "$CXR_DRY" == true  ]]
		then
		
			main.log -H  "Before you can run the simulation, you must clean the state database\nbecause a dryrun makes the same entries as a normal run.\nDo this using this command:\n \t ${CXR_CALL} -c \nIf you have adjusted the configuration in ${CXR_CONFIG}\nand are happy with the log, run the simulation like this (assuming an AFS/Kerberos environment):\n"     
			
			# Increase global indent level
			main.increaseLogIndent
  	
			main.log -b  "\t \$ k5run -b ${0}"
			
			# Decrease global indent level
			main.decreaseLogIndent
		
		else
			if [[ "$CXR_HOLLOW" == false ]]
			then
				# print some infos if we are not hollow 
				main.log  "Ending ${CXR_RUN}, its now $(date)"
				main.sendMessage "${CXR_RUN} finished on $CXR_MACHINE" "That is it, folks."
			fi
		fi
	fi
	
	# Change Back to rundir
	cd ${CXR_RUN_DIR} || return $CXR_RET_ERROR
	

	# What we do is kill the CAMxRunner processes (whole process group by giving negative PID)
	# See <http://stackoverflow.com/questions/392022/best-way-to-kill-all-child-processes>
	# Any child process can do this (which is good), however this means that all childs will re-execute this handler
	# because in their processes, the handler is still in place.
	kill -- -${CXR_PID}
	
}

# Set the trap (needs function first)
trap "main.doCleanup; exit" INT TERM EXIT

################################################################################
# Function: main.dieGracefully
#
# Prints a message and exits with value $2. must be in here because its important
# 
# Parameters:
# $1 - Last words
# [$2] - Run cleanup (if false, no cleanup is done)
# [$3] - Return code
################################################################################
function main.dieGracefully()
################################################################################
{
	# Release any traps
	trap - INT TERM EXIT
	local iFct
	local maxNests
	
	# Stop this run
	# Delete any .CONTINUE FILES
	
	if [[ -d "${CXR_STATE_DIR:-}"  ]]
	then
		rm -f ${CXR_STATE_DIR}/*.${CXR_STATE_CONTINUE} 2>/dev/null
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
			local last=$(( $iFct - 1 ))
			echo ${FUNCNAME[$iFct]}:${BASH_LINENO[$last]} 1>&2
		done
	fi # Callstack available?

	main.log -a  "##########################################################"
	
	# Cleanup kills the processes
	main.doCleanup
}

################################################################################
# Function: main.getBinaryName
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
# > CXR_BIN2ASC_EXEC=$(main.getBinaryName bin2asc)
#
#
# Parameters:
# $1 - name (without path!) of the binary in question
################################################################################
function main.getBinaryName()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully "needs the name of a binary as input"
	fi
	
	#Determine possible names
	local general_exec=${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/$1-${HOSTTYPE}
	local machine_exec=${general_exec}-$(uname -n)
	
	# Check name - machine first
	if [[ -x ${machine_exec}  ]]
	then
		# Machine dependent exists
		main.log   "Binary $1 is actually called ${machine_exec}"
		echo "${machine_exec}"
	elif [[ -x ${general_exec}  ]]
	then
		# general exists
		main.log   "Binary $1 is actually called ${general_exec}"
		echo "${general_exec}"
	else
		#None exists
		if [[ "$1" == false  ]]
		then
			# optional paratemeter is false,
			# We do not care and return CXR_GENERAL_EXEC
			echo "$general_exec" 
		else
			# NOK-just issue a warning (if we are compiling e.g.)
			main.log -w  "Could not find a suitable executable for name $1, neither ${machine_exec} nor ${general_exec} are present.\nRecompile these components using\n\t\$ CAMxRunner.sh -I"
		fi
	fi
	
	return 0
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
	
	if [[   $1 == CAMxRunner.sh || $1 == base || $1 == installer ]]
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
	
	if [[ ! -f "$filename"  ]]
	then
		# File inexistent
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
# Function: main.readConfig
#
# Determine relevant config files and load them, also report their names and versions.
# There are currently these:
# - site.conf
# - base.conf
# - Model version specific
# - run-specific (potentially expanded)
#
# The run name 'installer' is treated separately, there we load
# - site.conf
# - base.conf
# - Model version specific
# - installer.conf
# - ${CXR_MODEL}-${CXR_MODEL_VERSION}-installer.conf (if available)
#
# Also here, we set all variables that depend on model & version
#
# TODO: Make all parameters, except run name optional. Write function called
# pareseRunName or something.
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
	export CXR_RUN="${1}"
	
	CXR_MODEL="${2}"
	
	# Model Version
	CXR_MODEL_VERSION="${3}"
		
	CXR_RUN_DIR="${4}"
	
	if [[ -z "$CXR_MODEL_VERSION"  ]]
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

	main.log -v   "CXR_RUN_DIR of ${CXR_RUN} is $CXR_RUN_DIR"
	main.log  "Loading configuration for $CXR_RUN, $CXR_MODEL Version $CXR_MODEL_VERSION..."

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
	# Revisions
	########################################
	
	CXR_SITECONFIG_REV="$(main.getRevision $CXR_SITECONFIG)"
	CXR_BASECONFIG_REV="$(main.getRevision $CXR_BASECONFIG)"
	CXR_VERSIONCONFIG_REV="$(main.getRevision $CXR_VERSIONCONFIG)"
	CXR_CONFIG_REV="$(main.getRevision $CXR_CONFIG)"
	CXR_EXPANDED_CONFIG_REV="$(main.getRevision $CXR_EXPANDED_CONFIG)"
	
	
	########################################
	# Setting some specific variables
	########################################
	main.log -a -b "Setting variables specific for ${CXR_MODEL} ${CXR_MODEL_VERSION}..."
	
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
	
	# Hashes
	# The name of the hash that will hold the directories of the modules
	CXR_MODULE_PATH_HASH=module_paths_${CXR_MODEL}_${CXR_MODEL_VERSION}


	# The name of the hash that will hold the types of the modules
	CXR_MODULE_TYPE_HASH=module_types_${CXR_MODEL}_${CXR_MODEL_VERSION}

	########################################
	# Try to load the site config
	########################################
	
	# No drama if it does not exist
	if [[ -f "${CXR_SITECONFIG}" ]]
	then
		main.log -a   "Loading ${CXR_SITECONFIG} revision ${CXR_SITECONFIG_REV}"
	
		source "${CXR_SITECONFIG}"
	else
		main.log "I noticed that you have no site-specific configuration file (${CXR_SITECONFIG}) - this is not needed, but you might want to put site-specific stuff there."
	fi

	########################################
	# Try to load the base config
	########################################
	
	if [[ -f "${CXR_BASECONFIG}"  ]]
	then
		main.log -a   "Loading ${CXR_BASECONFIG} revision ${CXR_BASECONFIG_REV}"
		source "${CXR_BASECONFIG}"
	else
		main.dieGracefully "The base configuration ${CXR_BASECONFIG} is not available - exiting"
	fi
	
	########################################
	# Try to load CAMx version specific config
	########################################
	
	if [[ -f "$CXR_VERSIONCONFIG"  ]]
	then
		main.log -a   "Loading ${CXR_VERSIONCONFIG} revision ${CXR_VERSIONCONFIG_REV}"
		source "${CXR_VERSIONCONFIG}"
	else
		main.log  "No CAMx version specific config ($CXR_VERSIONCONFIG) found.\nThis is only needed if you want load special settings based no model version."
	fi
	
	########################################
	# Loading config that is run-specific
	########################################
	
	# Try to load an run-specific config that was expanded
	if [[ -f "${CXR_EXPANDED_CONFIG}"  ]]
	then
		# Found an expanded configuration (priority)
		main.log -a  "Found an expanded configuration for this run - loading ${CXR_EXPANDED_CONFIG} revision ${CXR_EXPANDED_CONFIG_REV}"
		source "${CXR_EXPANDED_CONFIG}"
		
		# This makes sure all following code looks at the right file
		CXR_CONFIG=${CXR_EXPANDED_CONFIG}
	else
		#No expanded version found - see if we find a normal one
		# Try to load the run-specific config
		# Does it exist?
		if [[ -f "${CXR_CONFIG}"  ]]
		then
			main.log -a   "Loading ${CXR_CONFIG} revision ${CXR_CONFIG_REV}"
			# Source it
			source "${CXR_CONFIG}"
		else
			# If the user types CAMxRunner.sh -h we land here
			if [[ "${CXR_RUN}" != "${CXR_RUNNER_NAME}"  ]]
			then
				main.dieGracefully "found no configuration for the run ${CXR_RUN}. Use \n CAMxRunner.sh -C to recreate the run!"
			fi
		fi
	fi
	
	########################################
	# Installer only: Loading special version specific config
	########################################
	
	if [[ "${CXR_RUN}" == "${CXR_INSTALLER}"  ]]
	then
		local INSTALLER_CONF="${CXR_CONF_DIR}/${CXR_MODEL}-v${CXR_MODEL_VERSION}-installer.conf"
		local INSTALLER_CONF_REV=$(main.getRevision $INSTALLER_CONF)
		
		if [[ -f "${INSTALLER_CONF}"  ]]
		then
			main.log -a   "Loading ${INSTALLER_CONF} revision ${INSTALLER_CONF_REV}"
			source "${INSTALLER_CONF}"
		else
			main.log -w  "No CAMx version specific config for installer ($INSTALLER_CONF) found.\nThis is only needed if you want to install and run different CAMx versions in parallel"
		fi
	fi
}

################################################################################
# Function: main.HandleHup
#
# Handles the HUP signal (sent to reread the configuration)
#
################################################################################
function main.HandleHup()
################################################################################
{
	main.log -a "Rereading Configuration of ${CXR_RUN}..."
	
	# Do it
	main.readConfig "${CXR_RUN}" "${CXR_MODEL}" "${CXR_MODEL_VERSION}" "${CXR_RUN_DIR}"
}

# Set the trap (needs function first)
trap "main.HandleHup" HUP


################################################################################
# Function: main.getModuleName
#
# Extracts the module name from a path, removes the leading number (if any) and the trailing extension
#
# Parameters:
# $1 - Module name with or without path
################################################################################
function main.getModuleName()
################################################################################
{
	local full_name
	local module_name
	local index_us
	
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "needs a string (the long module name) as input."
	fi	
	
	full_name=${1}
	
	# We just want the filename (without .sh)
	module_name=$(basename $full_name .sh)

	# Remove XX_ if present
	module_name="$(expr match "$module_name" '[0-9]\{0,\}_\{0,1\}\([_a-z]\{1,\}\)')"
	
	if [[ ! "$module_name" ]]
	then
		main.dieGracefully "Could not determine module name of $1"
	fi
	
	echo "$module_name"
	return $CXR_RET_OK
}

