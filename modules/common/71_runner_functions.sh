#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains CAMx Runner Functions
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# TODO: 
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=4

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains functions for the CAMxRunner (creation of new runs, module calls, process management)"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2009), CAMxRunner@psi.ch"

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

	Can ONLY be called by the CAMxRunner.
	
	Written by $CXR_META_MODULE_AUTHOR
	License: $CXR_META_MODULE_LICENSE
	
	Find more info here:
	$CXR_META_MODULE_DOC_URL
EOF
exit 1
}

################################################################################
# Function: common.runner.getX
# 
# Returns the x dimension  of a given grid (in grid cells of this grid)
# Hides the fact that the configuration is different for master and non-master grids.
# Note that for nested domains, the 2 buffer cells are added
#
# Parameters:
#
# $1 - A integer denoting the domain for which we need the dim (in the range 1..CXR_NUMBER_OF_GRIDS)
################################################################################
function common.runner.getX()
################################################################################
{
	local domain=${1}
	local xdim
		
	if [[  ! ( ${domain} -ge 1 && ${domain} -le ${CXR_NUMBER_OF_GRIDS} )   ]]
	then
		main.dieGracefully "Domain $domain is out of the range 1..${CXR_NUMBER_OF_GRIDS}"
	fi
	
	if [[ "${domain}" == 1  ]]
	then
		# Master Grid
		xdim=${CXR_MASTER_GRID_COLUMNS}
	else
		# Any other grid
		xdim=$(( (((${CXR_NEST_END_I_INDEX[${domain}]} - ${CXR_NEST_BEG_I_INDEX[${domain}]}) + 1) * ${CXR_NEST_MESHING_FACTOR[${domain}]}) + 2))
		#                                                                        |                                      |
		#                                                                    Fencepost                                  |
		#                                                                                                            Buffer Cells (left/right)
	fi
	
	echo ${xdim}
}

################################################################################
# Function: common.runner.getY
# 
# Returns the y dimension  of a given grid (in grid cells of this grid)
# Hides the fact that the configuration is different for master and non-master grids
# Note that for nested domains, the 2 buffer cells are added
#
# Parameters:
#
# $1 - A integer denoting the domain for which we need the dim (in the range 1..CXR_NUMBER_OF_GRIDS)
################################################################################
function common.runner.getY()
################################################################################
{
	local domain=${1:-0}
	local ydim
	
	if [[  ! ( ${domain} -ge 1 && ${domain} -le ${CXR_NUMBER_OF_GRIDS} )   ]]
	then
		main.dieGracefully "domain $1 is out of the range 1..${CXR_NUMBER_OF_GRIDS}"
	fi
	
	if [[ "${domain}" == 1  ]]
	then
		# Master Grid
		ydim=${CXR_MASTER_GRID_ROWS}
	else
		# Any other grid
		ydim=$(( (((${CXR_NEST_END_J_INDEX[${domain}]} - ${CXR_NEST_BEG_J_INDEX[${domain}]}) + 1) * ${CXR_NEST_MESHING_FACTOR[${domain}]}) + 2 ))
		#                                                                        |                                      |
		#                                                                    Fencepost                                  |
		#                                                                                                            Buffer Cells (up/down)
	fi
			
	echo ${ydim}
	
}

################################################################################
# Function: common.runner.getZ
# 
# Returns the z dimension  of a given grid (in grid cells of this grid)
# Hides the fact that the configuration is different for master and non-master grids
#
# Parameters:
#
# $1 - A integer denoting the domain for which we need the dim (in the range 1..CXR_NUMBER_OF_GRIDS)
################################################################################
function common.runner.getZ()
################################################################################
{
	domain=${1:-0}
	
	if [[  ! ( ${domain} -ge 1 && ${domain} -le ${CXR_NUMBER_OF_GRIDS} )   ]]
	then
		main.dieGracefully "domain $1 is out of the range 1..${CXR_NUMBER_OF_GRIDS}"
	fi
	
	echo ${CXR_NUMBER_OF_LAYERS[${domain}]}
	
}

################################################################################
# Function: common.runner.getMaxX
# 
# Returns the maximum x dimension of all grids 
# Useful for compiling CAMx
#
################################################################################
function common.runner.getMaxX()
################################################################################
{
	local new
	local max_xdim=0
	
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(common.runner.getX $i)"
		
		if [[ "$new" -gt "$max_xdim"  ]]
		then
			max_xdim=$new
		fi
	done
	
	echo ${max_xdim}
}

################################################################################
# Function: common.runner.getMaxY
# 
# Returns the maximum y dimension of all grids 
# Useful for compiling CAMx
#
################################################################################
function common.runner.getMaxY()
################################################################################
{
	local new
	local max_ydim=0
	
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(common.runner.getY $i)"
		
		if [[ "$new" -gt "$max_ydim"  ]]
		then
			max_ydim=$new
		fi
	done
	
	echo ${max_ydim}
}

################################################################################
# Function: common.runner.getMaxZ
# 
# Returns the maximum z dimension of all grids 
# Useful for compiling CAMx
#
################################################################################
function common.runner.getMaxZ()
################################################################################
{
	local new
	local max_zdim=0
	
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(common.runner.getZ $i)"
		
		if [[ "$new" -gt "$max_zdim"  ]]
		then
			max_zdim=$new
		fi
	done
	
	echo ${max_zdim}
}

################################################################################
# Function: common.runner.countCells3D
# 
# Returns the sum of the number of cells in all grids (3D)
# Used by <common.check.PredictModelOutputMb>
#
################################################################################
function common.runner.countCells3D()
################################################################################
{
	local new
	local sum=0
	
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(common.runner.getZ $i)"
		
		sum=$(( $sum + ( $(common.runner.getX $i) * $(common.runner.getY $i) * $(common.runner.getZ $i) ) ))
		
	done
	
	echo $sum
}

################################################################################
# Function: common.runner.countCells2D
# 
# Returns the sum of the number of cells in all grids (Just lowest layer)
# Used by <common.check.PredictModelOutputMb>
#
################################################################################
function common.runner.countCells2D()
################################################################################
{
	local new
	local sum=0
	
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(common.runner.getZ $i)"
		
		sum = $(( $sum + ( $(common.runner.getX $i) * $(common.runner.getY $i) ) ))
		
	done
	
	echo $sum
}

################################################################################
# Function: common.runner.reportDimensions
# 
# Prints the dimensions of all the defined grids.
#
################################################################################
function common.runner.reportDimensions()
################################################################################
{
	local i
	
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		main.log -B "Grid dimensions domain ${i}:\nX: $(common.runner.getX ${i})\nY: $(common.runner.getY ${i})\nZ: $(common.runner.getZ ${i})"
	done
	
	main.log -B "Total number of cells: $(common.runner.countCells3D)"
	
}

################################################################################
# Function: common.runner.evaluateRule
# 
# Evaluates a filerule and returns its expansion. Removes syntactical fluff unknown
# to non-bashers.
# If it is a file rule, the file might be compressed, <common.fs.TryDecompressingFile> is called.
#
# Side effect: if the file is compressed and we cannot decompress in place,
# the returned file name will change. If you want the "expected" file name,
# use the fourth parameter.
# ABSOLUTELY use this parameter for any OUTPUT_FILE because if the output would have been 
# compressed, CAMxRunner would decompress it, wich makes no sense.
#
# To be on the safe side, quote the call (double quotes!)
#
# Examples:
# This code does not if the rule expands to the empty string:
# >MY_DIR="$(common.runner.evaluateRule "$RULE" false "$rule_name")"
# >These  ^                                                ^
# >Ensure that the code does not fail if the string returned contains
# >spaces.
#
# This code accepts an empty string:
# >MY_DIR="$(common.runner.evaluateRule "$RULE" true "$rule_name")" 
#
# This code is also valid and will not fail on empty expansion:
# >MY_DIR="$(common.runner.evaluateRule "$RULE")"
#
# You can use it to generate any string that is made up by variables,
# but make sure that control sequences like \n are double-escaped (\\n)
# because the expansion otherwise removes the sequence:
#
# >CXR_FINISH_MESSAGE_RULE='Please copy this into https://wiki.intranet.psi.ch/twiki/LAC/CAMxRuns \\n \| $(date +"%Y/%M/%D") \| ${USER} \| ${CXR_STATUS} \| ${CXR_RUN} \| ${CXR_START_DATE} \| ${CXR_STOP_DATE} \| http://people.web.psi.ch/oderbolz/CAMx/conf/$(basename $CXR_CONFIG) \| http://people.web.psi.ch/oderbolz/CAMx/log/$(basename $CXR_LOG) \| \\n'
# ...
# >main.log  "$(common.runner.evaluateRule "$CXR_FINISH_MESSAGE_RULE" true CXR_FINISH_MESSAGE_RULE)"
#
# Parameters:
# $1 - The rule to be evaluated (a string, not a variable)
# [$2] - allow_empty if false, a rule must expand to a non-empty string
# [$3] - optional name of the rule
# [$4] - try_decompression if false, will not attempt compression (and consequenital renaming)
################################################################################
function common.runner.evaluateRule()
################################################################################
{
	if [[  $# -lt 1 && $# -gt 4 ]]
	then
		main.dieGracefully "needs at least string (the rule) as input, at most the rule, true/false, the rule name and true/false!"
	fi	
	
	local rule="$1"
	
	# Per default we allow rules to expand to the empty string
	local allow_empty="${2:-true}"
	local rule_name="${3:-}"
	# By default try decompression
	local try_decompression="${4:-true}"
	local expansion
	
	if [[ -z "$rule"  ]]
	then
		# If the rule is empty, we return empty
		main.log -v   "rule $rule_name was empty..."
		echo ""
		return
	fi
	
	main.log -v   "Evaluating rule $rule_name $rule..."

	# Original code: CXR_ROOT_OUTPUT=$(eval "echo $(echo $CXR_ROOT_OUTPUT_FILE_RULE)")
	expansion="$(eval "echo $(echo "$rule")")"
	
	main.log -v  "Evaluated rule: $expansion"
	
	# *_FILE_RULE might be compressed
	# Does the name of the rule end in _FILE_RULE ?
	if [[ "${rule_name: -10}" == "_FILE_RULE"  ]]
	#                 �
	# This space here � is vital, otherwise, bash thinks we mean a default (see http://tldp.org/LDP/common.math.abs/html/string-manipulation.html)
	then
		if [[ "${try_decompression}" == true  ]]
		then
	
			# Try to decompress
			expansion=$(common.fs.TryDecompressingFile $expansion)
			
		else
			main.log -v  "No decompression attempted."
		fi # try_decompression
	fi
	
	main.log -v  "Evaluated rule: $expansion"
	
	if [[  -z "$expansion" && "$allow_empty" == false   ]]
	then
		# Empty not allowed
		main.dieGracefully "Rule $rule_name ($rule) was expanded to the empty string which is not allowed in this context!"
	fi
	
	echo "$expansion"
	
}

################################################################################
# Function: common.runner.evaluateRule
# 
# Evaluates a filerule for a given simulation day offset (0..NUMBER_OF_SIM_DAYS-1).
# This is especially useful to determine how certain things where at the first day of 
# a simulation (e. g. a filename).
# The function needs to re-evaluate the date variables, but takes care to reset them properly.
#
# Parameters:
# $1 - The rule to be evaluated (a string, not a variable)
# $2 - The offset of the simulation day (0..NUMBER_OF_SIM_DAYS-1)
# [$3] - allow_empty if false, a rule must expand to a non-empty string
# [$4] - optional name of the rule
################################################################################
function common.runner.evaluateRuleAtDayOffset()
################################################################################
{
	if [[  $# -lt 2 && $# -gt 4 ]]
	then
		main.dieGracefully "needs at least one string (the rule) and one number (the day offset) as input!"
	fi
	
	# Local variables
	local current_offset
	local rule
	local day_offset
	local expansion
	
	rule="$1"
	day_offset="$2"

	# First store current offset
	current_offset=${CXR_DAY_OFFSET}
	
	# Re-evaluate the date variables
	common.date.setVars "${CXR_START_DATE}" "${day_offset}"
	
	# Evaluate the rule
	expansion=$(common.runner.evaluateRule "${rule}" "${3:-}" "${4:-}")
	
	# Reset Current offset
	CXR_DAY_OFFSET=${current_offset}
	
	# Reset the date vars
	common.date.setVars "${CXR_START_DATE}" "${CXR_DAY_OFFSET}"
	
	echo "$expansion"
}

################################################################################
# Function: common.runner.evaluateScalarRules
# 
# Evaluates all CXR_*_RULE environment variables in the given list.
#
# Arrays can *not* expanded using this technique!
#
# $1 - The list of rules to be evaluated (a space-separated string, not a variable)
# [$2] - allow_empty if false, *all* rules in the must expand to a non-empty string
################################################################################
function common.runner.evaluateScalarRules()
################################################################################
{
	if [[  $# -lt 1 && $# -gt 2   ]]
	then
		main.dieGracefully "needs a string (the list of rules) as input and optionally a boolean allow_empty value!"
	fi
	
	local rule_list="$1"
	local current_rule
	local variable
	
	# Per default we allow rules te expand to the empty string
	allow_empty=${2:-true}
		
	# Read the relevant rules from the environment and
	
	# Loop through them
	for current_rule in $rule_list
	do
		# Chop off _RULE
		# So e. g. CXR_FINISH_MESSAGE_RULE turns into CXR_FINISH_MESSAGE
		variable=${current_rule%_RULE}
		
		# Set variable to its evaluated form
		export $variable="$(common.runner.evaluateRule "${current_rule}" "$allow_empty" $current_rule)"
	done
}


################################################################################
# Function: common.runner.createDummyFile
#
# Creates a dummy file, shows a message and adds the file
# to the dummy file list. The size can be chosen if 
#
# Parameters:
# $1 - Filename
# [$2] - size in MB (Default 1)
################################################################################
function common.runner.createDummyFile()
################################################################################
{
	local filename="$1"
	local size="${2:-1}"
	
	main.log "Creating Dummy $filename of size $size"
	
	if [[ $(main.isNumeric? $size) == false ]]
	then
		main.log -w "You must supply a numeric size in MB. Using 1 MB now."
		size=1
	fi
	
	# Create file
	if [[ ! -f "$filename" ]]
	then
		dd bs=${size}M if=/dev/zero of=$filename count=1
	fi
	
	# Store Dummy file in the file list
	echo "$filename" >> "$CXR_INSTANCE_FILE_DUMMY_LIST"

	return 0
}

################################################################################
# Function: common.runner.createTempFile
#
# Returns the name of a temporary file with random name, shows a message and adds the file
# to the temp file list if this is needed. 
# Replaces calls to mktemp and removes the need to remove the temp files, this is done by 
# <common.runner.removeTempFiles>
#
# Recommended call:
# >TMPFILE=$(common.runner.createTempFile $FUNCNAME)
#
# Parameters:
# [$1] - identifier of tempfile, recommended to use $FUNCNAME
# [$2] - store, if false, we do not add the file to list. This is useful if we keep track of the files ourselves (e. g. when decompressing files)
################################################################################
function common.runner.createTempFile()
################################################################################
{
	
	if [[ ! -d "${CXR_TMP_DIR}" ]]
	then
		mkdir -p "${CXR_TMP_DIR}"
	fi
	
	# Check if that worked!
	if [[ ! -d "${CXR_TMP_DIR}" ]]
	then
		main.dieGracefully "could not create tmp directory ${CXR_TMP_DIR} (maybe its a broken Link?), CAMxRunner cannot continue."
	fi
	
	local store=${2:-true}
	
	# Create a template by using $1 
	# and adding 8 random alphanums
	# This way, the filename has a meaning
	local template="${CXR_TMP_DIR}/${CXR_TMP_PREFIX}${1:-temp}.XXXXXXXX"

	# replace eventual spaces by _
	template=${template// /_}
	
	local filename=$(mktemp $template)
	main.log -v   "Creating temporary file $filename"
	
	if [[ "${store}" == true  ]]
	then
		# Add to dummy list
		echo $filename >> "$CXR_INSTANCE_FILE_TEMP_LIST"
	fi
	
	echo $filename
	return 0
}

################################################################################
# Function: common.runner.removeTempFiles
#
# Removes all files from the temp file list if CXR_REMOVE_TEMP_FILES is true.
# Also removes decompressed files if requested.
#
################################################################################
function common.runner.removeTempFiles()
################################################################################
{
	local line
	local filename
	local sed_tmp
	local temp_file
	
	# remove decompressed files, if wanted
	# each removed file is also removed from the global list
	if [[ "$CXR_REMOVE_DECOMPRESSED_FILES" == true  ]]
	then
		if [[ -s "$CXR_DECOMPRESSED_LIST"  ]]
		then
			# List file is non-empty
			main.log  "Removing temporarily decompressed files..."
		
			# Loop trough all entries
			while read line 
			do
				# The line has the format compressed_file|decompressed_file
				filename=$(echo "$line" | cut -d${CXR_DELIMITER} -f2)
				
				main.log -v "Deleting $filename"
				rm -f "${filename}"
				
				# remove that line via sed
				sed_tmp=$(common.runner.createTempFile sed false) # Tempfile is not added to list (we move it away)
				sed "/$line/d" "${CXR_DECOMPRESSED_LIST}" > "${sed_tmp}"
				mv "${sed_tmp}" "${CXR_DECOMPRESSED_LIST}"
			done < "$CXR_DECOMPRESSED_LIST"
		fi
	else
		main.log  "The temporarily decompressed files \n$(cat ${CXR_DECOMPRESSED_LIST} | cut -d${CXR_DELIMITER} -f 2 2>/dev/null )\n will not be deleted because the variable CXR_REMOVE_DECOMPRESSED_FILES is false."
	fi

	# remove temporary files, if wanted
	if [[ "$CXR_REMOVE_TEMP_FILES" == true  ]]
	then
		# Does the list even exist?
		if [[ -s "$CXR_INSTANCE_FILE_TEMP_LIST"  ]]
		then
			main.log  "Removing temporary files..."
			
			# Clean temp files away
			for temp_file in $(cat "${CXR_INSTANCE_FILE_TEMP_LIST}")
			do
				main.log -v   "Deleting $temp_file"
				
				rm -f "$temp_file" >/dev/null 2>&1
			done
			
			# Empty the list of temp files just cleaned
			: > ${CXR_INSTANCE_FILE_TEMP_LIST}
		fi
	else
		main.log  "The temporary files $(cat ${CXR_INSTANCE_FILE_TEMP_LIST} 2>/dev/null ) will not be deleted because the variable CXR_REMOVE_TEMP_FILES is false."
	fi
	

}

################################################################################
# Function: common.runner.getLock
#
# Blocking call to the lockmanager. If we get the lock, it will be added to the 
# CXR_INSTANCE_FILE_LOCK_LIST. This allows to release the locks later when we exit from the program
# When the lock is not free, this call waits forever.
#
# Recommended call:
# > common.runner.getLock lockname
#
# Parameters:
# $1 - the name of the lock to get
################################################################################
function common.runner.getLock()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully "needs the name of a lock as input"
	fi
	
	local lock="$1"

	# For debug reasons, locking can be turned off
	if [[ $CXR_NO_LOCKING == false  ]]
	then
		main.log -v   "Waiting to set lock $lock..."
	
		$CXR_LOCK_MAN_EXEC set "$lock" -1
		
		main.log -v   "Got lock $lock."
		
		if [[ $(grep -c -e "^$lock\$") -ne 0  ]]
		then
			# lock already in list (should not happen!)
			main.log  "Weird: it seems as if the lock $lock was given out more than once!"
		else
			# Put the new lock on the list
			echo "$lock" >> $CXR_INSTANCE_FILE_LOCK_LIST
		fi
	fi
}

################################################################################
# Function: common.runner.releaseLock
#
# Releases a lock and removes it from CXR_INSTANCE_FILE_LOCK_LIST.
#
# Recommended call:
# > common.runner.releaseLock lockname
#
# Parameters:
# $1 - the name of the lock to release
################################################################################
function common.runner.releaseLock()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully "needs the name of a lock as input"
	fi
	
	local sed_tmp=$(common.runner.createTempFile $FUNCNAME)
	
	lock="$1"
	
	main.log -v   "Waiting to release lock $lock..."

	# We even release locks if locking is turned off
	$CXR_LOCK_MAN_EXEC unset "$lock"
	
	main.log -v   "lock $lock released."
	
	# Remove this line from the lock-list
	sed "/^$lock\$/d" $CXR_INSTANCE_FILE_LOCK_LIST > $sed_tmp
	
	# Exchange tempfile and new file
	mv $sed_tmp $CXR_INSTANCE_FILE_LOCK_LIST
	
}

################################################################################
# Function: common.runner.releaseAllLocks
#
# Releases all locks held by iterating over CXR_INSTANCE_FILE_LOCK_LIST.
#
#
# Parameters:
# None.
################################################################################
function common.runner.releaseAllLocks()
################################################################################
{
	local lock
	
	if [[ -s "$CXR_INSTANCE_FILE_LOCK_LIST"  ]]
	then
		main.log  "Releasing all locks..."
	
		for lock in $(cat $CXR_INSTANCE_FILE_LOCK_LIST)
		do
			common.runner.releaseLock $lock
		done
	fi
}

################################################################################
# Function: common.runner.createConfigFile
#
# Interactively creates a new configuration file for a run. 
#
# Can derive a new file from any given config file - also takes care of expansion.
# Also can create a new file using an .ask file, similar to the installer of CAMxRunner
#
# Parameters:
# $1 - Run-name
################################################################################	
function common.runner.createConfigFile() 
################################################################################
{
	local run="$1"
	local basefile
	local template
	local destination
	local askfile
	local playfile
	local tmpfile
	
	if [[ $(common.user.getOK "We create a configuration file for the new run now.\n Do you want to copy an existing file? (If you say no, you will be asked the values of the new configuration instead)" ) == true  ]]
	then

		# Show a list of existing files to choose from
		if [[ $(common.user.getOK "Do you want to use a file other than \n $(basename ${CXR_BASECONFIG}) as as starting point?" ) == false  ]]
		then
			#No, use base.conf
			basefile=${CXR_BASECONFIG}
		else
			#Yes, gimme options
			
			# To keep the list compact, we go into the conf dir and back out again
			cd "${CXR_CONF_DIR}" || main.dieGracefully "Could not change to ${CXR_CONF_DIR}!"
			
			basefile=${CXR_CONF_DIR}/$(common.user.getMenuChoice "Choose a file I should use:" "*.conf" )
			
			cd "$CXR_RUN_DIR" || main.dieGracefully "Could not change to $CXR_RUN_DIR"
		fi
	
		if [[ ! -f "$basefile"  ]]
		then
			main.dieGracefully "File $basefile is not readable!"
		fi
	
		# For the moment, I romoved the option to expand a config
		# Tell user if expand is on and let the user decide
		
#		if [ $(common.user.getOK "Do you want to expand the new configuration?\n \
#This means that any variable in $basefile that is not protected by \
#single quotes will be expanded to its value and then be written to the new configuration file. \
#This makes everything in it static, allowing you to preserve the actual settings used. \
#However, this can also be a disadvantage!" N ) == true ]
#		then
#			# Yes, expand.
#			common.runner.expandConfigFile ${CXR_BASECONFIG} ${CXR_EXPANDED_CONFIG}
#		else
			#No expansion, just copy.
		
			# Is the file already there?
			if [[ -f ${CXR_CONFIG}  ]]
			then
				# Continue even if file is there?
				if [[ $(common.user.getOK "${CXR_CONFIG} already exists. Do you want to overwrite this file?" ) == false  ]]
				then
					exit
				fi
			fi
		
			# The user just wants a copy
			# copy base config and make sure some file is present
			cp  ${basefile} ${CXR_CONFIG}
			touch ${CXR_CONFIG}
			chmod +x ${CXR_CONFIG}

#		fi # Decision to expand 

	else
		# The user wants to be asked a lot of questions.
		
		# This is for the replacement later
		basefile=base.conf

		# The template we use (can be chosen more elaborate, maybe)
		template="${CXR_TEMPLATES_DIR}/conf/base.tpl"

		destination="${CXR_CONF_DIR}/${run}.conf"

		if [[ -f "$destination"  ]]
		then
			# Continue even if file is there?
			if [[ $(common.user.getOK "$destination already exists. Do you want to overwrite this file?" N ) == false  ]]
			then
				exit
			fi
		fi

		# Let's first copy the template
		cp "$template" "$destination"
		
		# We will now ask the user a number of questions encoded in an ask-file
		# The result will be a play-file
		askfile=${CXR_INSTALLER_VERSION_INPUT_DIR}/base.ask
		playfile=${CXR_CONF_DIR}/${run}.play
		
		# Might be simplified later
		if [[ -s "$playfile"  ]]
		then
			# We already have a playfile
			# Do you want to replay?
			if [[ "$(common.user.getOK "Such a config file was already created. Do you want to look at the settings that where used then? (You will then be asked if you want to reinstall using those values)" Y )" == true  ]]
			then
				# Yes, show me
				cat "$playfile"
				
				if [[ "$(common.user.getOK "Should this installation be repeated with the existing settings?" N )" == true  ]]
				then
					# Playback, do nothing
					:
				else
					# Redo
					common.user.getAnswers "$askfile" "$playfile"
				fi
			else
				# Redo
				common.user.getAnswers "$askfile" "$playfile"
			fi
		else
			# Start from scratch
			common.user.getAnswers "$askfile" "$playfile"
		fi

		common.user.applyPlayfile $playfile 
		
		# We need this set later
		CXR_CONFIG=$destination

		# Should we add more tests?
		
	fi # Decision if copy or .ask
	
	main.log  "Edit the config file ${CXR_CONFIG} if needed - else just dry-run the script: \n \$ \t ${run} -d";
}

################################################################################
# Function: common.runner.expandConfigFile
#
# Replaces and non-single quoted variable in a config file by its value.
# This allows to preserve a config file (it is then independent of base.conf).
# Interactive function.
#
# Parameters:
# $1 - File to expand
# $2 - resulting output file
################################################################################
function common.runner.expandConfigFile() 
################################################################################
{
	local basefile=$1
	local expanded_config=$2
	
	# Is the file already there?
	if [[ -f ${expanded_config}  ]]
	then
		# Continue even if file is there?
		if [[ $(common.user.getOK "${expanded_config} already exists. Do you want to overwrite this file?" N ) == false  ]]
		then
			exit
		fi
	fi

	# First save some variables name of the baseconfig to a dummy name.
	# They are again exported before expanding.
	# Needed because these variables are not defined in a normal conf file,
	# but their values are still referenced. 
	# We first unset all variables, then load the config!

	local model_version=${CXR_MODEL_VERSION}
	local run_dir=${CXR_RUN_DIR}
	run=${CXR_RUN}

	# This nice construction expands any variables which are not
	# written in single quotes. Gotten out of a Usenet conversation on comp.unix.shell,
	# idea by Michael Schindler 
	# (the original line was
	# echo "$( unset ${!CXR_*}; source ${basefile} ;  set | grep ^CXR_ )" > ${CXR_EXPANDED_CONFIG}
	echo "$( unset ${!CXR_*}; CXR_RUN_DIR=${run_dir} ; CXR_RUN=${run}; CXR_MODEL_VERSION=${model_version}; source ${basefile} ;  set | grep ^CXR_ )" > ${expanded_config}

	CXR_CONFIG=$expanded_config
}

################################################################################
# Function: common.runner.getModelId
#
# Each model has a 0-based id (used for example to index CXR_SUPPORTED_MODEL_VERSIONS)
#
# Parameters:
# $1 - Exact name of model to use
################################################################################	
function common.runner.getModelId() 
################################################################################
{
	local needle="$1"
	
	# Find my element
	local current_id=0
	local current_model
	
	for current_model in $CXR_SUPPORTED_MODELS
	do
		if [[ $current_model == $needle ]]
		then
			echo $current_id
			return 0
		fi
		
		current_id=$(( $current_id + 1 ))
		
	done
	
	# Found nothing!
	return 1
}


################################################################################
# Function: common.runner.createNewRun
#
# Creates a new run by creating an appropriate link and calling <common.runner.createConfigFile>
#
# Tolerates runs that already exist (asks user).
#
################################################################################	
function common.runner.createNewRun() 
################################################################################
{

	local model="$(common.user.getMenuChoice "Which model should be used?\nIf your desired model is not in this list, adjust CXR_SUPPORTED_MODELS \n(Currently $CXR_SUPPORTED_MODELS)" "$CXR_SUPPORTED_MODELS" )"
	
	local model_id=$(common.runner.getModelId "$model") || main.dieGracefully "model $model is not known."
	
	# Extract the list of supported versions
	local supported="${CXR_SUPPORTED_MODEL_VERSIONS[${model_id}]}"
	
	#Generate a menu automatically
	local version="$(common.user.getMenuChoice "Which version of $model should be used?\nIf your desired version is not in this list, adjust CXR_SUPPORTED_MODEL_VERSIONS \n(Currently $supported)" "$supported" )"
	
	common.check.isVersionSupported? "$version" "$model" || main.dieGracefully "The version you supplied is not supported. Adjust CXR_SUPPORTED_MODEL_VERSIONS."
	
	local run=${model}-v${version}
	
	local addition
	
	echo "The Run name so far is ${run}- what do you want to add?"
	read addition

	run="${run}-$addition"

	# Name ok? ###################################################################
	common.check.RunName $run || main.dieGracefully "The name supplied does not contain a proper CAMx version. Rerun using $0 -C to be guided inturactively"
		
	# Name OK.
	
	# Extract and export model name and version 
	main.setModelAndVersion $run
	
	# This is the file name of an extended config
	CXR_EXPANDED_CONFIG=${CXR_CONF_DIR}/${run}.econf

	# This is a "normal" copy
	CXR_CONFIG=${CXR_CONF_DIR}/${run}.conf
	
	#name is OK - create link ##################################################
	if [[ ! -L $run  ]]
	then
		main.log   "Creating link $run."
			
		ln -s $CXR_RUNNER_NAME $run
		
	else
		# run already existists, OK? ###############################################
		if [[ $(common.user.getOK "$run already exists. Do you want to continue \n (Makes sense if you regenerate the configuration)?" N ) == false  ]]
		then
			# No
			exit
		fi
	fi
		
	# Create a configuration #####################################################
	common.runner.createConfigFile $run
	
	# Messages & Good wishes #####################################################
	main.log  "New run was created, start it with \n\t \$ $run -d"

}

################################################################################
# Function: test_module
#
# Runs the predefined tests for this module. If you add or remove tests, please
# update CXR_META_MODULE_NUM_TESTS in the header!
# 
################################################################################	
function test_module()
################################################################################
{
	if [[ "${CXR_TESTING_FROM_HARNESS:-false}" == false  ]]
	then
		# We need to do initialisation
	
		# This is the run we use to test this
		CXR_RUN=$CXR_META_MODULE_TEST_RUN
	
		# Safety measure if script is not called from .
		MY_DIR=$(dirname $0) && cd $MY_DIR
	
		# We step down the directory tree until we either find CAMxRunner.sh
		# or hit the root directory /
		while [[ $(pwd) != / ]]
		do
			# If we find CAMxRunner, we are there
			ls CAMxRunner.sh >/dev/null 2>&1 && break
			
			# If we are in root, we have gone too far
			if [[ $(pwd) == / ]]
			then
				echo "Could not find CAMxRunner.sh!"
				exit 1
			fi
			
			cd ..
		done
		
		# Save the number of tests, as other modules
		# will overwrite this (major design issue...)
		MY_META_MODULE_NUM_TESTS=$CXR_META_MODULE_NUM_TESTS
		
		# Include the init code
		source inc/init_test.inc
		
		# Plan the number of tests
		plan_tests $MY_META_MODULE_NUM_TESTS
	fi
	
	########################################
	# Setup tests if needed
	########################################
	
	i=1
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.runner.evaluateRule a) a "common.runner.evaluateRule constant"
	is $(common.runner.evaluateRule "$(common.math.abs -100)") 100 "common.runner.evaluateRule a function of CAMxRunner"
	is $(common.runner.evaluateRule "domain$(common.string.leftPadZero $i 3)") domain001 "common.runner.evaluateRule with formatting"
	is $(common.runner.evaluateRule "$(uname -n)") $(uname -n) "common.runner.evaluateRule with uname"
	
	########################################
	# teardown tests if needed
	########################################
	
	if [[ "${CXR_TESTING_FROM_HARNESS:-false}" == false ]]
	then
		# We where called stand-alone, cleanupo is needed
		main.doCleanup
	fi
	
}

################################################################################
# Are we running stand-alone? 
################################################################################


# If the CXR_META_MODULE_NAME  is not set
# somebody started this script alone
# Normlly this is not allowed, except to test using -t
if [[ -z "${CXR_META_MODULE_NAME:-}"  ]]
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
	else
		usage
	fi

fi


