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
# Function: cxr_common_get_x_dim
# 
# Returns the x dimension  of a given grid (in grid cells of this grid)
# Hides the fact that the configuration is different for master and non-master grids.
# Note that for nested domains, the 2 buffer cells are added
#
# Parameters:
#
# $1 - A integer denoting the domain for which we need the dim (in the range 1..CXR_NUMBER_OF_GRIDS)
################################################################################
function cxr_common_get_x_dim()
################################################################################
{
	local domain=${1}
	local xdim
		
	if [[  ! ( ${domain} -ge 1 && ${domain} -le ${CXR_NUMBER_OF_GRIDS} )   ]]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - Domain $domain is out of the range 1..${CXR_NUMBER_OF_GRIDS}"
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
# Function: cxr_common_get_y_dim
# 
# Returns the y dimension  of a given grid (in grid cells of this grid)
# Hides the fact that the configuration is different for master and non-master grids
# Note that for nested domains, the 2 buffer cells are added
#
# Parameters:
#
# $1 - A integer denoting the domain for which we need the dim (in the range 1..CXR_NUMBER_OF_GRIDS)
################################################################################
function cxr_common_get_y_dim()
################################################################################
{
	local domain=${1:-0}
	local ydim
	
	if [[  ! ( ${domain} -ge 1 && ${domain} -le ${CXR_NUMBER_OF_GRIDS} )   ]]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - domain $1 is out of the range 1..${CXR_NUMBER_OF_GRIDS}"
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
# Function: cxr_common_get_z_dim
# 
# Returns the z dimension  of a given grid (in grid cells of this grid)
# Hides the fact that the configuration is different for master and non-master grids
#
# Parameters:
#
# $1 - A integer denoting the domain for which we need the dim (in the range 1..CXR_NUMBER_OF_GRIDS)
################################################################################
function cxr_common_get_z_dim()
################################################################################
{
	domain=${1:-0}
	
	if [[  ! ( ${domain} -ge 1 && ${domain} -le ${CXR_NUMBER_OF_GRIDS} )   ]]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - domain $1 is out of the range 1..${CXR_NUMBER_OF_GRIDS}"
	fi
	
	echo ${CXR_NUMBER_OF_LAYERS[${domain}]}
	
}

################################################################################
# Function: cxr_common_get_max_x_dim
# 
# Returns the maximum x dimension of all grids 
# Useful for compiling CAMx
#
################################################################################
function cxr_common_get_max_x_dim()
################################################################################
{
	local new
	local max_xdim=0
	
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(cxr_common_get_x_dim $i)"
		
		if [[ "$new" -gt "$max_xdim"  ]]
		then
			max_xdim=$new
		fi
	done
	
	echo ${max_xdim}
}

################################################################################
# Function: cxr_common_get_max_y_dim
# 
# Returns the maximum y dimension of all grids 
# Useful for compiling CAMx
#
################################################################################
function cxr_common_get_max_y_dim()
################################################################################
{
	local new
	local max_ydim=0
	
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(cxr_common_get_y_dim $i)"
		
		if [[ "$new" -gt "$max_ydim"  ]]
		then
			max_ydim=$new
		fi
	done
	
	echo ${max_ydim}
}

################################################################################
# Function: cxr_common_get_max_z_dim
# 
# Returns the maximum z dimension of all grids 
# Useful for compiling CAMx
#
################################################################################
function cxr_common_get_max_z_dim()
################################################################################
{
	local new
	local max_zdim=0
	
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(cxr_common_get_z_dim $i)"
		
		if [[ "$new" -gt "$max_zdim"  ]]
		then
			max_zdim=$new
		fi
	done
	
	echo ${max_zdim}
}

################################################################################
# Function: cxr_common_get_num_cells_3d
# 
# Returns the sum of the number of cells in all grids (3D)
# Used by <cxr_common_predict_model_output_megabytes>
#
################################################################################
function cxr_common_get_num_cells_3d()
################################################################################
{
	local new
	local sum=0
	
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(cxr_common_get_z_dim $i)"
		
		sum=$(( $sum + ( $(cxr_common_get_x_dim $i) * $(cxr_common_get_y_dim $i) * $(cxr_common_get_z_dim $i) ) ))
		
	done
	
	echo $sum
}

################################################################################
# Function: cxr_common_get_num_cells_2d
# 
# Returns the sum of the number of cells in all grids (Just lowest layer)
# Used by <cxr_common_predict_model_output_megabytes>
#
################################################################################
function cxr_common_get_num_cells_2d()
################################################################################
{
	local new
	local sum=0
	
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(cxr_common_get_z_dim $i)"
		
		sum = $(( $sum + ( $(cxr_common_get_x_dim $i) * $(cxr_common_get_y_dim $i) ) ))
		
	done
	
	echo $sum
}

################################################################################
# Function: cxr_common_report_dimensions
# 
# Prints the dimensions of all the defined grids.
#
################################################################################
function cxr_common_report_dimensions()
################################################################################
{
	local i
	
	for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		cxr_main_logger -B $FUNCNAME "Grid dimensions domain ${i}:\nX: $(cxr_common_get_x_dim ${i})\nY: $(cxr_common_get_y_dim ${i})\nZ: $(cxr_common_get_z_dim ${i})"
	done
	
	cxr_main_logger -B $FUNCNAME "Total number of cells: $(cxr_common_get_num_cells_3d)"
	
}

################################################################################
# Function: cxr_common_evaluate_rule
# 
# Evaluates a filerule and returns its expansion. Removes syntactical fluff unknown
# to non-bashers.
# If it is a file rule, the file might be compressed, <cxr_common_try_decompressing_file> is called.
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
# >MY_DIR="$(cxr_common_evaluate_rule "$RULE" false "$rule_name")"
# >These  ^                                                ^
# >Ensure that the code does not fail if the string returned contains
# >spaces.
#
# This code accepts an empty string:
# >MY_DIR="$(cxr_common_evaluate_rule "$RULE" true "$rule_name")" 
#
# This code is also valid and will not fail on empty expansion:
# >MY_DIR="$(cxr_common_evaluate_rule "$RULE")"
#
# You can use it to generate any string that is made up by variables,
# but make sure that control sequences like \n are double-escaped (\\n)
# because the expansion otherwise removes the sequence:
#
# >CXR_FINISH_MESSAGE_RULE='Please copy this into https://wiki.intranet.psi.ch/twiki/LAC/CAMxRuns \\n \| $(date +"%Y/%M/%D") \| ${USER} \| ${CXR_STATUS} \| ${CXR_RUN} \| ${CXR_START_DATE} \| ${CXR_STOP_DATE} \| http://people.web.psi.ch/oderbolz/CAMx/conf/$(basename $CXR_CONFIG) \| http://people.web.psi.ch/oderbolz/CAMx/log/$(basename $CXR_LOG) \| \\n'
# ...
# >cxr_main_logger "$FUNCNAME" "$(cxr_common_evaluate_rule "$CXR_FINISH_MESSAGE_RULE" true CXR_FINISH_MESSAGE_RULE)"
#
# Parameters:
# $1 - The rule to be evaluated (a string, not a variable)
# [$2] - allow_empty if false, a rule must expand to a non-empty string
# [$3] - optional name of the rule
# [$4] - try_decompression if false, will not attempt compression (and consequenital renaming)
################################################################################
function cxr_common_evaluate_rule()
################################################################################
{
	if [[  $# -lt 1 && $# -gt 4 ]]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - needs at least string (the rule) as input, at most the rule, true/false, the rule name and true/false!"
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
		cxr_main_logger -v "$FUNCNAME"  "rule $rule_name was empty..."
		echo ""
		return
	fi
	
	cxr_main_logger -v "$FUNCNAME"  "Evaluating rule $rule_name $rule..."

	# Original code: CXR_ROOT_OUTPUT=$(eval "echo $(echo $CXR_ROOT_OUTPUT_FILE_RULE)")
	expansion="$(eval "echo $(echo "$rule")")"
	
	cxr_main_logger -v "$FUNCNAME" "Evaluated rule: $expansion"
	
	# *_FILE_RULE might be compressed
	# Does the name of the rule end in _FILE_RULE ?
	if [[ "${rule_name: -10}" == "_FILE_RULE"  ]]
	#                 ¦
	# This space here ¦ is vital, otherwise, bash thinks we mean a default (see http://tldp.org/LDP/cxr_common_abs/html/string-manipulation.html)
	then
		if [[ "${try_decompression}" == true  ]]
		then
	
			# Try to decompress
			expansion=$(cxr_common_try_decompressing_file $expansion)
			
		else
			cxr_main_logger -v "$FUNCNAME" "No decompression attempted."
		fi # try_decompression
	fi
	
	cxr_main_logger -v "$FUNCNAME" "Evaluated rule: $expansion"
	
	if [[  -z "$expansion" && "$allow_empty" == false   ]]
	then
		# Empty not allowed
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - Rule $rule_name ($rule) was expanded to the empty string which is not allowed in this context!"
	fi
	
	echo "$expansion"
	
}

################################################################################
# Function: cxr_common_evaluate_rule
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
#
################################################################################
function cxr_common_evaluate_rule_at_offset()
################################################################################
{
	if [[  $# -lt 2 && $# -gt 4   ]]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - needs at least one string (the rule) and one number (the day offset) as input!"
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
	cxr_common_set_date_variables "${CXR_START_DATE}" "${day_offset}"
	
	# Evaluate the rule
	expansion=$(cxr_common_evaluate_rule "${rule}" "${3:-}" "${4:-}")
	
	# Reset Current offset
	CXR_DAY_OFFSET=${current_offset}
	
	# Reset the date vars
	cxr_common_set_date_variables "${CXR_START_DATE}" "${CXR_DAY_OFFSET}"
	
	echo "$expansion"
}

################################################################################
# Function: cxr_common_evaluate_these_scalar_rules
# 
# Evaluates all CXR_*_RULE environment variables in the given list.
#
# Arrays can *not* expanded using this technique!
#
# $1 - The list of rules to be evaluated (a space-separated string, not a variable)
# [$2] - allow_empty if false, *all* rules in the must expand to a non-empty string
################################################################################
function cxr_common_evaluate_these_scalar_rules()
################################################################################
{
	if [[  $# -lt 1 && $# -gt 2   ]]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - needs a string (the list of rules) as input and optionally a boolean allow_empty value!"
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
		export $variable="$(cxr_common_evaluate_rule "${current_rule}" "$allow_empty" $current_rule)"
	done
}


################################################################################
# Function: cxr_common_create_dummyfile
#
# Creates a dummy file, shows a message and adds the file
# to the dummy file list. 
#
# Parameters:
# $1 - Filename
################################################################################
function cxr_common_create_dummyfile()
################################################################################
{
	local filename="$1"
	cxr_main_logger "$FUNCNAME" "Creating Dummy $filename"
	
	# Add to dummy list 
	echo "This is a Dummy output file generated by a dry-run of CAMxRunner" > "$filename"
	
	# Store Dummy file in the file list
	echo "$filename" >> "$CXR_INSTANCE_FILE_DUMMY_LIST"

	return 0
}

################################################################################
# Function: cxr_common_create_tempfile
#
# Returns the name of a temporary file with random name, shows a message and adds the file
# to the temp file list if this is needed. 
# Replaces calls to mktemp and removes the need to remove the temp files, this is done by 
# <cxr_common_remove_tempfiles>
#
# Recommended call:
# >TMPFILE=$(cxr_common_create_tempfile $FUNCNAME)
#
# Parameters:
# [$1] - identifier of tempfile, recommended to use $FUNCNAME
# [$2] - store, if false, we do not add the file to list. This is useful if we keep track of the files ourselves (e. g. when decompressing files)
################################################################################
function cxr_common_create_tempfile()
################################################################################
{
	
	if [[ ! -d "${CXR_TMP_DIR}"  ]]
	then
		mkdir -p "${CXR_TMP_DIR}"
	fi
	
	# Check if that worked!
	if [[ ! -d "${CXR_TMP_DIR}"  ]]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - could not create tmp directory ${CXR_TMP_DIR} (maybe its a broken Link?), CAMxRunner cannot continue."
	fi
	
	local store=${2:-true}
	
	# Create a template by using $1 
	# and adding 8 random alphanums
	# This way, the filename has a meaning
	local template="${CXR_TMP_DIR}/${CXR_TMP_PREFIX}${1:-temp}.XXXXXXXX"

	# replace eventual spaces by _
	template=${template// /_}
	
	local filename=$(mktemp $template)
	cxr_main_logger -v "$FUNCNAME"  "Creating temporary file $filename"
	
	if [[ "${store}" == true  ]]
	then
		# Add to dummy list
		echo $filename >> "$CXR_INSTANCE_FILE_TEMP_LIST"
	fi
	
	echo $filename
	return 0
}

################################################################################
# Function: cxr_common_remove_tempfiles
#
# Removes all files from the temp file list if CXR_REMOVE_TEMP_FILES is true.
# Also removes decompressed files if requested.
#
################################################################################
function cxr_common_remove_tempfiles()
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
			cxr_main_logger "$FUNCNAME" "Removing temporarily decompressed files..."
		
			# Loop trough all entries
			while read line 
			do
				# The line has the format compressed_file|decompressed_file
				filename=$(echo "$line" | cut -d${CXR_DELIMITER} -f2)
				
				cxr_main_logger -v "$FUNCNAME"  "Deleting $filename"
				rm -f "${filename}"
				
				# remove that line via sed
				sed_tmp=$(cxr_common_create_tempfile sed false) # Tempfile is not added to list (we move it away)
				sed '/$line/d' "${CXR_DECOMPRESSED_LIST}" > "${sed_tmp}"
				mv "${sed_tmp}" "${CXR_DECOMPRESSED_LIST}"
			done < "$CXR_DECOMPRESSED_LIST"
		fi
	else
		cxr_main_logger "$FUNCNAME" "The temporarily decompressed files \n$(cat ${CXR_DECOMPRESSED_LIST} | cut -d${CXR_DELIMITER} -f 2 2>/dev/null )\n will not be deleted because the variable CXR_REMOVE_DECOMPRESSED_FILES is false."
	fi

	# remove temporary files, if wanted
	if [[ "$CXR_REMOVE_TEMP_FILES" == true  ]]
	then
		# Does the list even exist?
		if [[ -s "$CXR_INSTANCE_FILE_TEMP_LIST"  ]]
		then
			cxr_main_logger "$FUNCNAME" "Removing temporary files..."
			
			# Clean temp files away
			for temp_file in $(cat "${CXR_INSTANCE_FILE_TEMP_LIST}")
			do
				cxr_main_logger -v "$FUNCNAME"  "Deleting $temp_file"
				
				rm -f "$temp_file" >/dev/null 2>&1
			done
			
			# Empty the list of temp files just cleaned
			: > ${CXR_INSTANCE_FILE_TEMP_LIST}
		fi
	else
		cxr_main_logger "$FUNCNAME" "The temporary files $(cat ${CXR_INSTANCE_FILE_TEMP_LIST} 2>/dev/null ) will not be deleted because the variable CXR_REMOVE_TEMP_FILES is false."
	fi
	

}

################################################################################
# Function: cxr_common_get_lock
#
# Blocking call to the lockmanager. If we get the lock, it will be added to the 
# CXR_INSTANCE_FILE_LOCK_LIST. This allows to release the locks later when we exit from the program
# When the lock is not free, this call waits forever.
#
# Recommended call:
# > cxr_common_get_lock lockname
#
# Parameters:
# $1 - the name of the lock to get
################################################################################
function cxr_common_get_lock()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - needs the name of a lock as input"
	fi
	
	local lock="$1"

	# For debug reasons, locking can be turned off
	if [[ $CXR_NO_LOCKING == false  ]]
	then
		cxr_main_logger -v "$FUNCNAME"  "Waiting to set lock $lock..."
	
		$CXR_LOCK_MAN_EXEC set "$lock" -1
		
		cxr_main_logger -v "$FUNCNAME"  "Got lock $lock."
		
		if [[ $(grep -c -e "^$lock\$") -ne 0  ]]
		then
			# lock already in list (should not happen!)
			cxr_main_logger "$FUNCNAME" "Weird: it seems as if the lock $lock was given out more than once!"
		else
			# Put the new lock on the list
			echo "$lock" >> $CXR_INSTANCE_FILE_LOCK_LIST
		fi
	fi
}

################################################################################
# Function: cxr_common_release_lock
#
# Releases a lock and removes it from CXR_INSTANCE_FILE_LOCK_LIST.
#
# Recommended call:
# > cxr_common_release_lock lockname
#
# Parameters:
# $1 - the name of the lock to release
################################################################################
function cxr_common_release_lock()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		cxr_main_die_gracefully "$FUNCNAME:$LINENO - needs the name of a lock as input"
	fi
	
	local sed_tmp=$(cxr_common_create_tempfile $FUNCNAME)
	
	lock="$1"
	
	cxr_main_logger -v "$FUNCNAME"  "Waiting to release lock $lock..."

	# We even release locks if locking is turned off
	$CXR_LOCK_MAN_EXEC unset "$lock"
	
	cxr_main_logger -v "$FUNCNAME"  "lock $lock released."
	
	# Remove this line from the lock-list
	sed "/^$lock\$/d" $CXR_INSTANCE_FILE_LOCK_LIST > $sed_tmp
	
	# Exchange tempfile and new file
	mv $sed_tmp $CXR_INSTANCE_FILE_LOCK_LIST
	
}

################################################################################
# Function: cxr_common_release_all_locks
#
# Releases all locks held by iterating over CXR_INSTANCE_FILE_LOCK_LIST.
#
#
# Parameters:
# None.
################################################################################
function cxr_common_release_all_locks()
################################################################################
{
	local lock
	
	if [[ -s "$CXR_INSTANCE_FILE_LOCK_LIST"  ]]
	then
		cxr_main_logger "$FUNCNAME" "Releasing all locks..."
	
		for lock in $(cat $CXR_INSTANCE_FILE_LOCK_LIST)
		do
			cxr_common_release_lock $lock
		done
	fi
}

################################################################################
# Function: cxr_common_create_config_file
#
# Interactively creates a new configuration file for a run. 
#
# Can derive a new file from any given config file - also takes care of expansion.
# Also can create a new file using an .ask file, similar to the installer of CAMxRunner
#
# Parameters:
# $1 - Run-name
################################################################################	
function cxr_common_create_config_file() 
################################################################################
{
	local run="$1"
	local basefile
	local template
	local destination
	local askfile
	local playfile
	local tmpfile
	
	if [[ $(cxr_common_get_consent "We create a configuration file for the new run now.\n Do you want to copy an existing file? (If you say no, you will be asked the values of the new configuration instead)" ) == true  ]]
	then

		# Show a list of existing files to choose from
		if [[ $(cxr_common_get_consent "Do you want to use a file other than \n $(basename ${CXR_BASECONFIG}) as as starting point?" ) == false  ]]
		then
			#No, use base.conf
			basefile=${CXR_BASECONFIG}
		else
			#Yes, gimme options
			
			# To keep the list compact, we go into the conf dir and back out again
			cd "${CXR_CONF_DIR}" || cxr_main_die_gracefully "$FUNCNAME:$LINENO - Could not change to ${CXR_CONF_DIR}!"
			
			basefile=${CXR_CONF_DIR}/$(cxr_common_get_menu_choice "Choose a file I should use:" "*.conf" )
			
			cd "$CXR_RUN_DIR" || cxr_main_die_gracefully "Could not change to $CXR_RUN_DIR"
		fi
	
		if [[ ! -f "$basefile"  ]]
		then
			cxr_main_die_gracefully "File $basefile is not readable!"
		fi
	
		# For the moment, I romoved the option to expand a config
		# Tell user if expand is on and let the user decide
		
#		if [ $(cxr_common_get_consent "Do you want to expand the new configuration?\n \
#This means that any variable in $basefile that is not protected by \
#single quotes will be expanded to its value and then be written to the new configuration file. \
#This makes everything in it static, allowing you to preserve the actual settings used. \
#However, this can also be a disadvantage!" N ) == true ]
#		then
#			# Yes, expand.
#			cxr_common_expand_config ${CXR_BASECONFIG} ${CXR_EXPANDED_CONFIG}
#		else
			#No expansion, just copy.
		
			# Is the file already there?
			if [[ -f ${CXR_CONFIG}  ]]
			then
				# Continue even if file is there?
				if [[ $(cxr_common_get_consent "${CXR_CONFIG} already exists. Do you want to overwrite this file?" ) == false  ]]
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
			if [[ $(cxr_common_get_consent "$destination already exists. Do you want to overwrite this file?" N ) == false  ]]
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
			if [[ "$(cxr_common_get_consent "Such a config file was already created. Do you want to look at the settings that where used then? (You will then be asked if you want to reinstall using those values)" Y )" == true  ]]
			then
				# Yes, show me
				cat "$playfile"
				
				if [[ "$(cxr_common_get_consent "Should this installation be repeated with the existing settings?" N )" == true  ]]
				then
					# Playback, do nothing
					:
				else
					# Redo
					cxr_common_get_answers "$askfile" "$playfile"
				fi
			else
				# Redo
				cxr_common_get_answers "$askfile" "$playfile"
			fi
		else
			# Start from scratch
			cxr_common_get_answers "$askfile" "$playfile"
		fi

		cxr_common_apply_playfile $playfile 
		
		# We need this set later
		CXR_CONFIG=$destination

		# Should we add more tests?
		
	fi # Decision if copy or .ask
	
	cxr_main_logger "$FUNCNAME" "$FUNCNAME:$LINENO - Edit the config file ${CXR_CONFIG} if needed - else just dry-run the script: \n \$ \t ${run} -d";
}

################################################################################
# Function: cxr_common_expand_config
#
# Replaces and non-single quoted variable in a config file by its value.
# This allows to preserve a config file (it is then independent of base.conf).
# Interactive function.
#
# Parameters:
# $1 - File to expand
# $2 - resulting output file
################################################################################
function cxr_common_expand_config() 
################################################################################
{
	local basefile=$1
	local expanded_config=$2
	
	# Is the file already there?
	if [[ -f ${expanded_config}  ]]
	then
		# Continue even if file is there?
		if [[ $(cxr_common_get_consent "${expanded_config} already exists. Do you want to overwrite this file?" N ) == false  ]]
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
# Function: cxr_common_get_model_id
#
# Each model has a 0-based id (used for example to index CXR_SUPPORTED_MODEL_VERSIONS)
#
# Parameters:
# $1 - Exact name of model to use
################################################################################	
function cxr_common_get_model_id() 
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
# Function: cxr_common_create_new_run
#
# Creates a new run by creating an appropriate link and calling <cxr_common_create_config_file>
#
# Tolerates runs that already exist (asks user).
#
################################################################################	
function cxr_common_create_new_run() 
################################################################################
{

	local model=$(cxr_common_get_menu_choice "Which model should be used?\nIf your desired model is not in this list, adjust CXR_SUPPORTED_MODELS \n(Currently $CXR_SUPPORTED_MODELS)" "$CXR_SUPPORTED_MODELS" )
	
	local model_id=$(cxr_common_get_model_id "$model") || cxr_main_die_gracefully "model $model is not known."
	
	# Extract the list of supported versions
	local supported="${CXR_SUPPORTED_MODEL_VERSIONS[${model_id}]}"
	
	#Generate a menu automatically
	local version=$(cxr_common_get_menu_choice "Which version of $model should be used?\nIf your desired version is not in this list, adjust CXR_SUPPORTED_MODEL_VERSIONS \n(Currently $supported)" "$supported" )
	
	cxr_common_is_version_supported $version $model || cxr_main_die_gracefully "The version you supplied is not supported. Adjust CXR_SUPPORTED_MODEL_VERSIONS."
	
	local run=${model}-v${version}
	
	local addition
	
	echo "The Run name so far is ${run}- what do you want to add?"
	read addition

	run="${run}-$addition"

	# Name ok? ###################################################################
	cxr_common_check_run_name $run || cxr_main_die_gracefully "$FUNCNAME:$LINENO - The name supplied does not contain a proper CAMx version. Rerun using $0 -C to be guided inturactively"
		
	# Name OK.
	
	# Extract and export model name and version 
	cxr_main_determine_model_and_version $run
	
	# This is the file name of an extended config
	CXR_EXPANDED_CONFIG=${CXR_CONF_DIR}/${run}.econf

	# This is a "normal" copy
	CXR_CONFIG=${CXR_CONF_DIR}/${run}.conf
	
	#name is OK - create link ##################################################
	if [[ ! -L $run  ]]
	then
		cxr_main_logger "$FUNCNAME"  "Creating link $run."
			
		ln -s $CXR_RUNNER_NAME $run
		
	else
		# run already existists, OK? ###############################################
		if [[ $(cxr_common_get_consent "$run already exists. Do you want to continue \n (Makes sense if you regenerate the configuration)?" N ) == false  ]]
		then
			# No
			exit
		fi
	fi
		
	# Create a configuration #####################################################
	cxr_common_create_config_file $run
	
	# Messages & Good wishes #####################################################
	cxr_main_logger "$FUNCNAME" "New run was created, start it with \n\t \$ $run -d"

}

################################################################################
# Function: cxr_common_check_runner_consistency
#	
# A Quick check to see if the CAMxRunner installation is OK
# and consistent with config (can be extended...)
################################################################################
function cxr_common_check_runner_consistency() 
################################################################################
{
	# Each directory in $CXR_RUN_SUBDIRS must exist
	local dir
	local subdir
	
	# Increase global indent level
	cxr_main_increase_log_indent

	cxr_main_logger "$FUNCNAME"  "Checking if subdirectories exist..."
	
	for SUBIDR in $CXR_RUN_SUBDIRS
	do
		# Increase global indent level
		cxr_main_increase_log_indent

		if [[ ! -d $CXR_RUN_DIR/$SUBIDR  ]]
		then
			# Oh Oh!
			mkdir -p $CXR_RUN_DIR/$SUBIDR
			cxr_main_logger "$FUNCNAME" "$FUNCNAME:$LINENO - The directory $CXR_RUN_DIR/$SUBIDR did not exist. According to the Variable CXR_RUN_SUBDIRS it should exist, however. I created it now, but you need to fill it with sensible stuff!" 
			
		else 

			cxr_main_logger -v "$FUNCNAME"  "The directory $CXR_RUN_DIR/$SUBIDR exists"

		fi
		
		# Decrease global indent level
		cxr_main_decrease_log_indent
	done
	
	# Check executables
	
	############################################################################
	cxr_main_logger "$FUNCNAME"  "Checking if all executables are present and executable..."
	
	# Increase global indent level
	cxr_main_increase_log_indent
	
	########################################
	cxr_main_logger "$FUNCNAME"  "Checking external files..."
	########################################
	
	# Increase global indent level
	cxr_main_increase_log_indent
	
	cxr_common_check_environment_executables
	
	# Decrease global indent level
	cxr_main_decrease_log_indent
	
	# Decrease global indent level
	cxr_main_decrease_log_indent
	
	# Decrease global indent level
	cxr_main_decrease_log_indent
	############################################################################
	
	# Each directory in $CXR_RUN_VERSION_SUBDIRS must have 
	# one subdir for each model and each version 
	
	cxr_main_logger "$FUNCNAME"  "Checking if version sub-subdirectories exist..."
	
	for subdir in $CXR_RUN_VERSION_SUBDIRS
	do
		
		# Increase global indent level
		cxr_main_increase_log_indent
		
		cxr_main_logger -v "$FUNCNAME"  "Checking subdirs of $subdir..."
		
		for model in $CXR_SUPPORTED_MODELS
		do
		
			cxr_main_logger -v "$FUNCNAME"  "Checking model $model..."
		
			# Get id of the current model
			model_id=$(cxr_common_get_model_id "$model") || cxr_main_die_gracefully "model $model is not known."
	
			# Extract the list of supported versions
			supported="${CXR_SUPPORTED_MODEL_VERSIONS[${model_id}]}"
	
			for version in $supported
			do
				
				cxr_main_logger -v "$FUNCNAME"  "Checking version $version..."
				
				dir=$CXR_RUN_DIR/$subdir/$model/$version
			
				if [[ ! -d $dir  ]]
				then
					# Oh Oh!
					mkdir -p $dir
					cxr_main_logger "$FUNCNAME" "$FUNCNAME:$LINENO - The directory $dir did not exist. All directories stored in CXR_RUN_VERSION_SUBDIRS need a subdirectory for each supported version of model and each supported version  (stored in CXR_SUPPORTED_MODEL_VERSIONS).\n I created this one now, but if you want to use model $model $version you need to fill it with sensible stuff!"
				else
	
					cxr_main_logger -v "$FUNCNAME"  "The directory $CXR_RUN_DIR/$subdir/$version exists"
	
				fi
			done # Versions
			
		done # model
		
		# Decrease global indent level
		cxr_main_decrease_log_indent
	done # Directory
	
	# Decrease global indent level
	cxr_main_decrease_log_indent
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
		while [ $(pwd) != / ]
		do
			cd ..
			# If we find CAMxRunner, we are there
			ls CAMxRunner.sh >/dev/null 2>&1 && break
			
			# If we are in root, we have gone too far
			if [[ $(pwd) == /  ]]
			then
				echo "Could not find CAMxRunner.sh!"
				exit 1
			fi
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
	
	is $(cxr_common_evaluate_rule a) a "cxr_common_evaluate_rule constant"
	is $(cxr_common_evaluate_rule "$(cxr_common_abs -100)") 100 "cxr_common_evaluate_rule a function of CAMxRunner"
	is $(cxr_common_evaluate_rule "domain$(cxr_common_n_digits $i 3)") domain001 "cxr_common_evaluate_rule with formatting"
	is $(cxr_common_evaluate_rule "$(uname -n)") $(uname -n) "cxr_common_evaluate_rule with uname"
	
	########################################
	# teardown tests if needed
	########################################

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


