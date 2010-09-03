# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
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

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=4

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains support functions for the CAMxRunner (creation of new runs, module calls, process management, temporary file management)"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'


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
	local domain
	local xdim
	
	domain=${1}
		
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
		#                                                                                      |                                             |
		#                                                                                  Fencepost                                         |
		#                                                                                                                   Buffer Cells (left/right)
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
	local domain
	local ydim
	
	domain=${1:-0}
	
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
	local max_xdim
	local iGrid
	
	max_xdim=0
	
	for iGrid in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(common.runner.getX $iGrid)"
		
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
	local max_ydim
	local iGrid
	
	max_ydim=0
	
	for iGrid in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(common.runner.getY $iGrid)"
		
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
	local max_zdim
	local iGrid
	
	max_zdim=0
	
	for iGrid in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		new="$(common.runner.getZ $iGrid)"
		
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
	local sum
	local iGrid
	
	sum=0
	
	for iGrid in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		sum=$(( $sum + ( $(common.runner.getX $iGrid) * $(common.runner.getY $iGrid) * $(common.runner.getZ $iGrid) ) ))
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
	local sum
	local iGrid
	
	sum=0
	
	for iGrid in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		sum = $(( $sum + ( $(common.runner.getX $iGrid) * $(common.runner.getY $iGrid) ) ))
	done
	
	echo $sum
}

################################################################################
# Function: common.runner.reportDimensions
# 
# Prints the dimensions and locations of all the defined grids.
#
################################################################################
function common.runner.reportDimensions()
################################################################################
{
	local iGrid
	local nCells
	local sw
	local ne
	local x
	local y
	local z
	
	for iGrid in $(seq 1 $CXR_NUMBER_OF_GRIDS);
	do
		# Get extent
		x=$(common.runner.getX ${iGrid})
		y=$(common.runner.getY ${iGrid})
		z=$(common.runner.getZ ${iGrid})
		
		main.log -a -B "Grid dimensions domain ${iGrid}:"
		main.log -a "X: ${x}\nY: ${y}\nZ: ${z}\n"
		
		main.log -a -B "Lon/Lat Corners ${iGrid}:"
		sw="$(common.map.indexesToLonLat 1 1 $iGrid)"
		
		# we add one because otherwise we get the lower left 
		# corner of the upper right most cell (see <common.map.indexesToLonLat>)
		ne="$(common.map.indexesToLonLat $(( $x + 1 )) $(( $y + 1 ))$iGrid)"
		
		main.log -a "south-west corner: $sw\nnorth-east corner: $ne\n"
	done
	
	nCells="$(common.runner.countCells3D)"
	
	main.log -B "Total number of cells: $nCells"
	
	# Set factor to correct times (integer division)
	CXR_TIME_NORM_FACTOR=$(( $nCells / $CXR_TIME_PER_CELLS ))
	
	# This factor must be >= 1
	if [[ $CXR_TIME_NORM_FACTOR -lt 1 ]]
	then
		CXR_TIME_NORM_FACTOR=1
	fi
	
	main.log -v "Time normalisation factor: $CXR_TIME_NORM_FACTOR"
	
}

################################################################################
# Function: common.runner.printSummary
# 
# Lists any relevant data on this run.
#
################################################################################
function common.runner.printSummary()
################################################################################
{
	##################
	# Revision control
	##################
	
	# Get revisions of configuration and the CAMxRunner.sh
	# The other variables are set in main.readConfig
	CXR_RUNNER_REV=$(main.getRevision $0)
	
	main.log -v -B "Runner (${CXR_RUN}) revision ${CXR_RUNNER_REV}" 
	
	if [[ "$CXR_BASECONFIG_REV" -gt "$CXR_CONFIG_REV" ]]
	then
		main.log -w "The Configuration file $(basename ${CXR_CONFIG}) \n was derived from an older revision ($CXR_CONFIG_REV) of the $CXR_BASECONFIG file (current revision: ${CXR_BASECONFIG_REV}).\n this is not necessarily bad, but check if the two files agree logically (e. g. using diff) \n\n To recreate the config, consider to rename the existing configuration and do a dry-run: \n \t \$ mv ${CXR_CONFIG} ${CXR_CONFIG}.old \n \t \$ $0 -d\n"	 
	fi
	
	# Look at the system load
	load=$(common.performance.getReaLoadPercent)
	main.log -a "System Load (Memory & CPU): $load %"

	mb_needed=$(common.check.PredictModelOutputMb)
	main.log "I estimate that this simulation will take ${mb_needed} MB of space in ${CXR_OUTPUT_DIR}."

	# Show grid dimensions
	common.runner.reportDimensions
	
	main.log -B "Using $CXR_NUMBER_OF_OUTPUT_SPECIES output species"
	
	if [[ "${CXR_LOG_LEVEL_SCREEN}" -ge "${CXR_LOG_LEVEL_VRB}"  ]]
	then
		common.variables.list
		
		common.variables.listSystemVars
		
		main.log -v -b "Bash stack size: $(ulimit -s)" 
	fi
	
	# Check if the selected binary supports our settings
	common.check.ModelLimits
}

################################################################################
# Function: common.runner.evaluateRule
# 
# Evaluates a filerule and returns its expansion. 
# This function is needed because we want to expand a rule in a controlled way, so that
# we can for example transparently decompress files or detect if a rule does not expand properly.
# 
# Note that the expansion of a rule depends on the environment (e. g. the value of date variables
# or other global variables). This makes it hard to cache the output of this function.
#
# Side effect: if the it is a _FILE_RULE and the file is compressed and we cannot decompress in place,
# the returned file name will change. If you want the "expected" file name,
# use the fourth parameter.
# ABSOLUTELY use this parameter for any OUTPUT_FILE because if the output would have been 
# compressed, CAMxRunner would decompress it, wich makes no sense.
# 
#
# The evaluator tests if the resulting dirname exists (_FILE_RULE only). This is needed if your rules
# contain things like /${VAR}/... because we have no way of knowing this name in the checker.
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
# [$4] - try_decompression if false (default true), will not attempt compression (and consequenital renaming)
################################################################################
function common.runner.evaluateRule()
################################################################################
{
	if [[  $# -lt 1 || $# -gt 4 ]]
	then
		main.dieGracefully "needs at least string (the rule) as input, at most the rule, true/false, the rule name and true/false!"
	fi	
	
	local rule
	local allow_empty
	local rule_name
	local try_decompression
	local expansion
	
	rule="$1"

	# Per default we allow rules to expand to the empty string
	allow_empty="${2:-true}"
	rule_name="${3:-}"
	# By default try decompression
	try_decompression="${4:-true}"
	
	if [[ -z "$rule" ]]
	then
		# If the rule is empty, we evaluate to empty
		# we decide below if this is a problem
		main.log -v "rule $rule_name was empty..."
		expansion=""
	else
		# Non-empty rule - do it

		# Original code example: CXR_ROOT_OUTPUT=$(eval "echo $(echo $CXR_ROOT_OUTPUT_FILE_RULE)")
		expansion="$(eval "echo $(echo "$rule")")"
		
		# *_FILE_RULE might be compressed
		# Does the name of the rule end in _FILE_RULE ?
		if [[ "${try_decompression}" == true && "${rule_name: -10}" == "_FILE_RULE" ]]
		#                                                    ¦
		#                                    This space here ¦ is vital, otherwise, 
		#                                    bash thinks we mean a default (see http://tldp.org/LDP/common.math.abs/html/string-manipulation.html)
		then
			# Try to decompress
			expansion=$(common.fs.TryDecompressingFile $expansion)
		fi
		
		main.log -v "rule $rule_name expanded to $expansion"
	fi
	
	# Test if expansion is empty but shouldn't
	if [[ -z "$expansion" ]]
	then
		# Empty
		if [[ "$allow_empty" == false ]]
		then
			# Empty not allowed
			main.dieGracefully "Rule $rule_name was expanded to the empty string which is not allowed in this context!"
		fi
	else
		# Not empty. Test if the dirname exists, but only if its a FILE_RULE
		# Does the name of the rule end in _FILE_RULE ?
		if [[ "${rule_name: -10}" == "_FILE_RULE" ]]
		#                  ¦
		# This space here  ¦ is vital, otherwise, bash thinks we mean a default (see http://tldp.org/LDP/common.math.abs/html/string-manipulation.html)
		then
			expansion_dir="$(dirname "$expansion")"
			if [[ ! -d "${expansion_dir}" ]]
			then
				# Dirname does not exists
				main.log -w "Dir $expansion_dir does not exist - creating it..."
				mkdir -p "$expansion_dir"
			fi
		fi
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
	if [[  $# -lt 2 || $# -gt 4 ]]
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
	if [[  $# -lt 1 || $# -gt 2   ]]
	then
		main.dieGracefully "needs a string (the list of rules) as input and optionally a boolean allow_empty value!"
	fi
	
	local 
	local current_rule
	variable
	local allow_empty
	
	
	rule_list="$1"
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
	local filename
	local size
	
	filename="$1"
	size="${2:-1}"
	
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
	
	# Store Dummy file in the file hash (dummy value)
	common.hash.put $CXR_INSTANCE_HASH_DUMMY_FILES $CXR_LEVEL_INSTANCE $filename dummy

	return 0
}

################################################################################
# Function: common.runner.createTempFile
#
# Returns the name of a temporary file with random name, shows a message and adds the file
# to the temp file list if this is needed. 
# Note that we cannot use a hash to store tempfiles since otherwise a deadlock would occur.
# 
# If your tempfile is used to call an executable like AHOMAP, use the wrapper <common.runner.createJobFile>
#
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
	local store
	local template
	local filename
	
	if [[ ! -d "${CXR_TMP_DIR}" ]]
	then
		mkdir -p "${CXR_TMP_DIR}"
	fi
	
	# Check if that worked!
	if [[ ! -d "${CXR_TMP_DIR}" ]]
	then
		main.dieGracefully "could not create tmp directory ${CXR_TMP_DIR} (maybe its a broken Link?), CAMxRunner cannot continue."
	fi
	
	store=${2:-true}
	
	# Create a template by using $1 
	# and adding 8 random alphanums
	# This way, the filename has a meaning
	template="${CXR_TMP_DIR}/${CXR_TMP_PREFIX}${1:-temp}.XXXXXXXX"

	# replace eventual spaces by _
	template=${template// /_}
	
	filename=$(mktemp $template)
	main.log -v "Creating temporary file $filename"
	
	if [[ "${store}" == true ]]
	then
		# Add to list
		echo $filename >> $CXR_INSTANCE_FILE_TEMP_LIST
	fi
	
	echo $filename
	return 0
}

################################################################################
# Function: common.runner.createJobFile
#
# A special variety of <common.runner.createTempFile> that sould be used whenever
# a jobfile for an application (such as AHOMAP) is needed. These tempfiles
# are stored in $CXR_JOBFILE_DIR.
# These are not put into the templist, becase we want to keep them.
#
# Recommended call:
# >exec_temp_file=$(common.runner.createJobFile AHOMAP)
#
# Parameters:
# [$1] - identifier of tempfile, use name of target executable if possible
################################################################################
function common.runner.createJobFile()
################################################################################
{
	local job
	local jobfile
	
	if [[ ! -d "$CXR_JOBFILE_DIR" ]]
	then
		mkdir -p "$CXR_JOBFILE_DIR"
	fi
	
	job="$1"
	jobfile=$(common.runner.createTempFile "$job" false)
	
	mv $jobfile "$CXR_JOBFILE_DIR" || main.dieGracefully "Could not move $jobfile to $CXR_JOBFILE_DIR"
	jobfile=$CXR_JOBFILE_DIR/$(basename $jobfile)
	
	main.log -v "Created this jobfile for $job: $(cat $jobfile)"
	
	echo $jobfile
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
	local temp_file
	local arrKeys
	local keyString
	local arrKeys
	local keyString
	
	# remove decompressed files, if wanted
	if [[ "$CXR_REMOVE_DECOMPRESSED_FILES" == true ]]
	then
			main.log  "Removing temporarily decompressed files..."
			
				# common.hash.getKeysAndValues returns a newline-separated list
			oIFS="$IFS"
			IFS='
'
			
			# loop through compressed_filename|filename pairs
			for pair in $(common.hash.getKeysAndValues CXR_GLOBAL_HASH_DECOMPRESSED_FILES $CXR_LEVEL_GLOBAL)
			do
				# Reset IFS
				IFS="$oIFS"
				
				# Parse the DB string
				oIFS="$IFS"
				IFS="$CXR_DELIMITER"
				
				set $pair
				
				compressed_filename="$1"
				filename="$2"
				# Reset IFS
				IFS="$oIFS"
				
				if [[ -e "$filename" ]]
				then
					main.log -v "Deleting $filename"
					rm -f "${filename}" &>/dev/null
				fi
			done
			
			IFS="$oIFS"
	else
		main.log  "The temporarily decompressed files will not be deleted because the variable CXR_REMOVE_DECOMPRESSED_FILES is false."
	fi
	
	# It is possible that 
	# the instance files where already deleted by another process
	
	if [[ -d "${CXR_INSTANCE_DIR}" ]]
	then
		#Make sure the list exists. 
		touch ${CXR_INSTANCE_FILE_TEMP_LIST}
	
		# remove temporary files, if wanted
		if [[ "$CXR_REMOVE_TEMP_FILES" == true ]]
		then
				main.log "Removing temporary files..."
				
				# Clean files away
				while read temp_file
				do
					if [[ -e "$temp_file" ]]
					then
						main.log -v "Deleting $temp_file"
						
						rm -f "$temp_file" &>/dev/null
					fi
				done < ${CXR_INSTANCE_FILE_TEMP_LIST}
				
				IFS="$oIFS"
		else
			main.log  "The temporary files will not be deleted because the variable CXR_REMOVE_TEMP_FILES is false."
		fi
	fi # is the instance dir still there
}

################################################################################
# Function: common.runner.getLockFile
#
# Returns the name of a lockfile to use.
#
# Parameters:
# $1 - the name of the lock to get
# $2 - the level of the lock, either of "$CXR_LEVEL_INSTANCE", "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
################################################################################
function common.runner.getLockFile()
################################################################################
{
	local level
	local lock
	local file
	
	lock="$1"
	level="$2"
	
	file="${lock}.lock"
	
	case $level in
		$CXR_LEVEL_INSTANCE) 	echo "$CXR_INSTANCE_DIR/$file" ;;
		$CXR_LEVEL_GLOBAL) 		echo "$CXR_GLOBAL_DIR/$file" ;;
		$CXR_LEVEL_UNIVERSAL) 	echo "$CXR_UNIVERSAL_DIR/$file" ;;
		*) main.dieGracefully "Lock level $level not supported";;
	esac
}

################################################################################
# Function: common.runner.waitForLock
#
# Waits until a lock is free to be aquired and acquries it if needed.
# When the lock is not free, we wait up to CXR_MAX_LOCK_TIME seconds, then return false in _retval
# If the lock is free, we wait for a random time between 0 and 1 sencond in order to separate 
# eventual concurrent processes and then may call this fuction again.
# We do not take any action if the lock is not free, this is up to tho user.
#
# Recommended call:
# > common.runner.waitForLock NextTask "$CXR_LEVEL_GLOBAL"
# > if [[ $_retval == false ]]
# > then
# > 	main.dieGracefully "Waiting for NextTask lock took too long"
# > fi
#
# Parameters:
# $1 - the name of the lock to get
# $2 - the level of the lock, either of "$CXR_LEVEL_INSTANCE", "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
# [$3] - an optional boolean (default true) indicating if we want to aquire the lock.
# [$4] - the time in decimal seconds waited so far (only relevant in the recursion case)
################################################################################
function common.runner.waitForLock()
################################################################################
{
	if [[ $# -lt 2 || $# -gt 4 ]]
	then
		main.dieGracefully "needs at least the name of a lock and a level as input, got $*"
	fi

	local lock
	local level
	local lockfile
	local time
	local shown

	lock="$1"
	level="$2"
	wanted="${3:-true}"
	# how long did we wait so far
	time="${4:-0}"
	
	shown=false
	
	lockfile="$(common.runner.getLockFile $lock $level)"

	########################################
	# We wait until lock is free
	########################################
	while [[ -f "$lockfile" ]]
	do
		if [[ $shown == false && $(common.math.FloatOperation "$time == 0" 0 false ) -eq 1 ]]
		then
			main.log -a "Waiting for lock $lock (level $level) ..."
			# Safe time thanks to short-circuit logic
			shown=true
		fi
		
		sleep $CXR_LOCK_SLEEP_SECONDS
		time=$(common.math.FloatOperation "$time + $CXR_LOCK_SLEEP_SECONDS" $CXR_NUM_DIGITS false )
		
		if [[ $(common.math.FloatOperation "$time > $CXR_LOCK_TIMEOUT_SEC" 0 false ) -eq 1 ]]
		then
			main.log -w "Lock $lock (${level}) took longer than CXR_MAX_LOCK_TIME to get!"
			_retval=false
			return $CXR_RET_OK
		fi
	done # is lock set?
	
	if [[ $wanted == false ]]
	then
		_retval=true
	else
		# We want to acquire the lock, we need to rule out 
		# competition
	
		# sleep some time between 0.01 and 0.5 seconds
		sleep $(common.math.RandomNumber 0.01 0.5 5)
		
		# There is a slight chance another process was faster
		if [[ -f "$lockfile" ]]
		then
			# Recursive call (retval is set internally)
			# it will set _retval itself
			common.runner.waitForLock $lock $level $wanted $time
		else
			_retval=true
		fi
	fi
}

################################################################################
# Function: common.runner.getLock
#
# Tries to get a lock. 
# Locks can have three levels (similar to hashes) 
# If we get the lock, a file in the appropiate directory is created and
# the path to the file is stored in the Instance Hash "Locks"
# These files can be purged by <common.runner.releaseAllLocks>
#
# The problem is that here, locking is not atomic, so 2 processes could try
# to get the same lock...
# TODO: Implement a general (n concurrent procs) Lamports Bakery, Peterson's or Dekkers algorithm
#
# Since SQLite seems to have some issues with locking, we must guard all write 
# access where we assume concurrency with a lock.
#
# Example:
# > if [[ $(common.runner.getLock $lock "$CXR_LEVEL_GLOBAL") == false ]]
# > then
# > 	main.dieGracefully "Could not get lock $lock"
# > fi
#
# Parameters:
# $1 - the name of the lock to get
# $2 - the level of the lock, either of "$CXR_LEVEL_INSTANCE", "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
################################################################################
function common.runner.getLock()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs the name of a lock and a level as input"
	fi
	
	local lock
	local level
	local turn
	local wait_array
	local arr_string
	
	lock="$1"
	level="$2"
	
	lockfile="$(common.runner.getLockFile "$lock" "$level")"

	# For debug reasons, locking can be turned off
	if [[ $CXR_NO_LOCKING == false ]]
	then
		
		# Wait for the lock, _retval is false if we exceeded the timeout
		common.runner.waitForLock "$lock" "$level"
		
		if [[ $_retval == false ]]
		then
			# Took to long...
			echo false
			return $CXR_RET_ERROR
		fi
		
		# We got the lock 
		# write our ID into the lockfile
		echo $CXR_INSTANCE > "$lockfile"
		
		main.log -v "lock $lock acquired."
		
	else
		main.log -w "CXR_NO_LOCKING is false, logging is turned off - no lock acquired."
	fi
	
	echo true
}

################################################################################
# Function: common.runner.releaseLock
#
# Releases a lock by deleting the relevant lock file.
#
# Recommended call:
# > common.runner.releaseLock lockname
#
# Parameters:
# $1 - the name of the lock to release
# $2 - the level of the lock, either of "$CXR_LEVEL_INSTANCE", "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
################################################################################
function common.runner.releaseLock()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs the name of a lock  and a lock-level as input"
	fi
	
	local lock
	local level
	
	lock="$1"
	level="$2"
	lockfile="$(common.runner.getLockFile $lock $level)"
	
	rm -f "$lockfile"
	
	main.log -v "lock $lock released."
}

################################################################################
# Function: common.runner.releaseAllLocks
#
# Releases all locks this instance holds by looking at the id in them.
#
#
# Parameters:
# None.
################################################################################
function common.runner.releaseAllLocks()
################################################################################
{
	local dirs
	local dir
	
	dirs="$CXR_INSTANCE_DIR $CXR_GLOBAL_DIR $CXR_UNIVERSAL_DIR"
	
	# Find all lock files that contain my PID and delete them
	for dir in $dirs
	do
		# Do this on all 3 levels
		find $dir -noleaf -name '*.lock' -exec grep -l $CXR_INSTANCE {} \; 2>/dev/null | xargs rm
	done
}

################################################################################
# Function: common.runner.getExistingConfigFile
#
# Interactively asks user for the name of an existing config file (no path)
#
################################################################################	
function common.runner.getExistingConfigFile() 
################################################################################
{
	local config
	
	# To keep the list compact, we go into the conf dir and back out again
	cd "${CXR_CONF_DIR}" || main.dieGracefully "Could not change to ${CXR_CONF_DIR}!"
		
	config=${CXR_CONF_DIR}/$(common.user.getMenuChoice "Choose an existing config file as starting point:" "*.conf" )
		
	cd "$CXR_RUN_DIR" || main.dieGracefully "Could not change to $CXR_RUN_DIR"
	
	echo $config
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
# [$2] - An existing run name to be used (if given, we ask no questions)
################################################################################	
function common.runner.createConfigFile() 
################################################################################
{
	local newRun
	local existingRun
	
	local basefile
	local template
	local destination
	local askfile
	local playfile
	local tmpfile
	
	newRun="$1"
	existingRun="${2:-}"
	
	if [[ "$existingRun" ]]
	then
		# Make sure it works, no matter what
		basefile=${CXR_CONF_DIR}/$(basename $existingRun .conf).conf
		destination=${CXR_CONF_DIR}/$(basename $newRun .conf).conf
		
		if [[ -f "$destination"  ]]
			then
				# Continue even if file is there?
				if [[ $(common.user.getOK "$destination already exists. Do you want to overwrite this file?" N ) == false ]]
				then
					exit
				fi
			fi
		
		cp ${basefile} ${destination}
		touch ${destination}
		chmod +x ${destination}
	else
		if [[ $(common.user.getOK "We create a configuration file for the new run now.\n Do you want to copy an existing file? (If you say no, you will be asked the values of the new configuration instead)" ) == true  ]]
		then
	
			# Show a list of existing files to choose from
			if [[ $(common.user.getOK "Do you want to use a file other than \n $(basename ${CXR_BASECONFIG}) as as starting point?" ) == false  ]]
			then
				#No, use base.conf
				basefile=${CXR_BASECONFIG}
			else
				#Yes, gimme options
				basefile=$(common.runner.getExistingConfigFile)
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
	
			destination="${CXR_CONF_DIR}/${newRun}.conf"
	
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
			playfile=${CXR_CONF_DIR}/${newRun}.play
			
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
		
		main.log  "Edit the config file ${CXR_CONFIG} if needed - else just dry-run the script: \n \$ \t ${newRun} -d";
	fi # New name given?
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
	local basefile
	local expanded_config
	local model_version
	local run_dir
	
	basefile=$1
	expanded_config=$2
	
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

	model_version=${CXR_MODEL_VERSION}
	run_dir=${CXR_RUN_DIR}
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
	local needle
	
	# Find my element
	local current_id
	local current_model
	
	needle="$1"
	# Find my element
	current_id=0
	
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
# Function: common.runner.getNewRunName
#
# Interactively asks user for new run name
#
################################################################################	
function common.runner.getNewRunName() 
################################################################################
{
	model
	model_id
	supported
	version
	run
	addition
	
	model="$(common.user.getMenuChoice "Which model should be used?\nIf your desired model is not in this list, adjust CXR_SUPPORTED_MODELS \n(Currently $CXR_SUPPORTED_MODELS)" "$CXR_SUPPORTED_MODELS" )"
	
	model_id=$(common.runner.getModelId "$model") || main.dieGracefully "model $model is not known."
	
	# Extract the list of supported versions
	supported="${CXR_SUPPORTED_MODEL_VERSIONS[${model_id}]}"
	
	#Generate a menu automatically
	version="$(common.user.getMenuChoice "Which version of $model should be used?\nIf your desired version is not in this list, adjust CXR_SUPPORTED_MODEL_VERSIONS \n(Currently $supported)" "$supported" )"
	
	common.check.isVersionSupported? "$version" "$model" || main.dieGracefully "The version you supplied is not supported. Adjust CXR_SUPPORTED_MODEL_VERSIONS."
	
	run=${model}-v${version}
	
	# Ask user for rest of name
	addition="$(common.user.getInput "The Run name so far is ${run}- what do you want to add (spaces will be replaced by _)?")"
	addition="$(common.string.trim $addition)"
	
	# Replace any spaces left by _
	addition=${addition// /_}

	run="${run}-$addition"
	
	# Name ok? 
	common.check.RunName $run || main.dieGracefully "Could not determine new run name"
	
	echo "$run"
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
	run="$(common.runner.getNewRunName)"

	# Extract and export model name and version 
	main.setModelAndVersion $run
	
	# This is the file name of an extended config
	CXR_EXPANDED_CONFIG=${CXR_CONF_DIR}/${run}.econf

	# This is a "normal" copy
	CXR_CONFIG=${CXR_CONF_DIR}/${run}.conf
	
	#name is OK - create link ##################################################
	if [[ ! -L $run  ]]
	then
		main.log -a "Creating link $run."
			
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
# Function: common.runner.createMissingDirs
#
# Interactive function if called as such
#
# Reads the configuration of a run and creates all directories that are visible.
# This function is not intended to be called during a run, just for preparation.
# It is also no substitute for the checks, because of rules (see below)
# Has a couple of drawbacks:
# - it cannot forsee any rules that alter directories or add subdirectories
# - the configuration must be up-to-date we pause to give user the chance to update it.
#
# Parameters:
# $1 - Run-name for which to create directories
# $2 - iteractive (boolean), if set and true, run interactively (IS THIS GOOD??)
################################################################################	
function common.runner.createMissingDirs() 
################################################################################
{

	# TODO: Add input check
	local runName
	local dir
	
	runName=$1
	
	main.log -a "Creating missing directories..."

	# Load config (silently)
	main.readConfig "${runName}" "$CXR_MODEL" "$CXR_MODEL_VERSION" "$CXR_RUN_DIR" &> /dev/null
	
	# Get directories (create as needed)
	for dir in $(set | grep -e "^CXR_\([A-Z_]\)*DIR=" | cut -d= -f1)
	do
			main.log -a "Variable $dir has value: ${!dir}\n"

			# is it set?
			if [[ "${!dir}" ]]
			then
				# does it exist?
				if [[ -d "${!dir}" ]]
				then
					# is it executable (dir: means accessible)?
					if [[ ! -x ${!dir} ]]
					then
						main.log -e "Directory ${!dir}, \nParameter $dir not accessible!"
					fi
				else
					# Does not exist, create it.
					if [[ $(common.fs.isAbsolutePath? ${!dir}) == true  ]]
					then
						main.log -w "Directory ${!dir}, \nParameter $dir does not exist - creating it"
						mkdir -p ${!dir}
					else
						main.log -v  "${!dir} does not exist, but is a relative path - no action taken"
					fi
				fi
				
			else
				main.log -w   "Variable $dir is not set (might not be a problem, though)"
			fi
		done 
}

################################################################################
# Function: common.runner.getConfigItem
#
# This function loads a runs configuration (full hierarchy) and then
# extracts the value of a given variable. If the variable is not found, we crash.
# Its recommended to call this function in a subshell (see below), otherwise,
# the current configuration is overwritten.
#
# Example:
# > oldEmissDir="$(common.runner.getConfigItem CXR_EMISSION_DIR $oldRun)"
#
# Parameters:
# $1 - item name (a variable name) 
# $2 - Run-name
################################################################################	
function common.runner.getConfigItem() 
################################################################################
{
	local item
	local runName
	
	item="${1}" 
	runName="${2}"
	
	# Load config (silently)
	main.readConfig "${runName}" "$CXR_MODEL" "$CXR_MODEL_VERSION" "$CXR_RUN_DIR" &> /dev/null
	
	echo "$(common.variables.getValue $item)"
}

################################################################################
# Function: common.runner.recreateInput
#
# Interactive function
#
# Asks the user which parts of an existing runs input data must by copied, moved or
# linked to a new run.
# TODO: Rewrite using a loop
#
# Parameters:
# $1 - New Run-name 
# $2 - Existing Run-name
################################################################################	
function common.runner.recreateInput() 
################################################################################
{
	# How fine-grained should we be?
	# Currently, we distinguish between two classes of Inputs: Emissions and all other Input data.
	# Actually, we work only on directory level.
	# In theory, we could ask for each file, but that would take ages...
	# Just make sure that the two directories (Emissions/Other Input) are distinct.
	
	local newRun
	local oldRun
	local oldEmissDir
	local newEmissDir
	local oldInputDir
	local newInputDir
	
	
	newRun=$1
	oldRun=$2
	
	# get the relevant directories
	oldEmissDir="$(common.runner.getConfigItem CXR_EMISSION_DIR $oldRun)"
	newEmissDir="$(common.runner.getConfigItem CXR_EMISSION_DIR $newRun)"
	
	oldInputDir="$(common.runner.getConfigItem CXR_INPUT_DIR $oldRun)"
	newInputDir="$(common.runner.getConfigItem CXR_INPUT_DIR $newRun)"
	
	# make sure they are not subdirs of each other
	if [[ "$(common.fs.isSubDirOf? "$oldEmissDir" "$oldInputDir" )" == true || "$(common.fs.isSubDirOf? "$oldInputDir" "$oldEmissDir" )" == true ]]
	then
		main.dieGracefully "To use the recreate feature, neither $oldEmissDir must be a subdirectory of $oldInputDir or vice versa."
	fi

	# Emissions
	if [[ "$(common.user.getOK "Do you want to re-use emission data of $oldRun?")" == true ]]
	then
		# Re-use Emissions
		
		# Do not take chances - only work on empty target directory
		# we use the fact that rmdir crashes if the directory is non-empty
		rmdir $newEmissDir || main.dieGracefully "Could not replace $newEmissDir by a link or copy to $oldEmissDir! $newEmissDir must be empty"
		
		if [[ "$(common.user.getOK "Do you want to copy the data? (if not, we symlink to it)")" == true ]]
		then
			# copy
			cp -r $oldEmissDir $newEmissDir || main.dieGracefully "Could not replace $newEmissDir by a copy of $oldEmissDir!"
		else
			# link
			ln -s $oldEmissDir $newEmissDir || main.dieGracefully "Could not replace $newEmissDir by a link to $oldEmissDir!"
		fi
	fi
	
	rmdir $newInputDir || main.dieGracefully "Could not replace $newInputDir by a link or copy to $oldInputDir! $newInputDir must be empty"
	
	# Other Inputs
	if [[ "$(common.user.getOK "Do you want to re-use other input data of $oldRun?")" == true ]]
	then
		# Re-use Other stuff
		if [[ "$(common.user.getOK "Do you want to copy the data? (if not, we symlink to it)")" == true ]]
		then
			# copy
			cp -r $oldInputDir $newInputDir || main.dieGracefully "Could not replace $newInputDir by a copy of $oldInputDir!"
		else
			# link
			ln -s $oldInputDir $newInputDir || main.dieGracefully "Could not replace $newInputDir by a link to $oldInputDir!"
		fi
	fi
}

################################################################################
# Function: common.runner.recreateRun
#
# Creates a copy of an existing config file and prepares everything to rerun.
# Depending on the users choice, Input data can be linked or copied.
# Automatically creates the needed directories. 
#
# Parameters:
# [$1] - Run-name to re-create (optional)
################################################################################	
function common.runner.recreateRun() 
################################################################################
{
	local oldRun
	local newRun
	
	# Do we have the old run name?
	if [[ -z "${1:-}" ]]
	then
		#no, ask
		oldRunConf=$(common.runner.getExistingConfigFile)
		
		# remove .conf
		oldRun="$(basename $oldRunConf .conf)"
	else
		#yes
		oldRun=${1}
	fi # got run name?
	
	# Verify
	if [[ $(common.check.RunName $oldRun) == false ]]
	then
		main.dieGracefully "The name of the run you want to repeat ($oldRun) is not correct. Make sure only characters allowed in filenames are included"
	fi # Supplied run name correct
	
	
	# Get the new one
	newRun="$(common.runner.getNewRunName)"
	
	#name is OK - create link ##################################################
	if [[ ! -L $newRun  ]]
	then
		main.log -a "Creating link $newRun."
			
		ln -s $CXR_RUNNER_NAME $newRun
	else
		# run already existists, OK? ###############################################
		if [[ $(common.user.getOK "$newRun already exists. Do you want to continue?" N ) == false  ]]
		then
			# No
			exit
		fi
	fi
	
	# Create config
	common.runner.createConfigFile "$newRun" "$oldRun"
	
	# Create Directories
	
	common.user.pause "We will now load the configuration file ${newRun}.conf (more than once) to create directories and Input.\nPlease adjust this file now if needed!\nPress any key to continue..."
	common.runner.createMissingDirs "$newRun"
	
	# Ask user if we need to copy/link input data 
	common.runner.recreateInput "$newRun" "$oldRun"
	
	# Thats all.
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
	########################################
	# Setup tests if needed
	########################################
	
	CXR_IGRID=1
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.runner.evaluateRule a) a "common.runner.evaluateRule constant"
	is $(common.runner.evaluateRule "$(common.math.abs -100)") 100 "common.runner.evaluateRule a function of CAMxRunner"
	is $(common.runner.evaluateRule "domain$(common.string.leftPadZero $CXR_IGRID 3)") domain001 "common.runner.evaluateRule with formatting"
	is $(common.runner.evaluateRule "$(uname -n)") $(uname -n) "common.runner.evaluateRule with uname"
	
	# Test Locking. We simulate the case that many processes want the same lock at the same instant.
	# This will not occur (hopefully) and is a pretty difficult situation.
	local lock
	lock=test
	
	# save & lower timeout
	oCXR_LOCK_TIMEOUT_SEC=$CXR_LOCK_TIMEOUT_SEC
	CXR_LOCK_TIMEOUT_SEC=5
	
	# How many processes?
	nProcs=5
	
	# This file saves as a barrier
	barrier=$(common.runner.createTempFile lock-barrier)
	
	main.log -a "Testing Locking - using a timeout of $CXR_LOCK_TIMEOUT_SEC s."
	
	for iter in $(seq 1 $nProcs)
	do
	
		# These are the proceses that carry out the test
		(
			# Wait until barrier is gone
			while [[ -f $barrier ]]
			do
				:
			done
			
			# Get an instance lock
			common.runner.getLock "$lock" "$CXR_LEVEL_INSTANCE" > /dev/null
			echo "Process $iter got the lock"
			common.runner.releaseLock "$lock" "$CXR_LEVEL_INSTANCE"
			echo "Process $iter released the lock"
		
		) &
	
	done
	
	# remove the barrier
	rm $barrier
	sleep $CXR_LOCK_TIMEOUT_SEC
	
	
	# Restore old settings
	CXR_LOCK_TIMEOUT_SEC=$oCXR_LOCK_TIMEOUT_SEC

	
	
	########################################
	# teardown tests if needed
	########################################
	
	
}
