#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Check Functions
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
CXR_META_MODULE_NUM_TESTS=2

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|md5sum|optional"

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|dos2unix"

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains most of the check functions for the CAMxRunner"

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
# Function: cxr_common_check_bash_version
#	
# Checks if the bash version is ok for what we do
################################################################################
function cxr_common_check_bash_version() 
################################################################################
{
	if [[ ${BASH_VERSINFO[0]} -lt 3  ]]
	then
		main.die_gracefully "We need at least Bash Version 3.x - please upgrade."
	fi
}

################################################################################
# Function: cxr_common_predict_file_size_megabytes
# 
# Gives a rough estimate  on the number of megabytes needed for a given output file.
# This function is NOT used by <cxr_common_predict_model_output_megabytes>, the purpose of
# this function here is to check if a file has about the right size.
#
# Parameters:
# $1 - the filename in question
# $2 - the type of file (AIRQUALITY, BOUNDARY, EMISSIONS...)
# $3 - the storage type (ASCII, BINARY, FDA, HDF)
#
################################################################################
function cxr_common_predict_file_size_megabytes ()
################################################################################
{
	# TODO Should be implemented
	echo 400
}

################################################################################
# Function: cxr_common_predict_model_output_megabytes
# 
# Gives a rough (hopefully over-)estimate  on the number of megabytes needed for the output.
#
# Formula: sum (dx_i * dy_i * dz_i) * ( t / t_res) * nspec * C(Options) * f_margin
# Takes into account if 3D output is requested or not.
# The factor C(Options) is currently a constant, it might take other options
# like HDF, source apportionment etc. into account later.
#
################################################################################
function cxr_common_predict_model_output_megabytes ()
################################################################################
{
	local cells
	local time_steps
	local size

	# Determine size of output field
	if [[ ${CXR_AVERAGE_OUTPUT_3D} == true  ]]; then
		cells=$(cxr_common_get_num_cells_3d)
	else
		cells=$(cxr_common_get_num_cells_2d)
	fi
	
	# Our constant is designed for 10^5 cells
	cells=$(common.math.FloatOperation "$cells / 100000")
	
	time_steps=$(common.math.FloatOperation "(60 * 24 * ${CXR_NUMBER_OF_SIM_DAYS}) / ${CXR_OUTPUT_FREQUENCY}" "0" )
	
	size=$(common.math.FloatOperation "${cells} * ${time_steps} * ${CXR_NUMBER_OF_OUTPUT_SPECIES} * ${CXR_C_SPACE} * ${CXR_F_MARGIN}" "0" false)
	
	echo "$size"
}

################################################################################
# Function: cxr_common_check_mb_needed
#	
# Checks if space in target directory is sufficient.
# Aborts if not sufficient.
# Internally uses <cxr_common_free_megabytes>
#
# Parameters:
# $1 - Directory to check
# $2 - Space needed (in megabytes)
################################################################################
function cxr_common_check_mb_needed() 
################################################################################
{
	if [[ $# -ne 2  ]]
	then
		main.die_gracefully "needs 2 parameters: a path and a number (megabytes needed)"
	fi
	
	local dir="$1"
	local mb="$2"
	
	main.log -v  "Checking if space in $dir is sufficient..."
		
	# Nonexistent Directory?
	if [[ ! -d $dir  ]]
	then
		# Create it!
		main.log -w  "Directory $dir is missing, I create it now."
		mkdir -p $dir
	fi
	
	if [[ "$(cxr_common_free_megabytes "$dir")" -ge "$mb"  ]]
	then
		main.log -i  "Space in $dir is sufficient."
	else
		main.die_gracefully "Space in $dir is not sufficient, need at least $mb Megabytes!"
	fi
	
}


################################################################################
# Function: cxr_common_check_datataype
#	
# Checks if a value has a certain datatype - needed for installation
# to make sure the user entered sensible values
#
# Datatypes:
#	S (String - allowed to be empty)
# I (Integer)
#	F (Float)
#	B (Boolean - either true or false)
# D (Directory - a high level string that is checked)
#
# Parameters:	
# $1 - Value to check
# $2 - Expected datatype (1 char)
################################################################################
function cxr_common_check_datataype()
################################################################################
{
	if [[ $# -ne 2  ]]
	then
		main.die_gracefully "needs 2 strings as input"
	fi
	
	local value=$1
	local datatype=$2
	
	main.log -d   "VALUE: *$value*\nDATATYPE: *$datatype*"

	case $datatype in
	S) # String - everything is ok, even an empty string
		echo true
		;;
	I) # Integer
		echo $(main.isNumeric? "$value")
		;;
	F) # Floating point number

		#Check for FP
		# If this returns 0, we look at a FP.
		# We look for any ocurrences of numbers,
		# mandatorily followed by a dot
		# optionally followed by any ocurrences of numbers
		
		# We neet the retval,
		# turn off strict checks
		set +e
		
		echo "$value" | grep "[0-9]*\.[0-9]*" >/dev/null
		
		if [[ $? -eq 0  ]]
		then
			echo true
		else
			echo false
		fi
		
		#Turn strict checks back on
		set -e
		
		;;
	B) # Boolean - either true or false
		if [[  "$value" == true || "$value" == false ]]
		then
			echo true
		else
			echo false
		fi
		;;
	D) # Directory - in principle we accept anything
		if [[ ! -d "$value" ]]
		then
			main.log  -s "The directory $value was not found"
			echo true
		fi
			
		;;
	*) main.die_gracefully "illegal Datatype $datatype" ;;
	esac
}

################################################################################
# Function: cxr_common_check_model_limits
#
# Checks if the current model supports our current settings by inspecting the 
# relevant .play file.
# If these checks change with the model version, put the definition of this function
# in a module under modules/common/model/version directory
#
################################################################################
function cxr_common_check_model_limits() 
################################################################################
{
	main.log -a -B  "Checking model limits for ${CXR_MODEL_EXEC}..."
	
	# We must find the play file
	local playfile=${CXR_INSTALLER_VERSION_INPUT_DIR}/$(basename ${CXR_MODEL_EXEC}).play
	local i
	local var
	local curr_var
	local f_val
	local f_nspec
	local cxr_value
	
	if [[ -f "${playfile}"  ]]
	then
		# playfile is present
		
		main.log -a  "This was the configuration used to compile ${CXR_MODEL_EXEC}:"
		cat "${playfile}" | tee -a "${CXR_LOG}"
		
		# Check geometry
		
		#Test each grid
		for i in $(seq 1 $CXR_NUMBER_OF_GRIDS);
		do
			# For column, row, layer (CAMx internal variables are called like lis, eg MXROW1
			for var in COL ROW LAY
			do
				case "${var}" in
					COL) cxr_value=$(cxr_common_get_x_dim ${i}) ;;
					ROW) cxr_value=$(cxr_common_get_y_dim ${i}) ;;
					LAY) cxr_value=$(cxr_common_get_z_dim ${i}) ;;
				esac
				
				# e. g. MXCOL1
				curr_var="MX${var}${i}"
				
				main.log -v  "Checking ${curr_var}..."
				
				# Read value
				f_val="$(grep "${curr_var}${CXR_DELIMITER}" "${playfile}" | cut -d${CXR_DELIMITER} -f2)"
				
				if [[ "${f_val}"  ]]
				then
					# Check if we are above the limit
					if [[ "${cxr_value}" -gt "${f_val}"  ]]
					then
						main.log -e  "The limit for the setting ${curr_var} (${f_val}) in too low in the executable ${CXR_MODEL_EXEC} (${cxr_value})\nPlease recompile CAMx using the installer."
					else
						main.log -v  "${curr_var} setting OK"
					fi
				else
					main.log -v  "There is no entry ${curr_var} in the playfile ${playfile}"
				fi
			done
		done

		# Check #of species
		f_nspec="$(grep "MXSPEC${CXR_DELIMITER}" "${playfile}" | cut -d${CXR_DELIMITER} -f2)"
		
		if [[ "${f_nspec}"  ]]
		then
			# Check if we are above the limit
			if [[ "${CXR_NUMBER_OF_OUTPUT_SPECIES}" -gt "${f_nspec}"  ]]
			then
				main.log -e  "The limit for the number of species (MXSPEC=${f_nspec}) in too low in the executable ${CXR_MODEL_EXEC} (${CXR_NUMBER_OF_OUTPUT_SPECIES})\nPlease recompile CAMx using the installer."
			else
				main.log -v  "Number of species is OK."
			fi
		else
			main.log -v  "There is no entry MXSPEC in the playfile ${playfile}."
		fi
	else
		main.log -a  "Found no playfile called ${playfile}.\nSo probably CAMx was not compiled using CAMxRunner, cannot check the capabilities of your executable."
	fi
	
	main.log -a  "Model limits checked."
}


################################################################################
# Function: cxr_common_check_runner_executables
#	
# Loop through all *.sh scripts in the ${CXR_RUN_DIR} and check if they are executable
#
################################################################################
function cxr_common_check_runner_executables ()
################################################################################
{
	local file
	
	# We use a bash 3.x structure, the so-called "here-variable"
	while read file
	do
		# Show that we are alive
		cxr_common_show_progress
	
		if [[ ! -x $file ]]
		then
			main.log -w  "File $file is not executable,I try to correct this"
			# File is not executable, try to correct
			chmod +x $file || main.die_gracefully "Could not change permissions on file $file - exiting"
			
			if [[ "$(cxr_common_is_dos? "$file")" == true ]]
			then
				main.log -w  "$file is in dos format. I will correct this."
				${CXR_DOS2UNIX_EXEC} $file
				
				if [[ $? -ne 0 ]]
				then
					main.die_gracefully "could not convert $file to Unix format!"
				fi
			fi
			
		fi
	# Make sure we exclude state dir
	done<<<"$(find ${CXR_RUN_DIR} -noleaf -type f -name \*.sh | grep -v "^${CXR_RUN_DIR}/state/")"
	
	main.log -a  "Checked."
}

################################################################################
# Function: cxr_common_check_environment_executables
#	
# Loop through all CXR_*.*_EXEC enviranment variables and check if they are 
# * Set
# * Present
# * Executable
# * also report their MD5 Hashes
#
# This function is only visual, does not terminate
################################################################################
function cxr_common_check_environment_executables ()
################################################################################
{
	local executable

	for executable in $(set | grep -e ^CXR_*.*_EXEC= | cut -d= -f1)
	do
	
		main.log -v   "Variable $executable has value: ${!executable}\n"
			
		# is it set?
		if [[ "${!executable}" ]]
		then
			# Does it exist?
			if [[ -f "${!executable}" ]]
			then
				# is it executable 
				if [[ ! -x ${!executable} ]]
				then
					main.log -w   "Executable ${!executable}, Parameter $executable not executable - I try to correct this ..."     
					
					chmod +x ${!executable} || main.log  "Could not change permissions on file $FILE"

					# Do not increase error count here - maybe we do not need this one
				else
					# All OK, just report MD5
					cxr_common_report_md5 "${!executable}"
				fi
			else
			  # Not present!
			  main.log -w   "Executable ${!executable}, Parameter $executable does not exist (might not be a problem, though, CAMx e. g. is not needed for postprocessing and vice-versa)!"
			fi
			
		else
			main.log -w   "Variable $executable is not set (might not be a problem, though)"
		fi
	done
}

################################################################################
# Function: cxr_common_md5
#	
# Returns an MD5 Hash of a file. Returns the empty string if file does not exist.
#
# Parameters:
# $1 - file to Hash
################################################################################
function cxr_common_md5() 
################################################################################
{
		if [[ $# -ne 1  ]]
		then
			echo -e  "Programming error: no filename passed!"
		fi
		
		local file="$1"
		
		if [[ -r "${file}" ]]
		then
			"${CXR_MD5_EXEC}" "${file}" | cut -d" " -f1
		else
			# Return the empty string
			echo -w  "File $file not readable."
			echo ""
		fi
}


################################################################################
# Function: cxr_common_report_md5
#	
# Logs the MD5 Hash of a file.
# Also stores this information in a global hash called MD5. If there is a current 
# value in there (generated during this run), we do not report a new value, 
# otherwise we do and compare the new with the old value.
#
# Parameters:
# $1 - file to Hash
################################################################################
function cxr_common_report_md5() 
################################################################################
{
	if [[ "${CXR_REPORT_MD5}" == true ]]
	then
	
		if [[ $# -ne 1 ]]
		then
			echo -e  "Programming error: no filename passed!"
		fi
		
		local file="$1"
		local hash
		local new_hash
		local old_hash
		local old_mtime
		
		# Is this file already in the cache?
		if [[ "$(cxr_common_hash_has? MD5 $CXR_HASH_TYPE_UNIVERSAL "${file}" )" == true ]]
		then
		
			# Did we encounter it recently?
			if [[ "$(cxr_common_hash_new? MD5 $CXR_HASH_TYPE_UNIVERSAL "${file}")" == false ]]
			then
				# it must be older, check if hash has changed.
				new_hash="$(cxr_common_md5 "$file")"
				old_hash="$(cxr_common_hash_get MD5 $CXR_HASH_TYPE_UNIVERSAL "${file}")"
				
				if [[ "$new_hash" != "$old_hash" ]]
				then
					# Get the old mtime
					old_mtime="$(cxr_common_hash_mtime MD5 $CXR_HASH_TYPE_UNIVERSAL "${file}" )"
					old_datetime="$(cxr_common_epoch2datetime $old_mtime)"
					cxr_logger -w "File ${file} has changed since ${old_datetime}. Old MD5 hash: ${old_hash}, new MD5 hash: ${new_hash}"
				fi
			fi
		
		else
			# Never seen this file before!
			hash="$(cxr_common_md5 "$file")"
			main.log -a  "MD5 Hash of ${file} is ${hash}"
			
			# Store in Cache
			cxr_common_hash_put MD5 $CXR_HASH_TYPE_UNIVERSAL "$file" "$hash"
		fi
		
	
			
		
			
		

	fi
}

################################################################################
# Function: cxr_common_check_preconditions
#	
# Checks if all input files listed in CXR_CHECK_THESE_INPUT_FILES are available.
# If -w is given, we wait until the files arrive.
# Also sees that output files listed in CXR_CHECK_THESE_OUTPUT_FILES are not present 
# - if -F is given, existing output files are deleted here. 
# If we detect empty output files, they are always removed.
#
# Files are checked for length if CXR_CHECK_MAX_PATH=true
#
# This function does not terminate the runner if errors are found,
# but it returns false.
#
# You can restrict the checks if wanted, currently, the one-time checks are always executed.
#
# Parameters:
# -i - check input files (Default: Check all)
# -o - check output files (Default: Check all)
################################################################################
function cxr_common_check_preconditions() 
################################################################################
{
	# Does the user want to limit the checks?
	local limited=false
	
	# First, all is switched off
	local do_input=false
	local do_output=false
	local errors_found
	local output_dir
	local dir
	local output_file
	local input_file

	while getopts ":io" opt
	do
		case $opt in
		i) limited=true ; do_input=true  ;;
		o) limited=true ; do_output=true ;;
		esac
	done
	
	# Fix switches if user did not restrict
	if [[ "${limited}" == false  ]]
	then
		do_input=true
		do_output=true
	fi
	
	# This is not strictly needed, but it allows to read 
	# non-named command line options
	shift $(($OPTIND - 1))
	
	# Make getopts ready again
	unset OPTSTRING
	unset OPTIND

	# In here, we disable strict checking of variables
	# because we want to fail in a controlled manner if anything is wrong
	set +u

	#Mark if there were errors
	errors_found=false
	
	##########################################
	#### this is only checked once
	##########################################
	if [[ "$CXR_CHECKED_ONCE" == false  ]]
	then
	
		main.log -v   "Performing one-time checks..."
		
		# Increase global indent level
		main.increaseLogIndent

		main.log -v   "Checking Output directories ..."
		
		# Increase global indent level
		main.increaseLogIndent

		# Create Output dirs if needed
		# cut extracts the variable name
		for output_dir in $(set | grep -e ^CXR_*.*_OUTPUT_DIR= | cut -d= -f1)
		do
			# is it set?
			if [[ "${!output_dir}"  ]]
			then
				main.log -v   "Variable $output_dir has value: ${!output_dir}\n"
				
				# Test length
				if [[ "${CXR_CHECK_MAX_PATH}" == true  ]]
				then
					if [[ $(string_len "${!output_dir}") -gt "${CXR_MAX_PATH}"  ]]
					then
						main.log -e  "Path to $output_dir longer than ${CXR_MAX_PATH}. Either disable this check (CXR_CHECK_MAX_PATH=false) or increase CXR_MAX_PATH.\nCheck if all binaries are ready for paths of this size!"
					fi
				fi
				
				# Does it exist
				if [[ ! -d ${!output_dir}  ]]
				then
					main.log -w  "Directory ${!output_dir} Parameter $output_dir does not exist - will create it"
					
					mkdir -p ${!output_dir} || main.die_gracefully "could not create output dir ${!output_dir}"
					
				# is it writeable?
				elif [[ ! -w ${!output_dir}  ]]
				then
					main.log -e "Output Directory ${!output_dir}, \nParameter $output_dir not writeable!"
				fi
			else
				main.log -w   "Variable $output_dir is not set (might not be a problem, though)"
			fi
		done
		
		# Decrease global indent level
		main.decreaseLogIndent
		
		main.log -v   "Checking Executables ..."
		
		# Increase global indent level
		main.increaseLogIndent

		# EXECUTABLES
		cxr_common_check_environment_executables
		
		# Decrease global indent level
		main.decreaseLogIndent
		
		main.log -v   "Checking Directories ..."
		
		# Increase global indent level
		main.increaseLogIndent

		
		# DIRECTORIES
		for dir in $(set | grep -e ^CXR_*.*_DIR= | cut -d= -f1)
		do
			main.log -v   "Variable $dir has value: ${!dir}\n"

			# is it set?
			if [[ "${!dir}"  ]]
			then
			
				# does it exist?
				if [[ -d "${!dir}"  ]]
				then
					# is it executable (dir: means accessible)?
					if [[ ! -x ${!dir}  ]]
					then
						main.log -e "Directory ${!dir}, \nParameter $dir not accessible!"
					fi
				else
					# Does not exist, create it.
					if [[ $(cxr_common_is_absolute_path ${!dir}) == true  ]]
					then
						main.log -w   "Directory ${!dir}, \nParameter $dir does not exist - creating it"
						mkdir -p ${!dir}
					else
						main.log -v  "${!dir} does not exist, but is a relative path - no action taken"
					fi
				fi
				
			else
				main.log -w   "Variable $dir is not set (might not be a problem, though)"
			fi
		done 
		
		# Decrease global indent level
		main.decreaseLogIndent
		
		# Check done
		CXR_CHECKED_ONCE=true
		
		##########################################
		#### END this is only checked once
		##########################################
		
	fi
	
	##########################################
	### From here, checks are run for each day
	##########################################
	
	main.log -v   "Performing per-day checks..."
	
	
	########################################
	# Input Check
	########################################
	if [[ "${do_input}" == true  ]]
	then
		# INPUT FILES - are they there?
		main.log -v   "Checking Input Files ..."
	
		# Increase global indent level
		main.increaseLogIndent
	
		for input_file in ${CXR_CHECK_THESE_INPUT_FILES}
		do
			# Test length
			if [[ "${CXR_CHECK_MAX_PATH}" == true  ]]
			then
				if [[ $(string_len "${input_file}") -gt "${CXR_MAX_PATH}"  ]]
				then
					main.log -e  "Path to $input_file longer than ${CXR_MAX_PATH}. Either disable this check (CXR_CHECK_MAX_PATH=false) or increase CXR_MAX_PATH.\nCheck if all binaries are ready for paths of this size!"
					errors_found=true
				fi
			fi
			
			# does  it exist?
			if [[ ! -f "${input_file}"  ]]
			then
				# does not exist!
				# do we wait?
				if [[ "${CXR_WAIT_4_INPUT}" == true ]]
				then
					# We want to wait
					if [[ "$(cxr_common_wait_for_file ${input_file})" == false ]]
					then
						# Waiting did not help
						errors_found=true
					fi
				else
					main.log -e  "File ${input_file} does not exist!"
					errors_found=true
				fi
			else
				# File exists.
				
				# If a process writes to the file, it might be that 
				# it still grows
				if [[ "${CXR_WAIT_4_INPUT}" == true ]]
				then
					if [[ "$(cxr_common_wait_for_stable_size ${input_file})" == false ]]
					then
						# Waiting did not help
						errors_found=true
					fi
				fi
				
				# Readable?
				if [[ -r "${input_file}"  ]]
				then
					# is it larger than 0 bytes?
					if [[ ! -s "${input_file}"  ]]
					then
						# Empty File!
						main.log -e  "File ${input_file} is empty!"
						errors_found=true
					else
						# Non-empty, report hash if wanted
						cxr_common_report_md5 "${input_file}"
					fi # larger than 0
				else
					# Not readable!
					main.log -e  "File ${input_file} not readable!"
					errors_found=true
				fi # readable
			fi
		done
	else
		main.log -v  "Will not check input"
	fi
	
	########################################
	# Output Check
	########################################
	if [[ "${do_output}" == true  ]]
	then
		# OUTPUT FILES - are they absent?
		main.log -v   "Checking if Output Files are absent..."
	
		# Increase global indent level
		main.increaseLogIndent
	
		for output_file in ${CXR_CHECK_THESE_OUTPUT_FILES}
		do
			# Test length
			if [[ "${CXR_CHECK_MAX_PATH}" == true  ]]
			then
				if [[ $(string_len "${output_file}") -gt "${CXR_MAX_PATH}"  ]]
				then
					main.log -e  "Path to $output_file longer than ${CXR_MAX_PATH}. Either disable this check (CXR_CHECK_MAX_PATH=false) or increase CXR_MAX_PATH.\nCheck if all binaries are ready for paths of this size!"
					errors_found=true
				fi
			fi
			
			# is it present?
			if [[ -f "${output_file}"  ]]
			then
				#############################
				# File Present!
				#############################
				
				# is it empty or do we force?
				if [[ ! -s "${output_file}"  ]]
				then
					# Empty
					main.log -w  "File ${output_file} already exists, but is empty. I will delete it now..."
					rm -f "${output_file}"
				elif [[ "$CXR_FORCE" == true  ]]
				then
					# Force overwrite
					main.log -w  "File ${output_file} already exists. You chose the -F option, so we delete it now..."
					
					if [[ "$CXR_DRY" == false  ]]
					then
						rm -f "${output_file}"
					else
						main.log -w  "Dryrun, file ${output_file} not removed. A real run removes the file if -F is given!"
					fi
				else
					#############################
					# No overwrite, no empty file
					#############################
					
					# Do we skip?
					if [[ "${CXR_SKIP_EXISTING}" == true  ]]
					then
						# Ok, skipping
						main.log -w  "File ${output_file} already exists. You chose to skip over this file..."
					else
						main.log -e  "File ${output_file} already exists! You can force the deletion of existing files by calling \n ${CXR_CALL} -F"
						errors_found=true
					fi
				fi
			else
				# Not there, OK
				main.log -v   "File $output_file does not yet exist - Good."
			fi
		done
		
		# Decrease global indent level
		main.decreaseLogIndent
	else
		main.log -v  "Will not check output"
	fi
	
	# re-enable checking of variables
	set -u
	
	# Inverting
	if [[ "${errors_found:-false}" == false ]]
	then
		# No errors
		echo true
	else
		# Detected errors
		echo false
	fi
	
	return $CXR_RET_OK
}

################################################################################
# Function: cxr_common_check_module_requirements
#	
# Checks if the currently loaded module requests a CAMxRunner and configuration
# Version less or equal than current one and if it needs anything special 
# (like special executables).
# The executable check is still a bit rough - we later need to integrate it with
# the new approach to determine any executables name.
#
# Returns true on success, false otherwise
################################################################################
function cxr_common_check_module_requirements() 
################################################################################
{
	# Test if Module was already announced (for efficiency and log-file size reasons)
	local found=$(common.string.isSubstringPresent? "$CXR_ANNOUNCED_MODULES" "$CXR_META_MODULE_NAME")
	local requirement
	local elements
	local n_elements
	local kind
	local what
	local need
	local executable
	local found=false
	
	# We announce only if it was not found
	if [[ "$found" == false  ]]
	then
	
		main.log -v   "\nLoading ${CXR_META_MODULE_NAME} - Version ${CXR_META_MODULE_VERSION}\n"    
	
		# Increase global indent level
		main.increaseLogIndent
		
		main.log -v   "==========================================================\nName of the Module: *${CXR_META_MODULE_NAME}*\n==========================================================\nType: *${CXR_META_MODULE_TYPE}*\nRequires at least CAMxRunner Revision: *${CXR_META_MODULE_REQ_RUNNER_VERSION}*\n"
		main.log -v   "Description:\n$CXR_META_MODULE_DESCRIPTION\n"     
		main.log -v   "More Information can be found here:\n$CXR_META_MODULE_DOC_URL\n"     
		main.log -v   "The Module was written by: $CXR_META_MODULE_AUTHOR\nAnd is released under these terms:\n$CXR_META_MODULE_LICENSE\n"

		if [[ "${CXR_META_MODULE_NUM_TESTS:-0}" -gt 0  ]]
		then
			main.log -v   "This module contains a test suite."
		fi

		# Decrease global indent level
		main.decreaseLogIndent
	fi
	
	################################################################################
	# Perform version check
	################################################################################	
	
	# Increase global indent level
	main.increaseLogIndent
	
	# check if the stuff is set properly
	# Check CAMxRunner revision
	if [[ "$CXR_META_MODULE_REQ_RUNNER_VERSION" && (  "$CXR_META_MODULE_REQ_RUNNER_VERSION" != "-" )   ]]
	then
		
		# Increase global indent level
		main.increaseLogIndent
		
		# CAMxRunner Version
		RUNNER_REVISION=$(cxr_common_get_svn_revision $CXR_RUN_DIR/CAMxRunner.sh)
		
		if [[ "$RUNNER_REVISION" -ge "$CXR_META_MODULE_REQ_RUNNER_VERSION"  ]] 
		then
			main.log -v   "CAMxRunner Version OK"
		else
			main.log -w  "Module $CXR_META_MODULE_NAME needs at least revision $CXR_META_MODULE_REQ_RUNNER_VERSION of CAMxRunner.\nCurrently running is revision $RUNNER_REVISION"
			echo false
			return 0
		fi
		
		# Decrease global indent level
		main.decreaseLogIndent
		
	else
			main.log -w  "Consider to set the required CAMxRunner Version in Module ${CXR_META_MODULE_NAME} using variable CXR_META_MODULE_REQ_RUNNER_VERSION"
	fi
	
	# check if the stuff is set properly
	# Check config revision
	if [[  "$CXR_META_MODULE_REQ_CONF_VERSION" && (  "$CXR_META_MODULE_REQ_CONF_VERSION" != "-" ) ]]
	then
		
		# Increase global indent level
		main.increaseLogIndent
		
		# Config Revision
		CONFIG_REVISION=$(cxr_common_get_svn_revision $CXR_CONFIG)

		if [[ "$CONFIG_REVISION" -ge "$CXR_META_MODULE_REQ_CONF_VERSION"  ]] 
		then
			main.log -v   "Config Version OK"
		else
			main.log -w  "Module $CXR_META_MODULE_NAME needs at least revision $CXR_META_MODULE_REQ_CONF_VERSION of the config file $CXR_CONFIG.\nCurrently loaded is revision $CONFIG_REVISION"
			echo false
			return 0
		fi
		
		# Decrease global indent level
		main.decreaseLogIndent
		
	else
			main.log -w  "Consider to set the required Config Version in Module ${CXR_META_MODULE_NAME} using variable CXR_META_MODULE_REQ_CONF_VERSION"
	fi
	
	################################################################################
	# Perform exectuable check
	################################################################################
	
	if [[ "${CXR_META_MODULE_REQ_SPECIAL:-}" && ( "${CXR_META_MODULE_REQ_SPECIAL:-}" != "-" ) ]]
	then
		# Parsing something like "exec|dot|optional exec|wget"
		
		# Attention - we need tha standard IFS here!
		for requirement in $CXR_META_MODULE_REQ_SPECIAL
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
				kind=$(string_trim ${elements[0]})
				what=$(string_trim ${elements[1]})
				# The default is that we need it really
				need=mandatory
			elif [[ ${n_elements} -eq 3 ]]
			then
				# only stl. "exec|dot|optional"
				kind=$(string_trim ${elements[0]})
				what=$(string_trim ${elements[1]})
				need=$(string_trim ${elements[2]})
			else
				# this is wrong!
				main.log -e  "Requirement string $requirement contains an error. We need two or three pipe-separated fields like exec|idl or exec|idl|optional depending on the actual needs"
			fi
			
			case "$kind" in
			
				"exec") # Now we search the environment for this executable
							for executable in $(set | grep -e ^CXR_*.*_EXEC= | cut -d= -f1)
							do
								if [[ "$(basename ${!executable})" == "$what" ]]
								then
									main.log -v   "${!executable} matches $what"
									found=true
									break
								fi
							done
							
							if [[ "$found" == false  ]]
							then
								if [[ "$need" == mandatory ]]
								then
									main.log -e  "Module $CXR_META_MODULE_NAME mandatorily needs the executable $what which was not found."
									echo false
									return 0
								else
									main.log -w  "Module $CXR_META_MODULE_NAME needs the executable $what which was not found."
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
	echo true
	return 0

}

################################################################################
# Function: cxr_common_check_result
#	
# Checks if the output was generated
# Later, we can think about a real size check
#
# We basically just loop through a list of files (this is a radical break with the 
# old approach where we looped over an array of arrays of files).
# This list is called CXR_CHECK_THESE_OUTPUT_FILES and must be set befor calling this function
#
# Additionally, each created file is added to the file CXR_INSTANCE_FILE_OUTPUT_LIST
#
# This function does not terminate the runner if errors are found,
# but it returns false
################################################################################
function cxr_common_check_result() 
################################################################################
{
	local errors_found=false
	local output_file
	
	# No output check for dryruns
	if [[ $CXR_DRY == true  ]]
	then
		
		main.log -w  "Output check is not carried out for dryruns - no output was generated.\nStill we generate dummy files now..."
		
		# generate dummy files if needed
		for output_file in ${CXR_CHECK_THESE_OUTPUT_FILES}
		do
			# does it not exist?
			if [[ ! -f "${output_file}"  ]]
			then
				cxr_common_create_dummyfile ${output_file}
			else
				main.log -w  "File ${output_file} seems to exist, we do not touch it."
			fi
		done
		
		echo true
		return 0
	fi

	main.log -v   "Checking Result Files..."

	# Increase global indent level
	main.increaseLogIndent

	# We do word splitting
	for output_file in ${CXR_CHECK_THESE_OUTPUT_FILES}
	do
		# does it exist and is it larger than 0 bytes?
		if [[ -s "${output_file}"  ]]
		then
			main.log -v  "Output File ${output_file} has non-zero size, OK."
			
			# Add the file to CXR_INSTANCE_FILE_OUTPUT_LIST if the file is not yet listed
			grep "${output_file}${CXR_DELIMITER}${CXR_META_MODULE_NAME}" "${CXR_INSTANCE_FILE_OUTPUT_LIST}"
			
			if [[ "$?" -ne 0  ]]
			then
				# File not yet listed, add 
				# structure is 
				# filename|module_that_created it
				echo "${output_file}${CXR_DELIMITER}${CXR_META_MODULE_NAME}" >> "${CXR_INSTANCE_FILE_OUTPUT_LIST}"
			fi
			
		else
			main.log -e "File $(basename ${output_file}) was not created properly"
			errors_found=true
		fi
	done
	# Decrease global indent level
	main.decreaseLogIndent
	
	if [[ "$errors_found" == false  ]]
	then
		# No errors
		echo true
	else
		# Detected errors
		echo false
	fi
}

################################################################################
# Function: cxr_common_is_version_supported
#	
# Checks if a version number is supported by a given model.
#
# Returns:
# 0 if ok, 1 otherwise
#
# Parameters:	
# $1 - The Version to check
# $2 - The Model name as indicated in CXR_SUPPORTED_MODELS
################################################################################
function cxr_common_is_version_supported()
################################################################################
{
	if [[ -z "${1:-}"  ]]
	then
		# Forget it - must be larger than ""
		return 1
	fi
	
	local version=$1
	local model=$2
	local model_id="$(cxr_common_get_model_id "$model")" || main.die_gracefully "Model $model is not known."
	local supported="${CXR_SUPPORTED_MODEL_VERSIONS[${model_id}]}"
	
	# Check the Version
	local found=$(common.string.isSubstringPresent? "$supported" "$version")
	
	if [[ $found == true ]]
	then
		# found, ok
		return 0
	else
		main.log -e   "Version $1 of $2 is not supported by CAMxRunner. Adjust CXR_SUPPORTED_MODEL_VERSIONS\n(Currently $supported)"    
		return 1
	fi
}

################################################################################
# Function: cxr_common_check_run_name
#	
# Checks if run name has correct form and length (max 60 characters)
# e. g. 
# CAMx-v4.51-ENVIRON_testcase or
# PMCAMx-v3.01-test
#
# So we basically split using "-"
#
# Much of the code here is repeated from <main.setModelAndVersion>
#
# Returns:
# Retruns 0 if ok, 1 otherwise.
#
# Parameters:
# $1 - String
################################################################################
function cxr_common_check_run_name()
################################################################################
{
	local oIFS
	local run_array
	local version
	local model

	if [[ $# -ne 1 ]]
	then
		echo 0
		main.log -e   "needs a string as input"
	fi
	
	# Length must not exceed 60 because we use it as
	# "note" field in all files
	if [[ $(string_len $1) -gt 60  ]]
	then
		echo 0
		main.log -e   "A run name must not be longer than 60 characters!"
	fi
	
	# Split it
	oIFS="$IFS"

	IFS=-

	# Suck line into array
	run_array=($1)

	# Reset IFS
	IFS="$oIFS"
	
	# First, there is the model
	model=${run_array[0]}
	
	# Then there is vVersion, remove v to the left:
	version=${run_array[1]#v}
	
	#This returns non-0 if its not ok
	cxr_common_is_version_supported $version $model
		
	# We return the return value
	return $?
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
	
	# Create dummy file to hash
	x=$(cxr_common_create_tempfile test)
	echo  "Franz jagt im komplett verwahrlosten Taxi quer durch Bayern" > $x
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(cxr_common_check_datataype 1 I) true "cxr_common_check_datataype 1 I"
	is $(cxr_common_md5 $x) 4868ac39fdeb60e886791d6be8c0fcb3 "cxr_common_md5 strings test"

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
