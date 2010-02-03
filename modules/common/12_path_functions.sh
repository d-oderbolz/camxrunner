#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Path/File functions of CAMxRunner.
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################

################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=6

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains some path functions for the CAMxRunner"

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
# Function: cxr_common_file_exists?
# 
# Returns true if argument is an existing file, false otherwise.
# Used mostly as a wrapper for testing
#
# Parameters:
# $1 - path of file to test
################################################################################
function cxr_common_file_exists?()
################################################################################
{
	if [ -f "${1}" ]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: cxr_common_is_absolute_path
# 
# Returns true if argument is an absolute path, false otherwise
#
# Parameters:
# $1 - path to test
################################################################################
function cxr_common_is_absolute_path()
################################################################################
{
	case $1 in
		/*) echo true ;;
		*) echo false ;;
	esac
}

################################################################################
# Function: cxr_common_get_file_type
# 
# Returns a parsed version of the output of "file". Curruntly used to detect compressed files by
# <cxr_common_try_decompressing_file> and <init_test.inc>. 
# Returns the empty string if the file is not readable.
#
# Parameters:
# $1 - file to test
################################################################################
function cxr_common_get_file_type()
################################################################################
{
	FILE=$1
	
	FILETYPE=$(file "${FILE}" | cut -f2 -d' ')
	
	if [ "$FILETYPE" == ERROR: ]
	then
		# Something went wrong
		# Could be improved!
		cxr_main_logger -e "$FUNCNAME" "file reported an error for $FILE"
		echo ""
	else
		echo "$FILETYPE"
	fi
}

################################################################################
# Function: cxr_common_compress_output
# 
# Compresses either all output files or such that match the pattern in
# CXR_COMPRESS_OUTPUT_PATTERN.
# We loop thorugh the file 
#
# Parameters:
# None.
#
# Variables:
# $CXR_COMPRESS_OUTPUT - if true, we do it, otherwise not
# $CXR_COMPRESSOR_EXEC - executable that we use
# $CXR_COMPRESS_OUTPUT_PATTERN - Pattern we apply to both filenames and module names. If match - we compress
#
# Files:
# $CXR_INSTANCE_FILE_OUTPUT_LIST - List of files to compress
################################################################################
function cxr_common_compress_output()
################################################################################
{
	if [ "${CXR_COMPRESS_OUTPUT}" == true -a "${CXR_DRY}" == false  ]
	then
		# Loop through file
		if [ ! -z "${CXR_INSTANCE_FILE_OUTPUT_LIST}" ]
		then
			while read line 
			do
				# Structure filename|module
				FILENAME=$(echo "$line" | cut -d${CXR_DELIMITER} -f1)
				MODULE=$(echo "$line" | cut -d${CXR_DELIMITER} -f2)
				
				if [ ! -z "${FILENAME}" ]
				then
					# OK file is not empty
				
					# Do we need to do pattern matching?
					if [ "${CXR_COMPRESS_OUTPUT_PATTERN:-}" ]
					then
						################
						# Check filename
						################
						local FOUND=$(expr match "${FILENAME}" "${CXR_COMPRESS_OUTPUT_PATTERN}")
						
						if [ $FOUND -gt 0 ]
						then
							# Do it
							cxr_main_logger -v "$FUNCNAME" "Compressing ${FILENAME} using ${CXR_COMPRESSOR_EXEC}"
							"${CXR_COMPRESSOR_EXEC}" "${FILENAME}"
							
						else
							################
							# filename did not match - try module name
							################
							local FOUND=$(expr match "${MODULE}" "${CXR_COMPRESS_OUTPUT_PATTERN}")
					
							if [ $FOUND -gt 0 ]
							then
								# Do it
								cxr_main_logger -v "$FUNCNAME" "Compressing ${FILENAME} using ${CXR_COMPRESSOR_EXEC}"
								"${CXR_COMPRESSOR_EXEC}" "${FILENAME}"
							else
								cxr_main_logger -v "$FUNCNAME" "Pattern ${CXR_COMPRESS_OUTPUT_PATTERN} did not match $line, will not compress this file"
							fi
						fi
					else
						# No Pattern matching
						cxr_main_logger -v "$FUNCNAME" "Compressing ${FILENAME} using ${CXR_COMPRESSOR_EXEC}"
						"${CXR_COMPRESSOR_EXEC}" "${FILENAME}"
					fi
				else
					#File empty
					cxr_main_logger -w "$FUNCNAME" "The output file ${FILENAME} is empty, no compression attempted!"
				fi
			done < "${CXR_INSTANCE_FILE_OUTPUT_LIST}" # Loop over lines
		else
			cxr_main_logger -w "$FUNCNAME" "The output file list ${CXR_INSTANCE_FILE_OUTPUT_LIST} is empty!"
		fi
	else
		cxr_main_logger "$FUNCNAME" "Will not compress any output files (either dry run or CXR_COMPRESS_OUTPUT is false.)"
	fi
}

################################################################################
# Function: cxr_common_try_decompressing_file
# 
# Checks if an input file is compressed, decompresses it and returns a new name.
# Wo do this by searching files that have specific suffixes added to their name.
#
# Due to potential permission issues, we decompress into temp files unless CXR_DECOMPRESS_IN_PLACE is true.
# Therefore, we need to keep track which files where decompressed to which tempfiles.
# This is stored in the file CXR_INSTANCE_FILE_DECOMPRESSED.
#
# If the decompression fails, we return the input string.
#
# This function is directly used by <cxr_common_evaluate_rule>
#
# Parameters:
# $1 - path to test
################################################################################
function cxr_common_try_decompressing_file()
################################################################################
{
	INPUT_FILE=$1
	
	cxr_main_logger -a -b "$FUNCNAME" "Testing if ${INPUT_FILE} was compressed..."
	
	local DELIMITER="${CXR_DELIMITER}"

	if [ "$CXR_DETECT_COMPRESSED_INPUT_FILES" == true ]
	then
	
		# Check first if we already have decompressed this file
		# Look for an entry like
		# /mnt/other/lacfs02/jkeller/emiss/emisscamx/20070101/sem050/camx_emiss_domain1_uw3_sem050_20070101.asc|/afs/psi.ch/user/o/oderbolz/tmp/cxr_compression.DWvu7649
		# In $CXR_INSTANCE_FILE_DECOMPRESSED
		touch "$CXR_INSTANCE_FILE_DECOMPRESSED"
		
		LINE=$(grep ${INPUT_FILE}${DELIMITER} $CXR_INSTANCE_FILE_DECOMPRESSED)
		
		if [ "$LINE" ]
		then
			# Already done this file
			# The tempfile is in the second field
			TEMPFILE=$(echo $LINE | cut -d${DELIMITER} -f2)
			
			cxr_main_logger -v "$FUNCNAME" "File ${INPUT_FILE} was already decompressed into $TEMPFILE"
		
			echo "$TEMPFILE"
			return $CXR_RET_OK
		else
			was_compressed=false
		fi
	
		# Create proper array of extensions
		A_CXR_COMPRESSED_EXT=($CXR_COMPRESSED_EXT)

		# Checking if a compressed version of this file exists
		for k in $(seq 0 $(( ${#A_CXR_COMPRESSED_EXT[@]} - 1 )) )
		do
			# The current extension (e. g. .gz)
			EXT=${A_CXR_COMPRESSED_EXT[$k]}
			
			# Note that our extensions already contain a dot
			COMP_FILE="${INPUT_FILE}${EXT}"
			cxr_main_logger -v "$FUNCNAME" "Looking for $COMP_FILE"
			
			if [ -r "$COMP_FILE" ]
			then
			
				# We decompress into a tempfile if we dont decompress in place
				if [ "$CXR_DECOMPRESS_IN_PLACE" == false ]
				then
					# Use a tempfile
					TEMPFILE=$(cxr_common_create_tempfile compression)
				else
					# The target is the "original" file name
					TEMPFILE=${INPUT_FILE}
				fi
				
				# What decompressor to use?
				# This is NOT derived from the filename
				FILETYPE=$(cxr_common_get_file_type "$COMP_FILE")
				
				if [ "${CXR_DRY}" == false ]
				then
				
					case $FILETYPE in
			
						bzip2)
							cxr_main_logger -a "$FUNCNAME" "File ${INPUT_FILE} was compressed using bzip2. I will use $CXR_BUNZIP2_EXEC to decompress."
							$CXR_BUNZIP2_EXEC -c "$COMP_FILE" > $TEMPFILE
							;;
							
						gzip)
							cxr_main_logger -a "$FUNCNAME" "File ${INPUT_FILE} was compressed using gzip. I will use $CXR_GUNZIP_EXEC to decompress."
							"$CXR_GUNZIP_EXEC" -c "$COMP_FILE" > $TEMPFILE
							;;
					
						zip)
							cxr_main_logger -a "$FUNCNAME" "File ${INPUT_FILE} was compressed using zip. I will use $CXR_GUNZIP_EXEC to decompress."
							"$CXR_GUNZIP_EXEC" -S .zip -c "$COMP_FILE" > $TEMPFILE
							;;
					
						*)
							cxr_main_logger -e "$FUNCNAME" "Compressed file type $FILETYPE not supported"
							;;
					esac
	
					# Check retval of decompressor
					if [ $? -eq 0 ]
					then
						was_compressed=true
						NEW_FILE=$TEMPFILE
						break
					else
						cxr_main_logger -e "${FUNCNAME}:${LINENO} - File ${COMP_FILE} could not be decompressed by $DECOMP"
					fi
				else
					cxr_main_logger -a "$FUNCNAME" "File ${INPUT_FILE} is compressed using $FILETYPE but in a dryrun, we do not decompress."
					was_compressed=true
					NEW_FILE=$TEMPFILE
					break
				fi
			fi
			
		done
		
		if [ "$was_compressed" == true ]
		then
			# In CXR_INSTANCE_FILE_DECOMPRESSED
			echo "${INPUT_FILE}${DELIMITER}${NEW_FILE}" >> $CXR_INSTANCE_FILE_DECOMPRESSED
		
			echo "$NEW_FILE"
		else
			# Was not compressed
			cxr_main_logger -a "$FUNCNAME" "File ${INPUT_FILE} is not compressed."
			echo "$INPUT_FILE"
		fi
		
	else
		# We do not consider compressed files
		cxr_main_logger -v "$FUNCNAME" "We do not try to decompress files, CXR_DETECT_COMPRESSED_INPUT_FILES is false"
		echo "$INPUT_FILE"
	fi
}


################################################################################
# Function: cxr_common_get_fs_type
#
# Returns the the lowercase name of the filesystem in use in a particular path.
# Returns the empty string on error.
#
# Note that df -T sometimes produces weird output - I try to catch it, but it might fail...
#
#
# Parameters:
# $1 - path to test
################################################################################
function cxr_common_get_fs_type()
################################################################################
{
	if [ $# -ne 1 ]
	then
		echo ext3
		return $CXR_RET_OK
	fi
	
	# Count numbers of line output (should be 2, but I have soon output like
	# Filesystem    Type   1K-blocks      Used Available Use% Mounted on
	# /dev/cciss/c0d0p2
	#            ext3    15872636   7125760   7927564  48% /
	
	# Count number of lines in output
	NUM_LINES=$(df -T "$1" | wc -l)
	
	# Get last line
	LAST_LINE=$(df -T "$1" | tail -n1)
	
	# Parse the Last line
	oIFS="$IFS"

	IFS=" "
	
	# Suck line into LINE_ARRAY
	DF_ARRAY=($LAST_LINE)
	
	# Reset IFS
	IFS="$oIFS"
	
	if [ "$NUM_LINES" -eq 3 ]
	then
		cxr_main_logger -v "${FUNCNAME}"  "FS type of $1 was determined to be ${DF_ARRAY[0]}. If this is strange, check your df -T output!"
		echo "${DF_ARRAY[0]}" 
	elif [ "$NUM_LINES" -eq 2 ]
	then
		cxr_main_logger -v "${FUNCNAME}"  "FS type of $1 was determined to be ${DF_ARRAY[1]}. If this is strange, check your df -T output!"
		echo "${DF_ARRAY[1]}" 
	else
		cxr_main_logger -e "${FUNCNAME}" "Could not determine FS type of $1. Check your df -T output!"
		echo ""
	fi
}

################################################################################
# Function: cxr_common_file_size_megabytes
#
# Returns the number of megabytes used by a file, rounded to the nearest MB
#
# Parameters:
# $1 - path to file to test
################################################################################
function cxr_common_file_size_megabytes()
################################################################################
{
	if [ ! -f "$1" ]
	then
		cxr_main_logger -w "$FUNCNAME" "The file $1 does not exist!"
		echo 0
		return $CXR_RET_PARAM_ERROR
	fi
	
	SIZE=$(du -m "$1" | cut -f1)
	
	echo $SIZE
	
	return $CXR_RET_OK
}

################################################################################
# Function: cxr_common_free_megabytes
#
# returns the number of megabytes free in given path (floored?)
# This might actually fail on non Linux-systems...
#
# Internally determines the FS type; supports quota on afs only
#
# Parameters:
# $1 - path to test
################################################################################
function cxr_common_free_megabytes()
################################################################################
{
	if [ $# -ne 1 ]
	then
		echo 0
		return $CXR_RET_OK
	fi
	
	# Get File system
	FS=$(cxr_common_get_fs_type $1)
	
	case $FS in
	afs)
			cxr_main_logger -v "${FUNCNAME}" "Directory $1 seems to be on AFS. Getting AFS quota..."
			
			# Get last line
			LAST_LINE=$(fs listquota "$1" | tail -n1)
			
			# Parse the Last line
			oIFS="$IFS"
		
			IFS=" "
			
			# Suck line into LINE_ARRAY
			# Line looks like this:
			# usr.oderbolz                3000000   2440839   81%         90%
			QUOTA_ARRAY=($LAST_LINE)
			
			# Reset IFS
			IFS="$oIFS"
			
			# Calculate free KiBi
			FREE_KB=$(( ${QUOTA_ARRAY[1]} - ${QUOTA_ARRAY[2]} ))
			
			# Convert to MB
			FREE_MB=$(( $FREE_KB / 1024 ))
			
			echo $FREE_MB
			;;
	*) 
			# Default
			df --block-size=1M $1 | tail -n1 | awk '{ print $3 }'
			;;
	esac
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
	if [ "${CXR_TESTING_FROM_HARNESS:-false}" == false ]
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
			if [ $(pwd) == / ]
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
	
	# We need a tempfile
	a=$(cxr_common_create_tempfile)
	
	echo "Hallo" > $a
	
	# Set settings
	CXR_COMPRESS_OUTPUT=true
	CXR_COMPRESSOR_EXEC="${CXR_BZIP2_EXEC}"
	CXR_COMPRESS_OUTPUT_PATTERN=
	
	# Add this file to the output file list
	echo "${a}${CXR_DELIMITER}path_functions" > "${CXR_INSTANCE_FILE_OUTPUT_LIST}"
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(cxr_common_is_absolute_path /) true "cxr_common_is_absolute_path /"
	is $(cxr_common_file_size_megabytes $a) 1 "cxr_common_file_size_megabytes of small file"
	
	# compress
	cxr_common_compress_output
	
	#Test
	is $(cxr_common_file_exists? ${a}.bz2 ) true "cxr_common_compress_output with simple file, no pattern"
	
	# Decompress again
	${CXR_BUNZIP2_EXEC} ${a}.bz2
	
	# Set pattern correct
	CXR_COMPRESS_OUTPUT_PATTERN="path_functions"
	
	# compress
	cxr_common_compress_output
	
	#Test
	is $(cxr_common_file_exists? ${a}.bz2 ) true "cxr_common_compress_output with simple file, matching pattern"
	
	# Decompress again
	${CXR_BUNZIP2_EXEC} ${a}.bz2
	
	# Set pattern correct
	CXR_COMPRESS_OUTPUT_PATTERN="path_.*"
	
	# compress
	cxr_common_compress_output
	
	#Test
	is $(cxr_common_file_exists? ${a}.bz2 ) true "cxr_common_compress_output with simple file, matching pattern"
	
	# Decompress again
	${CXR_BUNZIP2_EXEC} ${a}.bz2
	
	# Set pattern incorrect
	CXR_COMPRESS_OUTPUT_PATTERN=guagg
	
	# compress
	cxr_common_compress_output
	
	#Test
	is $(cxr_common_file_exists? ${a}.bz2 ) false "cxr_common_compress_output with simple file, not matching pattern"
	
	# No decompression needed (its not compressed)
	
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
if [ -z "${CXR_META_MODULE_NAME:-}" ]
then

	# When using getopts, never directly call a function inside the case,
	# otherwise getopts does not process any parametres that come later
	while getopts ":dvFST" opt
	do
		case "${opt}" in
		
			d) CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_LOG_EXT="-dry" ;;
			v) CXR_USER_TEMP_VERBOSE=true ; echo "Enabling VERBOSE (-v) output. All lines starting with % would not be present otherwise" ;;
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
	if [ "${TEST_IT:-false}" == true ]
	then
		test_module
	else
		usage
	fi

fi