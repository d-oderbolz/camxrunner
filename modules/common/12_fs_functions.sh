#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Filesystem functions of CAMxRunner.
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
CXR_META_MODULE_NUM_TESTS=12

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|dos2unix exec|unix2dos"

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains filesystem functions for the CAMxRunner"

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
# Function: common.fs.isNotEmpty?
# 
# Returns true if argument is an non-empty file, false otherwise.
# Used mostly as a wrapper for testing
#
# Parameters:
# $1 - path of file to test
################################################################################
function common.fs.isNotEmpty?()
################################################################################
{
	if [[ -s "${1}" ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.fs.exists?
# 
# Returns true if argument is an existing file, false otherwise.
# Used mostly as a wrapper for testing
#
# Parameters:
# $1 - path of file to test
################################################################################
function common.fs.exists?()
################################################################################
{
	if [[ -f "${1}" ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.fs.isAbsolutePath?
# 
# Returns true if argument is an absolute path, false otherwise
#
# Parameters:
# $1 - path to test
################################################################################
function common.fs.isAbsolutePath?()
################################################################################
{
	case $1 in
		/*) echo true ;;
		*) echo false ;;
	esac
}


################################################################################
# Function: common.fs.sameDevice?
# 
# Returns true if the two arguments reside on the same device.
# This test is crucial whan attempting to hard-link.
#
# Parameters:
# $1 - path1 to test
# $2 - path2 to test
################################################################################
function common.fs.sameDevice?()
################################################################################
{
	local file1="$1"
	local file2="$2"
	
	local dev1="$(stat -c"%d" "${file1}")"
	local dev2="$(stat -c"%d" "${file2}")"
	
	if [[ "$dev1" -eq "$dev2" ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.fs.getMtime
# 
# Returns the files mtime (modification time, last update of the data, or touch)
# As seconds since epoch (January 1, 1970).
# Returns 0 on error.
#
# Parameters:
# $1 - filename to analyse
################################################################################
function common.fs.getMtime()
################################################################################
{
	local file=$1
	local mtime
	
	if [[ -f "${file}"  ]]
	then
		mtime="$(stat "${file}" -c"%Y")"
	else
		main.log -e  "No valid filename passed!"
		mtime=0
	fi
	
	echo "${mtime}"
}

################################################################################
# Function: common.fs.getFileType
# 
# Returns a parsed version of the output of "file". Currently used to detect compressed files by
# <common.fs.TryDecompressingFile> and <init_test.inc>. 
# Returns the empty string if the file is not readable.
#
# Parameters:
# $1 - file to test
################################################################################
function common.fs.getFileType()
################################################################################
{
	local file=$1
	local filetype
	
	filetype=$(file "${file}" | cut -f2 -d' ')
	
	if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
	then
		# Something went wrong
		main.log -e  "the file command reported an error for $file"
		echo ""
	else
		echo "$filetype"
	fi
}

################################################################################
# Function: common.fs.isDos?
# 
# Returns a parsed version of the output of "file", can detect dos-files.
#
# Parameters:
# $1 - file to test
################################################################################
function common.fs.isDos?()
################################################################################
{
	local file=$1
	local filetype
	
	# Grep returns a string if found
	found="$(file "${file}" | grep "CRLF line terminators" )"
	
	if [[ "$found" ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.fs.FileSizeMb
#
# Returns the number of megabytes used by a file, rounded to the nearest MB
#
# Parameters:
# $1 - path to file to test
################################################################################
function common.fs.FileSizeMb()
################################################################################
{
	local size
	
	if [[ ! -f "$1"  ]]
	then
		main.log -w  "The file $1 does not exist!"
		echo 0
		return $CXR_RET_PARAM_ERROR
	fi
	
	size=$(du -m "$1" | cut -f1)
	
	echo $size
	
	return $CXR_RET_OK
}

################################################################################
# Function: common.fs.WaitForFile
#
# Waits for a file to appear. Returns true on success, false otherwise
#
# Parameters:
# $1 - path to file to test
#
# Variables:
# $CXR_TIMEOUT_MINS - number of minutes to wait for this file
# $CXR_TOTAL_WAITING_MINS - total number of minutes to wait
################################################################################
function common.fs.WaitForFile()
################################################################################
{
	local filename="$1"
	
	until [[ -f $filename && ! ( $waited_mins -gt $CXR_TIMEOUT_MINS || $total_waited_mins -gt $CXR_TOTAL_WAITING_MINS ) ]]
	do
		sleep ${CXR_WAITING_SLEEP_SECONDS}
		waited_mins=$(( ($waited_mins + $CXR_WAITING_SLEEP_SECONDS)/60  ))
		total_waited_mins=$(( $total_waited_mins + $waited_mins  ))
	done
	
	# If we arrive here and the file is not there, we failed
	if [[ ! -f "$filename" ]]
	then
		main.log -e  "$filename still does not exist, timeout reached."
		echo false
	else
		main.log -v  "$filename exists now."
		echo true
	fi
}

################################################################################
# Function: common.fs.WaitForStableSize
#
# Waits until a file has a constant size. Returns true on success, false otherwise
#
# Parameters:
# $1 - path to file to test
#
# Variables:
# $CXR_TIMEOUT_MINS - number of minutes to wait for this file
# $CXR_TOTAL_WAITING_MINS - total number of minutes to wait
################################################################################
function common.fs.WaitForStableSize()
################################################################################
{
	local filename="$1"
	local old_size=0
	
	until [[ $(common.fs.FileSizeMb $filename) -eq $old_size  &&  ( $waited_mins -gt $CXR_TIMEOUT_MINS || $total_waited_mins -gt $CXR_TOTAL_WAITING_MINS ) ]]
	do
		# Store the current size as old
		old_size="$(common.fs.FileSizeMb $filename)"
		
		sleep ${CXR_WAITING_SLEEP_SECONDS}
		waited_mins=$(( ($waited_mins + $CXR_WAITING_SLEEP_SECONDS)/60  ))
		total_waited_mins=$(( $total_waited_mins + $waited_mins  ))
	done
	
	# We fail if the filesize is 0 or it still grows
	if [[ $(common.fs.FileSizeMb $filename) -eq 0 || $(common.fs.FileSizeMb $filename) -ne $old_size ]]
	then
		main.log -e  "$filename still seems to grow."
		echo false
	else
		main.log -v  "$filename size is stable now."
		echo true
	fi
}

################################################################################
# Function: common.fs.CompressOutput
# 
# Compresses either all output files or such that match the pattern in
# CXR_COMPRESS_OUTPUT_PATTERN and are bigger than CXR_COMPRESS_THRESHOLD_MB
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
function common.fs.CompressOutput()
################################################################################
{
	local filename
	local module
	local do_this
	local found
	
	if [[  "${CXR_COMPRESS_OUTPUT}" == true && "${CXR_DRY}" == false ]]
	then
		# Loop through CXR_INSTANCE_FILE_OUTPUT_LIST
		if [[ -s "${CXR_INSTANCE_FILE_OUTPUT_LIST}"  ]]
		then
			while read line 
			do
				# Structure filename|module
				filename=$(echo "$line" | cut -d${CXR_DELIMITER} -f1)
				module=$(echo "$line" | cut -d${CXR_DELIMITER} -f2)
				
				if [[ -s "${filename}" ]]
				then
					# OK file is not empty
					
					# We do not yet know if we need to do it
					do_this=false
				
					# Do we need to do pattern matching?
					if [[ "${CXR_COMPRESS_OUTPUT_PATTERN:-}"  ]]
					then
						################
						# Check filename
						################
						found=$(expr match "${filename}" "${CXR_COMPRESS_OUTPUT_PATTERN}")
						
						if [[ $found -gt 0 ]]
						then
							# Do it
							do_this=true
						else
							################
							# filename did not match - try module name
							################
							found=$(expr match "${module}" "${CXR_COMPRESS_OUTPUT_PATTERN}")
					
							if [[ $found -gt 0  ]]
							then
								# Do it
								do_this=true
							else
								main.log -v  "Pattern ${CXR_COMPRESS_OUTPUT_PATTERN} did not match $line, will not compress this file"
							fi
						fi
					else
						# No Pattern matching needed
						do_this=true
					fi
					
					# Do it if it is large enough
					if [[ "$do_this" == true  ]]
					then
						if [[ $(common.fs.FileSizeMb ${filename}) -ge "${CXR_COMPRESS_THRESHOLD_MB}"  ]]
						then
							main.log -v  "Compressing ${filename} using ${CXR_COMPRESSOR_EXEC}"
							"${CXR_COMPRESSOR_EXEC}" "${filename}"
						else
							main.log -v  "${filename} is smaller than CXR_COMPRESS_THRESHOLD_MB (${CXR_COMPRESS_THRESHOLD_MB})."
						fi
					fi
				else
					#File empty
					main.log -w  "The output file ${filename} is empty, no compression attempted!"
				fi
			done < "${CXR_INSTANCE_FILE_OUTPUT_LIST}" # Loop over lines
		else
			main.log -w  "The output file list ${CXR_INSTANCE_FILE_OUTPUT_LIST} is empty!"
		fi
	else
		main.log -a "Will not compress any output files (either dry run or CXR_COMPRESS_OUTPUT is false.)"
	fi
}

################################################################################
# Function: common.fs.TryDecompressingFile
# 
# Checks if an input file is compressed, decompresses it and returns a new name.
# Wo do this by searching files that have specific suffixes added to their name.
# We decompress even if its a dry-run.
#
# Due to potential permission issues, we decompress into temp files unless CXR_DECOMPRESS_IN_PLACE is true.
# Therefore, we need to keep track which files where decompressed to which tempfiles.
# This is stored in the file CXR_DECOMPRESSED_LIST.
#
# If the decompression fails, we return the input string.
#
# This function is directly used by <common.runner.evaluateRule>
#
# Parameters:
# $1 - path to test
################################################################################
function common.fs.TryDecompressingFile()
################################################################################
{
	local input_file=$1
	# We assume that in was not compressed
	local was_compressed=false
	local line
	local tempfile
	local sed_tmp
	local k
	local ext
	local comp_file
	local filetype
	local new_file
	local a_cxr_compressed_ext
	
	if [[ "$CXR_DETECT_COMPRESSED_INPUT_FILES" == true  ]]
	then
		main.log -v -B  "Testing compression on $(basename ${input_file})..."
	
		# Check first if we already have decompressed this file
		# Look for an entry like
		# /mnt/other/lacfs02/jkeller/emiss/emisscamx/20070101/sem050/camx_emiss_domain1_uw3_sem050_20070101.asc|Some_Temp_File
		# In $CXR_DECOMPRESSED_LIST
		touch "$CXR_DECOMPRESSED_LIST"
		
		line="$(grep "${input_file}${CXR_DELIMITER}" $CXR_DECOMPRESSED_LIST | head -n1 )"
		
		if [[ "$line"  ]]
		then
			# Seems like we already did this file
			# The tempfile is in the second field
			tempfile="$(echo $line | cut -d${CXR_DELIMITER} -f2)"
			
			if [[ -s "$tempfile" ]]
			then
				main.log -v  "File ${input_file} was already decompressed into $tempfile"
			
				# tempfile not empty
				echo "$tempfile"
				return $CXR_RET_OK
			else
				# tempfile empty, need to repeat
				main.log -v  "File ${input_file} was already decompressed into $tempfile but for some reason that file is empty!"
				
				# First remove that line via sed
				sed_tmp=$(common.runner.createTempFile sed)
				sed '/$line/d' "${CXR_DECOMPRESSED_LIST}" > "${sed_tmp}"
				mv "${sed_tmp}" "${CXR_DECOMPRESSED_LIST}"
			fi	
		fi # Entry found in compressed list?
		
		# Create proper array of extensions
		a_cxr_compressed_ext=($CXR_COMPRESSED_EXT)

		# Checking if a compressed version of this file exists
		for k in $(seq 0 $(( ${#a_cxr_compressed_ext[@]} - 1 )) )
		do
			# The current extension (e. g. .gz)
			ext=${a_cxr_compressed_ext[$k]}
			
			# Note that our extensions already contain a dot
			comp_file="${input_file}${ext}"
			main.log -v  "Looking for $comp_file"
			
			if [[ -r "$comp_file"  ]]
			then
				# File is readable
				
				# We decompress into a tempfile if we don't decompress in place
				if [[ "$CXR_DECOMPRESS_IN_PLACE" == false  ]]
				then
					# Use a tempfile and give it a recogisable name. It will not be added to the templist (managed here)
					tempfile=$(common.runner.createTempFile decomp_$(basename ${input_file}) false)
				else
					# The target is the "original" file name
					tempfile=${input_file}
				fi
				
				# What decompressor to use?
				# This is NOT derived from the filename
				filetype=$(common.fs.getFileType "$comp_file")
				
				case $filetype in
		
					bzip2)
						main.log -a  "${input_file} is bzip2-compressed. Using $CXR_BUNZIP2_EXEC to decompress..."
						$CXR_BUNZIP2_EXEC -c "$comp_file" > $tempfile
						;;
						
					gzip)
						main.log -a  "${input_file} is gzip-compressed. Using $CXR_GUNZIP_EXEC to decompress..."
						"$CXR_GUNZIP_EXEC" -c "$comp_file" > $tempfile
						;;
				
					zip)
						main.log -a  "${input_file} is zip-compressed. Using $CXR_GUNZIP_EXEC to decompress..."
						"$CXR_GUNZIP_EXEC" -S .zip -c "$comp_file" > $tempfile
						;;
				
					*)
						main.log -e  "Compressed file type $filetype not supported"
						;;
				esac

				# Check retval of decompressor
				if [[ $? -eq 0  ]]
				then
					was_compressed=true
					new_file=$tempfile
					break
				else
					main.log -e "File ${comp_file} could not be decompressed!"
				fi

			fi # File readable?
			
		done # Loop over extensions


		if [[ "$was_compressed" == true  ]]
		then
			# In CXR_DECOMPRESSED_LIST
			echo "${input_file}${CXR_DELIMITER}${new_file}" >> $CXR_DECOMPRESSED_LIST
		
			echo "$new_file"
		else
			# Was not compressed
			main.log -v  "File ${input_file} is not compressed."
			echo "$input_file"
		fi
		
	else
		# We do not consider compressed files
		main.log -v  "We do not try to decompress files, CXR_DETECT_COMPRESSED_INPUT_FILES is false"
		echo "$input_file"
	fi # Detect compression?
}


################################################################################
# Function: common.fs.getType
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
function common.fs.getType()
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		main.log -e  "Could not determine FS type - no path passed!"
		echo ""
		return $CXR_RET_ERROR
	fi
	
	local num_lines
	local last_line
	local oIFS
	local df_array
	
	# Count numbers of line output (should be 2, but I have soon output like
	# Filesystem    Type   1K-blocks      Used Available Use% Mounted on
	# /dev/cciss/c0d0p2
	#            ext3    15872636   7125760   7927564  48% /
	
	# Count number of lines in output
	num_lines=$(df -T "$1" | wc -l)
	
	# Get last line
	last_line=$(df -T "$1" | tail -n1)
	
	# Parse the Last line
	oIFS="$IFS"

	IFS=" "
	
	# Suck line into LINE_ARRAY
	df_array=($last_line)
	
	# Reset IFS
	IFS="$oIFS"
	
	if [[ "$num_lines" -eq 3  ]]
	then
		main.log -v   "FS type of $1 was determined to be ${df_array[0]}. If this is strange, check your df -T output!"
		echo "${df_array[0]}" 
	elif [[ "$num_lines" -eq 2  ]]
	then
		main.log -v   "FS type of $1 was determined to be ${df_array[1]}. If this is strange, check your df -T output!"
		echo "${df_array[1]}" 
	else
		main.log -e  "Could not determine FS type of $1. Check your df -T output!"
		echo ""
	fi
}

################################################################################
# Function: common.fs.getFreeMb
#
# returns the number of megabytes free in given path (floored?)
# This might actually fail on non Linux-systems...
#
# Internally determines the FS type; supports quota on afs only
#
# Parameters:
# $1 - path to test
################################################################################
function common.fs.getFreeMb()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		echo 0
		return $CXR_RET_OK
	fi
	
	local last_line
	local fs
	local oIFS
	local quota_array
	local free_kb
	local free_mb
	
	# Get File system
	fs=$(common.fs.getType $1)
	
	case $fs in
	afs)
			main.log -v  "Directory $1 seems to be on AFS. Getting AFS quota..."
			
			# Get last line
			last_line=$(fs listquota "$1" | tail -n1)
			
			# Parse the Last line
			oIFS="$IFS"
		
			IFS=" "
			
			# Suck line into LINE_ARRAY
			# Line looks like this:
			# usr.oderbolz                3000000   2440839   81%         90%
			quota_array=($last_line)
			
			# Reset IFS
			IFS="$oIFS"
			
			# Calculate free KiBi
			free_kb=$(( ${quota_array[1]} - ${quota_array[2]} ))
			
			# Convert to MB
			free_mb=$(( $free_kb / 1024 ))
			
			echo $free_mb
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
# update CXR_META_module_NUM_TESTS in the header!
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
	
	# We need a few tempfiles
	a=$(common.runner.createTempFile $FUNCNAME)
	b=$(common.runner.createTempFile $FUNCNAME)
	
	echo "Hallo" > "$a"
	echo "Velo" >> "$a"
	
	# Set settings
	CXR_COMPRESS_OUTPUT=true
	CXR_COMPRESSOR_EXEC="${CXR_BZIP2_EXEC}"
	CXR_COMPRESS_OUTPUT_PATTERN=
	
	# Add this file to the output file list
	echo "${a}${CXR_DELIMITER}path_functions" > "${CXR_INSTANCE_FILE_OUTPUT_LIST}"
	
	# Mtime
	touch $a
	
	# This is our time
	local rtc=$(date "+%s") 
	
	# Time of file
	local ft=$(common.fs.getMtime $a)
	
	# create a 100 MB file
	dd bs=100M if=/dev/zero of=$b count=1
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	# We expect a difference of max 1 second (if we are at the boundary)
	differs_less_or_equal $rtc $ft 1 "common.fs.getMtime immediate, time difference ok"
	is $(common.fs.isAbsolutePath? /) true "common.fs.isAbsolutePath? /"
	is $(common.fs.FileSizeMb $a) 1 "common.fs.FileSizeMb of small file"
	is $(common.fs.FileSizeMb $b) 100 "common.fs.FileSizeMb of 100MB file"
	is $(common.fs.sameDevice? . .) true "cxr_common_same_fs with twice the current path"
	is $(common.fs.sameDevice? /proc .) false "cxr_common_same_fs with proc and current path"
	
	# test the dos-detection
	${CXR_UNIX2DOS_EXEC} "$a"
	is $(common.fs.isDos? "$a") true "common.fs.isDos? on dos-file"
	
	${CXR_DOS2UNIX_EXEC} "$a"
	is $(common.fs.isDos? "$a") false "common.fs.isDos? on unix-file"
	
	# compress
	common.fs.CompressOutput
	
	#Test
	is $(common.fs.exists? ${a}.bz2 ) true "common.fs.CompressOutput with simple file, no pattern"
	
	# Decompress again
	${CXR_BUNZIP2_EXEC} ${a}.bz2
	
	# Set pattern correct
	CXR_COMPRESS_OUTPUT_PATTERN="path_functions"
	
	# compress
	common.fs.CompressOutput
	
	#Test
	is $(common.fs.exists? ${a}.bz2 ) true "common.fs.CompressOutput with simple file, matching pattern"
	
	# Decompress again
	${CXR_BUNZIP2_EXEC} ${a}.bz2
	
	# Set pattern correct
	CXR_COMPRESS_OUTPUT_PATTERN="path_.*"
	
	# compress
	common.fs.CompressOutput
	
	#Test
	is $(common.fs.exists? ${a}.bz2 ) true "common.fs.CompressOutput with simple file, matching pattern"
	
	# Decompress again
	${CXR_BUNZIP2_EXEC} ${a}.bz2
	
	# Set pattern incorrect
	CXR_COMPRESS_OUTPUT_PATTERN=guagg
	
	# compress
	common.fs.CompressOutput
	
	#Test
	is $(common.fs.exists? ${a}.bz2 ) false "common.fs.CompressOutput with simple file, not matching pattern"
	
	# No decompression needed (its not compressed)
	
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