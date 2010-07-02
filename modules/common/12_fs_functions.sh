# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
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
CXR_META_MODULE_NUM_TESTS=24

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
CXR_META_MODULE_DESCRIPTION="Contains filesystem and memory functions for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2009), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'


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
# Function: common.fs.isSubDirOf?
# 
# Returns true if argument1 is a subdir of argument2.
# If they two directories are the same, they are defined to be subdirs of each other.
#
# Parameters:
# $1 - path to test if it is a subdir
# $2 - path suspected to be the root
################################################################################
function common.fs.isSubDirOf?()
################################################################################
{
	# first, we need proper names
	local path1="$1"
	local path2="$2"
	
	# If they are the same, they are subdirs of each other
	if [[ "$path1" == "$path2" ]]
	then
		echo true
	else
		# They are not the same
		# We add a slash to the suspected root unless it is root
		if [[ "$path2" != "/" ]]
		then
			path2="$path2"/
		fi
		
		# If path1 is a subdir of path2,
		# the string making up path1 must be the start of path2
		if [[ "$(common.string.isSubstringPresent? "$path1" "$path2")" == true ]]
		then
			echo true
		else
			echo false
		fi
	fi
	
}
################################################################################
# Function: common.fs.getType
# 
# Gets the filesystem a file or directory resides on. Needs full path.
# Returns the empty string on error.
# Can translate a few ones that stat does not always know.
# (See "man statfs" for more)
#
# Parameters:
# $1 - path to test
################################################################################
function common.fs.getType()
################################################################################
{
	local path=${1}
	local dir="$(basename $path)"
	
	if [[ "$dir" && -d "$dir" ]]
	then
		result="$(stat -f -L -c %T "$dir")"
		
		if [[ "$result" == "UNKNOWN (0x5346414f)" ]]
		then
			result=afs
		fi
	
		if [[ "$result" == "UNKNOWN (0xff534d42)" ]]
		then
			result=cifs
		fi

	else
		echo ""
	fi
	
}

################################################################################
# Function: common.fs.isLocal?
# 
# Returns true if a given path is on a local (non-networked) Filesystem, false otherwise.
#
# Parameters:
# $1 - path to test
################################################################################
function common.fs.isLocal?()
################################################################################
{
	local path=${1}
	local type="$(common.fs.getType "$path")"

	case "$type" in
	
		afs|nfs|cifs)	echo false ;;
		*) echo true ;;
	
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
	
	if [[ -e "${file}"  ]]
	then
		mtime="$(stat "${file}" -c"%Y")"
	else
		main.log -e  "No valid filename passed: ${file} !"
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
# Hashes:
# $CXR_INSTANCE_HASH_OUTPUT_FILES - Hash of files to compress
################################################################################
function common.fs.CompressOutput()
################################################################################
{
	local filename
	local module
	local do_this
	local found
	
	if [[ "${CXR_COMPRESS_OUTPUT}" == true && "${CXR_DRY}" == false ]]
	then

		oIFS="$IFS"
		local keyString="$(common.hash.getKeys $CXR_INSTANCE_HASH_OUTPUT_FILES $CXR_HASH_TYPE_INSTANCE)"
		IFS="$CXR_DELIMITER"
		# Turn string into array (we cannot call <common.hash.getKeys> directly here!)
		local arrKeys=( $keyString )
		# Reset Internal Field separator
		IFS="$oIFS"
		
		# looping through keys (safest approach)
		for iKey in $( seq 0 $(( ${#arrKeys[@]} - 1)) )
		do
			filename=${arrKeys[$iKey]}
		
			# the module is the value
			module="$(common.hash.get $CXR_INSTANCE_HASH_OUTPUT_FILES $CXR_HASH_TYPE_INSTANCE $filename)"
			
			main.log -v "Testing if we compress $filename (created by $module)..."
			
			if [[ -s "${filename}" ]]
			then
				# OK file is not empty
				
				# We do not yet know if we need to do it
				do_this=false
			
				# Do we need to do pattern matching?
				if [[ "${CXR_COMPRESS_OUTPUT_PATTERN:-}" ]]
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
							main.log -v  "Pattern ${CXR_COMPRESS_OUTPUT_PATTERN} did not match ${filename} or ${module}, will not compress this file"
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
						main.log -w "${filename} is smaller than CXR_COMPRESS_THRESHOLD_MB (${CXR_COMPRESS_THRESHOLD_MB}) - will not compress it."
					fi
				fi
			else
				#File empty
				main.log -w  "The output file ${filename} is empty, no compression attempted!"
			fi
		done # Loop over files

	else
		main.log -a "Will not compress any output files (either dry run or CXR_COMPRESS_OUTPUT is false.)"
	fi
}

################################################################################
# Function: common.fs.isCompressed?
# 
# Checks if an input file is compressed, returns true if so, false otherwise.
# Is not used by <common.fs.TryDecompressingFile> by design, but is used e. g.
# by <common.check.postconditions> to detect existing, but compressed files
#
#
# Parameters:
# $1 - path to test
################################################################################
function common.fs.isCompressed?()
################################################################################
{
		local input_file=${1:-/dev/null}
		local iExt
		local ext
		local comp_file
		
		# Create proper array of extensions
		a_cxr_compressed_ext=($CXR_COMPRESSED_EXT)

		# Checking if a compressed version of this file exists
		for iExt in $(seq 0 $(( ${#a_cxr_compressed_ext[@]} - 1 )) )
		do
			# The current extension (e. g. .gz)
			ext=${a_cxr_compressed_ext[${iExt}]}
			
			# Note that our extensions already contain a dot
			comp_file="${input_file}${ext}"
			main.log -v  "Looking for $comp_file"
			
			if [[ -r "$comp_file"  ]]
			then
				echo true
				return $CXR_RET_OK
			fi
		done
		
		# If we arrive here, its probably not compressed
		echo false
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
# This is stored in the hash CXR_GLOBAL_HASH_DECOMPRESSED_FILES.
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
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully  "Could not do decompression - no path passed!"
	fi
	
	
	local input_file=$1
	# We assume that in was not compressed
	local was_compressed=false
	local line
	local tempfile
	local sed_tmp
	local iExt
	local ext
	local comp_file
	local filetype
	local new_file
	local a_cxr_compressed_ext
	
	if [[ "$CXR_DETECT_COMPRESSED_INPUT_FILES" == true  ]]
	then
		main.log -v -B  "Testing compression on $(basename ${input_file})..."
	
		# Check first if we already have decompressed this file
		if [[ "$(common.hash.has? $CXR_GLOBAL_HASH_DECOMPRESSED_FILES $CXR_HASH_TYPE_GLOBAL "${input_file}")" == true ]]
		then
			# Seems like we already did this file
			# The tempfile is the value
			tempfile="$(common.hash.get $CXR_GLOBAL_HASH_DECOMPRESSED_FILES $CXR_HASH_TYPE_GLOBAL "${input_file}")"
			
			if [[ -s "$tempfile" ]]
			then
				main.log -v  "File ${input_file} was already decompressed into $tempfile"
			
				# tempfile not empty
				echo "$tempfile"
				return $CXR_RET_OK
			else
				# tempfile empty, need to repeat
				main.log -v  "File ${input_file} was already decompressed into $tempfile but for some reason that file is empty!"
				
				# First remove that entry
				common.hash.delete $CXR_GLOBAL_HASH_DECOMPRESSED_FILES $CXR_HASH_TYPE_GLOBAL "${input_file}"
			fi	
		fi # Entry found in compressed list?
		
		# Create proper array of extensions
		a_cxr_compressed_ext=($CXR_COMPRESSED_EXT)

		# Checking if a compressed version of this file exists
		for iExt in $(seq 0 $(( ${#a_cxr_compressed_ext[@]} - 1 )) )
		do
			# The current extension (e. g. .gz)
			ext=${a_cxr_compressed_ext[${iExt}]}
			
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
			# Put into Hash with new_file as value
			common.hash.put $CXR_GLOBAL_HASH_DECOMPRESSED_FILES $CXR_HASH_TYPE_GLOBAL "${input_file}" "$new_file"
		
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
			main.log -v  "Directory $1 seems to be on AFS. Getting AFS quota, if any..."
			
			# Get last line
			last_line=$(fs listquota "$1" | tail -n1)
			
			if [[ "$(common.string.isSubstringPresent? "$last_line" "no limit" )" == true ]]
			then
				main.log -v  "There seems to be no quota on $1, using df"
				
				df --block-size=1M $1 | tail -n1 | awk '{ print $4 }'
			else
				# Quota must be taken into account
			
				# Suck line into LINE_ARRAY
				# Line looks like this:
				# usr.oderbolz                3000000   2440839   81%         90%
				# or this
				# usr.oderbolz                5000000   3319991   66%        174%<<  <<WARNING
				# or even this:
				# lacfs.nb                   no limit-2092752580    0%       1077%<<  <<WARNING
				quota_array=($last_line)
				
				# Calculate free KiBi
				free_kb=$(( ${quota_array[1]} - ${quota_array[2]} ))
				
				# Convert to MB
				free_mb=$(( $free_kb / 1024 ))
				
				echo $free_mb
			fi
			;;
	*) 
			# Default
			df --block-size=1M $1 | tail -n1 | awk '{ print $4 }'
			;;
	esac
}

################################################################################
# Function: common.memory.getFreePercent
#
# Estimates the percentage of free memory from the output of top.
#
#
################################################################################
function common.memory.getFreePercent()
################################################################################
{
	local usedPercent=0
	
	# Memory percent is in the 10th column
	# The first 7 lines are header
	for used in $(top -b -n1 | sed '1,7d' | awk '{ print $10 }')
	do
		usedPercent="$(common.math.FloatOperation "$usedPercent + $used" 1 0)"
	done
	
	free="$(common.math.FloatOperation "100 - $usedPercent" -1 0)"
	
	main.log -v "Found $free % free memory"
	
	echo $free
	
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
	########################################
	# Setup tests if needed
	########################################
	
	# We need a few tempfiles
	a=$(common.runner.createTempFile $FUNCNAME)
	b=$(common.runner.createTempFile $FUNCNAME)
	c=$(common.runner.createTempFile $FUNCNAME)
	
	echo "Hallo" > "$a"
	echo "Velo" >> "$a"
	
	# Set settings
	CXR_COMPRESS_OUTPUT=true
	CXR_COMPRESSOR_EXEC="${CXR_BZIP2_EXEC}"
	CXR_COMPRESS_OUTPUT_PATTERN=
	
	# Compress also very small files
	CXR_COMPRESS_THRESHOLD_MB=0
	
	# Destroy output file hash
	common.hash.destroy $CXR_INSTANCE_HASH_OUTPUT_FILES $CXR_HASH_TYPE_INSTANCE
	
	# Add this file to the output file list
	common.hash.put $CXR_INSTANCE_HASH_OUTPUT_FILES $CXR_HASH_TYPE_INSTANCE  "${a}" "path_functions"
	
	# Change Mtime of $a
	touch $a
	
	# This is our time
	local rtc=$(date "+%s") 
	
	# MTime of file $a
	local ft=$(common.fs.getMtime $a)
	
	# create a 100 MB file called $b
	dd bs=100M if=/dev/zero of=$b count=1
	
	# Compression check
	gzip $c
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	main.log "Free memory: $(common.memory.getFreePercent) %"
	
	# We expect a difference of max 1 second (if we are at the boundary)
	differs_less_or_equal $rtc $ft 1 "common.fs.getMtime immediate, time difference ok"
	is $(common.fs.isAbsolutePath? /) true "common.fs.isAbsolutePath? /"
	is $(common.fs.FileSizeMb $a) 1 "common.fs.FileSizeMb of small file"
	is $(common.fs.FileSizeMb $b) 100 "common.fs.FileSizeMb of 100MB file"
	is $(common.fs.sameDevice? . .) true "common.fs.sameDevice? with twice the current path"
	is $(common.fs.sameDevice? /proc .) false "common.fs.sameDevice? with proc and current path"
	is "$(common.fs.getType /proc)" proc "common.fs.getType proc"
	is "$(common.fs.isLocal? /proc)" true "common.fs.isLocal? proc"
	is $(common.fs.isCompressed? "$c") true "common.fs.isCompressed? gzip"
	
	is "$(common.fs.isSubDirOf? /my_path / )" true "common.fs.isSubDirOf? root"
	is "$(common.fs.isSubDirOf? ./my_path . )" true "common.fs.isSubDirOf? relative path"
	is "$(common.fs.isSubDirOf? some/path some/otherpath )" false "common.fs.isSubDirOf? relative path"
	# That is a tough one
	is "$(common.fs.isSubDirOf? some/pathplus some/path )" false "common.fs.isSubDirOf? relative path"
	is "$(common.fs.isSubDirOf? some/path/and/more some/path )" true "common.fs.isSubDirOf? relative path"
	is "$(common.fs.isSubDirOf? some/path some/path/and/more )" false "common.fs.isSubDirOf? wrong order"
	# By definition, the same directories are subdirs of each other
	is "$(common.fs.isSubDirOf? some/path some/path )" true "common.fs.isSubDirOf? twice the same path"
	is "$(common.fs.isSubDirOf? / / )" true "common.fs.isSubDirOf? twice /"
	is "$(common.fs.isSubDirOf? . . )" true "common.fs.isSubDirOf? twice ."
	
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
	
	
}
