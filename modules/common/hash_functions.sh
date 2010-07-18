# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
#
# Version: $Id$ 
#
# Contains the Hash Functions, allows to map strings (keys) to other strings (values).
# We use files to store the hash values, their names are the URL-Encoded keys.
# This has the disadvantage of being slower than traditional, memory-base hashes (like perl
# has them), but it has two advantages: our hashes are persistent (they survive a run) and
# we can determine meta-information, such as the time of the last update of a single key.
# Each Hash is a directory with files representing the key value pairs, where the filename is the key
# and the content is the value.
# In filesystems like AFS, where we have a limit on the total lenght of all filenames in a directory,
# we try to catch such errors and delete older entries.
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################
# TODO: Improve performance
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=15

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|perl"

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the Hash functions for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2009), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: _common.hash.getDir
#
# Returns the hash_dir to use depending on the type.
#
# Parameters:
# $1 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL" 
################################################################################
function _common.hash.getDir()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully "needs a hash-type as input"
	fi

	local type="${1}"
	
	# Work out the directory
	case $type in
		$CXR_HASH_TYPE_INSTANCE) hash_dir="${CXR_INSTANCE_HASH_DIR}" ;;
		$CXR_HASH_TYPE_GLOBAL) hash_dir="${CXR_GLOBAL_HASH_DIR}" ;;
		$CXR_HASH_TYPE_UNIVERSAL) hash_dir="${CXR_UNIVERSAL_HASH_DIR}" ;;
		*) main.dieGracefully "Unknown Hashtype $type" ;;
	esac
	
	echo "$hash_dir"
	
}
################################################################################
# Function: common.hash.init
#
# Creates a hash with a given name. Hash names must be unique per run.
# Basically creates a directory into which we store files.
# Usage note: the name of a hash can convey important information. It can be dynamic,
# e.g. be dependent on the model name and version. This way, complex lookup functions
# depending on more than one parameter can be realized.
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL" 
################################################################################
function common.hash.init()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a hash and a valid hash-type as input"
	fi
	
	local hash="$1"
	local type="$2"
	local hash_dir
	
	# Work out the directory
	hash_dir="$(_common.hash.getDir "$type")"
	
	# Create the hash directory
	mkdir -p "${hash_dir}/${hash}"
	
	# Nobody else must modify this directory
	chmod 700 "${hash_dir}/${hash}"
}

################################################################################
# Function: common.hash.destroy
#
# Destroys a hash with a given name by deleting its directory.
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
################################################################################
function common.hash.destroy()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a hash and a valid hash-type as input"
	fi
	
	
	local hash="$1"
	local type="$2"
	local hash_dir
	
	# Work out the directory
	hash_dir="$(_common.hash.getDir "$type")"
	
	main.log "Deleting the Hash ${hash}"
	rm -rf "${hash_dir}/${hash}"
}

################################################################################
# Function: _common.hash.getFileName
#
# Generates a filename for a given hash and key. Internal function for use in hash functions.
# Tries to remedy missing directories (if <common.hash.init> was not called!)
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
################################################################################
function _common.hash.getFileName()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type and a key as input"
	fi
	
	local hash="$1"
	local type="$2"
	local key="$3"

	local hash_dir
	local fn
	
	# Work out the directory
	hash_dir="$(_common.hash.getDir "$type")"
	
	if [[ ! "$key" ]]
	then
		# Empty keys cannot be handled
		main.dieGracefully "Detected empty key!"
	fi
	
	# Generate the filename
	fn="${hash_dir}/${hash}/$(perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$key")"
	
	# Make the directory, just in case
	mkdir -p "$(dirname "${fn}")" >/dev/null 2>&1
	
	echo "${fn}"
}

################################################################################
# Function: common.hash.put
#
# Puts a value into a key of a given hash. First the key is urlencoded using perl
# then we use this value as a filename to store the value in.
# Also touches to directory of the hash (can be used for "last update time").
# We keep the data in a cache, just in case somebody wants to read it right away.
# We try to take care of the AFS issue mentioned in the file header.
# 
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
# $4 - value
################################################################################
function common.hash.put()
################################################################################
{
	if [[ $# -lt 4 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type, a key and a value as input. Got $@"
	fi
	
	local hash="$1"
	local type="$2"
	local key="$3"
	local value="$4"
	local hashdir

	local fn
	
	# Generate the filename
	fn="$(_common.hash.getFileName "$hash" "$type" "$key")"
	
	hashdir="$(dirname $fn)"
	
	if [[ ! -d "$hashdir" ]]
	then
		main.dieGracefully "Hash dir $hashdir not found!"
	else
		# Change the update time
		touch "$hashdir"
	fi
	
	# Turn off errexit
	set +e
	
	# Write the value
	echo "${value}" > "${fn}"
	
	if [[ $? -ne 0 ]]
	then
		main.log -w "Could not create hash entry ${fn}. It is possible that the TOC of the directory is full.\nI will delete the oldest file now."
		# Delete oldest entry
		ls -1t "$hashdir" | tail -1 | xargs rm
		
		# Try again
		echo "${value}" > "${fn}"
	fi
	
		#Turn strict checks back on unles we are testing
	if [[ ${CXR_TEST_IN_PROGRESS:-false} == false ]]
	then
		set -e
	fi
	
	# Fill cache
	CXR_CACHE_H_HASH="$hash"
	CXR_CACHE_H_TYPE="$type"
	CXR_CACHE_H_KEY="$key"
	CXR_CACHE_H_VALUE="$value"
}

################################################################################
# Function: common.hash.increment
#
# Increments the numeric value in a hash by a value (default 1).
# If the increment is negative, we do not go below 0.
# 
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
# [$4] - increment (default 1)
################################################################################
function common.hash.increment()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type, a key and a optional increment as input"
	fi
	
	local hash="$1"
	local type="$2"
	local key="$3"
	local increment="${4:1}"
	
	if [[ $(common.hash.has? "$hash" "$type" "$key") == true ]]
	then
		# The number we want to increment is there
		currentValue=$(common.hash.get "$hash" "$type")
	else
		# No value, start with 0
		currentValue=0
	fi
	
	# Test if we would go below 0
	if [[ $currentValue -eq 0 && $increment -lt 0 ]]
	then
		newValue=0
	else
		# Do the increment
		newValue=$(common.math.FloatOperation "$currentValue + $increment" 0 false )
	fi
	
	# Store
	common.hash.put Locks "$hash" "$type" "$key" "$newValue"
}

################################################################################
# Function: common.hash.decrement
#
# decrements the numeric value in a hash by a value (default 1)
# (Convenience wrapper of <common.hash.increment>).
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
# [$4] - decrement (default 1)
################################################################################
function common.hash.decrement()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type, a key and a optional decrement as input"
	fi
	
	local hash="$1"
	local type="$2"
	local key="$3"
	local decrement="${4:1}"
	
	# Invert the decrement
	local increment=$(common.math.FloatOperation "$decrement * -1" 0 false )
	common.hash.increment "$hash" "$type" "$key" "$increment"
}

################################################################################
# Function: common.hash.get
#
# Gets a certain value from a hash.
# Be careful, values might contain spaces and other nasties. Use like this:
# > value="$(common.hash.get "$hash" "$type" "$key")"
# Returns the empty string on error or if this key does not exist.
#
# Often, the same items are accessed several times in a row. For this reason,
# we cache the last value.
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.get()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type and a key as input"
	fi
	
	local hash="$1"
	local type="$2"
	local key="$3"
	local value
	
	local fn
	
	if [[ "$hash" == "${CXR_CACHE_H_HASH:-}" &&\
	      "$type" == "${CXR_CACHE_H_TYPE:-}" &&\
	      "$key" == "${CXR_CACHE_H_KEY:-}" ]]
	then
		# Match in Cache
		echo "$CXR_CACHE_H_VALUE"
	else
		# Lookup needed
		main.log -v "Getting $key out of hash $hash $type"
		
		# Generate the filename
		fn="$(_common.hash.getFileName "$hash" "$type" "$key")"
		
		if [[ -f "${fn}" ]]
		then
			# file there
			value="$(cat "${fn}")"
		else
			# no file - return the empty string
			value=""
		fi
		
		# Fill cache
		CXR_CACHE_H_HASH="$hash"
		CXR_CACHE_H_TYPE="$type"
		CXR_CACHE_H_KEY="$key"
		CXR_CACHE_H_VALUE="$value"
		
		# Return the value
		echo $value
	fi
}


################################################################################
# Function: common.hash.delete
#
# Deletes a certain value for a hash without returning its value.
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.delete()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type and a key as input"
	fi
	
	local hash="$1"
	local type="$2"
	local key="$3"
	
	local fn
	
	if [[ $(common.hash.has? "$hash" "$type" "$key") == true ]]
	then
		# Generate the filename
		fn="$(_common.hash.getFileName "$hash" "$type" "$key")"
		
		# remove the value
		rm -f "${fn}"
	else
		main.log -v  "Key "$key" not found in "$type" hash "$hash""
	fi
}


################################################################################
# Function: common.hash.remove
#
# Returns a value from a hash and then deletes it.
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.remove()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type and a key as input"
	fi
	
	local hash="$1"
	local type="$2"
	local key="$3"
	
	common.hash.get "$hash" "$type" "$key"
	common.hash.delete "$hash" "$type" "$key"
}

################################################################################
# Function: common.hash.getMtime
#
# Gets the modification time (Unix Epoch) for a given hash (the mtime of the hashdir)
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
################################################################################
function common.hash.getMtime()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a hash and a valid hash-type as input"
	fi
	
	local hash="$1"
	local type="$2"
	
	local basedir
	local hashdir
	
	# Generate the hashdir
	basedir="$(_common.hash.getDir "$type")"
	hashdir=${basedir}/${hash}

	# Get the mtime
	echo "$(common.fs.getMtime "$hashdir")"
}

################################################################################
# Function: common.hash.getValueMtime
#
# Gets the modification time (Unix Epoch) for a given value
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.getValueMtime()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type and a key as input"
	fi
	
	
	local hash="$1"
	local type="$2"
	local key="$3"
	
	local fn
	
	# Generate the filename
	fn="$(_common.hash.getFileName "$hash" "$type" "$key")"
	
	# Get the mtime
	echo "$(common.fs.getMtime "$fn")"
}

################################################################################
# Function: common.hash.has?
#
# Returns true if the given key is contained in the hash. Also fills cache on hit.
# and sets _has and _value (to avoid having to call <common.hash.get> again)
#
# If you use the "non-functional form" redirect output to /dev/null:
# > common.hash.has? $CXR_GLOBAL_HASH_DECOMPRESSED_FILES $CXR_HASH_TYPE_GLOBAL "${input_file}" > /dev/null
# > if [[ "$_has" == true ]]
# > then ...
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.has?()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type and a key as input"
	fi
	
	local hash="$1"
	local type="$2"
	local key="$3"
	
	local fn
	
	# Generate the filename
	fn="$(_common.hash.getFileName "$hash" "$type" "$key")"
	
	if [[ -f "${fn}" ]]
	then
		# Fill cache
		CXR_CACHE_H_HASH="$hash"
		CXR_CACHE_H_TYPE="$type"
		CXR_CACHE_H_KEY="$key"
		CXR_CACHE_H_VALUE="$(cat "${fn}")"
		_value="$CXR_CACHE_H_VALUE"
		_has=true
		echo true
	else
		_has=false
		echo false
	fi
}

################################################################################
# Function: common.hash.isNew?
#
# Returns true if the given key is contained in the hash and its update time is 
# newer than this runs start time (meaning that we do not need to update such a hash
# if it is used as a cache)
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.isNew?()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type and a key as input"
	fi
	
	
	local hash="$1"
	local type="$2"
	local key="$3"
	
	local res
	
	# Is it in the hash?
	if [[ $(common.hash.has? "$hash" "$type" "$key") == true ]]
	then
		# Exists, test age. CXR_EPOCH is the Epoch we started this run in
		# if the hash's epoch is smaller, it is older
		if [[ "$(common.hash.getValueMtime "$hash" "$type" "$key")" -lt "$CXR_EPOCH" ]]
		then
			res=false
		else
			res=true
		fi
	else
		# Does not exist
		res=false
	fi
	
	echo $res
}

################################################################################
# Function: common.hash.getKeys
#
# Returns a list of keys of the given hash as a quoted CXR_DELIMITER separated list.
# Do not assume any particular order, it depends on the order ls imposes on the
# encoded keys. See <common.hash.toFile> for an example on how to use this safely.
# If the Hash does not exist, returns the empty string.
# 
# Recommended use:
# > oIFS="$IFS"
# > local keyString="$(common.hash.getKeys $hash $CXR_HASH_TYPE_GLOBAL)"
# > IFS="$CXR_DELIMITER"
# > # Turn string into array (we cannot call <common.hash.getKeys> directly here!)
# > local arrKeys=( $keyString )
# > # Reset Internal Field separator
# > IFS="$oIFS"
# > 
# > # looping through keys (safest approach)
# > for iKey in $( seq 0 $(( ${#arrKeys[@]} - 1)) )
# > do
# > 	key=${arrKeys[$iKey]}
# > 	# Whatever more
# > done
# 
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
################################################################################
function common.hash.getKeys()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a hash and a valid hash-type as input"
	fi
	
	
	local hash="$1"
	local type="$2"
	
	local found=false
	local hash_dir
	local fn
	local key
	local list=""
	
	# Work out the directory
	hash_dir="$(_common.hash.getDir "$type")"
	
	main.log -v "Getting keys for $hash $type out of ${hash_dir}/${hash}..."
	
	if [[ -d ${hash_dir}/${hash} ]]
	then
		# Hash exists, get all files within
		for fn in $(find ${hash_dir}/${hash} -noleaf -type f -maxdepth 1 2>/dev/null)
		do
			found=true
			key="$(perl -MURI::Escape -e 'print uri_unescape($ARGV[0]);' "$(basename $fn)")"
			list="${list}${key}$CXR_DELIMITER"
		done
		
		# Remove last delimiter
		list="${list/%${CXR_DELIMITER}/}"
	
	else
		main.log -w "Hash ${hash} does not exist."
		list=""
	fi
	
	if [[ $found == false ]]
	then
		main.log -w "Hash ${hash_dir}/${hash} is empty..."
	fi
	
	echo $list
}

################################################################################
# Function: common.hash.toFile
#
# Serialises a Hash to a file (CXR_DELIMITER separated), which can later be read in via
# <common.hash.fromFile>.
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - file to write to
################################################################################
function common.hash.toFile()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type and a filename to write to as input"
	fi
	
	local hash="$1"
	local type="$2"
	local file="$3"
	
	local key
	local value
	local iKey
	
	if [[ -s "$file" ]]
	then
		main.log -w "Output file $file already exists. Hash data will be added."
	fi
	
	oIFS="$IFS"
	local keyString="$(common.hash.getKeys "$hash" "$type")"
	IFS="$CXR_DELIMITER"
	# Turn string into array (we cannot call <common.hash.getKeys> directly here!)
	local arrKeys=( $keyString )
	# Reset Internal Field separator
	IFS="$oIFS"
	
	# looping through keys (safest approach)
	for iKey in $( seq 0 $(( ${#arrKeys[@]} - 1)) )
	do
		key="${arrKeys[$iKey]}"
		value="$(common.hash.get "$hash" "$type" "$key")"
		echo "${key}${CXR_DELIMITER}${value}" >> "$file"
	done

	main.log "Data of ${hash} written to ${file}."
}

################################################################################
# Function: common.hash.fromFile
#
# Deserialises a Hash from a file (Pipe separated), which usually is prepared via
# <common.hash.ToFile>.
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - file to read from
# [$4] - mode:
#     'APPEND' - just add non-existing keys (default)
#     'UPDATE' - updates existing keys and adds non-existing ones
################################################################################
function common.hash.fromFile()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type and a filename read from, and optionally a mode as input"
	fi
	
	local hash="$1"
	local type="$2"
	local file="$3"
	local mode="${4:-APPEND}"
	
	local oIFS
	local key
	local value
	
	if [[ ! -f "$file" ]]
	then
		main.log -e "Input file $file does not exist, cannot load hash"
		return 1
	fi
	
	# Save IFS
	oIFS="$IFS"
	IFS="$CXR_DELIMITER"
	
	case $(common.string.toUpper "$mode") in
	
		APPEND)		main.log -v "Data of ${file} will be added to ${hash}."
		
					while read key value
					do
						if [[ $(common.hash.has? "$hash" "$type" "$key") == false ]]
						then
							common.hash.put "$hash" "$type" "$key" "$value"
						fi
					done < "$file"
					
					;;
					
		UPDATE)		main.log -v "Data of ${file} will be added to ${hash}." 
					while read key value
					do
						common.hash.put "$hash" "$type" "$key" "$value"
					done < "$file"
					;;
					
		*) 			main.log -e "Programming error: unknown mode ${mode} to add data to a hash"
					;;
		
	esac
	
	IFS="$oIFS"
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
	local iKey

	########################################
	# Setup tests if needed
	########################################
	# Instance hash
	common.hash.init test_instance $CXR_HASH_TYPE_INSTANCE
	common.hash.put test_instance $CXR_HASH_TYPE_INSTANCE /hallo/gugs SomeOtherValue
	common.hash.put test_instance $CXR_HASH_TYPE_INSTANCE /hallo/velo SomeOtherValue
	
	# Hash of arrays
	common.hash.init test_array $CXR_HASH_TYPE_INSTANCE
	common.hash.put test_array $CXR_HASH_TYPE_INSTANCE array1 "1 2 3 4 5"
	
	# Read again
	a=( $(common.hash.get test_array $CXR_HASH_TYPE_INSTANCE array1) )
	
	# Glabal Hash with strange keys
	common.hash.init test_global $CXR_HASH_TYPE_GLOBAL
	common.hash.put test_global $CXR_HASH_TYPE_GLOBAL "This key has spaces" "a value"
	common.hash.put test_global $CXR_HASH_TYPE_GLOBAL "This key also has spaces" "another value"
	
	# Universal Hash
	common.hash.init test_universal $CXR_HASH_TYPE_UNIVERSAL
	common.hash.put test_universal $CXR_HASH_TYPE_UNIVERSAL /hallo/gugs SomeOtherValue
	common.hash.put test_universal  $CXR_HASH_TYPE_UNIVERSAL /hallo/velo SomeOtherValue

	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is "$(common.hash.get test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo")" SomeOtherValue "common.hash.get test_instance with path as key"
	is "$(common.hash.has? test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo")" true "common.hash.has? test_instance with path as key"
	is "$(common.hash.getKeys test_instance $CXR_HASH_TYPE_INSTANCE)" "/hallo/gugs${CXR_DELIMITER}/hallo/velo" "common.hash.getKeys test_instance with path as key"
	is ${#a[@]} 5 "Hash of arrays"
	
	# testing the faster way to call common.hash.has? (has and get in one call)
	common.hash.has? test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo" > /dev/null
	is $_has true "common.hash.has? non-functional approach I"
	is $_value SomeOtherValue "common.hash.has? non-functional approach II"
	
	oIFS="$IFS"
	local keyString="$(common.hash.getKeys test_instance $CXR_HASH_TYPE_INSTANCE)"
	IFS="$CXR_DELIMITER"
	# Turn string into array (we cannot call <common.hash.getKeys> directly here!)
	local arrKeys=( $keyString )
	# Reset Internal Field separator
	IFS="$oIFS"
	
	# looping through keys (safest approach)
	for iKey in $( seq 0 $(( ${#arrKeys[@]} - 1)) )
	do
		key="${arrKeys[$iKey]}"
		is "$(common.hash.get test_instance $CXR_HASH_TYPE_INSTANCE "$key")" SomeOtherValue "Going trough keys in an interator"
	done
	
	# Lets retrieve those with spaces
	is "$(common.hash.get test_global $CXR_HASH_TYPE_GLOBAL "This key has spaces")" "a value" "common.hash.get test_instance - key with spaces"
	
	common.hash.delete test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo"
	is "$(common.hash.has? test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo")" false "common.hash.delete test_instance with path as key"

	is "$(common.hash.get test_universal $CXR_HASH_TYPE_UNIVERSAL "/hallo/velo")" SomeOtherValue "common.hash.get test_universal with path as key"
	is "$(common.hash.has? test_universal $CXR_HASH_TYPE_UNIVERSAL "/hallo/velo")" true "common.hash.has? test_universal with path as key"
	
	common.hash.delete test_universal $CXR_HASH_TYPE_UNIVERSAL "/hallo/velo" 
	is "$(common.hash.has? test_universal $CXR_HASH_TYPE_UNIVERSAL "/hallo/velo")" false "common.hash.delete test_universal with path as key"

	# Some serialisation/deserialisation
	hashfile=$(common.runner.createTempFile hash)
	
	# Save to file
	common.hash.toFile test_global $CXR_HASH_TYPE_GLOBAL $hashfile
	
	#destroy it
	common.hash.destroy test_global $CXR_HASH_TYPE_GLOBAL
	
	# load from file
	common.hash.fromFile test_global $CXR_HASH_TYPE_GLOBAL $hashfile
	
	is "$(common.hash.get test_global $CXR_HASH_TYPE_GLOBAL "This key has spaces")" "a value" "testing common.hash.toFile and .fromFile"
	is "$(common.hash.get test_global $CXR_HASH_TYPE_GLOBAL "This key also has spaces")" "another value" "testing common.hash.toFile and .fromFile"
	
	
	
	########################################
	# teardown tests if needed
	########################################
	common.hash.destroy test_instance $CXR_HASH_TYPE_INSTANCE
	common.hash.destroy test_array $CXR_HASH_TYPE_INSTANCE
	common.hash.destroy test_global $CXR_HASH_TYPE_GLOBAL
	common.hash.destroy test_universal $CXR_HASH_TYPE_UNIVERSAL

}
