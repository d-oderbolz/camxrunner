# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
#
# Version: $Id$ 
#
# Title: Functions to manage key-value pairs in a persistent and fast manner.
# This is a re-implementation of the file-based functions using sqlite <http://www.sqlite.org>.
# Compared to the old approach, this has a number of advantages:
# - there is no need to limit the number of keys per directory (was an issue on AFS)
# - less overhead
# - we can benefit from disk-chaching
# -> its faster (a simple test showed sqlite is about four to five times faster than my plain old filedb)
# - storage of additional metadata is easy
# - we can easily get lists of (key,value) tuples out (even faster!)
# - only one file per hash level needed (even better for the file cache)
# - storage of more than one value per key (versioning) is possible
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################
# TODO: Unite getKeys, getKeysAndValues and getValues. Create searchKey and searchValue (return list of 2-tuples)


# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=12

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|sqlite3"

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the Hash functions (using <db_functions.sh>) for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: _common.hash.getDbFile
#
# Returns the db_file to use depending on the level.
#
# Parameters:
# $1 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL" 
################################################################################
function _common.hash.getDbFile()
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "needs a hash-level as input"
	fi

	local level
	level="${1}"
	
	if [[ "${level}" ]]
	then
		# Work out the directory
		case $level in
			$CXR_LEVEL_INSTANCE) echo "${CXR_INSTANCE_DIR}/hashes.${CXR_DB_SUFFIX}" ;;
			$CXR_LEVEL_GLOBAL) echo "${CXR_GLOBAL_DIR}/hashes.${CXR_DB_SUFFIX}" ;;
			$CXR_LEVEL_UNIVERSAL) echo "${CXR_UNIVERSAL_DIR}/hashes.${CXR_DB_SUFFIX}" ;;
			*) main.dieGracefully "Unknown DB level $level" ;;
		esac
	else
		main.dieGracefully "Hash level is empty!" 
	fi
}

################################################################################
# Function: common.hash.init
#
# Creates a hash with a given name, if it already exists, nothing happens.
# We distinguish three visibility levels: 
# $CXR_LEVEL_INSTANCE - only visible for this instance
# $CXR_LEVEL_GLOBAL - visible for all instances of this run
# $CXR_LEVEL_UNIVERSAL - visible for all instances of all runs
# Usage note: since we can store metadata in a hash, it is not recommended to encode
# information in the name of the hash (like it was done before).
#
# Parameters:
# $1 - name of the hash (must be usable as a filename)
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL" 
################################################################################
function common.hash.init()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a hash name and a valid hash-level as input"
	fi
	
	local hash
	local level
	
	hash="$1"
	level="$2"

	local db_file
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$level")"
	
	main.log -v "Creating DB $db_file"
	
	# Create table, no matter what
	common.db.change "$db_file" "$level" - <<-EOT 
	
	CREATE TABLE IF NOT EXISTS hash (hash, key, value, epoch_c);
	
	-- Create two indexes
	CREATE INDEX IF NOT EXISTS hash_idx ON hash(hash);
	CREATE INDEX IF NOT EXISTS key_idx ON hash(key);
	
	EOT

	# Nobody else must modify this file
	chmod 600 "$db_file"
}

################################################################################
# Function: common.hash.destroy
#
# Destroys a hash with a given name by deleting the relevant entries in the db.
#
# Parameters:
# $1 - name of the hash
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
################################################################################
function common.hash.destroy()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a hash and a valid hash-level as input"
	fi
	
	local hash
	local level
	
	hash="$1"
	level="$2"

	local db_file
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$level")"
	
	main.log -v "Deleting all entries of Hash ${hash}.."
	
	common.db.change "$db_file" "$level" "DELETE FROM hash WHERE hash='$hash'"
}

################################################################################
# Function: common.hash.put
#
# Puts a value into a key of a given hash. Alos touches the DB file (needed to
# determine the last access).
# By default, only one entry of the same key is allowed, but if allow_multiple is true,
# we allow to store history.
#
# Parameters:
# $1 - name of the hash
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
# $3 - key
# $4 - value
# [$5] - boolean allow_multiple (default false), if true, allows more than 1 value per key 
################################################################################
function common.hash.put()
################################################################################
{
	if [[ $# -lt 4  || $# -gt 5 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-level, a key, a value and an optiotal boolean allow_multiple as input. Got $*"
	fi
	
	local hash
	local level
	local key
	local value
	local allow_multiple

	
	hash="$1"
	level="$2"
	key="$3"
	value="$4"
	allow_multiple="${5:-false}"
	
	local db_file
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$level")"
	
	if [[ ! -f "$db_file" ]]
	then
		main.log -w "DB $db_file not found!"
		common.hash.init "$hash" "$level"
	else
		# Change the update time
		touch "$db_file"
	fi
	
	# Delete if not allow_multiple
	if [[ $allow_multiple == false ]]
	then
		common.db.change "$db_file" "$level" "DELETE FROM hash WHERE hash='$hash' and key='$key'" || :
	fi
	
	# Write value to DB
	common.db.change "$db_file" "$level" "INSERT INTO hash (hash, key, value , epoch_c) VALUES ('$hash','$key','$value',$(date "+%s"))" || :
	
	# Fill cache
	CXR_CACHE_H_HASH="$hash"
	CXR_CACHE_H_LEVEL="$level"
	CXR_CACHE_H_KEY="$key"
	CXR_CACHE_H_VALUE="$value"
}

################################################################################
# Function: common.hash.get
#
# Gets a certain single value from a hash.
# Be careful, values might contain spaces and other nasties. Use like this:
# > value="$(common.hash.get "$hash" "$level" "$key")"
# Returns the empty string on error or if this key does not exist.
#
# Often, the same items are accessed several times in a row. For this reason,
# we cache the last value.
#
# Since a DB may contain more than one row with this key (by design), we return the 
# newest (highest epoch_c)
#
# Parameters:
# $1 - name of the hash
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.get()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-level and a key as input"
	fi
	
	local hash
	local level
	local key
	
	hash="$1"
	level="$2"
	key="$3"
	
	local value
	
	if [[ "$hash" == "${CXR_CACHE_H_HASH:-}" &&\
	      "$level" == "${CXR_CACHE_H_LEVEL:-}" &&\
	      "$key" == "${CXR_CACHE_H_KEY:-}" ]]
	then
		# Match in Cache
		echo "$CXR_CACHE_H_VALUE"
	else
		# Lookup needed
		main.log -v "Getting $key out of hash $hash $level"
		
		local db_file
	
		# Work out the filename
		db_file="$(_common.hash.getDbFile "$level")"
		
		# Get value
		if [[ ! -f "$db_file" ]]
		then
			main.log -w "DB $db_file not found!"
			echo ""
		else
			# Read the contents
			value="$(common.db.getResultSet "$db_file" "$level" "SELECT value FROM hash WHERE hash='$hash' AND key='$key' ORDER BY epoch_c DESC LIMIT 1")"
			
			# Fill cache
			CXR_CACHE_H_HASH="$hash"
			CXR_CACHE_H_LEVEL="$level"
			CXR_CACHE_H_KEY="$key"
			CXR_CACHE_H_VALUE="$value"
			
			# Return the value
			echo "$value"
		fi
	fi
}

################################################################################
# Function: common.hash.getAll
#
# Gets all versions of a single value out of a hash (oldest comes first)
# Be careful, values might contain spaces and other nasties. Use like this:
#
# > oIFS="$IFS"
# > # Set IFS to newline
# > IFS='
# > '
# > for value in $(common.hash.getAll "$hash" "$level" "$key")
# > do
# > echo "$value"
# > done
# > IFS="$oIFS"
#
# Returns the empty string on error or if this key does not exist.
#
# Parameters:
# $1 - name of the hash
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.getAll()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-level and a key as input"
	fi
	
	local hash
	local level
	local key
	
	hash="$1"
	level="$2"
	key="$3"

	local value
	
	main.log -v "Getting al values for $key out of hash $hash $level"
	
	local db_file

	# Work out the filename
	db_file="$(_common.hash.getDbFile "$level")"
	
	# Get value
	if [[ ! -f "$db_file" ]]
	then
		main.log -w "DB $db_file not found!"
		echo ""
	else
		# Dummy for the parser
		:
		# get the contents
		common.db.getResultSet "$db_file" "$level" "SELECT value FROM hash WHERE hash='$hash' AND key='$key' ORDER BY epoch_c ASC"
	fi
}

################################################################################
# Function: common.hash.delete
#
# Deletes a certain key from a hash without returning its value.
#
# Parameters:
# $1 - name of the hash
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
# $3 - key
# $4 - use_like - if true (default false), uses LIKE for the key 
################################################################################
function common.hash.delete()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-level and a key - plus optionally use_like as input"
	fi
	
	local hash
	local level
	local key
	local db_file
	local use_like
	
	hash="$1"
	level="$2"
	key="$3"
	use_like="${4:-false}"
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$level")"
	
	# Just delete
	if [[ ! -f "$db_file" ]]
	then
		main.log -w "DB $db_file not found!"
	else
		# delete entry
		
		if [[ "$use_like" == true ]]
		then
			# Wildcards accepted
			common.db.change "$db_file" "$level" "DELETE FROM hash WHERE hash='$hash' AND key LIKE '$key'"
		else
			# Exact matches
			common.db.change "$db_file" "$level" "DELETE FROM hash WHERE hash='$hash' AND key='$key'"
		fi # use_like?
	fi
}


################################################################################
# Function: common.hash.remove
#
# Returns a value from a hash and then deletes it.
#
# Parameters:
# $1 - name of the hash
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.remove()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-level and a key as input"
	fi
	
	local hash
	local level
	local key
	
	hash="$1"
	level="$2"
	key="$3"
	
	common.hash.get "$hash" "$level" "$key"
	common.hash.delete "$hash" "$level" "$key"
}

################################################################################
# Function: common.hash.getMtime
#
# Gets the modification time (Unix Epoch) for a given hash (the mtime of the db_file)
#
# Parameters:
# $1 - name of the hash
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
################################################################################
function common.hash.getMtime()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a hash and a valid hash-level as input"
	fi
	
	local hash
	local level
	local db_file
	
	hash="$1"
	level="$2"
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$level")"
	
	# Get the mtime
	common.fs.getMtime "$db_file"
}

################################################################################
# Function: common.hash.getValueMtime
#
# Gets the (latest) modification time (Unix Epoch) for a given value.
# If the value is not found, the empty string is returned
#
# Parameters:
# $1 - name of the hash
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.getValueMtime()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-level and a key as input"
	fi
	
	local hash
	local level
	local key
	local db_file
	
	hash="$1"
	level="$2"
	key="$3"
	local mtime
	
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$level")"
	
	# Get the value
	mtime=$(common.db.getResultSet "$db_file" "$level" "SELECT epoch_c FROM hash WHERE hash='$hash' AND key='$key' ORDER BY epoch_c DESC LIMIT 1")
	
	echo $mtime
}

################################################################################
# Function: common.hash.has?
#
# Returns true if the given key is contained in the hash. Also fills cache on hit.
# and sets _has and _value (to avoid having to call <common.hash.get> again)
#
# If you use the "non-functional form" redirect output to /dev/null:
# > common.hash.has? $CXR_GLOBAL_HASH_DECOMPRESSED_FILES $CXR_LEVEL_GLOBAL "${input_file}" > /dev/null
# > if [[ "$_has" == true ]]
# > then ...
#
# Parameters:
# $1 - name of the hash
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.has?()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-level and a key as input"
	fi
	
	local hash
	local level
	local key
	local db_file
	
	hash="$1"
	level="$2"
	key="$3"

	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$level")"
	
	if [[ ! -f "$db_file" ]]
	then
		main.log -w "DB $db_file not found!"
		_has=false
	else
		# get the rowcount
		rowcount=$(common.db.getResultSet "$db_file" "$level" "SELECT COUNT(*) FROM hash WHERE hash='$hash' AND key='$key' ORDER BY epoch_c DESC LIMIT 1")

		if [[ "$rowcount" -gt 0 ]]
		then
			# Get the value
			value=$(common.db.getResultSet "$db_file" "$level" "SELECT value FROM hash WHERE hash='$hash' AND key='$key' ORDER BY epoch_c DESC LIMIT 1")
			
			# Fill cache
			CXR_CACHE_H_HASH="$hash"
			CXR_CACHE_H_LEVEL="$level"
			CXR_CACHE_H_KEY="$key"
			CXR_CACHE_H_VALUE="$value"
			_has=true
			_value=$value
		else
			_has=false
		fi
	fi
	
	echo $_has
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
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.isNew?()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-level and a key as input"
	fi
	
	
	local hash
	local level
	local key
	
	hash="$1"
	level="$2"
	key="$3"
	
	local res
	
	# Is it in the hash?
	if [[ $(common.hash.has? "$hash" "$level" "$key") == true ]]
	then
		# Exists, test age. CXR_START_EPOCH is the Epoch we started this run in
		# if the hash's epoch is smaller, it is older
		if [[ "$(common.hash.getValueMtime "$hash" "$level" "$key")" -lt "$CXR_START_EPOCH" ]]
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
# Returns a unique list of keys of the given hash as newline-separated list.
# Do not assume any particular order, it depends on the order the hash imposes on the
# data. 
#
# Use this function only if you are not interested in the actual values in the hash
# (if you merely need to know something exists). 
# The function <common.hash.getKeysAndValues> also provides you with the data in one call.
# 
# Recommended use:
# > # Set IFS to newline only
# > oIFS="$IFS"
# > IFS='
# >'
# > for key in $(common.hash.getKeys $hash $CXR_LEVEL_GLOBAL)
# > do
# > 	# Reset IFS
# > 	IFS="$oIFS"
# > 	# Whatever
# > done
# 
# Parameters:
# $1 - name of the hash
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
################################################################################
function common.hash.getKeys()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a hash and a valid hash-level as input"
	fi
	
	local hash
	local level

	local db_file
	
	hash="$1"
	level="$2"

	# Work out the filename
	db_file="$(_common.hash.getDbFile "$level")"
	
	main.log -v "Getting keys for $hash $level out of ${db_file}..."
	
	# get data
	common.db.getResultSet "$db_file" "$level" "SELECT DISTINCT key FROM hash WHERE hash='$hash';"
}

################################################################################
# Function: common.hash.getKeysAndValues
#
# Returns rows of unique key value pairs as in 
# key1|value1
# key2|value2
# Do not assume any particular order, it depends on the order the hash imposes on the
# data. We ensure that for each key we return only one value (the most recent one).
# 
# Recommended use:
#
# > # looping through pairs
# > # Set IFS to newline only
# > oIFS="$IFS"
# > IFS='
# >'
# > for pair in $(common.hash.getKeysAndValues $hash $level)
# > do
# > 	# Reset IFS
# > 	IFS="$oIFS"
# >
# > 	oIFS="$IFS"
# >		IFS="$CXR_DELIMITER"
# > 	set $pair
# > 	key="$1"
# > 	value="$2"
# > 	IFS="$oIFS"
# > done
# 
# Parameters:
# $1 - name of the hash
# $2 - level of hash, either "$CXR_LEVEL_INSTANCE" , "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
################################################################################
function common.hash.getKeysAndValues()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a hash and a valid hash-level as input"
	fi
	
	local hash
	local level
	
	local found
	local key
	local list
	
	local db_file
	
	hash="$1"
	level="$2"
	
	found=false
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$level")"
	
	main.log -v "Getting keys for $hash $level out of ${db_file}..."
	common.db.getResultSet "$db_file" "$level" "SELECT key, value FROM hash WHERE hash='$hash' GROUP BY key, value HAVING MAX(epoch_c)" "$CXR_DELIMITER"

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
	local keyString
	local arrKeys
	
	########################################
	# Setup tests if needed
	########################################
	# Instance hash
	common.hash.init test_instance $CXR_LEVEL_INSTANCE
	common.hash.put test_instance $CXR_LEVEL_INSTANCE "/hallo/gugs" SomeOtherValue
	common.hash.put test_instance $CXR_LEVEL_INSTANCE "a key with spaces" SomeOtherValue
	
	# DB of arrays
	common.hash.init test_array $CXR_LEVEL_INSTANCE
	common.hash.put test_array $CXR_LEVEL_INSTANCE array1 "1 2 3 4 5"
	
	# Read again
	a=( $(common.hash.get test_array $CXR_LEVEL_INSTANCE array1) )
	
	# Glabal DB with strange keys
	common.hash.init test_global $CXR_LEVEL_GLOBAL
	common.hash.put test_global $CXR_LEVEL_GLOBAL "This key has spaces" "a value"
	common.hash.put test_global $CXR_LEVEL_GLOBAL "This key also has spaces" "another value"
	
	# Universal DB
	common.hash.init test_universal $CXR_LEVEL_UNIVERSAL
	common.hash.put test_universal $CXR_LEVEL_UNIVERSAL /hallo/gugs SomeOtherValue
	common.hash.put test_universal  $CXR_LEVEL_UNIVERSAL "a key with spaces" SomeOtherValue

	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is "$(common.hash.get test_instance $CXR_LEVEL_INSTANCE "a key with spaces")" SomeOtherValue "common.hash.get test_instance with path as key"
	is "$(common.hash.has? test_instance $CXR_LEVEL_INSTANCE "a key with spaces")" true "common.hash.has? test_instance with path as key"
	is ${#a[@]} 5 "DB of arrays"
	
	# testing the faster way to call common.hash.has? (has and get in one call)
	common.hash.has? test_instance $CXR_LEVEL_INSTANCE "a key with spaces" > /dev/null
	is $_has true "common.hash.has? non-functional approach I"
	is $_value SomeOtherValue "common.hash.has? non-functional approach II"
	
	oIFS="$IFS"
	IFS='
'

	# looping through keys (safest approach)
	for key in $(common.hash.getKeys test_instance $CXR_LEVEL_INSTANCE)
	do
		IFS="$oIFS"
		
		is "$(common.hash.get test_instance $CXR_LEVEL_INSTANCE "$key")" SomeOtherValue "Going through keys in an interator"
	done
	
	# Lets retrieve those with spaces
	is "$(common.hash.get test_global $CXR_LEVEL_GLOBAL "This key has spaces")" "a value" "common.hash.get test_instance - key with spaces"
	
	common.hash.delete test_instance $CXR_LEVEL_INSTANCE "a key with spaces"
	is "$(common.hash.has? test_instance $CXR_LEVEL_INSTANCE "a key with spaces")" false "common.hash.delete test_instance with path as key"

	is "$(common.hash.get test_universal $CXR_LEVEL_UNIVERSAL "a key with spaces")" SomeOtherValue "common.hash.get test_universal with path as key"
	is "$(common.hash.has? test_universal $CXR_LEVEL_UNIVERSAL "a key with spaces")" true "common.hash.has? test_universal with path as key"
	
	common.hash.delete test_universal $CXR_LEVEL_UNIVERSAL "a key with spaces" 
	is "$(common.hash.has? test_universal $CXR_LEVEL_UNIVERSAL "a key with spaces")" false "common.hash.delete test_universal with path as key"
	
	########################################
	# teardown tests if needed
	########################################
	common.hash.destroy test_instance $CXR_LEVEL_INSTANCE
	common.hash.destroy test_array $CXR_LEVEL_INSTANCE
	common.hash.destroy test_global $CXR_LEVEL_GLOBAL
	common.hash.destroy test_universal $CXR_LEVEL_UNIVERSAL
}
