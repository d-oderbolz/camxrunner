# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
#
# Version: $Id$ 
#
# This is a re-implementation of the file-based functions using sqlite <http://www.sqlite.org>.
# Compared to the old approach, this has a number of advantages:
# - there is no need to limit the number of keys per directory (was an issue on AFS)
# - less overhead
# - we can benefit from disk-chaching
# -> its faster (a simple test showed sqlite is about four to five times faster than my plain old filedb)
# - storage of additional metadata is easy
# - we can easily get lists of (key,value) tuples out (even faster!)
# - only one file per hash type needed (even better for the file cache)
# - storage of more than one value per key (versioning) is possible
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################
# TODO: Unite getKeys, getKeysAndValues and getValues


# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=13

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|sqlite3"

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the Hash functions (using sqlite) for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2009), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: _common.hash.getDbFile
#
# Returns the db_file to use depending on the type.
#
# Parameters:
# $1 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL" 
################################################################################
function _common.hash.getDbFile()
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "needs a hash-type as input"
	fi

	local type
	type="${1}"
	
	if [[ "${type}" ]]
	then
		# Work out the directory
		case $type in
			$CXR_HASH_TYPE_INSTANCE) echo "${CXR_INSTANCE_DIR}/hashes.${CXR_DB_SUFFIX}" ;;
			$CXR_HASH_TYPE_GLOBAL) echo "${CXR_GLOBAL_DIR}/hashes.${CXR_DB_SUFFIX}" ;;
			$CXR_HASH_TYPE_UNIVERSAL) echo "${CXR_UNIVERSAL_DIR}/hashes.${CXR_DB_SUFFIX}" ;;
			*) main.dieGracefully "Unknown DB type $type" ;;
		esac
	else
		main.dieGracefully "Hash type is empty!" 
	fi
}

################################################################################
# Function: common.hash.init
#
# Creates a hash with a given name, if it already exists, nothing happens.
# We distinguish three visibility levels: 
# $CXR_HASH_TYPE_INSTANCE - only visible for this instance
# $CXR_HASH_TYPE_GLOBAL - visible for all instances of this run
# $CXR_HASH_TYPE_UNIVERSAL - visible for all instances of all runs
# Usage note: since we can store metadata in a hash, it is not recommended to encode
# information in the name of the hash (like it was done before).
#
# Parameters:
# $1 - name of the hash (must be usable as a filename)
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL" 
################################################################################
function common.hash.init()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a hash name and a valid hash-type as input"
	fi
	
	local hash
	local type
	
	hash="$1"
	type="$2"

	local db_file
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$type")"
	
	main.log -v "Creating DB $db_file"
	
	# Create table, no matter what
	${CXR_SQLITE_EXEC} "$db_file" "CREATE TABLE IF NOT EXISTS hash (hash, key, value , model, version, epoch_c)"

	# Nobody else must modify this file
	chmod 600 "$db_file"
}

################################################################################
# Function: common.hash.destroy
#
# Destroys a hash with a given name by deleting its datafile.
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
	
	local hash
	local type
	
	hash="$1"
	type="$2"

	local db_file
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$type")"
	
	main.log -v "Deleting the Hash ${hash}"
	rm -f "${db_file}"
}

################################################################################
# Function: common.hash.put
#
# Puts a value into a key of a given hash. Alos touches the DB file (needed to
# determine the last access)
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
# $4 - value
# [$5] - restrict_model_version , boolean, if true (default false), we write tag this entry with the model and version
################################################################################
function common.hash.put()
################################################################################
{
	if [[ $# -lt 4 || $# -gt 5 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type, a key, a value and an optional boolean (restrict_model_version) as input. Got $@"
	fi
	
	local hash
	local type
	local key
	local value
	local restrict_model_version
	local model
	local version
	
	hash="$1"
	type="$2"
	key="$3"
	value="$4"
	restrict_model_version="${5:-false}"
	
	if [[ "$restrict_model_version" == true ]]
	then
		model=$CXR_MODEL
		version=$CXR_MODEL_VERSION
	else
		model=any
		version=any
	fi
	
	local db_file
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$type")"
	
	if [[ ! -f "$db_file" ]]
	then
		main.log -w "DB $db_file not found!"
		common.hash.init "$hash" "$type"
	else
		# Change the update time
		touch "$db_file"
	fi
	
	# Write value to DB
	${CXR_SQLITE_EXEC} "$db_file" "INSERT INTO hash (hash, key, value , model, version, epoch_c) VALUES ('$hash','$key','$value','$model','$version',$(date "+%s"))" || :
	
	# Fill cache
	CXR_CACHE_H_HASH="$hash"
	CXR_CACHE_H_TYPE="$type"
	CXR_CACHE_H_KEY="$key"
	CXR_CACHE_H_VALUE="$value"
}

################################################################################
# Function: common.hash.get
#
# Gets a certain single value from a hash.
# Be careful, values might contain spaces and other nasties. Use like this:
# > value="$(common.hash.get "$hash" "$type" "$key")"
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
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.hash.get()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type, a key and an optional boolean (restrict_model_version) as input"
	fi
	
	local hash
	local type
	local key
	local restrict_model_version
	local model
	local version
	
	hash="$1"
	type="$2"
	key="$3"
	restrict_model_version="${4:-false}"
	
	if [[ "$restrict_model_version" == true ]]
	then
		model=$CXR_MODEL
		version=$CXR_MODEL_VERSION
	else
		model=any
		version=any
	fi
	
	local value
	
	if [[ "$hash" == "${CXR_CACHE_H_HASH:-}" &&\
	      "$type" == "${CXR_CACHE_H_TYPE:-}" &&\
	      "$key" == "${CXR_CACHE_H_KEY:-}" ]]
	then
		# Match in Cache
		echo "$CXR_CACHE_H_VALUE"
	else
		# Lookup needed
		main.log -v "Getting $key out of hash $hash $type"
		
		local db_file
	
		# Work out the filename
		db_file="$(_common.hash.getDbFile "$type")"
		
		# Get value
		if [[ ! -f "$db_file" ]]
		then
			main.log -w "DB $db_file not found!"
			echo ""
		else
			# Read the contents
			value=$(${CXR_SQLITE_EXEC} "$db_file" "SELECT value FROM hash WHERE hash='$hash' AND key='$key' AND model='$model' AND version='$version' ORDER BY epoch_c DESC LIMIT 1")
			
			# Fill cache
			CXR_CACHE_H_HASH="$hash"
			CXR_CACHE_H_TYPE="$type"
			CXR_CACHE_H_KEY="$key"
			CXR_CACHE_H_VALUE="$value"
			
			# Return the value
			echo $value
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
# > for value in $(common.hash.getAll "$hash" "$type" "$key")
# > do
# > echo "$value"
# > done
# > IFS="$oIFS"
#
# Returns the empty string on error or if this key does not exist.
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.hash.getAll()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type, a key and an optional boolean (restrict_model_version) as input"
	fi
	
	local hash
	local type
	local key
	local restrict_model_version
	local model
	local version
	
	hash="$1"
	type="$2"
	key="$3"
	restrict_model_version="${4:-false}"
	
	if [[ "$restrict_model_version" == true ]]
	then
		model=$CXR_MODEL
		version=$CXR_MODEL_VERSION
	else
		model=any
		version=any
	fi
	
	local value
	
	main.log -v "Getting al values for $key out of hash $hash $type"
	
	local db_file

	# Work out the filename
	db_file="$(_common.hash.getDbFile "$type")"
	
	# Get value
	if [[ ! -f "$db_file" ]]
	then
		main.log -w "DB $db_file not found!"
		echo ""
	else
		# get the contents
		${CXR_SQLITE_EXEC} "$db_file" "SELECT value FROM hash WHERE hash='$hash' AND key='$key' AND model='$model' AND version='$version' ORDER BY epoch_c ASC")
	fi
}

################################################################################
# Function: common.hash.delete
#
# Deletes a certain key from a hash without returning its value.
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.hash.delete()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type, a key and an optional boolean (restrict_model_version) as input"
	fi
	
	local hash
	local type
	local key
	local db_file
	local restrict_model_version
	local model
	local version
	
	hash="$1"
	type="$2"
	key="$3"
	restrict_model_version="${4:-false}"
	
	if [[ "$restrict_model_version" == true ]]
	then
		model=$CXR_MODEL
		version=$CXR_MODEL_VERSION
	else
		model=any
		version=any
	fi
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$type")""
	
	# Just delete
	if [[ ! -f "$db_file" ]]
	then
		main.log -w "DB $db_file not found!"
	else
		# delete entry
		${CXR_SQLITE_EXEC} "$db_file" "DELETE FROM hash WHERE hash='$hash' AND key='$key' AND model='$model' AND version='$version'"
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
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.hash.remove()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type and a key as input"
	fi
	
	local hash
	local type
	local key
	local restrict_model_version
	
	hash="$1"
	type="$2"
	key="$3"
	restrict_model_version="${4:-false}"
	
	common.hash.get "$hash" "$type" "$key" $restrict_model_version
	common.hash.delete "$hash" "$type" "$key" $restrict_model_version
}

################################################################################
# Function: common.hash.getMtime
#
# Gets the modification time (Unix Epoch) for a given hash (the mtime of the db_file)
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
	
	local hash
	local type
	local db_file
	
	hash="$1"
	type="$2"
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$type")"
	
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
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.hash.getValueMtime()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type, a key and an optional boolean (restrict_model_version) as input"
	fi
	
	local hash
	local type
	local key
	local db_file
	local restrict_model_version
	local model
	local version
	
	hash="$1"
	type="$2"
	key="$3"
	restrict_model_version="${4:-false}"
	local mtime
	
	if [[ "$restrict_model_version" == true ]]
	then
		model=$CXR_MODEL
		version=$CXR_MODEL_VERSION
	else
		model=any
		version=any
	fi
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$type")"
	
	# Get the value
	mtime=$(${CXR_SQLITE_EXEC} "$db_file" "SELECT epoch_c FROM hash WHERE hash='$hash' AND key='$key' AND model='$model' AND version='$version' ORDER BY epoch_c DESC LIMIT 1")
	
	echo $mtime
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
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.hash.has?()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4  ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type, a key and an optional boolean (restrict_model_version) as input"
	fi
	
	local hash
	local type
	local key
	local db_file
	local restrict_model_version
	local model
	local version
	
	hash="$1"
	type="$2"
	key="$3"
	restrict_model_version="${4:-false}"
	
	if [[ "$restrict_model_version" == true ]]
	then
		model=$CXR_MODEL
		version=$CXR_MODEL_VERSION
	else
		model=any
		version=any
	fi
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$type")"
	
	if [[ ! -f "$db_file" ]]
	then
		main.log -w "DB $db_file not found!"
		_has=false
	else
		# get the rowcount
		rowcount=$(${CXR_SQLITE_EXEC} "$db_file" "SELECT COUNT(*) FROM hash WHERE hash='$hash' AND key='$key' AND model='$model' AND version='$version' ORDER BY epoch_c DESC LIMIT 1")

		if [[ $rowcount -gt 0 ]]
		then
			# Get the value
			value=$(${CXR_SQLITE_EXEC} "$db_file" "SELECT value FROM hash WHERE hash='$hash' AND key='$key' AND model='$model' AND version='$version' ORDER BY epoch_c DESC LIMIT 1")
			
			# Fill cache
			CXR_CACHE_H_HASH="$hash"
			CXR_CACHE_H_TYPE="$type"
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
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.hash.isNew?()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type, a key and an optional boolean (restrict_model_version) as input"
	fi
	
	
	local hash
	local type
	local key
	local restrict_model_version
	local model
	local version
	
	hash="$1"
	type="$2"
	key="$3"
	restrict_model_version="${4:-false}"
	
	local res
	
	# Is it in the hash?
	if [[ $(common.hash.has? "$hash" "$type" "$key" "$restrict_model_version") == true ]]
	then
		# Exists, test age. CXR_EPOCH is the Epoch we started this run in
		# if the hash's epoch is smaller, it is older
		if [[ "$(common.hash.getValueMtime "$hash" "$type" "$key" "$restrict_model_version")" -lt "$CXR_EPOCH" ]]
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
# Returns a unique list of keys of the given hash as a quoted CXR_DELIMITER separated list.
# Do not assume any particular order, it depends on the order the hash imposes on the
# data. 
# If the DB does not exist, returns the empty string.
#
# Use this function only if you are not interested in the actual values in the hash
# (if you merely need to know something exists). 
# The functions <common.hash.getValues> and <common.hash.getKeysAndValuesValues> 
# also provide you with the data in one call.
# 
# Recommended use:
# > oIFS="$IFS"
# > keyString="$(common.hash.getKeys $hash $CXR_HASH_TYPE_GLOBAL)"
# > IFS="$CXR_DELIMITER"
# > # Turn string into array (we cannot call <common.hash.getKeys> directly here!)
# > arrKeys=( $keyString )
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
# [$3] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.hash.getKeys()
################################################################################
{
	if [[ $# -lt 2 || $# -gt 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type an an optional boolean (restrict_model_version) as input"
	fi
	
	local hash
	local type
	
	local found
	local key
	local list
	local restrict_model_version
	local model
	local version
	local db_file
	
	hash="$1"
	type="$2"
	restrict_model_version="${3:-false}"
	
	if [[ "$restrict_model_version" == true ]]
	then
		model=$CXR_MODEL
		version=$CXR_MODEL_VERSION
	else
		model=any
		version=any
	fi
	
	found=false
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$type")"
	
	main.log -v "Getting keys for $hash $type out of ${db_file}..."
	
	if [[ -f ${db_file} ]]
	then
		# DB exists, get data
		for key in $(${CXR_SQLITE_EXEC} "$db_file" "SELECT DISTINCT key FROM hash WHERE hash='$hash' AND model='$model' AND version='$version'")
		do
			found=true

			list="${list}${key}$CXR_DELIMITER"
		done
		
		# Remove last delimiter
		list="${list/%${CXR_DELIMITER}/}"
	
	else
		main.log -w "DB ${db_file} does not exist."
		list=""
	fi
	
	if [[ $found == false ]]
	then
		main.log -w "DB ${db_file} is empty..."
	fi
	
	echo $list
}

################################################################################
# Function: common.hash.getValues
#
# Returns all valuos of the given hash as a quoted CXR_DELIMITER separated list.
# Do not assume any particular order, it depends on the order the hash imposes on the
# data. 
# If the DB does not exist, returns the empty string.
# 
# Recommended use:
# > oIFS="$IFS"
# > valueString="$(common.hash.getValues $hash $CXR_HASH_TYPE_GLOBAL)"
# > IFS="$CXR_DELIMITER"
# > # Turn string into array (we cannot call <common.hash.getKeys> directly here!)
# > arrVal=( $valueString )
# > # Reset Internal Field separator
# > IFS="$oIFS"
# > 
# > # looping through keys (safest approach)
# > for iVal in $( seq 0 $(( ${#arrVal[@]} - 1)) )
# > do
# > 	val=${arrVal[$iVal]}
# > 	# Whatever more
# > done
# 
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# [$3] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.hash.getValues()
################################################################################
{
	if [[ $# -lt 2 || $# -gt 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type an an optional boolean (restrict_model_version) as input"
	fi
	
	local hash
	local type
	
	local found
	local value
	local list
	local restrict_model_version
	local model
	local version
	local db_file
	
	hash="$1"
	type="$2"
	restrict_model_version="${3:-false}"
	
	if [[ "$restrict_model_version" == true ]]
	then
		model=$CXR_MODEL
		version=$CXR_MODEL_VERSION
	else
		model=any
		version=any
	fi
	
	found=false
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$type")"
	
	main.log -v "Getting keys for $hash $type out of ${db_file}..."
	
	if [[ -f ${db_file} ]]
	then
		# DB exists, get data
		for value in $(${CXR_SQLITE_EXEC} "$db_file" "SELECT value FROM hash WHERE hash='$hash' AND model='$model' AND version='$version'")
		do
			found=true

			list="${list}${value}$CXR_DELIMITER"
		done
		
		# Remove last delimiter
		list="${list/%${CXR_DELIMITER}/}"
	
	else
		main.log -w "DB ${db_file} does not exist."
		list=""
	fi
	
	if [[ $found == false ]]
	then
		main.log -w "DB ${db_file} is empty..."
	fi
	
	echo $list
}

################################################################################
# Function: common.hash.getKeysAndValues
#
# Returns rows of unique key value pairs as in 
# key1|value1
# key2|value2
# Do not assume any particular order, it depends on the order the hash imposes on the
# data. We ensure that for each key we return only one value (the most recet one).
# If the DB does not exist, returns the empty string.
# 
# Recommended use:
#
# > # looping through pairs
# > for pair in $(common.hash.getKeysAndValues $hash $type)
# > do
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
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# [$3] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.hash.getKeysAndValues()
################################################################################
{
	if [[ $# -lt 2 || $# -gt 3 ]]
	then
		main.dieGracefully "needs a hash, a valid hash-type an an optional boolean (restrict_model_version) as input"
	fi
	
	local hash
	local type
	
	local found
	local key
	local list
	local restrict_model_version
	local model
	local version
	local db_file
	
	hash="$1"
	type="$2"
	restrict_model_version="${3:-false}"
	
	if [[ "$restrict_model_version" == true ]]
	then
		model=$CXR_MODEL
		version=$CXR_MODEL_VERSION
	else
		model=any
		version=any
	fi
	
	found=false
	
	# Work out the filename
	db_file="$(_common.hash.getDbFile "$type")"
	
	main.log -v "Getting keys for $hash $type out of ${db_file}..."
	
	if [[ -f ${db_file} ]]
	then
		# DB exists, get data
		for key in $(${CXR_SQLITE_EXEC} "$db_file" "SELECT key, value FROM hash WHERE hash='$hash' AND model='$model' AND version='$version' GPOUP BY key, value HAVING MAX(epoch_c)")
		do
			found=true

			list="${list}${key}$CXR_DELIMITER"
		done
		
		# Remove last delimiter
		list="${list/%${CXR_DELIMITER}/}"
	
	else
		main.log -w "DB ${db_file} does not exist."
		list=""
	fi
	
	if [[ $found == false ]]
	then
		main.log -w "DB ${db_file} is empty..."
	fi
	
	echo $list
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
	common.hash.init test_instance $CXR_HASH_TYPE_INSTANCE
	common.hash.put test_instance $CXR_HASH_TYPE_INSTANCE /hallo/gugs SomeOtherValue
	common.hash.put test_instance $CXR_HASH_TYPE_INSTANCE /hallo/velo SomeOtherValue
	
	# DB of arrays
	common.hash.init test_array $CXR_HASH_TYPE_INSTANCE
	common.hash.put test_array $CXR_HASH_TYPE_INSTANCE array1 "1 2 3 4 5"
	
	# Read again
	a=( $(common.hash.get test_array $CXR_HASH_TYPE_INSTANCE array1) )
	
	# Glabal DB with strange keys
	common.hash.init test_global $CXR_HASH_TYPE_GLOBAL
	common.hash.put test_global $CXR_HASH_TYPE_GLOBAL "This key has spaces" "a value"
	common.hash.put test_global $CXR_HASH_TYPE_GLOBAL "This key also has spaces" "another value"
	
	# Universal DB
	common.hash.init test_universal $CXR_HASH_TYPE_UNIVERSAL
	common.hash.put test_universal $CXR_HASH_TYPE_UNIVERSAL /hallo/gugs SomeOtherValue
	common.hash.put test_universal  $CXR_HASH_TYPE_UNIVERSAL /hallo/velo SomeOtherValue

	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is "$(common.hash.get test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo")" SomeOtherValue "common.hash.get test_instance with path as key"
	is "$(common.hash.has? test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo")" true "common.hash.has? test_instance with path as key"
	is "$(common.hash.getKeys test_instance $CXR_HASH_TYPE_INSTANCE)" "/hallo/gugs${CXR_DELIMITER}/hallo/velo" "common.hash.getKeys test_instance with path as key"
	is ${#a[@]} 5 "DB of arrays"
	
	# testing the faster way to call common.hash.has? (has and get in one call)
	common.hash.has? test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo" > /dev/null
	is $_has true "common.hash.has? non-functional approach I"
	is $_value SomeOtherValue "common.hash.has? non-functional approach II"
	
	oIFS="$IFS"
	keyString="$(common.hash.getKeys test_instance $CXR_HASH_TYPE_INSTANCE)"
	IFS="$CXR_DELIMITER"
	# Turn string into array (we cannot call <common.hash.getKeys> directly here!)
	arrKeys=( $keyString )
	# Reset Internal Field separator
	IFS="$oIFS"
	
	# looping through keys (safest approach)
	for iKey in $( seq 0 $(( ${#arrKeys[@]} - 1)) )
	do
		key="${arrKeys[$iKey]}"
		is "$(common.hash.get test_instance $CXR_HASH_TYPE_INSTANCE "$key")" SomeOtherValue "Going through keys in an interator"
	done
	
	# Lets retrieve those with spaces
	is "$(common.hash.get test_global $CXR_HASH_TYPE_GLOBAL "This key has spaces")" "a value" "common.hash.get test_instance - key with spaces"
	
	common.hash.delete test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo"
	is "$(common.hash.has? test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo")" false "common.hash.delete test_instance with path as key"

	is "$(common.hash.get test_universal $CXR_HASH_TYPE_UNIVERSAL "/hallo/velo")" SomeOtherValue "common.hash.get test_universal with path as key"
	is "$(common.hash.has? test_universal $CXR_HASH_TYPE_UNIVERSAL "/hallo/velo")" true "common.hash.has? test_universal with path as key"
	
	common.hash.delete test_universal $CXR_HASH_TYPE_UNIVERSAL "/hallo/velo" 
	is "$(common.hash.has? test_universal $CXR_HASH_TYPE_UNIVERSAL "/hallo/velo")" false "common.hash.delete test_universal with path as key"
	
	########################################
	# teardown tests if needed
	########################################
	common.hash.destroy test_instance $CXR_HASH_TYPE_INSTANCE
	common.hash.destroy test_array $CXR_HASH_TYPE_INSTANCE
	common.hash.destroy test_global $CXR_HASH_TYPE_GLOBAL
	common.hash.destroy test_universal $CXR_HASH_TYPE_UNIVERSAL
}
