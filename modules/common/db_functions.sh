# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
#
# Version: $Id$ 
#
# This is a re-implementation of the db functions which uses sqlite <http://www.sqlite.org>.
# Compared to the old filebased approach, this has a number of advantages:
# - there is no need to limit the number of keys per directory (was an issue on AFS)
# - less overhead
# - we can benefit from disk-chaching
# -> its faster (a simple test showed sqlite is about four to five times faster than my plain old filedb)
# - storage of additional metadata is easy
# The goal is to map keys to values, but we also need this information:
# - When was an etry created?
# - To which model/Version does the entry belong (optional)
# - What keys are available
# - Storage of historical information (MD5 dbes at different times)
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
CXR_META_MODULE_NUM_TESTS=13

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|sqlite3"

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the DB functions for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2009), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: _common.db.getDbFile
#
# Returns the db_file to use depending on the type.
#
# Parameters:
# $1 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL" 
# $2 - name of the hash
################################################################################
function _common.db.getDbFile()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a db-type and a db name as input"
	fi

	local type
	local db
	type="${1}"
	db="${2}"
	
	if [[ "${db}" ]]
	then
		# Work out the directory
		case $type in
			$CXR_DB_TYPE_INSTANCE) echo "${CXR_INSTANCE_DB_DIR}/${db}.${CXR_DB_SUFFIX}" ;;
			$CXR_DB_TYPE_GLOBAL) echo "${CXR_GLOBAL_DB_DIR}/${db}.${CXR_DB_SUFFIX}" ;;
			$CXR_DB_TYPE_UNIVERSAL) echo "${CXR_UNIVERSAL_DB_DIR}/${db}.${CXR_DB_SUFFIX}" ;;
			*) main.dieGracefully "Unknown DB type $type" ;;
		esac
	else
		main.dieGracefully "DB name is empty!" 
	fi
}



################################################################################
# Function: common.db.init
#
# Creates a db with a given name, if it already exists, nothing happens.
# We distinguish three visibility levels: 
# $CXR_DB_TYPE_INSTANCE - only visible for this instance
# $CXR_DB_TYPE_GLOBAL - visible for all instances of this run
# $CXR_DB_TYPE_UNIVERSAL - visible for all instances of all runs
# Usage note: since we can store metadata in a db, it is not recommended to encode
# information in the name of the db (like it was done before).
#
# Parameters:
# $1 - name of the db (must be usable as a filename)
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL" 
################################################################################
function common.db.init()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a db name and a valid db-type as input"
	fi
	
	local db
	local type
	
	db="$1"
	type="$2"

	local db_file
	
	# Work out the filename
	db_file="$(_common.db.getDbFile "$type" "$db")"
	
	main.log -v "Creating DB $db_file"
	
	# Create table, no matter what
	${CXR_SQLITE_EXEC} "$db_file" "CREATE TABLE IF NOT EXISTS hash (key, value , model, version, epoch_c)"


	# Nobody else must modify this file
	chmod 600 "$db_file"
}

################################################################################
# Function: common.db.destroy
#
# Destroys a db with a given name by deleting its datafile.
#
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL"
################################################################################
function common.db.destroy()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a db and a valid db-type as input"
	fi
	
	local db
	local type
	
	db="$1"
	type="$2"

	local db_file
	
	# Work out the filename
	db_file="$(_common.db.getDbFile "$type" "$db")"
	
	main.log -v "Deleting the DB ${db}"
	rm -f "${db_file}"
}

################################################################################
# Function: common.db.put
#
# Puts a value into a key of a given db. Alos touches the DB file (needed to
# determine the last access)
#
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL"
# $3 - key
# $4 - value
# [$5] - restrict_model_version , boolean, if true (default false), we write tag this entry with the model and version
################################################################################
function common.db.put()
################################################################################
{
	if [[ $# -lt 4 || $# -gt 5 ]]
	then
		main.dieGracefully "needs a db, a valid db-type, a key, a value and an optional boolean (restrict_model_version) as input. Got $@"
	fi
	
	local db
	local type
	local key
	local value
	local restrict_model_version
	local model
	local version
	
	db="$1"
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
	db_file="$(_common.db.getDbFile "$type" "$db")"
	
	if [[ ! -f "$db_file" ]]
	then
		main.log -w "DB $db_file not found!"
		common.db.init "$db" "$type"
	else
		# Change the update time
		touch "$db_file"
	fi
	
	# Write value to DB
	${CXR_SQLITE_EXEC} "$db_file" "INSERT INTO hash (key, value , model, version, epoch_c) VALUES ('$key','$value','$model','$version',$(date "+%s"))" || :
	
	# Fill cache
	CXR_CACHE_H_HASH="$db"
	CXR_CACHE_H_TYPE="$type"
	CXR_CACHE_H_KEY="$key"
	CXR_CACHE_H_VALUE="$value"
}

################################################################################
# Function: common.db.get
#
# Gets a certain value from a db.
# Be careful, values might contain spaces and other nasties. Use like this:
# > value="$(common.db.get "$db" "$type" "$key")"
# Returns the empty string on error or if this key does not exist.
#
# Often, the same items are accessed several times in a row. For this reason,
# we cache the last value.
#
# Since a DB may contain more than one row with this key (by design), we return the 
# newest (highest epoch_c)
#
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL"
# $3 - key
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.db.get()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a db, a valid db-type, a key and an optional boolean (restrict_model_version) as input"
	fi
	
	local db
	local type
	local key
	local restrict_model_version
	local model
	local version
	
	db="$1"
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
	
	if [[ "$db" == "${CXR_CACHE_H_HASH:-}" &&\
	      "$type" == "${CXR_CACHE_H_TYPE:-}" &&\
	      "$key" == "${CXR_CACHE_H_KEY:-}" ]]
	then
		# Match in Cache
		echo "$CXR_CACHE_H_VALUE"
	else
		# Lookup needed
		main.log -v "Getting $key out of db $db $type"
		
		local db_file
	
		# Work out the filename
		db_file="$(_common.db.getDbFile "$type" "$db")"
		
		# Get value
		if [[ ! -f "$db_file" ]]
		then
			main.log -w "DB $db_file not found!"
			echo ""
		else
			# Read the contents
			value=$(${CXR_SQLITE_EXEC} "$db_file" "SELECT value FROM hash WHERE key='$key' AND model='$model' AND version='$version' ORDER BY epoch_c DESC LIMIT 1")
			
			# Fill cache
			CXR_CACHE_H_HASH="$db"
			CXR_CACHE_H_TYPE="$type"
			CXR_CACHE_H_KEY="$key"
			CXR_CACHE_H_VALUE="$value"
			
			# Return the value
			echo $value
		fi
	fi
}


################################################################################
# Function: common.db.delete
#
# Deletes a certain key from a db without returning its value.
#
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL"
# $3 - key
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.db.delete()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a db, a valid db-type, a key and an optional boolean (restrict_model_version) as input"
	fi
	
	local db
	local type
	local key
	local db_file
	local restrict_model_version
	local model
	local version
	
	db="$1"
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
	db_file="$(_common.db.getDbFile "$type" "$db")"
	
	# Just delete
	if [[ ! -f "$db_file" ]]
	then
		main.log -w "DB $db_file not found!"
	else
		# delete entry
		${CXR_SQLITE_EXEC} "$db_file" "DELETE FROM hash WHERE key='$key' AND model='$model' AND version='$version'"
	fi
}


################################################################################
# Function: common.db.remove
#
# Returns a value from a db and then deletes it.
#
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL"
# $3 - key
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.db.remove()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a db, a valid db-type and a key as input"
	fi
	
	local db
	local type
	local key
	local restrict_model_version
	
	db="$1"
	type="$2"
	key="$3"
	restrict_model_version="${4:-false}"
	
	common.db.get "$db" "$type" "$key" $restrict_model_version
	common.db.delete "$db" "$type" "$key" $restrict_model_version
}

################################################################################
# Function: common.db.getMtime
#
# Gets the modification time (Unix Epoch) for a given db (the mtime of the db_file)
#
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL"
################################################################################
function common.db.getMtime()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a db and a valid db-type as input"
	fi
	
	local db
	local type
	local db_file
	
	db="$1"
	type="$2"
	
	# Work out the filename
	db_file="$(_common.db.getDbFile "$type" "$db")"

	# Get the mtime
	common.fs.getMtime "$db_file"
}

################################################################################
# Function: common.db.getValueMtime
#
# Gets the (latest) modification time (Unix Epoch) for a given value.
# If the value is not found, the empty string is returned
#
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL"
# $3 - key
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.db.getValueMtime()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a db, a valid db-type, a key and an optional boolean (restrict_model_version) as input"
	fi
	
	local db
	local type
	local key
	local db_file
	local restrict_model_version
	local model
	local version
	
	db="$1"
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
	db_file="$(_common.db.getDbFile "$type" "$db")"
	
	# Get the value
	mtime=$(${CXR_SQLITE_EXEC} "$db_file" "SELECT epoch_c FROM hash WHERE key='$key' AND model='$model' AND version='$version' ORDER BY epoch_c DESC LIMIT 1")
	
	echo $mtime
}

################################################################################
# Function: common.db.has?
#
# Returns true if the given key is contained in the db. Also fills cache on hit.
# and sets _has and _value (to avoid having to call <common.db.get> again)
#
# If you use the "non-functional form" redirect output to /dev/null:
# > common.db.has? $CXR_GLOBAL_DB_DECOMPRESSED_FILES $CXR_DB_TYPE_GLOBAL "${input_file}" > /dev/null
# > if [[ "$_has" == true ]]
# > then ...
#
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL"
# $3 - key
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.db.has?()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4  ]]
	then
		main.dieGracefully "needs a db, a valid db-type, a key and an optional boolean (restrict_model_version) as input"
	fi
	
	local db
	local type
	local key
	local db_file
	local restrict_model_version
	local model
	local version
	
	db="$1"
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
	db_file="$(_common.db.getDbFile "$type" "$db")"
	
	if [[ ! -f "$db_file" ]]
	then
		main.log -w "DB $db_file not found!"
		_has=false
	else
		# get the rowcount
		rowcount=$(${CXR_SQLITE_EXEC} "$db_file" "SELECT COUNT(*) FROM hash WHERE key='$key' AND model='$model' AND version='$version' ORDER BY epoch_c DESC LIMIT 1")

		if [[ $rowcount -gt 0 ]]
		then
			# Get the value
			value=$(${CXR_SQLITE_EXEC} "$db_file" "SELECT value FROM hash WHERE key='$key' AND model='$model' AND version='$version' ORDER BY epoch_c DESC LIMIT 1")
			
			# Fill cache
			CXR_CACHE_H_HASH="$db"
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
# Function: common.db.isNew?
#
# Returns true if the given key is contained in the db and its update time is 
# newer than this runs start time (meaning that we do not need to update such a db
# if it is used as a cache)
#
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL"
# $3 - key
# [$4] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.db.isNew?()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a db, a valid db-type, a key and an optional boolean (restrict_model_version) as input"
	fi
	
	
	local db
	local type
	local key
	local restrict_model_version
	local model
	local version
	
	db="$1"
	type="$2"
	key="$3"
	restrict_model_version="${4:-false}"
	
	local res
	
	# Is it in the db?
	if [[ $(common.db.has? "$db" "$type" "$key" "$restrict_model_version") == true ]]
	then
		# Exists, test age. CXR_EPOCH is the Epoch we started this run in
		# if the db's epoch is smaller, it is older
		if [[ "$(common.db.getValueMtime "$db" "$type" "$key" "$restrict_model_version")" -lt "$CXR_EPOCH" ]]
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
# Function: common.db.getKeys
#
# Returns a unique list of keys of the given db as a quoted CXR_DELIMITER separated list.
# Do not assume any particular order, it depends on the order the db imposes on the
# data. 
# If the DB does not exist, returns the empty string.
# 
# Recommended use:
# > oIFS="$IFS"
# > keyString="$(common.db.getKeys $db $CXR_DB_TYPE_GLOBAL)"
# > IFS="$CXR_DELIMITER"
# > # Turn string into array (we cannot call <common.db.getKeys> directly here!)
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
# $1 - name of the db
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL"
# [$3] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.db.getKeys()
################################################################################
{
	if [[ $# -lt 2 || $# -gt 3 ]]
	then
		main.dieGracefully "needs a db, a valid db-type an an optional boolean (restrict_model_version) as input"
	fi
	
	local db
	local type
	
	local found
	local key
	local list
	local restrict_model_version
	local model
	local version
	local db_file
	
	db="$1"
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
	db_file="$(_common.db.getDbFile "$type" "$db")"
	
	main.log -v "Getting keys for $db $type out of ${db_file}..."
	
	if [[ -f ${db_file} ]]
	then
		# DB exists, get data
		for key in $(${CXR_SQLITE_EXEC} "$db_file" "SELECT DISTINCT key FROM hash WHERE model='$model' AND version='$version'")
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
# Function: common.db.getValues
#
# Returns all valuos of the given db as a quoted CXR_DELIMITER separated list.
# Do not assume any particular order, it depends on the order the db imposes on the
# data. 
# If the DB does not exist, returns the empty string.
# 
# Recommended use:
# > oIFS="$IFS"
# > valueString="$(common.db.getValues $db $CXR_DB_TYPE_GLOBAL)"
# > IFS="$CXR_DELIMITER"
# > # Turn string into array (we cannot call <common.db.getKeys> directly here!)
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
# $1 - name of the db
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL"
# [$3] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.db.getValues()
################################################################################
{
	if [[ $# -lt 2 || $# -gt 3 ]]
	then
		main.dieGracefully "needs a db, a valid db-type an an optional boolean (restrict_model_version) as input"
	fi
	
	local db
	local type
	
	local found
	local value
	local list
	local restrict_model_version
	local model
	local version
	local db_file
	
	db="$1"
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
	db_file="$(_common.db.getDbFile "$type" "$db")"
	
	main.log -v "Getting keys for $db $type out of ${db_file}..."
	
	if [[ -f ${db_file} ]]
	then
		# DB exists, get data
		for value in $(${CXR_SQLITE_EXEC} "$db_file" "SELECT value FROM hash WHERE model='$model' AND version='$version'")
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
# Function: common.db.getKeysAndValues
#
# Returns rows of unique key value pairs as in 
# key1|value1
# key2|value2
# Do not assume any particular order, it depends on the order the db imposes on the
# data. We ensure that for each key we return only one value (the most recet one).
# If the DB does not exist, returns the empty string.
# 
# Recommended use:
# > oIFS="$IFS"
# > IFS="$CXR_DELIMITER"
# > # looping through pairs
# > for pair in $(common.db.getKeysAndValues $db $type)
# > do
# > 	set $pair
# > 	key="$1"
# > 	value="$2"
# > done
# > IFS="$oIFS"
# 
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_DB_TYPE_INSTANCE" , "$CXR_DB_TYPE_GLOBAL" or "$CXR_DB_TYPE_UNIVERSAL"
# [$3] - restrict_model_version , boolean, if true (default false), we get only an entry for this model and version
################################################################################
function common.db.getKeysAndValues()
################################################################################
{
	if [[ $# -lt 2 || $# -gt 3 ]]
	then
		main.dieGracefully "needs a db, a valid db-type an an optional boolean (restrict_model_version) as input"
	fi
	
	local db
	local type
	
	local found
	local key
	local list
	local restrict_model_version
	local model
	local version
	local db_file
	
	db="$1"
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
	db_file="$(_common.db.getDbFile "$type" "$db")"
	
	main.log -v "Getting keys for $db $type out of ${db_file}..."
	
	if [[ -f ${db_file} ]]
	then
		# DB exists, get data
		for key in $(${CXR_SQLITE_EXEC} "$db_file" "SELECT key, value FROM hash WHERE model='$model' AND version='$version' GPOUP BY key, value HAVING MAX(epoch_c)")
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
	# Instance db
	common.db.init test_instance $CXR_DB_TYPE_INSTANCE
	common.db.put test_instance $CXR_DB_TYPE_INSTANCE /hallo/gugs SomeOtherValue
	common.db.put test_instance $CXR_DB_TYPE_INSTANCE /hallo/velo SomeOtherValue
	
	# DB of arrays
	common.db.init test_array $CXR_DB_TYPE_INSTANCE
	common.db.put test_array $CXR_DB_TYPE_INSTANCE array1 "1 2 3 4 5"
	
	# Read again
	a=( $(common.db.get test_array $CXR_DB_TYPE_INSTANCE array1) )
	
	# Glabal DB with strange keys
	common.db.init test_global $CXR_DB_TYPE_GLOBAL
	common.db.put test_global $CXR_DB_TYPE_GLOBAL "This key has spaces" "a value"
	common.db.put test_global $CXR_DB_TYPE_GLOBAL "This key also has spaces" "another value"
	
	# Universal DB
	common.db.init test_universal $CXR_DB_TYPE_UNIVERSAL
	common.db.put test_universal $CXR_DB_TYPE_UNIVERSAL /hallo/gugs SomeOtherValue
	common.db.put test_universal  $CXR_DB_TYPE_UNIVERSAL /hallo/velo SomeOtherValue

	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is "$(common.db.get test_instance $CXR_DB_TYPE_INSTANCE "/hallo/velo")" SomeOtherValue "common.db.get test_instance with path as key"
	is "$(common.db.has? test_instance $CXR_DB_TYPE_INSTANCE "/hallo/velo")" true "common.db.has? test_instance with path as key"
	is "$(common.db.getKeys test_instance $CXR_DB_TYPE_INSTANCE)" "/hallo/gugs${CXR_DELIMITER}/hallo/velo" "common.db.getKeys test_instance with path as key"
	is ${#a[@]} 5 "DB of arrays"
	
	# testing the faster way to call common.db.has? (has and get in one call)
	common.db.has? test_instance $CXR_DB_TYPE_INSTANCE "/hallo/velo" > /dev/null
	is $_has true "common.db.has? non-functional approach I"
	is $_value SomeOtherValue "common.db.has? non-functional approach II"
	
	oIFS="$IFS"
	keyString="$(common.db.getKeys test_instance $CXR_DB_TYPE_INSTANCE)"
	IFS="$CXR_DELIMITER"
	# Turn string into array (we cannot call <common.db.getKeys> directly here!)
	arrKeys=( $keyString )
	# Reset Internal Field separator
	IFS="$oIFS"
	
	# looping through keys (safest approach)
	for iKey in $( seq 0 $(( ${#arrKeys[@]} - 1)) )
	do
		key="${arrKeys[$iKey]}"
		is "$(common.db.get test_instance $CXR_DB_TYPE_INSTANCE "$key")" SomeOtherValue "Going through keys in an interator"
	done
	
	# Lets retrieve those with spaces
	is "$(common.db.get test_global $CXR_DB_TYPE_GLOBAL "This key has spaces")" "a value" "common.db.get test_instance - key with spaces"
	
	common.db.delete test_instance $CXR_DB_TYPE_INSTANCE "/hallo/velo"
	is "$(common.db.has? test_instance $CXR_DB_TYPE_INSTANCE "/hallo/velo")" false "common.db.delete test_instance with path as key"

	is "$(common.db.get test_universal $CXR_DB_TYPE_UNIVERSAL "/hallo/velo")" SomeOtherValue "common.db.get test_universal with path as key"
	is "$(common.db.has? test_universal $CXR_DB_TYPE_UNIVERSAL "/hallo/velo")" true "common.db.has? test_universal with path as key"
	
	common.db.delete test_universal $CXR_DB_TYPE_UNIVERSAL "/hallo/velo" 
	is "$(common.db.has? test_universal $CXR_DB_TYPE_UNIVERSAL "/hallo/velo")" false "common.db.delete test_universal with path as key"
	
	########################################
	# teardown tests if needed
	########################################
	common.db.destroy test_instance $CXR_DB_TYPE_INSTANCE
	common.db.destroy test_array $CXR_DB_TYPE_INSTANCE
	common.db.destroy test_global $CXR_DB_TYPE_GLOBAL
	common.db.destroy test_universal $CXR_DB_TYPE_UNIVERSAL
}
