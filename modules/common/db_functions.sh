# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
#
# Version: $Id$ 
#
# These functions provide access to the sqlite DB and provide logging and error handling.
# We implement the usual 3-layer visibility.
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=6

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|sqlite3"

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains sqlite3 access functions for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

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
# $1 - db name
# $1 - type , either "$CXR_TYPE_INSTANCE" , "$CXR_TYPE_GLOBAL" or "$CXR_TYPE_UNIVERSAL" 
################################################################################
function _common.db.getDbFile()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a db name and a type as input, got $@"
	fi

	local db
	local type
	local fn
	
	db="${1}"
	type="${2}"
	
	if [[ "${type}" ]]
	then
		# Work out the directory
		case $type in
			$CXR_TYPE_INSTANCE) fn="${CXR_INSTANCE_DIR}/hashes.${CXR_DB_SUFFIX}" ;;
			$CXR_TYPE_GLOBAL) fn="${CXR_GLOBAL_DIR}/hashes.${CXR_DB_SUFFIX}" ;;
			$CXR_TYPE_UNIVERSAL) fn="${CXR_UNIVERSAL_DIR}/hashes.${CXR_DB_SUFFIX}" ;;
			*) main.dieGracefully "Unknown DB type $type" ;;
		esac
	else
		main.dieGracefully "DB type is empty!" 
	fi
	
	# It is poassible that this file does not exist.
	echo "$fn"

}

################################################################################
# Function: common.db.init
#
# Performs version checks on all visible sqlite DBs.
#
################################################################################
function common.db.init()
################################################################################
{
	:
}

################################################################################
# Function: common.db.getResultSet
#
# Function that returns a resultset on stdout. 
# Do not use this function to alter the database - we aquire no writelock!
#
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_TYPE_INSTANCE" , "$CXR_TYPE_GLOBAL" or "$CXR_TYPE_UNIVERSAL"
# $3 - either a statement, a filename or - indicating input from stdin
# [$4] - otional separator (default $CXR_DELIMITER)
################################################################################
function common.db.getResultSet()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a db and a valid type and a statement (optional delimiter) as input, got $@"
	fi
	
	local db
	local type
	local statement
	local separator
	local input
	local currline
	
	local db_file
	
	db="$1"
	type="$2"
	statement="$3"
	separator="${4:-$CXR_DELIMITER}"

	# Work out the filename
	db_file="$(_common.db.getDbFile "$db" "$type")"
	
	if [[ ! -r $db_file ]]
	then
		main.log -w "DB file $db_file not readable"
		return $CXR_RET_OK
	fi
	
	# Detect type of statement
	if [[ "$statement" == - ]]
	then
		# statement is "-" (meaning we read from stdin)
		input=""
		while read currline
		do
			if [[ "$currline" ]]
			then
				input="$input $currline"
			fi
		done
		
		{CXR_SQLITE_EXEC} -separator "${separator}" "$db_file" < $(echo $input)
	elif [[ -r "$statement" ]]
	then
		# statement is a file, read from there
		${CXR_SQLITE_EXEC} -separator "${separator}" "$db_file" < $(cat "$statement")
	else
		# Execute the string
		${CXR_SQLITE_EXEC} -separator "${separator}" "$db_file" "$statement"
	fi
}

################################################################################
# Function: common.db.change
#
# Use this function (procedure) for all SQL statements containing DML (INSERT,UPDATE,DELETE)
# or DDL (CREATE, ALTER, DROP) statements. A writelock is aqciured.
#
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_TYPE_INSTANCE" , "$CXR_TYPE_GLOBAL" or "$CXR_TYPE_UNIVERSAL"
# $3 - either a statement, a filename or - indicating input from stdin
################################################################################
function common.db.change()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a db and a valid type and a statement as input, got $@"
	fi
	
	local db
	local type
	local statement
	local separator
	local input
	local currline
	
	local db_file
	
	db="$1"
	type="$2"
	statement="$3"

	# Work out the filename
	db_file="$(_common.db.getDbFile "$db" "$type")"

	# For security reasons, we lock all write accesses to the DB
	if [[ $(common.runner.getLock "$(basename $db_file)" "$type") == false ]]
	then
		main.dieGracefully "Could not get lock on $(basename $db_file)"
	fi
	
	# Detect type of statement
	if [[ "$statement" == - ]]
	then
		# statement is "-" (meaning we read from stdin)
		input=""
		while read currline
		do
			if [[ "$currline" ]]
			then
				input="$input $currline"
			fi
		done
		
		{CXR_SQLITE_EXEC} "$db_file" < $(echo $input)
	elif [[ -r "$statement" ]]
	then
		# statement is a file, read from there
		${CXR_SQLITE_EXEC} "$db_file" < $(cat "$statement")
	else
		# Execute the string
		${CXR_SQLITE_EXEC} "$db_file" "$statement"
	fi
	
	# Relase Lock
	common.runner.releaseLock "$(basename $db_file)" "$type"
}

################################################################################
# Function: common.db.dump
#
# Dumps the given DB to a given file.
#
# Parameters:
# $1 - name of the db
# $2 - type of db, either "$CXR_TYPE_INSTANCE" , "$CXR_TYPE_GLOBAL" or "$CXR_TYPE_UNIVERSAL"
# $3 - output filename (remcommended extension is .sql)
################################################################################
function common.db.dump()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a db and a valid type and a filename as input, got $@"
	fi
	
	local db
	local type
	local output_file
	local db_file
	
	db="$1"
	type="$2"
	output_file="$3"

	# Work out the filename
	db_file="$(_common.db.getDbFile "$db" "$type")"
	
	${CXR_SQLITE_EXEC} "$db_file" ".backup $output_file"
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
	
	# Create a small test DB
	db=test
	type=$CXR_TYPE_UNIVERSAL
	db_file="$(_common.db.getDbFile "$db" "$type")"
	
	${CXR_SQLITE_EXEC} $db_file <<-EOT
	
	DROP TABLE test;
	
	CREATE TABLE test (a,b);
	
	INSERT INTO test (a,b) VALUES ('Hallo','Velo');
	EOT
	
	sqlfile=$(common.runner.createTempFile sql)
	
	cat <<-EOT > $sqlfile
	-- This is a simple test-select
	
	SELECT * FROM test;
	
	EOT

	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	# Pass SQL statement directly
	res="$(common.db.getResultSet $db $type "SELECT * FROM test;")"
	is $res Hallo${CXR_DELIMITER}Velo "common.db.getResultSet - simple parameter"
	
	res="$(common.db.getResultSet $db $type "SELECT * FROM test;" "," )"
	is $res Hallo,Velo "common.db.getResultSet - simple parameter, different delimiter"
	
	# Use file
	res="$(common.db.getResultSet $db $type "$sqlfile")"
	is $res Hallo${CXR_DELIMITER}Velo "common.db.getResultSet - file"
	
	res="$(common.db.getResultSet $db $type "$sqlfile" "," )"
	is $res Hallo,Velo "common.db.getResultSet - file, different delimiter"
	
	# Use here-doc
	res="$(common.db.getResultSet $db $type <<-EOT
	-- This is a simple test-select
	
	SELECT * FROM test;
	EOT)"
	
	is $res Hallo${CXR_DELIMITER}Velo "common.db.getResultSet - here-doc"
	
	res="$(common.db.getResultSet $db $type "," <<-EOT
	-- This is a simple test-select
	
	SELECT * FROM test;
	EOT)"
	
	is $res Hallo,Velo "common.db.getResultSet - here-doc, different delimiter"
	
	########################################
	# teardown tests if needed
	########################################
}
