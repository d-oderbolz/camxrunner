# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
#
# Version: $Id$ 
#
# These functions provide access to the sqlite DB and provide logging and error handling.
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################
# TODO: Reduce redundancy

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=10

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
# Function: common.db.init
#
# Performs version checks on all visible sqlite DBs. It also checks the integrity of 
# sqlite and if we can load the extensions. (Rember recompilation issue on lcsl4a)
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
# $1 - full-path to db_file
# $2 - either a statement, a filename or - indicating input from stdin
# [$3] - otional separator (default $CXR_DELIMITER)
################################################################################
function common.db.getResultSet()
################################################################################
{
	if [[ $# -lt 2 || $# -gt 3 ]]
	then
		main.dieGracefully "needs a db file and a statement (optional delimiter) as input, got $@"
	fi
	
	local db_file
	local statement
	local separator
	
	local currline
	local sqlfile
	
	db_file="$1"
	statement="$2"
	separator="${3:-$CXR_DELIMITER}"

	if [[ ! -r $db_file ]]
	then
		main.log -w "DB file $db_file not readable"
		return $CXR_RET_OK
	fi
	
	# We have our own error handler here
	set +e
	
	# Detect type of statement
	if [[ "$statement" == - ]]
	then
		# statement is "-" (meaning we read from stdin)
		# in this case, create a tempfile (bash does a similar thing, see <http://tldp.org/LDP/abs/html/here-docs.html>)
		sqlfile="$(common.runner.createTempFile sql)"
		
		# Fill stdin into file (there are probably more elegant ways...)
		while read currline
		do
			echo "$currline" >> "$sqlfile"
		done
		
		main.log -v "Executing this SQL on $db_file:\n$(cat $sqlfile)"
		${CXR_SQLITE_EXEC} -separator "${separator}" "$db_file" < "$sqlfile"
		
		if [[ $? -ne 0 ]]
		then
			main.dieGracefully "Error in SQL statement: $(cat $sqlfile)"
		fi
		
	elif [[ -f "$statement" ]]
	then
		# statement is a file, read from there
		main.log -v "Executing this SQL on $db_file:\n$(cat $statement)" 
		${CXR_SQLITE_EXEC} -separator "${separator}" "$db_file" < "$statement"
		
		if [[ $? -ne 0 ]]
		then
			main.dieGracefully "Error in SQL statement: $(cat $statement)"
		fi
		
	else
		# Execute the string
		main.log -v "Executing this SQL on $db_file:\n$statement" 
		${CXR_SQLITE_EXEC} -separator "${separator}" "$db_file" "$statement"
		
		if [[ $? -ne 0 ]]
		then
			main.dieGracefully "Error in SQL statement: $statement"
		fi
		
	fi
	
	# fail-on-error on
	if [[ $CXR_TEST_IN_PROGRESS == false ]]
	then
		set -e
	fi
}

################################################################################
# Function: common.db.change
#
# Use this function (procedure) for all SQL statements containing DML (INSERT,UPDATE,DELETE)
# or DDL (CREATE, ALTER, DROP) statements. A writelock is aqciured.
#
# Parameters:
# $1 - full-path to db_file
# $2 - level of the lock to acquire, either "$CXR_LEVEL_INSTANCE", "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
# $3 - either a statement, a filename or - indicating input from stdin
################################################################################
function common.db.change()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a db file, a level and a statement as input, got $@"
	fi
	
	local db_file
	local statement
	local level

	local currline
	local sqlfile
	
	db_file="$1"
	level="$2"
	statement="$3"
	
	# For security reasons, we lock all write accesses to any DB
	if [[ $(common.runner.getLock "$(basename $db_file)" "$level") == false ]]
	then
		main.dieGracefully "Could not get lock on $(basename $db_file)"
	fi
	
	# We have our own error handler here
	set +e
	
	# Detect type of statement
	if [[ "$statement" == - ]]
	then
		# statement is "-" (meaning we read from stdin)
		# in this case, create a tempfile (bash does a similar thing, see <http://tldp.org/LDP/abs/html/here-docs.html>)
		sqlfile="$(common.runner.createTempFile sql)"
		
		# Fill stdin into file (there are probably more elegant ways...)
		while read currline
		do
			echo "$currline" >> "$sqlfile"
		done
		
		main.log -v "Executing this SQL on $db_file:\n$(cat $sqlfile)"
		${CXR_SQLITE_EXEC} "$db_file" < "$sqlfile"
		
		if [[ $? -ne 0 ]]
		then
			common.runner.releaseLock "$(basename $db_file)" "$level"
			main.dieGracefully "Error in SQL statement: $(cat $sqlfile)"
		fi
		
	elif [[ -f "$statement" ]]
	then
		# statement is a file, read from there
		main.log -v "Executing this SQL on $db_file:\n$(cat $statement)" 
		${CXR_SQLITE_EXEC} "$db_file" < "$statement"
		
		if [[ $? -ne 0 ]]
		then
			common.runner.releaseLock "$(basename $db_file)" "$level"
			main.dieGracefully "Error in SQL statement: $(cat $statement)"
		fi
		
	else
		# Execute the string
		main.log -v "Executing this SQL on $db_file:\n$statement" 
		${CXR_SQLITE_EXEC} "$db_file" "$statement"
		
		if [[ $? -ne 0 ]]
		then
			common.runner.releaseLock "$(basename $db_file)" "$level"
			main.dieGracefully "Error in SQL statement: $statement"
		fi
		
	fi
	
	# Relase Lock
	common.runner.releaseLock "$(basename $db_file)" "$level"
	
	# fail-on-error on
	set -e
}

################################################################################
# Function: common.db.dump
#
# Dumps the given DB to a given file.
#
# Parameters:
# $1 - full-path to db_file
# $2 - output filename (recommended extension is .sql)
################################################################################
function common.db.dump()
################################################################################
{
	if [[ $# -ne 2 ]]
	then
		main.dieGracefully "needs a db file and a filename as input, got $@"
	fi
	
	local db_file
	local type
	local output_file

	db_file="$1"
	output_file="$2"

	${CXR_SQLITE_EXEC} "$db_file" ".dump" > $output_file
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
	db_file="$(common.runner.createTempFile sql)"
	
	${CXR_SQLITE_EXEC} $db_file <<-EOT
	
	DROP TABLE IF EXISTS test;
	DROP TABLE IF EXISTS x ;
	DROP TABLE IF EXISTS y ;
	
	CREATE TABLE test (a,b);
	
	INSERT INTO test (a,b) VALUES ('Hallo','Velo');
	EOT
	
	sqlfile="$(common.runner.createTempFile sql)"
	
	echo "-- This is a simple test-select" > $sqlfile
	echo "" >> $sqlfile
	echo "SELECT * FROM test;" >> $sqlfile
	
	ddlfile="$(common.runner.createTempFile sql)"
	echo "ALTER TABLE x RENAME to y;" > $ddlfile
	
	dumpfile="$(common.runner.createTempFile sqldump)"

	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	# The syntax here looks a little strange, 
	# that is only because I use here documents in $()
	# normally, no-one would do this, but its neat for testing.
	
	#################
	# ResultSet
	#################
	
	# Pass SQL statement directly
	res="$(common.db.getResultSet $db_file "SELECT * FROM test;")"
	is "$res" "Hallo${CXR_DELIMITER}Velo" "common.db.getResultSet - simple parameter"
	
	res="$(common.db.getResultSet $db_file "SELECT * FROM test;" "," )"
	is "$res" "Hallo,Velo" "common.db.getResultSet - simple parameter, different delimiter"
	
	# Use file
	res="$(common.db.getResultSet $db_file "$sqlfile")"
	is "$res" "Hallo${CXR_DELIMITER}Velo" "common.db.getResultSet - file"
	
	res="$(common.db.getResultSet $db_file "$sqlfile" "," )"
	is "$res" "Hallo,Velo" "common.db.getResultSet - file, different delimiter"
	
	# Use here-doc
	res="$(common.db.getResultSet $db_file "-" <<-EOT
	-- This is a simple test-select
	
	SELECT * FROM test;
	EOT)"
	
	is "$res" "Hallo${CXR_DELIMITER}Velo" "common.db.getResultSet - here-doc"
	
	res="$(common.db.getResultSet $db_file "-" "," <<-EOT
	-- This is a simple test-select
	
	SELECT * FROM test;
	EOT)"
	
	is "$res" "Hallo,Velo" "common.db.getResultSet - here-doc, different delimiter"
	
	#################
	# dump
	#################
	
	common.db.dump $db_file $dumpfile
	main.log -a "Contents of dump: $(cat $dumpfile)"
	
	test -s $dumpfile
	is "$?" "0" "common.db.dump - simple size check"
	
	#################
	# change
	#################
	# Pass SQL statement directly
	common.db.change $db_file $CXR_LEVEL_INSTANCE "CREATE TABLE x (a,b);" 
	is "$?" "0" "common.db.change - simple parameter"
	
	# Use file
	common.db.change $db_file $CXR_LEVEL_INSTANCE $ddlfile
	is "$?" "0" "common.db.change - use file"
	
	# Use here-doc
	common.db.change $db_file $CXR_LEVEL_INSTANCE "-" <<-EOT
	DROP TABLE y;
	EOT
	
	is "$?" "0" "common.db.change - here-doc"
	
	########################################
	# teardown tests if needed
	########################################
}
