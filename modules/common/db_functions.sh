# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
#
# Version: $Id$ 
#
# Title: Functions to provide access to the SQLite DB <http://www.sqlite.org> and provide logging and error handling.
# Note that sqlite (V 3.7.2) has the nasty habit of issuing "I/O Error" if the DB is locked. 
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
# Function: common.db.bootstrap
#
# Since the DB is a core functionality, it is needed even to start the installer.
# This function compiles sqlite, if needed.
#
################################################################################
function common.db.bootstrap()
################################################################################
{
	main.log -a "It seems that you have no sqlite available. I will do an ad-hoc compilation..."
	
	# We need to load installer stuff
	source $CXR_CENF_DIR/installer.conf
	
	src_dir=$CXR_SQLITE_SRC_DIR
	
	binary_name=${CXR_BIN_DIR}/${executable}-${HOSTTYPE}
				
	# We constantly add to this file
	logfile=${binary_name}.log
	
	echo "**** $(date) Compiling source in $src_dir on $(uname -n)...\n" | tee -a $logfile
	
	if [[ -L "$src_dir" ]]
	then
		echo "(a link to $(common.fs.getLinkTarget $src_dir))" | tee -a $logfile
	fi
	
	cd $src_dir || main.dieGracefully "Could not change to $src_dir"
	
	libdir=${CXR_LIB_DIR}/${executable}/$HOSTTYPE
	mkdir -p $libdir
	
	# Clean up whatever there was
	echo "make clean DESTINATION=${CXR_BIN_DIR} LIBDIR=${libdir} SUFFIX=${suffix}" | tee -a $logfile
	make clean DESTINATION="${CXR_BIN_DIR}" LIBDIR=${libdir} SUFFIX="${suffix}" | tee -a $logfile
	
	# Make it!
	echo "make DESTINATION=${CXR_BIN_DIR} LIBDIR=${libdir} SUFFIX=${suffix}" | tee -a $logfile
	make DESTINATION="${CXR_BIN_DIR}" LIBDIR=${libdir} SUFFIX="${suffix}" | tee -a $logfile 

	if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
	then
		main.dieGracefully "The compilation of $executable did not complete successfully"
	fi

}

################################################################################
# Function: common.db.init
#
# Performs version checks on all visible sqlite DBs. It also checks the integrity of 
# sqlite and if we can load the extensions.
# Performs vacuum and checks the integrity. 
#
################################################################################
function common.db.init()
################################################################################
{
	local x
	
	local -a directories
	local -a levels
	
	local dir
	local iDir
	local dbs
	local db_file
	local level
	local dropfile
	local creafile
	local mtime
	
	if [[ ${CXR_DB_TRY_TIMES:-0} -lt 1 ]]
	then
		main.dieGracefully "CXR_DB_TRY_TIMES ($CXR_DB_TRY_TIMES) must be at least 1, otherwise no DB operations will happen."
	fi
	
	main.log -a "Initialising databases & Housekeeping..."
	
	# Testing integrity of sqlite itself
	x=$(common.runner.createTempFile sqlite-test)
	
	# Tempfiles for rebuild
	dropfile=$(common.runner.createTempFile idx-drop)
	creafile=$(common.runner.createTempFile idx-crea)
	
	# turn off errexit
	set +e
	
	sql_version="$( ${CXR_SQLITE_EXEC} -version )"
	
	# Either we get no version string or maybe it is too low
	if [[ -z "sql_version" || $(common.math.compareVersions 3.7 $sql_version) -eq -1 ]]
	then
		# Failed, we try to bootstrap
		common.db.bootstrap
		# The name needs to be reset
		CXR_SQLITE_EXEC="$(main.getBinaryName sqlite3 true)"
		
		# Retry
		${CXR_SQLITE_EXEC} $x <<-EOT
		SELECT * FROM sqlite_master WHERE 1=2;
		EOT
		
		if [[ $? -ne 0 ]]
		then
			main.dieGracefully "It seems that the binary sqlite must be recompiled (./CAMxRunner -I) , even the simplest query failed."
		fi
	fi
	
	# Test if we have libfunctions
	if [[ -e $CXR_SQLITE_LIBFUNCTIONS ]]
	then
		CXR_SQLITE_LIBFUNCTIONS_SUPPORTED=true
		
		# Test it
		${CXR_SQLITE_EXEC} $x <<-EOT
		
		SELECT load_extension('$CXR_SQLITE_LIBFUNCTIONS');
		select cos(radians(45)) FROM sqlite_master WHERE 1=2;
		
		EOT
		
		if [[ $? -ne 0 ]]
		then
			CXR_SQLITE_LIBFUNCTIONS_SUPPORTED=false
		fi
		
	else
		CXR_SQLITE_LIBFUNCTIONS_SUPPORTED=false
	fi
	
	if [[ $CXR_TEST_IN_PROGRESS == false ]]
	then
		# Turn errexit back on, if needed
		set -e
	fi
	
	# Loop through directories/levels and files
	directories=($CXR_GLOBAL_DIR $CXR_UNIVERSAL_DIR)
	levels=($CXR_LEVEL_GLOBAL $CXR_LEVEL_UNIVERSAL)
	
	for iDir in $(seq 0 $(( ${#directories[@]} - 1 )) )
	do
	
		dir=${directories[$iDir]}
		level=${levels[$iDir]}
	
		if [[ -d "$dir" ]]
		then
			dbs="$(find ${dir}/ -noleaf -maxdepth 1 -name '*.sqlite')"
			
			for db_file in $dbs
			do
				# These checks are done onec per day
				mtime=$(common.fs.getMtime $db_file)
				
				if [[ $(( $CXR_START_EPOCH - 86400 )) -gt $mtime ]]
				then 
					main.log -a "DB ${db_file}:"
					
					common.runner.getLock "$(basename $db_file)" "$level"
					
					# Do some basic maintenance
					${CXR_SQLITE_EXEC} $db_file <<-EOT
					
					-- Get exclusive access
					PRAGMA locking_mode=EXCLUSIVE; 
		
					-- Check integrity
					PRAGMA integrity_check;
					
					-- Remove fragmentation
					VACUUM;
					
					EOT
					
					# Relase Lock
					common.runner.releaseLock "$(basename $db_file)" "$level"
					
					main.log -a "Rebuilding indexes..."
					common.db.getResultSet $db_file $level "SELECT 'DROP index ' || name || ';' FROM sqlite_master WHERE type='index';" > $dropfile
					common.db.getResultSet $db_file $level "SELECT sql || ';' FROM sqlite_master WHERE type='index';" > $creafile
					
					common.db.change $db_file $level $dropfile
					common.db.change $db_file $level $creafile
					
					main.log -a ""
					
					touch "$db_file"
				fi # 24 hours passed?
				
			done # db_files
			
		fi # does dir exist?
		
	done # directories

}

################################################################################
# Function: common.db.getResultSet
#
# Function that returns a resultset on stdout. 
# Do not use this function to alter the database - we aquire no writelock in CAMxRunner!
#
#
# On error, we try to re-execute the statement, because we fail if the DB is locked.
# (SQLite issues "I/O Error" but means "DB locked")
#
# Parameters:
# $1 - full-path to db_file
# $2 - level of the lock to test, either "$CXR_LEVEL_INSTANCE", "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
# $3 - either a statement, a filename or - indicating input from stdin
# [$4] - otional separator (default $CXR_DELIMITER)
################################################################################
function common.db.getResultSet()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a db file, a statement and a level for the share-lock (optional delimiter) as input, got $*"
	fi
	
	local db_file
	local level
	local statement
	local separator
	
	local currline
	local retval
	local result
	
	# This variable will hold the actual SQL we execute
	local sql
	sql=""
	
	db_file="$1"
	level="$2"
	statement="$3"
	
	# count number of trials
	trial=1
	retval=1
	
	separator="${4:-$CXR_DELIMITER}"

	if [[ ! -r $db_file ]]
	then
		main.log -w "DB file $db_file not readable"
		return $CXR_RET_OK
	fi

	# Detect type of statement
	if [[ "$statement" == - ]]
	then
		# statement is "-" (meaning we read from stdin)
		
		# Fill stdin into file (there are probably more elegant ways for this...)
		while read currline
		do
			sql="$sql
$currline"
		done
	elif [[ -f "$statement" ]]
	then
		# statement is a file, read from there
			sql="$sql
$(cat $statement)"
	else
		# Add string 
			sql="$sql
$statement"
	fi # type-of-statement
	
	# add ; in case it was forgotten
	sql="$sql 
;"
	
	main.log -v "Executing this SQL on $db_file:\n$sql"
	
	# Before accessing the DB, we wait for any writelocks 
	common.runner.waitForLock "$(basename $db_file)" "$level"
	
	# We have our own error handler here
	set +e
	
	# we retry $CXR_DB_RETRY_TIMES times
	while [[ $retval -ne 0 && $trial -le $CXR_DB_TRY_TIMES ]]
	do
		
		if [[ $trial -gt 1 ]]
		then
			main.log -w "Retrying SQL statement: $sql"
		fi
		
		result="$(${CXR_SQLITE_EXEC} -separator "${separator}" "$db_file" <<-EOT
		$(echo -e "$sql")
		EOT
		)"
			
		retval=$?

		trial=$(( $trial + 1 ))
		
		# We prolong the waiting time 
		sleep $(common.math.FloatOperation "$CXR_DB_RETRY_WAIT_SECONDS * $trial")
		
	done # retry loop

	# fail-on-error on
	if [[ ${CXR_TEST_IN_PROGRESS:-false} == false ]]
	then
		set -e
	fi
	
	if [[ $retval -ne 0 ]]
	then
		main.dieGracefully "Error in SQL statement ($db_file): $sql"
	fi
	
	echo "$result"
}

################################################################################
# Function: common.db.change
#
# Use this function  for all SQL statements containing DML (INSERT,UPDATE,DELETE)
# or DDL (CREATE, ALTER, DROP) statements. A writelock is acquired.
# DO NOT USE THIS TO QUERY THE DB (SELECT), use <common.db.getResultSet> for this purpose.
#
# On error, we try to re-execute the statement, because we fail if the DB is locked.
# (SQLite issues "I/O Error" but means "DB locked")
#
# We add pragmas in front of the statement:
# PRAGMA legacy_file_format = on; to ensure compatibility with older sqlite3 systems
#
# All statements are grouped into one single, immediate transaction (see <http://www.sqlite.org/lang_transaction.html>
# This means that the DB will rollback any changes in case of an error, and we can retry.
#
# Parameters:
# $1 - full-path to db_file
# $2 - level of the exclusive lock to acquire, either "$CXR_LEVEL_INSTANCE", "$CXR_LEVEL_GLOBAL" or "$CXR_LEVEL_UNIVERSAL"
# $3 - either a statement, a filename or - indicating input from stdin
# [$4] - boolean do_trx if false (default true), no transaction is started (use for example for .import)
################################################################################
function common.db.change()
################################################################################
{
	if [[ $# -lt 3 || $# -gt 4 ]]
	then
		main.dieGracefully "needs a db file, a level, a statement and an optinal do_trx as input, got $*"
	fi
	
	local db_file
	local statement
	local level

	local currline
	local trial
	local retval
	local result
	local do_trx
	
	db_file="$1"
	level="$2"
	statement="$3"
	do_trx="${4:-true}"
	
	# count number of trials
	trial=1
	retval=1
	
	# Add pragmas
	sql="PRAGMA legacy_file_format = on;
"
	
	# Start TRX if needed
	if [[ "$do_trx" == true ]]
	then
		sql="$sql
BEGIN IMMEDIATE TRANSACTION;"
	fi
	
	# Detect type of statement
	if [[ "$statement" == - ]]
	then
		# statement is "-" (meaning we read from stdin)
		
		# Fill stdin into variable
		while read currline
		do
			sql="$sql
$currline"
		done
	elif [[ -f "$statement" ]]
	then
			# statement is a file, read from there
			sql="$sql
$(cat $statement)"
	else
		# Add string
		sql="$sql
$statement"
	fi # type-of-statement
	
	# add ; in case it was forgotten
	sql="$sql 
;"
	
	# End TRX, if needed
	if [[ "$do_trx" == true ]]
	then
			sql="$sql
COMMIT TRANSACTION;"
	fi
	
	main.log -v "Executing this SQL on $db_file:\n$sql"
	
	# For security reasons, we lock all write accesses to any DB
	common.runner.getLock "$(basename $db_file)" "$level"
	
	# We have our own error handler here
	set +e
	
	# we retry $CXR_DB_RETRY_TIMES times
	while [[ $retval -ne 0 && $trial -le $CXR_DB_TRY_TIMES ]]
	do
		if [[ $trial -gt 1 ]]
		then
			main.log -w "Retrying SQL statement: $sql"
		fi
	
		result="$(${CXR_SQLITE_EXEC} "$db_file" <<-EOT
		$(echo -e "$sql")
		EOT
		)"
		
		retval=$?

		trial=$(( $trial + 1 ))
		
		# We prolong the waiting time 
		sleep $(common.math.FloatOperation "$CXR_DB_RETRY_WAIT_SECONDS * $trial")
	
	done # retry loop
	
	# Relase Lock
	common.runner.releaseLock "$(basename $db_file)" "$level"
	
	# fail-on-error on
	if [[ ${CXR_TEST_IN_PROGRESS:-false} == false ]]
	then
		set -e
	fi

	if [[ $retval -ne 0 ]]
	then
		# Even after retrying, we failed
		main.dieGracefully "Error in SQL statement ($db_file): $sql"
	fi
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
		main.dieGracefully "needs a db file and a filename as input, got $*"
	fi
	
	local db_file
	local type
	local output_file

	db_file="$1"
	output_file="$2"
	
	# We have our own error handler here
	set +e

	${CXR_SQLITE_EXEC} "$db_file" ".dump" > $output_file
	
	if [[ $? -ne 0 ]]
	then
			main.log -w "Could not dump DB $db_file!"
		fi
	
	if [[ ${CXR_TEST_IN_PROGRESS:-false} == false ]]
	then
		set -e
	fi
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
	res="$(common.db.getResultSet $db_file "$CXR_LEVEL_INSTANCE" "SELECT * FROM test;")"
	is "$res" "Hallo${CXR_DELIMITER}Velo" "common.db.getResultSet - simple parameter"
	
	res="$(common.db.getResultSet $db_file "$CXR_LEVEL_INSTANCE" "SELECT * FROM test;" "," )"
	is "$res" "Hallo,Velo" "common.db.getResultSet - simple parameter, different delimiter"
	
	# Use file
	res="$(common.db.getResultSet $db_file "$CXR_LEVEL_INSTANCE" "$sqlfile")"
	is "$res" "Hallo${CXR_DELIMITER}Velo" "common.db.getResultSet - file"
	
	res="$(common.db.getResultSet $db_file "$CXR_LEVEL_INSTANCE" "$sqlfile" "," )"
	is "$res" "Hallo,Velo" "common.db.getResultSet - file, different delimiter"
	
	# Use here-doc
	res="$(common.db.getResultSet $db_file "$CXR_LEVEL_INSTANCE" "-" <<-EOT
	-- This is a simple test-select
	
	SELECT * FROM test;
	EOT)"
	
	is "$res" "Hallo${CXR_DELIMITER}Velo" "common.db.getResultSet - here-doc"
	
	res="$(common.db.getResultSet $db_file "$CXR_LEVEL_INSTANCE" "-" "," <<-EOT
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
