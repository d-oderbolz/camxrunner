# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Title: Functions to check and change the state af a run
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# TODO: 
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=2

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the functions to manage the state Database of the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: common.state.isRepeatedRun?
#
# Returns true, if this run was already started earlier (If there are any Non-Todo Tasks)
# 
################################################################################
function common.state.isRepeatedRun?()
################################################################################
{
	local count
	
	count=$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT COUNT(*) FROM tasks WHERE status NOT IN ('$CXR_STATUS_TODO')")
	main.log -v  "Counted $count tasks that where no longer in status $CXR_STATUS_TODO"
	
	if [[ "$count" -gt 0 ]]
	then
		echo true
	else
		# No "touched" tasks found
		echo false
	fi
}

################################################################################
# Function: common.state.getLastDayOffsetModelled
#
# Returns the last day_offset known in the state DB. Returns -1 on error or
# if there is no data
# 
################################################################################
function common.state.getLastDayOffsetModelled()
################################################################################
{
	local result
	
	# We use the ID of a task to got the ISO date
	result=$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT MAX(d.day_offset) FROM tasks t, days d WHERE d.day_iso=substr(t.id,1,10);")
	
	if [[ -z "$result" ]]
	then
		result=-1
	fi
	
	echo "$result"
	
	return $CXR_RET_OK
}

################################################################################
# Function: common.state.getFirstDayModelled
#
# Returns the first day (ISO) known in the database
# 
################################################################################
function common.state.getFirstDayModelled()
################################################################################
{
	# Let the database tell it
	common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT MIN(day_iso) FROM days"
	
	return $CXR_RET_OK
}

################################################################################
# Function: common.state.deleteMyContinueFile
#
# Deletes the continue files of this instance
################################################################################
function common.state.deleteMyContinueFile()
################################################################################
{
	main.log -w  "The continue file of ${CXR_INSTANCE} will be deleted now!"
	
	rm -f ${CXR_CONTINUE_FILE} &> /dev/null
}

################################################################################
# Function: common.state.deleteAllContinueFiles
#
# Deletes the continue files of all instances
################################################################################
function common.state.deleteAllContinueFiles()
################################################################################
{
	main.log -w  "The continue files of all instances of this run will be deleted now!"
	
	find ${CXR_ALL_INSTANCES_DIR}/ -noleaf -name ${CXR_CONTINUE} -exec rm -f {} \; &> /dev/null
}


################################################################################
# Function: common.state.updateInfo
#
# Builds the tasks and dependencies, if needed.
# 
# Parameters:
# $1 - longer - if true, the run was prolonged at the end
# $2 - success_file, an sql file containig old, successful tasks
################################################################################
function common.state.buildTasksAndDeps()
################################################################################
{
	local longer
	local success_file
	
	longer="$1"
	success_file="$2"
	
	if [[ ( $longer == true || $(common.state.isRepeatedRun?) == false ) && $CXR_FIRST_INSTANCE == true ]]
	then
		# Only build all tasks if its a new run
		main.log -v "Building tasks and dependencies..."
		
		# Basically this product:
		# modules x days x invocations
		# Of course, we must treat the One-Timers separate
		
		common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" - <<-EOT
		
		--------------------------------------------------------------------
		-- TASKS
		--------------------------------------------------------------------
		
		DELETE FROM tasks;
		DELETE FROM dependencies;
		
		-- Daily modules
		INSERT 	INTO tasks (
						id,
						module,
						type,
						exclusive,
						invocation,
						status,
						epoch_m) 
		SELECT 	d.day_iso || '@' || m.module || '@' || i.value, -- This must be consistent with common.task.getId
						m.module,
						m.type,
						m.exclusive, 
						i.value as invocation,
						'$CXR_STATUS_TODO',
						$(date "+%s")
		FROM 		modules m, 
						days d, 
						metadata i 
		WHERE 	i.module=m.module AND i.field='INVOCATION' 
		  AND 	m.type IN ('$CXR_TYPE_PREPROCESS_DAILY','$CXR_TYPE_MODEL','$CXR_TYPE_POSTPROCESS_DAILY');

		-- One-Time preprocessors
		
		INSERT 	INTO tasks (
						id,
						module,
						type,
						exclusive,
						invocation,
						status,
						epoch_m) 
						SELECT 	'$CXR_START_DATE' || '@' || m.module || '@' || i.value,
										m.module,
										m.type,
										m.exclusive,
										i.value as invocation,
										'$CXR_STATUS_TODO',
										$(date "+%s")
						FROM 		modules m, 
										metadata i
						WHERE 	i.module=m.module AND i.field='INVOCATION'
						  AND 	m.type IN ('$CXR_TYPE_PREPROCESS_ONCE');
		
		-- One-Time postprocessors
		
		INSERT 	INTO tasks (
						id,
						module,
						type,
						exclusive,
						invocation,
						status,
						epoch_m) 
						SELECT 	'$CXR_STOP_DATE' || '@' || m.module || '@' || i.value,
										m.module,
										m.type,
										m.exclusive, 
										i.value as invocation,
										'$CXR_STATUS_TODO',
										$(date "+%s")
						FROM 		modules m, 
										metadata i 
						WHERE 	i.module=m.module AND i.field='INVOCATION' 
						  AND 	m.type IN ('$CXR_TYPE_POSTPROCESS_ONCE');
						  
		--------------------------------------------------------------------
		-- DEPENCENCIES. 
		-- It is important to understand that dependencies are
		-- NOT on invocation and therefore task level. Dependencies exist
		-- between tuples of (module,day_offset).
		-- We add all dependencies whether we run them or not.
		-- it is up to <common.task.createParallelDependencyList> or 
		-- <common.task.createSequentialDependencyLis> to hanlde incative dependencies
		-- a challenge is the fact that we must replicate daily dependencies on OT Pre
		-- tasks (requiring equality of the day_offsets returns too few dependencies)
		--------------------------------------------------------------------
		
		--
		-- dependencies on single modules, without -<n> predicate
		--
		INSERT 	INTO dependencies (
						independent_module, 
						independent_day_offset, 
						dependent_module, 
						dependent_day_offset)
						SELECT 	independent.module,
										independent_days.day_offset,
										dependent.module,
										dependent_days.day_offset
						FROM 		tasks dependent,
										tasks independent,
										days dependent_days,
										days independent_days,
										metadata meta
						WHERE		dependent_days.day_iso = substr(dependent.id,1,10)
						AND			independent_days.day_iso = substr(independent.id,1,10)
						AND			independent.module = meta.value
						AND			dependent.module = meta.module
						AND			((dependent_days.day_offset = independent_days.day_offset AND independent.type IS NOT '${CXR_TYPE_PREPROCESS_ONCE}')
										OR (independent.type = '${CXR_TYPE_PREPROCESS_ONCE}')) --If the dependency is on OT Pre, no restriction applies to the day_offset
						AND			dependent.invocation = 1
						AND			independent.invocation = 1
						AND 		meta.field='CXR_META_MODULE_DEPENDS_ON'
						AND 		meta.value NOT IN (SELECT type FROM types)
						AND 		meta.value NOT GLOB '*-[0-9]' ; -- Test for - followed by one digit

		-- dependencies on single modules, only -<n> predicate
		-- here we must be careful not to add dependcies on types with predicate
		-- thats why the subselect has a UNION
		INSERT 	INTO dependencies (
						independent_module, 
						independent_day_offset, 
						dependent_module, 
						dependent_day_offset)
						SELECT 	independent.module,
										independent_days.day_offset,
										dependent.module,
										dependent_days.day_offset
						FROM 		tasks dependent,
										tasks independent,
										days dependent_days,
										days independent_days,
										metadata meta
						WHERE		dependent_days.day_iso = substr(dependent.id,1,10)
						AND			independent_days.day_iso = substr(independent.id,1,10)
						AND     independent.module = substr(meta.value,1,length(meta.value)-2)
						AND			dependent.module = meta.module
						AND			independent_days.day_offset = dependent_days.day_offset - abs(substr(meta.value,-1,1)) -- we subtract the digit after the -. abs is used like a type cast
						AND			dependent.invocation = 1
						AND			independent.invocation = 1
						AND 		meta.field='CXR_META_MODULE_DEPENDS_ON'
						AND 		meta.value NOT IN (SELECT type FROM types) -- it should not be a type
						AND 		substr(meta.value,1,length(meta.value)-2) NOT IN (SELECT type FROM types) -- and no -<n> type
						AND 		meta.value GLOB '*-[0-9]' ;  -- Test for - followed by one digit

		--
		-- dependencies on whole types without - predicate
		--
		INSERT 	INTO dependencies (
						independent_module, 
						independent_day_offset, 
						dependent_module, 
						dependent_day_offset)
						SELECT 	independent.module,
										independent_days.day_offset,
										dependent.module,
										dependent_days.day_offset
						FROM 		tasks dependent,
										tasks independent,
										days dependent_days,
										days independent_days,
										metadata meta
						WHERE		dependent_days.day_iso = substr(dependent.id,1,10)
						AND			independent_days.day_iso = substr(independent.id,1,10)
						AND			dependent.module = meta.module
						AND			((dependent_days.day_offset = independent_days.day_offset AND independent.type IS NOT '${CXR_TYPE_PREPROCESS_ONCE}')
										OR (independent.type = '${CXR_TYPE_PREPROCESS_ONCE}')) --If the dependency is on OT Pre, no restriction applies to the day_offset
						AND			independent.type = meta.value -- Because we check for equality, -<n> is automatically excluded
						AND			dependent.invocation = 1
						AND			independent.invocation = 1
						AND 		meta.field='CXR_META_MODULE_DEPENDS_ON';

		--
		-- dependencies on whole types only with - predicate
		-- 
		INSERT 	INTO dependencies (
						independent_module,
						independent_day_offset,
						dependent_module, 
						dependent_day_offset)
						SELECT 	independent.module,
										independent_days.day_offset,
										dependent.module,
										dependent_days.day_offset
						FROM 		tasks dependent,
										tasks independent,
										days dependent_days,
										days independent_days,
										metadata meta,
										types t
						WHERE		dependent_days.day_iso = substr(dependent.id,1,10)
						AND			independent_days.day_iso = substr(independent.id,1,10)
						AND			dependent.module = meta.module
						AND			independent.type = t.type
						AND			independent_days.day_offset = dependent_days.day_offset - abs(substr(meta.value,-1,1)) -- we subtract the digit after the -. abs is used like a type cast
						AND			dependent.invocation = 1
						AND			independent.invocation = 1
						AND			meta.value GLOB '*-[0-9]'   -- Test for - followed by one digit
						AND 		t.type = substr(meta.value,1,length(meta.value)-2)  ; -- remove the -n at the end 
		EOT
		
		# resurrect old success data if needed
		if [[ $longer == true ]]
		then
			common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" $success_file
		fi
		
	else
		main.log -a "We do not replace existing task data. You can do this manually by running the -c option."
	fi 
	
}

################################################################################
# Function: common.state.updateInfo
#
# Goes through all available modules for the current model and version, and collects 
# vital information in the state DB.
# 
# tables that are persistent:
# - tasks
# - days 
# - installed
# - types (constant)
#
# tables that contain only whats relevant for the current configuration
# - modules
# - metadata
# - instance_tasks (each instance selects its own tasks)
# (there are others, but they are taken care of in <common.task.init>)
#
# Parameters:
# $1 - boolean force (default false), if true we do it even for a hollow run.
################################################################################
function common.state.updateInfo()
################################################################################
{
	# Increase global indent level
	main.increaseLogIndent
	
	local sqlfile
	local iIteration
	local dirs
	local dir
	local types
	local type
	local files
	local file
	local module
	local variant
	local wanted_variant
	local run_it
	local fctlist
	local list
	local metafiled
	local list
	local field
	local value
	local dependency
	local dep_arr
	local iDependency
	local longer
	local day
	local day_list
	local first
	local success_file
	local force
	
	force=${1:-false}
	
	# If we are hollow, we assume its ok
	if [[ $force == false && $CXR_HOLLOW == true ]]
	then
		main.log -a "This is not a real run, we assume module info is up-to-date"
		return $CXR_RET_OK
	else
		main.log -a "Collecting module information, might take a while..."
	fi
	
	success_file=/dev/null
	
	# In this list, we store all modules we must run
	local sql_module_list
	
	# Marker for a run that was extended at the end (more days)
	longer=false
	
	# Deal with -r by modifying the enabled lists
	if [[ "$CXR_RUN_LIST" ]]
	then
		# There are arguments
		# we reset all explicitly enabled modules
		# (-r means "run ONLY these modules")
		CXR_ENABLED_ONCE_PREPROC=""
		CXR_ENABLED_DAILY_PREPROC=""
		CXR_ENABLED_MODEL=""
		CXR_ENABLED_DAILY_POSTPROC=""
		CXR_ENABLED_ONCE_POSTPROC=""
		
		CXR_DISABLED_ONCE_PREPROC="${CXR_SKIP_ALL}"
		CXR_DISABLED_DAILY_PREPROC="${CXR_SKIP_ALL}"
		CXR_DISABLED_MODEL="${CXR_SKIP_ALL}"
		CXR_DISABLED_DAILY_POSTPROC="${CXR_SKIP_ALL}"
		CXR_DISABLED_ONCE_POSTPROC="${CXR_SKIP_ALL}"
		
		
		# Decode arguments
		for module in $CXR_RUN_LIST
		do
			# It might be a type!
			if [[ "$(common.module.isType $module)" == true ]]
			then
				main.log -a "We will run all modules of type $module"
				# Yep, its a type
				# Exchange roles
				module_type=$module
				# The modules must be derived from the tpe
				module="$(common.module.resolveType $module_type)"
			else 
				# no, normal module name
				# Determine type. 
				# Due to CATCH-22 we cannot use <common.module.getType> here
				module_type="$(common.module.getTypeSlow "$module")"
			fi  # is it a type?
			
			case $module_type in
			
				${CXR_TYPE_PREPROCESS_ONCE} ) 
					CXR_ENABLED_ONCE_PREPROC="$CXR_ENABLED_ONCE_PREPROC $module"
					;;
					
				${CXR_TYPE_PREPROCESS_DAILY} ) 
					CXR_ENABLED_DAILY_PREPROC="$CXR_ENABLED_DAILY_PREPROC $module"
					;;
					
				${CXR_TYPE_MODEL} ) 
					CXR_ENABLED_MODEL="$CXR_ENABLED_MODEL $module"
					;;
					
				${CXR_TYPE_POSTPROCESS_DAILY} ) 
					CXR_ENABLED_DAILY_POSTPROC="$CXR_ENABLED_DAILY_POSTPROC $module"
					;;
					
				${CXR_TYPE_POSTPROCESS_ONCE} ) 
					CXR_ENABLED_ONCE_POSTPROC="$CXR_ENABLED_ONCE_POSTPROC $module"
					;;
					
				* ) main.dieGracefully "Module type $module_type not supported to be used with the -r option" ;;
			esac

		done # entries in -r
	
	fi # are there -r arguments?

	# For performance reasons, we collect all SQL change statements in a file and
	# execute it later (if CXR_FIRST_INSTANCE is true!)
	
	sqlfile=$(common.runner.createTempFile sql-updateInfo)
	
	# db_functions put a whole file into one TRX
	
	main.log -a "Cleaning Non-Persistent tables..."
	
	echo "DELETE FROM modules;" > $sqlfile
	echo "DELETE FROM metadata;" >> $sqlfile
	
	# Create a few working arrays we will go through
	types=($CXR_TYPE_PREPROCESS_ONCE \
	       $CXR_TYPE_PREPROCESS_DAILY \
	       $CXR_TYPE_MODEL \
	       $CXR_TYPE_POSTPROCESS_DAILY \
	       $CXR_TYPE_POSTPROCESS_ONCE)
	       
	dirs=($CXR_PREPROCESSOR_ONCE_INPUT_DIR \
	      $CXR_PREPROCESSOR_DAILY_INPUT_DIR \
	      $CXR_MODEL_INPUT_DIR \
	      $CXR_POSTPROCESSOR_DAILY_INPUT_DIR \
	      $CXR_POSTPROCESSOR_ONCE_INPUT_DIR)

	# For the syntax of the sql_module_list
	first=true
	
	for iIteration in $(seq 0 $(( ${#dirs[@]} - 1 )) )
	do
		type=${types[$iIteration]}
		dir=${dirs[$iIteration]}
		
		common.user.showProgress
		
		# The enabled and disabled strings are lists
		case "$type" in
			"${CXR_TYPE_PREPROCESS_ONCE}" )
				enabled_modules="$CXR_ENABLED_ONCE_PREPROC"
				disabled_modules="$CXR_DISABLED_ONCE_PREPROC"
				;;
				
			"${CXR_TYPE_PREPROCESS_DAILY}" )
				enabled_modules="$CXR_ENABLED_DAILY_PREPROC"
				disabled_modules="$CXR_DISABLED_DAILY_PREPROC"
				;;
				
			"${CXR_TYPE_MODEL}" )
				enabled_modules="$CXR_ENABLED_MODEL"
				disabled_modules="$CXR_DISABLED_MODEL"
				;;
				
			"${CXR_TYPE_POSTPROCESS_DAILY}" )
				enabled_modules="$CXR_ENABLED_DAILY_POSTPROC"
				disabled_modules="$CXR_DISABLED_DAILY_POSTPROC"
				;;
				
			"${CXR_TYPE_POSTPROCESS_ONCE}" )
				enabled_modules="$CXR_ENABLED_ONCE_POSTPROC"
				disabled_modules="$CXR_DISABLED_ONCE_POSTPROC"
				;;
				
			* ) 
				main.dieGracefully "The module type $type is not relevant here" ;;
		esac
		
		main.log -v "Adding $type modules..."
		
		if [[ -d "$dir" ]]
		then
			# Find all *.sh files in this dir (no descent!)
			files="$(find "${dir}/" -noleaf -maxdepth 1 -name '*.sh')"

			for file in $files
			do
				
				common.check.reportMD5 $file
				
				# Determine name and variant of the current file
				module="$(main.getModuleName $file)"
				variant="$(main.getModuleVariant $file)"

				# Did the user select a variant? 
				# Returns the empty string if not set
				wanted_variant=$(common.conf.get "${module}.variant")
				
				main.log -v "variant of $file is $variant (needed: $wanted_variant)"
				
				if [[ "$variant" == "$wanted_variant" ]]
				then
				
					if [[ "$wanted_variant" ]]
					then
						main.log -v "Considering variant $wanted_variant of $module"
					fi
				
					# Is this module active? (Enabled wins over disabled)
					# if the module name is in the enabled list, run it,no matter what
					if [[ "$(main.isSubstringPresent? "$enabled_modules" "$module")" == true ]]
					then
						# Module was explicitly enabled
						run_it=true
					elif [[  "$(main.isSubstringPresent? "$disabled_modules" "$module")" == false && "${disabled_modules}" != "${CXR_SKIP_ALL}" ]]
					then
						# Module was not explicitly disabled and we did not disable all
						run_it=true
					else
						# If the name of the module is in the disabled list, this should not be run (except if it was found in the enabled list)
						run_it=false
						main.log -a "  Module $module is disabled."
					fi
					
					# find out if we have a function called getNumInvocations
					fctlist=$(grep 'getNumInvocations' $file)
					
					if [[ -z "$fctlist" ]]
					then
						main.dieGracefully "Module file $file does not implement the function getNumInvocations()!\nCheck the developer documentation!"
					else
						# Here we need to source the file to call getNumInvocations()
						nInvocations=$(source $file &> /dev/null; getNumInvocations)
						
						# A module can disable itself
						if [[ $nInvocations -lt 1 ]]
						then
							main.log -a "Module $module returned 0 needed invocations. Probably the configuation is not compatible with this module."
							run_it=false
						fi
					fi
					
					# Add $file, $module and $type to DB
					echo "INSERT INTO modules (module,type,path) VALUES ('$module','$type','$file');" >> $sqlfile
					
					if [[ $run_it == true ]]
					then
						main.log -a "Planning to run ${module}..."
					
						if [[ $first == true ]]
						then
							sql_module_list="\'${module}\'"
							first=false
						else
							sql_module_list="${sql_module_list}, \'${module}\'"
						fi # first one?
					fi # running it?
					
					# Add metadata, unset old meta vars first
					unset ${!CXR_META_MODULE*}
					
					source $file
					
					# Check if we meet requirements
					if [[ $(main.CheckModuleRequirements "${module}" "${CXR_META_MODULE_DESCRIPTION:-}" "${CXR_META_MODULE_DOC_URL:-}" "${CXR_META_MODULE_AUTHOR:-}" "${CXR_META_MODULE_NUM_TESTS:-0}" "${CXR_META_MODULE_REQ_SPECIAL:-}") == false ]]
					then
						main.log -e "Requirements for module $module are not met!"
					fi
					
					# grep the CXR_META_ vars out of environment
					list=$(set | grep '^CXR_META_[_A-Z]\{1,\}=.*')
					
					oIFS="$IFS"
					# Set IFS to newline
					IFS='
'
					for metafield in $list
					do
						# Reset IFS immediately
						IFS="$oIFS"
						
						# Parse this
						# Field is to the left of the = sign
						field="$(expr match "$metafield" '\([_A-Z]\{1,\}\)=')" || :
						# the value is to the right
						value="$(expr match "$metafield" '.*=\(.*\)')" || :
						
						# OK, we want all quoting gone and variables expanded
						value="$(eval "echo $(echo "$value")")"

						# Treat some special Meta fields
						if [[ $field == CXR_META_MODULE_DEPENDS_ON ]]
						then
							# Make sure IFS is correct!
							for dependency in $value
							do
								echo "INSERT INTO metadata (module,field,value) VALUES ('$module','$field','$dependency');" >> $sqlfile
							done
						elif [[ $field == CXR_META_MODULE_RUN_EXCLUSIVELY ]]
						then
							echo "UPDATE modules SET exclusive=trim('$value') WHERE module='$module';" >> $sqlfile
						else
							# Nothing special
							echo "INSERT INTO metadata (module,field,value) VALUES ('$module','$field','$value');" >> $sqlfile
						fi # is it a special meta field
					done
					
					# Now also add each invocation as individial row.
					for iInvocation in $(seq 1 $nInvocations)
					do
						echo "INSERT INTO metadata (module,field,value) VALUES ('$module','INVOCATION','$iInvocation');" >> $sqlfile
					done
				
				else
				
					main.log -v "$file does not match variant $wanted_variant"
				
				fi # Right variant?
				
			done # Loop over files
		else
			main.dieGracefully "Tried to add modules in $dir - directory not found."
		fi # Directory exists?
	done # loop over type-index
	
	# Adding any new module types
	echo "INSERT OR IGNORE INTO types (type) SELECT DISTINCT value FROM metadata where field='CXR_META_MODULE_TYPE';" >> $sqlfile
	
	main.log -v "Removing any traces of running/failed tasks..."
	echo "UPDATE tasks SET status='$CXR_STATUS_TODO' WHERE status in ('$CXR_STATUS_RUNNING','$CXR_STATUS_FAILURE');" >> $sqlfile
	
	# Execute the file
	common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" $sqlfile
	
	# Check if any module is called the same as a type (not allowed)
	if [[ $(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT COUNT(*) FROM modules m, types t WHERE m.module=t.type") -gt 0 ]]
	then
		main.dieGracefully "At least one module has the same name as a module type - this is not supported!"
	fi
	
	# Check if a module name or a type looks like a -<n> dependency
	if [[ $(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT SUM(cnt) FROM (SELECT COUNT(*) AS cnt FROM modules WHERE module GLOB '*-[0-9]'  UNION ALL SELECT COUNT(*) AS cnt FROM types WHERE type GLOB '*-[0-9]')" ) -gt 0 ]]
	then
		main.dieGracefully "It seems that a type or a module has a name that ends in - followed by a number.\nThis is the syntax of the -<n> predicate for dependencies and not supported for names!"
	fi
	
	main.log -v "Adding information about simulation days..."
	
	# Is this a repetition of an earlier run?
	if [[ $(common.state.isRepeatedRun?) == true ]]
	then
		main.log "This run has already been started earlier."
		
		# Its dangerous if a run has been extended at the beginning
		# because the mapping of offset to days changed.
		first="$(common.state.getFirstDayModelled)"
		
		# first could be empty
		if [[ "$first" ]]
		then
			if [[ "$first" != ${CXR_START_DATE} ]]
			then
				main.dieGracefully "It seems that this run was extended at the beginning. This implies that the existing mapping of simulation days and real dates is broken.\nClean the state DB by running the -c (all) option or create a new run!"
			fi
		fi
		
		# Returns -1 on error
		last="$(common.state.getLastDayOffsetModelled)"
		
		# last could be empty
		if [[ "$last" ]]
		then
			if [[ $last -gt -1 && ${CXR_NUMBER_OF_SIMULATION_DAYS} -gt "$last" ]]
			then
				main.log "It seems that the number of simulation days increased since the last run. Make sure you repeat all needed steps (e. g. AHOMAP/TUV)"
				longer=true
				
				# we need to safe the IDs of all tasks that have status then CXR_STATUS_SUCCESS
				success_file=$(common.runner.createTempFile sql-success)
				
				# we build our SQL statements
				# Don't get confused, '' is an escaped ' (see <http://sqlite.org/lang_expr.html>)
				common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT 'UPDATE tasks SET status=''$CXR_STATUS_SUCCESS'' WHERE id=''' || id || '''' || ';' FROM tasks WHERE status='$CXR_STATUS_SUCCESS';" > $success_file

			fi # was the run extended at the end?
		fi # is there a last?
		
	fi # repeated run?
	
	# Replace the days
	echo "DELETE FROM days;" >> $sqlfile
	
	for iOffset in $(seq 0 $(( ${CXR_NUMBER_OF_SIMULATION_DAYS} - 1 )) )
	do
		echo  "INSERT INTO days (day_offset,day_iso) VALUES ($iOffset,'$(common.date.OffsetToDate $iOffset)');" >> $sqlfile
	done
	
	# Only update info (except instance_tasks) if we are the first instance
	if [[ $CXR_FIRST_INSTANCE == false ]]
	then
		# We are in a non-master runner
		main.log -a -b "We are not the first instance - we will not collect new information"
	else
		# Execute the file
		common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" $sqlfile
		
		# Create the tasks
		common.state.buildTasksAndDeps $longer $success_file
	fi #first?
	
	####################################
	# instance_tasks:
	# -Add all needed modules
	# -Delete any unwanted days from instance_tasks
	####################################
	
	common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" - <<-EOT
	
		DELETE FROM instance_tasks WHERE instance='$CXR_INSTANCE';
	
		INSERT INTO instance_tasks (id, instance)
		SELECT id, '$CXR_INSTANCE' FROM tasks WHERE
		module in ($sql_module_list);
	
	EOT
	
	if [[ "$CXR_SINGLE_DAYS" ]]
	then
		first=true
		# Create a day list
		for day in $CXR_SINGLE_DAYS
		do
			if [[ $first == true ]]
			then
				day_list="\'${day}\'"
				first=false
			else
				day_list="${day_list}, \'${day}\'"
			fi
		done
	
		common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" - <<-EOT
		
		DELETE FROM instance_tasks WHERE
		instance='$CXR_INSTANCE'
		AND id IN 
		  (SELECT id FROM tasks t WHERE substr(id,1,10) NOT IN ($day_list)
		  );
		
		EOT
	fi # do we want to run anly single days?
	
	main.log -a "Module data successfully collected."
	
	# decrease global indent level
	main.decreaseLogIndent
}

################################################################################
# Function: common.state.init
#
# Creates the state database for the current run
################################################################################
function common.state.init()
################################################################################
{
	if [[ -z "${CXR_STATE_DIR}" ]]
	then
		main.dieGracefully  "CXR_STATE_DIR not set!"
	fi
	
	# Create directory where we decompress files into
	CXR_TMP_DECOMP_DIR="$(common.runner.createTempDir decomp false)"
	
	# Creating .continue file
	echo "Creating the file ${CXR_CONTINUE_FILE}. If this file is deleted, the process stops as soon as possible." 1>&2
	echo "If you remove this file, the instance ${CXR_PID} on $(uname -n) will stop" > ${CXR_CONTINUE_FILE}
	
	main.log -v "Creating database schema in $CXR_STATE_DB_FILE..."
	
	# Do not fail if db_file is empty
	common.db.change "$CXR_UNIVERSAL_TIMING_DB" "$CXR_LEVEL_UNIVERSAL" - true false <<-EOT
		
		CREATE TABLE IF NOT EXISTS timing (	model 	TEXT,
																				version	TEXT,
																				module TEXT,
																				problem_size INTEGER,
																				machine	TEXT,
																				ReaLoad	NUMERIC,
																				elapsed_seconds NUMERIC,
																				epoch_m				  INTEGER,
																				run	TEXT);
		
	EOT
	
	main.log -v "Creating database schema in $CXR_STATE_DB_FILE..."
	
	# Do not fail if db_file is empty
	common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" - true false <<-EOT
	
	-- This is a "Oracle like" Dummy table 
	CREATE TABLE IF NOT EXISTS dual (dummy TEXT);
	DELETE FROM dual;
	INSERT INTO dual (dummy) VALUES ('X');
	
	-- Here we store all known simulation days
	CREATE TABLE IF NOT EXISTS days (day_offset INTEGER,
	                                 day_iso 		TEXT);

	-- All modules go here
	CREATE TABLE IF NOT EXISTS modules (module		TEXT, 
	                                    type			TEXT,
	                                    path			TEXT,
	                                    exclusive	TEXT);
	                                    
	CREATE UNIQUE INDEX IF NOT EXISTS module_idx ON modules(module);

	-- List of module types
	CREATE TABLE IF NOT EXISTS types (type	TEXT);
	CREATE UNIQUE INDEX IF NOT EXISTS type_idx ON types(type);
	
	-- Storage of module metadata
	CREATE TABLE IF NOT EXISTS metadata (module	TEXT,
	                                     field	TEXT,
	                                     value	TEXT);

	-- Table to store dependencies
	CREATE TABLE IF NOT EXISTS dependencies (independent_module			TEXT, 
	                                         independent_day_offset	TEXT, 
	                                         dependent_module				TEXT, 
	                                         dependent_day_offset		TEXT);
	
	-- Table for all tasks comprising a run (static)
	CREATE TABLE IF NOT EXISTS tasks (id								TEXT,
	                                 rank								INTEGER,
	                                 module							TEXT,
	                                 type								TEXT,
	                                 exclusive					TEXT,
	                                 invocation					INTEGER,
	                                 status							TEXT,
	                                 seconds_estimated	INTEGER,
	                                 seconds_real				INTEGER,
	                                 epoch_m						INTEGER);
	                                 
	-- Table Where an Instance selects relevant tasks
	CREATE TABLE IF NOT EXISTS instance_tasks (instance	TEXT,
	                                           id				TEXT); --task
	
	-- Table for workers
	CREATE TABLE IF NOT EXISTS workers (pid							INTEGER,
	                                    instance				TEXT,
	                                    status					TEXT,
	                                    current_task		TEXT,
	                                    epoch_m					INTEGER);
	
	EOT
	
	# Update the module path hash and form the lists of active modules
	common.state.updateInfo
}

################################################################################
# Function: common.state.storeStatus
#
# Stores the current status of a run, can be either $CXR_STATUS_RUNNING, $CXR_STATUS_SUCCESS or $CXR_STATUS_FAILURE.
# In the case of ERROR, we will take note of the fact.
#
# If a task starts, checks if this task was already executed.
# The name of the task is determined via <common.task.getId> if it is not passed in.
# The ability to pass a task name is used by installers and tests alike, they use a slightly different approach.
#
# If the user wants to run a specific module, we will disregard the fact that
# the step already ran, but we advise -F
#
# Returns:
# 	$CXR_RET_OK if OK (true)
# 	$CXR_RET_ALREADY_RUN if this task was already started (false)
#
# Parameters:
# $1 - status ($CXR_STATUS_RUNNING, $CXR_STATUS_SUCCESS or $CXR_STATUS_FAILURE.)
################################################################################
function common.state.storeStatus()
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "needs a state like $CXR_STATUS_RUNNING, $CXR_STATUS_SUCCESS or $CXR_STATUS_FAILURE as Input"   
	fi
	
	local status
	local task
	
	status=$1
	
	# Get a nice string describing where we are
	task=$(common.task.getId)
	
	
	# Set the current state data
	module="${CXR_META_MODULE_NAME}"
	day_offset="${CXR_DAY_OFFSET}"
	day_iso="${CXR_DATE}"
	invocation="${CXR_INVOCATION:-1}"
	
	# Do we care at all?
	# Set CXR_ENABLE_STATE_DB to false in tests etc.
	if [[ "$CXR_ENABLE_STATE_DB" == false ]]
	then
		main.log -v "You disabled the state DB (CXR_ENABLE_STATE_DB=false), new status will not be stored."
		echo true
		return $CXR_RET_OK
	fi
	
	case "$status" in
	
		"$CXR_STATUS_RUNNING") 
			# Check if this was already started
			if [[ $(common.state.hasFinished? "$module" "$day_iso" "$invocation") == true ]]
			then
				if [[ "$CXR_RUN_LIST" ]]
				then
					# Ran already, but user wants to run specifically this
					main.log -w  "Task $task was already started, but since you requested this specific module, we run it. If it fails try to run \n \t ${CXR_CALL} -F \n to remove existing output files."
					echo true
				else
					# Oops, this task was already started	
					main.log -w  "Task $task was already started, therefore we do not run it. To clean the state database, run \n \t ${CXR_CALL} -c \n and rerun."
					
					# false means already run
					echo false
					return $CXR_RET_ALREADY_RUN
				fi 
			else
				echo true
			fi
			;;
	
			"$CXR_STATUS_SUCCESS")
				main.log "Task $task successfully completed."
				echo true
				;;
	
			"$CXR_STATUS_FAILURE")
				# Set the global status
				CXR_STATUS=$CXR_STATUS_FAILURE
				main.log -e "An error has occured during the execution of task $task!"
				echo false
			;;
			
			*)
				CXR_STATUS=$CXR_STATUS_FAILURE
				main.dieGracefully "Unknown state $status given"
				echo false
				return $CXR_RET_ERROR
			;;
	esac
	
	# Update the database
	common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "UPDATE tasks set status='$status' WHERE module='$module' AND substr(id,1,10)='$day_iso' AND invocation=$invocation ;"
	
	return $CXR_RET_OK
}

################################################################################
# Function: common.state.isInstanceAlive?
#
# Tests if a given instance is alive by checking the alive files or looking at the processes
#
# Parameters:
# $1 - an instance name of teh form PID@Machine
################################################################################
function common.state.isInstanceAlive?()
################################################################################
{
	local instance
	local pid
	local machine
	local alive_file
	local mtime
	local current_time
	local alive
	
	instance="$1"
	alive=false
	
	pid=$(cut $instance -d'@' -f1)
	machine=$(cut $instance -d'@' -f2)
	
	if [[ $machine == $CXR_MACHINE ]]
	then
		# local, test process
		
		# kill returns non-zero if process is gone, 0 is pseudo signal 
		# we avoid termination with || :
		kill -0 $pid &> /dev/null || :
	
		if [[ $? -eq 0 ]]
		then
			alive=true
		fi
		
	else
		# remote, check alive file
		alive_file=${CXR_ALL_INSTANCES_DIR}/$instance/${CXR_ALIVE}
		if [[ -e $alive_file ]]
		then
			mtime=$(common.fs.getMtime $alive_file)
			# We allow an age difference of up to 10 times CXR_WAITING_SLEEP_SECONDS
			delta=$(( 10 * $CXR_WAITING_SLEEP_SECONDS))
			
			current_time="$(date "+%s")"
			
			if [[ $(( $current_time - $delta )) -le $mtime ]]
			then
				# Alive file is younger than the current time minus delta
				alive=true
			fi
		fi # alive file around?
		
	fi # local?
		
	echo $alive
}

################################################################################
# Function: common.state.countInstances
#
# Counts living CAMxRunners.
#
################################################################################
function common.state.countInstances()
################################################################################
{
	local instances
	local instance
	local count

	count=0
	
	instances=$(find "${CXR_ALL_INSTANCES_DIR}/" -type d -maxdepth 1 -noleaf -name ${CXR_CONTINUE} 2>/dev/null)
	
	for instance in $instances
	do
		
		instance=$(basename $instance)

		if [[ $(common.state.isInstanceAlive? $instance) == true ]]
		then
			count=$(( $count + 1 ))
		else
			main.log -w "It seems that $instance is inactive and can be deleted"
		fi
		
	done
	
	echo $count
}

################################################################################
# Function: common.state.detectInstances
#
# Check if there are still living processes and warn
#
################################################################################
function common.state.detectInstances()
################################################################################
{
	local process_count
	process_count=$(common.state.countInstances) 
	
	if [[ ${process_count} -ne 0 ]]
	then
		# There are other instances running
		main.log -a "Found other instances - maybe these processes died or they are still running:\n(Check their age!)"
		
		find "${CXR_ALL_INSTANCES_DIR}/" -noleaf -type d -maxdepth 1 2>/dev/null | tee -a ${CXR_LOG}
		
		main.log -e "Check manually if the processes still run, if not clean the state db by runnig \n\t ${CXR_CALL} -c \n"    
	fi
}

################################################################################
# Function: common.state.hasFinished?
#	
# Checks if a specific task has finished.
#
# Parameters:	
# $1 - module name
# $2 - day_iso
# $3 - invocation
################################################################################
function common.state.hasFinished?()
################################################################################
{
	if [[ $# -ne 3 ]]
	then
		main.dieGracefully "needs a module, a day_iso and a invocation as input" 
		echo false
	fi
	
	local module
	local day_iso
	local invocation
	local task
	local status
	
	module="$1"
	day_iso="$2"
	invocation="$3"
	
	task="$(common.task.getId $module $day_iso $invocation)"
	
	main.log -v "Testing if task ${task} is done..."
	
	status=$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT status FROM tasks WHERE module='$module' AND substr(id,1,10)='$day_iso' AND invocation=$invocation")
	
	case $status in
	
		$CXR_STATUS_SUCCESS) 
			echo true 
			;;
			
		$CXR_STATUS_FAILURE) 
			main.log -w "Task $task failed."
			echo false
			;;
		
		$CXR_STATUS_RUNNING)
			main.log -v "Task $task is marked as still running."
			echo false
			;;
			
		$CXR_STATUS_TODO)
			main.log -v "Task $task has not started yet."
			echo false
			;;
		
		*) 
			main.log -e "Task $task has unknown status: $status"
			echo false
			;;
	esac
}

################################################################################
# Function: common.state.cleanup
#	
# Interactive function. Depending on user selection:
# - Deletes all state information
# - Modifies only part of the state information
# - shows MD5 information
# All is in a endless loop so one can quickly modify a lot of stuff
#
# 
################################################################################
function common.state.cleanup()
################################################################################
{
	local what
	local what_detail
	local days
	local which_day
	local module_types
	local num_module_types
	local steps
	local which_step
	local message
	local start_offset
	local stop_offset
	local iOffset
	local current_date
	local instance
	local instances 
	local binstance
	
	message="Do you want to access the state database?"
	
	while [[ "$(common.user.getOK "$message" )" == true ]]
	do
		# Fix the message
		message="Do you want to further access the state database?"
		
		# what do you want?
		what=$(common.user.getMenuChoice "Which part of the state database do you want to modify/access (none exits this function)?\nNote that you might need to delete output files in order to repeat a run, or run with ${CXR_CALL} -F (overwrite existing files)" "dump release-all-locks modify-tasks delete-all-tasks file-checksums delete-old-instances none" "none")
		
		case "$what" in
		
			dump)
				if [[ "$(common.user.getOK "Do you really want to dump the content of the state database to a file?" )" == true ]]
				then
					# Dumping the content of the state DB
					ofile="$(common.user.getInput "Which file (whole path) should I dump to? (.sql extension recommended)")" 
					
					if [[ ! -e "$ofile" ]]
					then
						common.db.dump "$CXR_STATE_DB_FILE" $ofile
					else
						main.dieGracefully "File $ofile already exists!"
					fi
				
				fi
				;;
		
			release-all-locks)
				if [[ "$(common.user.getOK "Do you really want to delete all lockfiles stored under ${CXR_STATE_DIR}?" )" == false ]]
				then
					# No 
					main.log -a "Will not modify any state information"
				else
					find ${CXR_STATE_DIR}/ -noleaf -name '*.lock' -exec rm {} \; 2>/dev/null
				fi
				;;
		
			delete-all-tasks)
				# Do we do this?
				if [[ "$(common.user.getOK "Do you really want to delete the whole state database ${CXR_STATE_DB_FILE}?" )" == false  ]]
				then
					# No 
					main.log -a "Will not modify any state information"
				else
					# Yes
					rm -f ${CXR_STATE_DB_FILE}
					main.log -a "Done."
				fi #Delete?
				
				;; # delete-all-tasks
				
			file-checksums)
			
				if [[ "$(common.user.getOK "This option allows you to query checksums (MD5) of files. Continue?" )" == true  ]]
				then
					common.state.checksumInterface
				fi

				;; # file-checksums
			
			delete-old-instances)
			
				# Do we do this?
				if [[ "$(common.user.getOK "Do you really want to remove old instances?" )" == false  ]]
				then
					# No 
					main.log -a   "Will not modify any state information"
					return 0
				else
					instances=$(find "${CXR_ALL_INSTANCES_DIR}/" -noleaf -type d -maxdepth 1 -name ${CXR_CONTINUE} 2>/dev/null)
	
					for instance in $instances
					do
						binstance=$(basename $instance)
				
						if [[ $(common.state.isInstanceAlive? $binstance) == false ]]
						then
							rm -rf $instance
						else
							main.log -a "$binstance is active"
						fi
					done
					
					main.log -a "Done."
				fi #Delete?
			
				;; # delete-old-instances
					
			modify-tasks)
			
				
				if [[ "$(common.user.getOK "Do you want to prevent the tasks you will select from running by marking them as SUCCESS? (If not, they will be marked as TODO and hence be rerun)" N )" == true ]]
				then
					new_status=$CXR_STATUS_SUCCESS
				else
					new_status=$CXR_STATUS_TODO
				fi
				
				# First, we build a select statement,
				# then we execute it (SELECT & UPDATE) one-by-one
				
				# Defaults
				confirm_days=false

				# First select the module type or module name
				if [[ "$(common.user.getOK "Do you want to update specific module types (otherwise, you get a list of modules)?" )" == true  ]]
				then
					# Module types it is.
					# We add the value "all" and "none" to the result
					steps="$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT type FROM types UNION SELECT 'all' FROM dual UNION SELECT 'none' FROM dual")"
					
					oIFS="$IFS"
					# set IFS to newline that select parses correctly
					IFS='
'
					which_step="$(common.user.getMenuChoice "Which module types state information should be updated?" "${steps}" "none" )"
					# Reset IFS
					IFS="$oIFS"
					
					if [[ $which_step == all ]]
					then
						main.log -a "You pre-selected all module types for removal"
						where_module=""
					elif [[ $which_step == none ]]
					then
						main.log -a "You chose not to modify any data."
						continue
					else
						where_module="type='$which_step'"
					fi
					
				else
					# Module names
					
					# We add the value "all" and "none" to the result
					steps="$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT module FROM modules UNION SELECT 'all' FROM dual UNION SELECT 'none' FROM dual")"
					
					oIFS="$IFS"
					# set IFS to newline that select parses correctly
					IFS='
'
					which_step="$(common.user.getMenuChoice "Which modules state information should be updated?" "${steps}" "none" )"
					# Reset IFS
					IFS="$oIFS"
					
					if [[ $which_step == all ]]
					then
						main.log -a "You pre-selected all modules for removal"
						where_module="1=1"
					elif [[ $which_step == none ]]
					then
						main.log -a "You chose not to modify any data."
						continue
					else
						where_module="module='$which_step'"
					fi
				fi
				
				# Get all days and add all as above
				# The id contains the ISO date
				days="$(common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT substr(t.id,1,10) day_iso FROM tasks t WHERE $where_module UNION SELECT 'all' FROM dual UNION SELECT 'none' FROM dual")"
				
				oIFS="$IFS"
				# set IFS to newline that select parses correctly
				IFS='
'
				which_day="$(common.user.getMenuChoice "Which days state information should be updated?" "$days" "none" )"
				
				# Reset IFS
				IFS="$oIFS"
				
				if [[ $which_day == all ]]
				then
					main.log -a "You pre-selected all days for removal"
					start_offset=0
					stop_offset=$(( ${CXR_NUMBER_OF_SIMULATION_DAYS} - 1 ))
					all_days=true
					following_days=false
					
				elif [[ $which_day == none ]]
					then
						main.log -a "You chose not to modify any data."
						continue
				else
					start_offset=$(common.date.toOffset $which_day)
					# By defalut, delete one day
					stop_offset=$start_offset
					all_days=false
					# If this is true, we loop through the following days
					following_days="$(common.user.getOK "Do you want to update some following days as well?" )"
					
					if [[ "$following_days" == true ]]
					then
						stop_day="$(common.user.getMenuChoice "Until (and including) which day should we update?" "$days" )"
						
						if [[ $stop_day == all ]]
						then
							stop_offset=$(( ${CXR_NUMBER_OF_SIMULATION_DAYS} - 1 ))
						elif [[ $stop_day == none ]]
							then
								main.log -a "You chose not to modify any data."
								continue
						else
							stop_offset=$(common.date.toOffset $stop_day)
						fi

						confirm_days="$(common.user.getOK "Do you want to confirm each removal?" )"
					fi
				fi
				
				if [[ "$confirm_days" == true ]]
				then
					for iOffset in $(seq $start_offset $stop_offset)
					do
						main.log -a "Planning to update these tasks:"
						
						# Delete just the current one
						where="$where_module AND day_iso IN (SELECT day_iso FROM days WHERE day_offset =$iOffset)"
						
						common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT id FROM tasks WHERE $where"
	
						if [[ "$(common.user.getOK "Do you really want to update these tasks?" )" == false ]]
						then
							# No 
							main.log -a "Will not change this information"
							
							if [[ "$following_days" == true ]]
							then
								if [[ "$(common.user.getOK "Do you want to continue removing tasks?" )" == false ]]
								then
									break
								else
									continue
								fi
							fi
						else
							#Yes, mark as new_status
							common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "UPDATE tasks SET status='$new_status' WHERE $where"
						fi
							
					done
					
				else
						# Confirm all at once
						
						where="$where_module AND substr(id,1,10) in (SELECT day_iso FROM days WHERE day_offset BETWEEN $start_offset AND $stop_offset)"
						
						main.log -a "Planning to update these tasks:"
						common.db.getResultSet "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "SELECT id FROM tasks WHERE $where"
						
						if [[ "$(common.user.getOK "Do you really want to update these tasks?" )" == false ]]
						then
							# No 
							main.log -a "Will not modify this information"
							continue
						else
							# Yes 
							common.db.change "$CXR_STATE_DB_FILE" "$CXR_LEVEL_GLOBAL" "UPDATE tasks SET status='$CXR_STATUS_TODO' WHERE $where"
						fi
						
					fi # Confirm-each?

				main.log -a "Done."
				
				;; # specific
			none)
				main.log -w "Will not modify any state information" 
				return 0
				;; # none
		
		esac # The big one...
	
	done # Loop where user can repeatedly delete data
}

################################################################################
# Function: common.state.checksumInterface
#	
# Provides a primitive interface to the MD5 hashes.
#
################################################################################
function common.state.checksumInterface()
################################################################################
{
	db_file=${CXR_UNIVERSAL_DIR}/hashes.${CXR_DB_SUFFIX}
	
	main.log -a "Starting simple interface to query MD5 hashes in ${db_file}\nYou can also use ${SQLITE_EXEC} or the Firefox extension SQLite Manager for this.\nYou can either search for files or MD5 hashes."

	message="Do you want to query the MD5  hash database?"
	
	while [[ "$(common.user.getOK "$message" )" == true ]]
	do
		# Fix the message
		message="Do you want to further query the MD5  hash database?"
	
		# what do you want?
		what=$(common.user.getMenuChoice "" "file-MD5 MD5-file none"  "none")
		
		case "$what" in
		
			file-MD5)
				file=$(common.user.getInput "Please enter the filename you are looking for (% is allowed)")
				select="SELECT key, hash, datetime(epoch_c, 'unixepoch') FROM hash WHERE key LIKE '$file';"

				;;
			
			MD5-file)
				md5=$(common.user.getInput "Please enter the MD5 hash you are looking for (% is allowed)")
				select="SELECT key, hash, datetime(epoch_c, 'unixepoch') FROM hash WHERE hash LIKE '$md5';"

				;;
		
			none) return 0
		
		esac
		
		result=$(common.db.getResultSet "$db_file" "$CXR_LEVEL_UNIVERSAL" "$select")
		
		echo
		echo "$result"
		echo
	
	done
	
}

################################################################################
# Function: common.state.doContinue?
#	
# Checks if the .continue file still exists,
# if not, CXR_RET_CONTINUE_MISSING is returned. Also checks the error threshold
# ends run if we are too high and toches the CONTINUE file
#
################################################################################
function common.state.doContinue?()
################################################################################
{
	local error_count
	error_count=$(main.countErrors)
	
	# Report error count and ReaLoad
	main.log -a -b "Current Error Count: $error_count\nCurrent ReaLoad: $(common.performance.getReaLoadPercent) %"

	# Check error threshold, but only if the value of
	# of CXR_ERROR_THRESHOLD is not -1
	if [[ ( ${CXR_ERROR_THRESHOLD} != ${CXR_NO_ERROR_THRESHOLD} ) && ( ${error_count} -gt ${CXR_ERROR_THRESHOLD} )   ]]
	then
		main.dieGracefully "The number of errors occured (${error_count}) exceeds the threshold (${CXR_ERROR_THRESHOLD})"
	fi
	
	# Do we care at all?
	# Set this in tests etc.
	if [[ "$CXR_ENABLE_STATE_DB" == false  ]]
	then
		main.log -v "You disabled the state DB, cannot determine presence of the continue file!"
		return $CXR_RET_OK
	fi
	
	# If the variable is not defined, nothing will happen
	if [[ "${CXR_CONTINUE_FILE}" ]]
	then
		if [[ ! -f ${CXR_CONTINUE_FILE} ]]
		then
			main.log -w  "The Continue file no longer exists, exiting."
			return $CXR_RET_CONTINUE_MISSING
		fi
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
	
	main.log -a "Initialising state DB in ${CXR_STATE_DIR}"
	common.state.init
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.state.isRepeatedRun?) false "common.state.isRepeatedRun?"
	is "$(common.state.countInstances)" 0 "common.state.countInstances in $CXR_ALL_INSTANCES_DIR"

	########################################
	# teardown tests if needed
	########################################
	
}
