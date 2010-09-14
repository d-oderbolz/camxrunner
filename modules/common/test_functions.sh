# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the test harness of CAMxRunner. Also contains tests for some inc functions.
# There is one test-configuration for each Model Version, called something like
# CAMx-v4.51-test.conf or CAMx-v4.42-test.conf. 
#
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
CXR_META_MODULE_NUM_TESTS=14

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains functions to test CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: common.test.loadData
#	
# Loads testdata for a testrun into CXR_TMP_DIR/CXR_RUN. Data comes from
# tesctcase/CAMxRunner/Model/Version and goes to CXR_TMP_DIR/CXR_RUN. 
# There may be more than one tarball present, just make sure to structure them using these 
# three directories:
# inputs/
# expected_outputs/
# outputs/ (Normally empty)
#
################################################################################
function common.test.loadData()
################################################################################
{
	local filetype
	
	# Need to load test data
	main.log -B " Loading test data (CXR_LOAD_TEST_DATA is true)... "

	if [[   "${CXR_TEST_DATA_OUTPUT_DIR:-}" && -d "${CXR_TEST_DATA_OUTPUT_DIR:-}" && -f "${CXR_TEST_DATA_INPUT_FILE:-}"    ]]
	then
	
		cd "${CXR_TEST_DATA_OUTPUT_DIR}" || main.dieGracefully "Could not change to ${CXR_TEST_DATA_OUTPUT_DIR}"
		
		# Query filetype
		filetype=$(common.fs.getFileType "${CXR_TEST_DATA_INPUT_FILE}")
		
		# We support gzip and bzip compression
		case $filetype in
		
			bzip2)
				tar --use-compress-program=$CXR_BUNZIP2_EXEC -xf "${CXR_TEST_DATA_INPUT_FILE}" .
				;;
				
			gzip)
				tar --use-compress-program=$CXR_GUNZIP_EXEC -xf "${CXR_TEST_DATA_INPUT_FILE}" .
				;;
		
			*)
				main.log -e  "File type $FILETYPE not supported"
				;;
		esac
		
		cd $CXR_RUN_DIR || main.dieGracefully "Could not change back to ${CXR_RUN_DIR}"
	
	else
		main.log -e "Cannot load test data, either CXR_TEST_DATA_OUTPUT_DIR or CXR_TEST_DATA_INPUT_FILE not set correctly or permission problem!"
	fi
}


################################################################################
# Function: common.test.all
#	
# Test all testable modules, generates a .tap file that contains the output of the tests.
#
# Parameters:
# [$1] - The Model Version to use
# [$2] - The Modelname to use
################################################################################
function common.test.all()
################################################################################
{
	local iput_model
	local input_version
	local model_id
	local supported
	local array
	local default_version
	local total_tests
	local message
	local function_file
	local metafield
	local extended
	local oTempfiles
	
	iput_model="${1:-}"
	input_version="${2:-}"
	extended=false
	
	# This function runs all tests that are availabe (asking the user before each new test if wanted).
	# For this, all modules are enumerated and then tested if marked as testable.
	# To do this, we need to know the model name and version to test for (otherwise we might repeat many tests)

	message="Do you want to run the test suite of CAMxRunner?"
	
	while [[ "$(common.user.getOK "$message" )" == true ]]
	do
	
		# Fix the message
		message="Do you want to further run the test suite of CAMxRunner (for other models/versions)?"
		
		################################################################################
		# Determine model and version
		################################################################################	

		if [[ ! "${iput_model}" ]]
		then
			# Model was not passed
			model="$(common.user.getMenuChoice "Which model should the tests be run for?\nIf your desired model is not in this list, adjust CXR_SUPPORTED_MODELS \n(Currently $CXR_SUPPORTED_MODELS)" "$CXR_SUPPORTED_MODELS" "CAMx")"
		else
			model=$iput_model
		fi
		
		model_id=$(common.runner.getModelId "$model") || main.dieGracefully "Model $model is not known."
		
		
		if [[ ! "${input_version}" ]]
		then
			# version was not passed
		
			# Extract the list of supported versions
			supported="${CXR_SUPPORTED_MODEL_VERSIONS[${model_id}]}"
			
			# Set the default to the first entry
			# Save old IFS
			oIFS="$IFS"
		
			IFS="$CXR_SPACE"
			
			# Suck line into array
			array=($supported)
			
			# Reset IFS
			IFS="$oIFS"
			
			default_version=${array[0]}
		
			#Generate a menu automatically
			version="$(common.user.getMenuChoice "Which version of $model should be used?\nIf your desired version is not in this list, adjust CXR_SUPPORTED_MODEL_VERSIONS \n(Currently $supported)" "$supported" "$default_version")"
		else
			version=$input_version
		fi
		
		common.check.isVersionSupported? "$version" "$model"
		
		main.log -a "Testing system using modules for $model $version..."
		
		if [[ "$(common.user.getOK "Do you want to run the extended tests (can run up to 30 minutes)?" )" == true ]]
		then
			extended=true
		else
			extended=false
		fi
		
		# We delete the old instance files
		oInstanceDir="$CXR_INSTANCE_DIR"
		
		CXR_RUN=${model}-v${version}-test
		
		# Prepare environment
		main.setModelAndVersion ${CXR_RUN}
		source $CXR_RUN_DIR/inc/defaults.inc
		main.readConfig "${CXR_RUN}" "${model}" "${version}" "${CXR_RUN_DIR}"
		
		source inc/tap-functions.inc
		
		# For the time being, we turn off errexit
		# Because tap-functions uses non-0 returns
		set +e
		
		# Delete old instance dir
		rm -rf "${oInstanceDir}" &>/dev/null

		# When this is true, we know a test is running
		CXR_TEST_IN_PROGRESS=true
		
		# We do not need to see MD5 again
		CXR_REPORT_MD5=false
		
		#Disable state DB (we want no logging of what has been done and not)
		CXR_ENABLE_STATE_DB=false
		
		# No Notification etc.
		CXR_HOLLOW=true
		
		# count simulation days
		CXR_NUMBER_OF_SIM_DAYS=$(common.date.DaysBetween "$CXR_START_DATE" "$CXR_STOP_DATE")
		
		# Let's just initialise the date variables for day 0
		common.date.setVars "$CXR_START_DATE" "0"
		
		# Init state DB
		common.state.init
		
		########################################
		#  Count all tests (Harness needs to know the count)
		########################################
		
		total_tests=0
		
		if [[ "$extended" == true ]]
		then
			# Do the long list
			
			# save stdin and redirect it from an in-line file
			exec 9<&0 <<-EOF
			# Here we list all directories and a comment on each
			# The order might need to change later
			$CXR_COMMON_INPUT_DIR                 "Common Modules"
			$CXR_COMMON_MODEL_INPUT_DIR           "Model specific common Modules"
			$CXR_COMMON_VERSION_INPUT_DIR         "Version specific  common Modules"
			$CXR_PREPROCESSOR_DAILY_INPUT_DIR     "Daily preprocessor Modules"
			$CXR_PREPROCESSOR_ONCE_INPUT_DIR      "One-Time preprocessor Modules"
			$CXR_POSTPROCESSOR_DAILY_INPUT_DIR    "Daily postprocessor Modules"
			$CXR_POSTPROCESSOR_ONCE_INPUT_DIR     "One-Time postprocessor Modules"
			$CXR_MODEL_INPUT_DIR                  "Model Modules"
			$CXR_INSTALLER_INPUT_DIR              "Common installer Modules"
			$CXR_INSTALLER_MODEL_INPUT_DIR        "Model specific installer Modules"
			$CXR_INSTALLER_VERSION_INPUT_DIR      "Version specific installer Modules"
			EOF
		else
			# Do the short list
			# save stdin and redirect it from an in-line file
			exec 9<&0 <<-EOF
			# Here we list all directories and a comment on each
			# The order might need to change later
			$CXR_COMMON_INPUT_DIR                 "Common Modules"
			$CXR_COMMON_MODEL_INPUT_DIR           "Model specific common Modules"
			$CXR_COMMON_VERSION_INPUT_DIR         "Version specific common Modules"
			EOF
		fi
		while read CURRENT_DIR COMMENT
		do
			# ignore comment and blank lines
			echo "${CURRENT_DIR}" |egrep -v "^(#|$)" >/dev/null || continue
	
			main.log "Counting tests in ${CURRENT_DIR} (${COMMENT})..."
			
			for function_file in $(ls ${CURRENT_DIR}/*.sh 2>/dev/null)
			do
				module="$(main.getModuleName "$function_file")"

				# Extract the number of tests using grep
				# Similar code is used in <common.state.updateInfo>

				metafield=$(grep '^[[:space:]]\{0,\}CXR_META_MODULE_NUM_TESTS\{1,\}=.*' $function_file)

				# the value is to the right
				num_tests="$(expr match "$metafield" '.*=\(.*\)')" || :

				# OK, we want all quoting gone and variables expanded
				num_tests="$(eval "echo $(echo "$num_tests")")"

				total_tests=$(( $total_tests + $num_tests ))
			done
		done
		# Restore stdin and close fd 9
		exec 0<&9 9<&-
		
		########################################
		#  Plan these tests
		########################################
		main.log "Planning to run $total_tests tests..."
		
		# Plan them
		plan_tests $total_tests
	
		########################################
		#  Do these tests
		########################################
		
		# This is to remember the last loaded config
		LAST_LOADED_CONFIG=$CXR_RUN
		
		if [[ "$extended" == true ]]
		then
			# Do the long list
			
			# save stdin and redirect it from an in-line file
			exec 9<&0 <<-EOF
			# Here we list all directories and a comment on each
			# The order might need to change later
			$CXR_COMMON_INPUT_DIR                 "Common Modules"
			$CXR_COMMON_MODEL_INPUT_DIR           "Model specific common Modules"
			$CXR_COMMON_VERSION_INPUT_DIR         "Version specific  common Modules"
			$CXR_PREPROCESSOR_DAILY_INPUT_DIR     "Daily preprocessor Modules"
			$CXR_PREPROCESSOR_ONCE_INPUT_DIR      "One-Time preprocessor Modules"
			$CXR_POSTPROCESSOR_DAILY_INPUT_DIR    "Daily postprocessor Modules"
			$CXR_POSTPROCESSOR_ONCE_INPUT_DIR     "One-Time postprocessor Modules"
			$CXR_MODEL_INPUT_DIR                  "Model Modules"
			$CXR_INSTALLER_INPUT_DIR              "Common installer Modules"
			$CXR_INSTALLER_MODEL_INPUT_DIR        "Model specific installer Modules"
			$CXR_INSTALLER_VERSION_INPUT_DIR      "Version specific installer Modules"
			EOF
		else
			# Do the short list
			# save stdin and redirect it from an in-line file
			exec 9<&0 <<-EOF
			# Here we list all directories and a comment on each
			# The order might need to change later
			$CXR_COMMON_INPUT_DIR                 "Common Modules"
			$CXR_COMMON_MODEL_INPUT_DIR           "Model specific common Modules"
			$CXR_COMMON_VERSION_INPUT_DIR         "Version specific common Modules"
			EOF
		fi
		while read CURRENT_DIR COMMENT
		do
			# ignore comment and blank lines
			echo "${CURRENT_DIR}" |egrep -v "^(#|$)" >/dev/null || continue
	
			main.log "Executing ${COMMENT} tests..."
			
			for function_file in $(ls ${CURRENT_DIR}/*.sh 2>/dev/null)
			do
				module="$(main.getModuleName $function_file)"
				
				# Load it
				CXR_META_MODULE_NAME=$module
					
				source $function_file
				
				if [[ ${CXR_META_MODULE_NUM_TESTS:-0} -gt 0  ]]
				then
					main.log -b "Testing $CXR_META_MODULE_NAME ($CXR_META_MODULE_NUM_TESTS tests)..."
					
					test_module
					
				else
					main.log -v  "There are no tests in $CXR_META_MODULE_NAME yet."
				fi
				
			done
		done
		# Restore stdin and close fd 9
		exec 0<&9 9<&-
		
		########################################
		#  Get stats
		########################################
		summarize_tests
		
	done
	
	CXR_TEST_IN_PROGRESS=false
	
	# Reset strict return checks
	set -e
}

################################################################################
# Function: test_module
#
# Runs the predefined tests for this module
# 
################################################################################	
function test_module()
################################################################################
{
	########################################
	# Setup tests if needed
	########################################
	
	# Create a file with one revision
	test_file1=$(common.runner.createTempFile $FUNCNAME)
	echo -n '$' > "$test_file1"
	echo 'Id: 30_version_control_functions.sh 2605 2010-02-14 13:14:29Z oderbolz $' >> "$test_file1"
	
	# Create a file with 2 revisions (must find the first)
	test_file2=$(common.runner.createTempFile $FUNCNAME)
	echo -n '$' > "$test_file2"
	echo 'Id: 30_version_control_functions.sh 2605 2010-02-14 13:14:29Z oderbolz $' >> "$test_file2"
	echo 'Id: 30_version_control_functions.sh 2600 2010-02-14 13:14:29Z oderbolz $' >> "$test_file2"
	
	# Create a file with no revisions
	test_file3=$(common.runner.createTempFile $FUNCNAME)
	echo -n '$' > "$test_file3"
	echo 'Id: 30_version_control_functions.sh 2010-02-14 13:14:29Z oderbolz $' >> "$test_file3"
	
	# Create a file with garbage afterwards
	test_file4=$(common.runner.createTempFile $FUNCNAME)
	echo -n '$' > "$test_file4"
	echo 'Id: 30_version_control_functions.sh 12345 2010-02-14 13:14:29Z oderbolz $ andhereisgarbage' >> "$test_file4"
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	#Here, we test some main functions
	
	is "$(main.getBinaryName ls)" "$(which ls)" "Test of main.getBinaryName using ls"
	
	is $(main.countDelimitedElements "one${CXR_DELIMITER}two") 2 "main.countDelimitedElements with 2 elements, default delimiter"
	is $(main.countDelimitedElements "one two" " ") 2 "main.countDelimitedElements with 2 elements, space"
	is $(main.countDelimitedElements "one two " " ") 2 "main.countDelimitedElements with 2 elements, space at end"
	is $(main.countDelimitedElements "") 0 "main.countDelimitedElements with 0 elements"

	
	is $(main.isNumeric? 0) true "main.isNumeric? 0"
	is $(main.isNumeric? -1000) true "main.isNumeric? -1000"
	is $(main.isNumeric? "") false "main.isNumeric? empty string"
	is $(main.isNumeric? "A100") false "main.isNumeric? A100"
	
	is $(main.getRevision "$test_file1") 2605 "main.getRevision normal"
	is $(main.getRevision "$test_file2") 2605 "main.getRevision double-contradiction"
	
	main.log -a  "We provoke an error message here - you can ignore this..."
	is $(main.getRevision "$test_file3") 0 "main.getRevision missing revision"
	is $(main.getRevision "$test_file4") 12345 "main.getRevision with garbage at end"
	
	is $(main.getRevision /some/nonexisting/file) 0 "main.getRevision missing file"
	
	########################################
	# teardown tests if needed
	########################################
	
}
