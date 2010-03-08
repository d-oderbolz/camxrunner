#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Version Control/Check Functions
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

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=5

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the function to determine the revision of a file"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2009), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

# just needed for stand-alone usage help
progname=$(basename $0)
################################################################################

################################################################################
# Function: usage
#
# Shows that this script can only be used from within the CAMxRunner
# For common scripts, remove the reference to CAMxRunner options
#
################################################################################
function usage() 
################################################################################
{
	# At least in theory compatible with help2man
	cat <<EOF

	$progname - A part of the CAMxRunner tool chain.

	Can ONLY be called by the CAMxRunner.
	
	Written by $CXR_META_MODULE_AUTHOR
	License: $CXR_META_MODULE_LICENSE
	
	Find more info here:
	$CXR_META_MODULE_DOC_URL
EOF
exit 1
}

################################################################################
# Function: common.version_control.getRevision
#
# Extracts the svn version of a file. 
# 
# Returns:
# If the revision is not set or the file does not exist, returns 0
#
# otherwise, return revision found
#
# Parameters:	
# $1 - Full filename
################################################################################
function common.version_control.getRevision()
################################################################################
{
	# Define & Initialize local vars
	local filename
	local version_string
	local revision
	
	if [[ $# -ne 1  ]]
	then
		# No filename supplied
		filename=/dev/null
	else
		filename=$1
	fi
	
	if [[ ! -f "$filename"  ]]
	then
		# File inexistent
		revision=0
	else
		# File exists, get first version string
		# We want only the version string, nothing afterwards
		version_string="$(grep -o '\$Id.*\$' ${filename})"
		
		# We expect 7 fields:
		# "$Id$"
		
		if [[ $(common.array.countDelimitedElements "${version_string}" " ") -eq 7  ]]
		then
			# Get the lines with $Id, cut away $, get the 3rd field and make sure we get only one line
			revision=$( echo "${version_string}" | cut -d $ -f 2 | cut -d" " -f3 | head -n1)
		else
			# We do not have 6 fields, cannot garantee anything!
			main.log -w  "Version string of file $filename is broken. Fix using svn!"
			revision=0
		fi
		
		# Correct any garbage
		if [[ $(main.isNumeric? "$revision") == false   ]]
		then
			revision=0
		fi
	fi
	
	echo $revision
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
	if [[ "${CXR_TESTING_FROM_HARNESS:-false}" == false  ]]
	then
		# We need to do initialisation
	
		# This is the run we use to test this
		CXR_RUN=$CXR_META_MODULE_TEST_RUN
	
		# Safety measure if script is not called from .
		MY_DIR=$(dirname $0) && cd $MY_DIR
	
		# We step down the directory tree until we either find CAMxRunner.sh
		# or hit the root directory /
		while [ $(pwd) != / ]
		do
			cd ..
			# If we find CAMxRunner, we are there
			ls CAMxRunner.sh >/dev/null 2>&1 && break
			
			# If we are in root, we have gone too far
			if [[ $(pwd) == /  ]]
			then
				echo "Could not find CAMxRunner.sh!"
				exit 1
			fi
		done
		
		# Save the number of tests, as other modules
		# will overwrite this (major design issue...)
		MY_META_MODULE_NUM_TESTS=$CXR_META_MODULE_NUM_TESTS
		
		# Include the init code
		source inc/init_test.inc
		
		# Plan the number of tests
		plan_tests $MY_META_MODULE_NUM_TESTS
		
	fi
	
	########################################
	# Setup tests if needed
	########################################
	
	# We need to make sure that svn does not update Id here :-)
	# Therefore, we write the first $ separately
	
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
	
	is $(common.version_control.getRevision "$test_file1") 2605 "common.version_control.getRevision normal"
	is $(common.version_control.getRevision "$test_file2") 2605 "common.version_control.getRevision double-contradiction"
	
	main.log -a  "We provoke an error mesage here - you can ignore this..."
	is $(common.version_control.getRevision "$test_file3") 0 "common.version_control.getRevision missing revision"
	is $(common.version_control.getRevision "$test_file4") 12345 "common.version_control.getRevision with garbage at end"
	
	is $(common.version_control.getRevision /some/nonexisting/file) 0 "common.version_control.getRevision missing file"

	########################################
	# teardown tests if needed
	########################################

}

################################################################################
# Are we running stand-alone? 
################################################################################


# If the CXR_META_MODULE_NAME  is not set
# somebody started this script alone
# Normlly this is not allowed, except to test using -t
if [[ -z "${CXR_META_MODULE_NAME:-}"  ]]
then

	# When using getopts, never directly call a function inside the case,
	# otherwise getopts does not process any parametres that come later
	while getopts ":dvFST" opt
	do
		case "${opt}" in
		
			d) CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_LOG_EXT="-dry" ;;
			v) CXR_USER_TEMP_VERBOSE=true ; echo "Enabling VERBOSE (-v) output. " ;;
			F) CXR_USER_TEMP_FORCE=true ;;
			S) CXR_USER_TEMP_SKIP_EXISTING=true ;;
			
			T) TEST_IT=true;;
			
		esac
	done
	
	# This is not strictly needed, but it allows to read 
	# non-named command line options
	shift $((${OPTIND} - 1))

	# Make getopts ready again
	unset OPTSTRING
	unset OPTIND
	
	# This is needed so that getopts surely processes all parameters
	if [[ "${TEST_IT:-false}" == true  ]]
	then
		test_module
	else
		usage
	fi

fi


