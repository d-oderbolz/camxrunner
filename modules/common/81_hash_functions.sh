#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
#
# Version: $Id$ 
#
# Contains the Hash Functions, allows to map stirngs (keys) to other strings (values).
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################
# TODO: 
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=3

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

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
# Function: cxr_common_hash_init
#
# Creates a hash with a given name. Hash names must be unique per run.
# Basically creates a directory into which we store files.
#
# Parameters:
# $1 - name of the hash
################################################################################
function cxr_common_hash_init ()
################################################################################
{
	HASH="$1"
	
	# Create the hash directory
	mkdir -p "${CXR_HASH_DIR}/${HASH}"
	
	# Nobody else must modify this directory
	chmod 700 "${CXR_HASH_DIR}/${HASH}"
}

################################################################################
# Function: cxr_common_hash_destroy
#
# Destroys a hash with a given name by deleting its directory.
#
# Parameters:
# $1 - name of the hash
################################################################################
function cxr_common_hash_destroy ()
################################################################################
{
	HASH="$1"
	
	cxr_main_logger -i "Deleting the Hash ${HASH}"
	rm -rf "${CXR_HASH_DIR}/${HASH}"
}

################################################################################
# Function: cxr_common_hash_put
#
# Puts a value into a key of a given hash. First the key is urlencoded using perl
# then we use this value as a filename to store the value in.
#
# Parameters:
# $1 - name of the hash
# $2 - key
# $3 - value
################################################################################
function cxr_common_hash_put ()
################################################################################
{
	HASH="$1"
	KEY="$2"
	VALUE="$3"
	
	# Generate the filename
	FN="$(perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$KEY")"
	
	# Write the value
	echo "${VALUE}" > "${CXR_HASH_DIR}/${HASH}/${FN}"
	
}

################################################################################
# Function: cxr_common_hash_get
#
# Gets a certain value for a hash
#
# Parameters:
# $1 - name of the hash
# $2 - key
################################################################################
function cxr_common_hash_get ()
################################################################################
{
	HASH="$1"
	KEY="$2"
	
	# Generate the filename
	FN="$(perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$KEY")"
	
	# Get the value
	cat "${CXR_HASH_DIR}/${HASH}/${FN}"
}

################################################################################
# Function: cxr_common_hash_has?
#
# Returns true if the given key is contained in the hash
#
# Parameters:
# $1 - name of the hash
# $2 - key
################################################################################
function cxr_common_hash_has? ()
################################################################################
{
	HASH="$1"
	KEY="$2"
	
	# Generate the filename
	FN="$(perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$KEY")"
	
	if [ -f "${CXR_HASH_DIR}/${HASH}/${FN}" ]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: cxr_common_hash_keys
#
# Returns a list of keys of the given hash
#
# Parameters:
# $1 - name of the hash
################################################################################
function cxr_common_hash_keys ()
################################################################################
{
	for FN in $(ls ${CXR_HASH_DIR}/${HASH})
	do
		KEY="$(perl -MURI::Escape -e 'print uri_unescape($ARGV[0]);' "$FN")"
		echo -n "$KEY "
	done
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
	if [ "${CXR_TESTING_FROM_HARNESS:-false}" == false ]
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
			if [ $(pwd) == / ]
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
	cxr_common_hash_init test
	
	cxr_common_hash_put test /hallo/velo SomeOtherValue
	
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(cxr_common_hash_get test "/hallo/velo") SomeOtherValue "cxr_common_hash_get test with path as key"
	is $(cxr_common_hash_has? test "/hallo/velo") true "cxr_common_hash_has? test with path as key"
	is $(cxr_common_hash_keys test) /hallo/velo "cxr_common_hash_keys test with path as key"
	
	NUMEL=10000
	
	echo "Hash Performance: Adding $NUMEL elements..."
	time (
		for a in $(seq 1 $NUMEL)
		do 
			cxr_common_hash_put test $a $a
		done 
	)
	
	echo "Array Performance: Adding $NUMEL elements..."
	time (
		for a in $(seq 1 $NUMEL)
		do 
			b[$a]=$a
		done 
	)
	
	########################################
	# teardown tests if needed
	########################################
	cxr_common_hash_destroy test

}

################################################################################
# Are we running stand-alone? 
################################################################################


# If the CXR_META_MODULE_NAME  is not set
# somebody started this script alone
# Normlly this is not allowed, except to test using -t
if [ -z "${CXR_META_MODULE_NAME:-}" ]
then

	# When using getopts, never directly call a function inside the case,
	# otherwise getopts does not process any parametres that come later
	while getopts ":dvFST" opt
	do
		case "${opt}" in
		
			d) CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_LOG_EXT="-dry" ;;
			v) CXR_USER_TEMP_VERBOSE=true ; echo "Enabling VERBOSE (-v) output. All lines starting with % would not be present otherwise" ;;
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
	if [ "${TEST_IT:-false}" == true ]
	then
		test_module
	else
		usage
	fi

fi


