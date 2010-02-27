#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
#
# Version: $Id$ 
#
# Contains the Hash Functions, allows to map strings (keys) to other strings (values).
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
CXR_META_MODULE_NUM_TESTS=8

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
# [$2] - type of hash, either "instance" (default) or "global"
################################################################################
function cxr_common_hash_init ()
################################################################################
{
	local hash="$1"
	local type=${2:-instance}
	local hash_dir
	
	# Work out the directory
	case $type in
		instance) hash_dir="${CXR_INSTANCE_HASH_DIR}" ;;
		global) hash_dir="${CXR_GLOBAL_HASH_DIR}" ;;
		*) cxr_main_logger -e "$FUNCNAME" "Unknown Hashtype $type" ;;
	esac
	
	# Create the hash directory
	mkdir -p "${hash_dir}/${hash}"
	
	# Nobody else must modify this directory
	chmod 700 "${hash_dir}/${hash}"
}

################################################################################
# Function: cxr_common_hash_destroy
#
# Destroys a hash with a given name by deleting its directory.
#
# Parameters:
# $1 - name of the hash
# [$2] - type of hash, either "instance" (default) or "global"
################################################################################
function cxr_common_hash_destroy ()
################################################################################
{
	local hash="$1"
	local type=${2:-instance}
	local hash_dir
	
	# Work out the directory
	case $type in
		instance) hash_dir="${CXR_INSTANCE_HASH_DIR}" ;;
		global) hash_dir="${CXR_GLOBAL_HASH_DIR}" ;;
		*) cxr_main_logger -e "$FUNCNAME" "Unknown Hashtype $type" ;;
	esac
	
	cxr_main_logger -i "Deleting the Hash ${hash}"
	rm -rf "${hash_dir}/${hash}"
}

################################################################################
# Function: _hash_fn
#
# Generates a filename for a given hash and key. Internal function for use in hash functions.
#
# Parameters:
# $1 - name of the hash
# $2 - key
# [$3] - type of hash, either "instance" (default) or "global"
################################################################################
function _hash_fn ()
################################################################################
{
	hash="$1"
	key="$2"
	local type=${3:-instance}
	local hash_dir
	local fn
	
	# Work out the directory
	case $type in
		instance) hash_dir="${CXR_INSTANCE_HASH_DIR}" ;;
		global) hash_dir="${CXR_GLOBAL_HASH_DIR}" ;;
		*) cxr_main_logger -e "$FUNCNAME" "Unknown Hashtype $type" ;;
	esac
	
	# Generate the filename
	fn="$(perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$key")"
	
	echo "${hash_dir}/${hash}/${fn}"
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
# [$4] - type of hash, either "instance" (default) or "global"
################################################################################
function cxr_common_hash_put ()
################################################################################
{
	local hash="$1"
	local key="$2"
	local value="$3"
	local type=${4:-instance}
	local fn
	
	# Generate the filename
	fn="$(_hash_fn $hash $key $type)"
	
	# Write the value
	echo "${value}" > "${fn}"
}

################################################################################
# Function: cxr_common_hash_get
#
# Gets a certain value for a hash
#
# Parameters:
# $1 - name of the hash
# $2 - key
# [$3] - type of hash, either "instance" (default) or "global"
################################################################################
function cxr_common_hash_get ()
################################################################################
{
	local hash="$1"
	local key="$2"
	local type=${3:-instance}
	local fn
	
	# Generate the filename
	fn="$(_hash_fn $hash $key $type)"
	
	# Get the value
	cat "${fn}"
}

################################################################################
# Function: cxr_common_hash_delete
#
# Deletes a certain value for a hash
#
# Parameters:
# $1 - name of the hash
# $2 - key
# [$3] - type of hash, either "instance" (default) or "global"
################################################################################
function cxr_common_hash_delete ()
################################################################################
{
	local hash="$1"
	local key="$2"
	local type=${3:-instance}
	local fn
	
	if [[ $(cxr_common_hash_has? $hash $key $type) == true ]]
	then
		# Generate the filename
		fn="$(_hash_fn $hash $key $type)"
		
		# remove the value
		rm -f "${fn}"
	else
		cxr_main_logger -w "$FUNCNAME" "Key $key not found in $type hash $hash"
	fi
}

################################################################################
# Function: cxr_common_hash_mtime
#
# Gets the modification time (Unix Epoch) for a given value
#
# Parameters:
# $1 - name of the hash
# $2 - key
# [$3] - type of hash, either "instance" (default) or "global"
################################################################################
function cxr_common_hash_mtime ()
################################################################################
{
	local hash="$1"
	local key="$2"
	local type=${3:-instance}
	local fn
	
	# Generate the filename
	fn="$(_hash_fn $hash $key $type)"
	
	# Get the mtime
	$(cxr_common_get_file_mtime "$fn")
}

################################################################################
# Function: cxr_common_hash_has?
#
# Returns true if the given key is contained in the hash
#
# Parameters:
# $1 - name of the hash
# $2 - key
# [$3] - type of hash, either "instance" (default) or "global"
################################################################################
function cxr_common_hash_has? ()
################################################################################
{
	local hash="$1"
	local key="$2"
	local type=${3:-instance}
	local fn
	
	# Generate the filename
	fn="$(_hash_fn $hash $key $type)"
	
	if [[ -f "${fn}"  ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: cxr_common_hash_new?
#
# Returns true if the given key is contained in the hash and its update time is 
# newer than this runs start time (meaning that we do not need to update such a hash
# if it is used as a cache)
#
# Parameters:
# $1 - name of the hash
# $2 - key
# [$3] - type of hash, either "instance" (default) or "global"
################################################################################
function cxr_common_hash_new? ()
################################################################################
{
	local hash="$1"
	local key="$2"
	local type=${3:-instance}
	
	local res
	
	# Is it in the hash?
	if [[ $(cxr_common_hash_has? $hash $key $type) == true  ]]
	then
		# Exists, test age. CXR_EPOCH is the Epoch we started this run in
		# if the hash es epoch is smaller, it is older
		if [[ "$(cxr_common_hash_mtime $hash $key $type)" -lt $CXR_EPOCH  ]]
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
# Function: cxr_common_hash_keys
#
# Returns a list of keys of the given hash as a quoted space separated list.
#
# Parameters:
# $1 - name of the hash
# [$2] - type of hash, either "instance" (default) or "global"
################################################################################
function cxr_common_hash_keys ()
################################################################################
{
	local hash=${1}
	local type=${2:-instance}
	local hash_dir
	local fn
	local key
	local list=""
	
	# Work out the directory
	case $type in
		instance) hash_dir="${CXR_INSTANCE_HASH_DIR}" ;;
		global) hash_dir="${CXR_GLOBAL_HASH_DIR}" ;;
		*) cxr_main_logger -e "$FUNCNAME" "Unknown Hashtype $type" ;;
	esac
	
	for fn in $(ls ${hash_dir}/${hash})
	do
		key="$(perl -MURI::Escape -e 'print uri_unescape($ARGV[0]);' "$fn")"
		list="${list}\"$key\" "
	done
	
	# Remove last space
	list=${list%\ }
	
	echo "$list"
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
	# Instance hash
	cxr_common_hash_init test
	cxr_common_hash_put test /hallo/velo SomeOtherValue
	cxr_common_hash_put test /hallo/gugs SomeOtherValue
	
	# Global Hash
	cxr_common_hash_init test_g global
	cxr_common_hash_put test_g /hallo/velo SomeOtherValue global
	cxr_common_hash_put test_g /hallo/gugs SomeOtherValue global
	
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is "$(cxr_common_hash_get test "/hallo/velo")" SomeOtherValue "cxr_common_hash_get test (instance) with path as key"
	is "$(cxr_common_hash_has? test "/hallo/velo")" true "cxr_common_hash_has? test (instance) with path as key"
	is "$(cxr_common_hash_keys test)" "/hallo/gugs /hallo/velo" "cxr_common_hash_keys test (instance) with path as key"
	
	cxr_common_hash_delete test "/hallo/velo"
	is "$(cxr_common_hash_has? test "/hallo/velo")" false "cxr_common_hash_delete test (instance) with path as key"

	
	is "$(cxr_common_hash_get test_g "/hallo/velo" global)" SomeOtherValue "cxr_common_hash_get test (global) with path as key"
	is "$(cxr_common_hash_has? test_g "/hallo/velo" global)" true "cxr_common_hash_has? test (global) with path as key"
	is "$(cxr_common_hash_keys test_g global)" "/hallo/gugs /hallo/velo" "cxr_common_hash_keys test (global) with path as key"
	
	cxr_common_hash_delete test_g "/hallo/velo" 
	is "$(cxr_common_hash_has? test_g "/hallo/velo")" false "cxr_common_hash_delete test (global) with path as key"


	NUMEL=100
	
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
	cxr_common_hash_destroy test_g global
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


