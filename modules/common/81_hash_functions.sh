#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
#
# Version: $Id$ 
#
# Contains the Hash Functions, allows to map strings (keys) to other strings (values).
# We use files to store the hash values, their names are the URL-Encoded keys.
# This has the disadvantage of being slower than traditional, memory-base hashes (like perl
# has them), but it has two advantages: our hashes are persistent (they survive a run) and
# we can determine meta-information, such as the time of the last update of a single key.
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
CXR_META_MODULE_NUM_TESTS=11

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|perl"

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
# Function: _common.hash.getDir
#
# Returns the hash_dir to use depending on the type.
#
# Parameters:
# $1 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL" 
################################################################################
function _common.hash.getDir()
################################################################################
{
	local type="${1}"
	
	# Work out the directory
	case $type in
		$CXR_HASH_TYPE_INSTANCE) hash_dir="${CXR_INSTANCE_HASH_DIR}" ;;
		$CXR_HASH_TYPE_GLOBAL) hash_dir="${CXR_GLOBAL_HASH_DIR}" ;;
		$CXR_HASH_TYPE_UNIVERSAL) hash_dir="${CXR_UNIVERSAL_HASH_DIR}" ;;
		*) main.dieGracefully "Unknown Hashtype $type" ;;
	esac
	
	echo "$hash_dir"
	
}
################################################################################
# Function: common.hash.init
#
# Creates a hash with a given name. Hash names must be unique per run.
# Basically creates a directory into which we store files.
# Usage note: the name of a hash can convey important information. It can be dynamic,
# e.g. be dependent on the model name and version. This way, complex lookup functions
# depending on more than one parameter can be realized.
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL" 
################################################################################
function common.hash.init()
################################################################################
{
	local hash="$1"
	local type="$2"
	local hash_dir
	local complete_dir
	
	# Work out the directory
	hash_dir="$(_common.hash.getDir "$type")"
	complete_dir="${hash_dir}/${hash}"
	
	# Create the hash directory
	if [[ ! -d "${complete_dir}" ]]
	then
		mkdir -p "${complete_dir}"
		# Nobody else must modify this directory
		chmod 700 "${complete_dir}"
	else
		touch "${complete_dir}"
	fi
}

################################################################################
# Function: common.hash.destroy
#
# Destroys a hash with a given name by deleting its directory.
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
################################################################################
function common.hash.destroy()
################################################################################
{
	local hash="$1"
	local type="$2"
	local hash_dir
	
	# Work out the directory
	hash_dir="$(_common.hash.getDir "$type")"
	
	main.log -i "Deleting the Hash ${hash}"
	rm -rf "${hash_dir}/${hash}"
}

################################################################################
# Function: _common.hash.getFileName
#
# Generates a filename for a given hash and key. Internal function for use in hash functions.
# Trims the key of leading or trailing double quotes because <common.hash.getKeys> adds them.
# Tries to remedy missing directories (if <common.hash.init> was not called!)
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
################################################################################
function _common.hash.getFileName()
################################################################################
{
	local hash="$1"
	local type="$2"
	local key="$3"

	local hash_dir
	local fn
	
	# Work out the directory
	hash_dir="$(_common.hash.getDir "$type")"
	
	# Remove leading or trailing quotes
	key="$(common.string.trim "${key}" '\"')"
	
	if [[ ! "$key" ]]
	then
		# Empty keys cannot be handled
		main.dieGracefully "Detected empty key!"
	fi
	
	# Generate the filename
	fn="${hash_dir}/${hash}/$(perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$key")"
	
	# Make the directory, just in case
	mkdir -p "$(dirname "${fn}")" >/dev/null 2>&1
	
	echo "${fn}"
}

################################################################################
# Function: common.hash.put
#
# Puts a value into a key of a given hash. First the key is urlencoded using perl
# then we use this value as a filename to store the value in.
# 
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
# $4 - value
################################################################################
function common.hash.put()
################################################################################
{
	local hash="$1"
	local type="$2"
	local key="$3"
	local value="$4"

	local fn
	
	# Generate the filename
	fn="$(_common.hash.getFileName "$hash" "$type" "$key")"
	
	# Write the value
	echo "${value}" > "${fn}"
}

################################################################################
# Function: common.hash.get
#
# Gets a certain value from a hash
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.get()
################################################################################
{
	local hash="$1"
	local type="$2"
	local key="$3"

	local fn
	
	# Generate the filename
	fn="$(_common.hash.getFileName "$hash" "$type" "$key")"
	
	# Get the value
	cat "${fn}"
}

################################################################################
# Function: common.hash.delete
#
# Deletes a certain value for a hash
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.delete()
################################################################################
{
	local hash="$1"
	local type="$2"
	local key="$3"
	
	local fn
	
	if [[ $(common.hash.has? "$hash" "$type" "$key") == true ]]
	then
		# Generate the filename
		fn="$(_common.hash.getFileName "$hash" "$type" "$key")"
		
		# remove the value
		rm -f "${fn}"
	else
		main.log -v  "Key "$key" not found in "$type" hash "$hash""
	fi
}

################################################################################
# Function: common.hash.getMtime
#
# Gets the modification time (Unix Epoch) for a given value
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.getMtime()
################################################################################
{
	local hash="$1"
	local type="$2"
	local key="$3"
	
	local fn
	
	# Generate the filename
	fn="$(_common.hash.getFileName "$hash" "$type" "$key")"
	
	# Get the mtime
	echo "$(common.fs.getMtime "$fn")"
}

################################################################################
# Function: common.hash.has?
#
# Returns true if the given key is contained in the hash
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
# $3 - key
################################################################################
function common.hash.has?()
################################################################################
{
	local hash="$1"
	local type="$2"
	local key="$3"
	
	local fn
	
	# Generate the filename
	fn="$(_common.hash.getFileName "$hash" "$type" "$key")"
	
	if [[ -f "${fn}"  ]]
	then
		echo true
	else
		echo false
	fi
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
################################################################################
function common.hash.isNew?()
################################################################################
{
	local hash="$1"
	local type="$2"
	local key="$3"
	
	local res
	
	# Is it in the hash?
	if [[ $(common.hash.has? "$hash" "$type" "$key") == true ]]
	then
		# Exists, test age. CXR_EPOCH is the Epoch we started this run in
		# if the hash's epoch is smaller, it is older
		if [[ "$(common.hash.getMtime "$hash" "$type" "$key")" -lt "$CXR_EPOCH" ]]
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
# Returns a list of keys of the given hash as a quoted space separated list.
# Do not assume any particular order, it depends on the order ls imposes on the
# encoded keys.
#
# Parameters:
# $1 - name of the hash
# $2 - type of hash, either "$CXR_HASH_TYPE_INSTANCE" , "$CXR_HASH_TYPE_GLOBAL" or "$CXR_HASH_TYPE_UNIVERSAL"
################################################################################
function common.hash.getKeys()
################################################################################
{
	local hash="$1"
	local type="$2"
	local hash_dir
	local fn
	local key
	local list=""
	
	# Work out the directory
	hash_dir="$(_common.hash.getDir "$type")"
	
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
		while [[ $(pwd) != / ]]
		do
			# If we find CAMxRunner, we are there
			ls CAMxRunner.sh >/dev/null 2>&1 && break
			
			# If we are in root, we have gone too far
			if [[ $(pwd) == / ]]
			then
				echo "Could not find CAMxRunner.sh!"
				exit 1
			fi
			
			cd ..
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
	common.hash.init test_instance $CXR_HASH_TYPE_INSTANCE
	common.hash.put test_instance $CXR_HASH_TYPE_INSTANCE /hallo/gugs SomeOtherValue
	common.hash.put test_instance $CXR_HASH_TYPE_INSTANCE /hallo/velo SomeOtherValue

	
	# Glabal Hash with strange keys
	common.hash.init test_global $CXR_HASH_TYPE_GLOBAL
	common.hash.put test_global $CXR_HASH_TYPE_GLOBAL "This key has spaces" "a value"
	common.hash.put test_global $CXR_HASH_TYPE_GLOBAL "This key also has spaces" "another value"
	
	# Universal Hash
	common.hash.init test_universal $CXR_HASH_TYPE_UNIVERSAL
	common.hash.put test_universal $CXR_HASH_TYPE_UNIVERSAL /hallo/gugs SomeOtherValue
	common.hash.put test_universal  $CXR_HASH_TYPE_UNIVERSAL /hallo/velo SomeOtherValue

	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is "$(common.hash.get test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo")" SomeOtherValue "common.hash.get test_instance with path as key"
	is "$(common.hash.has? test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo")" true "common.hash.has? test_instance with path as key"
	is "$(common.hash.getKeys test_instance $CXR_HASH_TYPE_INSTANCE)" '"/hallo/gugs" "/hallo/velo"' "common.hash.getKeys test_instance with path as key"
	
	# Now lets iterate over keys
	for key in $(common.hash.getKeys test_instance $CXR_HASH_TYPE_INSTANCE)
	do
		is "$(common.hash.get test_instance $CXR_HASH_TYPE_INSTANCE "$key")" SomeOtherValue "Going trough keys in an interator"
	done
	
	# Lets retrieve those with spaces
	is "$(common.hash.get test_global $CXR_HASH_TYPE_GLOBAL "This key has spaces")" "a value" "common.hash.get test_instance - key with spaces"
	
	common.hash.delete test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo"
	is "$(common.hash.has? test_instance $CXR_HASH_TYPE_INSTANCE "/hallo/velo")" false "common.hash.delete test_instance with path as key"

	
	is "$(common.hash.get test_universal $CXR_HASH_TYPE_UNIVERSAL "/hallo/velo")" SomeOtherValue "common.hash.get test_universal with path as key"
	is "$(common.hash.has? test_universal $CXR_HASH_TYPE_UNIVERSAL "/hallo/velo")" true "common.hash.has? test_universal with path as key"
	is "$(common.hash.getKeys test_universal $CXR_HASH_TYPE_UNIVERSAL)" '"/hallo/gugs" "/hallo/velo"' "common.hash.getKeys test_universal with path as key"
	
	common.hash.delete test_universal $CXR_HASH_TYPE_UNIVERSAL "/hallo/velo" 
	is "$(common.hash.has? test_universal $CXR_HASH_TYPE_UNIVERSAL "/hallo/velo")" false "common.hash.delete test_universal with path as key"

	########################################
	# teardown tests if needed
	########################################
	common.hash.destroy test_instance $CXR_HASH_TYPE_INSTANCE
	common.hash.destroy test_global $CXR_HASH_TYPE_GLOBAL
	common.hash.destroy test_universal $CXR_HASH_TYPE_UNIVERSAL
	
	if [[ "${CXR_TESTING_FROM_HARNESS:-false}" == false ]]
	then
		# We where called stand-alone, cleanupo is needed
		main.doCleanup
	fi
	
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


