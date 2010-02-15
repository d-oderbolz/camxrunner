#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains CAMx Runner Functions
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# 
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=1

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains installation for the CAMxRunner (the modules needed are in the installer directory)"

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
	Start the installation using the -I Option.

	Written by $CXR_META_MODULE_AUTHOR
	License: $CXR_META_MODULE_LICENSE
	
	Find more info here:
	$CXR_META_MODULE_DOC_URL
EOF
exit 1
}

################################################################################
# Function: cxr_common_install
#	
# * Interactively installs the CAMxRunner, CAMx and the testcase.
# * TODO: Use the state DB to keep track of what is installed already.
# * Uses <cxr_common_run_modules> to loop through the relevant files under ${CXR_INSTALLER_INPUT_DIR}
#   and executes them in order
################################################################################
function cxr_common_install()
################################################################################
{
	# What we do here is similar to the pre- or postprocessors:
	# - go through all the files in the installer/$CXR_MODEL_VERSION directory
	# - Run them (while checking they are not yet run)
	# The interactivity is mostly hidden in these modules
	
	# Do we do this?
	if [ "$(cxr_common_get_consent "Do you want to run the installer for the CAMxRunner, some converters, model and the testcase \n (you can select the steps you want later)" Y )" == false ]
	then
		exit
	fi
	
	cxr_main_logger -a "$FUNCNAME"  "Checking internal files..."
	
	cxr_common_check_runner_executables
	
	MODEL=$(cxr_common_get_menu_choice "Which model should be installed?\nIf your desired model is not in this list, adjust CXR_SUPPORTED_MODELS \n(Currently $CXR_SUPPORTED_MODELS) - of course the installer needs to be extended too!" "$CXR_SUPPORTED_MODELS" "CAMx")
	
	MODEL_ID=$(cxr_common_get_model_id "$MODEL") || cxr_main_die_gracefully "Model $MODEL is not known."
	
	# Extract the list of supported versions
	SUPPORTED="${CXR_SUPPORTED_MODEL_VERSIONS[${MODEL_ID}]}"
	
	# Set the default to the first entry
	# Save old IFS
	oIFS="$IFS"

	IFS="$CXR_SPACE"
	
	# Suck line into array
	ARRAY=($SUPPORTED)
	
	# Reset IFS
	IFS="$oIFS"
	
	DEFAULT_VERSION=${ARRAY[0]}

	#Generate a menu automatically
	VERSION=$(cxr_common_get_menu_choice "Which version of $MODEL should be used?\nIf your desired version is not in this list, adjust CXR_SUPPORTED_MODEL_VERSIONS \n(Currently $SUPPORTED)" "$SUPPORTED" "$DEFAULT_VERSION")
	
	cxr_common_is_version_supported $VERSION $MODEL
	
	cxr_main_logger "${FUNCNAME}" "Installing system for $MODEL $VERSION..."
	
	# reload config for this version (the run is called "installer")
	cxr_main_read_config "installer" "$VERSION" "$MODEL" "$CXR_RUN_DIR"
	
	# Run the required modules (we could even select them!)
	cxr_common_run_modules ${CXR_TYPE_INSTALLER}

	cxr_main_logger -a -b "${FUNCNAME}" "All installation actions finished."
}

################################################################################
# Function: cxr_common_determine_patch_target
#	
# Parses a Patch file and finds out the target file base name.
# This allows us to name the patches arbitrarily.
#
# Parameters:
# $1 - Full Path to the patch in question
################################################################################
function cxr_common_determine_patch_target()
################################################################################
{
	PATCH=${1:-}
	
	if [ ! -f "$PATCH" ]
	then
		cxr_main_die_gracefully "$FUNCNAME - no filename passed or file not readable!"
	fi
	
	# Simple parser, we look for (See also http://en.wikipedia.org/wiki/Diff)
	# +++ /path/to/new some other stuff might be here
	# I know I spawn 2 procs here...
	FILE=$(basename $(grep -h '+++' "$PATCH" | head -n1 | cut -f2 -d' '))
	
	if [ "$FILE" ]
	then
		echo "$FILE"
	else
		cxr_main_die_gracefully "$FUNCNAME - Could not find any filename"
	fi
	
}

################################################################################
# Function: cxr_common_apply_patches
#	
# Recursively patches a number of files in a directory by applying patches in another directory.
# Refer to CAMxRunner Developers Handbook on patch naming convention.
#
# .svn directories are ignored
#
# Parameters:
# $1 - Directory containig the patches
# $2 - Directory containig the files to be patched
# $3 - ASK_USER, if false do not prompt user for consent, default true
################################################################################
function cxr_common_apply_patches()
################################################################################
{

	ASK_USER="${3:-true}"
	
	PATCH_DIR="$1"
	SRC_DIR="$2"
	
	if [ ! -d "$PATCH_DIR" -o ! -d "$SRC_DIR" ]
	then
		cxr_main_die_gracefully "$FUNCNAE:$LINENO - needs two existing directories as input, either $PATCH_DIR or $SRC_DIR not found."
	fi
	
	cxr_main_logger "${FUNCNAME}" "Applying patches in $PATCH_DIR to $SRC_DIR..."
	
	# Create a list of all patches in all Subdirectories of $PATCH_DIR
	PATCHLIST=$(cxr_common_create_tempfile $FUNCNAME)
	
	# Prepare the files containing all patches and no .svn stuff
	find $PATCH_DIR -noleaf -type f -name \*.patch | grep -v ".svn" > $PATCHLIST
	
	# Loop through all patches
	
	# The canonical way to loop through a file is
	# while read LINE
	# do
	# done

	# This is not possible because I want to read input from stdin as well.
	# Therefore, I use a slightly more cumbersome/inefficient construct:
	CURLINE=1
	while [ $CURLINE -le $(wc -l < $PATCHLIST) ]
	do
		# Read the current line (I know this is not nice, read comment above)
		# Read first $CURLINE lines
		# the last of which is line $CURLINE
		PATCH_FILE=$(head -n $CURLINE $PATCHLIST | tail -n 1)
		
		# Which file do we need to patch?
		FILE=$(cxr_common_determine_patch_target $PATCH_FILE)
		
		##########
		# Get the relative path of the current patch
		##########
		
		# How long is the part we need to romeve?
		# we add 1 because of /
		LEN_PATCH_DIR=$(( ${#PATCH_DIR} + 1 ))

		# Give back substring starting at position $LEN_PATCH_DIR
		# and then the "path-part"
		CURRENT_DIR=$(dirname ${PATCH_FILE:$LEN_PATCH_DIR})
		
		# Complete path of the File to patch
		REAL_FILE=$SRC_DIR/$CURRENT_DIR/$FILE
		
		cxr_main_logger -v "${FUNCNAME}"  "${FUNCNAME}:${LINENO} - $PATCH_FILE\nREAL_FILE: $REAL_FILE"

		if [ -f $REAL_FILE ]
		then
			if [ "$ASK_USER" == true ]
			then
				# Ask user
				if [ "$(cxr_common_get_consent "Do you want to apply the patch $PATCH_FILE to $REAL_FILE?\nCheck if the patch is compatible with the current platform." Y )" == true ]
				then
					patch $REAL_FILE < $PATCH_FILE
				fi
			else
				# Just do it
				patch $REAL_FILE < $PATCH_FILE
			fi
		else
			cxr_main_logger -e "${FUNCNAME}" "File $REAL_FILE does not exist. Check the header of $PATCH_FILE!"
		fi
		
		# Increment
		CURLINE=$(( $CURLINE + 1 ))
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
	
	# Create a broken patch
	# According to spec, we consider only the first one
	a=$(cxr_common_create_tempfile)
	echo "+++ /path/to/new" > $a
	echo "+++ /path/to/newer" >> $a
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(cxr_common_determine_patch_target $a) new "cxr_common_determine_patch_target of a broken patch"

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
if [ -z "${CXR_META_MODULE_NAME:-}" ]
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
	if [ "${TEST_IT:-false}" == true ]
	then
		test_module
	else
		usage
	fi

fi