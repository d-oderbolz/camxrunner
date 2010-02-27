#!/usr/bin/env bash
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains the Date functions
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# TODO: Implement retval check in execute_model
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
# This is checked by "main_announce_and_check_module" at the end of the module
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=1

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=400

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=400

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the functions to call CAMx"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2009), CAMxRunner@psi.ch"

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

	${progname} - A part of the CAMxRunner tool chain.

	Is designed to be called by the CAMxRunner.
	
	You can, however, call it like this:
	
	$ ${progname} -t
	
	this starts the self-test of the module.

	
	Written by ${CXR_META_MODULE_AUTHOR}
	License: ${CXR_META_MODULE_LICENSE}
	
	Find more info here:
	${CXR_META_MODULE_DOC_URL}
EOF
exit 1
}

################################################################################
# Function: get_chemparam_file
#
# Tries to determine the name of the chemparam file - complains if a problem exists
# (e. g. requiring aerosols for unsupported mechanism).
# Also reports MD5 hash of the file, if needed.
# Check in this order:
# - Run scecific (${CXR_RUN}_chemparam)
# - General (CAMx${CXR_MODEL_VERSION}.chemparam.${CHEMICAL_MECHANISM}_${AEROSOL_MECHANISM}
#
# Parameters:
# $1 - the chemical mechanism
# $2 - the aerosol mechanism
################################################################################
function get_chemparam_file()
################################################################################
{
	if [[ $# -ne 2  ]]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - need the name of the Chemical and Aerosol Mechanism!"
	fi
	
	local our_chemparam
	RUN_SPECIFIC=${CXR_MODEL_BIN_DIR}/chemparam/${CXR_RUN}_chemparam
	
	# First check if there is a run-specific file
	if [[ -r "${RUN_SPECIFIC}"  ]]
	then
		our_chemparam="${RUN_SPECIFIC}"
	else
		# No specific one found, check further
		
		CHEMICAL_MECHANISM=$1
		AEROSOL_MECHANISM=$2
	
		MY_MECHANISM=${1}_${2}
	
		MY_CHEMPARAM_INPUT_FILE=${CXR_MODEL_BIN_DIR}/chemparam/CAMx${CXR_MODEL_VERSION}.chemparam.${MY_MECHANISM}
	
		if [[ -r "${MY_CHEMPARAM_INPUT_FILE}"  ]]
		then
			our_chemparam="${MY_CHEMPARAM_INPUT_FILE}"
		else
			# File does not exist, maybe the file is not for 4.51 but for 4.5 (as an examlple)
			# Maybe we have to cut off the last digit of the Version number
			MY_CHEMPARAM_INPUT_FILE=${CXR_MODEL_BIN_DIR}/chemparam/CAMx${CXR_MODEL_VERSION:0:3}.chemparam.${MY_MECHANISM}
	
			if [[ -f "${MY_CHEMPARAM_INPUT_FILE}"  ]]
			then
				our_chemparam="${MY_CHEMPARAM_INPUT_FILE}"
			else
				# File still does not exist
				cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Cannot determine name of chemparam file (parameter CXR_CHEMPARAM_INPUT_FILE), tried ${MY_CHEMPARAM_INPUT_FILE}"
			fi
		fi
	fi
	
	# found one
	if [[ ${CXR_RUN_MODEL} == true  ]]
	then
		# Report only if we run the model
		cxr_main_logger -a $FUNCNAME "Using chemparam file (${our_chemparam})."
		
		# Also report MD5
		if [[ "${CXR_REPORT_MD5}" == true  ]]
		then
			cxr_main_logger -a "$FUNCNAME" "MD5 Hash of ${our_chemparam} is $(cxr_common_md5 ${our_chemparam})"
		fi
	fi
	
	echo "${our_chemparam}"
}

################################################################################
# Function: get_model_exec
#
# Returns the full path name of the CAMx Executable depending on current settings.
# The name depends on:
# - CXR_PARALLEL_PARADIGM (None, OMP or MPI)
# - The Machine Architecture
# - Whether probing is needed or not.
# - (The machine name)
# 
# First it tries to find a run-specific binary, then a machine dependent one, if this is not found, 
# a general one is seeked.
#
# Normally fails if no suitable binary was found - if the optional parameter is false
# ignores this fact. In this case, the name of a general binary without machine
# name is returned. Also we ignore a missing binary if we do not run the model.
#
# It looks like this:
#
# > ${CXR_MODEL_BIN_DIR}/CAMx-${CXR_PARALLEL_PARADIGM}-${CXR_PROBING_TOOL}-${HOSTTYPE}[-$(uname -n)] (e. g. CAMx-OMP-None-x86_64 or CAMx-OMP-None-x86_64-lcsl5a)
# Parameters:
# [$1] - if false - ignore that file does not exist (necessary during installation)
################################################################################
function get_model_exec()
################################################################################
{
	#Determine possible names
	GENERAL_EXEC=${CXR_MODEL_BIN_DIR}/CAMx-${CXR_PARALLEL_PARADIGM}-${CXR_PROBING_TOOL}-${HOSTTYPE}
	MACHINE_EXEC=${GENERAL_EXEC}-$(uname -n)
	RUN_EXEC=${CXR_MODEL_BIN_DIR}/${CXR_RUN}-${HOSTTYPE}
	
	# Check name - run first
	if [[ -x ${RUN_EXEC}  ]]
	then
		# Run dependent exists
		cxr_main_logger -v "${FUNCNAME}"  "CAMx Binary is actually called ${RUN_EXEC}"
		echo "${RUN_EXEC}"
	elif [[ -x ${MACHINE_EXEC}  ]]
	then
		# Machine dependent exists
		cxr_main_logger -v "${FUNCNAME}"  "CAMx Binary is actually called ${MACHINE_EXEC}"
		echo "${MACHINE_EXEC}"
	elif [[ -x ${GENERAL_EXEC}  ]]
	then
		# general exists
		cxr_main_logger -v "${FUNCNAME}"  "CAMx Binary is actually called ${GENERAL_EXEC}"
		echo "${GENERAL_EXEC}"
	else
		#None exists
		if [[  "${1:-true}" == false || ${CXR_RUN_MODEL} == false   ]]
		then
			# optional paratemeter is false, or we do not run the model
			# We do not care and return CXR_GENERAL_EXEC
			echo "${GENERAL_EXEC}" 
		else
			# NOK - just issue a warning (if we are compiling e.g.)
			xr_main_logger -w "${FUNCNAME}" "Could not find a suitable CAMx executable, neither ${MACHINE_EXEC} nor ${GENERAL_EXEC} are present.\nRecompile CAMx using\n\t\$ CAMxRunner.sh -I\n(Ignore this message during installation :-)"
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
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(basename $(get_chemparam_file 6 CF)) CAMx4.5.chemparam.6_CF "get_chemparam_file 6 CF"

	########################################
	# teardown tests if needed
	########################################

}

################################################################################
# Are we running stand-alone? 
################################################################################


# If the CXR_META_MODULE_NAME  is a subset of the progname,
# somebody started this script alone
# Normlly this is not allowed, exept to test using -t
if [[ $(expr match "${progname}" ".*$CXR_META_MODULE_NAME.*") -gt 0  ]]
then

	while getopts ":t" opt
	do
		case "${opt}" in
		t) test_module;;
		esac
	done
	
	# This is not strictly needed, but it allows to read 
	# non-named command line options
	shift $((${OPTIND} - 1))

	# Make getopts ready again
	unset OPTSTRING
	unset OPTIND
	
	usage
	
fi

################################################################################
# Finalisation - not executed in stand-alone operation
################################################################################
