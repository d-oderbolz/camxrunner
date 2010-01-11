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
CXR_META_MODULE_NUM_TESTS=0

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
# Function: get_chemparm_file
#
# Tries to determine the name of the chemparm file - complains if a problom exists
# (e. g. requiring aerosols for unsupported mechanism).
# Check in this order:
# - Run scecific (${CXR_RUN}_chemparam)
# - General (CAMx${CXR_MODEL_VERSION}.chemparam.${CHEMICAL_MECHANISM}_${AEROSOL_MECHANISM}
#
# Parameters:
# $1 - the chemical mechanism
# $2 - the aerosol mechanism
################################################################################
function get_chemparm_file()
################################################################################
{
	if [ $# -ne 2 ]
	then
		cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - need the name of the Chemical and Aerosol Mechanism!"
	fi
	
	RUN_SPECIFIC=${CXR_CAMX_DIR}/chemparam/${CXR_RUN}_chemparam
	
	# First check if there is a run-specific file
	if [ -f "${RUN_SPECIFIC}" ]
	then
		cxr_main_logger $FUNCNAME "Using run-specific chemparam file (${RUN_SPECIFIC})."
	
		echo "${RUN_SPECIFIC}"
		
		return $CXR_RET_OK
	fi
	
	CHEMICAL_MECHANISM=$1
	AEROSOL_MECHANISM=$2

	MY_MECHANISM=${1}_${2}

	MY_CHEMPARAM_INPUT_FILE=${CXR_CAMX_DIR}/chemparam/CAMx${CXR_MODEL_VERSION}.chemparam.${MY_MECHANISM}

	if [ ! -f "${MY_CHEMPARAM_INPUT_FILE}" ]
	then
		# Maybe we have to cut off the last digit of the Version number
		MY_CHEMPARAM_INPUT_FILE=${CXR_CAMX_DIR}/chemparam/CAMx${CXR_MODEL_VERSION:0:3}.chemparam.${MY_MECHANISM}

		if [ ! -f "${MY_CHEMPARAM_INPUT_FILE}" ]
		then
			cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Cannot determine name of chemparam file (parameter CXR_CHEMPARAM_INPUT_FILE), tried ${MY_CHEMPARAM_INPUT_FILE}"
		else
			cxr_main_logger $FUNCNAME "Using general chemparam file (${MY_CHEMPARAM_INPUT_FILE})."
		fi
	fi
	echo "${MY_CHEMPARAM_INPUT_FILE}"
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
# name is returned.
#
# It looks like this:
#
# > ${CXR_CAMX_DIR}/CAMx-${CXR_PARALLEL_PARADIGM}-${CXR_PROBING_TOOL}-${HOSTTYPE}[-$(uname -n)] (e. g. CAMx-OMP-None-x86_64 or CAMx-OMP-None-x86_64-lcsl5a)
# Parameters:
# [$1] - if false - ignore that file does not exist (necessary during installation)
################################################################################
function get_model_exec()
################################################################################
{
	#Determine possible names
	GENERAL_EXEC=${CXR_CAMX_DIR}/CAMx-${CXR_PARALLEL_PARADIGM}-${CXR_PROBING_TOOL}-${HOSTTYPE}
	MACHINE_EXEC=${GENERAL_EXEC}-$(uname -n)
	RUN_EXEC=${CXR_CAMX_DIR}/${CXR_RUN}-${HOSTTYPE}
	
	# Check name - run first
	if [ -x ${RUN_EXEC} ]
	then
		# Run dependent exists
		cxr_main_logger -v "${FUNCNAME}"  "CAMx Binary is actually called ${RUN_EXEC}"
		echo "${RUN_EXEC}"
	elif [ -x ${MACHINE_EXEC} ]
	then
		# Machine dependent exists
		cxr_main_logger -v "${FUNCNAME}"  "CAMx Binary is actually called ${MACHINE_EXEC}"
		echo "${MACHINE_EXEC}"
	elif [ -x ${GENERAL_EXEC} ]
	then
		# general exists
		cxr_main_logger -v "${FUNCNAME}"  "CAMx Binary is actually called ${GENERAL_EXEC}"
		echo "${GENERAL_EXEC}"
	else
		#None exists
		if [ "${1:-true}" == false ]
		then
			# optional paratemeter is false,
			# We do not care and return CXR_GENERAL_EXEC
			echo "${GENERAL_EXEC}" 
		else
			# NOK - we fail
			cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Could not find a suitable CAMx executable, neither ${MACHINE_EXEC} nor ${GENERAL_EXEC} are present.\nRecompile CAMx using\n\t\$ CAMxRunner.sh -I\n(Ignore this message during installation :-)"
		fi
	fi
}


################################################################################
# Are we running stand-alone? 
################################################################################


# If the CXR_META_MODULE_NAME  is a subset of the progname,
# somebody started this script alone
# Normlly this is not allowed, exept to test using -t
if [ $(expr match "${progname}" ".*$CXR_META_MODULE_NAME.*") -gt 0 ]
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
