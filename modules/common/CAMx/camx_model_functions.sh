# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
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
# TODO: 
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
# This is checked by "main_announce_and_check_module" at the end of the module
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=1

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
		main.dieGracefully "need the name of the Chemical and Aerosol Mechanism!"
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
				main.dieGracefully "Cannot determine name of chemparam file (parameter CXR_CHEMPARAM_INPUT_FILE), tried ${MY_CHEMPARAM_INPUT_FILE}"
			fi
		fi
	fi
	
	# found one
	if [[ ${CXR_RUN_MODEL} == true  ]]
	then
		# Report only if we run the model
		main.log -a "Using chemparam file (${our_chemparam})."
		
		# Also report MD5
		common.check.reportMD5 "${our_chemparam}"
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
# > ${CXR_MODEL_BIN_DIR}/${CXR_MODEL}-v${CXR_MODEL_VERSION}-${CXR_PARALLEL_PARADIGM}-${CXR_PROBING_TOOL}-${HOSTTYPE}[-$(uname -n)] (e. g. CAMx-v4.51-OMP-None-x86_64 or CAMx-v4.51-OMP-None-x86_64-lcsl5a)
# Parameters:
# [$1] - if false - ignore that file does not exist (necessary during installation)
################################################################################
function get_model_exec()
################################################################################
{
	#Determine possible names
	local general_exec
	local machine_exec
	local run_exec
	
	general_exec=${CXR_MODEL_BIN_DIR}/${CXR_MODEL}-v${CXR_MODEL_VERSION}-${CXR_PARALLEL_PARADIGM}-${CXR_PROBING_TOOL}-${HOSTTYPE}
	machine_exec=${general_exec}-$(uname -n)
	run_exec=${CXR_MODEL_BIN_DIR}/${CXR_RUN}-${HOSTTYPE}
	
	# Check name - run first
	if [[ -x ${run_exec}  ]]
	then
		# Run dependent exists
		main.log -v   "CAMx Binary is actually called ${run_exec}"
		echo "${run_exec}"
	elif [[ -x ${machine_exec}  ]]
	then
		# Machine dependent exists
		main.log -v   "CAMx Binary is actually called ${machine_exec}"
		echo "${machine_exec}"
	elif [[ -x ${general_exec}  ]]
	then
		# general exists
		main.log -v   "CAMx Binary is actually called ${general_exec}"
		echo "${general_exec}"
	else
		#None exists
		if [[  "${1:-true}" == false || ${CXR_RUN_MODEL} == false   ]]
		then
			# optional paratemeter is false, or we do not run the model
			# We do not care and return general_exec
			echo "${general_exec}" 
		else
			# NOK - just issue a warning (if we are compiling e.g.)
			main.log -w  "Could not find a suitable CAMx executable, neither ${machine_exec} nor ${general_exec} are present.\nRecompile CAMx using\n\t\$ CAMxRunner.sh -I\n(Ignore this message during installation :-)"
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
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(basename $(get_chemparam_file 6 CF)) CAMx4.5.chemparam.6_CF "get_chemparam_file 6 CF"

	########################################
	# teardown tests if needed
	########################################
	
}
