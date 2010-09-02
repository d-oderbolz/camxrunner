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

################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=0

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the functions to call PMCAMx"

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
# Totally static for now (we have only one file to deliver.)
#
# Parameters:
# $1 - the chemical mechanism
# $2 - the aerosol mechanism
################################################################################
function get_chemparam_file ()
################################################################################
{
	if [[ $# -ne 2  ]]
	then
		main.dieGracefully "need the name of the Chemical and Aerosol Mechanism!"
	fi
	
	RUN_SPECIFIC=${CXR_MODEL_BIN_DIR}/chemparam/${CXR_RUN}_chemparam
	
	# First check if there is a run-specific file
	if [[ -f "${RUN_SPECIFIC}"  ]]
	then
		main.log "Using run-specific chemparam file (${RUN_SPECIFIC})."
	
		echo "${RUN_SPECIFIC}"
		
		return $CXR_RET_OK
	fi
	
	CHEMICAL_MECHANISM=$1
	AEROSOL_MECHANISM=$2

	MY_MECHANISM=${1}_${2}

	# Static - we need more choice later$
	# The Rxns contains non-0 rate constants for the oxidation of Condensable gasses
	# While the other file does set these to 0.
	MY_CHEMPARAM_INPUT_FILE=${CXR_MODEL_BIN_DIR}/chemparam/CAMx4.chemparam.5.FINAL_SEMIvol_Rxns

	if [[ ! -f "${MY_CHEMPARAM_INPUT_FILE}"  ]]
	then
		main.dieGracefully "Cannot determine name of chemparam file (parameter CXR_CHEMPARAM_INPUT_FILE), tried ${MY_CHEMPARAM_INPUT_FILE}"
	else
		main.log "Using general chemparam file (${MY_CHEMPARAM_INPUT_FILE})."
	fi
	
	common.check.reportMD5 "${MY_CHEMPARAM_INPUT_FILE}"
	echo "$MY_CHEMPARAM_INPUT_FILE"
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
# > ${CXR_MODEL_BIN_DIR}/PMCAMx-${HOSTTYPE}[-$(uname -n)] (e. g. PMCAMx-x86_64 or PMCAMx-x86_64-lcsl5a)
# Parameters:
# [$1] - if false - ignore that file does not exist (necessary during installation)
################################################################################
function get_model_exec()
################################################################################
{
	#Determine possible names
	GENERAL_EXEC=${CXR_MODEL_BIN_DIR}/PMCAMx-${CXR_PARALLEL_PARADIGM}-${CXR_PROBING_TOOL}-${HOSTTYPE}
	MACHINE_EXEC=${GENERAL_EXEC}-$(uname -n)
	RUN_EXEC=${CXR_MODEL_BIN_DIR}/${CXR_RUN}-${HOSTTYPE}
	
	# Check name - run first
	if [[ -x ${RUN_EXEC}  ]]
	then
		# Run dependent exists
		main.log -v   "CAMx Binary is actually called ${RUN_EXEC}"
		echo "${RUN_EXEC}"
	elif [[ -x ${MACHINE_EXEC}  ]]
	then
		# Machine dependent exists
		main.log -v   "PMCAMx Binary is actually called ${MACHINE_EXEC}"
		echo "${MACHINE_EXEC}"
	elif [[ -x ${GENERAL_EXEC}  ]]
	then
		# general exists
		main.log -v   "PMCAMx Binary is actually called ${GENERAL_EXEC}"
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
			main.log -w  "Could not find a suitable PMCAMx executable, neither ${MACHINE_EXEC} nor ${GENERAL_EXEC} are present.\nRecompile CAMx using\n\t\$ CAMxRunner.sh -I\n(Ignore this message during installation :-)"
		fi
	fi
}

################################################################################
# Function: test_module
#
# Runs the predefined tests for this module. If you add or remove tests, please
# update CXR_META_module_NUM_TESTS in the header!
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
	
	# None yet.
	:
	
	########################################
	# teardown tests if needed
	########################################
	

	
}
