# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains Functions to manipulate variables dynamically
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
CXR_META_MODULE_NUM_TESTS=0

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the variable functions for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2009), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: common.variables.list
#	
# Writes all CXR_* variables out
################################################################################
function common.variables.list()
################################################################################
{
	main.log -B "The current Environment looks like this:"
	
	# Increase global indent level
	main.increaseLogIndent
	
	for VAR in $(set | sort | grep ^CXR_.*= | cut -d= -f1)
	do
		# The ! allows for _indirection_
		main.log   "${VAR}=${!VAR}"
	done

	# Decrease global indent level
	main.decreaseLogIndent
}

################################################################################
# Function: common.variables.getValue
#	
# Extracts the value of a given (non-exported) variable from the environment.
# If the variable is not found, the empty string is returned and an error is throwed
#
# Parameters:
# $1 - item to find
################################################################################
function common.variables.getValue()
################################################################################
{
	local item=${1}
	
	main.log -a reading $item
	main.log -a $(set)
	
	set | grep ${item}= 2>&1 > /dev/null
	
	if [[ $? -ne 0 ]]
	then
		# variable not known!
		main.log -e "variable $item not found!"
		echo ""
	else
		main.log -v "${item}: ${!item}"

		# Return value (indirect)
		echo ${!item}
	fi
}

################################################################################
# Function: common.variables.listSystemVars
#	
# Writes important system variables out
################################################################################	
function common.variables.listSystemVars()
################################################################################
{
	local var
	
	# Increase global indent level
	main.increaseLogIndent

	CXR_OMP_VARS="NCPUS MPSTKZ OMP_NUM_THREADS"
	for var in $CXR_OMP_VARS
	do
		main.log   "$VAR: ${!VAR}"
	done
	
	# Decrease global indent level
	main.decreaseLogIndent
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
	
	#None yet
	:

	########################################
	# teardown tests if needed
	########################################
	
}