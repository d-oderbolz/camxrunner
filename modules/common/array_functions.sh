# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Title: Array Functions
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

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=3

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the Array functions for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: common.array.allElementsZero?
#	
# Checks if the values in a given array are all zero and returns true if its ok (all 0),
# false otherwise.
#
# Example:
# > a=$(ls | wc -k 2>/dev/null )
# >if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
# >then
#	>	main.log -e "Pipe at $LINENO:failed"
# >fi
#
# Parameters:
# $1 - The array to test, pass it as "${array[@]}" (including quotes!)
################################################################################
function common.array.allElementsZero?() 
################################################################################
{
	# Here we store our status, default is OK
	local status
	local i_arr
	
	status=true
	
	# We must suck the array passed as list into an array again
	local array
	
	array=( "$@" )
	
	for i_arr in $(seq 0 $(( ${#array[@]} - 1 )) )
	do
		if [[ "${array[$i_arr]}" != 0 ]]
		then
			status=false
		fi
	done
	
	echo $status
}

################################################################################
# Function: common.array.exportArray
#
# Exports / Serialises an array to the environment.
#
# Bash does not support exporting arrays easily, they must be
# serialised to a string, which is done here and then can be deserialised using <common.array.importArrays>
#
# The name of the serialised array is the original name with _ARR_X at the end
#
# This function is deprecated but if might be useful to allow communication over
# environment variables
# 
# Parameters:
# $1 - Name of the array to serialise
################################################################################
function common.array.exportArray()
################################################################################
{
	# Example of idea:
	# CXR_NUMBER_OF_LAYERS_ARR_X=$(printf '%q ' "${CXR_NUMBER_OF_LAYERS[@]}")

	export ${1}_ARR_X="$(printf '%q ' $(eval "echo \${${1}[@]}"))"
}

################################################################################
# Function: common.array.importArrays
#	
# Imports arrays into the current scope
# Was needed because we used env to reflect about arrays
# and bash cannot really export arrays... (now with set that is no longer an issue)
# Partner function of <common.array.exportArray>
################################################################################
function common.array.importArrays()
################################################################################
{
	# Bash does not support exporting arrays easily, they must be
	# serialised to a string and then be deserialised, which is done here
	# These arrays are all 0-indexed, therefore the index 0 positions are pure dummies
	
	local var
	local arr_name
	
	for var in $(set | sort | grep ^CXR_.*_ARR_X= | cut -d= -f1)
	do
		# With this pattern, we extract the name of the Array
		arr_name=$(expr match $var '\(.*\)_ARR_X$')  || :
		
		# Then we import
		# This code is due to stephane_chazelas_at_yahoo.fr - http://unix.derkeiler.com/Newsgroups/comp.unix.shell/2003-05/0603.html
		# Idea: eval "CXR_OUTPUT_SPECIES_NAMES=( ${CXR_OUTPUT_SPECIES_NAMES_ARR_X[*]} )"
		# This is its dynamic form:
		set $arr_name=$(eval "echo \${${var}[*]}")
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
	########################################
	# Setup tests if needed
	########################################
	
	# Arrays to count zeros
	a=(1 1 1 1)
	b=(0 0 0 0)
	c=(-1 1 1 -1)
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	is $(common.array.allElementsZero? "${a[@]}") false "common.array.allElementsZero? all 1"
	is $(common.array.allElementsZero? "${b[@]}") true "common.array.allElementsZero? all 0"
	is $(common.array.allElementsZero? "${c[@]}") false "common.array.allElementsZero? all cancelling out"
	
	########################################
	# teardown tests if needed
	########################################
}
