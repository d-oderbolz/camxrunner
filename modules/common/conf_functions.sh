# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Title: Functions to set module-specific config items (in a hash)
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################

################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=5

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL=""

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains functions to set module-specific config items (in a hash) for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'


################################################################################
# Function: common.conf.set
# 
# Stores a certain key value pair in the instance conf hash used to configure
# module behaviour. 
# By convention, the keys are of the form module_name.setting, where setting can also 
# indicate an array like module_name.setting[0]. 
# The argument passed looks like a traditional assignment (without spaces!), it is parsed in this
# function. For security reasons, we trim the variable name.
#
# Hashes:
# CXR_INSTANCE_HASH_CONF
#
# Parameters:
# $1 - statement of the form "module_name.setting=some_value"
################################################################################
function common.conf.set()
################################################################################
{
	local -a arr
	
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "Make sure to pass just one argument of the form module_name.setting=some_value. Got $*"
	fi
	
	oIFS="$IFS"
	IFS='='
	
	# Parse it using = as IFS
	arr=($1)
	
	IFS="$oIFS"
	
	# Store data
	common.hash.put $CXR_INSTANCE_HASH_CONF "$CXR_LEVEL_INSTANCE" "$(common.string.trim ${arr[0]})" "${arr[1]}"
}

################################################################################
# Function: common.conf.unset
#
# Removes a module-specific variable from the instance conf hash.
# Do remove all elements of an array variable, mark them with empty [] like this:
# > common.conf.unset "some_funky_module.array[]"
# Of course, single items can be removed effortless:
# > common.conf.unset "some_funky_module.array[1]"
# 
# No error is thrown if the variable in question is non-existent.
#
# Parameters:
# $1 - variable name of the form "module_name.setting"
################################################################################
function common.conf.unset()
################################################################################
{
	local variable
	local noBrackets
	
	variable=${1}
	
	# Is it a whole array?
	
	if [[ "${variable: -2}" == "[]" ]]
	then
		# Whole array
		noBrackets=${variable%[]}
		common.hash.delete $CXR_INSTANCE_HASH_CONF "$CXR_LEVEL_INSTANCE" "${noBrackets}[%]"
	else
		# normal case
		common.hash.delete $CXR_INSTANCE_HASH_CONF "$CXR_LEVEL_INSTANCE" "$variable"
	fi # whole array?
}

################################################################################
# Function: common.conf.get
# 
# Convenience function to retrieve items from the instance conf hash.
#
# Parameters:
# $1 - key of the form module.variable
################################################################################
function common.conf.get()
################################################################################
{
	local key
	
	key=${1}
	
	common.hash.get $CXR_INSTANCE_HASH_CONF "$CXR_LEVEL_INSTANCE" "$key" 
}

################################################################################
# Function: common.conf.enumerate
# 
# If you store a hierachy in the conf like
# a.b.c
# a.b.d
# a.b.e
#
# This function emumerates all entries if you pass in a.b
# You get a newline-separated list of variable|value pairs
#
# Parameters:
# $1 - start of a key of the form module.variable
################################################################################
function common.conf.enumerate()
################################################################################
{
	local key
	
	key=${1}
	
	common.hash.searchKeys "$CXR_INSTANCE_HASH_CONF" "$CXR_LEVEL_INSTANCE" "${key}%" 
}
################################################################################
# Function: test_module
#
# Runs the predefined tests for this module
# 
################################################################################
function test_module()
################################################################################
{
	########################################
	# Setup tests if needed
	########################################
	
	common.conf.set "some_funky_module.run=fast"
	common.conf.set some_funky_module.noquote=works
	
	common.conf.set some_funky_module.array[0]=element0
	common.conf.set some_funky_module.array[1]=element1
	
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is "$(common.conf.get some_funky_module.run)" fast "common.conf.set/get quoted scalar"
	is "$(common.conf.get some_funky_module.noquote)" works "common.conf.set/get unquoted scalar"
	
	is "$(common.conf.get some_funky_module.array[0])" element0 "common.conf.set/get array[0]"
	is "$(common.conf.get some_funky_module.array[1])" element1 "common.conf.set/get array[1]"
	
	is "$(common.conf.get anon_existent_module.test)" "" "common.conf.get non-existent value"
	
	########################################
	# teardown tests if needed
	########################################
	
	common.conf.unset "some_funky_module.run"
	common.conf.unset "some_funky_module.noquote"
	common.conf.unset "some_funky_module.array[]"
}

