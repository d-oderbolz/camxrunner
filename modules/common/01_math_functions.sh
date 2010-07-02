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

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=23
# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains some date functions for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2009), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'


################################################################################
# Function: common.math.FloatOperation
#
# Performs FP arithmetic (other than $(()) )
# If the result is a whole number, a dot is appended at the end (FORTRAN compatibility)
# TODO: Invert default behaviour (most callers do not expect FT compatibility)
#
# Parameters:
# $1 - an expression
# $2 - an optional scale parameter (default CXR_NUM_DIGITS). -1 Means truncate
# $3 - an optional boolean parameter indicating if a trailing decimal should be added (default: true)
################################################################################
function common.math.FloatOperation()
################################################################################
{
	if [[ $# -lt 1 ]]
	then
		main.dieGracefully  "needs at least an expression as input"
	fi
	
	# Define & Initialize local vars
	local resolution=${2:-$CXR_NUM_DIGITS}
	local add_trailing_dp=${3:-true}
	local result
	local bc_res
	
	# Fix resolution (-1 is just a marker)
	if [[ "$resolution" -eq -1 ]]
	then
		bc_res=0
	else
		bc_res=$resolution
	fi
	
	# Set resolution & pass expression
	result=$( echo "scale=$bc_res; $1" | bc )
	
	if [[ "$resolution" -eq -1 ]]
	then
		# Chop off the decimals
		result=${result%%\.*}
	fi
	
	# The scale function counts digits after the decimal point
	if [[  "${add_trailing_dp}" == true && "$( echo "scale(${result})" | bc )" -eq 0   ]]
	then
		# Integer,  and we need to add a trailing .
		echo ${result}.
	else
		echo ${result}
	fi

}

################################################################################
# Function: common.math.abs
#
# Calculates the absolute value of a number
#
# Parameters:
# $1 - number to be treated
################################################################################
function common.math.abs()
################################################################################
{
	if [[  $# -ne 1 || "$(main.isNumeric? "$1")" == false ]]
	then
		main.log -e  "needs a number as input"
		echo false
		return $CXR_RET_ERROR
	fi
	
	# Is the number 0?
	if [[ "$(common.math.FloatOperation "$1 == 0" -1 false)" == 1 ]]
	then
		echo 0
	# Is the number negative?
	elif [[ "$(common.math.FloatOperation "$1 < 0" "" false)" == 1 ]]
	then
		echo "$(common.math.FloatOperation "0 - $1" "" false)"
	else
		echo $1
	fi
	
	return $CXR_RET_OK
}


################################################################################
# Function: common.math.sumVector
#
# Calculates the sum of a list of values passed as a string-delimited list (FP)
#
# Parameters:
# $1 - a list of numeric values (string-delimited)
# $2 - an optional scale parameter (default -1)
################################################################################
function common.math.sumVector()
################################################################################
{
	if [[ $# -lt 1 || -z "$1" ]]
	then
		main.log -e  "needs a list of numeric values as input"
		echo 0
		return $CXR_RET_ERROR
	fi
	
	local list="${1}"
	local scale="${2:--1}"
	local iElement
	local result=0
	
	arr=( $list )
	
	# We might get many elements - seq by default does Engineering notation
	for iElement in $(seq -f"%.0f" 0 $(( ${#arr[@]} - 1 )))
	do
		result="$(common.math.FloatOperation "$result + ${arr[$iElement]}" "$scale" false)"
	done
	
	echo $result

}

################################################################################
# Function: common.math.meanVector
#
# Calculates the arithmetic mean of a list of values passed as a string-delimited list
# By default we return an integer value (calculation is carried out FP)
#
# Parameters:
# $1 - a list of numeric values (string-delimited)
# $2 - an optional scale parameter (default -1)
################################################################################
function common.math.meanVector()
################################################################################
{
	if [[ $# -lt 1 || -z "$1" ]]
	then
		main.log -e  "needs a list of numeric values as input"
		echo 0
		return $CXR_RET_ERROR
	fi
	
	local list="${1}"
	local scale="${2:--1}"
	
	nValues=$(main.countDelimitedElements "$list" " ")
	
	# Just to be on the safe side
	if [[ $nValues -lt 1 ]]
	then
		main.log -e  "needs a list of numeric values as input"
		echo 0
		return $CXR_RET_ERROR
	fi
	
	sum=$(common.math.sumVector "$list" "$scale")
	
	result=$(common.math.FloatOperation "$sum / $nValues" "$scale" false)
	
	echo $result
}

################################################################################
# Function: common.math.stdevVector
#
# Calculates the standard deviation of a list of values passed as a string-delimited list.
#
# Parameters:
# $1 - a list of numeric values (string-delimited)
# [$2] - the mean value (if not given, will be calculated)
################################################################################
function common.math.stdevVector()
################################################################################
{
	if [[ $# -lt 1 || -z "$1" ]]
	then
		main.log -e  "needs a list of numeric values as input"
		echo 0
		return $CXR_RET_ERROR
	fi
	
	local list="${1}"
	local mean="${2:-}"
	local SumSquaredDeviation=0
	
	nValues=$(main.countDelimitedElements "$list" " ")
	
	# Just to be on the safe side
	if [[ $nValues -lt 1 ]]
	then
		main.log -e  "needs a list of numeric values as input"
		echo 0
		return $CXR_RET_ERROR
	fi
	
	# Get mean if needed
	if [[ -z "$mean" ]]
	then
		main.log -v "Mean not given - we calculate it..."
		mean=$(common.math.meanVector "$list")
	fi
	
	arr=( $list )
	
	# We might get many elements - seq by default does Engineering notation
	for iElement in $(seq -f"%.0f" 0 $(( ${#arr[@]} - 1 )))
	do
		SumSquaredDeviation="$(common.math.FloatOperation "$SumSquaredDeviation + ( ${arr[$iElement]} - $mean )^2" "" false)"
	done

	result="$(common.math.FloatOperation "sqrt($SumSquaredDeviation / $nValues)" "" false)"
	
	echo $result
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
	
	is "$(common.math.abs 0)" 0 "common.math.abs of 0"
	is "$(common.math.abs -0)" 0 "common.math.abs of -0"
	is "$(common.math.abs 1)" 1 "common.math.abs of 1"
	is "$(common.math.abs -1)" 1 "common.math.abs of -1"
	is "$(common.math.abs -1000000)" 1000000 "common.math.abs of -1000000"
	is "$(common.math.abs -3.14159)" 3.14159 "common.math.abs of -3.14159"
	is "$(common.math.abs 3.14159)" 3.14159 "common.math.abs of 3.14159"
	
	is "$(common.math.FloatOperation "-1000000 * -1" )"  1000000. "common.math.FloatOperation -1000000 * -1"
	is "$(common.math.FloatOperation "12.43 * 2" )" 24.86 "common.math.FloatOperation 12.43 * 2"
	is "$(common.math.FloatOperation "5" )"  5. "common.math.FloatOperation 5 (trailing .)"
	is "$(common.math.FloatOperation "5" "" false )"  5 "common.math.FloatOperation 5 false (no trailing .)"
	is "$(common.math.FloatOperation "0.0000224784 * 300000" -1 false)" 6 "common.math.FloatOperation Truncation"

	is "$(common.math.sumVector "1 -1 1")" 1 "common.math.sumVector - alternating signs"
	is "$(common.math.sumVector "10.4" 1)" 10.4 "common.math.sumVector - single fp"
	is "$(common.math.sumVector "0.1 1.1 3.14159" 5)" 4.34159 "common.math.sumVector - many FP"
	main.log -a "Provocing error message - ignore"
	is "$(common.math.sumVector "")" 0 "common.math.sumVector - empty list"
	
	is "$(common.math.stdevVector "1 1 1 1 1")" 0 "common.math.stdevVector - all equal"
	is "$(common.math.stdevVector "1 2 3 4 5")" 1.4142 "common.math.stdevVector - 1 through 5"
	is "$(common.math.stdevVector "1 2 3 4 5" "3" )" 1.4142 "common.math.stdevVector - 1 through 5, passing mean"
	
	is "$(common.math.meanVector "1 1 1")" 1 "common.math.meanVector - all 1"
	is "$(common.math.meanVector "0 0 0")" 0 "common.math.meanVector - all 0"
	is "$(common.math.meanVector "1000")" 1000 "common.math.meanVector - single 1000"
	main.log -a "Provocing error message - ignore"
	is "$(common.math.meanVector "")" 0 "common.math.meanVector - empty list"

	########################################
	# teardown tests if needed
	########################################
	
}
