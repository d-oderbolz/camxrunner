# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# 
#
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=27

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=100

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=100

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains the String functions for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2009), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: common.string.isSubstringPresent?
#
# Returns true if a substring needle was found within haystack, false otherwise.
#
# Parameters:
# $1 - haystack, the string in which we search
# $2 - needle, the substring to be found
################################################################################
function common.string.isSubstringPresent?() 
################################################################################
{
	local haystack=$1
	local needle=$2
	
	if [[ -z "$haystack" || -z "$needle" ]]
	then
		# This can happen often and is not necessarily an error
		main.log -v "Either of these parameters is empty: $haystack $needle"
	fi
	
	
	local found=$(expr match " $haystack" ".*$needle.*")
	# For safety, here        ^ is a space, so that things never start at 0
	
	if [[ $found -gt 0 ]]
	then
		main.log -v "Substring $needle matches (partially) with $haystack"
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.string.toLower
#
# Converts the given argument into an all lower case string.
#
# Parameters:
# $1 - string to be converted
################################################################################
function common.string.toLower() 
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		echo ""
	else
		echo $1 | tr "[:upper:]" "[:lower:]"
	fi
} 

################################################################################
# Function: common.string.toUpper
#
# Converts the given argument into an all lower case string.
#
# Parameters:
# $1 - string to be converted
################################################################################
function common.string.toUpper() 
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		echo ""
	else
		echo $1 | tr "[:lower:]" "[:upper:]"
	fi
}

################################################################################
# Function: common.string.trim
#
# Trims spaces or other characters on both ends of a string.
#
# Parameters:
# $1 - the string to be trimmed
# [$2] - the character that should be removed (default: space-trims tabs, too)
################################################################################
function common.string.trim() 
################################################################################
{
	local string="${1}"
	local to_remove="${2:-" "}"
	local out
	
	if [[ $# -lt 1 || $# -gt 2 ]]
	then
		main.log -e  "Programming error: wrong call."
		echo ""
	else
		# trim in front
		out=${string##$to_remove}
		# and at the back
		out=${out%%$to_remove}
		
		echo $out
	fi
}

################################################################################
# Function: common.string.repeat
#
# Repeats a string n times. Does currently not support escape sequences like \t
# Other ideas: <http://www.unix.com/shell-programming-scripting/46584-repeat-character-printf.html>
# TODO: Make more efficient, accept escape sequences
#
# Parameters:
# $1 - the string to be repeated
# $2 - the number of repetitions
################################################################################
function common.string.repeat() 
################################################################################
{
	local string="${1}"
	local times="${2}"
	local dummy
	
	if [[ $# -ne 2 ]]
	then
		main.log -e  "Programming error: wrong call, we need 2 arguments."
		echo ""
	else
		# Create n spaces
		dummy="$(printf "%${times}s" "")"
		# Replace saces by string
		echo "${dummy// /$string}"
	fi
}

################################################################################
# Function: common.string.len
#
# Returns the lenght of a string
#
# Parameters:
# $1 - string that should be measured
################################################################################
function common.string.len() 
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		echo 0
	else
		echo ${#1}
	fi
}

################################################################################
# Function: common.string.getCharacters
#
# Extracts a number of characters from the start of a string
#
# Parameters:
# $1 - string from which the chars should be extracted
# $2 - number of chars to return
################################################################################
function common.string.getCharacters() 
################################################################################
{
	if [[ $# -ne 2  ]]
	then
		echo ""
	else
		# We extract $2 chars starting on position 0 of variable $1
		echo ${1:0:$2}
	fi
}


################################################################################
# Function: common.string.leftPadZero
#
# Makes sure a number has n digits (left padded with 0). Can be used in File rules to make sure
# you get something like my_file0001.dat or something.
#
# Parameters:
# $1 - a number to be padded
# $2 - number of digits to be used
################################################################################
function common.string.leftPadZero
################################################################################
{
	if [[ $# -ne 2  ]]
	then
		main.dieGracefully "We need 2 digits as input: the number to pad and the number of digits to pad to"
	fi
	
	local number="$1"
	local digits="$2"
	
	printf "%0${digits}d" "$number"
}

################################################################################
# Function: common.string.getPrefixNumber
#
# Extracts a 2-digit number from the beginning of a string
#
# Parameters:
# $1 - string from which the numbers should be extracted
################################################################################
function common.string.getPrefixNumber
################################################################################
{
	# Define & Initialize local vars
	local string
	local numbers
	
	if [[ $# -ne 1 ]]
	then
		echo 0
	else
		string=$1
		
		#Dont be afraid, the bash does not seem to recognize the quantor +
		numbers=$(expr match $string '\([0-9]*\)')
		
		# Extract 2 Digits
		numbers=$(common.string.getCharacters "${numbers}" 2)
		
		echo $(common.string.leftPadZero "${numbers}" 2)
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
	
	is "$(common.string.isSubstringPresent? abc a)" true "common.string.isSubstringPresent? is a in abc?"
	is "$(common.string.isSubstringPresent? /hallo/velo /hallo )" true "common.string.isSubstringPresent? is /hallo in /hallo/velo"
	is "$(common.string.isSubstringPresent? "" "")" true "common.string.isSubstringPresent? is empty in empty?"
	is "$(common.string.isSubstringPresent? abc "")" true "common.string.isSubstringPresent? empty in abc?"
	
	is "$(common.string.repeat " " 3 )" "   " "common.string.repeat - three spaces"
	is "$(common.string.repeat "|" 1 )" "|" "common.string.repeat - one pipe"
	is "$(common.string.repeat "|" 1 )" "|" "common.string.repeat - one pipe"
	
	is "$(common.string.toLower QUERTY)" querty "common.string.toLower QUERTY"
	is "$(common.string.toLower querty)" querty "common.string.toLower querty"
	is "$(common.string.toLower '')" "" "common.string.toLower empty string"
	
	is "$(common.string.toUpper QUERTY)" QUERTY "common.string.toUpper QUERTY"
	is "$(common.string.toUpper querty)" QUERTY "common.string.toUpper querty"
	is "$(common.string.toUpper "")" '' "common.string.toUpper empty string"
	
	is "$(common.string.trim "")" '' "common.string.trim empty string (implicit)"
	is "$(common.string.trim "    ")" '' "common.string.trim spaces (implicit)"
	is "$(common.string.trim "		")" '' "common.string.trim tabs (implicit)"
	is "$(common.string.trim " hallo")" "hallo" "common.string.trim front (implicit)"
	is "$(common.string.trim "hallo ")" "hallo" "common.string.trim back (implicit)"
	is "$(common.string.trim " hallo ")" "hallo" "common.string.trim both (implicit)"
	is "$(common.string.trim "aba" "a" )" 'b' "common.string.trim remove all a (explicit)"
	is "$(common.string.trim " b " " " )" 'b' "common.string.trim remove spaces (explicit)"
	is "$(common.string.trim "guaggc" "guagg" )" 'c' "common.string.trim remove a whole string (explicit)"
	
	is "$(common.string.getCharacters 1234 2)" 12 "common.string.getCharacters"
	
	is "$(common.string.leftPadZero 1 2)" 01 "common.string.leftPadZero 1"
	is "$(common.string.leftPadZero 01 2)" 01 "common.string.leftPadZero 01"
	is "$(common.string.leftPadZero 001 2)" 01 "common.string.leftPadZero too long"
	
	is "$(common.string.getPrefixNumber 00_ )" 00 "common.string.getPrefixNumber not at beginning"
	
	########################################
	# teardown tests if needed
	########################################
	
}
