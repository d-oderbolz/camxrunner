# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Contains functions to simplify user interaction
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
CXR_META_MODULE_NUM_TESTS=1

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains functions to simplify user interaction"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: common.user.showProgress
#
# Function to show the user a simple feedback during long-running operations.
# Works by iterating through a list of 4 characters and displaying them at the same 
# place. (Inspired by the Intel PXE loader of my Laptop :-)
# Writes to stderr.
# 
#
################################################################################
function common.user.showProgress()
################################################################################
{
	# echo -n . 1>&2
	currentProgressState=$(( ($currentProgressState + 1) % 4 ))
	echo -e -n "${CXR_PROGRESS_STATES[$currentState]}" 1>&2
	echo -e -n "\b" 1>&2
}

################################################################################
# Function: common.user.showProgressBar
#
# Given a number in percent, produces a progress bar on stderr.
# 
# Parameters:
# $1 - a number in percent
################################################################################
function common.user.showProgressBar()
################################################################################
{
	local string
	local spaces
	local nSpaces
	local nElements
	local progress
	
	progress=$(common.math.FloatOperation "(${1:-0} * $CXR_MAX_BAR_WIDTH) / 100" -1 false)

	nElements=$(( $progress - 1 ))
	
	if [[ $nElements -lt 1 ]]
	then
		nElements=0
		string=""
	else
		string="$(common.string.repeat "=" $nElements)"
	fi
	
	nSpaces=$(( $CXR_MAX_BAR_WIDTH - $progress ))
	spaces="$(common.string.repeat "." $nSpaces)"
	
	echo "|${string}>${spaces}|" 1>&2
}


################################################################################
# Function: common.user.getOK
#	
# Asks te user if he/she agrees (Y) to a question or not (N).
# Automatically adds [Y/N] to the question.
#
# Loops until either Y or N is given. (Case is ignored)
#
# Returns:
# true - user says Y
# false - user says N
#
# Example (Note the double quotes around the call):
#> if [[ "$(common.user.getOK "Do you want to run the installer for the CAMxRunner, CAMx and the testcase \n (you can select the steps you want later)?" )" == false  ]]
#> then
#>	exit
#> fi
#
# Cannot be used in a loop like (because read is used internally): 
#
#>while read LINE
#>do
#>done
#
# Parameters:
# $1 - question
# [$2] - Optional default value (either Y or N)
################################################################################
function common.user.getOK()
################################################################################
{
	local message
	local default
	local answer
	
	# default only accepted if either Y or N
	if [[ "${2:-}" == Y || "${2:-}" == N ]]
	then
			
		########################################
		# default is set
		########################################
		default=${2:-}

		
		message="$1 [Y/N/D], D=$default"
		
		answer=$(common.user.getInput "$message")
	
		until [[  "$answer" == Y || "$answer" == y \
							|| "$answer" == N || "$answer" == n \
							|| "$answer" == D || "$answer" == d ]]
		do
			answer=$(common.user.getInput "$message\nAnswer with either Y, N or D")
		done
		
		if [[  "$answer" == d || "$answer" == D ]]
		then
			answer=$default
		fi
		
	else
	
		########################################
		# No default
		########################################	

		message="$1 [Y/N]"
		
		answer=$(common.user.getInput "$message")
	
		until [[  "$answer" == Y || "$answer" == y \
							|| "$answer" == N || "$answer" == n ]]
		do
			answer=$(common.user.getInput "$message\nAnswer with either Y or N.")
		done
		
	fi
	
	if [[ "$answer" == Y || "$answer" == y ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.user.getInput
#	
# Poses a question to the user and returns the answer.
# Optionally, the lenght of the input can be restricted.
#
# Returns:
# the value the user enters.
#
# Example:
#> MEAL=$(common.user.getInput "Whats you favourite meal?")
#
# Cannot be used in a loop like (because read is used internally): 
#
#>while read LINE
#>do
#>done
#
# Parameters:
# $1 - Question
# [$2] - Expected lenght (Optional)
################################################################################
function common.user.getInput() 
################################################################################
{
	local message
	local answer
	
	message=$1
	
	# Communicate with the user on STDERR.
	echo "${CXR_SINGLE_LINE}" 1>&2
	echo -e "${message}" 1>&2
	
	if [[ $(main.isNumeric? "${2:-}") == true  ]]
	then
		read -n $2 answer
	else
		read answer
	fi

	echo $answer
}

################################################################################
# Function: common.user.pause
#	
# Waits until the user presses a button
#
# Example:
#> pause
#
# Cannot be used in a loop like (because read is used internally): 
#
#>while read LINE
#>do
#>done
#
# Parameters:
# [$1] - message
################################################################################
function common.user.pause() 
################################################################################
{
	local dummy
	
	echo "${CXR_SINGLE_LINE}" 1>&2
	echo -e "${1:-Press a key of your choice to continue...}" 1>&2
	read -n 1 dummy
}

################################################################################
# Function: common.user.getMenuChoice
#	
# Poses a question to the user and presents a menu of choices.
# The choices are either given using a pattern (ls-like) for filenames
# or using a string containing space-delimited options.
#
# If the user enters a non-numeric sequence, the defauld value is returned.
#
# Returns:
# the value the user selects (the string, not the number).
#
# Cannot be used in a loop like (because read is used internally): 
#
#>while read LINE
#>do
#>	:
#>done
#
# Example:
#> MEAL=$(common.user.getMenuChoice "Whats you favourite meal?" "hot-dog hamburger pizza organic" "pizza")
# In the next 2 examples, the quotes around the second parameter are vital!
#> File=$(common.user.getMenuChoice "Select a file" "*" )
#> Version=$(common.user.getMenuChoice "Select a Version" "$CXR_SUPPORTED_MODEL_VERSIONS" )
#
# Can be nicley used in conjuntion with the case statement:
# 
#>WHAT=$(common.user.getMenuChoice "Which part of the state database do you want to clean?" "all none" "none")
#>
#>case "$WHAT" in 
#> all) 
#>	delete_all ;;
#>none)
#>	main.log   "Will not delete any state information" ;;
#>esac
#
# Parameters:
# $1 - Question
# $2 - Option list (eiter a pattern or a string)
# [$3] - Optional default value
################################################################################
function common.user.getMenuChoice()
################################################################################
{
	echo "${CXR_SINGLE_LINE}" 1>&2
	local message
	local options
	local default
	local choice
	local chosen
	
	message="$1\nEnter the *number* of your choice:"
	options=$2
	
	if [[ "${3:-}"  ]]
	then
		# We have a default, set advanced prompt and add default as last
		default="${3:-}"
		PS3="Your choice (a number - press [d] for default value [$default]): "
	else
		# no default, set normal prompt
		PS3="Your choice (a number): "
	fi
	
	# Communicate with the user on STDERR.
	echo -e "${message}" 1>&2
	
	select choice in ${options};
	do
	
		chosen="$choice"
		# Without the break we are left in an endless loop...
		break
		
	done
	
	# default handling. If the user presses d (or any non-numeric character),
	# Value is empty
	if [[ -z "$(common.string.trim "$chosen")" ]]
	then
		# User pressed non-numeric character
		if [[ "${default:-}"  ]]
		then
			# There is a default
			chosen="$default"
		else
			# There is no default. Do it again
			echo "Please type in a number!" 1>&2
			chosen="$(common.user.getMenuChoice "$1" "$2")"
		fi
	fi
	
	echo "$chosen"
}

################################################################################
# Function: common.user.getAnswers
#	
# Poses questions given in a so called ask-file to the user.
# Creates a so-called play-file that contains the answer of the user 
# as key-value pairs.
#
# Loop through all lines of the ask file,
# Split - ask question - store result
#
# Format of .ask files:
# variable|datatype|Question|Default|Value1|Value2|...|ValueN
# A minimal line looks like this (An integer value without question and default):
# MXCOLA|I||
#
# A support program (bin/extract_ask.sh) can automatically extract variables from
# a template and create a raw .ask file. IF THE FORMAT OF .ASK SHOULD CHANGE,
# bin/extract_ask.sh NEEDS TO BE CHANGED AS WELL
#
# All but the first string are optional, the number of Values at the end is not fixed.
# Actually, a line could just consist of a Variablename,
# the installer would then choose these vales:
# * datatype S
# * Question: What is the value of Variablename?
# * Default: Value of the variable from environment if set
#
# You can use variables or combinations thereof as Default (all that is allowed in file rules is allowed here)
#
# At the end, a variable amount of possible values is allowed - but you can also specify a variable name
# that contains a list of values (makes maintenance easier, you can maintain these lists in base.conf or something)
#
# datatype can be (as supported by common.check.DataType)
# S (String - Default)
# I (Integer)
# F (Float)
# B (Boolean - either true or false)
# D (Directory - a high level string that is checked)
#
# Of course, the "|"  is reserved, # is a comment (as first char in the line ONLY)
# Lines of the Form COMMENT|String are echoed as is
#
#
# Format of resulting .play files:
# variable:Value
#
# Returns:
# void - aborts program on failure.
#
# Example:
# common.user.getAnswers $askfile $playfile
#
# Parameters:
# $1 - askfile (with full path)
# $2 - playfile to store answers in (with full path - WILL BE OVERWRITTEN!)
################################################################################
function common.user.getAnswers()
################################################################################
{
	local askfile
	local playfile
	local curline
	local line
	local lov
	local curlov
	local oIFS
	local line_array
	local num_elements
	local variable
	local datatype
	local question
	local default
	local value

	askfile="$1" 
	playfile="$2"

	
	main.log -a  "Using ask-file ${askfile} to create playfile ${playfile}"
	
	# Write a comment to the file
	echo -e "#This is a machine-generated file. Generated on $(date) by user $USER\n#It can be used to replay an installation." > $playfile
	
	# The canonical way to loop through a file is
	# while read line
	# do
	# done
	# This is not possible because I want to read input from stdin as well.
	
	curline=1

	while [[ $curline -le $(wc -l < $askfile) ]]
	do
		# Read the line (I know this is not nice, read comment above)
		# This is a ugly standard construct: Read first $curline lines
		# the last of which is line $curline
		line="$(head -n $curline $askfile | tail -n 1)"
		
		# lov (List of values) is unset yet
		lov=""

		# Ignore Comments - but only if in first column
		if [[ "${line:0:1}" == \#  ]]
		then
			curline=$(( $curline + 1 ))
			continue
		fi
		
		# Ignore empty lines
		if [[ -z "$(common.string.trim "$line")"  ]]
		then
			curline=$(( $curline + 1 ))
			continue
		fi

		# Parse it - structure variable|datatype|question|Default|Value1|Value2|...|ValueN
		# All but the first are optional, the number of Values at the end is not fixed.
		# 

		# We mainpulate word-splitting to get the list of values

		# Save old IFS
		oIFS="$IFS"

		IFS="$CXR_DELIMITER"

		# Suck line into line_array
		line_array=($line)
		
		# Reset IFS
		IFS="$oIFS"

		num_elements=${#line_array[@]}

		if [[ "$num_elements" -ge 1  ]]
		then
			variable=${line_array[0]}
		else
			# No variable - skip
			curline=$(( $curline + 1 ))
			continue
		fi
		
		if [[ "${variable}" == COMMENT  ]]
		then
			########################################
			# A comment the user should see, then skip
			########################################
			echo "${CXR_DOUBLE_LINE}"
			echo -e "${line_array[1]}"
			echo "${CXR_DOUBLE_LINE}"
			curline=$(( $curline + 1 ))
			continue
		fi

		if [[ "$num_elements" -ge 2  ]]
		then
			datatype=${line_array[1]}
		else
			datatype=S
		fi

		if [[ "$num_elements" -ge 3  ]]
		then
			question=${line_array[2]}
		else
			question="What should be the value of $variable?"
		fi

		if [[ "$num_elements" -ge 4  ]]
		then
			default=${line_array[3]}
			
			# If it starts with CXR_ we knwo its a variable and try to resolve it
			if [[ ${default:0:4} == CXR_  ]]
			then
				# Create an expandable rule
				default="\$$default"
			fi
			
		else
			# Otherwise just try to create an expandable rule
			default="\$$variable"
		fi

		########################################	
		# Handling of an optinal list of values (lov)
		########################################	
		if [[ "$num_elements" -eq 5  ]]
		then
			# If we have exactly one value, it is taken as the list of values
			# this is to simplify the maintenance of base.ask
			lov="${line_array[4]}"
		elif [[ "$num_elements" -gt 5  ]]
		then
			# We must go through a list and reconstruct it
			for curlov in $(seq 4 $(( $num_elements - 1 )) )
			do
				lov="$lov ${line_array[$curlov]}"
			done
		fi

		main.log -v   "variable: $variable"
		main.log -v   "question: $question"
		main.log -v   "default: $default"
		main.log -v   "datatype: $datatype"
		main.log -v   "lov: $lov"


		if [[ -z "$(common.string.trim "${variable}")"  ]]
		then
			# No variable - skip
			curline=$(( $curline + 1 ))
			continue
		else
			########################################	
			# A standard variable
			########################################	

			# Some optimizations

			# If the question is empty, supply your own
			if [[ -z "$(common.string.trim "$question")"  ]]
			then
				question="What should be the value of $variable?"
			fi

			# default - might contain an expandable rule
			# It does not matter if the rule does not expand (are we?)
			# We call the rule DEFAULT
			default=$(common.runner.evaluateRule "$default" true DEFAULT)
			
			# Do we have a list of values?
			if [[ -z "$lov"  ]]
			then
				# No
				# Add Default to question
				question="$question\n(default: $default)"

				value=$(common.user.getInput "$question\n[D] for default")

				if [[  "$value" == D || "$value" == d   ]]
				then
					# Use default
					value="$default"
				fi

			else
				# Yes
				value=$(common.user.getMenuChoice "$question" "$lov")
			fi

			if [[ "$(common.check.DataType "$value" "$datatype")" == false  ]]
			then
				main.log  "Datatpe of $value is not $datatype! I use the default instead."
				# Use default
				value="$default"
			fi

			# If the user does not want to write this 
			# value, we will ask the same question again (by not increasing curline)
			# Thats actually an advantage of this form of loop.
			if [[ "$(common.user.getOK "Is the value $value for variable $variable correct?" "Y" )" == true  ]]
			then
			
				# Write data to play-file
				echo "${variable}${CXR_DELIMITER}${value}" >> $playfile

				curline=$(( $curline + 1 ))
			fi
		fi
	done
}


################################################################################
# Function: common.user.applyPlayfile
#	
# Complementary function of <common.user.getAnswers>. Loops through a playfile and 
# uses its information to search for variables in all listed files and
# replaces them by the repective value.
#
# Format of input .play files:
# variable|Value
# Some older playfiles use : as delimiter, we can detect this
#
# Returns:
# void - aborts program on failure.
#
# Example:
# common.user.applyPlayfile $playfile "file1 file2"
#
# Parameters:
# $1 - playfile (with full path)
# $2 - A list of files to operate on (search & replace) - they WILL BE OVERRIDDEN!
################################################################################
function common.user.applyPlayfile()
################################################################################
{
	# The delimiter in use here (might change below if file has a different format)
	local delimiter
	
	delimiter="${CXR_DELIMITER}"

	if [[ $# -ne 2  ]]
	then
		main.dieGracefully "needs a playfile and a list of input files as input"
	fi

	local playfile
	local files
	local sed_tmp
	local current_file
	local line
	local line_array
	local variable
	local value
	local curline
	
	playfile="$1"
	files="$2"

	########################################
	main.log  "Playback of $playfile..."
	########################################
	
	# Quickly check if is old-style
	# We count the number of lines containing colons
	# The first line is ok (comment contains a time like 15:10:55)
	if [[ "$(grep -c : "$playfile")" -gt 1  ]]
	then	
		# More than 1 line contains :
		main.log -w "More than one line of the file $playfile contains colons (the former delimiter used for .play files). I will now assume : as delimiter, but please replace : by | in the file manually, thanks!"
		# Use old delimiter
		delimiter=":"
	fi
	
	# This one will be re-used
	sed_tmp=$(common.runner.createTempFile sed)
	
	# Start at line 1
	curline=1
	
	main.log -a  "Applying changes to the template files..."
		
	# Loop through playfile, curline is the line index 1..n
	while [ $curline -le $(wc -l < $playfile) ]
	do
		# Read the current line (needed because we use read in the loop)
		# This is an ugly standard construct: Read first $curline lines
		# the last of which is line $curline
		line=$(head -n $curline $playfile | tail -n 1)
			
			# Ignore Comments - but only if in first column
			if [[ "${line:0:1}" == \#  ]]
			then
				curline=$(( $curline + 1 ))
				continue
			fi
			
			# Ignore empty lines
			if [[ -z "$(common.string.trim "$line")"  ]]
			then
				curline=$(( $curline + 1 ))
				continue
			fi
			
			# Split
			oIFS="$IFS"

			IFS="$delimiter"
		
			# Suck line into line_array
			line_array=($line)
		
			# Reset IFS
			IFS="$oIFS"
			
			variable=${line_array[0]}
			value=${line_array[1]}
			
			if [[ -z "$variable"  ]]
			then
				# Skip empty variables
				curline=$(( $curline + 1 ))
				continue
			fi

			main.log -v  "For each variable, we go through all files now."

			# Replace the current variable in all files listed
			for current_file in $files
			do
				main.log -v  "current_file: $current_file"
				
				# replace the @variable@ with the value (globally) 
				# send the output to $sed_tmp (sed cannot work on the same file as the input)
				# The TEMPFILE is automatically re-used (overwritten)
				sed -e "s/@$variable@/$value/g" $current_file > $sed_tmp

				# Copy file back
				cp $sed_tmp $current_file || main.dieGracefully "Could not copy $sed_tmp to the draft file $current_file"
				
				# Empty tempfile
				: > $sed_tmp
				
			done # Loop through files
			
			# Advance to next line in playfile
			curline=$(( $curline + 1 ))

		done # Loop through playfile

		if [[ "$(common.user.getOK "Do you want to have a look at the new files?" N )" == true  ]]
		then
			for current_file in $files
			do
				echo -e "${CXR_BOX_HUGE}"
				
				echo "File $current_file:"
				
				echo -e "${CXR_BOX_HUGE}"
				
				cat $current_file
				
				echo -e "${CXR_BOX_HUGE}\n\n"
				echo "(That was file ${current_file})"
			
				common.user.pause
			done
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
	
	# To test this function, we must redirect stderr to stdout
	is "$(common.user.showProgressBar 10 2>&1)" '|=======>........................................................................|' "common.user.showProgressBar"

	########################################
	# teardown tests if needed
	########################################

	
}
