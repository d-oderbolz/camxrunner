 # Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Title: Date functions
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# TODO: Use  <date> more often.
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=45

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Contains some date functions for the CAMxRunner"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'

################################################################################
# Function: common.date.decompose
# 
# Put date components into multiple variables
#
# Written by Chris Johnson
# Taken from <http://it.toolbox.com/wiki/index.php/Put_date_components_into_multiple_variables>
# 
#
# Parameters:
# $1 - date
################################################################################
function common.date.decompose()
################################################################################
{
	eval "$(date ${1:+"$@"} "+
		DATE=%Y-%m-%d
		YEAR=%Y
		MONTH=%m
		DAY=%d
		TIME=%H:%M:%S
		HOUR=%H
		MINUTE=%M
		SECOND=%S
		datestamp=%Y-%m-%d_%H.%M.%S
		DayOfWeek=%a
		DayOfMonth=%d
		DayOfYear=%j
		DayNum=%w
		MonthAbbrev=%b
		MonthFull=%B
		MonthName=%B
		DayName=%A
")"
}

################################################################################
# Function: common.date.isSimulationDay?
#
# Checks if a date is within the boundaries of CXR_START_DATE and CXR_END_DATE
# 
#
# Parameters:
# $1 - date in YYYY-MM-DD form
################################################################################
function common.date.isSimulationDay?()
################################################################################
{
	local ThisDate
	local StartJul
	local StopJul
	local ThisJul
	
	ThisDate=${1}
	
	StartJul=$(common.date.toJulian ${CXR_START_DATE})
	StopJul=$(common.date.toJulian ${CXR_STOP_DATE})
	ThisJul=$(common.date.toJulian ${ThisDate})
	
	if [[ $ThisJul -ge $StartJul && $ThisJul -le $StopJul ]]
	then
		echo true
	else
		echo false
	fi

}

################################################################################
# Function: common.date.isYYYYMMDD?
#
# Checks if a date is really in YYYY-MM-DD form.
# Relatively basic check.
# 
#
# Parameters:
# $1 - date in YYYY-MM-DD form
################################################################################
function common.date.isYYYYMMDD?()
################################################################################
{
	# Define & Initialize local vars
	local date
	date="$1"
	
	# We neet the retval,
	# turn off strict checks
	set +e

	#Check for Date
	# If this returns 0, we look at a correct date
	echo "$date" | grep "\(19\|20\)[0-9]\{2\}-[0-9]\{2\}-[0-9]\{2\}" >/dev/null
	
	if [[ $? -eq 0  ]]
	then
		echo true
	else
		echo false
	fi
	
		#Turn strict checks back on unles we are testing
	if [[ ${CXR_TEST_IN_PROGRESS:-false} == false ]]
	then
		set -e
	fi
	
}

################################################################################
# Function: common.date.toRaw
#
# Converts a YYYY-MM-DD into YYYYMMDD
# 
#
# Parameters:
# $1 - date in YYYY-MM-DD form
################################################################################
function common.date.toRaw()
################################################################################
{
	# Define & Initialize local vars
	local new_date
	
	if [[ $# -ne 1 || $(common.date.isYYYYMMDD? "$1") == false ]]
	then
		main.dieGracefully "needs a date as input. Got $*"
	fi
	
	# Replace - by nothing
	new_date="${1//-/}"
	
	echo "$new_date"
	
}

################################################################################
# Function: common.date.toISO
#
# Converts a date to  YYYY-MM-DD. If the empty string is passed, it is also returned.
# 
# Parameters:
# $1 - date 
################################################################################
function common.date.toISO()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully "needs 1 date as input. Got $*"
	fi
	
	# If we got the empty string, return the empty string
	if [[ ! "$1"  ]]
	then
		main.log -w  "Got the empty string and will return it"
	else
		# Convert
		date +%Y-%m-%d -d $1
	fi
	
	return $CXR_RET_OK
	
}

################################################################################
# Function: common.date.humanSeconds
#
# Converts a number of seconds to days, hours, minutes and seconds.
# Mostly used for rumtime estimation (totally seconds-based)
# 
# Parameters:
# $1 - time interval in seconds 
################################################################################
function common.date.humanSeconds()
################################################################################
{
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "Programming error: needs 1 time interval in seconds as input. Got $*"
	fi
	
	local secondsLeft
	local SecPerMin
	local SecPerHour
	local SecPerDay
	local days
	local hours
	local minutes
	local seconds
	local result
	
	secondsLeft=${1}
	SecPerMin=60
	SecPerHour=$(( 60 * $SecPerMin ))
	SecPerDay=$(( 24 * $SecPerHour ))
	
	days=$(( $secondsLeft / $SecPerDay ))
	secondsLeft=$(( $secondsLeft % $SecPerDay ))
	
	hours=$(( $secondsLeft / $SecPerHour ))
	secondsLeft=$(( $secondsLeft % $SecPerHour ))
	
	minutes=$(( $secondsLeft / $SecPerMin ))
	
	seconds=$(( $secondsLeft % $SecPerMin ))
	
	if [[ $days -gt 0 ]]
	then
		result="$days d,"
	fi
	
	if [[ $hours -gt 0 ]]
	then
		result="$result $hours hr,"
	fi
	
	if [[ $minutes -gt 0 ]]
	then
		result="$result $minutes min,"
	fi
	
	if [[ $seconds -gt 0 ]]
	then
		result="$result $seconds sec"
	fi
	
	# Remove trailing , if any
	result=${result%,}
	
	echo $result
	
	return $CXR_RET_OK
	
}


################################################################################
# Function: common.date.split
# 
# Splits a date into constituents
#
# Written by Chris Johnson
# Taken from <http://it.toolbox.com/wiki/index.php/Split_a_date_into_its_parts>
# 
# Example:
# > $ common.date.split 04-12-07 month day year
# > $ echo "year=$year month=$month day=$day"
# > year=7 month=4 day=12
#
# Parameters:
# $1 - date in YYYY-MM-DD form (or almost any other form)
# Further - parts needed
################################################################################
function common.date.split()
################################################################################
{
	sd_1=${2:-SD_YEAR}
	sd_2=${3:-SD_MONTH}
	sd_3=${4:-SD_DAY}
	sd_4=${5:-SD_HOUR}
	sd_5=${6:-SD_MINUTE}
	sd_6=${7:-SD_SECOND}
	sd_7=${8:-SD_TZ}
	case $1 in
	.|"")
		shift
		common.date.decompose
		set "$datestamp" "$@"
		;;
	esac
	oIFS=$IFS
	IFS=":-_/. "
	set -f ## To be on the safe side; it shouldn't be necessary
	set -- $1
	set +f
	IFS=$oIFS
	
	[ $# -lt 3 ] && return 1
	
	eval "$sd_1=\${1#0} $sd_2=\${2#0} $sd_3=\${3#0}"
	[ $# -gt 3 ] &&
		eval "$sd_4=\${4#0} $sd_5=\${5#0} $sd_6=\${6#0}"
	[ $# -gt 6 ] && {
		shift 6
		eval "$sd_7=\$*"
	} || true
}

################################################################################
# Function: common.date.toJulian
# 
# Calculates the Julinan day number from a date in YYYY-MM-DD format
# 
# Written by Chris Johnson, modified by dco
# Taken from http://it.toolbox.com/wiki/index.php/Convert_a_date_to_a_Julian_day
#
# From http://en.wikipedia.org/wiki/Julian_day:
# The Julian day or Julian day number (JDN) is the integer number of days that 
# have elapsed since an initial epoch defined as noon Universal Time (UT) Monday, 
# January 1, 4713 BC in the proleptic Julian calendar.
# Wednesday September 24 2008 has a JDN of 2454734. (Which this function properly returns)
#
# Example:
# > $ common.date.toJulian 2008-12-01 # year-month-day
# > 2454802
#
# Parameters:
# $1 - date in YYYY-MM-DD format
################################################################################
function common.date.toJulian() 
################################################################################
{
	# Define & Initialize local vars
	local d2j_tmpmonth
	local d2j_tmpyear
	
	case $1 in "")
		common.date.decompose
		set -- $TODAY ;; 
	esac
	
	common.date.split $1 d2j_year d2j_month d2j_day || return 2

	## Calculate number of months from March
	d2j_tmpmonth=$((12 * $d2j_year + $d2j_month - 3))
	d2j_tmpyear=$(( $d2j_tmpmonth / 12))
	echo $(( (734 * $d2j_tmpmonth + 15) / 24 -  2 * $d2j_tmpyear + \
	$d2j_tmpyear/4 - $d2j_tmpyear/100 + $d2j_tmpyear/400 + $d2j_day + 1721119 ))
}


################################################################################
# Function: common.date.JulianToDate
# 
# Calculates the date in YYYY-MM-DD format from a julian date
# 
# Written by Chris Johnson, modified by dco
# Taken from <http://it.toolbox.com/wiki/index.php/Convert_a_Julian_day_to_a_date>
#
#
# Example:
# > $ common.date.toJulian 2454802
# > 2008-12-01
#
# Parameters:
# $1 - integer julian date (numeric)
################################################################################
function common.date.JulianToDate()
################################################################################
{
	# Check for numeric input
	if [[  $# -ne 1 || ! "$1" =~ $CXR_PATTERN_NUMERIC ]]
	then
		main.dieGracefully "needs one number as input. Got $*"
	fi
	
	j2d_tmpday=$(( $1 - 1721119 ))
	
	j2d_centuries=$(( (4 * $j2d_tmpday - 1) / 146097))
	j2d_tmpday=$(( $j2d_tmpday + $j2d_centuries - $j2d_centuries/4))
	j2d_year=$(( (4 * $j2d_tmpday - 1) / 1461))
	j2d_tmpday=$(( $j2d_tmpday - (1461 * $j2d_year) / 4))
	j2d_month=$(( (10 * $j2d_tmpday - 5) / 306))
	j2d_day=$(( $j2d_tmpday - (306 * $j2d_month + 5) / 10))
	j2d_month=$(( $j2d_month + 2))
	j2d_year=$(( $j2d_year + $j2d_month/12))
	j2d_month=$(( $j2d_month % 12 + 1))
	case $j2d_day in ?) j2d_day=0$j2d_day;; esac
	case $j2d_month in ?) j2d_month=0$j2d_month;; esac
	
	echo $j2d_year-$j2d_month-$j2d_day
}

################################################################################
# Function: common.date.EpochToDate
# 
# Calculates the date in YYYY-MM-DD format from a Unix Epoch time,
# Unix Epoch is defined as (http://en.wikipedia.org/wiki/Unix_time)
# the number of seconds elapsed since midnight proleptic Coordinated Universal Time (UTC) of January 1, 1970.
#
# Parameters:
# $1 - integer epoch date (numeric)
################################################################################
function common.date.EpochToDate()
################################################################################
{
	# Check for numeric input
	if [[  $# -ne 1 || ! "$1" =~ $CXR_PATTERN_NUMERIC ]]
	then
		main.dieGracefully  "needs one number as input. Got $*"
	fi
	
	local epoch_seconds
	epoch_seconds=$1
	
	echo "$(date -d "1970-01-01 $epoch_seconds sec" +"%Y-%m-%d")"
}

################################################################################
# Function: common.date.EpochToDateTime
# 
# Calculates the date in YYYY-MM-DD HH:MM:SS UTC format from a Unix Epoch time.
# Unix Epoch is defined as (http://en.wikipedia.org/wiki/Unix_time)
# the number of seconds elapsed since midnight proleptic Coordinated Universal Time (UTC) of January 1, 1970.
#
# Parameters:
# $1 - integer epoch date (numeric)
################################################################################
function common.date.EpochToDateTime()
################################################################################
{
	# Check for numeric input
	if [[  $# -ne 1 || ! "$1" =~ $CXR_PATTERN_NUMERIC ]]
	then
		main.dieGracefully "needs one number as input. Got $*"
	fi
	
	local epoch_seconds
	epoch_seconds=$1
	
	echo "$(date -d "1970-01-01 $epoch_seconds sec" +"%Y-%m-%d %T")"
}



################################################################################
# Function: common.date.WeekOfYear
# 
# Returns the one-based week of year (Monday is the first day, starting with week 0)
#
# Parameters:
# $1 - date in YYYY-MM-DD format
# $2 - no leading zero (default: false - 2 digits)
################################################################################
function common.date.WeekOfYear() 
################################################################################
{

	if [[ $# -lt 1 || $# -gt 2 ]]
	then
		main.dieGracefully "needs a date of the form YYYY-MM-DD and an optional boolean (no leading zero) as input. Got $*"
	fi
	
	local date
	local no_zeroes
	
	date=$1
	no_zeroes=${2:-false}
	
	if [[ $no_zeroes == true ]]
	then
		# Hyphen turns off padding
		date +%-W -d $1
	else
		date +%W -d $1
	fi
}

################################################################################
# Function: common.date.MonthOfYear
# 
# Returns the one-based month of year (2 digits)
#
# Parameters:
# $1 - date in YYYY-MM-DD format
################################################################################
function common.date.MonthOfYear() 
################################################################################
{

	if [[ $# -lt 1   ]]
	then
		main.dieGracefully "needs a date of the form YYYY-MM-DD as input. Got $*"
	fi

	date +%m -d $1
}

################################################################################
# Function: common.date.DayOfYear
# 
# Calculates the number of the day in the year form a date in YYYY-MM-DD format
# Jan 01 is day number 1. (This is called the "ordinal date" in ISO terminology)
#
# Parameters:
# $1 - date in YYYY-MM-DD format
# [$2] - length of resulting string (0 padded)
################################################################################
function common.date.DayOfYear() 
################################################################################
{

	# Define & Initialize local vars
	local year
	local len 
	local julend
	local julstart
	
	if [[ $# -lt 1 || $# -gt 2 ]]
	then
		main.dieGracefully "needs a date of the form YYYY-MM-DD as input. Got $*"
	fi

	year=${1:0:4}
	len=${2:-0}
	
	julend=$(common.date.toJulian $1)
	julstart=$(common.date.toJulian ${year}-01-01)
	
	if [[ "${len}" -gt 0 ]]
	then
		# Corrected length
		printf "%0${len}d" $(( $julend - $julstart + 1 ))
	else
		# Length "as is"
		echo $(( $julend - $julstart + 1 ))
	fi
}

################################################################################
# Function: common.date.DaysInMonth
# 
# Returns the number of days in a month of a given year (needs GNU date)
#
# Parameters:
# $1 - month
# $2 - year
################################################################################
function common.date.DaysInMonth()
################################################################################
{
	# Define & Initialize local vars
	local dim_m
	local dim_y
	
	if [[ $# -lt 2  ]]
	then
		main.dieGracefully "needs a month and a year as input. Got $*"
	fi
	
	dim_m=${1}
	dim_y=${2}

	date -d "$dim_m/01/$dim_y +1month -1day" +%d
}

################################################################################
# Function: common.date.DayOfWeek
# 
# Returns the day of the week
# Returns 1 on Monday and 7 on Sunday
#
# Parameters:
# $1 - date
################################################################################
function common.date.DayOfWeek()
################################################################################
{
	# Define & Initialize local vars
	local dim_d
	local dow
	
	if [[ $# -lt 1  ]]
	then
		main.dieGracefully  "needs a date as input. Got $*"
	fi
	
	dim_d=${1}

	dow="$(date -d "$dim_d" +%u)"
	
	echo "$dow"
}

################################################################################
# Function: common.date.DaysLeftInWeek
# 
# Returns the number of days left in a week including the given day
# Returns 7 on Monday and 1 on Sunday
#
# Parameters:
# $1 - date
# [$2] - truncate, if true (default false) we only report the days left to the end of the simulation.
################################################################################
function common.date.DaysLeftInWeek()
################################################################################
{
	# Define & Initialize local vars
	local dim_d
	local dow
	local left
	local truncate
	local start_offset
	local difference
	
	if [[ $# -lt 1  ]]
	then
		main.dieGracefully "needs a date as input. Got $*"
	fi
	
	dim_d=${1}
	truncate=${2:-false}

	# Day of week
	dow=$(date -d "$dim_d" +%u)
	left=$(( 7 - $dow + 1))
	
	if [[ $truncate == true ]]
	then
		start_offset=$(common.date.toOffset $dim_d)
		difference=$(( ${CXR_NUMBER_OF_SIM_DAYS} - $start_offset ))
		
		if [[ $difference -lt $left ]]
		then
			main.log -v "There are only $difference days left to the end of the simulation!"
			echo $difference
		else
			echo $left
		fi
	else
		echo $left
	fi
}


################################################################################
# Function: common.date.DaysLeftInMonth
# 
# Returns the number of days left in a month (including the given day).
#
# Parameters:
# $1 - date
# [$2] - truncate, if true (default false) we only report the days left to the end of the simulation.
################################################################################
function common.date.DaysLeftInMonth()
################################################################################
{
	# Define & Initialize local vars
	local month
	local dom
	local ndom
	local year
	local left
	local truncate
	local start_offset
	local difference
	
	if [[ $# -lt 1 ]]
	then
		main.dieGracefully "needs a date as input. Got $*"
	fi
	
	dim_d=${1}
	truncate=${2:-false}
	
	dom=$(date -d "$dim_d" +%e)
	month=$(date -d "$dim_d" +%m)
	year=$(date -d "$dim_d" +%Y)
	ndom=$(common.date.DaysInMonth $month $year)
	left=$(( $ndom - $dom + 1))
	
	if [[ $truncate == true ]]
	then
		start_offset=$(common.date.toOffset $dim_d)
		difference=$(( ${CXR_NUMBER_OF_SIM_DAYS} - $start_offset ))
		
		if [[ $difference -lt $left ]]
		then
			main.log -v "There are only $difference days left to the end of the simulation!"
			echo $difference
		else
			echo $left
		fi
	else
		echo $left
	fi
}


################################################################################
# Function: common.date.isLeapYear?
# 
# Returns "true" if the year YYYY is a leap year
#
# Parameters:
# $1 - Year in YYYY form
################################################################################
function common.date.isLeapYear?
################################################################################
{
	# Define & Initialize local vars
	local year
	
	if [[ $# -ne 1  ]]
	then
		main.dieGracefully  "needs a year as input. Got $*"
	fi
	
	year=$1
	
	if [[ $(expr $year % 400 ) -eq "0"  ]]
	then
		echo true
	elif [[ $(expr $year % 4 ) -eq 0  ]]
	then
			if [[ $(expr $year % 100 ) -ne 0  ]]
				then
					# Those just dividable by 4 are leap years
					echo true
				else
					# Those dividable by 4 and 100 are no leap years
					echo false
				fi
	else
			echo false
	fi
}

################################################################################
################################################################################
# Functions for simulation day arithmetics
################################################################################
################################################################################

################################################################################
# Function: common.date.toOffset
# 
# Takes a valid date (YYYY-MM-DD) as input and converts it to a simulation day offset.
# It does so by calculating the difference of the julian dates.
# 
# Example (assuming the simulation starts 2007-01-01):
# > $ common.date.toOffset 2007-01-02
# > 1
#
# Parameters:
# $1 - date in YYYY-MM-DD format
################################################################################
function common.date.toOffset()
################################################################################
{
	# Define & Initialize local vars
	local start
	local wanted
	local offset
	
	# Check input
	if [[ $# -ne 1 ]]
	then
		main.dieGracefully "needs one YYYY-MM-DD date as input, got $*"
	fi
	
	start=$(common.date.toJulian ${CXR_START_DATE})
	wanted=$(common.date.toJulian ${1})
	
	offset=$(( ${wanted} - ${start} ))
	
	if [[ ${offset} -lt 0  ]]
	then
		main.log -e  "The date you requested is smaller than the start date of the simulation.\nMake sure to supply a correct date in YYYY-MM-DD form. Got $*"
		echo ""
		return $CXR_RET_ERROR
	elif [[ ${offset} -gt $(( ${CXR_NUMBER_OF_SIM_DAYS} -1 )) ]]
	then
		main.log -e  "The date you requested is larger than the end date of the simulation.\nMake sure to supply a correct date in YYYY-MM-DD form. Got $*"
		echo ""
		return $CXR_RET_ERROR
	else
		echo ${offset}
		return $CXR_RET_OK
	fi
	
}

################################################################################
# Function: common.date.OffsetToDate
# 
# Takes a day offset as input and converts it to a date (YYYY-MM-DD)
# It does so by calculating the difference of the julian dates.
#
#
# Example (assuming the simulation starts 2007-01-01):
# > $ common.date.OffsetToDate 1
# > 2007-01-02
#
# Parameters:
# $1 - offset
################################################################################
function common.date.OffsetToDate()
################################################################################
{
	# Define & Initialize local vars
	local start
	local end
	local offset
	
	# Check input
	if [[ $# -ne 1 ]]
	then
		main.log -e  "needs a day offset as input. Got *"
		echo false
		return $CXR_RET_ERROR
	fi
	
	offset="$1"
	start=$(common.date.toJulian ${CXR_START_DATE})
	end=$(( $start + $offset ))
	
	echo $(common.date.JulianToDate "$end")
	
}

################################################################################
# Function: common.date.getModelHour
# 
# Calculates the number of hours since model start (the current day does not count, since
# we are at the very beginning of it. Considers CXR_START_HOUR.
# Note that CAMx gives hours in hhhh format, this function returns truncated hh fomat.
#
#
# Example:
# > $ common.date.getModelHour 1
# > 24
#
# Parameters:
# $1 - day offset
################################################################################
function common.date.getModelHour()
################################################################################
{
	# Define & Initialize local vars
	local offset
	
	# Check for numeric input
	if [[ $# -ne 1 ]]
	then
		main.log -e  "needs a day offset as input, got $*"
		echo ""
		return $CXR_RET_ERROR
	fi

	offset="$1"
	
	if [[ "${offset}" -lt 0 ]]
	then
		main.log -e  "The date you requested is smaller than the start date of the simulation.\nMake sure to supply a correct offset in the range 0 - $(( ${CXR_NUMBER_OF_SIM_DAYS} -1 )). Got $*"
		echo ""
		return $CXR_RET_ERROR
	elif [[ ${offset} -gt $(( ${CXR_NUMBER_OF_SIM_DAYS} -1 )) ]]
	then
		main.log -e  "The date you requested is larger than the end date of the simulation.\nMake sure to supply a correct offset in the range 0 - $(( ${CXR_NUMBER_OF_SIM_DAYS} -1 )). Got $*"
		echo ""
		return $CXR_RET_ERROR
	elif [[ ${offset} -eq 0 ]]
	then
		# First day
		echo $(( ( 2400 - ${CXR_START_HOUR} ) / 100 ))
		return $CXR_RET_OK
	else
		echo $(( ( ( ${offset} * 2400 ) - ${CXR_START_HOUR} ) / 100 ))
		return $CXR_RET_OK
	fi
	
}

################################################################################
# Function: common.date.getTotalModelHours
# 
# Calculates the number of hours in the whole simulation
#
#
# Example:
# > $ common.date.getTotalModelHours
# > 720
#
################################################################################
function common.date.getTotalModelHours()
################################################################################
{
	local totHours
	totHours=$(common.date.getModelHour $(( $CXR_NUMBER_OF_SIM_DAYS - 1 )) )
	
	# Add the hours of the last day        (given like 2400)
	#                                                    |
	totHours=$(( $totHours + ( $CXR_STOP_HOUR_LAST_DAY / 100) ))
	
	echo $totHours
}

################################################################################
# Function: common.date.DaysBetween
#
# Returns the number of days between two dates (Date2 - Date1)
# 
# Parameters:
# $1 - Date 1 in YYYY-MM-DD form
# $2 - Date 2 in YYYY-MM-DD form
################################################################################	
function common.date.DaysBetween()
################################################################################
{
	# Define & Initialize local vars
	local diff
	local julend
	local julstart
	
	if [[   $# -ne 2 || $(common.date.isYYYYMMDD? "$1") == false || $(common.date.isYYYYMMDD? "$2") == false    ]]
	then
		main.log -e  "needs 2 dates as input. Got $*"
		echo false
		return $CXR_RET_ERROR
	fi

	# we convert the dates to julian dates and then subtract.
	julstart=$(common.date.toJulian $1)
	julend=$(common.date.toJulian $2)
	
	diff="$(( $julend - $julstart ))"
	
	if [[ $diff -lt 0  ]]
	then
		main.log -e  "Date2 is smaller than Date1"
		echo 0
		return $CXR_RET_ERROR
	fi
	
	# Now add one because of off-by-one
	diff=$(( $diff + 1))
	
	echo $diff
}

################################################################################
# Function: common.date.WeeksBetween
#
# Returns the number of distinct weeks between two dates (inclusive) across years. 
# Monday is the start of the week.
# This number is at least 1 (if the days are in the same week)
#
# TODO: Improve efficiency
# 
# Parameters:
# $1 - Date 1 in YYYY-MM-DD form
# $2 - Date 2 in YYYY-MM-DD form
################################################################################	
function common.date.WeeksBetween()
################################################################################
{
	# Define & Initialize local vars
	local diff
	local start
	local end
	local julend
	local julstart
	local oldweek
	local iDay
	local nWeeks
	
	# # Even if in the same week it's one week
	nWeeks=1
	
	if [[   $# -ne 2 || $(common.date.isYYYYMMDD? "$1") == false || $(common.date.isYYYYMMDD? "$2") == false    ]]
	then
		main.log -e  "needs 2 dates as input. Got $*"
		echo false
		return $CXR_RET_ERROR
	fi
	
	start="$1"
	end="$2"

	# we convert the dates to julian dates and then loop
	julstart=$(common.date.toJulian "$start" )
	julend=$(common.date.toJulian "$end" )
	
	diff="$(( $julend - $julstart ))"
	
	if [[ $diff -lt 0 ]]
	then
		main.log -e  "Date2 is smaller than Date1. Got $*"
		echo 0
		return $CXR_RET_ERROR
	fi
	
	oldweek=$(common.date.WeekOfYear "$start" true)
	
	# Be careful - by default seq returns engineering notation!
	for iDay in $(seq -f"%.0f" $julstart $julend)
	do
		week=$(common.date.WeekOfYear "$(common.date.JulianToDate $iDay)" true)
		
		# Is it a different week (not necessarily larger across years)?
		if [[ "$week" != "$oldweek" ]]
		then
			nWeeks=$(( $nWeeks + 1 ))
		fi
		
		oldweek=$week
	done
	
	echo $nWeeks
}

################################################################################
# Function: common.date.MonthsBetween
#
# Returns the number of months between two dates (inclusive) across years.
# This number is at least 1 (if the days are in the same week)
#
# TODO: Improve efficiency
# 
# Parameters:
# $1 - Date 1 in YYYY-MM-DD form
# $2 - Date 2 in YYYY-MM-DD form
################################################################################	
function common.date.MonthsBetween()
################################################################################
{
	# Define & Initialize local vars
	local diff
	local start
	local end
	local julend
	local julstart
	local oldmonth
	local iDay
	local nMonths
	
	# Even if in the same month it's one month
	nMonths=1
	
	if [[   $# -ne 2 || $(common.date.isYYYYMMDD? "$1") == false || $(common.date.isYYYYMMDD? "$2") == false    ]]
	then
		main.log -e  "needs 2 dates as input. Got $*"
		echo false
		return $CXR_RET_ERROR
	fi
	
	start="$1"
	end="$2"

	# we convert the dates to julian dates and then loop
	julstart=$(common.date.toJulian $start)
	julend=$(common.date.toJulian $end)
	
	diff="$(( $julend - $julstart ))"
	
	if [[ $diff -lt 0 ]]
	then
		main.log -e "Date2 is smaller than Date1. Got $*"
		echo 0
		return $CXR_RET_ERROR
	fi
	
	oldmonth=$(common.date.MonthOfYear "$start")
	
	# Attn: by default, seq returns engineering notation
	for iDay in $(seq -f"%.0f" $julstart $julend)
	do
		month=$(common.date.MonthOfYear "$(common.date.JulianToDate $iDay)")
		
		# Is it a different month (not necessarily larger across years)?
		if [[ "$month" != "$oldmonth" ]]
		then
			nMonths=$(( $nMonths + 1 ))
		fi
		
		oldmonth=$month
	done

	
	echo $nMonths
}

################################################################################
# Function: common.date.addDays
#
# Adds a number of days to a date and returns result
# 
# Parameters:
# $1 - Date in YYYY-MM-DD form
# $2 - number of days to add (integer!)
################################################################################	
function common.date.addDays()
################################################################################
{
	# Define & Initialize local vars
	local dateresult
	local julresult
	local julstart
	
	if [[   $# -ne 2 || $(common.date.isYYYYMMDD? "$1") == false || ! "$2" =~ $CXR_PATTERN_NUMERIC ]]
	then
		main.log -e  "needs one date and one number as input. Got $*"
		echo false
		return $CXR_RET_ERROR
	fi

	# Convert to julian and add, convert back
	julstart=$(common.date.toJulian $1)

	julresult=$(( $julstart + $2 ))
	
	dateresult=$(common.date.JulianToDate $julresult)
	
	echo "$dateresult"
}

################################################################################
# Function: common.date.subtractDays
#
# Subtracts a number of days from a date and returns result
# 
# Parameters:
# $1 - Date in YYYY-MM-DD form
# $2 - number of days to subtract (integer!)
################################################################################	
function common.date.subtractDays()
################################################################################
{
	# Define & Initialize local vars
	local dateresult
	local julresult
	local julstart
	
	if [[   $# -ne 2 || $(common.date.isYYYYMMDD? "$1") == false || ! "$2" =~ $CXR_PATTERN_NUMERIC ]]
	then
		main.log -e  "needs one date and one number as input. Got $*"
		echo false
		return $CXR_RET_ERROR
	fi

	# Convert to julian and add, convert back
	julstart=$(common.date.toJulian $1)

	julresult=$(( $julstart - $2 ))
	
	dateresult=$(common.date.JulianToDate $julresult)
	
	echo "$dateresult"
}

################################################################################
# Function: common.date.setVars
#
# Exports a number of date variables from a simulation day offset
# Maybe this can be done more efficiently by using date directly
# (see http://ss64.com/bash/date.html)
#
# Also directly expands a couplo of inmportant variables
# TODO: Check if place is apropriate
#
# Example:
# > common.date.setVars "$CXR_START_DATE" "0"
# 
# Parameters:
# $1 - Start day in YYYY-MM-DD notation
# $2 - Simulation day offset
################################################################################	
function common.date.setVars()
################################################################################
{

	if [[   $# -ne 2 || $(common.date.isYYYYMMDD? "$1") == false || ! "$2" =~ $CXR_PATTERN_NUMERIC ]]
	then
		main.log -e  "needs one date and one number as input"
		echo false
		return $CXR_RET_ERROR
	fi

	START_DATE=$1
	
	# Export the offset so that we can save and restore settings
	# Also we need it for certain date functions like common.date.isFirstDayOfSimulation?
	CXR_DAY_OFFSET=$2
	
	# Set start and stop hour correctly
	if [[ $(common.date.isFirstDayOfSimulation?) == "true" ]]
	then
		CXR_START_HOUR=${CXR_START_HOUR_FIRST_DAY}
		main.log -v "We are at the first day, simulation time starts at ${CXR_START_HOUR}"
	else
		CXR_START_HOUR=0000
		main.log -v "We are at a normal day, simulation time starts at ${CXR_START_HOUR}"
	fi
	
	if [[ $(common.date.isLastDayOfSimulation?) == "true" ]]
	then
		CXR_STOP_HOUR=${CXR_STOP_HOUR_LAST_DAY}
		main.log -v "We are at the last day, simulation time stops at ${CXR_STOP_HOUR}"
	else
		CXR_STOP_HOUR=2400
		main.log -v "We are at a normal day, simulation time stops at ${CXR_STOP_HOUR}"
	fi
	
	
	
	# Calculate the julian day of the start
	# then add the offset - we calculate all in "julian space"
	
	JULSTART=$(common.date.toJulian "$START_DATE")
	
	# Julian date
	CXR_JUL=$(( $JULSTART + $CXR_DAY_OFFSET ))
	
	# Convert to real date in YYYY-MM-DD form
	CXR_DATE=$(common.date.JulianToDate $CXR_JUL)
	
	# Date without separators (YYYYMMDD)
	CXR_DATE_RAW=$(common.date.toRaw $CXR_DATE)
	
	#Split it
	common.date.split $CXR_DATE year month day
	
	# YYYY year
	CXR_YEAR=$year
	
	# YY year
	CXR_YEAR_S=${CXR_YEAR:2:2}
	
	# MM month
	CXR_MONTH=$(common.string.leftPadZero $month 2)
	
	# DD day
	CXR_DAY=$(common.string.leftPadZero $day 2)
	
	# Modelling hour
	CXR_MODEL_HOUR=$(common.date.getModelHour $CXR_DAY_OFFSET)
	
	# Day of year
	CXR_DOY=$(common.date.DayOfYear $CXR_DATE)
	
	# Day of week
	CXR_DOW=$(common.date.DayOfWeek $CXR_DATE)
	
	# Week of year
	CXR_WOY=$(common.date.WeekOfYear $CXR_DATE)
	
	# if offset is 0, we are at the initial day
	if [[ "$CXR_DAY_OFFSET" -eq 0 ]]
	then
		# The initial meteo files are called .._s
		CXR_DAY_METEO="$(common.runner.evaluateRule "$CXR_DAY_METEO_FIRST_DAY_RULE" false CXR_DAY_METEO_FIRST_DAY_RULE)"
	else
		# this variable is different the first day
		CXR_DAY_METEO=${CXR_DAY}
	fi
		
	########################################
	# Calculate it all for yesterday (mainly for inst files)
	# We do this even if we are at the first day because we might
	# want to restart a run in the middle
	########################################	
	CXR_JUL_YESTERDAY=$(( $CXR_JUL - 1 ))
	CXR_DATE_YESTERDAY=$(common.date.JulianToDate $CXR_JUL_YESTERDAY)
	
	CXR_DATE_RAW_YESTERDAY=$(common.date.toRaw $CXR_DATE_YESTERDAY)
	
	#Split it
	common.date.split $CXR_DATE_YESTERDAY year_yesterday month_yesterday day_yesterday
	

	# YYYY year
	CXR_YEAR_YESTERDAY=$year_yesterday
	
	# YY year
	CXR_YEAR_S_YESTERDAY=${CXR_YEAR_YESTERDAY:2:2}
	
	# MM month
	CXR_MONTH_YESTERDAY=$(common.string.leftPadZero $month_yesterday 2)
	
	# DD day
	CXR_DAY_YESTERDAY=$(common.string.leftPadZero $day_yesterday 2)
	
	# Day of year
	CXR_DOY_YESTERDAY=$(common.date.DayOfYear $CXR_DATE_YESTERDAY)
	
	# Day of week
	CXR_DOW_YESTERDAY=$(common.date.DayOfWeek $CXR_DATE_YESTERDAY)
	
	# week of year
	CXR_WOY_YESTERDAY=$(common.date.WeekOfYear $CXR_DATE_YESTERDAY)

}

################################################################################
# Function: common.date.isFirstDayOfWeek?
#
# Returns true if the day currently processed is the first of a week (Monday).
#
# Parameters:
# $1 - day in YYYY-MM-DD notation
################################################################################
function common.date.isFirstDayOfWeek? ()
################################################################################
{
	local date
	date=$1
	
	local days_left
	days_left=$(common.date.DaysLeftInWeek $date)
	
	if [[ "$days_left" -eq 7 ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.date.isFirstDayOfMonth?
#
# Returns true if the day currently processed is the first of a month (01).
#
# Parameters:
# $1 - day in YYYY-MM-DD notation
################################################################################	
function common.date.isFirstDayOfMonth?()
################################################################################
{
	local date
	date=$1
	
	if [[ "$CXR_DAY" == "01" ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.date.isFirstDayOfYear?
#
# Returns true if the day currently processed is the first of a month (01).
#
# Parameters:
# $1 - day in YYYY-MM-DD notation
################################################################################	
function common.date.isFirstDayOfYear?()
################################################################################
{
	local date
	date=$1
	
	month="$(date -d "$date" +%m)"
	day="$(date -d "$date" +%d)"
	
	if [[ "$month" == "01" && "$day" == "01" ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.date.isFirstDayOfSimulation?
#
# Returns true if the day currently processed is the first simulated.
#
# Variables:
# CXR_DAY_OFFSET - the current day offset
# 
################################################################################	
function common.date.isFirstDayOfSimulation? ()
################################################################################
{
	if [[ "$CXR_DAY_OFFSET" -eq 0 ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: common.date.isLastDayOfSimulation?
#
# Returns true if the day currently processed is the last simulated
#
# Variables:
# CXR_DAY_OFFSET - the current day offset
# 
################################################################################	
function common.date.isLastDayOfSimulation? ()
################################################################################
{
	if [[ "$CXR_DAY_OFFSET" -eq "$((${CXR_NUMBER_OF_SIM_DAYS} -1 ))"  ]]
	then
		echo true
	else
		echo false
	fi
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
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(common.date.isYYYYMMDD? 1999-06-12) true "common.date.isYYYYMMDD?"
	is $(common.date.isYYYYMMDD? 1999-6-12)  false "common.date.isYYYYMMDD?"
	is $(common.date.toRaw 2009-01-12) 20090112 "common.date.toRaw"
	is $(common.date.toISO 20090112) 2009-01-12 "common.date.toISO"
	is $(common.date.toJulian 2009-01-27) 2454859 "common.date.toJulian"
	is $(common.date.JulianToDate 2454859) 2009-01-27 "common.date.JulianToDate"
	is $(common.date.EpochToDate 1266874169) 2010-02-22 "common.date.EpochToDate normal"
	is $(common.date.EpochToDate 0) 1970-01-01 "common.date.EpochToDate base"
	is $(common.date.DayOfYear 2009-01-01) 1 "DOY"
	is $(common.date.DayOfYear 2009-01-01 4) 0001 "DOY trailing 0"
	is $(common.date.DayOfYear 2003-04-12) 102 "DOY"
	is $(common.date.DayOfYear 2009-12-31) 365 "DOY"
	
	is $(common.date.DayOfWeek 2009-12-31) 4 "DOW"
	is $(common.date.WeekOfYear 2009-12-31) 52 "WOY normal"
	is $(common.date.WeekOfYear 2009-01-01 true) 0 "WOY no padding"
	is $(common.date.MonthOfYear 2009-12-31) 12 "MOY"
	
	is "$(common.date.humanSeconds 60)" '1 min' "common.date.humanSeconds One minute"
	is "$(common.date.humanSeconds 61)" '1 min, 1 sec' "common.date.humanSeconds one minute, one second"
	is "$(common.date.humanSeconds 3661)" '1 hr, 1 min, 1 sec' "common.date.humanSeconds one hour, one minute, one second"
	
	
	is $(common.date.DaysInMonth 01 2008) 31 "common.date.DaysInMonth normal"
	is $(common.date.DaysInMonth 02 2008) 29 "common.date.DaysInMonth feb leap year"
	is $(common.date.DaysInMonth 02 2000) 29 "common.date.DaysInMonth feb leap year"
	is $(common.date.DaysInMonth 02 2001) 28 "common.date.DaysInMonth normal"
	is $(common.date.DaysInMonth 02 2000) 29 "common.date.DaysInMonth feb 2000 (leap year)"
	is $(common.date.DaysLeftInWeek 2004-01-04) 1 "common.date.DaysLeftInWeek"
	is $(common.date.DaysLeftInMonth 2004-01-30) 2 "common.date.DaysLeftInMonth"
	is $(common.date.DaysBetween 2009-01-01 2009-01-01) 1 "common.date.DaysBetween same day"
	is $(common.date.DaysBetween 2009-01-01 2009-12-31) 365 "common.date.DaysBetween one year (non-leap)"
	is $(common.date.DaysBetween 2004-01-01 2004-12-31) 366 "common.date.DaysBetween one year (leap)"
	is $(common.date.DaysBetween 2009-01-01 2009-02-28) 59 "common.date.DaysBetween"
	
	is $(common.date.WeeksBetween 2009-01-01 2009-01-01) 1 "common.date.WeeksBetween same day"
	is $(common.date.WeeksBetween 2009-01-01 2009-01-02) 1 "common.date.WeeksBetween same week"
	is $(common.date.WeeksBetween 2010-09-13 2010-09-26) 2 "common.date.WeeksBetween two weeks"
	is $(common.date.WeeksBetween 2009-01-01 2009-01-31) 5 "common.date.WeeksBetween longer period"
	
	is $(common.date.MonthsBetween 2009-01-01 2009-01-01) 1 "common.date.MonthsBetween same day"
	is $(common.date.MonthsBetween 2009-01-01 2009-01-12) 1 "common.date.MonthsBetween same month"
	is $(common.date.MonthsBetween 2003-08-01 2003-09-30) 2 "common.date.MonthsBetween two months"
	is $(common.date.MonthsBetween 2009-01-01 2009-04-01) 4 "common.date.MonthsBetween longer period"
	
	
	is $(common.date.addDays 2009-02-28 1) 2009-03-01 "common.date.addDays"
	is $(common.date.addDays 2004-02-28 1) 2004-02-29 "common.date.addDays"
	is $(common.date.subtractDays 2004-02-29 1) 2004-02-28 "common.date.subtractDays"
	is $(common.date.isFirstDayOfYear? 2012-01-01) true "common.date.isFirstDayOfYear? 2012-01-01"
	is $(common.date.isFirstDayOfWeek? 2010-03-01) true "common.date.isFirstDayOfWeek? 2010-03-01"
	is $(common.date.isFirstDayOfWeek? 1996-10-01) false "common.date.isFirstDayOfWeek? 1996-10-01"
	is $(common.date.isFirstDayOfMonth? 2010-10-01) true "common.date.isFirstDayOfMonth? 2010-10-01"

	########################################
	# teardown tests if needed
	########################################
	
	
}

