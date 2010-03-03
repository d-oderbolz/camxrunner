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
# TODO: Use  <date> more often.
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=31

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

	$progname - A part of the CAMxRunner tool chain.

	Is designed to be called by the CAMxRunner.
	
	You can, however, call it like this:
	
	$ $progname -T
	
	this starts the self-test of the module.

	
	Written by $CXR_META_MODULE_AUTHOR
	License: $CXR_META_MODULE_LICENSE
	
	Find more info here:
	$CXR_META_MODULE_DOC_URL
EOF
exit 1
}

################################################################################
# Function: cxr_common_date_vars
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
function cxr_common_date_vars()
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
# Function: cxr_common_is_yyyymmdd_format?
#
# Checks if a date is really in YYYY-MM-DD form.
# Relatively basic check.
# 
#
# Parameters:
# $1 - date in YYYY-MM-DD form
################################################################################
function cxr_common_is_yyyymmdd_format?()
################################################################################
{
	# Define & Initialize local vars
	local date="$1"
	
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
	
	#Turn strict checks back on
	set -e
	
}

################################################################################
# Function: cxr_common_raw_date
#
# Converts a YYYY-MM-DD into YYYYMMDD
# 
#
# Parameters:
# $1 - date in YYYY-MM-DD form
################################################################################
function cxr_common_raw_date()
################################################################################
{
	# Define & Initialize local vars
	local new_date
	
	if [[  $# -ne 1 || $(cxr_common_is_yyyymmdd_format? "$1") == false   ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs a date as input"
		echo false
		return $CXR_RET_ERROR
	fi
	
	# Replace - by nothing
	new_date="${1//-/}"
	
	echo "$new_date"
	
}

################################################################################
# Function: cxr_common_to_iso_date
#
# Converts a date to  YYYY-MM-DD. If the empty string is passed, it is also returned.
# 
# Parameters:
# $1 - date 
################################################################################
function cxr_common_to_iso_date()
################################################################################
{
	if [[ $# -ne 1  ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs 1 date as input"
		echo false
		return $CXR_RET_ERROR
	fi
	
	# If we got the empty string, return the empty string
	if [[ ! "$1"  ]]
	then
		cxr_main_logger -w "$FUNCNAME" "Got the empty string and will return it"
	else
		# Convert
		date +%Y-%m-%d -d $1
	fi
	
	return $CXR_RET_OK
	
}

################################################################################
# Function: cxr_common_split_date
# 
# Splits a date into constituents
#
# Written by Chris Johnson
# Taken from <http://it.toolbox.com/wiki/index.php/Split_a_date_into_its_parts>
# 
# Example:
# > $ cxr_common_split_date 04-12-07 month day year
# > $ echo "year=$year month=$month day=$day"
# > year=7 month=4 day=12
#
# Parameters:
# $1 - date in YYYY-MM-DD form (or almost any other form)
# Further - parts needed
################################################################################
function cxr_common_split_date()
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
		cxr_common_date_vars
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
# Function: cxr_common_date2julian
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
# > $ cxr_common_date2julian 2008-12-01 # year-month-day
# > 2454802
#
# Parameters:
# $1 - date in YYYY-MM-DD format
################################################################################
function cxr_common_date2julian() 
################################################################################
{
	# Define & Initialize local vars
	local d2j_tmpmonth
	local d2j_tmpyear
	
	case $1 in "")
		cxr_common_date_vars
		set -- $TODAY ;; 
	esac
	
	cxr_common_split_date $1 d2j_year d2j_month d2j_day || return 2

	## Calculate number of months from March
	d2j_tmpmonth=$((12 * $d2j_year + $d2j_month - 3))
	d2j_tmpyear=$(( $d2j_tmpmonth / 12))
	echo $(( (734 * $d2j_tmpmonth + 15) / 24 -  2 * $d2j_tmpyear + \
	$d2j_tmpyear/4 - $d2j_tmpyear/100 + $d2j_tmpyear/400 + $d2j_day + 1721119 ))
}


################################################################################
# Function: cxr_common_julian2date
# 
# Calculates the date in YYYY-MM-DD format from a julian date
# 
# Written by Chris Johnson, modified by dco
# Taken from <http://it.toolbox.com/wiki/index.php/Convert_a_Julian_day_to_a_date>
#
#
# Example:
# > $ cxr_common_date2julian 2454802
# > 2008-12-01
#
# Parameters:
# $1 - integer julian date (numeric)
################################################################################
function cxr_common_julian2date()
################################################################################
{
	# Check for numeric input
	if [[  $# -ne 1 || $(cxr_main_is_numeric "$1") == false   ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs one number as input"
		echo false
		return $CXR_RET_ERROR
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
# Function: cxr_common_epoch2date
# 
# Calculates the date in YYYY-MM-DD format from a Unix Epoch time,
# Unix Epoch is defined as (http://en.wikipedia.org/wiki/Unix_time)
# the number of seconds elapsed since midnight proleptic Coordinated Universal Time (UTC) of January 1, 1970.
#
# Parameters:
# $1 - integer epoch date (numeric)
################################################################################
function cxr_common_epoch2date()
################################################################################
{
	# Check for numeric input
	if [[  $# -ne 1 || $(cxr_main_is_numeric "$1") == false   ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs one number as input"
		echo false
		return $CXR_RET_ERROR
	fi
	
	local epoch_seconds=$1
	
	echo "$(date -d "1970-01-01 $epoch_seconds sec" +"%Y-%m-%d")"
}

################################################################################
# Function: cxr_common_epoch2datetime
# 
# Calculates the date in YYYY-MM-DD HH:MM:SS UTC format from a Unix Epoch time.
# Unix Epoch is defined as (http://en.wikipedia.org/wiki/Unix_time)
# the number of seconds elapsed since midnight proleptic Coordinated Universal Time (UTC) of January 1, 1970.
#
# Parameters:
# $1 - integer epoch date (numeric)
################################################################################
function cxr_common_epoch2datetime()
################################################################################
{
	# Check for numeric input
	if [[  $# -ne 1 || $(cxr_main_is_numeric "$1") == false   ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs one number as input"
		echo false
		return $CXR_RET_ERROR
	fi
	
	local epoch_seconds=$1
	
	echo "$(date -d "1970-01-01 $epoch_seconds sec" +"%Y-%m-%d %T")"
}



################################################################################
# Function: cxr_common_week_of_year
# 
# Returns the one-based week of year (2 digits)
#
# Parameters:
# $1 - date in YYYY-MM-DD format
################################################################################
function cxr_common_week_of_year() 
################################################################################
{

	if [[ $# -lt 1   ]]
	then
		cxr_main_logger -e "${FUNCNAME}"  "${FUNCNAME}:${LINENO} - needs a date of the form YYYY-MM-DD as input"
		echo false
		return $CXR_RET_ERROR
	fi

	date +%V -d $1
}

################################################################################
# Function: cxr_common_day_of_year
# 
# Calculates the number of the day in the year form a date in YYYY-MM-DD format
# Jan 01 is day number 1. (This is called the "ordinal date" in ISO terminology)
#
# Parameters:
# $1 - date in YYYY-MM-DD format
# [$2] - length of resulting string (0 padded)
################################################################################
function cxr_common_day_of_year() 
################################################################################
{

	# Define & Initialize local vars
	local year
	local len 
	local julend
	local julstart
	
	if [[ $# -lt 1 && $# -gt 2 ]]
	then
		cxr_main_logger -e "${FUNCNAME}"  "${FUNCNAME}:${LINENO} - needs a date of the form YYYY-MM-DD as input"
		echo false
		return $CXR_RET_ERROR
	fi

	year=${1:0:4}
	len=${2:-0}
	
	julend=$(cxr_common_date2julian $1)
	julstart=$(cxr_common_date2julian ${year}-01-01)
	
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
# Function: cxr_common_days_in_month
# 
# Returns the number of days in a month of a given year (needs GNU date)
#
# Parameters:
# $1 - month
# $2 - year
################################################################################
function cxr_common_days_in_month
################################################################################
{
	# Define & Initialize local vars
	local dim_m
	local dim_y
	
	if [[ $# -lt 2  ]]
	then
		cxr_main_logger -e "${FUNCNAME}"  "${FUNCNAME}:${LINENO} - needs a month and a year as input"
		echo false
		return $CXR_RET_ERROR
	fi
	
	dim_m=${1}
	dim_y=${2}

	date -d "$dim_m/01/$dim_y +1month -1day" +%d
}


################################################################################
# Function: cxr_common_days_left_in_week
# 
# Returns the number of days left in a week including the given day
# Returns 7 on Monday and 1 on Sunday
#
# Parameters:
# $1 - date
################################################################################
function cxr_common_days_left_in_week
################################################################################
{
	# Define & Initialize local vars
	local dim_d
	local dow
	
	if [[ $# -lt 1  ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs a date as input"
		echo false
		return $CXR_RET_ERROR
	fi
	
	dim_d=${1}

	dow=$(date -d "$dim_d" +%u)
	
	echo $(( 7 - $dow + 1))
}


################################################################################
# Function: cxr_common_days_left_in_month
# 
# Returns the number of days left in a month (including the given day)
#
# Parameters:
# $1 - date
################################################################################
function cxr_common_days_left_in_month
################################################################################
{
	# Define & Initialize local vars
	local month
	local dom
	local ndom
	local year
	
	if [[ $# -lt 1  ]]
	then
		cxr_main_logger -e "${FUNCNAME}"  "${FUNCNAME}:${LINENO} - needs a date as input"
		echo false
		return $CXR_RET_ERROR
	fi
	
	dim_d=${1}

	dom=$(date -d "$dim_d" +%e)
	month=$(date -d "$dim_d" +%m)
	year=$(date -d "$dim_d" +%Y)
	ndom=$(cxr_common_days_in_month $month $year)
	
	echo $(( $ndom - $dom + 1))
}


################################################################################
# Function: cxr_common_is_leap_year
# 
# Returns "true" if the year YYYY is a leap year
#
# Parameters:
# $1 - Year in YYYY form
################################################################################
function cxr_common_is_leap_year
################################################################################
{
	# Define & Initialize local vars
	local year
	
	if [[ $# -ne 1  ]]
	then
		cxr_main_logger -e "${FUNCNAME}"  "${FUNCNAME}:${LINENO} - needs a year as input"
		echo false
		return $CXR_RET_ERROR
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
# Function: cxr_common_date2offset
# 
# Takes a valid date (YYYY-MM-DD) as input and converts it to a simulation day offset.
# It does so by calculating the difference of the julian dates.
#
#
# Example (assuming the simulation starts 2007-01-01):
# > $ cxr_common_date2offset 2007-01-02
# > 1
#
# Parameters:
# $1 - date in YYYY-MM-DD format
################################################################################
function cxr_common_date2offset()
################################################################################
{
	
	# Define & Initialize local vars
	local start
	local wanted
	local offset
	
	# Check input
	if [[ $# -ne 1  ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs one YYYY-MM-DD date as input"
		echo false
		return $CXR_RET_ERROR
	fi
	
	start=$(cxr_common_date2julian ${CXR_START_DATE})
	wanted=$(cxr_common_date2julian ${1})
	
	offset=$(( ${wanted} - ${start} ))
	
	if [[ ${offset} -lt 0  ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "The date you requested is smaller than the start date of the simulation.\nMake sure to supply a correct date in YYYY-MM-DD form."
		echo false
		return $CXR_RET_ERROR
	elif [[ ${offset} -gt $(( ${CXR_NUMBER_OF_SIM_DAYS} -1 )) ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "The date you requested is larger than the end date of the simulation.\nMake sure to supply a correct date in YYYY-MM-DD form."
		echo false
		return $CXR_RET_ERROR
	else
		echo ${offset}
		return $CXR_RET_OK
	fi
	
}

################################################################################
# Function: cxr_common_offset2date
# 
# Takes a day offset as input and converts it to a raw date (YYYY-MM-DD)
# It does so by calculating the difference of the julian dates.
#
#
# Example (assuming the simulation starts 2007-01-01):
# > $ cxr_common_offset2date 1
# > 2007-01-02
#
# Parameters:
# $1 - offset
################################################################################
function cxr_common_offset2date()
################################################################################
{
	# Define & Initialize local vars
	local start
	local end
	local offset
	
	# Check input
	if [[ $# -ne 1  ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs a number as input"
		echo false
		return $CXR_RET_ERROR
	fi
	
	offset="$1"
	start=$(cxr_common_date2julian ${CXR_START_DATE})
	end=$(( $start + $offset ))
	
	echo $(cxr_common_julian2date "$end")
	
}

################################################################################
# Function: cxr_common_offset2_raw_date
# 
# Takes a day offset as input and converts it to a raw date (YYYYMMDD)
# It does so by calculating the difference of the julian dates.
#
# Example (assuming the simulation starts 2007-01-01):
# > $ cxr_common_offset2_raw_date 1
# > 20070102
#
# Parameters:
# $1 - offset
################################################################################
function cxr_common_offset2_raw_date()
################################################################################
{
	# Define & Initialize local vars
	local date
	
	# Check input
	if [[ $# -ne 1  ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs a number as input"
		echo false
		return $CXR_RET_ERROR
	fi
	
	date=$(cxr_common_offset2date "$1")
	
	echo $(cxr_common_raw_date "$date")
}

################################################################################
# Function: cxr_common_modelling_hour
# 
# Calculates the number of hours since model start (the current day does not count, since
# we are at the very beginning of it. Considers CXR_START_HOUR.
#
#
# Example (assuming the simulation starts 2007-01-01):
# > $ cxr_common_modelling_hour 2007-01-02
# > 24
#
# Parameters:
# $1 - date in YYYY-MM-DD format
################################################################################
function cxr_common_modelling_hour()
################################################################################
{
	# Define & Initialize local vars
	local offset
	
	# Check for numeric input
	if [[ $# -ne 1  ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs one YYYY-MM-DD date as input"
		echo false
		return $CXR_RET_ERROR
	fi

	offset=$(cxr_common_date2offset $1)
	
	if [[ ${offset} -lt 0  ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "The date you requested is smaller than the start date of the simulation.\nMake sure to supply a date in YYYY-MM-DD form."
		echo false
		return $CXR_RET_ERROR
	elif [[ ${offset} -gt $(( ${CXR_NUMBER_OF_SIM_DAYS} -1 )) ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "The date you requested is larger than the end date of the simulation.\nMake sure to supply a correct date in YYYY-MM-DD form."
		echo false
		return $CXR_RET_ERROR
	else
		echo $(( ( ${offset} * 24 ) - ${CXR_START_HOUR} ))
		return $CXR_RET_OK
	fi
	
}

################################################################################
# Function: cxr_common_days_between
#
# Returns the number of days between two dates (Date2 - Date1)
# 
# Parameters:
# $1 - Date 1 in YYYY-MM-DD form
# $2 - Date 2 in YYYY-MM-DD form
################################################################################	
function cxr_common_days_between()
################################################################################
{
	# Define & Initialize local vars
	local diff
	local julend
	local julstart
	
	if [[   $# -ne 2 || $(cxr_common_is_yyyymmdd_format? "$1") == false || $(cxr_common_is_yyyymmdd_format? "$2") == false    ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "needs 2 dates as input"
		echo false
		return $CXR_RET_ERROR
	fi

	# we convert the dates to julian dates and then subtract.
	julstart=$(cxr_common_date2julian $1)
	julend=$(cxr_common_date2julian $2)
	
	diff="$(( $julend - $julstart ))"
	
	if [[ $diff -lt 0  ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "Date2 is smaller than Date1"
		echo 0
		return $CXR_RET_ERROR
	fi
	
	# Now add one because of off-by-one
	diff=$(( $diff + 1))
	
	echo $diff
}

################################################################################
# Function: cxr_common_add_days
#
# Adds a number of days to a date and returns result
# 
# Parameters:
# $1 - Date in YYYY-MM-DD form
# $2 - number of days to add (integer!)
################################################################################	
function cxr_common_add_days()
################################################################################
{
	# Define & Initialize local vars
	local dateresult
	local julresult
	local julstart
	
	if [[   $# -ne 2 || $(cxr_common_is_yyyymmdd_format? "$1") == false || $(cxr_main_is_numeric "$2") == false    ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs one date and one number as input"
		echo false
		return $CXR_RET_ERROR
	fi

	# Convert to julian and add, convert back
	julstart=$(cxr_common_date2julian $1)

	julresult=$(( $julstart + $2 ))
	
	dateresult=$(cxr_common_julian2date $julresult)
	
	echo "$dateresult"
}

################################################################################
# Function: cxr_common_subtract_days
#
# Subtracts a number of days from a date and returns result
# 
# Parameters:
# $1 - Date in YYYY-MM-DD form
# $2 - number of days to subtract (integer!)
################################################################################	
function cxr_common_subtract_days()
################################################################################
{
	# Define & Initialize local vars
	local dateresult
	local julresult
	local julstart
	
	if [[   $# -ne 2 || $(cxr_common_is_yyyymmdd_format? "$1") == false || $(cxr_main_is_numeric "$2") == false    ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs one date and one number as input"
		echo false
		return $CXR_RET_ERROR
	fi

	# Convert to julian and add, convert back
	julstart=$(cxr_common_date2julian $1)

	julresult=$(( $julstart - $2 ))
	
	dateresult=$(cxr_common_julian2date $julresult)
	
	echo "$dateresult"
}

################################################################################
# Function: cxr_common_set_date_variables
#
# Exports a number of date variables from a simulation day offset
# Maybe this can be done more efficiently by using date directly
# (see http://ss64.com/bash/date.html)
#
# Example:
# > cxr_common_set_date_variables "$CXR_START_DATE" "0"
# 
# Parameters:
# $1 - Start day in YYYY-MM-DD notation
# $2 - Simulation day offset
################################################################################	
function cxr_common_set_date_variables()
################################################################################
{

	if [[   $# -ne 2 || $(cxr_common_is_yyyymmdd_format? "$1") == false || $(cxr_main_is_numeric "$2") == false    ]]
	then
		cxr_main_logger -e "${FUNCNAME}" "${FUNCNAME}:${LINENO} - needs one date and one number as input"
		echo false
		return $CXR_RET_ERROR
	fi

	START_DATE=$1
	
	# Export the offset so that we can save and restore settings
	# Alos we need it for certain date functions like cxr_common_is_first_simulation_day?
	CXR_DAY_OFFSET=$2
	
	# Set start and stop hour correctly
	if [[ $(cxr_common_is_first_simulation_day?) == "true" ]]
	then
		CXR_START_HOUR=${CXR_START_HOUR_FIRST_DAY}
		cxr_main_logger -v "We are at the first day, simulation time starts at ${CXR_START_HOUR}"
	else
		CXR_START_HOUR=0000
		cxr_main_logger -v "We are at a normal day, simulation time starts at ${CXR_START_HOUR}"
	fi
	
	if [[ $(cxr_common_is_last_simulation_day?) == "true" ]]
	then
		CXR_STOP_HOUR=${CXR_STOP_HOUR_LAST_DAY}
		cxr_main_logger -v "We are at the last day, simulation time stops at ${CXR_STOP_HOUR}"
	else
		CXR_STOP_HOUR=2400
		cxr_main_logger -v "We are at a normal day, simulation time stops at ${CXR_STOP_HOUR}"
	fi
	
	
	
	# Calculate the julian day of the start
	# then add the offset - we calculate all in "julian space"
	
	JULSTART=$(cxr_common_date2julian "$START_DATE")
	
	# Julian date
	CXR_JUL=$(( $JULSTART + $CXR_DAY_OFFSET ))
	
	# Convert to real date in YYYY-MM-DD form
	CXR_DATE=$(cxr_common_julian2date $CXR_JUL)
	
	# Date without separators (YYYYMMDD)
	CXR_DATE_RAW=$(cxr_common_raw_date $CXR_DATE)
	
	#Split it
	cxr_common_split_date $CXR_DATE year month day
	
	# YYYY year
	CXR_YEAR=$year
	
	# YY year
	CXR_YEAR_S=${CXR_YEAR:2:2}
	
	# MM month
	CXR_MONTH=$(cxr_common_two_digits $month)
	
	# DD day
	CXR_DAY=$(cxr_common_two_digits $day)
	
	# Modelling hour
	CXR_MODEL_HOUR=$(cxr_common_modelling_hour $CXR_DATE)
	
	# Day of year
	CXR_DOY=$(cxr_common_day_of_year $CXR_DATE)
	
	# Week of year
	CXR_WOY=$(cxr_common_week_of_year $CXR_DATE)
	
	# if offset is 0, we are at the initial day
	if [[ "$CXR_DAY_OFFSET" -eq 0  ]]
	then
		# any yesterday vars are undefined
		CXR_YESTERDAY=""
		# The initial meteo files are called .._s
		CXR_DAY_METEO="$(cxr_common_evaluate_rule "$CXR_DAY_METEO_FIRST_DAY_RULE" false CXR_DAY_METEO_FIRST_DAY_RULE)"
	else
		# this variable is different the first day
		CXR_DAY_METEO=${CXR_DAY}
		
		########################################
		# Calculate it all for yesterday (mainly for inst files)
		########################################	
		CXR_JUL_YESTERDAY=$(( $CXR_JUL - 1 ))
		CXR_DATE_YESTERDAY=$(cxr_common_julian2date $CXR_JUL_YESTERDAY)
		
		CXR_DATE_RAW_YESTERDAY=$(cxr_common_raw_date $CXR_DATE_YESTERDAY)
		
		#Split it
		cxr_common_split_date $CXR_DATE_YESTERDAY year_yesterday month_yesterday day_yesterday
		
	
		# YYYY year
		CXR_YEAR_YESTERDAY=$year_yesterday
		
		# YY year
		CXR_YEAR_S_YESTERDAY=${CXR_YEAR:2:2}
		
		# MM month
		CXR_MONTH_YESTERDAY=$(cxr_common_two_digits $month_yesterday)
		
		# DD day
		CXR_DAY_YESTERDAY=$(cxr_common_two_digits $day_yesterday)
		
		# Day of year
		CXR_DOY_YESTERDAY=$(cxr_common_day_of_year $CXR_DATE)
		
		# week of year
		CXR_WOY_YESTERDAY=$(cxr_common_week_of_year $CXR_DATE)
	fi
}

################################################################################
# Function: cxr_common_is_first_day_of_week?
#
# Returns true if the day currently processed is the first of a week (Monday).
#
# Parameters:
# $1 - day in YYYY-MM-DD notation
################################################################################	
function cxr_common_is_first_day_of_week? ()
################################################################################
{
	local date=$1
	local days_left=$(cxr_common_days_left_in_week $date)
	
	if [[ "$days_left" -eq 7 ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: cxr_common_is_first_day_of_month?
#
# Returns true if the day currently processed is the first of a month (01).
#
# Parameters:
# $1 - day in YYYY-MM-DD notation
################################################################################	
function cxr_common_is_first_day_of_month? ()
################################################################################
{
	local date=$1
	
	if [[ "$CXR_DAY" == "01" ]]
	then
		echo true
	else
		echo false
	fi
}

################################################################################
# Function: cxr_common_is_first_day_of_year?
#
# Returns true if the day currently processed is the first of a month (01).
#
# Parameters:
# $1 - day in YYYY-MM-DD notation
################################################################################	
function cxr_common_is_first_day_of_year? ()
################################################################################
{
	local date=$1
	
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
# Function: cxr_common_is_first_simulation_day?
#
# Returns true if the day currently processed is the first simulated.
#
# Variables:
# CXR_DAY_OFFSET - the current day offset
# 
################################################################################	
function cxr_common_is_first_simulation_day? ()
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
# Function: cxr_common_is_last_simulation_day?
#
# Returns true if the day currently processed is the last simulated
#
# Variables:
# CXR_DAY_OFFSET - the current day offset
# 
################################################################################	
function cxr_common_is_last_simulation_day? ()
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
	if [[ "${CXR_TESTING_FROM_HARNESS:-false}" == false  ]]
	then
		# We need to do initialisation
	
		# This is the run we use to test this
		CXR_RUN=$CXR_META_MODULE_TEST_RUN
	
		# Safety measure if script is not called from .
		MY_DIR=$(dirname $0) && cd $MY_DIR
	
		# We step down the directory tree until we either find CAMxRunner.sh
		# or hit the root directory /
		while [ $(pwd) != / ]
		do
			cd ..
			# If we find CAMxRunner, we are there
			ls CAMxRunner.sh >/dev/null 2>&1 && break
			
			# If we are in root, we have gone too far
			if [[ $(pwd) == /  ]]
			then
				echo "Could not find CAMxRunner.sh!"
				exit 1
			fi
		done
		
		# Save the number of tests, as other modules
		# will overwrite this (major design issue...)
		MY_META_MODULE_NUM_TESTS=$CXR_META_MODULE_NUM_TESTS
		
		# Include the init code
		source inc/init_test.inc
		
		# Plan the number of tests
		plan_tests $MY_META_MODULE_NUM_TESTS
		
	fi
	
	########################################
	# Setup tests if needed
	########################################
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	is $(cxr_common_is_yyyymmdd_format? 1999-06-12) true "cxr_common_is_yyyymmdd_format?"
	is $(cxr_common_is_yyyymmdd_format? 1999-6-12)  false "cxr_common_is_yyyymmdd_format?"
	is $(cxr_common_raw_date 2009-01-12) 20090112 "cxr_common_raw_date"
	is $(cxr_common_to_iso_date 20090112) 2009-01-12 "cxr_common_to_iso_date"
	is $(cxr_common_date2julian 2009-01-27) 2454859 "cxr_common_date2julian"
	is $(cxr_common_julian2date 2454859) 2009-01-27 "cxr_common_julian2date"
	is $(cxr_common_epoch2date 1266874169) 2010-02-22 "cxr_common_epoch2date normal"
	is $(cxr_common_epoch2date 0) 1970-01-01 "cxr_common_epoch2date base"
	is $(cxr_common_day_of_year 2009-01-01) 1 "DOY"
	is $(cxr_common_day_of_year 2009-01-01 4) 0001 "DOY trailing 0"
	is $(cxr_common_day_of_year 2003-04-12) 102 "DOY"
	is $(cxr_common_day_of_year 2009-12-31) 365 "DOY"
	is $(cxr_common_week_of_year 2009-12-31) 53 "WOY"
	is $(cxr_common_days_in_month 01 2008) 31 "cxr_common_days_in_month normal"
	is $(cxr_common_days_in_month 02 2008) 29 "cxr_common_days_in_month feb leap year"
	is $(cxr_common_days_in_month 02 2000) 29 "cxr_common_days_in_month feb leap year"
	is $(cxr_common_days_in_month 02 2001) 28 "cxr_common_days_in_month normal"
	is $(cxr_common_days_in_month 02 2000) 29 "cxr_common_days_in_month feb 2000 (leap year)"
	is $(cxr_common_days_left_in_week 2004-01-04) 1 "cxr_common_days_left_in_week"
	is $(cxr_common_days_left_in_month 2004-01-30) 2 "cxr_common_days_left_in_month"
	is $(cxr_common_days_between 2009-01-01 2009-01-01) 1 "cxr_common_days_between same day"
	is $(cxr_common_days_between 2009-01-01 2009-12-31) 365 "cxr_common_days_between one year (non-leap)"
	is $(cxr_common_days_between 2004-01-01 2004-12-31) 366 "cxr_common_days_between one year (leap)"
	is $(cxr_common_days_between 2009-01-01 2009-02-28) 59 "cxr_common_days_between"
	is $(cxr_common_add_days 2009-02-28 1) 2009-03-01 "cxr_common_add_days"
	is $(cxr_common_add_days 2004-02-28 1) 2004-02-29 "cxr_common_add_days"
	is $(cxr_common_subtract_days 2004-02-29 1) 2004-02-28 "cxr_common_subtract_days"
	is $(cxr_common_is_first_day_of_year 1900-01-01) true "cxr_common_is_first_day_of_year 1900-01-01"
	is $(cxr_common_is_first_day_of_week 2010-03-01) true "cxr_common_is_first_day_of_week 2010-03-01"
	is $(cxr_common_is_first_day_of_week 1996-10-01) false "cxr_common_is_first_day_of_week 1996-10-01"
	is $(cxr_common_is_first_day_of_month 2010-10-01) true "cxr_common_is_first_day_of_month 2010-10-01"
	
	########################################
	# teardown tests if needed
	########################################

}

################################################################################
# Are we running stand-alone? 
################################################################################


# If the CXR_META_MODULE_NAME  is not set
# somebody started this script alone
# Normlly this is not allowed, except to test using -t
if [[ -z "${CXR_META_MODULE_NAME:-}"  ]]
then

	# When using getopts, never directly call a function inside the case,
	# otherwise getopts does not process any parametres that come later
	while getopts ":dvFST" opt
	do
		case "${opt}" in
		
			d) CXR_USER_TEMP_DRY=true; CXR_USER_TEMP_DO_FILE_LOGGING=false; CXR_USER_TEMP_LOG_EXT="-dry" ;;
			v) CXR_USER_TEMP_VERBOSE=true ; echo "Enabling VERBOSE (-v) output. " ;;
			F) CXR_USER_TEMP_FORCE=true ;;
			S) CXR_USER_TEMP_SKIP_EXISTING=true ;;
			
			T) TEST_IT=true;;
			
		esac
	done
	
	# This is not strictly needed, but it allows to read 
	# non-named command line options
	shift $((${OPTIND} - 1))

	# Make getopts ready again
	unset OPTSTRING
	unset OPTIND
	
	# This is needed so that getopts surely processes all parameters
	if [[ "${TEST_IT:-false}" == true  ]]
	then
		test_module
	else
		usage
	fi
	
fi