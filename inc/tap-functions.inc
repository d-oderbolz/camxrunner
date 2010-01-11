# Tap-functions, a bash library to generate TAP (test anything protocol) messages
# Written by Patrick LeBoutillier <patl@cpan.org> 
# Modified by Daniel Oderbolz to be incorporated into CAMxRunner.
#
# See http://testanything.org
#
# These functions now write their output to a .tap file that can be parsed later
#
#EXAMPLE:
#   #!/bin/bash
# 
#   . tap-functions
# 
#   plan_tests 7
# 
#   me=$USER
#   is $USER $me "I am myself"
#   like $HOME $me "My home is mine"
#   like "`id`" $me "My id matches myself"
# 
#   /bin/ls $HOME 1>&2
#   ok $? "/bin/ls $HOME"
#   # Same thing using okx shortcut
#   okx /bin/ls $HOME
# 
#   [[ "`id -u`" != "0" ]]
#   i_am_not_root=$?
#   skip $i_am_not_root "Must be root" || {
#     okx ls /root
#   }
# 
#   TODO="figure out how to become root..."
#   okx [ "$HOME" == "/root" ]
#   unset TODO

################################################################################
# Function: plan_tests
#
# Tells the harness how many tests we intend to run
#
# Parameters:
# $1 - number of tests to run
################################################################################
function plan_tests()
################################################################################
{
	local tests="${1:-}"

	(( ${_plan_set:-0} != 0 )) && _die "You tried to plan twice!"
	(( ${tests:-0} == 0 )) && _die "You said to run 0 tests!  You've got to run something."

	_print_plan ${tests:-0}
	_expected_tests=${tests:-0}
	_plan_set=1

	return ${tests:-0}
}

################################################################################
# Function: _print_plan
#
# INTERNAL harness function
#
# Parameters:
# $1 - number of tests
# $2 - directive (like TODO or SKIP)
################################################################################
function _print_plan()
################################################################################
{
	local tests="${1:?}"
	local directive="${2:-''}"

	echo -n "1..${tests:-0}"
	[[ -n "$directive" ]] && echo -n " # $directive"
	echo
}

################################################################################
# Function: pass
#
# Tells the harness that the current test has passed. This is useful if the test is more complex 
# than simple equalit or inequality.
#
# Parameters:
# $1 - name of the current test
################################################################################
function pass()
################################################################################
{
	local name="$1"
	ok 0 "$name"
}

################################################################################
# Function: fail
#
# Contrary of <pass>
#
# Parameters:
# $1 - name of the current test
################################################################################
function fail()
################################################################################
{
	local name="$1"
	ok 1 "$name"
}

################################################################################
# Function: ok
#
# This is the workhorse method that actually prints the TAP compatible tests result.
# This function is usually not called from outside (use <okx>,<pass> or <fail> instead)
# The output of this function goes to a TAP file and stdout.
#
# Parameters:
# $1 - result (if this is 0, test ok, else nok)
# $2 - name of the current test
################################################################################
function ok()
################################################################################
{
	local result="${1:-}"
	local name="${2:-''}"

	(( ${_plan_set:-0} == 0 )) && _die "You tried to run a test without a plan!  Gotta have a plan."

	_executed_tests=$(( ${_executed_tests:-0} + 1 ))

	if [[ -n "$name" ]] ; then
		if _matches "$name" "^[0-9]+$" ; then
			diag "    You named your test '$name'.  You shouldn't use numbers for your test names."
			diag "    Very confusing."
		fi
	fi

	if (( result != 0 )) ; then
		echo -n "not "
		_failed_tests=$(( ${_failed_tests:-0} + 1 ))
	fi
	echo -n "ok ${_executed_tests:-0}"

	if [[ -n "$name" ]] ; then
		local ename=${name//\#/\\#}
		echo -n " - $ename"
	fi

	if [[ -n "${TODO:-}" ]] ; then
		echo -n " # TODO ${TODO:-}" ;
		if (( ${result:-0} != 0 )) ; then
			_failed_tests=$(( ${_failed_tests:-0} - 1 ))
		fi
	fi

	echo
	if (( result != 0 )) ; then
		local file='tap-functions'
		local func=
		local line=

		local i=0
		local bt=$(caller $i)
		while _matches "$bt" "tap-functions$" ; do
			i=$(( $i + 1 ))
			bt=$(caller $i)
		done
		local backtrace=
		eval $(caller $i | (read line func file ; echo "backtrace=\"$file:$func() at line $line.\""))
			
		local t=
		[[ -n "${TODO:-}" ]] && t="(TODO) "

		if [[ -n "$name" ]] ; then
			diag "  Failed ${t}test '$name'"
			diag "  in $backtrace"
		else
			diag "  Failed ${t}test in $backtrace"
		fi
	fi

	return $result
}

################################################################################
# Function: okx
#
# Executes a command and depending on return status, we pass or fail
#
# Parameters:
# $1 - the command to run with all parameters
################################################################################
function okx()
################################################################################
{
	local command="$@"

	local line=
	diag "Output of '$command':"
	$command | while read line ; do
		diag "$line"
	done
	ok ${PIPESTATUS[0]} "$command"
}


_equals(){
	local result=${1:-}
	local expected=${2:-}

	if [[ "$result" == "$expected" ]] ; then
		return 0
	else 
		return 1
	fi
}

################################################################################
# Function: _matches
#
# INTERNAL Allows pattern matching against a result (expected result is formulated as a pattern)
#
# Parameters:
# $1 - an actual result of a command
# $2 - a pattern to match the result against
################################################################################
function _matches()
################################################################################
{
	local result="${1:-}"
	local pattern="${2:-}"
	
	# Thanks to Aaron Kangas for the patch to allow regexp matching
	# under bash < 3.
 	_bash_major_version=${BASH_VERSION%%.*}
 	
	if [[ -z "$result" || -z "$pattern" ]] ; then
		return 1
	else
		if (( _bash_major_version >= 3 )) ; then
			[[ "$result" =~ "$pattern" ]]
		else
			echo "$result" | egrep -q "$pattern"
		fi
	fi
}

################################################################################
# Function: _is_diag
#
# INTERNAL shows diagnosis data
#
# Parameters:
# $1 - actual result
# $2 - expected result
################################################################################
function _is_diag()
################################################################################
{
	local result="${1:-}"
	local expected="${2:-}"

	diag "         got: '$result'" 
	diag "    expected: '$expected'"
}

################################################################################
# Function: is
#
# Tests for equality, the most important test function.
#
# Example:
# > is $(cxr_common_abs 0) 0 "cxr_common_abs of 0 is 0"
#
# Parameters:
# $1 - actual result
# $2 - expected result
# [$3] - test name
################################################################################
function is()
################################################################################
{

	local result="${1:-}"
	local expected="${2:-}"
	local name="${3:-''}"

	_equals "$result" "$expected"
	(( $? == 0 ))
	ok $? "$name"
	local r=$?
	(( r != 0 )) && _is_diag "$result" "$expected"
	return $r 
}

################################################################################
# Function: isnt
#
# Contrary of <is>
#
# Parameters:
# $1 - actual result
# $2 - expected result
# [$3] - test name
################################################################################
function isnt()
################################################################################
{
	local result="${1:-}"
	local expected="${2:-}"
	local name="${3:-''}"

	_equals "$result" "$expected"
	(( $? != 0 ))
	ok $? "$name"
	local r=$?
	(( r != 0 )) && _is_diag "$result" "$expected"
	return $r 
}

################################################################################
# Function: like
#
# Allows pattern matching of an actual result to an expected pattern.
#
# Parameters:
# $1 - actual result
# $2 - expected result
# [$3] - test name
################################################################################
function like()
################################################################################
{
	local result="${1:-}"
	local pattern="${2:-}"
	local name="${3:-''}"

	_matches "$result" "$pattern"
	(( $? == 0 ))
	ok $? "$name"
	local r=$?
	(( r != 0 )) && diag "    '$result' doesn't match '$pattern'"
	return $r
}


################################################################################
# Function: unlike
#
# Contrary of <like>
#
# Parameters:
# $1 - actual result
# $2 - expected result
# [$3] - test name
################################################################################
function unlike()
################################################################################
{
	local result="${1:-}"
	local pattern="${2:-}"
	local name="${3:-''}"

	_matches "$result" "$pattern"
	(( $? != 0 ))
	ok $? "$name"
	local r=$?
	(( r != 0 )) && diag "    '$result' matches '$pattern'"
	return $r
}

################################################################################
# Function: skip
#
# Allows you to skip tests conditionally
#
# Parameters:
# $1 - condition (if == 0, we will skip)
# $2 - a reason to skip
# $3 - n number of tests to skip
################################################################################
function skip()
################################################################################
{
	local condition="${1:?}"
	local reason="${2:-''}"
	local n="${3:-1}"

	if (( condition == 0 )) ; then
		local i=
		for (( i=0 ; i<$n ; i++ )) ; do
			_executed_tests=$(( ${_executed_tests:-0} + 1 ))
			echo "ok ${_executed_tests:-0} # skip: $reason" 
		done
		return 0
	else
		return
	fi
}

################################################################################
# Function: diag
#
# INTERNAL write log info
#
# Parameters:
# $1 - message
################################################################################
function diag()
################################################################################
{
	local msg="${1:?}"

	if [[ -n "$msg" ]] ; then
		cxr_main_logger "TAP" "# $msg"
	fi
	
	return 1
}

################################################################################
# Function: _die
#
# INTERNAL kills test run
#
# Parameters:
# $1 - reason
################################################################################
function _die()
################################################################################
{
	local reason="${1:-'<unspecified error>'}"

	# Call our logger
	cxr_main_logger -e "TAP" "$reason"
	
	_test_died=1
	_exit 255
}

################################################################################
# Function: _exit
#
# Ends test and shows test summary
#
################################################################################
function _exit()
################################################################################
{
	local rc=0

	if (( ${_plan_set:-0} == 0 )) ; then
		diag "Looks like your test died before it could output anything."
		return $rc
	fi

	if (( ${_test_died:-0} != 0 )) ; then
		diag "Looks like your test died just after ${_executed_tests:-0}."
		return $rc
	fi

	if (( ${_skip_all:-0} == 0 && ${_no_plan:-0} != 0 )) ; then
		_print_plan ${_executed_tests:-0}
	fi

	local s=
	if (( ${_no_plan:-0} == 0 && ${_expected_tests:-0} < ${_executed_tests:-0} )) ; then
		s= ; (( ${_expected_tests:-0} > 1 )) && s=s
		local extra=$(( ${_executed_tests:-0} - ${_expected_tests:-0} ))
		diag "Looks like you planned ${_expected_tests:-0} test$s but ran $extra extra."
		rc=-1 ;
	fi

	if (( ${_no_plan:-0} == 0 && ${_expected_tests:-0} > ${_executed_tests:-0} )) ; then
		s= ; (( ${_expected_tests:-0} > 1 )) && s=s
		diag "Looks like you planned ${_expected_tests:-0} test$s but only ran ${_executed_tests:-0}."
	fi

	if (( ${_failed_tests:-0} > 0 )) ; then
		s= ; (( ${_failed_tests:-0} > 1 )) && s=s
		diag "Looks like you failed ${_failed_tests:-0} test$s of ${_executed_tests:-0}."
	else
		diag "Looks like all tests (${_executed_tests:-0}/${_executed_tests:-0}) passed!."
	fi

	return $rc
}

################################################################################
# Function: summarize_tests
#
# Wrapper to call <_exit> under a better name
#
# Parameters:
# $1 -
################################################################################
function summarize_tests()
################################################################################
{
	_exit
}
