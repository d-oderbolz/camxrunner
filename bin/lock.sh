#!/usr/bin/env bash
#
# File based locking mechanism in shell script written by 
# Henrik Bilar (henrik@bilar.co.uk). 
#
# Latest version can be found at http://bilar.co.uk/wiki/
#
# This is free software and it is released under the terms 
# of the GNU GPL (http://www.gnu.org)
#
# Some minor changes made by Daniel Oderbolz (dco)

# dco: added parametrisation
LOCKDIR=${CXR_LOCK_DIR:-/tmp/locks}

# dco this was missing
if [ ! -d "$LOCKDIR" ]
then
	mkdir -p "$LOCKDIR"
fi

LOCKEXPIRE=3600
DEBUG=0
VERSION=1

debug()
{
	if [[ $DEBUG -eq 1 ]]; then 
		echo $*
	fi	
}

# Try to set a lock.
# args: lockname, timeout
# return code: 1 on failure, 0 on success
TrySetLock()
{
    LOCKNAME=$1
    
    # Create file with current pid and current time
    TMPLOCK=$LOCKDIR/tmp.$LOCKNAME.$RANDOM
    
    touch $TMPLOCK
    
    TMPLOCK_NOW=$(date +"%s");
    date +"locktime: $TMPLOCK_NOW" > $TMPLOCK
    if [[ $2 -ge 0 ]]; then
       echo "expire: $(expr $TMPLOCK_NOW + $2)" >> $TMPLOCK
    else
       echo "expire: -1" >> $TMPLOCK
    fi

    echo "PID: $(awk '{print $4}' /proc/self/stat)" >> $TMPLOCK

    # Create link - if it fails, the lock could not be created
    ln $TMPLOCK $LOCKDIR/$LOCKNAME >/dev/null 2>&1
    TRYLOCK_RC=$?

    # We don't need this file anymore as either the lock was
    # created successfully, in which case the file is now also
    # known as $LOCKDIR/$LOCKNAME, or if the lock could not be
    # set, we also don't need the file
    if [[ -f $TMPLOCK ]]; then
	rm $TMPLOCK >/dev/null 2>&1
    fi

    # The return code will be that of ln
    return $TRYLOCK_RC
}


# Unset lock
# args: lockname
# return code: 0 on success, 1 if lock was not set, 2 on
#              other errors
UnsetLock()
{
    LOCKNAME=$1

    if [[ -f $LOCKDIR/$LOCKNAME ]]; then
	rm -f $LOCKDIR/$LOCKNAME >/dev/null 2>&1
	UNSETLOCK_RC=$?
	if [[ $UNSETLOCK_RC == 0 ]]; then
	    return 0
	else
	    return 2
	fi
    fi
    return 1
}


# Expire lock
# args: lockname
# return value: 0 if lock was expired, 1 if it wasnt
ExpireLock()
{
    LOCKNAME=$1

    if [[ -f $LOCKDIR/$LOCKNAME ]]; then
	EXP_TIME_STR=$(grep "expire:" $LOCKDIR/$LOCKNAME 2>/dev/null)
	if [[ $? -ne 0 ]]; then
	    # Couldn't read lock, probably unset already
	    return 0;
	fi
	EXP_TIME=$(echo $EXP_TIME_STR | cut -f2 -d\ );
	TMPEXPIRE_NOW=$(date +"%s");
	if [[ $EXP_TIME -ge 0 && $TMPEXPIRE_NOW -gt $EXP_TIME ]]; then
	    # Later than the expire time, so unset lock
	    UnsetLock $1
	    return 0;
	else
            debug "$(expr $EXP_TIME - $TMPEXPIRE_NOW) seconds left of lock"
	    return 1
	fi
    else
	# Lock doesn't exist
	return 0;
    fi
}


HASTIMEDOUT=0
SETLOCK=0

# Set lock
# args: lockname, timeout (default of LOCKEXPIRE)
SetLock()
{
    LOCKNAME=$1
    TMP_EXPIRE=$2

    if [[ ! -n $TMP_EXPIRE ]]; then
	TMP_EXPIRE=$LOCKEXPIRE
    fi

    SETLOCK_DONE=0;

    # A dco addition: if the continue file is missing, we stop.
    # The variable is exported by the CAMxRunner
    while [[ $SETLOCK_DONE -eq 0 && $HASTIMEDOUT -eq 0 && -f ${CXR_CONTINUE_FILE} ]]; do 
	TrySetLock $LOCKNAME $TMP_EXPIRE
	TRYLOCKSET_RC=$?

	if [[ $TRYLOCKSET_RC -eq 1 ]]; then
	    # Failed to set lock
	    
	    # Can we expire the other lock?
	    debug "Trying to expire lock"

	    ExpireLock $LOCKNAME
	    EXP_LOCK_RC=$?
	    debug "EXP_LOCK_RC = $EXP_LOCK_RC"
	    if [[ $EXP_LOCK_RC -eq 1 ]]; then
		# Lock not expired yet, so need to sleep
		sleep 1
	    fi
	    # If the lock was unset, we'll set it in the next
	    # iteration, hopefully
	else
	    # Lock set
	    SETLOCK_DONE=1
            SETLOCK=1
	    if [[ -n $CHILDPID && $HASTIMEDOUT -eq 0 ]]; then
		debug "PARENT: killing $CHILDPID"
		kill $CHILDPID
	    fi
	fi
    done

    # Unset any traps we may have set
    trap - SIGCHLD
  
    # Determine if we were successful 
    if [[ $SETLOCK_DONE -eq 1 ]]; then 
        return 0
    elif [[ $HASTIMEDOUT -eq 1 ]]; then 
        return 2;
    else
        return 1;
    fi
}


TimeOutFunction()
{
    HASTIMEDOUT=1
}


case $1 in 
    "set" )
	if [[ -n $4 ]]; then 
	    debug "SETTING MAX WAIT TIME TO $4 SECONDS"
            # wait timeout specified
            trap TimeOutFunction SIGALRM
            # Start child to sleep for $4 seconds
	    KILLPID=$(awk '{print $4}' /proc/self/stat) 
	    debug "PARENT: MY PID IS "
            (   sleep $4;
		debug "CHILD: KILLING $KILLPID";
		ps -ef | awk ' { print $2 } ' | grep $KILLPID >/dev/null
		if [[ $? -eq 0 ]]; then
		    kill -ALRM $KILLPID
		fi
	    ) &
	    # Get rid of stderr, otherwise we sometimes get output from
	    # the subshell being killed
	    exec 2>/dev/null
	    CHILDPID=$!
	    debug "PARENT: CHILDS PID IS $CHILDPID"
	fi
        SetLock $2 $3
        rCode=$?
        debug "SetLock returned $rCode"
        ;;
    "unset" )
        UnsetLock $2
        rCode=$?
        debug "UnsetLock returned $rCode"
        ;;
    *)
        echo
	echo "Lock manager written in bash. Locks and unlocks named locks."
	echo 
        echo -n "Usage: $(basename $0) <unset,set> <lockname> " 
        echo "[lock expire time (sec)] [max wait time (sec)]"
        echo
        echo "To never expire a lock automatically, set exp time to -1"
        echo "To try to set a lock for n seconds and then give up, set max wait time to n."
        echo "If max wait time is not specified, the program will try indefinitely until"
        echo "it succeeds in setting the lock"
		echo 
        echo "Examples: "
        echo "  1.     $(basename $0) set testlock -1 "
        echo "  2.     $(basename $0) set testlock -1 60"
        echo "  3.     $(basename $0) set testlock 600"
        echo "  4.     $(basename $0) unset testlock"
        echo "  5.     $(basename $0) unset testlock 0 0"
        echo
        echo "  Example 1 sets the lock 'testlock' to never expire (until unset). "
        echo "  Example 2 sets the lock 'testlock' to never expire but only waits for 60 "
        echo "  seconds before timing out. Uses /my/lock/dir to store the lock"
        echo "  Example 3 sets the lock 'testlock' to expire in 10 minutes but waits"
        echo "  indefinitely until the lock has been successfully set."
        echo "  Example 4 unsets the lock 'testlock' in the default directory"
		echo "  Example 5 unsets the lock 'testlock' in /my/lock/dir directory (positional parameter!)"
        echo
        echo "Return codes:"
	echo "  set operation:"
        echo "    0   -   Lock successfully set"
        echo "    1   -   Failed to set lock"
	echo "    2   -   Timeout while setting lock"
	echo
	echo "  unset operation:"
	echo "    0   -   Successfully unset lock"
	echo "    1   -   Lock was not set"
	echo "    2   -   Error (e.g. permissions issues etc)"
	rCode=1
	;;
esac

exit $rCode
