#!/bin/bash
# Executes multiple runs in a row, does compression as we go along
# $Id$
#
# Define base directories
RUNDIR=/afs/psi.ch/user/o/oderbolz/CAMxRunner
OUTDIR=/afs/psi.ch/intranet/LAC/oderbolz/CAMxRuns/Runs

# Names of runs
RUNS="CAMx-v4.51-bafu3-june-2006-s147-sem045-ni CAMx-v4.51-bafu3-june-2006-s147-sem045-nb CAMx-v4.51-bafu3-june-2006-s147-sem045-nt"

for RUN in $RUNS
do
	echo
	echo "Running $RUN"
	$RUNDIR/$RUN
	
	echo "Compressing files in $OUTDIR/$RUN"
	echo "Size before compression:"
	du -hs $OUTDIR/$RUN
	find $OUTDIR/$RUN -wholename '*/Outputs/CAMx-*.grd0?' -exec /afs/psi.ch/user/o/oderbolz/bin/pbzip2 {} \;
	echo "Size after compression:"
	du -hs $OUTDIR/$RUN
	
	echo "Compressing files in /afs/psi.ch/intranet/LAC/oderbolz/@direct"
	echo "Size before compression:"
	du -hs /afs/psi.ch/intranet/LAC/oderbolz/@direct
	find /afs/psi.ch/intranet/LAC/oderbolz/@direct -wholename './@direct/camx-v*.grd0?.asc_??' -exec /afs/psi.ch/user/o/oderbolz/bin/pbzip2 {} \;
	echo "Size after compression:"
	du -hs /afs/psi.ch/intranet/LAC/oderbolz/@direct

done



