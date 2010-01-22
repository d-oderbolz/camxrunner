#!/usr/bin/env bash
#
# Renames some input files of the ENVIRON Testcase to suit the CAMxRunner
#
# $Id$
#
#
################################################################################
# TODO: Run using loops
################################################################################
function usage()
################################################################################
{
cat <<EOF
	Usage: $0 [-h] [-u]
	
	$0 - Renames some input files of the ENVIRON Testcase to suit the CAMxRunner
	
	This is needed because the CAMxRunner cannot support different naming 
	conventions on a per-grid basis. In other words, it is not OK for the CAMxRunner
	to have a file called emiss.stl.12kmsmall.20020603.a1.bin for grid 1 and
	emiss.stl.36km.20020603.a1.bin for grid 2 because it expects them to contain the 
	grid number instead of 36km and 12km.
	note that the old jobfile supplied with the testcase works only if the files have the
	old names.
	
	Written by Dani Oderbolz (dco)
	CAMxRunner@psi.ch	
	
	Options:
	  -h       shows this screen
	  -u       undoes renaming and restores the old names

EOF
exit 1
}

################################################################################
function undo()
################################################################################
{
	# Rename Emissions
	mv emiss/emiss.stl.grid-2.20020603.a1.bin emiss/emiss.stl.12kmsmall.20020603.a1.bin 2>/dev/null
	mv emiss/emiss.stl.grid-2.20020604.a1.bin emiss/emiss.stl.12kmsmall.20020604.a1.bin 2>/dev/null

	mv emiss/emiss.stl.grid-1.20020603.a1.bin emiss/emiss.stl.36km.20020603.a1.bin 2>/dev/null
	mv emiss/emiss.stl.grid-1.20020604.a1.bin emiss/emiss.stl.36km.20020604.a1.bin 2>/dev/null
	
	# Rename Landuse files
	mv inputs/lu.STL_grid-1.bin inputs/lu.STL_36_68X68.bin 2>/dev/null
	mv inputs/lu.STL_grid-2.bin inputs/lu.STL_12_92X113.bin 2>/dev/null
	
	# Rename Meteo files
	
	mv inputs/met/camx.zp.20020603.grid-1.bin inputs/met/camx.zp.20020603.36k.bin 2>/dev/null
	mv inputs/met/camx.zp.20020604.grid-1.bin inputs/met/camx.zp.20020604.36k.bin 2>/dev/null
	
	mv inputs/met/camx.zp.20020603.grid-2.bin inputs/met/camx.zp.20020603.12ksmall.bin 2>/dev/null
	mv inputs/met/camx.zp.20020604.grid-2.bin inputs/met/camx.zp.20020604.12ksmall.bin 2>/dev/null
	
	
	mv inputs/met/camx.uv.20020603.grid-1.bin inputs/met/camx.uv.20020603.36k.bin 2>/dev/null
	mv inputs/met/camx.uv.20020604.grid-1.bin inputs/met/camx.uv.20020604.36k.bin 2>/dev/null
	
	mv inputs/met/camx.uv.20020603.grid-2.bin inputs/met/camx.uv.20020603.12ksmall.bin 2>/dev/null
	mv inputs/met/camx.uv.20020604.grid-2.bin inputs/met/camx.uv.20020604.12ksmall.bin 2>/dev/null
	
	
	mv inputs/met/camx.tp.20020603.grid-1.bin inputs/met/camx.tp.20020603.36k.bin 2>/dev/null
	mv inputs/met/camx.tp.20020604.grid-1.bin inputs/met/camx.tp.20020604.36k.bin 2>/dev/null
	                   
	mv inputs/met/camx.tp.20020603.grid-2.bin inputs/met/camx.tp.20020603.12ksmall.bin 2>/dev/null
	mv inputs/met/camx.tp.20020604.grid-2.bin inputs/met/camx.tp.20020604.12ksmall.bin 2>/dev/null
	
	
	mv inputs/met/camx.qa.20020603.grid-1.bin inputs/met/camx.qa.20020603.36k.bin 2>/dev/null
	mv inputs/met/camx.qa.20020604.grid-1.bin inputs/met/camx.qa.20020604.36k.bin 2>/dev/null
	
	mv inputs/met/camx.qa.20020603.grid-2.bin inputs/met/camx.qa.20020603.12ksmall.bin 2>/dev/null
	mv inputs/met/camx.qa.20020604.grid-2.bin inputs/met/camx.qa.20020604.12ksmall.bin 2>/dev/null
	
	
	mv inputs/met/camx.kv.20020603.grid-1.bin inputs/met/camx.kv.20020603.36k.bin
	mv inputs/met/camx.kv.20020604.grid-1.bin inputs/met/camx.kv.20020604.36k.bin
	
	mv inputs/met/camx.kv.20020603.grid-2.bin inputs/met/camx.kv.20020603.12ksmall.bin 2>/dev/null
	mv inputs/met/camx.kv.20020604.grid-2.bin inputs/met/camx.kv.20020604.12ksmall.bin 2>/dev/null
	
	
	mv inputs/met/camx.cr.20020603.grid-1.bin inputs/met/camx.cr.20020603.36k.bin 2>/dev/null
	mv inputs/met/camx.cr.20020604.grid-1.bin inputs/met/camx.cr.20020604.36k.bin 2>/dev/null
	
	mv inputs/met/camx.cr.20020603.grid-2.bin inputs/met/camx.cr.20020603.12ksmall.bin 2>/dev/null
	mv inputs/met/camx.cr.20020604.grid-2.bin inputs/met/camx.cr.20020604.12ksmall.bin 2>/dev/null
	
	exit 0
}


################################################################################
# Read the command line options
################################################################################
while getopts ":hu" opt
do
	case "${opt}" in
		u) undo ;;
		h) usage ;;
		\?) usage ;;
		*) usage ;;
	esac
done

# allows to read 
# non-named command line options
shift $((${OPTIND} - 1))

# Make getopts ready again
unset OPTSTRING
unset OPTIND

# Rename Emissions
mv emiss/emiss.stl.12kmsmall.20020603.a1.bin emiss/emiss.stl.grid-2.20020603.a1.bin 2>/dev/null
mv emiss/emiss.stl.12kmsmall.20020604.a1.bin emiss/emiss.stl.grid-2.20020604.a1.bin 2>/dev/null

mv emiss/emiss.stl.36km.20020603.a1.bin emiss/emiss.stl.grid-1.20020603.a1.bin 2>/dev/null
mv emiss/emiss.stl.36km.20020604.a1.bin emiss/emiss.stl.grid-1.20020604.a1.bin 2>/dev/null

# Rename Landuse files
mv inputs/lu.STL_36_68X68.bin inputs/lu.STL_grid-1.bin 2>/dev/null
mv inputs/lu.STL_12_92X113.bin inputs/lu.STL_grid-2.bin 2>/dev/null

# Rename Meteo files
mv inputs/met/camx.zp.20020603.36k.bin inputs/met/camx.zp.20020603.grid-1.bin 2>/dev/null
mv inputs/met/camx.zp.20020604.36k.bin inputs/met/camx.zp.20020604.grid-1.bin 2>/dev/null

mv inputs/met/camx.zp.20020603.12ksmall.bin inputs/met/camx.zp.20020603.grid-2.bin 2>/dev/null
mv inputs/met/camx.zp.20020604.12ksmall.bin inputs/met/camx.zp.20020604.grid-2.bin 2>/dev/null


mv inputs/met/camx.uv.20020603.36k.bin inputs/met/camx.uv.20020603.grid-1.bin 2>/dev/null
mv inputs/met/camx.uv.20020604.36k.bin inputs/met/camx.uv.20020604.grid-1.bin 2>/dev/null

mv inputs/met/camx.uv.20020603.12ksmall.bin inputs/met/camx.uv.20020603.grid-2.bin 2>/dev/null
mv inputs/met/camx.uv.20020604.12ksmall.bin inputs/met/camx.uv.20020604.grid-2.bin 2>/dev/null


mv inputs/met/camx.tp.20020603.36k.bin inputs/met/camx.tp.20020603.grid-1.bin 2>/dev/null
mv inputs/met/camx.tp.20020604.36k.bin inputs/met/camx.tp.20020604.grid-1.bin 2>/dev/null
                   
mv inputs/met/camx.tp.20020603.12ksmall.bin inputs/met/camx.tp.20020603.grid-2.bin 2>/dev/null
mv inputs/met/camx.tp.20020604.12ksmall.bin inputs/met/camx.tp.20020604.grid-2.bin 2>/dev/null


mv inputs/met/camx.qa.20020603.36k.bin inputs/met/camx.qa.20020603.grid-1.bin 2>/dev/null
mv inputs/met/camx.qa.20020604.36k.bin inputs/met/camx.qa.20020604.grid-1.bin 2>/dev/null

mv inputs/met/camx.qa.20020603.12ksmall.bin inputs/met/camx.qa.20020603.grid-2.bin 2>/dev/null
mv inputs/met/camx.qa.20020604.12ksmall.bin inputs/met/camx.qa.20020604.grid-2.bin 2>/dev/null


mv inputs/met/camx.kv.20020603.36k.bin inputs/met/camx.kv.20020603.grid-1.bin 2>/dev/null
mv inputs/met/camx.kv.20020604.36k.bin inputs/met/camx.kv.20020604.grid-1.bin 2>/dev/null

mv inputs/met/camx.kv.20020603.12ksmall.bin inputs/met/camx.kv.20020603.grid-2.bin 2>/dev/null
mv inputs/met/camx.kv.20020604.12ksmall.bin inputs/met/camx.kv.20020604.grid-2.bin 2>/dev/null


mv inputs/met/camx.cr.20020603.36k.bin inputs/met/camx.cr.20020603.grid-1.bin 2>/dev/null
mv inputs/met/camx.cr.20020604.36k.bin inputs/met/camx.cr.20020604.grid-1.bin 2>/dev/null

mv inputs/met/camx.cr.20020603.12ksmall.bin inputs/met/camx.cr.20020603.grid-2.bin 2>/dev/null
mv inputs/met/camx.cr.20020604.12ksmall.bin inputs/met/camx.cr.20020604.grid-2.bin 2>/dev/null


echo "Done."



