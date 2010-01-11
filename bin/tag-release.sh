#!/usr/bin/env bash
#
# Script to tag a revision of the SVN repository for release,
# Idea from https://staff.b-mas.com/wiki/index.php/FreeMED:Subversion_Scripts
#
# Adapted by Daniel C. Oderbolz (CAMxRunner@psi.ch).
#
# Version: $Id$ 
# 
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Who	When			What
# dco 16.10.08	Adapted
# dco 17.10.08	Added new options

DEFAULT_SVNREPOS='file:///afs/psi.ch/project/MM5V3-7-4/svn'
DEFAULT_PROJECT=CAMx-runner
DEFAULT_FROM=trunk
DEFAULT_TAGS_DIR=tags

# Contral variables for options
# needed because we need to werk out all other options
# before starting an action
DO_LIST_TAGS=false

progname=$(basename "$0")

################################################################################
# Function: usage
#
# Shows the proper usage of the script
################################################################################
function usage()
################################################################################
{
	# At least in theory compatible with help2man
	cat <<EOF

	Usage: $progname [options] 
	
	$progname - Script to tag a revision of a SVN repository.
	
	Written by Daniel C. Oderbolz (dco)
	CAMxRunner@psi.ch
	
	Options:
		-h			shows this screen  
		-l			lists
		-t tag	creates a new tag called tag
		-r rep	uses repository 'rep' - Default "$DEFAULT_SVNREPOS"
		-p proj	uses project 'proj' - Default "$DEFAULT_PROJECT"
		-w loc	uses 'loc' as source - Default "$DEFAULT_FROM"
		-d dir	uses 'dir' as tag directory - Default "$DEFAULT_TAGS_DIR"
		
	Examples:
	
	To see already created tags (uses $DEFAULT_SVNREPOS and $DEFAULT_PROJECT):

		$  $progname -l
		
		To create a new tag 'mytag' in the repository (uses $DEFAULT_SVNREPOS and $DEFAULT_PROJECT)

		$  $progname -t mytag
		
		To specify another repository and project use
		
		$  $progname -t mytag -p myproject -r https://svn.master.freemedsoftware.org
EOF
exit 0
}

################################################################################
# Function: list_tags
#
# List tags available 
################################################################################
function list_tags()
################################################################################
{
	echo  "Tags available in ${SVNREPOS}/${PROJECT}/${TAG_DIR} ... "
	
	svn list ${SVNREPOS}/${PROJECT}/${TAG_DIR}
	
	exit 0
}

PROJECT=$DEFAULT_PROJECT
SVNREPOS=$DEFAULT_SVNREPOS
FROM=$DEFAULT_FROM
TAG_DIR=$DEFAULT_TAGS_DIR

while getopts ":hlt:r:p:w:d:" opt
do
	case "${opt}" in
	l) DO_LIST_TAGS=true;;
	r) SVNREPOS=${OPTARG};;
	p) PROJECT=${OPTARG};;
	w) FROM=${OPTARG};;
	d) TAG_DIR=${OPTARG};;
	t) TAG=${OPTARG};;
	h) usage;;
	*) usage ;;
	esac
done

# This is not strictly needed, but it allows to read 
# non-named command line options
shift $((${OPTIND} - 1))

# Make getopts ready again
unset OPTSTRING
unset OPTIND



if [ $DO_LIST_TAGS == true ]
then
	list_tags
fi

if [ -z "${TAG}" ]
then
	usage
fi

echo " * Tagging project '${PROJECT}' with tag '${TAG}' ..."
svn copy \
	${SVNREPOS}/${PROJECT}/${FROM} \
	${SVNREPOS}/${PROJECT}/${TAG_DIR}/${TAG} \
	-m "Tagged version \"${TAG}\""