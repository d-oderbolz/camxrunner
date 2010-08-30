#!/usr/bin/env bash
#
# Script to export a tag of the SVN repository for release,
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
DEFAULT_PROJECT=CAMxRunner
DEFAULT_FROM=tags

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
	
	$progname - Script to export a tag of a SVN repository for release.
	
	Written by Daniel C. Oderbolz (dco)
	CAMxRunner@psi.ch
	
	Options:
		-h			shows this screen  
		-l			lists
		-v ver	exports version "ver"
		-r rep	uses repository "rep" - Default "$DEFAULT_SVNREPOS"
		-p proj	uses project "proj" - Default "$DEFAULT_PROJECT"
		-w loc	uses "loc" as source - Default "$DEFAULT_FROM"
		
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
	echo  "Tags available in ${SVNREPOS}/${PROJECT}/${FROM} ... "
	
	svn list ${SVNREPOS}/${PROJECT}/${FROM}
	
	exit 0
}

# Set Defaults
PROJECT=$DEFAULT_PROJECT
SVNREPOS=$DEFAULT_SVNREPOS
FROM=$DEFAULT_FROM

while getopts ":hlt:r:p:w:v:" opt
do
	case "${opt}" in
	l) DO_LIST_TAGS=true;;
	r) SVNREPOS=${OPTARG};;
	p) PROJECT=${OPTARG};;
	w) FROM=${OPTARG};;
	v) VERSION=${OPTARG};;
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

# Put path together
BRANCH="${FROM}/${VERSION}"

if [[ $DO_LIST_TAGS == true  ]]
then
	list_tags
fi

if [[ -z "${VERSION}"  ]]
then
	usage
fi

################################################################################	
echo " - Using branch ${BRANCH}. "
echo " * Checking version ${VERSION} of ${PROJECT} out of Subversion ... "
################################################################################	
svn export ${SVNREPOS}/${PROJECT}/${BRANCH} ${PROJECT}-${VERSION}

################################################################################	
echo " * Re-Building NaturalDocs documentation ... "
################################################################################	
( cd ${PROJECT}-${VERSION}; bin/generate-api-doc.sh )

echo -n " * Building Changelog from Subversion XML log (may take some time) ... "
svn --verbose --xml log \
    ${SVNREPOS}/${PROJECT}/${BRANCH} | \
xsltproc --stringparam strip-prefix trunk \
    --stringparam linelen 75 \
    --stringparam include-rev yes \
    ${PROJECT}-${VERSION}/bin/svn2cl.xsl - > ${PROJECT}-${VERSION}/doc/ChangeLog
echo "[done]"

################################################################################	
echo -n " * Creating a tarball ... "
################################################################################	
tar czf ${PROJECT}_${VERSION}.tar.gz ${PROJECT}-${VERSION}
echo "[done]"

################################################################################	
echo -n " * Cleaning up ... "
################################################################################	
rm -Rf ${PROJECT}-${VERSION}
echo "[done]"