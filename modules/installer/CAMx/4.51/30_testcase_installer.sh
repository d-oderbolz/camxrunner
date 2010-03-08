#!/usr/bin/env bash
#
# Installer for CAMx 4.51
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# TODO: 
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Installs and prepares the ENVIRON test case"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}""

CXR_META_MODULE_TYPE="${CXR_TYPE_POSTPROCESS_DAILY}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=0

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|wget"

# Min CAMxRunner Version needed (Revision number)
CXR_META_MODULE_REQ_RUNNER_VERSION=94

# Min Revision number of configuration needed (to avoid that old runs try to execute new modules)
# The revision number is automatically extracted from the config file
CXR_META_MODULE_REQ_CONF_VERSION=94
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

	Can ONLY be called by the CAMxRunner.
	
	Start the installation using the -I Option.
	
	Written by $CXR_META_MODULE_AUTHOR
	License: $CXR_META_MODULE_LICENSE
	
	Find more info here:
	$CXR_META_MODULE_DOC_URL
EOF
exit 1
}

################################################################################
# Function: CAMx_installer
#
# Downloads and prepares the ENVIRON Testcase for CAMx 4.51
#
function testcase_installer() 
################################################################################
{
	
	
	if [[ "$(cxr_common_get_consent "Do you want to install the ${CXR_MODEL_VERSION} testcase to $TESTCASE_DIR?\n\nRequires about $CXR_CAMX_TESTCASE_MEGABYTES_REQUIRED MB of space." Y )" == true  ]]
	then
		mkdir -p $TESTCASE_DIR || main.die_gracefully "Could not create $TESTCASE_DIR"
		
		########################################
		# Check space
		########################################
		cxr_common_check_mb_needed "$TESTCASE_DIR" "$CXR_CAMX_TESTCASE_MEGABYTES_REQUIRED"
		
		cd $TESTCASE_DIR || main.die_gracefully "Could change to $TESTCASE_DIR"
		
		########################################
		main.log  "Downloading three files - might take a while..."
		sleep 2
		########################################
		
		# Get the 3 files
		${CXR_WGET_EXEC} ${CXR_CAMX_TESTCASE_TGZ_MET_LOC} -O ${CXR_CAMX_TESTCASE_TGZ_MET} || main.die_gracefully "could not download $CXR_CAMX_TESTCASE_TGZ_MET_LOC"
		${CXR_WGET_EXEC} ${CXR_CAMX_TESTCASE_TGZ_IN_LOC} -O ${CXR_CAMX_TESTCASE_TGZ_IN} || main.die_gracefully "could not download $CXR_CAMX_TESTCASE_TGZ_IN_LOC"
		${CXR_WGET_EXEC} ${CXR_CAMX_TESTCASE_TGZ_OUT_LOC} -O ${CXR_CAMX_TESTCASE_TGZ_OUT} || main.die_gracefully "could not download $CXR_CAMX_TESTCASE_TGZ_OUT_LOC"
		
		########################################
		main.log  "Unpacking tar files..."
		########################################
		
		# Untar
		tar xzvf ${CXR_CAMX_TESTCASE_TGZ_MET} || main.die_gracefully "could not untar $CXR_CAMX_TESTCASE_TGZ_MET"
		tar xzvf ${CXR_CAMX_TESTCASE_TGZ_IN} || main.die_gracefully "could not untar $CXR_CAMX_TESTCASE_TGZ_IN"
		tar xzvf ${CXR_CAMX_TESTCASE_TGZ_OUT} || main.die_gracefully "could not untar $CXR_CAMX_TESTCASE_TGZ_OUT"
		
		if [[ "$(cxr_common_get_consent "Do you want to remove the tar files?\nRecommended, they are large and unneeded." Y )" == true  ]]
		then
			rm ${CXR_CAMX_TESTCASE_TGZ_MET}
			rm ${CXR_CAMX_TESTCASE_TGZ_IN}
			rm ${CXR_CAMX_TESTCASE_TGZ_OUT}
		fi
		
		########################################
		main.log  "Renaming testcase files for use with CAMxRunner run CAMx-v${CXR_MODEL_VERSION}.ENVIRON_testcase"
		########################################	
		
		# Run renamer
		${CXR_RUN_DIR}/testcase/${CXR_MODEL}/${CXR_MODEL_VERSION}/rename_inputs4CAMxRunner.sh
		
		main.log  "Done. You should now have a working testcase for CAMx ${CXR_MODEL_VERSION}.\nStart if with the run CAMx.v${CXR_MODEL_VERSION}.ENVIRON_testcase\nThe expected output is in ${CXR_RUN_DIR}/testcase/${CXR_MODEL_VERSION} and the new model output will be in ${CXR_RUN_DIR}/testcase/${CXR_MODEL_VERSION}/outputs after the run."
			
	
	fi
	
	
}


################################################################################
# Are we running stand-alone? - Can only show help
################################################################################

# If the CXR_META_MODULE_NAME  is not set,
# somebody started this script alone
if [[ -z "${CXR_META_MODULE_NAME:-}"   ]]
then
	usage
fi

################################################################################
# Code beyond this point is not executed in stand-alone operation
################################################################################




