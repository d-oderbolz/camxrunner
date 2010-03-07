#!/usr/bin/env bash
#
# Installer for the CAMx Runner, Converter installer for 4.51 ONLY (no link!)
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
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

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Installs some pre- and postprocessors for CAMx (sources already in package)"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}""
CXR_META_MODULE_TYPE="${CXR_TYPE_INSTALLER}"

# If >0 this module supports testing via -t
CXR_META_MODULE_NUM_TESTS=0

# This is the run name that is used to test this module
CXR_META_MODULE_TEST_RUN=base

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
# Function: 	Pre_and_Postprocessor_installer
#
# Compiles a couple of pre- and postprocessor binaries.
#
function Pre_and_Postprocessor_installer() 
################################################################################
{
	if [[ "$(cxr_common_get_consent "Do you want to compile the pre- and postprocessors?" Y )" == true  ]]
	then
	
		# Is the compiler there?
		if [[ ! -x ${CXR_FORTRAN_COMPILER_EXEC}  ]]
		then
			main_dieGracefully "Compiler ${CXR_FORTRAN_COMPILER_EXEC} not found/executable, eiter adjust CXR_FORTRAN_COMPILER_EXEC (and maybe the Makefiles) or make sure the compiler is there (module add?)"
		fi
		
		########################################
		# Determine platform.
		########################################
		
		# Determine default from Machine Type
		case "$MACHTYPE" in
		
			x86_64) DEFAULT=p7-64;;
			i386) DEFAULT=p6;;
		 *) DEFAULT=px;;
		
		esac
		
		CXR_CURRENT_PLATFORM=$(cxr_common_get_menu_choice "What platform do we compile for?" "$CXR_FORTRAN_PLATFORMS" "$DEFAULT")
		
		########################################
		# Now compile all converters
		########################################
		
		# Loop through the source-directories
		for SRC_DIR in $CXR_PRE_AND_POST_PROG_SCR_ARR
		do
			if [[ "$(cxr_common_get_consent "Do you want to compile $(basename $SRC_DIR)?" )" == true  ]]
			then
				main_log -a "${FUNCNAME}" "****Compiling source in $SRC_DIR...\n"
				
				cd $SRC_DIR || main_dieGracefully "Could not change to $SRC_DIR"
				
				# Clean up whatever there was
				make clean DESTINATION="${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}"
				
				# Make it!
				make FC=${CXR_FORTRAN_COMPILER_EXEC} PLATFORM=$CXR_CURRENT_PLATFORM DESTINATION="${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}" || main_dieGracefully "The compilation did not complete successfully"
			fi
		done
		
		cd $CXR_RUN_DIR || return $CXR_RET_ERROR
		
		main_log -a "${FUNCNAME}" "Converters compiled."

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



