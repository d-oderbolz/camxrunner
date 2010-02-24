#!/usr/bin/env bash
#
# Installer for the CAMx Runner
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
CXR_META_MODULE_DESCRIPTION="installs the CAMxRunner"

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
# Function: 	CAMxRunner_installer
#
# Installs the CAMxRunner and other functions needed.
#
function CAMxRunner_installer() 
################################################################################
{
	
	# This function asks automatically
	cxr_common_test_all_modules "${CXR_MODEL}" "${CXR_MODEL_VERSION}"

	if [[ "$(cxr_common_get_consent "Do you want to generate a new base.conf file?" N )" == true  ]]
	then
		# Yes
		
		if [[ "$(cxr_common_get_consent "Do you want to have a look at the existing base.conf file?" )" == true  ]]
		then
			echo -e "********************************************************************************"
			cat ${CXR_BASECONFIG}
			echo -e "********************************************************************************\n\n"
		fi

		# The template we use (can be chosen more elaborate, maybe)
		TEMPLATE=${CXR_TEMPLATES_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/base.tpl

		# This is the unfinished file
		DRAFTFILE=$(cxr_common_create_tempfile $FUNCNAME)

		########################################
		# Prepare draft file
		########################################
		cp $TEMPLATE $DRAFTFILE || cxr_main_die_gracefully "Could not copy $CXR_BASECONFIG to the draft file $DRAFTFILE"

		# We will now ask the user a number of questions encoded in an ask-file
		# The result will be a play-file
		ASKFILE=${CXR_TEMPLATES_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/base.ask
		PLAYFILE=${CXR_INSTALLER_VERSION_INPUT_DIR}/CAMxRunner.play
		
		# Might be simplified later
		if [[ -s "$PLAYFILE"  ]]
		then
			# We already have a playfile
			# Do you want to replay?
			if [[ "$(cxr_common_get_consent "CAMxRunner was already installed. Do you want to look at the settings that where used then? (You will then be asked if you want to reinstall using those values)" Y )" == true  ]]
			then
				# Yes, show me
				cat "$PLAYFILE"
				
				if [[ "$(cxr_common_get_consent "Should this installation be repeated with the existing settings?" N )" == true  ]]
				then
					# Playback, do nothing
					:
				else
					# Redo
					cxr_common_get_answers "$ASKFILE" "$PLAYFILE"
				fi
			else
				# Redo
				cxr_common_get_answers "$ASKFILE" "$PLAYFILE"
			fi
		else
	 	# From scratch
			cxr_common_get_answers "$ASKFILE" "$PLAYFILE"
		fi

		cxr_common_apply_playfile $PLAYFILE $DRAFTFILE

		########################################
		# We have all values, we can copy the file
		########################################
		
		if [[ "$(cxr_common_get_consent "Do you want to install the new file ?" Y )" == true  ]]
		then
			cp $DRAFTFILE $CXR_BASECONFIG || cxr_main_die_gracefully "Could not copy $DRAFTFILE to $CXR_BASECONFIG!"
		fi
		
		cxr_main_logger "${FUNCNAME}" "You must manually adjust the ${CXR_CONF_DIR}/site.conf file - add any site specific settings there\nAlso, you might need to change CAMx Version specific settings in ${CXR_CONFIG_DIR}/${CXR_MODEL}-v${CXR_MODEL_VERSION}.conf"
		
	fi
	
	##############################################################################
	if [[ "$(cxr_common_get_consent "Do you want to regenerate the API documentation?" N )" == true  ]]
	then
		cxr_main_logger "${FUNCNAME}" "Regenerating API documentation..."
		$CXR_API_DOC_EXEC
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




