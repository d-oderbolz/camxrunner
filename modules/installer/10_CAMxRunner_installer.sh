# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
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

# If >0, this module supports testing
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


################################################################################
# Function: 	CAMxRunner_installer
#
# Installs the CAMxRunner and other functions needed.
#
function CAMxRunner_installer() 
################################################################################
{
	
	# This function asks automatically
	common.test.all "${CXR_MODEL}" "${CXR_MODEL_VERSION}"

	if [[ "$(common.user.getOK "Do you want to generate a new base.conf file?" N )" == true  ]]
	then
		# Yes
		
		if [[ "$(common.user.getOK "Do you want to have a look at the existing base.conf file?" )" == true  ]]
		then
			echo -e "********************************************************************************"
			cat ${CXR_BASECONFIG}
			echo -e "********************************************************************************\n\n"
		fi

		# The template we use (can be chosen more elaborate, maybe)
		TEMPLATE=${CXR_TEMPLATES_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/base.tpl

		# This is the unfinished file
		DRAFTFILE=$(common.runner.createTempFile $FUNCNAME)

		########################################
		# Prepare draft file
		########################################
		cp $TEMPLATE $DRAFTFILE || main.dieGracefully "Could not copy $CXR_BASECONFIG to the draft file $DRAFTFILE"

		# We will now ask the user a number of questions encoded in an ask-file
		# The result will be a play-file
		ASKFILE=${CXR_TEMPLATES_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/base.ask
		PLAYFILE=${CXR_INSTALLER_VERSION_INPUT_DIR}/CAMxRunner.play
		
		# Might be simplified later
		if [[ -s "$PLAYFILE" ]]
		then
			# We already have a playfile
			# Do you want to replay?
			if [[ "$(common.user.getOK "CAMxRunner was already installed. Do you want to look at the settings that where used then?\n(You will then be asked if you want to reinstall using those values)\nThere is a chance that in the meatime other capabilities are available that are not yet reflected in this older file." Y )" == true  ]]
			then
				# Yes, show me
				cat "$PLAYFILE"
				
				if [[ "$(common.user.getOK "Should this installation be repeated with the existing settings?" N )" == true  ]]
				then
					# Playback, do nothing
					:
				else
					# Redo
					common.user.getAnswers "$ASKFILE" "$PLAYFILE"
				fi
			else
				# Redo
				common.user.getAnswers "$ASKFILE" "$PLAYFILE"
			fi
		else
	 	# From scratch
			common.user.getAnswers "$ASKFILE" "$PLAYFILE"
		fi

		common.user.applyPlayfile $PLAYFILE $DRAFTFILE

		########################################
		# We have all values, we can copy the file
		########################################
		
		if [[ "$(common.user.getOK "Do you want to install the new file ?" Y )" == true  ]]
		then
			cp $DRAFTFILE $CXR_BASECONFIG || main.dieGracefully "Could not copy $DRAFTFILE to $CXR_BASECONFIG!"
		fi
		
		main.log  "You must manually adjust the ${CXR_CONF_DIR}/site.conf file - add any site specific settings there\nAlso, you might need to change CAMx Version specific settings in ${CXR_CONFIG_DIR}/${CXR_MODEL}-v${CXR_MODEL_VERSION}.conf"
		
	fi
	
	##############################################################################
	if [[ "$(common.user.getOK "Do you want to regenerate the API documentation?" N )" == true  ]]
	then
		main.log  "Regenerating API documentation..."
		$CXR_API_DOC_EXEC
	fi

}

################################################################################
# Function: test_module
#
# Runs the predefined tests for this module. If you add or remove tests, please
# update CXR_META_MODULE_NUM_TESTS in the header!
# 
################################################################################	
function test_module()
################################################################################
{

	########################################
	# Setup tests if needed
	########################################
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	# None yet.
	:

	########################################
	# teardown tests if needed
	########################################
	
}