# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Installer for the CAMx Runner, Converter installer for 4.51 ONLY (no link!)
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id: 22_Pre_and_Postprocessor_installer.sh 5816 2011-02-19 12:01:57Z oderbolz $ 
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

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=0

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id: 22_Pre_and_Postprocessor_installer.sh 5816 2011-02-19 12:01:57Z oderbolz $'

################################################################################
# Function: 	Pre_and_Postprocessor_installer
#
# Compiles a couple of pre- and postprocessor binaries.
#
function Pre_and_Postprocessor_installer() 
################################################################################
{
	if [[ "$(common.user.getOK "Do you want to compile the pre- and postprocessors?" Y )" == true  ]]
	then
	
		# Is the compiler there?
		if [[ ! -x ${CXR_FORTRAN_COMPILER_EXEC} ]]
		then
			main.dieGracefully "Compiler ${CXR_FORTRAN_COMPILER_EXEC} not found/executable, eiter adjust CXR_FORTRAN_COMPILER_EXEC (and maybe the Makefiles) or make sure the compiler is there (module add?)"
		fi

		########################################
		# Now compile all converters
		########################################
		
		# Loop through the source-directories
		for SRC_DIR in $CXR_PRE_AND_POST_PROG_SCR_ARR
		do
			if [[ "$(common.user.getOK "Do you want to compile $(basename $SRC_DIR)?" )" == true  ]]
			then
				main.log -a  "****Compiling source in $SRC_DIR...\n"
				
				cd $SRC_DIR || main.dieGracefully "Could not change to $SRC_DIR"
				
				# Clean up whatever there was
				main.log -a "make clean DESTINATION=${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}"
				make clean DESTINATION="${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}"
				
				# Make it!
				main.log -a "make FC=${CXR_FORTRAN_COMPILER_EXEC} DESTINATION=${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}"
				make FC=${CXR_FORTRAN_COMPILER_EXEC} DESTINATION="${CXR_BIN_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}" || main.dieGracefully "The compilation did not complete successfully"
			fi
		done
		
		cd $CXR_RUN_DIR || return $CXR_RET_ERROR
		
		main.log -a  "Converters compiled."

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