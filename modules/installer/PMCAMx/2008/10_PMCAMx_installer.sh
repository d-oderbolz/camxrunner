#!/usr/bin/env bash
#
# Installer for CAMx
# See http://people.web.psi.ch/oderbolz/CAMxRunner 
#
# Version: $Id$ 
#
# Relatively similar to CAMx 4.42, but: 
# - No HDF
# - No Parallel option
# - Currently not downloadable (will change)
#
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# TODO: Make more flexible, no explicit prm treatment
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Installs PMCAMx"

# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}""
CXR_META_MODULE_TYPE=${CXR_TYPE_INSTALLER}

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
# Function: CAMx_installer
#
# Downloads and compiles CAMx 4.51
#
function PMCAMx_installer() 
################################################################################
{
	if [ "$(cxr_common_get_consent "Do you want to compile ${CXR_MODEL} ${CXR_MODEL_VERSION}?\nRequires about $CXR_PMCAMX_MEGABYTES_REQUIRED MB of space." Y )" == true ]
	then
	
		########################################
		# Setup
		########################################
		
		# Currently not needed.
		
		########################################
		# Check space
		########################################
		cxr_common_check_mb_needed "$CXR_PMCAMX_SRC_DIR" "$CXR_PMCAMX_MEGABYTES_REQUIRED"
		
		########################################
		#cxr_main_logger "${FUNCNAME}" "Create the target directories..."
		########################################
		
		#mkdir -p "$CXR_PMCAMX_SRC_DIR"

		# Go to location
		cd "$CXR_PMCAMX_SRC_DIR" || cxr_main_die_gracefully "could not change to $CXR_PMCAMX_SRC_DIR"
	
		########################################
		#cxr_main_logger "${FUNCNAME}" "Downloading ..."
		########################################
		
		#${CXR_WGET_EXEC} ${CXR_PMCAMX_TAR_LOC} -O ${CXR_PMCAMX_TAR} || cxr_main_die_gracefully "could not download $CXR_PMCAMX_TAR_LOC"
		
		########################################
		#cxr_main_logger "${FUNCNAME}" "Expanding  ${CXR_CAMX_TAR}..."
		########################################
		#tar xvzf ${CXR_PMCAMX_TAR}
		
		# Go to directory
		cd ${CXR_PMCAMX_TAR_DIR} || cxr_main_die_gracefully "could not change to $CXR_PMCAMX_TAR_DIR"
		
		########################################
		cxr_main_logger "${FUNCNAME}" "Setup Input directory containing templates..."
		########################################
		
		INPUT_DIR=${CXR_INSTALLER_INPUT_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/input/${CXR_MODEL}
		
		if [ ! -d "$INPUT_DIR" ]
		then
			cxr_main_die_gracefully "Could not find the input directory $INPUT_DIR"
		fi
		
		########################################
		cxr_main_logger "${FUNCNAME}" "Determine name of binary..."
		########################################
		
		#	${CXR_MODEL}-${HOSTTYPE}
		
		PARALLEL_PARADIGM=$(cxr_common_get_menu_choice "What kind of parallel paradigm do you want to use?" "$CXR_SUPPORTED_PARALLEL_PARADIGMS" "$CXR_PARALLEL_PARADIGM")
		PROBING_TOOL=$(cxr_common_get_menu_choice "What kind of probing tool do you want to enable?\n(A CAMx/PMCAMx binary is optimized for one tool, we will also use this in the name of the binary)" "$CXR_SUPPORTED_CAMX_PROBING_TOOLS" "$CXR_PROBING_TOOL")
		
		BINARY_NAME=${CXR_MODEL_BIN_DIR}/${CXR_MODEL}-${PARALLEL_PARADIGM}-${PROBING_TOOL}-${HOSTTYPE}
		
		# This is an argument to the make process
		# and deterimes the name of the .play file
		DOMAIN=${HOSTTYPE}
		
		# The name of the .prm file to use
		# At a later stage, we should derive the directory structure automatically.
		# Note the subtle diference betwen CAMx and PMCAMx (Includes V. Inc)
		TARGET_PRM_FILE=$CXR_CAMX_SRC_DIR/Inc/camx.prm.$DOMAIN
		
		# We call get_model_exec with false to disable existance check
		EXPECTED_NAME="$(get_model_exec false)"

		# Check if CAMxRunner expects this name
		if [ "$(basename "$EXPECTED_NAME")" != "$(basename "$BINARY_NAME")" ]
		then
			cxr_main_logger "${FUNCNAME}" "Note that your configuration expects the binary to be called $EXPECTED_NAME.\n Adjust CXR_PARALLEL_PARADIGM, CXR_PROBING_TOOL and check your machine type!"
		fi
		
		# Now we can add a machine name
		if [ "$(cxr_common_get_consent "Do you want to add the machine name $(uname -n) to the name of the binary?\nUse this option if you use different machines with the same architecture but incompatible libraries on the same filesystem (normally not the case)" N )" == true ]
		then
			BINARY_NAME=${BINARY_NAME}-$(uname -n)
		fi
		
		cxr_main_logger "${FUNCNAME}" "The new binary will be called $BINARY_NAME"
			
		# Get the platform string
		
		#Default is linux using intel compiler
		if [ "$PARALLEL_PARADIGM" == None ]
		then
			DEFAULT_PLATFORM=linux
		else
			DEFAULT_PLATFORM=linuxomp
		fi
		
		CXR_CURRENT_PLATFORM=$(cxr_common_get_menu_choice "What platform should be used to compile $CXR_MODEL ?\n(Should be consistent with the parallel paradigm chosen earlier)" "$CXR_SUPPORTED_PLATFORMS" "$DEFAULT_PLATFORM")
		
		#File resulting from compilation due to CAMx defaults
		RESULTING_BINARY=$CXR_CAMX_SRC_DIR/CAMx.$DOMAIN.$CXR_CURRENT_PLATFORM
		
		########################################
		cxr_main_logger "${FUNCNAME}" "Setup Input directories containing patches..."
		########################################
		
		# We compile PMCAMx
		CXR_CURRENT_BINARY=PMCAMx
		
		# These directories might not exist!
		PATCH_ALL_DIR=$(cxr_common_evaluate_rule "$CXR_PATCH_ALL_DIR_RULE" false CXR_PATCH_ALL_DIR_RULE) 
		
		PATCH_PLATFORM_DIR=$(cxr_common_evaluate_rule "$CXR_PATCH_PLATFORM_DIR_RULE" false CXR_PATCH_PLATFORM_DIR_RULE)
		
		########################################
		# Ask user for more settings
		########################################
		
		# We will operate on all files below $INPUT_DIR,
		# and we need a copy of those files
		
		DRAFT_DIR=$(mktemp -d)
		
		cp -r $INPUT_DIR $DRAFT_DIR || die_gracefully "Could not make a copy of the templates"

		## Clean up draft dir
		# Readmes
		find $DRAFT_DIR -noleaf -type f -name README.txt -exec rm -f {} \;
		# subversion drectories
		find $DRAFT_DIR -noleaf -type d -name .svn -exec rm -rf {} \;


		# We will now ask the user a number of questions encoded in
		# a ask-file
		# The result will be a play-file
		ASKFILE=${CXR_INSTALLER_VERSION_INPUT_DIR}/camx.ask
		PLAYFILE=${CXR_INSTALLER_VERSION_INPUT_DIR}/${DOMAIN}.play
		
		# Might be simplified later
		if [ -s "$PLAYFILE" ]
		then
			# We already have a playfile
			# Do you want to replay?
			if [ "$(cxr_common_get_consent "${CXR_MODEL} was already installed. Do you want to look at the settings that where used then? (You will then be asked if you want to reinstall using those values)" Y )" == true ]
			then
				# Yes, show me
				cat "$PLAYFILE"
				
				if [ "$(cxr_common_get_consent "Should this installation be repeated with the existing settings?" N )" == true ]
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
	 		# Start from scratch
			cxr_common_get_answers "$ASKFILE" "$PLAYFILE"
		fi
		
		cxr_common_apply_playfile $PLAYFILE $( find $DRAFT_DIR -noleaf -type f | grep -v ".svn" | grep -v README.txt)"

		if [ "$(cxr_common_get_consent "Do you want to install the new files ?" Y )" == true ]
		then
			# Just copy all out - the relative paths will be preserved!
			cd $DRAFT_DIR || cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Could not change to $DRAFT_DIR"
			cp -r * $CXR_CAMX_SRC_DIR
			cd ${CXR_RUN_DIR}  || return $CXR_RET_ERROR
		fi
		
		########################################
		cxr_main_logger "${FUNCNAME}" "Prepare prm file..."
		########################################
		
		# The PRM file needs a special name!
		PRM_FILE=$(find $DRAFT_DIR -noleaf -type f -name camx.prm)
		
		cp $PRM_FILE $TARGET_PRM_FILE || cxr_main_die_gracefully "Could not prepare prm file $TARGET_PRM_FILE"

		########################################
		cxr_main_logger "${FUNCNAME}" "Applying patches..."
		########################################
		
		if [ -d "$PATCH_ALL_DIR" ]
		then
			cxr_common_apply_patches "$PATCH_ALL_DIR" "$CXR_CAMX_SRC_DIR"
		fi
		
		if [ -d "$PATCH_PLATFORM_DIR" ]
		then
			cxr_common_apply_patches "$PATCH_PLATFORM_DIR" "$CXR_CAMX_SRC_DIR"
		fi
		
		########################################
		cxr_main_logger "${FUNCNAME}" "Compile..."
		########################################
		
		echo "make clean"
		make clean || cxr_main_die_gracefully "make clean for ${CXR_MODEL} ${CXR_MODEL_VERSION} failed"
		
		echo "make $CXR_CURRENT_PLATFORM  DOMAIN=$DOMAIN"
		make $CXR_CURRENT_PLATFORM  DOMAIN=$DOMAIN || cxr_main_die_gracefully "make for ${CXR_MODEL} ${CXR_MODEL_VERSION} failed"
		
		########################################
		cxr_main_logger "${FUNCNAME}" "Moving binary..."
		########################################
		
		cp $RESULTING_BINARY $BINARY_NAME || cxr_main_die_gracefully "Could not copy $RESULTING_BINARY to $BINARY_NAME"
		
		
		########################################
		cxr_main_logger "${FUNCNAME}" "Cleanup..."
		########################################
		
		# We no longer need the draft files
		rm -rf $DRAFT_DIR
		
		# We do not need te "old" binary - we copied it away
		rm $RESULTING_BINARY
		
		if [ "$(cxr_common_get_consent "Do you want to remove the tar file $CXR_CAMX_SRC_DIR/${CXR_CAMX_TAR} ?" Y )" == true ]
		then
			# Remove tar file
			rm $CXR_CAMX_SRC_DIR/${CXR_CAMX_TAR}
		fi
		
		cxr_main_logger "${FUNCNAME}" "Done. You should now have a working ${CXR_MODEL} ${CXR_MODEL_VERSION}"
		
	fi
}


################################################################################
# Are we running stand-alone? - Can only show help
################################################################################

# If the CXR_META_MODULE_NAME  is not set,
# somebody started this script alone
if [ -z "${CXR_META_MODULE_NAME:-}"  ]
then
	usage
fi

################################################################################
# Code beyond this point is not executed in stand-alone operation
################################################################################




