#!/usr/bin/env bash
#
# Installer for CAMx
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
CXR_META_MODULE_DESCRIPTION="Installs CAMx"

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
function CAMx_installer() 
################################################################################
{
	if [[ "$(cxr_common_get_consent "Do you want to compile ${CXR_MODEL} ${CXR_MODEL_VERSION}?\nRequires about $CXR_CAMX_MEGABYTES_REQUIRED MB of space.\nPlease register here: http://camx.com/down/\nalso consider joining the CAMx mailinglist <camxusers@environ.org>" Y )" == true  ]]
	then
	
		########################################
		# Setup
		########################################
		
		# Where to find hdf
		export MYLIBDIR=$(cxr_common_evaluate_rule "$MYLIBDIR_RULE" false MYLIBDIR_RULE)
		
		########################################
		# Check space
		########################################
		cxr_common_check_mb_needed "$CXR_CAMX_SRC_DIR" "$CXR_CAMX_MEGABYTES_REQUIRED"
		
		########################################
		cxr_main_logger -a "${FUNCNAME}" "We will install the sourcode to ${CXR_CAMX_SRC_DIR}"
		cxr_main_logger -a "${FUNCNAME}" "Creating the target directories..."
		########################################
		
		mkdir -p "$CXR_CAMX_SRC_DIR"

		# Go to location
		cd "$CXR_CAMX_SRC_DIR" || cxr_main_die_gracefully "could not change to $CXR_CAMX_SRC_DIR"
		
		# Is the tar file already present?
		if [[ -s ${CXR_CAMX_TAR}  ]]
		then
			# Does the user still want to download?
			if [[ "$(cxr_common_get_consent "We seem to have a local copy of $(basename $CXR_CAMX_TAR). Do you want to repeat the download?" N )" == true  ]]
			then
				########################################
				cxr_main_logger -a "${FUNCNAME}" "Downloading ..."
				########################################
				
				${CXR_WGET_EXEC} ${CXR_CAMX_TAR_LOC} -O ${CXR_CAMX_TAR} || cxr_main_die_gracefully "could not download $CXR_CAMX_TAR_LOC"
			fi
		else
			########################################
			cxr_main_logger -a "${FUNCNAME}" "Downloading ..."
			########################################
			
			${CXR_WGET_EXEC} ${CXR_CAMX_TAR_LOC} -O ${CXR_CAMX_TAR} || cxr_main_die_gracefully "could not download $CXR_CAMX_TAR_LOC"
		fi
		
		########################################
		cxr_main_logger -a "${FUNCNAME}" "Expanding  ${CXR_CAMX_TAR}..."
		########################################
		tar xvzf ${CXR_CAMX_TAR}
		
		# Go to directory
		cd ${CXR_CAMX_TAR_DIR} || cxr_main_die_gracefully "could not change to $CXR_CAMX_TAR_DIR"
		
		########################################
		cxr_main_logger -a "${FUNCNAME}" "Setup Input directory containing templates..."
		########################################
		
		INPUT_DIR=${CXR_INSTALLER_INPUT_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/input/${CXR_MODEL}
		
		if [[ ! -d "$INPUT_DIR"  ]]
		then
			cxr_main_die_gracefully "Could not find the input directory $INPUT_DIR"
		fi
		
		########################################
		cxr_main_logger -a "${FUNCNAME}" "Determine name of binary..."
		########################################
		
		#	${CXR_MODEL}-${PARALLEL_PARADIGM}-${PROBING_TOOL}-${HOSTTYPE}
		
		PARALLEL_PARADIGM=$(cxr_common_get_menu_choice "What kind of parallel paradigm do you want to use?" "$CXR_SUPPORTED_PARALLEL_PARADIGMS" "$CXR_PARALLEL_PARADIGM")
		PROBING_TOOL=$(cxr_common_get_menu_choice "What kind of probing tool do you want to enable?\n(A CAMx/PMCAMx binary is optimized for one tool, we will also use this in the name of the binary)" "$CXR_SUPPORTED_CAMX_PROBING_TOOLS" "$CXR_PROBING_TOOL")
		
		# This is an argument to the make process
		# and deterimes the name of the .play file
		DOMAIN=${PARALLEL_PARADIGM}-${PROBING_TOOL}-${HOSTTYPE}
		
		BINARY_NAME=${CXR_MODEL_BIN_DIR}/${CXR_MODEL}-${DOMAIN}
		
		# The name of the .prm file to use
		TARGET_PRM_FILE=$CXR_CAMX_SRC_DIR/Includes/camx.prm.$DOMAIN
		
		# We call get_model_exec with false to disable existance check
		EXPECTED_NAME="$(get_model_exec false)"

		# Check if CAMxRunner expects this name
		if [[ "$(basename "$EXPECTED_NAME")" != "$(basename "$BINARY_NAME")"  ]]
		then
			cxr_main_logger "${FUNCNAME}" "Note that your configuration expects the binary to be called $EXPECTED_NAME.\n Adjust CXR_PARALLEL_PARADIGM, CXR_PROBING_TOOL and check your machine type!"
		fi
		
		# Now we can add a machine name,
		# specify a run name for the binary
		# or give a completely different name
		if [[ "$(cxr_common_get_consent "Do you want to add the machine name $(uname -n) to the name of the binary?\nUse this option if you use different machines with the same architecture but incompatible libraries on the same filesystem (normally not the case)" N )" == true  ]]
		then
			BINARY_NAME=${BINARY_NAME}-$(uname -n)
		elif [[ "$(cxr_common_get_consent "Do you want to create a binary that is specific for a given run?" N )" == true  ]]
		then
			# Now we need to choose a run name. Look for links in the CAMx dir
			RUN="$(basename $(cxr_common_get_menu_choice "Choose a run I should use (ignore the paths displayed):" "$(find "$CXR_RUN_DIR" -noleaf -maxdepth 1 -type l  2>/dev/null)" ))"
			
			BINARY_NAME=${CXR_MODEL_BIN_DIR}/${RUN}-${HOSTTYPE}
		elif [[ "$(cxr_common_get_consent "Do you want to provide your own name for the binary?" N )" == true  ]]
		then
			BINARY_NAME=${CXR_MODEL_BIN_DIR}/$(cxr_common_get_user_input "What should be the name of the new binary?")
		fi
		
		cxr_main_logger "${FUNCNAME}" "The new binary will be called $BINARY_NAME"
		
		# HDF? (Not reflected in name)
		HDF=$(cxr_common_get_consent "Do you want to compile ${CXR_MODEL} with HDF support?\nthis requires the HDF library (see previous step of installation)" Y )
		
		# Get the platform string and adjust some settings
		case "$PARALLEL_PARADIGM" in
		
			None)
				DEFAULT_PLATFORM=i_linux
				MPI=false
				;;
			OMP)
				DEFAULT_PLATFORM=i_linuxomp
				MPI=false
				;;
			MPI)
				DEFAULT_PLATFORM=pg_linuxomp
				MPI=true
				;;
				
		esac
		
		CXR_CURRENT_PLATFORM=$(cxr_common_get_menu_choice "What platform should be used to compile ${CXR_MODEL}?\n(Should be consistent with the parallel paradigm chosen earlier)" "$CXR_SUPPORTED_PLATFORMS" "$DEFAULT_PLATFORM")
		
		#File resulting from compilation due to CAMx defaults
		RESULTING_BINARY=$CXR_CAMX_SRC_DIR/CAMx.$DOMAIN.$CXR_CURRENT_PLATFORM
		
		########################################
		cxr_main_logger -a "${FUNCNAME}" "Setup Input directories containing patches..."
		########################################
		
		# We compile CAMx
		CXR_CURRENT_BINARY=${CXR_MODEL}
		
		# These directories might not exist!
		PATCH_ALL_DIR=$(cxr_common_evaluate_rule "$CXR_PATCH_ALL_DIR_RULE" false CXR_PATCH_ALL_DIR_RULE) 
		
		PATCH_PLATFORM_DIR=$(cxr_common_evaluate_rule "$CXR_PATCH_PLATFORM_DIR_RULE" false CXR_PATCH_PLATFORM_DIR_RULE)

		########################################
		# Ask user for more settings
		########################################
		
		# We will operate on all files below $INPUT_DIR,
		# and we need a copy of those files
		
		DRAFT_DIR=$(mktemp -d ${CXR_TMP_DIR}/${FUNCNAME}-dir.XXXXXXXX)
		
		cxr_main_logger -a $FUNCNAME "We copy our templates to $DRAFT_DIR and work there..."
		
		cd $INPUT_DIR || cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Could not change to $INPUT_DIR"
		cp -r * $DRAFT_DIR || die_gracefully "Could not create a copy of the templates"
		cd ${CXR_RUN_DIR} || cxr_main_die_gracefully "Could not change to $CXR_RUN_DIR"
		
		## Clean up draft dir
		# Readmes
		find $DRAFT_DIR -noleaf -type f -name README.txt -exec rm -f {} \; 2>/dev/null
		# subversion drectories
		find $DRAFT_DIR -noleaf -type d -name .svn -exec rm -rf {} \; 2>/dev/null


		# We will now ask the user a number of questions encoded in
		# an ask-file
		# The result will be a play-file
		ASKFILE=${CXR_INSTALLER_VERSION_INPUT_DIR}/camx.ask
		PLAYFILE=${CXR_INSTALLER_VERSION_INPUT_DIR}/${CXR_MODEL}-${DOMAIN}.play
		
		# Might be simplified later
		if [[ -s "$PLAYFILE"  ]]
		then
			# We already have a playfile
			# Do you want to replay?
			if [[ "$(cxr_common_get_consent "${CXR_MODEL} was already installed using ${PARALLEL_PARADIGM}, ${PROBING_TOOL} on ${HOSTTYPE}.\n Do you want to look at the settings that where used then? (You will then be asked if you want to reinstall using those values)" Y )" == true  ]]
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
	 		# Start from scratch
			cxr_common_get_answers "$ASKFILE" "$PLAYFILE"
		fi
		
		cxr_common_apply_playfile "$PLAYFILE" "$( find $DRAFT_DIR -noleaf -type f | grep -v ".svn" | grep -v README.txt)"

		########################################
		cxr_main_logger -a "${FUNCNAME}" "Installing the changed files..."
		########################################
		
		# Just copy all out.
		cd $DRAFT_DIR || cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Could not change to $DRAFT_DIR"
		cp -r * $CXR_CAMX_SRC_DIR || die_gracefully "Could not copy changed files back to $CXR_CAMX_SRC_DIR"
		cd ${CXR_RUN_DIR}  || cxr_main_die_gracefully "Could not change to $CXR_RUN_DIR"

		
		########################################
		cxr_main_logger -a "${FUNCNAME}" "Prepare prm file..."
		########################################
		
		# The PRM file needs a special name!
		PRM_FILE=$(find $DRAFT_DIR -noleaf -type f -name camx.prm)
		
		cp $PRM_FILE $TARGET_PRM_FILE || cxr_main_die_gracefully "Could not prepare prm file $TARGET_PRM_FILE"
		
		########################################
		cxr_main_logger -a "${FUNCNAME}" "Applying patches..."
		########################################
		
		if [[ -d "$PATCH_ALL_DIR"  ]]
		then
			cxr_common_apply_patches "$PATCH_ALL_DIR" "$CXR_CAMX_SRC_DIR"
		else
			cxr_main_logger -w "${FUNCNAME}" "Did not find general patch dir $PATCH_ALL_DIR"
		fi
		
		if [[ -d "$PATCH_PLATFORM_DIR"  ]]
		then
			cxr_common_apply_patches "$PATCH_PLATFORM_DIR" "$CXR_CAMX_SRC_DIR"
		else
			cxr_main_logger -w "${FUNCNAME}" "Did not find specific patch dir $PATCH_PLATFORM_DIR"
		fi
		
		cd $CXR_CAMX_SRC_DIR || cxr_main_die_gracefully "${FUNCNAME}:${LINENO} - Could not change to $CXR_CAMX_SRC_DIR"
		
		########################################
		cxr_main_logger -a "${FUNCNAME}" "Compile..."
		########################################
		
		echo "make clean"
		make clean || cxr_main_die_gracefully "make clean for ${CXR_MODEL} ${CXR_MODEL_VERSION} failed"
		
		echo "make $CXR_CURRENT_PLATFORM  DOMAIN=$DOMAIN HDF=$HDF MPI=$MPI"
		make $CXR_CURRENT_PLATFORM  DOMAIN=$DOMAIN HDF=$HDF MPI=$MPI || cxr_main_die_gracefully "make for ${CXR_MODEL} ${CXR_MODEL_VERSION} failed"
		
		########################################
		cxr_main_logger -a "${FUNCNAME}" "Moving binary..."
		########################################
		
		cp $RESULTING_BINARY $BINARY_NAME || cxr_main_die_gracefully "Could not copy $RESULTING_BINARY to $BINARY_NAME"
		
		########################################
		cxr_main_logger -a "${FUNCNAME}" "Saving playfile..."
		########################################
		
		cp "$PLAYFILE" "${BINARY_NAME}.conf"
		
		########################################
		cxr_main_logger -a "${FUNCNAME}" "Cleanup..."
		########################################
		
		# We do not need te "old" binary - we copied it away
		rm $RESULTING_BINARY
		
		# We no longer need the draft files
		rm -rf $DRAFT_DIR
		
		
		if [[ "$(cxr_common_get_consent "Do you want to remove the tar file $CXR_CAMX_SRC_DIR/${CXR_CAMX_TAR} ?" N )" == true  ]]
		then
			# Remove tar file
			rm $CXR_CAMX_SRC_DIR/${CXR_CAMX_TAR}
		fi
		
		cxr_main_logger -a "${FUNCNAME}" "Done. You should now have a working ${CXR_MODEL} ${CXR_MODEL_VERSION}"
		
		
		cd ${CXR_RUN_DIR} || cxr_main_die_gracefully "Could not change to $CXR_RUN_DIR"
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




