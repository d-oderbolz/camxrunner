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
# TODO: Apply changes of CAMx
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
	if [[ "$(cxr_common_get_consent "Do you want to compile ${CXR_MODEL} ${CXR_MODEL_VERSION}?\nRequires about $CXR_PMCAMX_MEGABYTES_REQUIRED MB of space." Y )" == true  ]]
	then
	
		local input_dir
		local parallel_paradigm
		local probing_tool
		local domain
		local binary_name
		local target_prm_file
		local expected_name
		local run
		local conffile
		local logfile
		local default_platform
		local resulting_binary
		local patch_all_dir
		local patch_platform_dir
		local draft_dir
		local askfile
		local playfile
		local prm_file
		
		########################################
		# Setup
		########################################
		
		# Currently not needed.
		
		########################################
		# Check space
		########################################
		cxr_common_check_mb_needed "$CXR_PMCAMX_SRC_DIR" "$CXR_PMCAMX_MEGABYTES_REQUIRED"
		
		########################################
		#main.log "${FUNCNAME}" "Create the target directories..."
		########################################
		
		mkdir -p "$CXR_PMCAMX_SRC_DIR"

		# Go to location
		cd "$CXR_PMCAMX_SRC_DIR" || main.die_gracefully "could not change to $CXR_PMCAMX_SRC_DIR"
	
		########################################
		#main.log "${FUNCNAME}" "Downloading ..."
		########################################
		
		#${CXR_WGET_EXEC} ${CXR_PMCAMX_TAR_LOC} -O ${CXR_PMCAMX_TAR} || main.die_gracefully "could not download $CXR_PMCAMX_TAR_LOC"
		
		########################################
		#main.log "${FUNCNAME}" "Expanding  ${CXR_CAMX_TAR}..."
		########################################
		#tar xvzf ${CXR_PMCAMX_TAR}
		
		# Go to directory
		cd ${CXR_PMCAMX_TAR_DIR} || main.die_gracefully "could not change to $CXR_PMCAMX_TAR_DIR"
		
		########################################
		main.log "${FUNCNAME}" "Setup Input directory containing templates..."
		########################################
		
		input_dir=${CXR_INSTALLER_INPUT_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/input/${CXR_MODEL}
		
		if [[ ! -d "$input_dir" ]]
		then
			main.die_gracefully "Could not find the input directory $input_dir"
		fi
		
		########################################
		main.log "${FUNCNAME}" "Determine name of binary..."
		########################################
		
		#	${CXR_MODEL}-${HOSTTYPE}
		
		parallel_paradigm=$(cxr_common_get_menu_choice "What kind of parallel paradigm do you want to use?" "$CXR_SUPPORTED_PARALLEL_PARADIGMS" "$CXR_PARALLEL_PARADIGM")
		probing_tool=$(cxr_common_get_menu_choice "What kind of probing tool do you want to enable?\n(A CAMx/PMCAMx binary is optimized for one tool, we will also use this in the name of the binary)" "$CXR_SUPPORTED_CAMX_PROBING_TOOLS" "$CXR_PROBING_TOOL")
		
		binary_name=${CXR_MODEL_BIN_DIR}/${CXR_MODEL}-${parallel_paradigm}-${probing_tool}-${HOSTTYPE}
		
		# This is an argument to the make process
		# and deterimes the name of the .play file
		domain=${HOSTTYPE}
		
		# The name of the .prm file to use
		# At a later stage, we should derive the directory structure automatically.
		# Note the subtle diference betwen CAMx and PMCAMx (Includes V. Inc)
		target_prm_file=$CXR_CAMX_SRC_DIR/Inc/camx.prm.$domain
		
		# We call get_model_exec with false to disable existance check
		expected_name="$(get_model_exec false)"

		# Check if CAMxRunner expects this name
		if [[ "$(basename "$expected_name")" != "$(basename "$binary_name")"  ]]
		then
			main.log "${FUNCNAME}" "Note that your configuration expects the binary to be called $expected_name.\n Adjust CXR_PARALLEL_PARADIGM, CXR_PROBING_TOOL and check your machine type!"
		fi
		
		# Now we can add a machine name,
		# specify a run name for the binary
		# or give a completely different name
		if [[ "$(cxr_common_get_consent "Do you want to add the machine name $(uname -n) to the name of the binary?\nUse this option if you use different machines with the same architecture but incompatible libraries on the same filesystem (normally not the case)" N )" == true  ]]
		then
			binary_name=${binary_name}-$(uname -n)
		elif [[ "$(cxr_common_get_consent "Do you want to create a binary that is specific for a given run?" N )" == true  ]]
		then
			# Now we need to choose a run name. Look for links in the CAMx dir
			run="$(basename $(cxr_common_get_menu_choice "Choose a run I should use (ignore the paths displayed):" "$(find "$CXR_RUN_DIR" -noleaf -maxdepth 1 -type l  2>/dev/null)" ))"
			
			binary_name=${CXR_MODEL_BIN_DIR}/${run}-${HOSTTYPE}
		elif [[ "$(cxr_common_get_consent "Do you want to provide your own name for the binary?" N )" == true  ]]
		then
			binary_name=${CXR_MODEL_BIN_DIR}/$(cxr_common_get_user_input "What should be the name of the new binary?")
		fi
		
		main.log "${FUNCNAME}" "The new binary will be called $binary_name"
		
		# Here we store the current configuration
		conffile=${binary_name}.conf
		
		# Here, we store a log of this cmpilation
		logfile=${binary_name}.log
		
		# Clean the logfile
		: > "${logfile}"
		
		# Logging
		echo "This file documents a compilation of $CXR_MODEL $CXR_MODEL_VERSION, done on $(date) by $USER" >> "${logfile}"
		echo "Output Binary name: $binary_name" >> "${logfile}"
			
		# Get the platform string
		
		#Default is linux using intel compiler
		if [[ "$parallel_paradigm" == None  ]]
		then
			default_platform=linux
		else
			default_platform=linuxomp
		fi
		
		CXR_CURRENT_PLATFORM=$(cxr_common_get_menu_choice "What platform should be used to compile $CXR_MODEL ?\n(Should be consistent with the parallel paradigm chosen earlier)" "$CXR_SUPPORTED_PLATFORMS" "$default_platform")
		
		# Logging
		echo "PLATFORM: $CXR_CURRENT_PLATFORM" >> "${logfile}"
		
		#File resulting from compilation due to CAMx defaults
		resulting_binary=$CXR_CAMX_SRC_DIR/CAMx.$domain_$CXR_CURRENT_PLATFORM
		
		########################################
		main.log "${FUNCNAME}" "Setup Input directories containing patches..."
		########################################
		
		# We compile PMCAMx
		CXR_CURRENT_BINARY=PMCAMx
		
		# These directories might not exist!
		patch_all_dir=$(cxr_common_evaluate_rule "$CXR_PATCH_ALL_DIR_RULE" false CXR_PATCH_ALL_DIR_RULE) 
		
		patch_platform_dir=$(cxr_common_evaluate_rule "$CXR_PATCH_PLATFORM_DIR_RULE" false CXR_PATCH_PLATFORM_DIR_RULE)
		
		########################################
		# Ask user for more settings
		########################################
		
		# We will operate on all files below $input_dir,
		# and we need a copy of those files
		
		draft_dir=$(mktemp -d)
		
		cp -r $input_dir $draft_dir || main.die_gracefully "Could not make a copy of the templates"

		## Clean up draft dir
		# Readmes
		find $draft_dir -noleaf -type f -name README.txt -exec rm -f {} \;
		# subversion drectories
		find $draft_dir -noleaf -type d -name .svn -exec rm -rf {} \;


		# We will now ask the user a number of questions encoded in
		# a ask-file
		# The result will be a play-file
		askfile=${CXR_INSTALLER_VERSION_INPUT_DIR}/camx.ask
		playfile=${CXR_INSTALLER_VERSION_INPUT_DIR}/${domain}.play
		
		# Might be simplified later
		if [[ -s "$playfile"  ]]
		then
			# We already have a playfile
			# Do you want to replay?
			if [[ "$(cxr_common_get_consent "${CXR_MODEL} was already installed. Do you want to look at the settings that where used then?\n(You will then be asked if you want to reinstall using those values)\nThere is a chance that in the meantime other features are available that are not yet reflected in this older file." Y )" == true  ]]
			then
				# Yes, show me
				cat "$playfile"
				
				if [[ "$(cxr_common_get_consent "Should this installation be repeated with the existing settings?" N )" == true  ]]
				then
					# Playback, do nothing
					:
				else
					# Redo
					cxr_common_get_answers "$askfile" "$playfile"
				fi
			else
				# Redo
				cxr_common_get_answers "$askfile" "$playfile"
			fi
		else
	 		# Start from scratch
			cxr_common_get_answers "$askfile" "$playfile"
		fi
		
		cxr_common_apply_playfile $playfile $( find $draft_dir -noleaf -type f | grep -v ".svn" | grep -v README.txt)"

		if [[ "$(cxr_common_get_consent "Do you want to install the new files ?" Y )" == true  ]]
		then
			# Just copy all out - the relative paths will be preserved!
			cd $draft_dir || main.die_gracefully "${FUNCNAME}:${LINENO} - Could not change to $draft_dir"
			cp -r * $CXR_CAMX_SRC_DIR
			cd ${CXR_RUN_DIR}  || return $CXR_RET_ERROR
		fi
		
		########################################
		main.log "${FUNCNAME}" "Prepare prm file..."
		########################################
		
		# The PRM file needs a special name!
		prm_file=$(find $draft_dir -noleaf -type f -name camx.prm)
		
		cp $prm_file $target_prm_file || main.die_gracefully "Could not prepare prm file $target_prm_file"

		########################################
		main.log "${FUNCNAME}" "Applying patches..."
		########################################
		
		if [[ -d "$patch_all_dir"  ]]
		then
			cxr_common_apply_patches "$patch_all_dir" "$CXR_CAMX_SRC_DIR" "${logfile}"
		else
			main.log -w "${FUNCNAME}" "Did not find general patch dir $patch_all_dir"
		fi
		
		if [[ -d "$patch_platform_dir"  ]]
		then
			cxr_common_apply_patches "$patch_platform_dir" "$CXR_CAMX_SRC_DIR" "${logfile}"
		else
			main.log -w "${FUNCNAME}" "Did not find specific patch dir $patch_platform_dir"
		fi
		
		########################################
		main.log "${FUNCNAME}" "Compile..."
		########################################
		
		echo "make clean"
		make clean 2>&1 | tee -a ${logfile} 
		
		# Test status
		if [[ $(cxr_common_array_zero "${PIPESTATUS[@]}") == false ]]
		then
			main.die_gracefully "make clean for ${CXR_MODEL} ${CXR_MODEL_VERSION} failed"
		fi
		
		echo "make $CXR_CURRENT_PLATFORM  DOMAIN=$domain"
		make $CXR_CURRENT_PLATFORM DOMAIN=$domain 2>&1 | tee -a ${logfile}
		
		# Test status
		if [[ $(cxr_common_array_zero "${PIPESTATUS[@]}") == false ]]
		then
			main.die_gracefully "make for ${CXR_MODEL} ${CXR_MODEL_VERSION} failed"
		fi
		
		########################################
		main.log "${FUNCNAME}" "Moving binary..."
		########################################
		
		cp $resulting_binary $binary_name || main.die_gracefully "Could not copy $resulting_binary to $binary_name"
		
		
		########################################
		main.log "${FUNCNAME}" "Cleanup..."
		########################################
		
		# We no longer need the draft files
		rm -rf $draft_dir
		
		# We do not need te "old" binary - we copied it away
		rm $resulting_binary
		
		if [[ "$(cxr_common_get_consent "Do you want to remove the tar file $CXR_CAMX_SRC_DIR/${CXR_CAMX_TAR} ?" Y )" == true  ]]
		then
			# Remove tar file
			rm $CXR_CAMX_SRC_DIR/${CXR_CAMX_TAR}
		fi
		
		main.log "${FUNCNAME}" "Done. You should now have a working ${CXR_MODEL} ${CXR_MODEL_VERSION}"
		
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




