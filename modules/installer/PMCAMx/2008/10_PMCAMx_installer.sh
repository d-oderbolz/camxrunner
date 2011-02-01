# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
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

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=0

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'


################################################################################
# Function: CAMx_installer
#
# Downloads and compiles CAMx 4.51
#
function PMCAMx_installer() 
################################################################################
{
	if [[ "$(common.user.getOK "Do you want to compile ${CXR_MODEL} ${CXR_MODEL_VERSION}?\nRequires about $CXR_PMCAMX_MEGABYTES_REQUIRED MB of space." Y )" == true  ]]
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
		common.check.MbNeeded "$CXR_PMCAMX_SRC_DIR" "$CXR_PMCAMX_MEGABYTES_REQUIRED"
		
		########################################
		#main.log  "Create the target directories..."
		########################################
		
		mkdir -p "$CXR_PMCAMX_SRC_DIR"

		# Go to location
		cd "$CXR_PMCAMX_SRC_DIR" || main.dieGracefully "could not change to $CXR_PMCAMX_SRC_DIR"
	
		########################################
		#main.log  "Downloading ..."
		########################################
		
		#${CXR_WGET_EXEC} ${CXR_PMCAMX_TAR_LOC} -O ${CXR_PMCAMX_TAR} || main.dieGracefully "could not download $CXR_PMCAMX_TAR_LOC"
		
		########################################
		#main.log  "Expanding  ${CXR_CAMX_TAR}..."
		########################################
		#tar xvzf ${CXR_PMCAMX_TAR}
		
		# Go to directory
		cd ${CXR_PMCAMX_TAR_DIR} || main.dieGracefully "could not change to $CXR_PMCAMX_TAR_DIR"
		
		########################################
		main.log  "Setup Input directory containing templates..."
		########################################
		
		input_dir=${CXR_INSTALLER_INPUT_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/input/${CXR_MODEL}
		
		if [[ ! -d "$input_dir" ]]
		then
			main.dieGracefully "Could not find the input directory $input_dir"
		fi
		
		########################################
		main.log  "Determine name of binary..."
		########################################
		
		#	${CXR_MODEL}-${HOSTTYPE}
		
		parallel_paradigm=$(common.user.getMenuChoice "What kind of parallel paradigm do you want to use?" "$CXR_SUPPORTED_PARALLEL_PARADIGMS" "$CXR_PARALLEL_PARADIGM")
		probing_tool=$(common.user.getMenuChoice "What kind of probing tool do you want to enable?\n(A CAMx/PMCAMx binary is optimized for one tool, we will also use this in the name of the binary)" "$CXR_SUPPORTED_CAMX_PROBING_TOOLS" "$CXR_PROBING_TOOL")
		
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
			main.log  "Note that your configuration expects the binary to be called $expected_name.\n Adjust CXR_PARALLEL_PARADIGM, CXR_PROBING_TOOL and check your machine type!"
		fi
		
		# Now we can add a machine name,
		# specify a run name for the binary
		# or give a completely different name
		if [[ "$(common.user.getOK "Do you want to add the machine name $(uname -n) to the name of the binary?\nUse this option if you use different machines with the same architecture but incompatible libraries on the same filesystem (normally not the case)" N )" == true  ]]
		then
			binary_name=${binary_name}-$(uname -n)
		elif [[ "$(common.user.getOK "Do you want to create a binary that is specific for a given run?" N )" == true  ]]
		then
			# Now we need to choose a run name. Look for links in the CAMx dir
			run="$(basename $(common.user.getMenuChoice "Choose a run I should use (ignore the paths displayed):" "$(find "${CXR_RUN_DIR}/" -noleaf -maxdepth 1 -type l  2>/dev/null)" ))"
			
			binary_name=${CXR_MODEL_BIN_DIR}/${run}-${HOSTTYPE}
		elif [[ "$(common.user.getOK "Do you want to provide your own name for the binary?" N )" == true  ]]
		then
			binary_name=${CXR_MODEL_BIN_DIR}/$(common.user.getInput "What should be the name of the new binary?")
		fi
		
		main.log  "The new binary will be called $binary_name"
		
		# Here we store the current configuration
		conffile=${binary_name}.conf
		
		# Here, we store a log of this cmpilation
		# We constantly add to this file
		logfile=${binary_name}.log
		
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
		
		CXR_CURRENT_PLATFORM=$(common.user.getMenuChoice "What platform should be used to compile $CXR_MODEL ?\n(Should be consistent with the parallel paradigm chosen earlier)" "$CXR_SUPPORTED_PLATFORMS" "$default_platform")
		
		# Logging
		echo "PLATFORM: $CXR_CURRENT_PLATFORM" >> "${logfile}"
		
		#File resulting from compilation due to CAMx defaults
		resulting_binary=$CXR_CAMX_SRC_DIR/CAMx.$domain_$CXR_CURRENT_PLATFORM
		
		########################################
		main.log  "Setup Input directories containing patches..."
		########################################
		
		# We compile PMCAMx
		CXR_CURRENT_BINARY=PMCAMx
		
		# These directories might not exist!
		patch_all_dir=$(common.runner.evaluateRule "$CXR_PATCH_ALL_DIR_RULE" false CXR_PATCH_ALL_DIR_RULE) 
		
		patch_platform_dir=$(common.runner.evaluateRule "$CXR_PATCH_PLATFORM_DIR_RULE" false CXR_PATCH_PLATFORM_DIR_RULE)
		
		########################################
		# Ask user for more settings
		########################################
		
		# We will operate on all files below $input_dir,
		# and we need a copy of those files
		
		draft_dir=$(common.runner.createTempDir ${FUNCNAME})
		
		cp -r $input_dir $draft_dir || main.dieGracefully "Could not make a copy of the templates"

		## Clean up draft dir
		# Readmes
		find ${draft_dir}/ -noleaf -type f -name README.txt -exec rm -f {} \;
		# subversion drectories
		find ${draft_dir}/ -noleaf -type d -name .svn -exec rm -rf {} \;


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
			if [[ "$(common.user.getOK "${CXR_MODEL} was already installed. Do you want to look at the settings that where used then?\n(You will then be asked if you want to reinstall using those values)\nThere is a chance that in the meantime other capabilities are available that are not yet reflected in this older file." Y )" == true  ]]
			then
				# Yes, show me
				cat "$playfile"
				
				if [[ "$(common.user.getOK "Should this installation be repeated with the existing settings?" N )" == true  ]]
				then
					# Playback, do nothing
					:
				else
					# Redo
					common.user.getAnswers "$askfile" "$playfile"
				fi
			else
				# Redo
				common.user.getAnswers "$askfile" "$playfile"
			fi
		else
	 		# Start from scratch
			common.user.getAnswers "$askfile" "$playfile"
		fi
		
		common.user.applyPlayfile $playfile $( find ${draft_dir}/ -noleaf -type f | grep -v ".svn" | grep -v README.txt)"

		if [[ "$(common.user.getOK "Do you want to install the new files ?" Y )" == true  ]]
		then
			# Just copy all out - the relative paths will be preserved!
			cd $draft_dir || main.dieGracefully "Could not change to $draft_dir"
			cp -r * $CXR_CAMX_SRC_DIR
			cd ${CXR_RUN_DIR}  || return $CXR_RET_ERROR
		fi
		
		########################################
		main.log  "Prepare prm file..."
		########################################
		
		# The PRM file needs a special name!
		prm_file=$(find ${draft_dir}/ -noleaf -type f -name camx.prm)
		
		cp $prm_file $target_prm_file || main.dieGracefully "Could not prepare prm file $target_prm_file"

		########################################
		main.log  "Applying patches..."
		########################################
		
		if [[ -d "$patch_all_dir"  ]]
		then
			common.install.applyPatch "$patch_all_dir" "$CXR_CAMX_SRC_DIR" "${logfile}"
		else
			main.log -w  "Did not find general patch dir $patch_all_dir"
		fi
		
		if [[ -d "$patch_platform_dir"  ]]
		then
			common.install.applyPatch "$patch_platform_dir" "$CXR_CAMX_SRC_DIR" "${logfile}"
		else
			main.log -w  "Did not find specific patch dir $patch_platform_dir"
		fi
		
		########################################
		main.log  "Compile..."
		########################################
		
		echo "make clean"
		make clean 2>&1 | tee -a ${logfile} 
		
		# Test status
		if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
		then
			main.dieGracefully "make clean for ${CXR_MODEL} ${CXR_MODEL_VERSION} failed"
		fi
		
		echo "make $CXR_CURRENT_PLATFORM  DOMAIN=$domain"
		make $CXR_CURRENT_PLATFORM DOMAIN=$domain 2>&1 | tee -a ${logfile}
		
		# Test status
		if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
		then
			main.dieGracefully "make for ${CXR_MODEL} ${CXR_MODEL_VERSION} failed"
		fi
		
		########################################
		main.log  "Moving binary..."
		########################################
		
		cp $resulting_binary $binary_name || main.dieGracefully "Could not copy $resulting_binary to $binary_name"
		
		
		########################################
		main.log  "Cleanup..."
		########################################
		
		# We no longer need the draft files
		rm -rf $draft_dir
		
		# We do not need te "old" binary - we copied it away
		rm $resulting_binary
		
		if [[ "$(common.user.getOK "Do you want to remove the tar file $CXR_CAMX_SRC_DIR/${CXR_CAMX_TAR} ?" Y )" == true  ]]
		then
			# Remove tar file
			rm $CXR_CAMX_SRC_DIR/${CXR_CAMX_TAR}
		fi
		
		main.log  "Done. You should now have a working ${CXR_MODEL} ${CXR_MODEL_VERSION}"
		
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