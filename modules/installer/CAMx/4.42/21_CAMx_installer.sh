# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Installer for CAMx 4.x-5.x
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

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=0

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL="exec|wget"

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
# Downloads and compiles CAMx 4.42 and 4.51. Stores a .conf file and a .log file
# So that we can later reproduce the compilation.
#
################################################################################
function CAMx_installer() 
################################################################################
{
	if [[ "$(common.user.getOK "Do you want to compile ${CXR_MODEL} ${CXR_MODEL_VERSION}?\nRequires about $CXR_CAMX_MEGABYTES_REQUIRED MB of space.\nPlease register here: http://camx.com/down/\nalso consider joining the CAMx mailinglist <camxusers@environ.org>" Y )" == true  ]]
	then
	
		local input_dir
		local parallel_paradigm
		local probing_tool
		local domain
		
		# This is the filename we finally want
		local binary_name
		local target_prm_file
		local expected_name
		local run
		local conffile
		local logfile
		local hdf
		local mpi
		local default_platform
		
		# This is the name the Makefie chooses
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
		
		# Where to find hdf
		export MYLIBDIR=$(common.runner.evaluateRule "$MYLIBDIR_RULE" false MYLIBDIR_RULE)
		
		########################################
		# Check space
		########################################
		common.check.MbNeeded "$CXR_CAMX_SRC_DIR" "$CXR_CAMX_MEGABYTES_REQUIRED"
		
		########################################
		main.log -a  "We will install the sourcode to ${CXR_CAMX_SRC_DIR}"
		main.log -a  "Creating the target directories..."
		########################################
		
		mkdir -p "$CXR_CAMX_SRC_DIR"

		# Go to location
		cd "$CXR_CAMX_SRC_DIR" || main.dieGracefully "could not change to $CXR_CAMX_SRC_DIR"
		
		# Is the tar file already present?
		if [[ -s ${CXR_CAMX_TAR} ]]
		then
			# Does the user still want to download?
			if [[ "$(common.user.getOK "We seem to have a local copy of $(basename $CXR_CAMX_TAR). Do you want to repeat the download?" N )" == true  ]]
			then
				########################################
				main.log -a  "Downloading ..."
				########################################
				
				${CXR_WGET_EXEC} ${CXR_CAMX_TAR_LOC} -O ${CXR_CAMX_TAR} || main.dieGracefully "could not download $CXR_CAMX_TAR_LOC"
			fi
		else
			########################################
			main.log -a  "Downloading ..."
			########################################
			
			${CXR_WGET_EXEC} ${CXR_CAMX_TAR_LOC} -O ${CXR_CAMX_TAR} || main.dieGracefully "could not download $CXR_CAMX_TAR_LOC"
		fi
		
		########################################
		main.log -a  "Expanding  ${CXR_CAMX_TAR}..."
		########################################
		tar xvzf ${CXR_CAMX_TAR}
		
		# Go to directory
		cd ${CXR_CAMX_TAR_DIR} || main.dieGracefully "could not change to $CXR_CAMX_TAR_DIR"
		
		########################################
		main.log -a  "Setup Input directory containing templates..."
		########################################
		
		input_dir=${CXR_INSTALLER_INPUT_DIR}/${CXR_MODEL}/${CXR_MODEL_VERSION}/input/${CXR_MODEL}
		
		if [[ ! -d "$input_dir" ]]
		then
			main.dieGracefully "Could not find the input directory $input_dir"
		fi
		
		########################################
		main.log -a  "Determining name of binary..."
		########################################
		
		#	${CXR_MODEL}-${parallel_paradigm}-${probing_tool}-${HOSTTYPE}
		
		parallel_paradigm=$(common.user.getMenuChoice "What kind of parallel paradigm do you want to use?" "$CXR_SUPPORTED_PARALLEL_PARADIGMS" "$CXR_PARALLEL_PARADIGM")
		probing_tool=$(common.user.getMenuChoice "What kind of probing tool do you want to enable?\n(A CAMx/PMCAMx binary is optimized for one tool, we will also use this in the name of the binary)" "$CXR_SUPPORTED_CAMX_PROBING_TOOLS" "$CXR_PROBING_TOOL")
		
		# This is an argument to the make process
		# and deterimes the name of the .play file
		domain=${parallel_paradigm}-${probing_tool}-${HOSTTYPE}
		
		binary_name=${CXR_MODEL_BIN_DIR}/${CXR_MODEL}-v${CXR_MODEL_VERSION}-${domain}
		
		# The name of the .prm file to use
		target_prm_file=$CXR_CAMX_SRC_DIR/Includes/camx.prm.$domain
		
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
			run="$(basename $(common.user.getMenuChoice "Choose a run I should use (ignore the paths displayed):" "$(find "$CXR_RUN_DIR" -noleaf -maxdepth 1 -type l  2>/dev/null)" ))"
			
			binary_name=${CXR_MODEL_BIN_DIR}/${run}-${HOSTTYPE}
		elif [[ "$(common.user.getOK "Do you want to provide your own name for the binary?" N )" == true  ]]
		then
			binary_name=${CXR_MODEL_BIN_DIR}/$(common.user.getInput "What should be the name of the new binary?")
		fi
		
		main.log  "The new binary will be called $binary_name"
		
		# Here we store the current configuration
		conffile=${binary_name}.conf
		
		# Here, we store a log of this cmpilation
		logfile=${binary_name}.log
		
		# Clean the logfile
		: > "${logfile}"
		
		# Logging
		echo "This file documents a compilation of $CXR_MODEL $CXR_MODEL_VERSION, done on $(date) by $USER" >> "${logfile}"
		echo "Output Binary name: $binary_name" >> "${logfile}"
		
		# hdf? (Not reflected in name)
		hdf=$(common.user.getOK "Do you want to compile ${CXR_MODEL} with hdf support?\nthis requires the hdf library (see previous step of installation)" Y )
		
		# Logging
		echo "HDF: $hdf" >> "${logfile}"
		
		# Get the platform string and adjust some settings
		case "$parallel_paradigm" in
		
			None)
				default_platform=pg_linux
				mpi=false
				;;
			OMP)
				default_platform=pg_linuxomp
				mpi=false
				;;
			MPI)
				default_platform=pg_linuxomp
				mpi=true
				;;
				
		esac
		
		# Logging
		echo "PARALELL PARADIGM: $parallel_paradigm" >> "${logfile}"
		
		CXR_CURRENT_PLATFORM=$(common.user.getMenuChoice "What platform should be used to compile ${CXR_MODEL}?\n(Should be consistent with the parallel paradigm chosen earlier)" "$CXR_SUPPORTED_PLATFORMS" "$default_platform")
		
		# Logging
		echo "PLATFORM: $CXR_CURRENT_PLATFORM" >> "${logfile}"
		
		#File resulting from compilation due to CAMx defaults
		#CAMx 5.x adds MPI info here
		if [[ ${CXR_MODEL_VERSION:0:1} -eq 5 ]]
		then
			if [[ "$mpi" == true ]]
			then
				resulting_binary=${CXR_CAMX_SRC_DIR}/CAMx.${domain}.MPI.${CXR_CURRENT_PLATFORM}
			else
				resulting_binary=${CXR_CAMX_SRC_DIR}/CAMx.${domain}.noMPI.${CXR_CURRENT_PLATFORM}
			fi
		else
			resulting_binary=${CXR_CAMX_SRC_DIR}/CAMx.${domain}.${CXR_CURRENT_PLATFORM}
		fi
		
		########################################
		main.log -a  "Setup Input directories containing patches..."
		########################################
		
		# We compile CAMx
		CXR_CURRENT_BINARY=${CXR_MODEL}
		
		# These directories might not exist!
		patch_all_dir=$(common.runner.evaluateRule "$CXR_PATCH_ALL_DIR_RULE" false CXR_PATCH_ALL_DIR_RULE) 
		
		patch_platform_dir=$(common.runner.evaluateRule "$CXR_PATCH_PLATFORM_DIR_RULE" false CXR_PATCH_PLATFORM_DIR_RULE)

		########################################
		# Ask user for more settings
		########################################
		
		# We will operate on all files below $input_dir,
		# and we need a copy of those files
		
		draft_dir=$(common.runner.createTempDir ${FUNCNAME})
		
		main.log -a "We copy our templates to $draft_dir and work there..."
		
		cd $input_dir || main.dieGracefully "Could not change to $input_dir"
		cp -r * $draft_dir || main.dieGracefully "Could not create a copy of the templates"
		cd ${CXR_RUN_DIR} || main.dieGracefully "Could not change to $CXR_RUN_DIR"

		## Clean up draft dir
		
		# Readmes
		main.log -a "Removing README files..."
		find $draft_dir -noleaf -type f -name README.txt -exec rm -f {} \; &>/dev/null || :
		#                                                                               Do not fail on error
		# subversion drectories
		main.log -a "Removing version control system files..."
		find $draft_dir -noleaf -type d -name .svn -exec rm -rf {} \; &>/dev/null || :
		
		
		main.log -a "Working with these draft files: $(ls $draft_dir)"


		# We will now ask the user a number of questions encoded in
		# an ask-file
		# The result will be a play-file
		askfile=${CXR_INSTALLER_VERSION_INPUT_DIR}/camx.ask
		playfile=${CXR_INSTALLER_VERSION_INPUT_DIR}/${CXR_MODEL}-v${CXR_MODEL_VERSION}-${domain}.play
		
		# Might be simplified later
		if [[ -s "$playfile" ]]
		then
			# We already have a playfile
			# Do you want to replay?
			if [[ "$(common.user.getOK "${CXR_MODEL} was already installed using ${parallel_paradigm}, ${probing_tool} on ${HOSTTYPE}.\n Do you want to look at the settings that where used then?\n(You will then be asked if you want to reinstall using those values)\n\nThere is a chance that in the meantime other capabilities are available that are not yet reflected in this older file." Y )" == true  ]]
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
		
		common.user.applyPlayfile "$playfile" "$( find $draft_dir -noleaf -type f | grep -v ".svn" | grep -v README.txt)"

		########################################
		main.log -a  "Installing the changed files..."
		########################################
		
		# Just copy all out.
		cd $draft_dir || main.dieGracefully "Could not change to $draft_dir"
		cp -r * $CXR_CAMX_SRC_DIR || main.dieGracefully "Could not copy changed files back to $CXR_CAMX_SRC_DIR"
		cd ${CXR_RUN_DIR}  || main.dieGracefully "Could not change to $CXR_RUN_DIR"

		
		########################################
		main.log -a  "Prepare prm file..."
		########################################
		
		# The PRM file needs a special name!
		prm_file=$(find $draft_dir -noleaf -type f -name camx.prm)
		
		cp $prm_file $target_prm_file || main.dieGracefully "Could not prepare prm file $target_prm_file"
		
		########################################
		main.log -a  "Applying patches..."
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
		
		cd $CXR_CAMX_SRC_DIR || main.dieGracefully "Could not change to $CXR_CAMX_SRC_DIR"
		
		########################################
		main.log -a  "Compilation starts..."
		########################################
		
		echo "make clean"
		make clean 2>&1 | tee -a ${logfile} 
		
		# Test status
		if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
		then
			main.dieGracefully "make clean for ${CXR_MODEL} ${CXR_MODEL_VERSION} failed"
		fi
		
		echo "make $CXR_CURRENT_PLATFORM  DOMAIN=$domain HDF=$hdf MPI=$mpi"
		make $CXR_CURRENT_PLATFORM  DOMAIN=$domain HDF=$hdf MPI=$mpi 2>&1 | tee -a ${logfile}
		
		# Test status
		if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
		then
			main.dieGracefully "make for ${CXR_MODEL} ${CXR_MODEL_VERSION} failed"
		fi
		
		########################################
		main.log -a  "Moving binary..."
		########################################
		
		cp $resulting_binary $binary_name || main.dieGracefully "Could not copy $resulting_binary to $binary_name"
		
		########################################
		main.log -a  "Saving playfile..."
		########################################
		
		cp "$playfile" "${conffile}"
		
		########################################
		main.log -a  "Cleanup..."
		########################################
		
		# We do not need to keep the "old" binary - we copied it away
		rm $resulting_binary
		
		# removeTempFiles will also remove the draft dir
		
		
		if [[ "$(common.user.getOK "Do you want to remove the tar file $CXR_CAMX_SRC_DIR/${CXR_CAMX_TAR} ?" N )" == true  ]]
		then
			# Remove tar file
			rm $CXR_CAMX_SRC_DIR/${CXR_CAMX_TAR}
		fi
		
		main.log -a  "Done. You should now have a working ${CXR_MODEL} ${CXR_MODEL_VERSION}"
		
		
		cd ${CXR_RUN_DIR} || main.dieGracefully "Could not change to $CXR_RUN_DIR"
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