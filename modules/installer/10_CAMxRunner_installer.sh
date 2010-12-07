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

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2010), CAMxRunner@psi.ch"

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
	local libdir
	local executable
	local binary_name
	local src_dir
	local suffix
	local logfile
	local backupfile
	
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
			# Backup first
			backupfile=${CXR_BASECONFIG%%.conf}.bak
			
			if [[ -e "$backupfile" ]]
			then
				backupfile=${backupfile}.${RANDOM}
			fi
			
			main.log -a "Backing up ${CXR_BASECONFIG} to ${backupfile}..."
			
			cp ${CXR_BASECONFIG} ${backupfile} || main.dieGracefully "Could not create backup ${backupfile}"
			
			cp ${DRAFTFILE} ${CXR_BASECONFIG} || main.dieGracefully "Could not copy $DRAFTFILE to $CXR_BASECONFIG!"
		fi
		
		main.log  "You must manually adjust the ${CXR_CONF_DIR}/site.conf file - add any site specific settings there\nAlso, you might need to change CAMx Version specific settings in ${CXR_CONFIG_DIR}/${CXR_MODEL}-v${CXR_MODEL_VERSION}.conf"
		
	fi
	
	##############################################################################
	if [[ "$(common.user.getOK "Do you want to regenerate the API documentation?" N )" == true  ]]
	then
		main.log  "Regenerating API documentation..."
		$CXR_API_DOC_EXEC
	fi
	
	##############################################################################
	if [[ "$(common.user.getOK "Do you want to compile essential executables for CAMxRunner? (not model specific)" N )" == true  ]]
	then
	
		if [[ "$(common.user.getOK "Do you want to create machine-specific programs?\n(by appending -${CXR_MACHINE} to the names)" )" == true ]]
		then
			suffix=-${CXR_MACHINE}
		else
			suffix=""
		fi
	
		# Loop through the source-directories
		for src_dir in $CXR_BIN_SCR_ARR
		do
			
			executable="$(basename "$src_dir")"
			# Init some vars needed for some compilations
			export CPPFLAGS=""
			export CFLAGS=""
			export LDFLAGS=""
			export LIBS=""
			
			if [[ "$(common.user.getOK "Do you want to compile $executable ?" )" == true  ]]
			then
				binary_name=${CXR_BIN_DIR}/${executable}-${HOSTTYPE}${suffix}
				
				# We constantly add to this file
				logfile=${binary_name}.log
				
				echo "**** $(date) Compiling source in $src_dir on $(uname -n)...\n" | tee -a $logfile
				
				if [[ -L "$src_dir" ]]
				then
					echo "(a link to $(common.fs.getLinkTarget $src_dir))" | tee -a $logfile
				fi
				
				cd $src_dir || main.dieGracefully "Could not change to $src_dir"
				
				libdir=${CXR_LIB_DIR}/${executable}/$HOSTTYPE
				mkdir -p $libdir
				
				# Configure when compiling proj or lzop
				if [[ $executable == proj ]]
				then
					
					./configure --prefix=${CXR_BIN_DIR} \
					            --exec-prefix=${CXR_BIN_DIR} \
					            --bindir=${CXR_BIN_DIR} \
					            --libdir=${libdir} \
					            --includedir=${CXR_TMP_DIR}  \
					            --program-suffix=-${HOSTTYPE}${suffix} \
					            --datarootdir=${CXR_BIN_DIR} \
					            --enable-shared=no \
					            --enable-static=no \
					            --without-jni | tee -a $logfile
					            
				elif [[ $executable == lzo ]]
				then
					
					./configure --prefix=${CXR_BIN_DIR} \
					            --exec-prefix=${CXR_BIN_DIR} \
					            --bindir=${CXR_BIN_DIR} \
					            --sbindir=${CXR_BIN_DIR} \
					            --includedir=${libdir} \
					            --libdir=${libdir} \
					            --program-suffix=-${HOSTTYPE}${suffix} \
					            --enable-shared=yes \
					            --enable-static=yes   | tee -a $logfile	
										            
				elif [[ $executable == lzop ]]
				then
				
					export CPPFLAGS="-I${CXR_LIB_DIR}/lzo/${HOSTTYPE}/lzo"
					export CFLAGS="$CPPFLAGS"
					export LDFLAGS="-L${CXR_LIB_DIR}/lzo/${HOSTTYPE}"
					export LIBS="-llzo2"
					export LD_LIBRARY_PATH="${CXR_LIB_DIR}/lzo/${HOSTTYPE}:${LD_LIBRARY_PATH}"
					
					# We need to use the libdir of lzop
					libdir=${CXR_LIB_DIR}/lzo/${HOSTTYPE}
					
					./configure --prefix=${CXR_BIN_DIR} \
					            --exec-prefix=${CXR_BIN_DIR} \
					            --bindir=${CXR_BIN_DIR} \
					            --sbindir=${CXR_BIN_DIR} \
					            --program-suffix=-${HOSTTYPE}${suffix} 
					            --enable-shared=no \
					            --enable-static=yes \
					            --mandir=/dev/null | tee -a $logfile

				fi
				
				# Clean up whatever there was
				echo "make clean DESTINATION=${CXR_BIN_DIR} LIBDIR=${libdir} SUFFIX=${suffix}" | tee -a $logfile
				make clean DESTINATION="${CXR_BIN_DIR}" LIBDIR=${libdir} SUFFIX="${suffix}" | tee -a $logfile
				
				# Make it!
				echo "make DESTINATION=${CXR_BIN_DIR} LIBDIR=${libdir} SUFFIX=${suffix}" | tee -a $logfile
				make DESTINATION="${CXR_BIN_DIR}" LIBDIR=${libdir} SUFFIX="${suffix}" | tee -a $logfile 
			
				if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
				then
					main.dieGracefully "The compilation of $executable did not complete successfully"
				fi
			
				# make install when compiling proj or lzo(p)
				if [[ $executable == proj || $executable == lzo || $executable == lzop ]]
				then
					make install | tee -a $logfile 
					
					if [[ $(common.array.allElementsZero? "${PIPESTATUS[@]}") == false ]]
					then
						main.dieGracefully "The installation of $executable did not complete successfully"
					fi
					
				fi
			fi
			
		done # Loop through directories containing stuff to install
		
		cd $CXR_RUN_DIR || return $CXR_RET_ERROR
		
		main.log -a  "Essential executables compiled."

	
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