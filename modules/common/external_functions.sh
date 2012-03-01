# Processing modules are not meant to be executed stand-alone, so there is no
# she-bang and the permission "x" is not set.
#
# Common script for the CAMxRunner 
# See http://people.web.psi.ch/oderbolz/CAMxRunner
#
# Version: $Id$ 
#
# Title: Functions to prepare external runs on large HPC systems
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch).
# This software is provided as is without any warranty whatsoever. See doc/Disclaimer.txt for details. See doc/Disclaimer.txt for details.
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################
# Module Metadata. Leave "-" if no setting is wanted
################################################################################


# Either "${CXR_TYPE_COMMON}", "${CXR_TYPE_PREPROCESS_ONCE}", "${CXR_TYPE_PREPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_DAILY}","${CXR_TYPE_POSTPROCESS_ONCE}", "${CXR_TYPE_MODEL}" or "${CXR_TYPE_INSTALLER}"
CXR_META_MODULE_TYPE="${CXR_TYPE_COMMON}"

# If >0, this module supports testing
CXR_META_MODULE_NUM_TESTS=0

# This string describes special requirements this module has
# it is a space-separated list of requirement|value[|optional] tuples.
# If a requirement is not binding, optional is added at the end
CXR_META_MODULE_REQ_SPECIAL=

# Add description of what it does (in "", use \n for newline)
CXR_META_MODULE_DESCRIPTION="Functions to prepare external runs on large HPC systems"

# URL where to find more information
CXR_META_MODULE_DOC_URL="http://people.web.psi.ch/oderbolz/CAMxRunner"

# Who wrote this module?
CXR_META_MODULE_AUTHOR="Daniel C. Oderbolz (2008 - 2011), CAMxRunner@psi.ch"

# Add license info if applicable (possibly with URL)
CXR_META_MODULE_LICENSE="Creative Commons Attribution-Share Alike 2.5 Switzerland (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)"

# Do not change this line, but make sure to run "svn propset svn:keywords "Id" FILENAME" on the current file
CXR_META_MODULE_VERSION='$Id$'


################################################################################
# Function: common.external.init
#
# Prepares a run for external use. Produces one script to transfer the input data
# and another one to control the server. Also creates all needed CAMx.in files.
# The control script uses CXR_EXTERNAL_TEMPLATE as a basis.
#
################################################################################
function common.external.init()
################################################################################
{
	local tempdir
	local iFile
	local suffix

	if [[ ! -e $CXR_EXTERNAL_TEMPLATE ]]
	then
		main.dieGracefully "Could not find ${CXR_EXTERNAL_TEMPLATE}!"
	fi
	
	# On an external machine it is possible that no run was ever started
	# Therefore we must initialize (force!)
	common.state.init
	common.state.updateInfo true
	
	main.log -a "Preparing external run on a HPC machine...\nErrors of the type *unbound variable* may happen and can be ignored."
	
	tmpdir=$(common.runner.createTempDir run-external false)
	
	main.log -a "######################################"
	main.log -a "You will find all files for the run in $tmpdir"
	main.log -a "######################################"
	
	ofile=$tmpdir/CAMx_job.sh
	ofilelist=$tmpdir/copy_input_files.sh
	
	# Add comment to the ofilelist
	echo "#!/bin/bash" > $ofilelist
	echo "#This script copies the input data for $CXR_RUN to the HPC system" >> $ofilelist
	echo "#It works perfectly if you have passwordless shh" >> $ofilelist
	echo "#<http://www.debian-administration.org/articles/152> set up" >> $ofilelist
	echo "#You need to run it from a directory containing links to all the files" >> $ofilelist
	echo "#to be copied." >> $ofilelist
	echo "#" >> $ofilelist
	
	# Change permissions
	touch $ofile $ofilelist
	chmod +x $ofile $ofilelist
	
	cd $tmpdir
	
	# Evaluate template
	while read line
	do
		common.user.showProgress
		
		newline=$(common.runner.evaluateRule "$line" true line false false) &> /dev/null
		
		# If there are errors we could not expand
		if [[ "$newline" ]]
		then
			echo "$newline" >> $ofile
		else
			echo "$line" >> $ofile
		fi
		
	done < $CXR_EXTERNAL_TEMPLATE
	
	# Now create all CAMx.in files
	# Source the model functions to get its functions
	
	module_path="$(common.module.getPath "model")"
	
	source $module_path
	
	# We must modify the rule for the CAMx.in files
	CXR_MODEL_CTRL_FILE_RULE='${tmpdir}/CAMx.${CXR_DATE_RAW}.in'
	
	for iOffset in $(seq 0 $(( ${CXR_NUMBER_OF_SIMULATION_DAYS} - 1 )) )
	do
		common.date.setVars "$CXR_START_DATE" "$iOffset"
		
		main.log -a "Processing ${CXR_DATE}..."
		
		# Call the relevant functions of the model module
		#             We do not want the rule evaluator to create any missing dirs
		set_variables false
		write_model_control_file
		
		iFile=1
		
		# Write out input files
		for InputFile in $CXR_CHECK_THESE_INPUT_FILES
		do
			if [[ $InputFile =~ $CXR_EXTERNAL_INPUT_FILE_LIST_PATTERN ]]
			then
				#Make sure colons are escaped
				escapedInputFile="${InputFile//:/\\:}"
				
				# Build the copy command. 
				echo "${CXR_EXTERNAL_COPY_COMMAND} \"$(basename $escapedInputFile)\" \"${CXR_EXTERNAL_REMOTE_USER}@${CXR_EXTERNAL_REMOTE_HOST}:${escapedInputFile}\" &" >> $ofilelist
			
				if [[ $(( ${iFile} % ${CXR_EXTERNAL_NUMBER_OF_CONNECTIONS} )) -eq 0 ]]
				then 
					# Every CXR_EXTERNAL_NUMBER_OF_CONNECTIONS'th command is "wait"
					# to wait for all previous subprocesses
					echo "wait " >> $ofilelist
				fi
			
				iFile=$(( $iFile + 1 ))
			fi
		done

	done
	
	main.log -a "Done. Your files are in $tmpdir"
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
	:
	
	########################################
	# Tests. If the number changes, change CXR_META_MODULE_NUM_TESTS
	########################################
	
	########################################
	# teardown tests if needed
	########################################
}
