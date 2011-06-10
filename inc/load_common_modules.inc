	main.log -H "Loading external modules from ${CXR_COMMON_INPUT_DIR}..." 
	
	# Increase global indent level
	main.increaseLogIndent
	
	main.log "Get more information about the modules using verbose output (-v)" 
	
	# Find all external functions.
	# 3 levels:
	# - General
	# - Model specific
	# - Version specific

	for CURRENT_DIR in $CXR_COMMON_INPUT_DIR $CXR_COMMON_MODEL_INPUT_DIR $CXR_COMMON_VERSION_INPUT_DIR
	do
	
		main.log "Reading modules from ${CURRENT_DIR} (if there are any)..."
		
		for FUNCTION_FILE in $(ls ${CURRENT_DIR}/*.sh 2>/dev/null) 
		do
			
			# Create the "bare" name
			BASE_FUNCTION_NAME=$(basename $FUNCTION_FILE)
			RAW_FUNCTION_NAME=${BASE_FUNCTION_NAME%.sh}

			# CAMxRunner needs this variable set
			CXR_META_MODULE_NAME=$RAW_FUNCTION_NAME
			
			source $FUNCTION_FILE
			
			if [[ $(main.CheckModuleRequirements $CXR_META_MODULE_NAME \
																					 $CXR_META_MODULE_VERSION \
																					 $CXR_META_MODULE_DESCRIPTION \
																					 $CXR_META_MODULE_DOC_URL \
																					 $CXR_META_MODULE_AUTHOR \
																					 $CXR_META_MODULE_NUM_TESTS \
																					 $CXR_META_MODULE_REQ_SPECIAL) == false ]]
			then
				main.dieGracefully "Requirements for core module $CXR_META_MODULE_NAME are not met!"
			fi
			
		done
		
	done
	
	# Decrease global indent level
	main.decreaseLogIndent
	
	main.log -H "Modules loaded." 
