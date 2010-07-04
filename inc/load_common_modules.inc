	main.log -H "load_common_modules" "Loading external modules from ${CXR_COMMON_INPUT_DIR}..." 
	
	# Increase global indent level
	main.increaseLogIndent
	
	main.log "load_common_modules" "Get more information about the modules using verbose output (-v)" 
	
	# Find all external functions.
	# 3 levels:
	# - General
	# - Model specific
	# - Version specific
	
	# If a module is in CXR_NOLOAD it will not be loaded

	for CURRENT_DIR in $CXR_COMMON_INPUT_DIR $CXR_COMMON_MODEL_INPUT_DIR $CXR_COMMON_VERSION_INPUT_DIR
	do
	
		main.log "load_common_modules" "Reading modules from ${CURRENT_DIR} (if there are any)..."
		
		for FUNCTION_FILE in $(ls ${CURRENT_DIR}/*.sh 2>/dev/null)
		do
			
			# Create the "bare" name (analogous to extract_module_name, but hen&egg)
			BASE_FUNCTION_NAME=$(basename $FUNCTION_FILE)
			RAW_FUNCTION_NAME=${BASE_FUNCTION_NAME%.sh}

			if [[ "$RAW_FUNCTION_NAME" != "${CXR_NOLOAD:-}"  ]]
			then
				# Load it
				
				CXR_META_MODULE_NAME=$RAW_FUNCTION_NAME
				
				source $FUNCTION_FILE
			else
				main.log "load_common_modules" "\nModule ${CXR_NOLOAD} seems to be running already"
			fi
		done
		
	done
	
	# Decrease global indent level
	main.decreaseLogIndent
	
	main.log -H "load_common_modules" "Modules loaded." 
