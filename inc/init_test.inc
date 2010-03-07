#
# This file contains code needed to load a test.
# 
# Leave the next line intact, it is used for change detection
# Version: $Id$ 
#
# Written by Daniel C. Oderbolz (CAMxRunner@psi.ch),
# Released under the Creative Commons "Attribution-Share Alike 2.5 Switzerland"
# License, (http://creativecommons.org/licenses/by-sa/2.5/ch/deed.en)
################################################################################

CXR_RUN_DIR="$(pwd)"

# Source the stuff we need to produce TAP messages (test anything protocol)
source $CXR_RUN_DIR/inc/tap-functions.inc

# Source the functions that are central
source $CXR_RUN_DIR/inc/main_functions.inc

# We need to know our version prior to loading modules
main_setModelAndVersion ${CXR_RUN}

# Source Defaults if needed
if [[ "${CXR_DEFAULTS_LOADED:-false}" != true  ]]
then
	source $CXR_RUN_DIR/inc/defaults.inc
fi

# For the time being, we turn off errexit
# Because tap-functions uses non-0 returns
set +e

# Correct with user-supplied stuff
# Turn any CXR_USER_TEMP_ into a CXR_ variable
for VAR in $(set | sort | grep ^CXR_USER_TEMP.*= )
do
	# Now Var contains a VAR=Value string
	NAME="$(echo "${VAR}" | cut -d= -f1)"
	# Rewrite name
	NEW_NAME="${NAME//CXR_USER_TEMP/CXR}"
	VALUE="$(echo "${VAR}" | cut -d= -f2)"

	# Set rewritten variable
	export ${NEW_NAME}="$VALUE"
done

# Load common modules 
source ${CXR_RUN_DIR}/inc/load_common_modules.inc

# Determine our name
CXR_META_MODULE_NAME=$(main_getModuleName $0)

#Disable state DB
CXR_ENABLE_STATE_DB=false

# No Notification etc.
CXR_HOLLOW=true

# load configuration data.
main_readConfig "${CXR_RUN}" "${CXR_MODEL_VERSION}" "${CXR_MODEL}" "."

# count simulation days
CXR_NUMBER_OF_SIM_DAYS=$(cxr_common_days_between "$CXR_START_DATE" "$CXR_STOP_DATE")

# Let's just initialise the date variables for day 0
date_setVars "$CXR_START_DATE" "0"

# Init state DB
cxr_common_initialize_state_db

