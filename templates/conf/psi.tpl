
# $Id$



CXR_STATION_LABEL=NABEL

CXR_AQMFAD_OUTPUT_DIR=${CXR_OUTPUT_DIR}/aqmfad
CXR_STATION_OUTPUT_DIR=${CXR_OUTPUT_DIR}/Nabel

################## NABEL (Station dependent)#####################################

# The local variable $station will be replaced by the actual name of the station in question
CXR_STATION_FILE_RULE='${CXR_STATION_OUTPUT_DIR}/${CXR_RUN}.${CXR_YEAR_S}${CXR_MONTH}${CXR_DAY}_${station}.dat'

# This is the name of the cumulated file
CXR_CUMULATIVE_STATION_FILE_RULE='${CXR_STATION_OUTPUT_DIR}/${CXR_RUN}.${CXR_YEAR_S}${CXR_MONTH}_${station}-complete.dat'


# Aqmfad options
# This is treated like an array
#CXR_RUN_AQMFAD_ON_GRID="1 2 3"
CXR_RUN_AQMFAD_ON_GRID="3"

# the same for emifad
CXR_RUN_EMIFAD_ON_GRID="1 2 3"

################################################################################
# Options for extraction of NABEL station data
################################################################################

# We will write a small .pro file with this and
# load it using the @-notation

# The Coordinates are expressed in terms of grid cells
# of the 3rd Grid and needs to be changed if the grid definition changes

# Entry 0 no dummy here!

CXR_STATION[0]=basel
CXR_STATION_X[0]=44.62
CXR_STATION_Y[0]=73.26

CXR_STATION[1]=bern
CXR_STATION_X[1]=42.15
CXR_STATION_Y[1]=51.21

CXR_STATION[2]=chaumont
CXR_STATION_X[2]=30.34
CXR_STATION_Y[2]=54.34

CXR_STATION[3]=davos
CXR_STATION_X[3]=103.53
CXR_STATION_Y[3]=50.23

CXR_STATION[4]=duebendorf
CXR_STATION_X[4]=70.68
CXR_STATION_Y[4]=69.65

CXR_STATION[5]=haerkingen
CXR_STATION_X[5]=51.02
CXR_STATION_Y[5]=65.10

CXR_STATION[6]=jungfrau
CXR_STATION_X[6]=56.76
CXR_STATION_Y[6]=37.05

CXR_STATION[7]=laegern
CXR_STATION_X[7]=64.28
CXR_STATION_Y[7]=72.05

CXR_STATION[8]=lausanne
CXR_STATION_X[8]=22.55
CXR_STATION_Y[8]=34.43

CXR_STATION[9]=lugano
CXR_STATION_X[9]=82.88
CXR_STATION_Y[9]=18.78

CXR_STATION[10]=magadino
CXR_STATION_X[10]=81.90
CXR_STATION_Y[10]=24.27

CXR_STATION[11]=payerne
CXR_STATION_X[11]=29.85
CXR_STATION_Y[11]=45.54

CXR_STATION[12]=rigi
CXR_STATION_X[12]=67.73
CXR_STATION_Y[12]=57.02

CXR_STATION[13]=sion
CXR_STATION_X[13]=41.00
CXR_STATION_Y[13]=24.05

CXR_STATION[14]=taenikon
CXR_STATION_X[14]=77.78
CXR_STATION_Y[14]=72.99

CXR_STATION[15]=zurich
CXR_STATION_X[15]=68.66
CXR_STATION_Y[15]=68.52

# We count the number (do not change unless you know why)
CXR_NUMBER_OF_STATIONS=${#CXR_STATION[@]}

