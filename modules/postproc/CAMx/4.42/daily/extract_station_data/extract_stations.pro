pro extract_stations,input_file,output_dir,mmout_file,day,month,year,extract_species,chemistry_mechanism,aerosol_mechanism,write_header,interpolation_method,file_format,output_number_format,is_latlon,x_dim,y_dim,num_levels,stations,do_normalization,normalize_temperature_k,normalize_pressure_pa,is_master_domain
;
; Function: extract_stations
;
;*******************************************************************************************************
;            IDL Program to extract data from a camx average file 
;
;            based on a script by Sebnem Andreani-Aksoyoglu, 11.1.2007
;
;            Rewritten by Daniel Oderbolz (daniel.oderbolz@psi.ch)
;            Incorporates code by Iakovos Barmpadimos (coordinate transfromation, data extraction from MM5)	
;
;            $Id$
;
;            This script is called automatically by the output processor
;            <extract_station_data> - *hesitate to change the calling interface*!
;
; Dependencies:
; - needs Hannes Kellers rv3
;
; External Files:
; - header_parser__define.pro - Defines a Parser for Headers of ASCII CAMx files
; - define_species - defines all species and compound species based on aerosol_mechanism and chemistry_mechanism
;
; Assumptions:
; - we extract data only for the lowest model layer
;
; Parameters:
;
; input_file - input file to operate on
; output_dir - directory where to store output (no trailing /)
; mmout_file - file of the mmout file to use for ground level pressure
; model_hour - the number of hours already modelled. Used as an offset for time calculations. The first dataset gets this time (model_hour+0)
; extract_day - Day to be extracted in YYYY-MM-DD Format
; extract_species - string array of species names to extract
; chemistry_mechanism - chemistry mechanism (CBMIV, CBM05 or SAPRC99)
; aerosol_mechanism - aerosol mechanism (currently, only CF is supported)
; write_header - boolean flag, if true, a header will be written
; interpolation_method - currently only NONE and BILINEAR are supported
; file_format - either 'CSV' or 'IDL'
; time_format - either 'HOUR' or an IDL date string
; output_number_format - The output format of the numbers.
; is_latlon - boolean flag, if true, coordinates are assumed to be geographic rather that grid indexes
; x_dim - the x dimension of the grid in grid cells of the grid in question  
; y_dim - the y dimension of the grid in grid cells of the grid in question
; num_levels - the number of levels of the grid in question
; stations - a 2D string array with [x,y,filename] in it (x,y may be integer or float grid indexes)
; do_normalization - boolean flag, if true, concentrations are normalized
; gasses_unit - Unit in which gasses are represented, suports PPM (Default), PPB, MUGCM (microgram/m**3)
; normalize_temperature_k - Temperature in K for normalisation 
; normalize_pressure_pa - Pressure in Pa for normalisation 
; is_master_domain - 1 for the master domain, 0 otherwise. For domains other than master, we need to shift the coordinates.
;
;>            idl No            Species                    unit
;>              0               NO                         (ppb)
;>              1               NO2                        (ppb)
;>              2               O3                         (ppb)
;>              3               TOL                        (ppb)
;>              4               XYL                        (ppb)
;>              5               FORM                       (ppb)
;>              6               PAN                        (ppb)
;>              7               CO                         (ppb)
;>              8               HONO                       (ppb)
;>              9               HNO3                       (ppb)
;>             10               H2O2                       (ppb)
;>             11               ISOP                       (ppb)
;>             12               PNA                        (ppb)
;>             13               SO2                        (ppb)
;>             14               NH3                        (ppb)
;>             15               PH2O                       (microg/m3)
;>             16               PNO3                       (microg/m3)
;>             17               PSO4                       (microg/m3)
;>             18               PNH4                       (microg/m3)
;>             19               POA                        (microg/m3)
;>             20               PEC                        (microg/m3)
;>             21               SOA1                       (microg/m3)
;>             22               SOA2                       (microg/m3)
;>             23               SOA3                       (microg/m3)
;>             24               SOA4                       (microg/m3)
;>             25               SOA5                       (microg/m3)
;>             26               SOA6                       (microg/m3)
;>             27               SOA7                       (microg/m3)
;>             28               SOPA                       (microg/m3)
;>             29               SOPB                       (microg/m3)
;>
;
;*******************************************************************************************************
; Who   When        What
; dco   22.07.2009  Added more flexibility towards chemical mechanisms, read critical data from file header
; dco   27.07.2009  Included ability to convert from lat/lon to grid cells.
;*******************************************************************************************************
; TODO:
; Check the update times in the file
; Add CXR_STATION_Z to extract not only level 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compiler Pragmas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We want 32 Bit integers and disable () array indexing
COMPILE_OPT IDL2

my_revision_string='$Id$'
my_revision_arr=strsplit(my_revision_string,' ',/EXTRACT)

print,my_revision_arr[1] + ' has revision ' + my_revision_arr[2]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

num_extract_species = n_elements(species)

; stations are multidimensional
s = size(stations,/DIMENSIONS)
num_stations = s[1]

if ( num_extract_species EQ 0) then message,"Must get more than 0 species to extract!"
if ( num_stations EQ 0) then message,"Must get more than 0 stations to extract!"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set Defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; is number_format set?

if n_Elements(input_number_format) EQ 0 then input_number_format='(5e14.7)'

; Standard conditions (according to EU legislation)
T0 = 293 ; K
p0 = 101300 ; Pa


if n_Elements(normalize_temperature_k) EQ 0 then normalize_temperature_k=T0
if n_Elements(normalize_pressure_pa) EQ 0 then normalize_pressure_pa=p0

;;;; SETUP constants

R = 8.314472 ; J K-1 mol-1 (CODATA)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Init Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load header Parser
hp=obj_new('header_parser',input_file)

; Get number of species in average file
scalars=hp->get_scalars()
num_input_species=scalars->get('nspec')

if ( num_extract_species GT num_input_species ) then message,'Cannot extract more species than available in the file!'

; Get list of species in average file
species=hp->get_species()

; x, y, z, species, hours
; This array has the same dimensions as the average file
c=fltArr(x_dim,y_dim,num_levels,num_input_species,24)

; This is one slice of the output file (one specific hour and layer)
c1=fltArr(x_dim,y_dim)

; This array contains the output
c_output=fltArr(num_stations,num_extract_species,24)

; time
t=fltArr(24)
;

;Station information
; We must read the first 2 fields and conver to float afterwards
station_pos_str=stations[0:1,*]
station_pos=float(station_pos_str)

; Names of the station files (read from stations array)
; Add output dir (saves space in call)
station_files=output_dir + '/' + stations[2,*]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Load external data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;z, species, hours, station
z=fltArr(num_levels,num_input_species,24,num_stations)

; the time offset (formerly t(i)) is the number of hours already simulated 
offset=model_hour

print,'Loading species information from define_species.pro'
@define_species
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Do coordinate transformation of the station coordinates if needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Before this block, station_pos contains station,lat,lon
; After it, it contains station,col,row tuples
if (is_latlon) then
	; Sanity check not included!
	
	; Offset to turn MM5 coordinates into CAMx ones (subtract from MM5)
	; Equal to 2 because MM5 starts one outer grid cell earlier, but
	; 1 of the 3 cells is used up by the buffer cell
	; THIS IS ONLY USEFUL FOR NON-MASTER GRIDS!!
	
	if (is_master_domain EQ 0) then begin
		MM5Offset = 2
	endif else begin
		MM5Offset = 0
	endelse

	cmd = 'rm -f LONGICRS LONGICRS.hdr LATITCRS LATITCRS.hdr'
	spawn, cmd
	
	; Read lon-lat data from MM5
	rv3, mmout_file, 'longicrs', lon, bhi, bhr
	rv3, mmout_file, 'latitcrs', lat, bhi, bhr
	
	; Interchange the first two dimensions
	; [y,x,z,time] -> [x,y,z,time]
	lon = TRANSPOSE(lon, [1, 0, 2, 3])
	lat = TRANSPOSE(lat, [1, 0, 2, 3])
	
	; Loop through stations
	FOR l = 0, num_stations-1 DO BEGIN
	
		; The lat-lon values of the selected station are assigned to a variable
		station_lon = FLOAT(station_pos[0,l])
		station_lat = FLOAT(station_pos[1,l])
		
		; Calculate the squared distance of each grid point from
		; the point of interest
		lon_dif = lon[*, *, 0, 1] - station_lon
		lat_dif = lat[*, *, 0, 1] - station_lat
		sq_dist = lon_dif[*,*]^2 + lat_dif[*,*]^2
		
		; Find the grid point where the squared distance is minimised
		; We accept a non-integer value
		index = WHERE(sq_dist EQ MIN(sq_dist))
		hor_dim = N_ELEMENTS(lon[*,1,0,1])
		
		if ( interpolation_method EQ 'NONE' ) then
		begin
			; If we do not interpolate, return integer index
			col = ( index / hor_dim ) - MM5Offset
			row = ( index MOD hor_dim ) - MM5Offset
		endif else
			; Return a "decimal index"
			col = FLOAT( index ) / hor_dim  - MM5Offset
			row =      ( index MOD hor_dim ) - MM5Offset
		endelse
		
		; If the correction leads to negative values, warn
		IF (col LT 0) THEN BEGIN
		  print,'WARNING: CAMx correction leads to negative coordinate for station ' + station
		  col = 0
		ENDIF
		
		IF (row LT 0) THEN BEGIN
		  print,'WARNING: CAMx correction leads to negative coordinate for station ' + station
		  row = 0
		ENDIF
		
		; Set new values
		station_pos[0, l] = col
		station_pos[1, l] = row
	
	ENDFOR
	
	cmd = 'rm -f LONGICRS LONGICRS.hdr LATITCRS LATITCRS.hdr'
	spawn, cmd
endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Begin Pressure/Temp preparation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if ( do_normalization OR gasses_unit EQ 'MUGCM' ) then begin

	print,'Preparing Meteo data to obtain Temperature and Pressure data...'

	pressure = FLTARR(151,106,31,49)
	
	; Pressure at MM5 Top in Pa
	ptop = 10000
	
	; Some spawn command.
	cmd = 'rm -f SIGMAH SIGMAH.hdr PSTARCRS PSTARCRS.hdr PP PP.hdr T T.hdr'
	spawn, cmd
	
	rv3, mmout_file, 'sigmah', sigmah, bhi, bhr
	rv3, mmout_file, 'pstarcrs', pstarcrs, bhi_pstarcrs, bhr_pstarcrs
	rv3, mmout_file, 'pp', pp, bhi_pp, bhr_pp
	rv3, mmout_file, 't', t, bhi_t, bhr_t
	
	; Some spawn command.
	cmd = 'rm -f SIGMAH SIGMAH.hdr PSTARCRS PSTARCRS.hdr PP PP.hdr T T.hdr'
	spawn, cmd
	
	pp = TRANSPOSE(pp, [1, 0, 2, 3])
	pstarcrs = TRANSPOSE(pstarcrs, [1, 0, 2, 3])
	t = TRANSPOSE(t, [1, 0, 2, 3])
	
	; Estimation of pressure
	FOR time = 1, 24 DO BEGIN
	  FOR level = 0, 30 DO BEGIN
	    pressure[*,*,level,time] = sigmah[level,0,0,time] * pstarcrs[*,*,0,time] + ptop + pp[*,*,level,time]
	  ENDFOR
	ENDFOR
	
	t_ts = FLTARR(24)
	p_ts = FLTARR(24)
	sigmah_prof = FLTARR(31)
	sigmah_prof[0:30] = sigmah[*, 0, 0, 1]
	
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Read input data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Open the input
openr,input_lun,input_file, /GET_LUN


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Skip Header
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; We skip 4+nspec lines (Header was read elsewhere)
	skip_lun, input_lun,4 + num_input_species, /LINES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Read dataset - array c in the end contains all data from the file
; We read all we have.
; For each update time, we have this structure(See CAMx Manual):
; ibdate,btime,iedate,etime
; Loop from 1 to nspec species:
; Loop from 1 to nz layers:
; ione,mspec(l),((conc(i,j,k,l),i=1,nx),j=1,ny)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; loop for time
for hour=0L,23 do begin
	; Skip the ibdate,btime,iedate,etime line
	skip_lun, input_lun,1, /LINES

	;loop for species
	for ispec=0L,num_input_species-1 do begin
	
		;do loop for layers
		for level=0L,num_levels-1 do begin
		
			; Really interesting for us here is only layer 0.
			; still, we read all layers
			; (later we might add code to extract individual layers on a per-station basis)
		
			; Skip the ione, mspec(l) line
			skip_lun, input_lun,1, /LINES
			
			; read in a whole array of concentrations
			; Earlier, we specified a format, we let IDL decide
			readf,input_lun,c1
			
			; add it to the "total" array c
			c[0,0,level,ispec,hour]=c1
			
			endfor ; stations
				
		endfor ; levels
	
	endfor ; species

endfor ; time

; Close Input
close,input_lun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Prepare Output structure (Interpolation, selection of relevant parts...)
; We transfor the original array c (x,y,z,input_species,time) into c_output (stations, output_species, time)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; loop through the stations
for station=0L,num_stations-1 do begin

	; loop through the output species
	for current_output_species=0L,num_extract_species-1 do begin
	
		

		; Select the appropriate point and interpolate if needed
		; The z-ccordiantes are set to 0 implicitly
		
		if ( interpolation_method EQ 'BILINEAR' ) then begin
			c_output[station,ispec,*]=bilinear(c,station_pos[0,station],station_pos[1,station],0,*)
		endif else
			c_output[station,ispec,*]=c[station_pos[0,station],station_pos[1,station],0,*,*]
		endelse
		
		; Some preliminary calculations
		if ( do_normalization OR gasses_unit EQ 'MUGCM' ) then begin
			; Where do we look (in the MM5 world)
			col = station_pos[0,station] + mm5coord_offset
			row = station_pos[1,station] + mm5coord_offset
			
			;print,'Extracting meteo data for file ' + station_files(station) + ' at MM5 col ' + string(col) + ' row ' + string(row)
			
			; Linear in sigma extrapolation of pressure in order to obtain a surface value
			p_colts = pressure[col,row,*,1:24]
			p_colts = REFORM(p_colts)
			
			t_colts = t[col,row,*,1:24]
			t_colts = REFORM(t_colts)
			
			; Determine pressure at station for all times by extrapolation
			p = p_colts[29,*] + ((1 - sigmah_prof[29]) * (p_colts[30,*] - p_colts[29,i]) / (sigmah_prof[30] - sigmah_prof[29]) )
			
			; Determine temperature at station for all times by extrapolation
			Temp = t_colts[29,*] + ((1 - sigmah_prof[29]) * (t_colts[30,*] - t_colts[29,*]) / (sigmah_prof[30] - sigmah_prof[29]) )
			
			; There are arrays (dimension time)
			V_n = ( R * Temp ) / p
			V_0 = ( R * T0 ) / p0
			
			f_n = V_n / V_0
		endif
		
		; If it is a gas, convert
		if (gasses->iscontained(current_species)) then
		begin
		
			case gasses_unit of
			
				PPM:	BEGIN
							; CAMx provides PPM by default
						END
						
				PPB:	BEGIN
							; Simple conversion
							c_output[station,*,*] = c_output[station,*,*] * 1000
						END
				
				MUGCM:	BEGIN
				
							; A bit harder - we need the molecular weight. it is contained in the gasses hashtable
							; I currently don't see how to get rid of the loop
							for ispec=0L,num_species-1 do begin
		
								current_species=strupcase(extract_species[ispec])
	
								; microg/m3 = 1000 * ppm * (M/V_n) * f_n where M is the Molar weight [g/mole] of the species
								; and V_n is the molar volume (the volume of one mole) [L/mole], 
								; f_n is a correction factor to translate the concontration to to norm conditions of 0 deg C and 1013 hPa
								;
								; V_n = ( R * T ) / p where R = 8.314472 J K-1 mol-1 (CODATA)
								
								c_output[station,ispec,*] = c_output[station,ispec,*] * 1000 * ( gasses->get(current_species) / V_n[*] )
								
							endfor
							
						END
			endcase
		endif
		
		; if needed, normalize
		if (do_normalization) then begin
			c_output[station,*,*] = c_output[station,*,*] * f_n[*]
		endif
	endfor
endfor
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Conversion to string, correction of the time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Create a String version of the output array
; How can we influcence rounding?
c_output_strarr = StrTrim(c_output,2)

for hour=0L,23 do begin

	case time_format of
	
			HOUR: begin
						; Use the given model_hour plus offset
							c_output_strarr[*,*,hour] = strtrim(model_hour + hour,2)
						end
						
			ELSE: begin
			
							message,"Please use only HOUR time_format for time format, custom formats are not yet implemented."
			
							; Use the given string
							year = Fix(StrMid(StrTrim(extract_day,2), 0, 4))
							month = Fix(StrMid(StrTrim(extract_day,2), 5, 2))
							day = Fix(StrMid(StrTrim(extract_day,2), 8, 2))
							
							julstart=JULDAY(month,day,year)
							; Just add hour/24 to the julian start date to get the current date
							current = julstart + (FLOAT(hour)/24)
							
							; Now print
							PRINT, date_time, format = '(C(CMOI2.2, "/", CDI2.2, "/", CYI))'
						end
	
	endcase

endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Write Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; Here, c_output_strarr is a 3D String Array with stations, species, time as dimensions.
	; We select one slice (one station) and print this structure to the relevant output file
	; Depending on the output format (IDL or CSV), different approaches are taken.

	; loop through the NABEL stations
	for station=0L,num_stations-1 do begin
	
		; Create correct output format
		case file_format of
		
			IDL: 	begin
			
							; Open File
							openw,current_output_lun,station_files[i], /GET_LUN
							
							; If needed, print header
							if (write_header) then begin
							
							endif
			
							printf,current_output_lun,c_output_strarr[station,*,*]
						end
			
			CSV:	Begin
							; Create CSV according to Fanning
							s = size(c_output_strarr,/Dimensions)
							xsize = s[0]
							linewidth=1600
							delimiter=","
							
							; Open File (respect linewidth!)
							openw,current_output_lun,station_files[i], /GET_LUN, Width=linewidth
							
							; If needed, print header
							if (write_header) then begin
							
							endif
							
							; Add delimiters to all elements except the last column
							c_output_strarr[station,0:xsize-2,*] = c_output_strarr[station,0:xsize-2,*] + delimiter
							
							; Print it
							printf,current_output_lun,c_output_strarr[station,*,*]
						end
		
		endcase
		
		; free lun and close the file (implicit)
		free_lun, current_output_lun
		
		
	endfor ; stations

	return
end
