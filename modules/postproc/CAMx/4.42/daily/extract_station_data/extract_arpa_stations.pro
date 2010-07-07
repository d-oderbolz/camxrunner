pro extract_arpa_stations,input_file,output_dir,write_header,day,month,year,x_dim,y_dim,num_levels,stations,temp_file,zp_file,format=fmt
	;
	; Function: extract_arpa_stations
	;
	;*******************************************************************************************************
	;						IDL Program to extract data from a camx average file from domain 3
	;						for ARPA stations. Modified version of extract_nabel_stations written for co5.
	;						THe list of species to be extracted is given by the project.
	;
	;						based on a script by Sebnem Andreani-Aksoyoglu, 11.1.2007
	;
	;						modified by Daniel Oderbolz (daniel.oderbolz@psi.ch)
	;
	;						$Id$
	;
	;						This script is called automatically by the output processor
	;						<extract_station_data> - *hesitate to change the calling interface*!
	;
	;						Originally, the code used \GET_LUN, but IDL can only open 29 files like this
	;						(http://idlastro.gsfc.nasa.gov/idl_html_help/Understanding_(LUNs).html) so we
	;						had to go for the less beautiful approach...
	;
	; Parameters:
	; input_file - input file to operate on
	; output_dir - directory where to store output (no trailing /)
	; write_header - boolean flag to indicate if a header is needed (usually for first day)
	; day - day to be extracted
	; month - month	to be extracted
	; year - year to be extracted	
	; x_dim - the x dimension of the grid in grid cells of the grid in question	
	; y_dim - the y dimension of the grid in grid cells of the grid in question
	; num_levels - the number of levels of the grid in question
	; stations - a 2D string array with [x,y,filename] in it (x,y may be integer or float grid indexes)
	; zp_file - pressure/height ASCII file
	; temp_file - temperature ASCII file
	; format - The format of the numbers. Normally specified as fmt='(9e14.9)', bin2asc writes (5e14.7)
	;
	; Output format (comma-separated):
	; dd : day (character 2 digit)
	; mm: month (character 2 digit)
	; yyyy year (character 4 digit)
	; hh: hour (character 2 digit)
	; PM10: hourly	concentration ( ?g/m?)	(REAL o INTEGER) 
	; NO2 : hourly concentration (?g/m?)	. (REAL o INTEGER)
	; NO : hourly concentration	(?g/m?) (REAL o INTEGER)
	; O3: hourly concentration	( ?g/m?)	(REAL o INTEGER)
	
	; Of course, the CXR_OUTPUT_SPECIES_NAMES must match this list here:
	
	;>						idl No						Species										output unit
	;>						0							 NO											(microg/m3)	
	;>						1							 NO2										(microg/m3)	
	;>						2							 O3											(microg/m3)	
	;>						3							 TOL										(microg/m3)	
	;>						4							 XYL										(microg/m3)	
	;>						5							 FORM										(microg/m3)	
	;>						6							 PAN										(microg/m3)	
	;>						7							 CO											(microg/m3)	
	;>						8							 HONO										(microg/m3)	
	;>						9							 HNO3										(microg/m3)	
	;>						10							 H2O2										(microg/m3)	
	;>						11							 ISOP										(microg/m3)	
	;>						12							 PNA										(microg/m3)	
	;>						13							 SO2										(microg/m3)	
	;>						14							 NH3										(microg/m3)	
	;>						15							 PH2O										(microg/m3)
	;>						16							 PNO3										(microg/m3)
	;>						17							 PSO4										(microg/m3)
	;>						18							 PNH4										(microg/m3)
	;>						19							 POA										(microg/m3)
	;>						20							 PEC										(microg/m3)
	;>						21							 SOA1										(microg/m3)
	;>						22							 SOA2										(microg/m3)
	;>						23							 SOA3										(microg/m3)
	;>						24							 SOA4										(microg/m3)
	;>						25							 SOA5										(microg/m3)
	;>						26							 SOA6										(microg/m3) 
	;>						27							 SOA7										(microg/m3) 
	;>						28							 SOPA										(microg/m3) 
	;>						29							 SOPB										(microg/m3) 
	;>						30							 NA											(microg/m3) 
	;>						31							 PCL										(microg/m3) 
	;>						32							 FPRM										(microg/m3) 
	;>						33							 FCRS										(microg/m3) 
	;>						34							 CPRM										(microg/m3) 
	;>						35							 CCRS										(microg/m3) 
	;>
	;>						The stations are loaded with an @ script which can be created by the CAMx-runner.sh
	;
	;*******************************************************************************************************
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; IDL Settings
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Show Math errors as they occur
	!EXCEPT=2
	COMPILE_OPT IDL2
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Version control
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	my_revision_string='$Id$'
	my_revision_arr=strsplit(my_revision_string,' ',/EXTRACT)
	
	print,my_revision_arr[1] + ' has revision ' + my_revision_arr[2]
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Init Header Parser
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	; Load header Parser
	hp=obj_new('header_parser',input_file)
	
	; Get number of species in average file
	scalars=hp->get_scalars()
	num_input_species=scalars->get('nspec')
	head_length = hp->get_header_length()
	
	; Get list of species in average file
	species=hp->get_species()

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Check settings
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	; stations are multidimensional
	s = size(stations,/DIMENSIONS)
	num_stations = s[1]
	
	if ( num_stations EQ 0) then message,"Must get more than 0 stations to extract!"
	
	;; We cannot open more than 99 files!
	IF (num_stations GT 99) THEN MESSAGE,'IDL cannot open more than 99 files...'
	
	; Set format if needed
	if n_Elements(fmt) EQ 0 then fmt='(9e14.9)'
	
	;;;; SETUP some constants
	
	; Define header vector
	columnHeaders = ['dd','mm','yyyy','hh','PM10','NO2','NO','O3']
	xsize = N_Elements(columnHeaders)
	
	R = 8.314472 ; J K-1 mol-1 (CODATA)
	
	; Convert mbar to PA
	mb2pa = 1E2
	
	; Standard conditions (according to EU legislation)
	T0 = 293 ; K
	p0 = 101300 ; Pa
	
	;	Define Molecular weights in kg/mol
	M_NO = 0.030
	M_NO2 = 0.046
	M_O3 = 0.048
	M_TOL = 0.0921
	M_XYL = 0.1062
	M_FORM = 0.030
	M_PAN = 0.121
	M_CO = 0.028
	M_HONO = 0.047
	M_HNO3 = 0.063
	M_H2O2 = 0.034
	M_ISOP = 0.0681
	M_PNA = 0.079
	M_SO2 = 0.064
	M_NH3 = 0.017
	
	;;;;;;;;;;;;;;;;;;;; Read Pressure and Temperature data
	
	; We create 2 arrays
	; pressure[col,row,hour]
	; t[col,row,hour]
	
	; The Target arrays
	pressure=fltarr(x_dim, y_dim, 24)
	t=fltarr(x_dim, y_dim, 24)
	
	; we need the next arrays becase we need to interpolate to the ground
	total_pressure=fltarr(x_dim, y_dim, num_levels)
	total_temperature=fltarr(x_dim, y_dim, num_levels)
	total_height=fltarr(x_dim, y_dim, num_levels)
	
	; to read, we need the slices
	pressure_slice=fltarr(x_dim, y_dim)
	height_slice=fltarr(x_dim, y_dim)
	
	; Open the input
	openr,input_t,temp_file, /GET_LUN
	openr,input_zp,zp_file, /GET_LUN
	
	;do loop for time
	for iHour=0L,23 do begin
	
		; temperature comes in levels 
		; Skip header of T file
		skip_lun,input_t,1
		
		; read one slice
		readf,input_t,total_temperature
	
		;do loop for layers (height & temp file)
		for iver=0L,num_levels-1 do begin

			; skip one line (timestamp)
			skip_lun,input_zp,1
			
			; height data is first in the ZP file
			readf,input_zp,height_slice
			
			; skip another line (timestamp) of the pressure file
			skip_lun,input_zp,1
			
			; Height data is second
			readf,input_zp,pressure_slice
			
			; Fill the "Total" Arrays
			total_height[0,0,iver] = height_slice
			total_pressure[0,0,iver] = pressure_slice

		endfor ; layer
		
		print,total_height
		print,total_pressure
		print,total_temperature

		; For the vertical interpolation, we use 1D Interpolation
		; Therefore, we need to loop (is there a better way??)
		for iCol = 0, x_dim - 1 do begin
			for jRow = 0, y_dim - 1 do begin
				; Interpolate ground pressure and temperature
				; Both temperature and pressure correspond to the hights
				pressure[iCol,jRow,iHour] = interpol(total_pressure[iCol,jRow,*],total_height[iCol,jRow,*],1)
				;                                                                                          |
				;                                                                                       We want the value at h=0
				
				; Lets do the same for temperature
				t[iCol,jRow,iHour] = interpol(total_temperature[iCol,jRow,*],total_height[iCol,jRow,*],1)
				;                                                                                      |
				;                                                                                   We want the value at h=0
			endfor ; rows
		endfor ; columns
		
	endfor ; time
	
	; Close files
	free_lun,input_t
	free_lun,input_zp
	
	; Convert pressure in mb to Pascal
	pressure = pressure * mb2pa
	
	;;;;;;;;;;;;;;;;;;;; End pressure/temp preparation
	
	
	; x, y
	conc_slice=fltArr(x_dim,y_dim)
	
	;Station information
	; We must read the first 2 fields and conver to float afterwards
	station_pos_str=stations[0:1,*]
	station_pos=float(station_pos_str)
	
	; Names of the station files (read from stations array)
	; Add output dir (saves space in call)
	station_files=output_dir + '/' + stations[2,*]
	
	; Logical Unit Numbers of theses files (not assigned by IDL - else we could only open 29 files!)
	; We generate 0..n-1 and then add 1 to each element (Luns go 1..99)
	station_luns=INDGEN(num_stations) + 1
	
	;z, species, hours, station
	z=fltArr(num_levels,num_input_species,24,num_stations)
	
	; Open the input
	openr,input_lun,input_file, /GET_LUN
	
	; Open all the output files
	; and store the luns
	for i=0L,num_stations-1 do begin

		; We can no longer get the lun...
		current_output_lun = i + 1
		openw,current_output_lun,station_files[i], width=2400
		station_luns[i]=current_output_lun
		
		; If we must, write the header
		if (write_header and N_Elements(columnHeaders) NE 0) then begin
			print,'Writing Header...'
			
				; This is Fanning code: http://www.dfanning.com/tip_examples/write_csv_data.pro
				; Make sure these are strings.
				sColumns = StrTrim(columnHeaders, 2)
	
				; Add a comma to each value except the last one.
				sColumns[0:xsize-2] = sColumns[0:xsize-2] + ','
	
				; Write the headers to the file.
				PrintF, station_luns[i], sColumns
		endif

	endfor
	
	;skip informational data and species
	
	
	skip_lun, input_lun,head_length, /LINES
	;
	;do loop for time
	;
	for i=0L,23 do begin
	
		skip_lun, input_lun,1, /LINES
		;
		;do loop for species
		;
		for ispec=0L,num_input_species-1 do begin
		
			;do loop for layers
			for iver=0L,num_levels-1 do begin
				
				skip_lun, input_lun,1, /LINES 
				
				; We let IDL work out the format
				readf,input_lun,conc_slice,format=fmt
					
				; loop through the stations and do the interpolation
				for station=0L,num_stations-1 do begin
				
					;ix=44.62
					;jy=73.26
					;z1(iver,ispec,i)=bilinear(conc_slice,ix,jy)
					; bilinear allows us to retrieve decimal indices
					z[iver,ispec,i,station]=bilinear(conc_slice,station_pos[0,station],station_pos[1,station])
					
				endfor ; stations
			endfor ; layers
		endfor ; species
		
		; loop through the stations
		for station=0L,num_stations-1 do begin
		
			; All gasses need to be converted to microg/m3 (from ppm)
			; using this approach:
			; microg/m3 = 1000 * ppm * (M/V_n) * f_n where M is the Molar weight [g/mole] of the species
			; and V_n is the molar volume (the volume of one mole) [L/mole], 
			; f_n is a correction factor to translate the concontration to to norm conditions of 0 deg C and 1013 hPa
			;
			; V_n = ( R * T ) / p where R = 8.314472 J K-1 mol-1 (CODATA)
			;
			; We do it for all gasses, even if we need only 3 of them for future reference
			; Aerosols are corrected for norm concentrations
			
			; Where do we look
			col = station_pos[0,station]
			row = station_pos[1,station]
			
			; We support fractional indexes
			p =  bilinear(pressure[*,*,i],col,row)
			Temp = bilinear(t[*,*,i],col,row)
			
			V_n = ( R * Temp ) / p
			V_0 = ( R * T0 ) / p0
			
			f_n = V_n / V_0

			; Gasses need convesion to ppb and norm-volume correction
			if (species->iscontained('NO')) then begin
				no=z[0,species->get('NO'),i,station] * 1000 * ( M_NO / V_n) * f_n
			endif else begin
				no=0
			endelse

			if (species->iscontained('NO2')) then begin
				no2=z[0,species->get('NO2'),i,station] * 1000 * ( M_NO2 / V_n) * f_n
			endif else begin
				no2=0
			endelse
			
			if (species->iscontained('O3')) then begin
				o3=z[0,species->get('O3'),i,station] * 1000 * ( M_O3 / V_n) * f_n
			endif else begin
				o3=0
			endelse
			
			; Start of aerosol species (already in proper unit - but need correction to norm conditions)
			
			if (species->iscontained('PH2O')) then begin
				ph2o=z[0,species->get('PH2O'),i,station] * f_n
			endif else begin
				ph2o=0
			endelse
			
			if (species->iscontained('PNO3')) then begin
				pno3=z[0,species->get('PNO3'),i,station] * f_n
			endif else begin
				pno3=0
			endelse
			
			if (species->iscontained('PSO4')) then begin
				pso4=z[0,species->get('PSO4'),i,station] * f_n
			endif else begin
				pso4=0
			endelse
			
			if (species->iscontained('PNH4')) then begin
				pnh4=z[0,species->get('PNH4'),i,station] * f_n
			endif else begin
				pnh4=0
			endelse
			
			if (species->iscontained('POA')) then begin
				poa=z[0,species->get('POA'),i,station] * f_n
			endif else begin
				poa=0
			endelse
			
			if (species->iscontained('PEC')) then begin
				pec=z[0,species->get('PEC'),i,station] * f_n
			endif else begin
				pec=0
			endelse
			
			if (species->iscontained('SOA1')) then begin
				soa1=z[0,species->get('SOA1'),i,station] * f_n
			endif else begin
				soa1=0
			endelse
			
			if (species->iscontained('SOA2')) then begin
				soa2=z[0,species->get('SOA2'),i,station] * f_n
			endif else begin
				soa2=0
			endelse
			
			if (species->iscontained('SOA3')) then begin
				soa3=z[0,species->get('SOA3'),i,station] * f_n
			endif else begin
				soa3=0
			endelse
			
			if (species->iscontained('SOA4')) then begin
				soa4=z[0,species->get('SOA4'),i,station] * f_n
			endif else begin
				soa4=0
			endelse
			
			if (species->iscontained('SOA5')) then begin
				soa5=z[0,species->get('SOA5'),i,station] * f_n
			endif else begin
				soa5=0
			endelse
			
			if (species->iscontained('SOA6')) then begin
				soa6=z[0,species->get('SOA6'),i,station] * f_n
			endif else begin
				soa6=0
			endelse
			
			if (species->iscontained('SOA7')) then begin
				soa7=z[0,species->get('SOA7'),i,station] * f_n
			endif else begin
				soa7=0
			endelse
			
			if (species->iscontained('SOPA')) then begin
				sopa=z[0,species->get('SOPA'),i,station] * f_n
			endif else begin
				sopa=0
			endelse
			
			if (species->iscontained('SOPB')) then begin
				sopb=z[0,species->get('SOPB'),i,station] * f_n
			endif else begin
				sopb=0
			endelse
			
			if (species->iscontained('NA')) then begin
				na	=z[0,species->get('NA'),i,station] * f_n
			endif else begin
				na=0
			endelse
			
			if (species->iscontained('PCL')) then begin
				pcl =z[0,species->get('PCL'),i,station] * f_n
			endif else begin
				pcl=0
			endelse
			
			if (species->iscontained('FPRM')) then begin
				fprm=z[0,species->get('FPRM'),i,station] * f_n
			endif else begin
				fprm=0
			endelse
			
			if (species->iscontained('FCRS')) then begin
				fcrs=z[0,species->get('FCRS'),i,station] * f_n
			endif else begin
				fcrs=0
			endelse
			
			if (species->iscontained('CPRM')) then begin
				cprm=z[0,species->get('CPRM'),i,station] * f_n
			endif else begin
				cprm=0
			endelse
			
			if (species->iscontained('CCRS')) then begin
				ccrs=z[0,species->get('CCRS'),i,station] * f_n
			endif else begin
				ccrs=0
			endelse
			
			pm = pno3 + pso4 + pnh4	+ poa + pec + soa1	+ soa2 + soa3	+ soa4	+ soa5 + soa6 + soa7 + sopa + sopb + na + pcl + fprm + fcrs + cprm + ccrs
			
			; dd : day (character 2 digit)
			; mm: month (character 2 digit)
			; yyyy year (character 4 digit)
			; hh: hour	(character 2 digit)
			
			
			; this is the new format statement required by COST. 
			ofmt = '(A2, "," , A2, "," , A4, "," , A2, 4(",",F5.1) )'
			
			printf,station_luns[station],strtrim(string(day),2),strtrim(string(month),2),strtrim(string(year),2),strtrim(string(i),2),pm,no2,no,o3,format = ofmt
			
		endfor ; stations
		
	endfor ; time
	
	; close all output files
	for i=0L,num_stations-1 do begin
	
		free_lun,station_luns[i]
		
	endfor
	
	; And the input 
	close,input_lun
	
	return
end
