pro extract_arpa_stations,input_file,output_dir,write_header,day,month,year,species,x_dim,y_dim,num_levels,stations,temp_file,zp_file,format=fmt
	;
	; Function: extract_arpa_stations
	;
	;*******************************************************************************************************
	;						IDL Program to extract data from a camx average file from domain 3
	;						for ARPA stations. Modified version of extract_nabel_stations written for co5.
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
	; species - a string array of the species to extract
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
	; PM10: hourly	concentration ( �g/m�)	(REAL o INTEGER) 
	; NO2 : hourly concentration (�g/m�)	. (REAL o INTEGER)
	; NO : hourly concentration	(�g/m�) (REAL o INTEGER)
	; O3: hourly concentration	( �g/m�)	(REAL o INTEGER)
	
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
	
	
	my_revision_string='$Id$'
	my_revision_arr=strsplit(my_revision_string,' ',/EXTRACT)
	
	print,my_revision_arr[1] + ' has revision ' + my_revision_arr[2]
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Check settings
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	num_species = n_elements(species)
	
	; stations are multidimensional
	s = size(stations,/DIMENSIONS)
	num_stations = s[1]
	
	if ( num_species EQ 0) then message,"Must get more than 0 species to extract!"
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
	
	; we need the next arrays becase we need to interolate to the ground
	total_pressure=fltarr(x_dim, y_dim, num_levels)
	total_temperature=fltarr(x_dim, y_dim, num_levels)
	total_height=fltarr(x_dim, y_dim, num_levels)
	
	; to read, we need the slices
	pressure_slice=fltarr(x_dim, y_dim)
	height_slice=fltarr(x_dim, y_dim)
	t_slice=fltarr(x_dim, y_dim)
	
	; Open the input
	openr,input_t,temp_file, /GET_LUN
	openr,input_zp,zp_file, /GET_LUN
	
	;do loop for time
	for i=0L,23 do begin
	
		;do loop for layers
		for iver=0L,num_levels-1 do begin

			; skip one line (timestamp)
			skip_lun,input_t,1
			skip_lun,input_zp,1
			
			; Read height data first
			readf,input_zp,height_slice
			; skip another line (timestamp) of the pressure file
			skip_lun,input_zp,1
			
			; read one slice
			readf,input_t,pressure_slice
			readf,input_zp,t_slice
			
			; Fill the "Total" Arrays
			total_pressure[0,0,iver] = pressure_slice
			total_height[0,0,iver] = height_slice
			total_temperature[0,0,iver] = temperature_slice
			

		endfor ; layer

		; Interpolate ground pressure and temperature
		; we have the pressure (that is our function of h)
		; in total_pressure[i,j,k]
		; the height are the interfarce heights. 
		; We calculate the center height of the two lowest levels (our x values)
		; (Assuming that these height correspond to the average pressure of each level)
		x0 = total_height[*,*,0]/2
		x1 = total_height[*,*,0] + ((total_height[*,*,1] - total_height[*,*,0])/2)
		
		; These are the corresponding pressures
		y0 = total_pressure[*,*,0]
		y1 = total_pressure[*,*,1]

		; We want to get the value for a height of 0
		x=replicate(0,x_dim, y_dim)
		
		pressure[0,0,i] = y0 + (x - x0)*((y1 - y0)/(x1 - x0))
		
		; Lets do the same for temperature
		y0 = total_temperature[*,*,0]
		y1 = total_temperature[*,*,1]
		
		t[0,0,i] = y0 + (x - x0)*((y1 - y0)/(x1 - x0))
	
	endfor ; time
	
	; Close files
	free_lun,input_t
	free_lun,input_zp
	
	
	
	;;;;;;;;;;;;;;;;;;;; End pressure/temp preparation
	
	
	; x, y, z, species, hours
	c=fltArr(x_dim,y_dim,num_levels,num_species,24)
	
	; x, y
	c1=fltArr(x_dim,y_dim)
	
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
	z=fltArr(num_levels,num_species,24,num_stations)
	
	; the time offset (formerly t(i)) is calculated like this (We assume that hour 1 is alwas at the beginning of a month):
	; only needed if time should be relative to first day.
	offset=(Fix(day)-1)*24+1
	
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
	
	; The length of the header depends on the number of species
	; because they are listed first
	head_length=num_species + 4
	
	skip_lun, input_lun,head_length, /LINES
	;
	;do loop for time
	;
	for i=0L,23 do begin
	
		skip_lun, input_lun,1, /LINES
		;
		;do loop for species
		;
		for ispec=0L,num_species-1 do begin
		
			;do loop for layers
			for iver=0L,num_levels-1 do begin
				
				skip_lun, input_lun,1, /LINES 
				
				; We let IDL work out the format
				readf,input_lun,c1,format=fmt
					
					c[0,0,iver,ispec,i]=c1
					
					; loop through the stations and do the interpolation
					for station=0L,num_stations-1 do begin
					
						;ix=44.62
						;jy=73.26
						;z1(iver,ispec,i)=bilinear(c1,ix,jy)
						; bilinear allows us to retrieve decimal indices
						z[iver,ispec,i,station]=bilinear(c1,station_pos[0,station],station_pos[1,station])
						
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
		 
		 ;print,'Extracting meteo data for file ' + station_files(station) + ' at MM5 col ' + string(col) + ' row ' + string(row)
		 
		 p =  pressure[col,row,i]
		 Temp = t[col,row,i]
		 
		 V_n = ( R * Temp ) / p
		 V_0 = ( R * T0 ) / p0
		 
		 f_n = V_n / V_0
		 
		 ;print,'Extracting chemical data for file ' + station_files(station) + ' at CAMx col ' + string(station_pos[0,station]) + ' row ' + string(station_pos[1,station])

			no=z[0,0,i,station] * 1000 * ( M_NO / V_n) * f_n
			no2=z[0,1,i,station] * 1000 * ( M_NO2 / V_n) * f_n
			o3=z[0,2,i,station] * 1000 * ( M_O3 / V_n) * f_n
			tol=z[0,3,i,station] * 1000 * ( M_TOL / V_n) * f_n
			xyl=z[0,4,i,station] * 1000 * ( M_XYL / V_n) * f_n
			form=z[0,5,i,station] * 1000 * ( M_FORM / V_n) * f_n
			pan=z[0,6,i,station] * 1000 * ( M_PAN / V_n) * f_n
			co=z[0,7,i,station] * 1000 * ( M_CO / V_n) * f_n
			hono=z[0,8,i,station] * 1000 * ( M_HONO / V_n) * f_n
			hno3=z[0,9,i,station] * 1000 * ( M_HNO3 / V_n) * f_n
			h2o2=z[0,10,i,station] * 1000 * ( M_H2O2 / V_n) * f_n
			isop=z[0,11,i,station] * 1000 * ( M_ISOP / V_n) * f_n
			pna=z[0,12,i,station] * 1000 * ( M_PNA / V_n) * f_n
			so2=z[0,13,i,station] * 1000 * ( M_SO2 / V_n) * f_n
			nh3=z[0,14,i,station] * 1000 * ( M_NH3 / V_n) * f_n
			
			; Start of aerosol species (already in proper unit - but need correction to norm conditions)
			
			ph2o=z[0,15,i,station] * f_n
			pno3=z[0,16,i,station] * f_n
			pso4=z[0,17,i,station] * f_n
			pnh4=z[0,18,i,station] * f_n
			poa=z[0,19,i,station] * f_n
			pec=z[0,20,i,station] * f_n
			soa1=z[0,21,i,station] * f_n
			soa2=z[0,22,i,station] * f_n
			soa3=z[0,23,i,station] * f_n
			soa4=z[0,24,i,station] * f_n
			soa5=z[0,25,i,station] * f_n
			soa6=z[0,26,i,station] * f_n
			soa7=z[0,27,i,station] * f_n
			sopa=z[0,28,i,station] * f_n
			sopb=z[0,29,i,station] * f_n
			na	=z[0,30,i,station] * f_n
			pcl =z[0,31,i,station] * f_n
			fprm=z[0,32,i,station] * f_n
			fcrs=z[0,33,i,station] * f_n
			cprm=z[0,34,i,station] * f_n
			ccrs=z[0,35,i,station] * f_n
			
			pm = pno3 + pso4 + pnh4	+ poa + pec + soa1	+ soa2 + soa3	+ soa4	+ soa5 + soa6 + soa7 + sopa + sopb + na + pcl + fprm + fcrs + cprm + ccrs
			
			; the time in hours was formerly calculated using the offset
			; printf,station_luns(station),i+offset,no,no2,o3,tol,xyl,form,pan,co,hono,hno3,h2o2,isop,pna,so2,nh3,ph2o,pno3,pso4,pnh4,poa,pec,soa1,soa2,soa3,soa4,soa5,format = '(A,27G15.7)'
			
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
	
		free_lun,station_luns(i)
		
	endfor
	
	; And the input 
	close,input_lun
	
	return
end
