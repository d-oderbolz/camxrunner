pro extract_arpa_stations,input_file,output_dir,write_header,day,month,year,stations,temp_file,zp_file,format=fmt,norm_method=norm_method,is_binary=is_binary
	;
	; Function: extract_arpa_stations
	;
	;*******************************************************************************************************
	;	IDL Program to extract data from a camx average file from domain 3
	;	for ARPA stations. Modified version of extract_nabel_stations written for co5.
	;	THe list of species to be extracted is given by the project.
	;
	;	based on a script by Sebnem Andreani-Aksoyoglu, 11.1.2007
	;
	;	modified by Daniel Oderbolz (daniel.oderbolz@psi.ch)
	;
	;	$Id$
	;
	;	This script is called automatically by the output processor
	;	<extract_station_data> - *hesitate to change the calling interface*!
	;
	;	Originally, the code used \GET_LUN, but IDL can only open 29 files like this
	;	(http://idlastro.gsfc.nasa.gov/idl_html_help/Understanding_(LUNs).html) so we
	;	had to go for the less beautiful approach...
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
	; z_dim - the number of levels of the grid in question
	; stations - a 2D string array with [x,y,filename] in it (x,y may be integer or float grid indexes)
	; zp_file - pressure/height ASCII file
	; temp_file - temperature ASCII file
	; [format=] - The format of the numbers. Normally specified as fmt='(9e14.9)', bin2asc writes (5e14.7)
	; [norm_method=] - a string selecting the approach to normalize concentrations to standard conditions:
	;                        'physical' - using the models T and P fields (default)
	;                        'nabel' - use the NABELs constant factors (ASSUMING h < 1500 m)
	;                        'none' - do not correct
	; [is_binary=] - a boolean, if true (default false), we read binary
	; Output format (comma-separated):
	; dd : day (character 2 digit)
	; mm: month (character 2 digit)
	; yyyy year (character 4 digit)
	; hh: hour (character 2 digit)
	; PM10: hourly	concentration ( ug/m**2)	(REAL o INTEGER) 
	; NO2 : hourly concentration (ug/m**2)	. (REAL o INTEGER)
	; NO : hourly concentration	(ug/m**2) (REAL o INTEGER)
	; O3: hourly concentration	( ug/m**2)	(REAL o INTEGER)
	;
	; Gaseous species are converted to norm conditions, aerosols not (b. c. these are normally integrated measurements)
	;*******************************************************************************************************
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; IDL Settings
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Show Math errors as they occur
	;!EXCEPT=2
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
	
	if (n_elements(is_binary) eq 0) then is_binary=0
	
	; Load header Parser
	hp=obj_new('header_parser',input_file,is_binary)
	
	; Get number of species in average file
	scalars=hp->get_scalars()
	num_input_species=scalars->get('nspec')
	head_length = hp->get_header_length()
	
	; Get dimensions
	x_dim=scalars->get('nx')
	y_dim=scalars->get('ny')
	z_dim=scalars->get('nz')
	
	; Get list of species in average file
	species=hp->get_species()

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Check settings
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
	
	; physical is default norm method
	if (n_elements(norm_method) EQ 0) then norm_method='physical'
	norm_method=strlowcase(norm_method)
	
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
	
	; Convert ppm to ppb
	ppm2ppb = 1E3
	
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
	total_pressure=fltarr(x_dim, y_dim, z_dim)
	total_temperature=fltarr(x_dim, y_dim, z_dim)
	total_height=fltarr(x_dim, y_dim, z_dim)
	
	; to read, we need the slices
	pressure_slice=fltarr(x_dim, y_dim)
	height_slice=fltarr(x_dim, y_dim)
	temp_slice=fltarr(x_dim, y_dim)
	
	; Open the input
	openr,input_t,temp_file, /GET_LUN
	openr,input_zp,zp_file, /GET_LUN
	
	;do loop for time
	for iHour=0L,23 do begin
	
		; temperature is stored as surface temp and that in levels
		; Skip header of T file
		skip_lun,input_t,1
		
		; read surface temp
		readf,input_t,temp_slice
		
		t[0,0,iHour]=temp_slice
	
		;do loop for layers (height & temp file)
		for iver=0L,z_dim-1 do begin
			
			; skip next temp header
			skip_lun,input_t,1
			
			; This is the height-dependent portion
			readf,input_t,temp_slice
			
			; skip one line (timestamp)
			skip_lun,input_zp,1
			
			; height data is first in the ZP file
			readf,input_zp,height_slice
			
			; skip another line (timestamp) of the pressure file
			skip_lun,input_zp,1
			
			; Height data is second
			readf,input_zp,pressure_slice
			
			; Fill the "Total" Arrays
			total_temperature[0,0,iver] = temp_slice
			total_height[0,0,iver] = height_slice
			total_pressure[0,0,iver] = pressure_slice

		endfor ; layer
		
		; For the vertical interpolation, we use 1D Interpolation
		; Therefore, we need to loop (is there a better way??)
		for iCol = 0, x_dim - 1 do begin
			for jRow = 0, y_dim - 1 do begin
				; Interpolate ground pressure and temperature
				; Both temperature and pressure correspond to the heights
				
				; We want to get the data at this height above ground
				h0 = 0
				
				; IDLs interpolate does not anticipate DIV/0 errors,
				; so we need to check for it:
				
				if ( total_height[iCol,jRow,0] NE total_height[iCol,jRow,1] ) then begin
				
					; Pressure interpolation
					pressure[iCol,jRow,iHour] = interpol(total_pressure[iCol,jRow,*],total_height[iCol,jRow,*],h0)
	
				endif else begin
					
					; For some reason height[1] == height[0]
					print,'WRN: Cannot interpolate at col ' + strtrim(iCol,2) + ' row ' + strtrim(jRow,2)
					
					; We use the lowest level
					pressure[iCol,jRow,iHour] = total_pressure[iCol,jRow,0]
					
				endelse

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
	
	; contains the lowest levels avg concentrations for the current hour
	; at the corrdinates of the stations
	station_conc=fltArr(num_input_species,num_stations)
	
	; Open the input
	if (is_binary) then begin
		openr,input_lun,input_file,/GET_LUN,/F77_UNFORMATTED,/SWAP_ENDIAN
	endif else begin
		openr,input_lun,input_file, /GET_LUN
	endelse
	
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
	print,'Skipping big header.'
	if (is_binary) then begin
		point_lun, input_lun,head_length
	endif else begin
		skip_lun, input_lun,head_length, /LINES
	endelse
	
	;
	;do loop for time
	;
	for iHour=0L,23 do begin
	
		; Deal with time header
		if (is_binary) then begin
			; Reading the time header into dummy variables
			ibdate=0L
			btime=0L
			iedate=0L
			etime=0L
			
			readu,input_lun,ibdate,btime,iedate,etime
		endif else begin
			skip_lun, input_lun,1, /LINES
		endelse
		
		
		;
		;do loop for species
		;
		for ispec=0L,num_input_species-1 do begin
		
			;do loop for layers
			for iver=0L,z_dim-1 do begin
			
				if (is_binary) then begin
					ione=1L
					mspec=hp->prefill(4,10)
	
					; Read the current data
					readu,current_input_lun,ione,mspec,conc_slice
				endif else begin
					skip_lun, input_lun,1, /LINES 
				
					; We let IDL work out the format
					readf,input_lun,conc_slice,format=fmt
				endelse
				
				; We are only interested in ground level 
				if ( iver EQ 0) then begin
					
					; loop through the stations and do the interpolation
					for station=0L,num_stations-1 do begin
	
						; bilinear allows us to retrieve decimal indices
						station_conc[ispec,station]=bilinear(conc_slice,station_pos[0,station],station_pos[1,station])
						
					endfor ; stations
				
				endif
			endfor ; layers
		endfor ; species
		
		; loop through the stations
		for station=0L,num_stations-1 do begin
		
			; All gasses need to be converted to microg/m3 (from ppm)
			; and corrected for STP
			; Aerosols are not corrected for norm conditions
			
			; Where do we look
			col = station_pos[0,station]
			row = station_pos[1,station]
			
			; We support fractional indexes
			p =  bilinear(pressure[*,*,iHour],col,row)
			Temp = bilinear(t[*,*,iHour],col,row)
			
			; conversion from ppb to ug/m**3
			ppb2ugm = p / ( R * Temp )
			
			; Adjustment to environment
			f_n = (T0 / Temp) * (p / p0)

			if (~Finite(ppb2ugm)) then begin
				print,'WRN: ppb2ugm is non-finite at time ' + strtrim(iHour,2)  + ', using norm values at col ' + strtrim(col,2) + ' row ' + strtrim(row,2)
				ppb2ugm = p0 / ( R * T0 )
			endif
			
			if (~Finite(f_n)) then begin
				print,'WRN: f_n is non-finite at time ' + strtrim(iHour,2)  + ', using 1 at col ' + strtrim(col,2) + ' row ' + strtrim(row,2)
				f_n = 1
			endif
			
			; Determine the conversion factors according to to chosen method
			case norm_method of
			
				'physical': begin
								; Here we consider normalisation
								C_NO = M_NO * ppb2ugm * f_n
								C_NO2 = M_NO2 * ppb2ugm * f_n
								C_O3 = M_O3 * ppb2ugm * f_n
							end
							
				'nabel': 	begin
								; Just use constants
								C_NO = 1.25
								C_NO2 = 1.91
								C_O3 = 2.00
							end
				
				'none':		begin
								; Only do the ppb conversion, but not the normalisation
								C_NO = M_NO * ppb2ugm
								C_NO2 = M_NO2 * ppb2ugm
								C_O3 = M_O3 * ppb2ugm
							end
							
				else:		begin
								print,'WRN: Unsupported norm_method ' + norm_method + ' falling back to none'
								; Only do the ppb conversion, but not the normalisation
								C_NO = M_NO * ppb2ugm
								C_NO2 = M_NO2 * ppb2ugm
								C_O3 = M_O3 * ppb2ugm
							end
			endcase
			
			if (iHour EQ 9) then begin
				print,'Conversion factors for hour 9 for file (ppb -> ugm)' + stations[2,station]
				print,'NO:' + strtrim(C_NO,2)
				print,'NO2:' + strtrim(C_NO2,2)
				print,'O3:' + strtrim(C_O3,2)
				
				if (station EQ 0) then begin
					print,'Factors used by NABEL < 1500 masl: (NABEL,2007)'
					print,'NO: 1.25'
					print,'NO2:1.91'
					print,'O3:2.00' 
				endif
				
			endif 

			; Gasses need convesion to ppb and norm-volume correction
			if (species->iscontained('NO')) then begin
				no=station_conc[species->get('NO'),station] * ppm2ppb * C_NO
			endif else begin
				no=0
			endelse

			if (species->iscontained('NO2')) then begin
				no2=station_conc[species->get('NO2'),station] * ppm2ppb * C_NO2
			endif else begin
				no2=0
			endelse
			
			if (species->iscontained('O3')) then begin
				o3=station_conc[species->get('O3'),station] * ppm2ppb *  C_O3
			endif else begin
				o3=0
			endelse
			
			; Start of aerosol species (already in proper unit - and will not be corrected to norm conditions)
			
			if (species->iscontained('PH2O')) then begin
				ph2o=station_conc[species->get('PH2O'),station]
			endif else begin
				ph2o=0
			endelse
			
			if (species->iscontained('PNO3')) then begin
				pno3=station_conc[species->get('PNO3'),station]
			endif else begin
				pno3=0
			endelse
			
			if (species->iscontained('PSO4')) then begin
				pso4=station_conc[species->get('PSO4'),station]
			endif else begin
				pso4=0
			endelse
			
			if (species->iscontained('PNH4')) then begin
				pnh4=station_conc[species->get('PNH4'),station]
			endif else begin
				pnh4=0
			endelse
			
			if (species->iscontained('POA')) then begin
				poa=station_conc[species->get('POA'),station]
			endif else begin
				poa=0
			endelse
			
			if (species->iscontained('PEC')) then begin
				pec=station_conc[species->get('PEC'),station]
			endif else begin
				pec=0
			endelse
			
			if (species->iscontained('SOA1')) then begin
				soa1=station_conc[species->get('SOA1'),station]
			endif else begin
				soa1=0
			endelse
			
			if (species->iscontained('SOA2')) then begin
				soa2=station_conc[species->get('SOA2'),station]
			endif else begin
				soa2=0
			endelse
			
			if (species->iscontained('SOA3')) then begin
				soa3=station_conc[species->get('SOA3'),station]
			endif else begin
				soa3=0
			endelse
			
			if (species->iscontained('SOA4')) then begin
				soa4=station_conc[species->get('SOA4'),station]
			endif else begin
				soa4=0
			endelse
			
			if (species->iscontained('SOA5')) then begin
				soa5=station_conc[species->get('SOA5'),station]
			endif else begin
				soa5=0
			endelse
			
			if (species->iscontained('SOA6')) then begin
				soa6=station_conc[species->get('SOA6'),station]
			endif else begin
				soa6=0
			endelse
			
			if (species->iscontained('SOA7')) then begin
				soa7=station_conc[species->get('SOA7'),station]
			endif else begin
				soa7=0
			endelse
			
			if (species->iscontained('SOPA')) then begin
				sopa=station_conc[species->get('SOPA'),station]
			endif else begin
				sopa=0
			endelse
			
			if (species->iscontained('SOPB')) then begin
				sopb=station_conc[species->get('SOPB'),station]
			endif else begin
				sopb=0
			endelse
			
			if (species->iscontained('NA')) then begin
				na	=station_conc[species->get('NA'),station]
			endif else begin
				na=0
			endelse
			
			if (species->iscontained('PCL')) then begin
				pcl =station_conc[species->get('PCL'),station]
			endif else begin
				pcl=0
			endelse
			
			if (species->iscontained('FPRM')) then begin
				fprm=station_conc[species->get('FPRM'),station]
			endif else begin
				fprm=0
			endelse
			
			if (species->iscontained('FCRS')) then begin
				fcrs=station_conc[species->get('FCRS'),station]
			endif else begin
				fcrs=0
			endelse
			
			if (species->iscontained('CPRM')) then begin
				cprm=station_conc[species->get('CPRM'),station]
			endif else begin
				cprm=0
			endelse
			
			if (species->iscontained('CCRS')) then begin
				ccrs=station_conc[species->get('CCRS'),station]
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
			
			printf,station_luns[station],strtrim(string(day),2),strtrim(string(month),2),strtrim(string(year),2),strtrim(string(iHour),2),pm,no2,no,o3,format = ofmt
			
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
