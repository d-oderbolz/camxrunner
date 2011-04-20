pro extract_nabel_stations,input_file,output_dir,day,month,year,model_hour,species,stations,num_levels,is_binary=is_binary
;
; Function: extract_nabel_stations
;
;*******************************************************************************************************
;	IDL Program to extract data from a camx average file from domain 3
;	for Nabel stations
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
; Parameters:
; input_file - input file to operate on
; output_dir - directory where to store output (no trailing /)
; day - day to be extracted
; month - month  to be extracted
; year - year to be extracted
; model_hour - the number of hours already modelled. Used as an offset for time calculations. The first dataset gets this time (model_hour+0)
; species - a string array of the species to extract
; stations - a 2D string array with [x,y,filename] in it (x,y may be integer or float grid indexes)
; num_levels - because we cannot infer the levels from the output (may be 1 no save space), we need to know it.
; [is_binary=] - a boolean, if true (default false), we read binary
;
;>            The stations are loaded with an @ script which can be created by the CAMx-runner.sh
;
;*******************************************************************************************************
; Who   When        What
; dco   22.07.2009  Improved time handling

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Report revision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
my_revision_string='$Id$'
my_revision_arr=strsplit(my_revision_string,' ',/EXTRACT)
print,my_revision_arr[1] + ' has revision ' + my_revision_arr[2]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; if a species is in here, its treated as a gas
gasses=['NO','NO2','O3','TOL','XYL','FORM','PAN','HONO','HNO3','H2O2','ISOP','PNA','SO2','NH3']

ppm2ppb=1000.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (n_elements(is_binary) eq 0) then is_binary=0
	
	
	; Load header Parser
	hp=obj_new('header_parser',input_file,is_binary)
	
	; Get number of species in average file
	scalars=hp->get_scalars()
	num_input_species=scalars->get('nspec')
	input_species=hp->get_species_arr()
	
	; Get dimensions
	x_dim_file=scalars->get('nx')
	y_dim_file=scalars->get('ny')
	z_dim_file=scalars->get('nz')
	
	num_species = n_elements(species)
	
	; stations are multidimensional
	s = size(stations,/DIMENSIONS)
	num_stations = s[1]
	
	if ( num_species EQ 0) then message,"Must get more than 0 species to extract!"
	if ( num_stations EQ 0) then message,"Must get more than 0 stations to extract!"

	print,'Extracting ' + strtrim(num_species,2) + ' species.'
	
	; x, y
	conc_slice=fltArr(x_dim_file,y_dim_file)

	; Station information
	; We must read the first 2 fields and convert to float afterwards
	station_pos_str=stations[0:1,*]
	station_pos=float(station_pos_str)
	
	; Names of the station files (read from stations array)
	; Add output dir (saves space in call)
	station_files=output_dir + '/' + stations[2,*]
	
	; Logical Unit Numbers of theses files (assigned by IDL)
	station_luns=intArr(num_stations)
	
	;species, station
	current_station_conc=fltArr(num_species,num_stations)
	
	print,'Opening Input file.'
	
	; Open the input
	if (is_binary) then begin
		openr,input_lun,input_file,/GET_LUN,/F77_UNFORMATTED,/SWAP_ENDIAN
	endif else begin
		openr,input_lun,input_file, /GET_LUN
	endelse
	
	print,'Opening all Output files.'
	
	; Open all the output files
	; and store the luns
	for i=0L,num_stations-1 do begin
	
		; We can no longer get the lun because we could only open 29 files then
		current_output_lun = i + 1

		openw,current_output_lun,station_files[i], width=2400
		station_luns[i]=current_output_lun
		
	endfor
	
	;skip informational data and species
	
	; Get header length
	head_length = hp->get_header_length()
	
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
		
			; Get the name of the species
			current_species=input_species[ispec]
		
			;do loop for layers (we need to do this
		  ; even if we would not want this species)
			for iver=0L,z_dim_file-1 do begin
			
			if (is_binary) then begin
					ione=1L
					mspec=hp->prefill(4,10)
	
					; Read the current data
					readu,input_lun,ione,mspec,conc_slice
				endif else begin
					skip_lun, input_lun,1, /LINES 
				
					; We let IDL work out the format
					readf,input_lun,conc_slice
				endelse
				
				; Do we want this species?
				index=WHERE(species EQ current_species, count)
				
				if (count GT 0 && iver EQ 0) then begin
					; Yes
					; loop through the NABEL stations
					for station=0L,num_stations-1 do begin
					
						; Do the bilinear interpolation
							current_station_conc[index,station]=bilinear(conc_slice,station_pos[0,station],station_pos[1,station])
					
							; Fix the gasses
							; is it a gas?
							dummy=WHERE(gasses EQ current_species, count)
							
							if (count GT 0) then begin
								; its a gas
								if (station EQ 0 && iHour EQ 0) then print,current_species + ' is a gas.'
								
								current_station_conc[index,station]=current_station_conc[index,station]*ppm2ppb
							endif
					
					endfor
				
				endif ; do-we-want-species?
				
			endfor
		endfor
		
		; loop through the NABEL stations
		for station=0L,num_stations-1 do begin
		
			; the time in hours is calculated using the offset
			; The with of the arguments is calculated from the number of species
			printf,station_luns[station],iHour+model_hour,current_station_conc[*,station],format = '(A,' + strtrim((num_species + 1),2) + 'G15.7)'
			
		endfor
	endfor
	
	; close all output files
	for iStation=0L,num_stations-1 do begin
	
		close,station_luns(iStation)
	
	endfor
	
	; And the input 
	close,input_lun
	
	return
end
