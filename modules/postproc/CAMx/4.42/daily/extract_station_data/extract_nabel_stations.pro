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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (n_elements(is_binary) eq 0) then is_binary=0


; Load header Parser
hp=obj_new('header_parser',input_file,is_binary)

; Get number of species in average file
scalars=hp->get_scalars()

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
	; time
	t=fltArr(24)
	;
	
	; Station information
	; We must read the first 2 fields and convert to float afterwards
	station_pos_str=stations[0:1,*]
	station_pos=float(station_pos_str)
	
	; Names of the station files (read from stations array)
	; Add output dir (saves space in call)
	station_files=output_dir + '/' + stations[2,*]
	
	; Logical Unit Numbers of theses files (assigned by IDL)
	station_luns=intArr(num_stations)
	
	;z, species, hours, station
	z=fltArr(z_dim_file,num_species,24,num_stations)
	
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
	for i=0L,23 do begin
	
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
		for ispec=0L,num_species-1 do begin
		
			;do loop for layers
			for iver=0L,num_levels-1 do begin
			
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

				; loop through the NABEL stations
				for station=0L,num_stations-1 do begin
				
					;ix=44.62
					;jy=73.26
					;z1(iver,ispec,i)=bilinear(conc_slice,ix,jy)
					
					z[iver,ispec,i,station]=bilinear(conc_slice,station_pos[0,station],station_pos[1,station])
					
				endfor
				
			endfor
		endfor
		
		; loop through the NABEL stations
		for station=0L,num_stations-1 do begin
		
			;ix=44.62
			;jy=73.26
			;z1(iver,ispec,i)=bilinear(conc_slice,ix,jy)
			
			; fix the gasses (be careful! Future releases will no longer do this here)
			
			z[0,0,i,station]=z[0,0,i,station]*1000
			z[0,1,i,station]=z[0,1,i,station]*1000
			z[0,2,i,station]=z[0,2,i,station]*1000
			z[0,3,i,station]=z[0,3,i,station]*1000
			z[0,4,i,station]=z[0,4,i,station]*1000
			z[0,5,i,station]=z[0,5,i,station]*1000
			z[0,6,i,station]=z[0,6,i,station]*1000
			z[0,7,i,station]=z[0,7,i,station]*1000
			z[0,8,i,station]=z[0,8,i,station]*1000
			z[0,9,i,station]=z[0,9,i,station]*1000
			z[0,10,i,station]=z[0,10,i,station]*1000
			z[0,11,i,station]=z[0,11,i,station]*1000
			z[0,12,i,station]=z[0,12,i,station]*1000
			z[0,13,i,station]=z[0,13,i,station]*1000
			z[0,14,i,station]=z[0,14,i,station]*1000
			
			; the time in hours is calculated using the offset
			; The with of the arguments is calculated from the number of species
			printf,station_luns[station],i+model_hour,z[0,*,i,station],format = '(A,' + strtrim((num_species + 1),2) + 'G15.7)'
			
		endfor
	endfor
	
	; close all output files
	for i=0L,num_stations-1 do begin
	
		close,station_luns(i)
	
	endfor
	
	; And the input 
	close,input_lun
	
	return
end
