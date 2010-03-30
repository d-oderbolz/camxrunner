pro extract_nabel_stations,input_file,output_dir,write_header,day,month,year,model_hour,species,x_dim,y_dim,num_levels,stations,format=fmt
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
;
; Parameters:
; input_file - input file to operate on
; output_dir - directory where to store output (no trailing /)
; write_header - boolean flag to indicate if a header is needed (usually for first day)
; day - day to be extracted
; month - month  to be extracted
; year - year to be extracted
; model_hour - the number of hours already modelled. Used as an offset for time calculations. The first dataset gets this time (model_hour+0)
; species - a string array of the species to extract
; x_dim - the x dimension of the grid in grid cells of the grid in question  
; y_dim - the y dimension of the grid in grid cells of the grid in question
; num_levels - the number of levels of the grid in question
; stations - a 2D string array with [x,y,filename] in it (x,y may be integer or float grid indexes)
; format - The format of the numbers. Normally specified as fmt='(9e14.9)', bin2asc writes (5e14.7)
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

num_species = n_elements(species)

; stations are multidimensional
s = size(stations,/DIMENSIONS)
num_stations = s[1]


; Define header (time plus species)
columnHeaders = strarr(num_species + 1)
columnHeaders[0]='TIME'
columnHeaders[1:num_species] = species
xsize = N_Elements(columnHeaders)

if ( num_species EQ 0) then message,"Must get more than 0 species to extract!"
if ( num_stations EQ 0) then message,"Must get more than 0 stations to extract!"

	; is fmt set?
	
	print,'Extracting ' + strtrim(num_species,2) + ' species.'
	
	if n_Elements(fmt) EQ 0 then fmt='(9e14.9)'
	
	; x, y, z, species, hours
	c=fltArr(x_dim,y_dim,num_levels,num_species,24)
	
	; x, y
	c1=fltArr(x_dim,y_dim)
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
	z=fltArr(num_levels,num_species,24,num_stations)
	
	print,'Opening Input file.'
	
	; Open the input
	openr,input_lun,input_file, /GET_LUN
	
	print,'Opening all Output files.'
	
	; Open all the output files
	; and store the luns
	; write header if needed
	for i=0L,num_stations-1 do begin

		openw,current_output_lun,station_files[i], /GET_LUN, width=2400
		station_luns[i]=current_output_lun
		
		; If we must, write the header
		if (write_header and N_Elements(columnHeaders) NE 0) then begin
			print,'Writing Header...'
			
				; This is Fanning code: http://www.dfanning.com/tip_examples/write_csv_data.pro
				; Make sure these are strings.
				sColumns = StrTrim(columnHeaders, 2)
	
				; Write the headers to the file.
				PrintF, station_luns[i], sColumns, ,format = '(A,' + strtrim((num_species + 1),2) + ')'
		endif
		
	endfor
	
	;skip informational data and species
	
	; The length of the header depends on the number of species
	; because they are listed first
	head_length=num_species + 4
	
	print,'Skipping big header.'
	skip_lun, input_lun,head_length, /LINES
	;
	;do loop for time
	;
	for i=0L,23 do begin
	
		;print,'Skipping time header.'
		skip_lun, input_lun,1, /LINES
		;
		;do loop for species
		;
		for ispec=0L,num_species-1 do begin
		
			;do loop for layers
			for iver=0L,num_levels-1 do begin
			
			;print,'Skipping Species header.'
			skip_lun, input_lun,1, /LINES 
			
			readf,input_lun,c1,format=fmt
				
				c[0,0,iver,ispec,i]=c1
				
				; loop through the NABEL stations
				for station=0L,num_stations-1 do begin
				
					;ix=44.62
					;jy=73.26
					;z1(iver,ispec,i)=bilinear(c1,ix,jy)
					
					z[iver,ispec,i,station]=bilinear(c1,station_pos[0,station],station_pos[1,station])
					
				endfor
				
			endfor
		endfor
		
		; loop through the NABEL stations
		for station=0L,num_stations-1 do begin
		
			;ix=44.62
			;jy=73.26
			;z1(iver,ispec,i)=bilinear(c1,ix,jy)
			
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
