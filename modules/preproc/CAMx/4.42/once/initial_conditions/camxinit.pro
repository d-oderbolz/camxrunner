pro camxinit $
	,fmoz $
	,first_meteo_file $
	,metmodel $
	,zpfile $
	,outfile_ic $
	,outfile_tc $
	,nlevs $
	,mozart_specs $
	,camx_specs $
	,note $
	,xorg $
	,yorg $
	,delx $
	,dely $
	,ibdate $
	,extra=extra

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procedure camxinit.pro. Prepares an ASCII version of the initial conditions file for CAMx simulations
; using MOZART output data. See CAMx User's Guide for and overview of the format of the
; output file. Currently, the data if the 0th time is read.
; Also creates a topconc file (is fed ASCII to CAMx)
;
; Iakovos Barmpadimos, PSI, February 2009
; with some changes by Daniel Oderbolz
; $Id$
;
; Parameters:
; fmoz - mozart input file for initial day
; first_meteo_file - name of a MM5 output file of domain 1 for initial day or a WRF IDL savefile (needed only for lat/lon information)
; metmodel - name of the meteo model ('MM5', 'WRF')
; zpfile - zp file of domain 1 for initial day
; outfile_ic - name of the output file for initial conditions (ASCII)
; outfile_tc - name of the output file for top concentrations
; nlevs - the number of vertical layers in CAMx
; mozart_specs - string array with mozart species to extract
; camx_specs - string array with camx species to extract (same order as mozart_specs of course)
; note - a comment that will be written to the output file 
; xorg - Grid x-origin at southwest corner
; yorg - Grid y-origin at southwest corner
; delx - x resolution in meters or degrees longitute (float)
; dely - y resolution in meters or degrees longitute (float)
; ibdate - initial date in YYDOY form so 02001 is the 1. Jan 2002
; extra - structure to pass in additional arguments to increase certain species (iO3) or set them constant (cO3) (either one or the other. Unit: PPM)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

COMPILE_OPT IDL2

;; Print Revision
my_revision_string='$Id$'
my_revision_arr=strsplit(my_revision_string,' ',/EXTRACT)

print,my_revision_arr[1] + ' has revision ' + my_revision_arr[2]

nspec=n_elements(mozart_specs)
nspec_c=n_elements(camx_specs)

; this is the time index we consider.
; later, we may use a smarter approach to find this index
read_time_index = 0

if ( nspec NE nspec_c) then message,"Must get as many mozart as camx species!"
if ( nspec EQ 0) then message,"Must get more than 0 species to extract!"

;;;;;;;;;;;;;;;;;;;;
; Extract stuff from extra
;;;;;;;;;;;;;;;;;;;;

; This variable holds a 'none' if data is taken verbatim, 'constant' if we use constant data or 'increment' if we add an offset
data_modification='none'

if ( N_tags(extra) GT 0 ) then begin
	; we got an extra structure
	; the tags look like this (for ozone): cO3 or iO3
	
	; fetch list 
	tags = Tag_names(extra)
	
	; Make sure we do not mix i and c ones
	for i=0,n_elements(tags)-1 do begin
	
		; The first character is the type of modification
		type=strupcase(strmid(tags[i],0,1))
		
		case type of
			'I' : begin
					if (data_modification EQ 'none') then begin
						data_modification='increment'
						data_modification_prefix='I'
					endif else begin
						if (data_modification EQ 'constant') then begin
							message,"You cannot use constant and increment together!"
						endif
					endelse
				  end
				
			'C' : begin
					if (data_modification EQ 'none') then begin
						data_modification='constant'
						data_modification_prefix='C'
					endif else begin
						if (data_modification EQ 'increment') then begin
							message,"You cannot use constant and increment together!"
						endif
					endelse
				  end
				
			else : message,"Unknown type of modification: " + type
		endcase

	endfor
endif

; Correction factor to convert Volume mixing ratio to ppm
vmr2ppm=1E6

; Clean up old headers produced by rv3
cmd = 'rm -f LONGICRS LONGICRS.hdr LATITCRS LATITCRS.hdr SIGMAH SIGMAH.hdr PSTARCRS PSTARCRS.hdr PP PP.hdr'
spawn, cmd

case metmodel of 

	'MM5' : begin
	
						print,'Reading long/lat from MM5 ' + first_meteo_file + ' ...'

						; Read lon-lat data from MM5
						rv3, first_meteo_file, 'longicrs', meteoLon, bhi, bhr
						rv3, first_meteo_file, 'latitcrs', meteoLat, bhi, bhr

						; Interchange the first two dimensions
						; [y,x,z,time] -> [x,y,z,time]
						meteoLon = TRANSPOSE(meteoLon, [1, 0, 2, 3])
						meteoLat = TRANSPOSE(meteoLat, [1, 0, 2, 3])
						
						; cut off z and time
						meteoLon=REFORM(meteoLon[*,*,0,1])
						meteoLat=REFORM(meteoLat[*,*,0,1])
					
						; Cleaning up some trash
						cmd = 'rm -f LONGICRS LONGICRS.hdr LATITCRS LATITCRS.hdr SIGMAH SIGMAH.hdr PSTARCRS PSTARCRS.hdr PP PP.hdr'
						spawn, cmd
						
						; Definition of some variables required for the horizontal interpolation
						dims = SIZE(meteoLon, /DIMENSIONS)
						; get rid of extra rows/columns
						ncols = dims[0] - 1
						nrows = dims[1] - 1
	
					end
					
	'WRF' : begin
	
						print,'Reading long/lat from WRF ' + first_meteo_file + ' ...'
						restore,first_meteo_file
						
						meteoLon = longicrs
						meteoLat = latitcrs
						
						; Definition of some variables required for the horizontal interpolation
						dims = SIZE(meteoLon, /DIMENSIONS)
						; get rid of extra rows/columns
						ncols = dims[0]
						nrows = dims[1]

					end

endcase



; Read MOZART input file.

print,'Reading netCDF file ' + fmoz + ' ...'

ncid = NCDF_OPEN(fmoz)            ; Open The NetCDF file

; These variables are needed in any case
NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'T') , T      ; Read in variable 'T'

; Get lon and lat to calculate step for horizontal interpolation
NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'lon') , lonmoz
NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'lat') , latmoz


; Reading the dimensions of MOZART output
dimsmoz = SIZE(T, /DIMENSIONS)
ncolsmoz = dimsmoz[0]
nrowsmoz = dimsmoz[1]
nlevsmoz = dimsmoz[2]
ntime = dimsmoz[3]

; we need to re-construct the pressure from the surface pressure in PS
; and 2 coefficients.
; See <http://www.ecmwf.int/products/data/technical/model_levels/model_def_60.html> 
; and <http://www.ecmwf.int/research/ifsdocs/DYNAMICS/Chap2_Discretization4.html#961180> for details

;Input is in Pascal, output is in mBar

Pa2mBar = 0.01

NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'PS') , mozart_surface_pressure  ; in Pa, ncols*nrows*ntime
NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'hyam') , hyam
NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'hybm') , hybm

; directly reverse hyam and hymb that lower levels are at lower indexes
hyam = REVERSE(hyam)
hybm = REVERSE(hybm)

; this variable will hold the pressures converted to mb
mozart_pressure = FLTARR(ncolsmoz,nrowsmoz,nlevsmoz)

IF (n_elements(hyam) LT nlevsmoz || n_elements(hybm) LT nlevsmoz) then begin
	MESSAGE,"Inconsistent number of levels in hyam and/or hybm!"
endif

; it is possible that there are more levels in hyam and hybm:
hyam=hyam[0:nlevsmoz-1]
hybm=hybm[0:nlevsmoz-1]

; not elegant, later we might fix it using rebin/reform: <http://www.dfanning.com/tips/rebin_magic.html>
FOR level = 0,nlevsmoz -1 DO BEGIN
	; fill the current pressure level
	mozart_pressure[*,*,level] = Pa2mBar * (replicate(hyam[level],ncolsmoz,nrowsmoz) + (mozart_surface_pressure[*,*,read_time_index] * replicate(hybm[level],ncolsmoz,nrowsmoz)))
endfor ; level


; In order to be able to loop over species, an array containing all of them is created
allspecs = FLTARR(ncolsmoz, nrowsmoz, nlevsmoz, ntime, nspec)

; Loop through the MOZART species and loading them
; we ASSUME that all are gaseous and given as Volume mixing ratio
for ispec=0,nspec-1 do begin

	; Do we have such a variable?
	varid = NCDF_VARID(ncid, mozart_specs[ispec])
	
	if ( varid EQ -1 ) then message,"The MOZART file " + fmoz + " does not contain " + mozart_specs[ispec]

	print,"Loading " + mozart_specs[ispec] + " (CAMx species " + camx_specs[ispec] + ")"

	NCDF_VARGET, ncid, varid ,dummy
	
	; Now it depends on whether we need to modify the data or not
	case data_modification of
	
		'none' : begin
		 			; We also need to reverse all concentrations (3rd dimension, here 1 based)
					; Also we convert to PPM
					allspecs[*,*,*,*,ispec] = reverse(dummy,3) * vmr2ppm
				end
	
		'constant' : begin
						
						; Find the correspondig entry in the extra structure
						; it is called cO3 for Ozone (CAMx convention)
						Tag_Num = where( Tags EQ data_modification_prefix + camx_specs[ispec] )
						Tag_Num = Tag_Num[0]
						if (Tag_Num LT 0) then begin
						
							; Tag not found, no modification
							print,"You specified constant values for some species but not for " + camx_specs[ispec]
						
							; We also need to reverse all concentrations (3rd dimension, here 1 based)
							; Also we convert to PPM
							allspecs[*,*,*,*,ispec] = reverse(dummy,3) * vmr2ppm
							
						endif else begin
						
							; Found a tag, set the values correspondingly
						
							constant_dummy = extra.(Tag_Num)
							print,'WRN: The species ' + camx_specs[ispec] + ' will be set to ' + strtrim(constant_dummy,2) + 'PPM everywhere!'
						
							allspecs[*,*,*,*,ispec] = constant_dummy
						endelse
						
					 end
					 
		'increment' : begin
		
						; Find the correspondig entry in the extra structure
						; it is called iO3 for Ozone (CAMx convention)
						Tag_Num = where( Tags EQ data_modification_prefix + camx_specs[ispec] )
						Tag_Num = Tag_Num[0]
						if (Tag_Num LT 0) then begin
							; Tag not found
							increment_dummy = 0
						endif else begin
							; Tag found, get the data 
							increment_dummy = extra.(Tag_Num)
							print,'WRN: The species ' + camx_specs[ispec] + ' will be increased by ' + strtrim(increment_dummy,2) + 'PPM everywhere!'
						endelse
		
						; We also need to reverse all concentrations (3rd dimension, here 1 based)
						; Also we convert to PPM
						allspecs[*,*,*,*,ispec] = (reverse(dummy,3) * vmr2ppm) + increment_dummy
					  end
	endcase
	
	
endfor

NCDF_CLOSE, ncid      ; Close the NetCDF file

print,'netCDF file read sucessfully.'

t_meteoLon = FLTARR(ncols, nrows)
indexlon = FLTARR(ncols, nrows)
indexlat = FLTARR(ncols, nrows)

; The x and y dimensions are reduced by 1 to later
; correct the range of angles
meteoLonr = meteoLon[0:ncols-1, 0:nrows-1]
meteoLatr = meteoLat[0:ncols-1, 0:nrows-1]

; Here we store the horizontally interpolated mozart pressure field
mozart_pressureinterp = FLTARR(ncols,nrows,nlevsmoz)

print,'Reading pressure file...'

; Read zp input file
; Note that free format reading might not work
; if the height and pressure values have such a number of digits
; that their field is full and different fields are not separated
; by space. In that case a format specification is required.
; This format depends on the converter that was used to produce the
; ascii height/pressure files and possibly on the platform.
OPENR, lun, zpfile, /GET_LUN
height = FLTARR(ncols, nrows, nlevs)
pres = FLTARR(ncols, nrows, nlevs)
height2d = FLTARR(ncols,nrows)
pres2d = FLTARR(ncols,nrows)
datemm5camx = 0.0
timemm5camx = 0

FOR k = 0, nlevs - 1 DO BEGIN
  READF, lun, FORMAT = formatdt, datemm5camx, timemm5camx
  READF, lun, height2d
  READF, lun, FORMAT = formatdt, datemm5camx, timemm5camx
  READF, lun, pres2d
    
  height[*,*,k] = height2d
  pres[*,*,k] = pres2d
ENDFOR

FREE_LUN, lun

print,'Horizontal Interpolation...'

; Calculation of the longitude and latitude steps of the MOZART grid
; we assume that the global CTM runs on a grid with constant lat/lon spacing
; we do not necessarily get a global file.

lonstep = abs(lonmoz[1] - lonmoz[0])
latstep = abs(latmoz[1] - latmoz[0])

; Creation of the grid indices (indexlon,indexlat) for horizontal interpolation
FOR i = 0, ncols - 1 DO BEGIN
	FOR j = 0, nrows - 1 DO BEGIN
	
		; The MM5 longitude is converted from the [-180,180] range to the
		; [0,360] range for compatibility with the MOZART longtitude, 
		; but ONLY if MOZART offers the full globe (otherwise, its longitude is also given in [-180,180])
		
		if (abs(lonmoz[n_elements(lonmoz) - 1] - lonmoz[0]) + lonstep EQ 360 ) then begin
			t_meteoLon[i,j] = (meteoLonr[i,j] + 360.0) MOD 360.0
		endif else begin
			t_meteoLon[i,j] = meteoLonr[i,j] 
		endelse
		
		; The decimal grid indices in the MOZART grid, which coincide with
		; the MM5 cross grid points are calculated
		indexlon[i,j] = t_meteoLon[i,j] / lonstep
		indexlat[i,j] = (meteoLatr[i,j] / latstep - (0.5*latstep)) + (0.5 * nrowsmoz)
	ENDFOR ; rows
ENDFOR ; columns

mspec = STRARR(1,nspec)

; Define the header array
mspec[0,*] = camx_specs

; Horizontal interpolation
allspecinterp = FLTARR(ncols, nrows, nlevsmoz, nspec)
FOR ispec = 0, nspec - 1 DO BEGIN
	FOR k = 0, nlevsmoz - 1 DO BEGIN
    allspecred = allspecs[*,*,k,read_time_index,ispec]
    allspecinterp2d = INTERPOLATE(allspecred, indexlon, indexlat)
    allspecinterp[*,*,k,ispec] = allspecinterp2d
  ENDFOR ; level
ENDFOR ; species
; horizontal interpolation of pressure
FOR k = 0, nlevsmoz - 1 DO BEGIN
	FOR t = 0, ntime - 1 DO BEGIN
		; interpolate just one pressure slice
		pressred = mozart_pressure[*,*,k]
		mozart_pressureinterp2D = INTERPOLATE(pressred, indexlon, indexlat)
		mozart_pressureinterp[*,*,k] = mozart_pressureinterp2D
	ENDFOR ; time
ENDFOR ; level


print,'Vertical interpolation...'

; Vertical interpolation, linear in pressure
; This is the target array
allspecinterpv = FLTARR(ncols,nrows,nlevs,nspec)
; Switches which will be used to ensure that warnings are printed only once

l = 1 
m = 1 

FOR ispec = 0, nspec - 1 DO BEGIN
	FOR i = 0, ncols - 1 DO BEGIN
		FOR j = 0, nrows - 1 DO BEGIN
			; we have the concentrations (that is our function of p)
			; in allspecinterp[i,j,ilev,ispec]
			; the pressures at which we want to sample are in pres[i,j,k]
			; the pressures at which the function is defined are in mozart_pressureinterp
			interpolated = INTERPOL(allspecinterp[i,j,*,ispec],mozart_pressureinterp[i,j,*],pres[i,j,*])
			
			; If there are CAMx pressure levels below the lowest MOZART level, those CAMx levels
			; get the value which corresponds to the lowest level of MOZART. This is done
			; in order to avoid negative, zero or too low values which occured as a result of
			; the interpolation below the lowest MOZART level.
			below_mozart_level = WHERE(pres[i,j,*] GT mozart_pressureinterp[i,j,0], count)
			IF count NE 0 THEN BEGIN
				interpolated[below_mozart_level] = allspecinterp[i,j,0,ispec]
				
				IF l EQ 1 THEN print,'WRN: Some negative values below the lowest MOZART level were replaced by the value of the lowest MOZART level.'
				l = 0
			ENDIF

			; Sometimes negative or too low values occur above the lowest MOZART level as well.
			; This can happen when the values of a pollutant are low and decrease abruptly between two
			; consecutive MOZART levels. When negative values occur, they are replaced by the first positive
			; interpolated value which is encountered above the levels where the negative values occured.
			ind_negative = WHERE(interpolated LT 0, count)
			IF count NE 0 THEN BEGIN
				interpolated[ind_negative] = interpolated[ind_negative[N_ELEMENTS(ind_negative)-1] + 1]
				
				IF m EQ 1 THEN print,'WRN: Some negative values which occured above the lowest MOZART level were replaced by the first positive interpolated value which is encountered above the negative values.'
				m = 0
			ENDIF
		
			allspecinterpv[i,j,*,ispec]=interpolated
		ENDFOR ; rows (CAMx)
	ENDFOR ; columns (CAMx)
ENDFOR ; species (CAMx)

; Definition of variables for the initial conditions file
name = 'AIRQUALITY'
ione = 1

iedate = ibdate

btime = 00
etime = 24
rdum = 0.0
iutm = 0
nx = ncols
ny = nrows
nz = nlevs
idum = 0

print,'******************************************'
print,'Some overview data of file ' + outfile_ic
print,'For each species and level, reporting '
print,'Min, Avg, Max converted to PPB (data written is in PPM)'

FOR ispec = 0, nspec - 1 DO BEGIN
	print,mspec[0,ispec]
	FOR k = 0, nlevs - 1 DO BEGIN
		; Get the maximum
		max_ppm = MAX(allspecinterpv[*,*,k,ispec])
		
		; Where is it? (Disabled, looks ugly)
		;ind_max_ppm = WHERE(allspecinterpv[*,*,k,ispec] EQ max_ppm)
		; Turn into array indices
		;arr_ind_max_ppm = ARRAY_INDICES(allspecinterpv,ind_max_ppm)
		; And this back into a string
		;str_arr_ind_max_ppm = STRTRIM(arr_ind_max_ppm,2)
		; Join to beatiful string
		;join_str_arr_ind_max_ppm = STRJOIN(str_arr_ind_max_ppm,',')
	
		print,strtrim(k,2)+': ',$
			MIN(allspecinterpv[*,*,k,ispec])*1000,$
			MEAN(REFORM(allspecinterpv[*,*,k,ispec],ncols*nrows))*1000,$
			max_ppm*1000
  ENDFOR
ENDFOR

print,'******************************************'

print,'Writing INITIAL data file ' + outfile_ic

; Write the data into the ic output file
OPENW, lun, outfile_ic, /GET_LUN

; Header 
PRINTF, lun, name, note
line2 = '(I2, I3, I7, F6.0, I6, F6.0)'
PRINTF, lun, ione, nspec, ibdate, btime, iedate, etime, FORMAT = line2
line3 = '(F10.4,1X,F10.4,1X,I3,F10.4,1X,F10.4,1X,2F7.4,5I4,3F7.0)'
PRINTF, lun, rdum, rdum, iutm, xorg, yorg, delx, dely, nx, ny, nz, idum, idum, rdum, rdum, rdum, FORMAT = line3
line4 = '(4I5)'
PRINTF, lun, 0, 0, nx, ny, FORMAT = line4
PRINTF, lun, mspec

; 'Time variant portion'
line19 = '(5X, I10, F10.2, I10, F10.2)'
line20 = '(I4, A)'
line21 = '(9E14.7)'
PRINTF, lun, ibdate, btime, iedate, etime, FORMAT = line19
FOR ispec = 0, nspec - 1 DO BEGIN
  FOR k = 0, nlevs - 1 DO BEGIN
    PRINTF, lun, ione, mspec[ispec], FORMAT = line20
    PRINTF, lun, allspecinterpv[*,*,k,ispec], FORMAT = line21
  ENDFOR
ENDFOR


FREE_LUN, lun

;; Topconc might have been produced using other methods,
;; so check if its already there

; first test for presence of the topconc file
IF (FILE_TEST(outfile_tc) EQ 0) THEN BEGIN

  ; file not present - prepare new one
  print,'Writing TOPCONC data file ' + outfile_tc

  ;;;;;;;;;;;  Prepare the TOPCONC file
  ; Now we average all species in the highest layer
  
  ; Define array to hold values in the top level
  top_arr = FLTARR(ncols,nrows,nspec)
  
  ; Get top level data
  top_arr[*,*,*] = allspecinterpv[*,*,nlevs - 1,*]
  ; This ^ silly construct is needed, else you get an additional dimension!!
  
  ; This is the target array
  conc = FLTARR(nspec)
  
  FOR ispec = 0, nspec - 1 DO BEGIN
  	; Might be possible to do this smarter
  	conc[ispec] = mean(reform(top_arr[*,*,ispec], (nrows * ncols) ))
  ENDFOR
  
  ; Write the data to the output file
  OPENW, lun, outfile_tc, /GET_LUN
  
  tc_format = '(A-9,F-12.9)'
  
  FOR ispec = 0, nspec - 1 DO BEGIN
  	PRINTF, lun, mspec[ispec],conc[ispec], FORMAT = tc_format
  ENDFOR
ENDIF ELSE BEGIN
  print,'File ' + outfile_tc + ' exists, will not overwrite it.'
ENDELSE

print,'INITIAL Conditions & TOCONC prepared.'

END

