pro camxbound $
	,fmoz $
	,first_meteo_file $
	,metmodel $
	,zpfile $
	,outfile_bc $
	,nlevs $
	,mozart_specs $
	,camx_specs $
	,note $
	,xorg $
	,yorg $
	,delx $
	,dely $
	,ibdate $ 
	,doplots $
	,plot_base_dir $
	,MOZtime $
	,run_name $
	,dopng $
	,deleteps $
	,extra=extra
;
; Procedure camxbound.pro. Prepares the boundary conditions file for CAMx simulations
; using MOZART output data. See CAMx User's Guide for and overview of the format of the
; output file.
;
; Since MOZART data is provided 3-hourly, we use this feature of CAMx:
; "The time span of each set of boundary data records may be set arbitrarily; 
; e.g., a set of boundary conditions may be specified for a six hour span, 
; followed by a set spanning just an hour" (CAMx 4.51 Users guide)
;
;
; Iakovos Barmpadimos, PSI, February 2009
; with some changes by Daniel Oderbolz
; $Id$
;
;
; Parameters:
; fmoz - mozart input file for given day
; first_meteo_file - name of a MM5 output file of domain 1 for initial day
; metmodel - name of the meteo model ('MM5', 'WRF')
; zpfile - zp file of domain 1 for given day
; outfile_bc - name of the output file for boundary conditions (ASCII)
; nlevs - the number of vertical layers in CAMx
; mozart_specs - string array with mozart species to extract
; camx_specs - string array with camx species to extract (same order as mozart_specs of course)
; note - a comment that will be written to the output file 
; xorg - Grid x-origin at southwest corner
; yorg - Grid y-origin at southwest corner
; delx - x resolution in meters or degrees longitute (float)
; dely - y resolution in meters or degrees longitute (float)
; ibdate - date in YYDOY form so 02001 is the 1. Jan 2002
; doplots - if one, diagnostic plots will be written to plot_dir
; plot_base_dir - directory where to put the plots
; MOZtime - Mozart time step to plot
; run_name - run name, just used for plotting
; dopng - if 1, convert plots to png
; deleteps - if 1, ps files are deleted
; extra - structure to pass in additional arguments to increase certain species (iO3) or set them constant (cO3) (either one or the other. Unit: PPM)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

COMPILE_OPT IDL2

;; Print Revision
my_revision_string='$Id$'
my_revision_arr=strsplit(my_revision_string,' ',/EXTRACT)

print,my_revision_arr[1] + ' has revision ' + my_revision_arr[2]

; Set default parameters if needed
; 13 parameters are mandatory
if ( N_PARAMS() LT 14) then begin
	doplots=0
	plot_base_dir='~/@plot' 
	MOZtime=5 
	run_name='test' 
	dopng=1 
	deleteps=1 
endif

nspec=n_elements(mozart_specs)
nspec_c=n_elements(camx_specs)

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


; Create a hashtable to map species to a plotting max value
spec2max = obj_new('hashtable')


spec2max->add,'O3_VMR_inst',0.3
spec2max->add,'CO_VMR_inst',0.3
spec2max->add,'FORM_VMR_inst',0.005
spec2max->add,'NO_VMR_inst',0.0003
spec2max->add,'NO2_VMR_inst',0.003


; Time Interval between MOZART records in hours
time_interval_h = 3


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

						; Cleaning up some trash
						cmd = 'rm -f LONGICRS LONGICRS.hdr LATITCRS LATITCRS.hdr SIGMAH SIGMAH.hdr PSTARCRS PSTARCRS.hdr PP PP.hdr'
						spawn, cmd
	
					end
					
	'WRF' : begin
	
						print,'Reading long/lat from WRF ' + first_meteo_file + ' ...'
						restore,first_meteo_file
						
						meteoLon = longicrs
						meteoLat = latitcrs
						
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
mozart_pressure = FLTARR(ncolsmoz,nrowsmoz,nlevsmoz,ntime)

IF (n_elements(hyam) LT nlevsmoz || n_elements(hybm) LT nlevsmoz) then begin
	MESSAGE,"Inconsistent number of levels in hyam and/or hybm!"
endif

; it is possible that there are more levels in hyam and hybm:
hyam=hyam[0:nlevsmoz-1]
hybm=hybm[0:nlevsmoz-1]

; not elegant, later we might fix it using rebin/reform: <http://www.dfanning.com/tips/rebin_magic.html>
FOR t = 0, ntime - 1 DO BEGIN
	FOR level = 0,nlevsmoz -1 DO BEGIN
		; fill the current pressure level
		mozart_pressure[*,*,level,t] = Pa2mBar * (replicate(hyam[level],ncolsmoz,nrowsmoz) + mozart_surface_pressure[*,*,t] * replicate(hybm[level],ncolsmoz,nrowsmoz))
	endfor ; level
endfor ; time


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

; Definition of some variables required for the horizontal interpolation
dims = SIZE(meteoLon, /DIMENSIONS)
ncols = dims[0] - 1
nrows = dims[1] - 1
nhours = dims[3]
t_meteoLon = FLTARR(ncols, nrows, 1, nhours)
indexlon = FLTARR(ncols, nrows)
indexlat = FLTARR(ncols, nrows)

; The x and y dimensions are reduced by 1
; Actually, IDL does not care if the last two dimensions are missing
meteoLonr = meteoLon[0:ncols-1, 0:nrows-1, *, *]
meteoLatr = meteoLat[0:ncols-1, 0:nrows-1, *, *]

print,'Reading pressure file...'

; Read zp input file
; Note that free format reading might not work
; if the height and pressure values have such a number of digits
; that their field is full and different fields are not separated
; by space. In that case a format specification is required.
; This format depends on the converter that was used to produce the
; ascii height/pressure files and possibly on the platform.
OPENR, lun, zpfile, /GET_LUN
height = FLTARR(ncols,nrows,nlevs,24)
pres = FLTARR(ncols,nrows,nlevs,24)
height2d = FLTARR(ncols,nrows)
pres2d = FLTARR(ncols,nrows)
datemm5camx = 0.0
timemm5camx = 0

FOR t = 0, 23 DO BEGIN
	FOR k = 0,  nlevs - 1 DO BEGIN
		READF, lun, FORMAT = formatdt, datemm5camx, timemm5camx
		READF, lun, height2d
		READF, lun, FORMAT = formatdt, datemm5camx, timemm5camx
		READF, lun, pres2d
		
		height[*,*,k,t] = height2d
		pres[*,*,k,t] = pres2d
		
	ENDFOR
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
			t_meteoLon[i,j,0,1] = (meteoLonr[i,j,0,1] + 360.0) MOD 360.0
		endif else begin
			t_meteoLon[i,j,0,1] = meteoLonr[i,j,0,1] 
		endelse
		
		; The decimal grid indices in the MOZART grid, which coincide with
		; the MM5 cross grid points are calculated
		indexlon[i,j] = t_meteoLon[i,j,0,1] / lonstep
		indexlat[i,j] = (meteoLatr[i,j,0,1] / latstep - (0.5*latstep)) + (0.5 * nrowsmoz)
	ENDFOR ; rows
ENDFOR ; columns

mspec = STRARR(1,nspec)

; Define the header array
mspec[0,*] = camx_specs

; Horizontal interpolation
allspecinterp = FLTARR(ncols, nrows, nlevsmoz, nspec)
FOR ispec = 0, nspec - 1 DO BEGIN
	FOR k = 0, nlevsmoz - 1 DO BEGIN
		FOR t = 0, ntime - 1 DO BEGIN
			allspecred = allspecs[*,*,k,t,ispec]
			allspecinterp2d = INTERPOLATE(allspecred, indexlon, indexlat)
			allspecinterp[*,*,k,t,ispec] = allspecinterp2d
		ENDFOR ; time
	ENDFOR ; level
ENDFOR ; species

; horizontal interpolation of pressure
FOR k = 0, nlevsmoz - 1 DO BEGIN
	FOR t = 0, ntime - 1 DO BEGIN
		; interpolate just one pressure slice
		pressred = mozart_pressure[*,*,k,t]
		mozart_pressureinterp2D = INTERPOLATE(pressred, indexlon, indexlat)
		mozart_pressureinterp[*,*,k,t] = mozart_pressureinterp2D
	ENDFOR ; time
ENDFOR ; level


print,'Vertical interpolation...'

; Vertical interpolation, linear in pressure
; This is the target array
allspecinterpv = FLTARR(ncols,nrows,nlevs,ntime,nspec)

; Switches which will be used to ensure that warnings are printed only once
l = 1 
m = 1 

FOR ispec = 0, nspec - 1 DO BEGIN
	FOR t = 0, ntime - 1 DO BEGIN
		FOR i = 0, ncols - 1 DO BEGIN
			FOR j = 0, nrows - 1 DO BEGIN
				; we have the concentrations (that is our function of p)
				; in allspecinterp[i,j,ilev,t,ispec]
				; the pressures at which we want to sample are in pres[i,j,k,t * 3]
				; the pressures at which the function is defined are in mozart_pressureinterp
				interpolated = INTERPOL(allspecinterp[i,j,*,t,ispec],mozart_pressureinterp[i,j,*,t],pres[i,j,*,t * 3])

				; If there are CAMx pressure levels below the lowest MOZART level, those CAMx levels
				; get the value which corresponds to the lowest level of MOZART. This is done
				; in order to avoid negative, zero or too low values which occured as a result of
				; the interpolation below the lowest MOZART level.
				below_mozart_level = WHERE(pres[i,j,*] GT mozart_pressureinterp[i,j,0,t], count)
				IF count NE 0 THEN BEGIN
					interpolated[below_mozart_level] = allspecinterp[i,j,0,t,ispec]
					
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
					
				allspecinterpv[i,j,*,t,ispec]=interpolated
			ENDFOR ; rows (CAMx)
		ENDFOR ; columns (CAMx)
	ENDFOR ; time 
ENDFOR ; species (CAMx)

; Definition of variables for the boundary conditions file
name = 'BOUNDARY  '
ione = 1
btime = 00
; The end of the file is 00 of next day
iedate = ibdate + 1
etime = 00
rdum = 0.0
iutm = 0
nx = ncols
ny = nrows
nz = nlevs
idum = 0
ncell = [0, nrows, nrows, ncols, ncols]
icell = [0, 2, ncols-1, 2, nrows-1]

header_boundary = FLTARR(ncols*4, 5)

; Creation of an array which contains part of the header. The contents of this array
; are written later to the output file
FOR iedge = 1, 4 DO BEGIN
  FOR i = 0, ncell[iedge] - 1 DO BEGIN
    j = i * 4
    ind1 = j
    ind2 = j + 1
    ind3 = j + 2
    ind4 = j + 3
    header_boundary[ind1, iedge] = icell[iedge]
    header_boundary[ind2, iedge] = idum
    header_boundary[ind3, iedge] = idum
    header_boundary[ind4, iedge] = idum
    IF (i EQ 0) THEN header_boundary[ind1:ind4, iedge] = [0, 0, 0, 0]
    IF (i EQ ncell[iedge] - 1) THEN header_boundary[ind1:ind4, iedge] = [0, 0, 0, 0] 
  ENDFOR
ENDFOR

print,"******************************************"
print,'Some overview data of file ' + outfile_bc
print,'For each species and level, reporting '
print,'Min, Avg, Max in PPB [col_max, row_max,time_max,0,0]'

FOR ispec = 0, nspec - 1 DO BEGIN
	print,mspec[0,ispec]
	FOR k = 0, nlevs - 1 DO BEGIN
		; Get the maximum
		max_ppm = MAX(allspecinterpv[*,*,k,*,ispec])
		
		; Where is it? (Disabled, looks ugly)
		;ind_max_ppm = WHERE(allspecinterpv[*,*,k,*,ispec] EQ max_ppm)
		; Turn into array indices
		;arr_ind_max_ppm = ARRAY_INDICES(allspecinterpv,ind_max_ppm)
		; And this back into a string
		;str_arr_ind_max_ppm = STRTRIM(arr_ind_max_ppm,2)
		; Join to beatiful string
		;join_str_arr_ind_max_ppm = STRJOIN(str_arr_ind_max_ppm,',')

		print,strtrim(k,2)+': ',$
			MIN(allspecinterpv[*,*,k,*,ispec])*1000,$
			MEAN(REFORM(allspecinterpv[*,*,k,*,ispec],ncols*nrows*ntime))*1000,$
			max_ppm*1000
	ENDFOR ; Levels
ENDFOR ; Species

print,"******************************************"

; We create 2 transposed output arrays, one for West/East
; and one for South/North.
; This transpose is needed because we need to write the data
; in the level-first order. Otherwise, we would get the row or column first.

;print, 'We need to transpose the output array for W/E...,

; [x,y,z,time,spec] -> [z,y,x,time,spec]
allspecinterpvwe = TRANSPOSE(allspecinterpv, [2, 1, 0, 3, 4])

;print, 'We need to transpose the output array for S/N...,

; [x,y,z,time,spec] -> [z,x,y,time,spec]
allspecinterpvsn = TRANSPOSE(allspecinterpv, [2, 0, 1, 3, 4])

print,'Writing BOUNDARY data file ' + outfile_bc

; Write the data in the output file
OPENW, lun, outfile_bc, /GET_LUN

; Header
PRINTF, lun, name, note
line2 = '(I2, I3, I7, F6.0, I6, F6.0)'
PRINTF, lun, ione, nspec, ibdate, btime, iedate, etime, FORMAT = line2
line3 = '(F10.1, F11.1, I4, F10.1, F11.1, F7.0, F7.0, I4, I4, I4, I4, I4, F7.0, F7.0, F7.0)'
PRINTF, lun, rdum, rdum, iutm, xorg, yorg, delx, dely, nx, ny, nz, idum, idum, rdum, rdum, rdum, FORMAT = line3
line4 = '(4I5)'
PRINTF, lun, 0, 0, nx, ny, FORMAT = line4
PRINTF, lun, mspec
line19 = '(3I10)'
line20 = '(9I14)'
FOR iedge = 1, 4 DO BEGIN
  PRINTF, lun, ione, iedge, ncell[iedge], FORMAT = line19
  start = 0
  finish = 4 * ncell[iedge] - 1
  PRINTF, lun, header_boundary[start:finish, iedge],  FORMAT = line20
ENDFOR

; Time variant portion. Mozart provides 3-hourly output 
line127 = '(5X, I10, F10.2, I10, F10.2)'
line128 = '(6X, I4, A, 6X, I10)'
line129 = '(9E14.7)'

FOR t = 0, ntime - 1 DO BEGIN

	; n-Hourly data (Start of next record = end of last)
	
	; Start time of record
	t0 = t * time_interval_h
	; End time of record
	t1 = t0 + time_interval_h
	
	PRINTF, lun, ibdate, t0, ibdate, t1, FORMAT = line127
	FOR ispec = 0, nspec - 1 DO BEGIN
	
		iedge = 1 ; West
		PRINTF, lun, ione, mspec[ispec], iedge, FORMAT = line128
		PRINTF, lun, allspecinterpvwe[*,*,0,t,ispec], FORMAT = line129
		
		iedge = 2 ; East
		PRINTF, lun, ione, mspec[ispec], iedge, FORMAT = line128
		PRINTF, lun, allspecinterpvwe[*,*,ncols-1,t,ispec], FORMAT = line129

		iedge = 3 ; South
		PRINTF, lun, ione, mspec[ispec], iedge, FORMAT = line128
		PRINTF, lun, allspecinterpvsn[*,*,0,t,ispec], FORMAT = line129
		
		iedge = 4 ; North
		PRINTF, lun, ione, mspec[ispec], iedge, FORMAT = line128
		PRINTF, lun, allspecinterpvsn[*,*,nrows-1,t,ispec], FORMAT = line129
	ENDFOR
ENDFOR
  
FREE_LUN, lun

; Certain diagnostic plots are created. These are the plots of O3, CO, FORM, NO and NO2,
; at the lowest level, at 15:00 and at the first simulation day.
; Four different versions of the IC/BC data are plotted. The first one is the raw global MOZART data.
; The second one is the raw MOZART data over Europe. The third one is the European data after horizontal
; intepolation and the fourth one is the European data after vertical interpolation.
; Some parameters of the CONTOUR command (esp. max_value) may need to be adjusted


time24 = MOZtime * time_interval_h
time24 = STRCOMPRESS(time24, /REMOVE_ALL)
IF (doplots = 1) THEN BEGIN

	;;;;;; First the world plots using raw MOZART data are created
	; Array allspecs is rearranged so that a more intuitive version of
	; the world is displayed.
	lefthalf = FLTARR(96, 96, 60, 8, 5)
	righthalf = FLTARR(96, 96, 60, 8, 5)
	cuteallspecs = FLTARR(192, 96, 60, 8, 5)
	
	lefthalf = allspecs[0:95,*,*,*,*]
	righthalf = allspecs[96:191,*,*,*,*]
	cuteallspecs[0:95,*,*,*,*] = righthalf
	cuteallspecs[96:191,*,*,*,*] = lefthalf
	
	;ylatsouth = FINDGEN(49)
	;ylatsouth = REVERSE(-ylatsouth[1:*] * latstep)
	;ylatnorth = FINDGEN(48) * latstep
	;ylat = [ylatsouth, ylatnorth]
	xlonwest = FINDGEN(97)
	xlonwest = REVERSE(-xlonwest[1:*] * lonstep)
	xloneast = FINDGEN(96) * latstep
	xlon = [xlonwest, xloneast]
	ylatsouth = FINDGEN(48)
	ylatsouth = REVERSE(-ylatsouth * latstep)
	ylatnorth = FINDGEN(49) * latstep
	ylatnorth = ylatnorth[1:*]
	ylat = [ylatsouth, ylatnorth]

	; Plot settings
	; Sizes of an A4 sheet (Portrait, cm)
	a4_xsize_p = 18
	a4_ysize_p = 26
	; Sizes of an A4 sheet (Landscape, cm)
	a4_xsize_l = a4_ysize_p
	a4_ysize_l = a4_xsize_p
	SET_PLOT, 'PS'
	
	plotdir = plot_base_dir + '/' + 'ICBC-' + run_name + '/'
	SPAWN, 'mkdir -p ' + plotdir
	
	PRINT, 'Writing plots at ' + plotdir + ' directory.'


	for ispec=0,nspec-1 do begin

		DEVICE, FILENAME=plotdir+mozart_specs[ispec]+'world_'+ ibdate+'_' +time24+'.ps', /COLOR, XSIZE=a4_xsize_l, YSIZE=a4_ysize_l, XOFFSET=2, YOFFSET=2
		MAP_SET, /continents, /isotropic

		; Do we have a max-value?
		if (spec2max->iscontained(mozart_specs[ispec])) then begin
			CONTOUR, cuteallspecs[*,*,0,MOZtime,ispec], xlon, ylat, c_charsize=1, max_value=spec2max->get(mozart_specs[ispec]), /overplot, c_colors=[FSC_Color('purple'), FSC_Color('blue'), FSC_Color('green'), FSC_Color('orange'), FSC_Color('red')], nlevels=5, /isotropic, font=0, c_thick=2, c_labels=[1,1,1,1,1]
		endif else begin
			CONTOUR, cuteallspecs[*,*,0,MOZtime,ispec], xlon, ylat, c_charsize=1, /overplot, c_colors=[FSC_Color('purple'), FSC_Color('blue'), FSC_Color('green'), FSC_Color('orange'), FSC_Color('red')], nlevels=5, /isotropic, font=0, c_thick=2, c_labels=[1,1,1,1,1]
		endelse

		DEVICE, /CLOSE
	

	endfor


	;;;;;; Now the raw MOZART data for Europe are plotted
	europe = cuteallspecs[88:115, 65:80, *, *, *]
	eurlon = xlon[88:115]
	eurlat = ylat[65:80]

	for ispec=0,nspec-1 do begin
	
		DEVICE, FILENAME=plotdir+mozart_specs[ispec]+'eur_'+ ibdate+'_' +time24+'.ps', /COLOR, XSIZE=a4_xsize_l, YSIZE=a4_ysize_l, XOFFSET=2, YOFFSET=2
		MAP_SET, /continents, /isotropic, limit=[33,-12,70,23]
	
		; Do we have a max-value?
		if (spec2max->iscontained(mozart_specs[ispec])) then begin
			CONTOUR, europe[*,*,0,MOZtime,ispec], eurlon, eurlat,  c_charsize=1, max_value=spec2max->get(mozart_specs[ispec]), /overplot, c_colors=[FSC_Color('purple'), FSC_Color('blue'), FSC_Color('green'), FSC_Color('orange'), FSC_Color('red')], nlevels=5, /isotropic, font=0, c_thick=2, c_labels=[1,1,1,1,1]
		endif else begin
			CONTOUR, europe[*,*,0,MOZtime,ispec], eurlon, eurlat,  c_charsize=1, /overplot, c_colors=[FSC_Color('purple'), FSC_Color('blue'), FSC_Color('green'), FSC_Color('orange'), FSC_Color('red')], nlevels=5, /isotropic, font=0, c_thick=2, c_labels=[1,1,1,1,1]
		endelse

		DEVICE, /CLOSE
	

	endfor	
		

	;;;;;; Plots of the MOZART data after horizontal interpolation

	for ispec=0,nspec-1 do begin

		DEVICE, FILENAME=plotdir+mozart_specs[ispec]+'eur_horint_'+ ibdate+'_' +time24+'.ps', /COLOR, XSIZE=a4_xsize_l, YSIZE=a4_ysize_l, XOFFSET=2, YOFFSET=2
		MAP_SET, /continents, /isotropic, limit=[33,-12,70,23]

		; Do we have a max-value?
		if (spec2max->iscontained(mozart_specs[ispec])) then begin
			CONTOUR, allspecinterp[*,*,0,MOZtime,ispec], meteoLonr[*,*,0,1], meteoLatr[*,*,0,1], c_charsize=1, max_value=spec2max->get(mozart_specs[ispec]), /overplot, c_colors=[FSC_Color('purple'), FSC_Color('blue'), FSC_Color('green'), FSC_Color('orange'), FSC_Color('red')],  nlevels=5, /isotropic, font=0, c_thick=2, c_labels=[1,1,1,1,1]
		endif else begin
			CONTOUR, allspecinterp[*,*,0,MOZtime,ispec], meteoLonr[*,*,0,1], meteoLatr[*,*,0,1], c_charsize=1, /overplot, c_colors=[FSC_Color('purple'), FSC_Color('blue'), FSC_Color('green'), FSC_Color('orange'), FSC_Color('red')],  nlevels=5, /isotropic, font=0, c_thick=2, c_labels=[1,1,1,1,1]
		endelse

		DEVICE, /CLOSE
	

	endfor	


	;;;;;; Plots of the MOZART data after horizontal and vertical interpolation

	for ispec=0,nspec-1 do begin

		DEVICE, FILENAME=plotdir+mozart_specs[ispec]+'eur_verint_'+ ibdate+'_' +time24+'.ps', /COLOR, XSIZE=a4_xsize_l, YSIZE=a4_ysize_l, XOFFSET=2, YOFFSET=2
		MAP_SET, /continents, /isotropic, limit=[33,-12,70,23]

		; Do we have a max-value?
		if (spec2max->iscontained(mozart_specs[ispec])) then begin
			CONTOUR, allspecinterpv[*,*,0,MOZtime,ispec], meteoLonr[*,*,0,1], meteoLatr[*,*,0,1], c_charsize=1, max_value=spec2max->get(mozart_specs[ispec]), /overplot, c_colors=[FSC_Color('purple'), FSC_Color('blue'), FSC_Color('green'), FSC_Color('orange'), FSC_Color('red')],  nlevels=5, /isotropic, font=0, c_thick=2, c_labels=[1,1,1,1,1]
		endif else begin
			CONTOUR, allspecinterpv[*,*,0,MOZtime,ispec], meteoLonr[*,*,0,1], meteoLatr[*,*,0,1], c_charsize=1, /overplot, c_colors=[FSC_Color('purple'), FSC_Color('blue'), FSC_Color('green'), FSC_Color('orange'), FSC_Color('red')],  nlevels=5, /isotropic, font=0, c_thick=2, c_labels=[1,1,1,1,1]
		endelse

		DEVICE, /CLOSE
	

	endfor

	IF (dopng EQ 1) THEN BEGIN
		for ispec=0,nspec-1 do begin
			SPAWN, 'convert ' + plotdir+mozart_specs[ispec]+'world_'+ ibdate+'_' +time24+'.ps ' +plotdir+mozart_specs[ispec]+'world'+time24+'.png'
			SPAWN, 'convert ' + plotdir+mozart_specs[ispec]+'eur_'+ ibdate+'_' +time24+'.ps ' +plotdir+mozart_specs[ispec]+'eur'+time24+'.png'
			SPAWN, 'convert ' + plotdir+mozart_specs[ispec]+'eur_verint_'+ ibdate+'_' +time24+'.ps '+plotdir+mozart_specs[ispec]+'eur_verint'+time24+'.png' 
			SPAWN, 'convert ' + plotdir+mozart_specs[ispec]+'eur_horint_'+ ibdate+'_' +time24+'.ps ' + plotdir+mozart_specs[ispec]+'eur_horint'+time24+'.png'
		endfor
	ENDIF
	
	IF (dopng EQ 1 AND deleteps EQ 1) THEN BEGIN
		for ispec=0,nspec-1 do begin
			SPAWN, 'rm -f ' + plotdir+mozart_specs[ispec]+'world_'+ ibdate+'_' +time24+'.ps'
			SPAWN, 'rm -f ' + plotdir+mozart_specs[ispec]+'eur_'+ ibdate+'_' +time24+'.ps'
			SPAWN, 'rm -f ' + plotdir+mozart_specs[ispec]+'eur_verint_'+ ibdate+'_' +time24+'.ps'
			SPAWN, 'rm -f ' + plotdir+mozart_specs[ispec]+'eur_horint_'+ ibdate+'_' +time24+'.ps'
		endfor
	ENDIF


	SET_PLOT,'X'	

ENDIF


END

