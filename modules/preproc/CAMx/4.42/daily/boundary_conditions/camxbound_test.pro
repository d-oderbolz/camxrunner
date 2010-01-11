pro camxbound,fmoz,fln,mm5camxinfile,outfile_bc,nlevs,nspec,note,xorg,yorg,delx,dely,ibdate
;
; Procedure camxbound_test.pro: Tests whether procedure camxbound.pro procuces the right output.
;
; In order to carry out the testing, an matrix is created which has the same dimensions as the matrix that
; contains the MOZART input data. Every element of this matrix is set equal to 10.0. Then, the matrix
; is treated (mainly interpolated) in exactly the same way as MOZART input data would be treated in camxbound.pro.
; If the treatment is correct, the resulting interpolated matrix should have all its elements equal to 10.0.
; When this is not the case, an error message appears.
; 
;
; Iakovos Barmpadimos, PSI, February 2009
; with some changes by Daniel Oderbolz
; $Id: camxbound.pro 1804 2009-09-22 08:45:16Z oderbolz $
;
;
; Parameters:
; fmoz - mozart input file for given day
; fln - name of a MM5 output file of domain 1 for initial day
; mm5camxinfile - zp file of domain 1 for given day
; outfile_bc - name of the output file for boundary conditions (ASCII)
; nlevs - the number of vertical layers in CAMx
; nspec - the number of MOZART species to extract
; note - a comment that will be written to the output file 
; xorg - Grid x-origin at southwest corner
; yorg - Grid y-origin at southwest corner
; delx - x resolution in meters or degrees longitute (float)
; dely - y resolution in meters or degrees longitute (float)
; ibdate - date in YYDOY form so 02001 is the 1. Jan 2002
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Time Interval between MOZART records in hours
time_interval_h = 3

; Report my revision
my_revision_string='$Id: camxbound.pro 1804 2009-09-22 08:45:16Z oderbolz $'
my_revision_arr=strsplit(my_revision_string,' ',/EXTRACT)

print,my_revision_arr[1] + ' has revision ' + my_revision_arr[2]


; Correction factor to convert Volume mixing ratio to ppm
vmr2ppm=1E6

; Clean up old headers produced by rv3
cmd = 'rm -f LONGICRS LONGICRS.hdr LATITCRS LATITCRS.hdr SIGMAH SIGMAH.hdr PSTARCRS PSTARCRS.hdr PP PP.hdr'
spawn, cmd

print,'Reading long/lat from MM5 ' + fln + ' ..."

; Read lon-lat from MM5
rv3, fln, 'longicrs', mm5lon, bhi, bhr
rv3, fln, 'latitcrs', mm5lat, bhi, bhr

; Interchange the first two dimensions
; [y,x,z,time] -> [x,y,z,time]
mm5lon = TRANSPOSE(mm5lon, [1, 0, 2, 3])
mm5lat = TRANSPOSE(mm5lat, [1, 0, 2, 3])

; Read MOZART input file. We no longer use cdf2idl
; but use the commands needed directly. Faster because we read less stuff.
; Less dynamic: if the list of variable changes, we have to rewrite this.

print,'Reading netCDF file ' + fmoz + ' ..."

ncid = NCDF_OPEN(fmoz)            ; Open The NetCDF file

NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'lev') , lev      ; Read in variable 'lev'
NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'T') , T      ; Read in variable 'T'
NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'O3_VMR_inst') , O3_VMR_inst      ; Read in variable 'O3_VMR_inst'
NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'CO_VMR_inst') , CO_VMR_inst      ; Read in variable 'CO_VMR_inst'
NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'CH2O_VMR_inst') , CH2O_VMR_inst      ; Read in variable 'CH2O_VMR_inst'
NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'NO_VMR_inst') , NO_VMR_inst      ; Read in variable 'NO_VMR_inst'
NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'NO2_VMR_inst') , NO2_VMR_inst      ; Read in variable 'NO2_VMR_inst'

; Reversing concentrations so that ground level is at 0 index
; Attention: Dimensions are indexed 1-based!!
lev = REVERSE(lev)
O3_VMR_inst = REVERSE(O3_VMR_inst,3)
CO_VMR_inst = REVERSE(CO_VMR_inst,3)
CH2O_VMR_inst = REVERSE(CH2O_VMR_inst,3)
NO_VMR_inst = REVERSE(NO_VMR_inst,3)
NO2_VMR_inst = REVERSE(NO2_VMR_inst,3)

; The last few are only needed if we read more than 5 variables
; This is needed because prior to 2004, MOZART only had the first 5 species

if (nspec GT 5) then begin
	NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'HNO3_VMR_inst') , HNO3_VMR_inst      ; Read in variable 'HNO3_VMR_inst'
	NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'HO2NO2_VMR_inst') , HO2NO2_VMR_inst      ; Read in variable 'HO2NO2_VMR_inst'
	NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'H2O2_VMR_inst') , H2O2_VMR_inst      ; Read in variable 'H2O2_VMR_inst'
	NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'PAN_VMR_inst') , PAN_VMR_inst      ; Read in variable 'PAN_VMR_inst'
	NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'CH4_VMR_inst') , CH4_VMR_inst      ; Read in variable 'CH4_VMR_inst'
	NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'CH3CHO_VMR_inst') , CH3CHO_VMR_inst      ; Read in variable 'CH3CHO_VMR_inst'
	NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'C2H6_VMR_inst') , C2H6_VMR_inst      ; Read in variable 'C2H6_VMR_inst'
	NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'ISOP_VMR_inst') , ISOP_VMR_inst      ; Read in variable 'ISOP_VMR_inst'
	NCDF_VARGET, ncid,  NCDF_VARID(ncid, 'TOLUENE_VMR_inst') , TOLUENE_VMR_inst      ; Read in variable 'TOLUENE_VMR_inst'

; Reversing concentrations so that ground level is at 0 index
HNO3_VMR_inst = REVERSE(HNO3_VMR_inst,3)
HO2NO2_VMR_inst = REVERSE(HO2NO2_VMR_inst,3)
H2O2_VMR_inst = REVERSE(H2O2_VMR_inst,3)
PAN_VMR_inst = REVERSE(PAN_VMR_inst,3)
CH4_VMR_inst = REVERSE(CH4_VMR_inst,3)
CH3CHO_VMR_inst = REVERSE(CH3CHO_VMR_inst,3)
C2H6_VMR_inst = REVERSE(C2H6_VMR_inst,3)
ISOP_VMR_inst = REVERSE(ISOP_VMR_inst,3)
TOLUENE_VMR_inst = REVERSE(TOLUENE_VMR_inst,3)

endif

NCDF_CLOSE, ncid      ; Close the NetCDF file

print,'netCDF file read sucessfully.'

; We need to convert VMR to PPM for each species (there are certainly nicer approaches)
print,'Converting VMR to PPM'

O3_VMR_inst = O3_VMR_inst * vmr2ppm
CO_VMR_inst = CO_VMR_inst * vmr2ppm
CH2O_VMR_inst = CH2O_VMR_inst * vmr2ppm
NO_VMR_inst = NO_VMR_inst * vmr2ppm
NO2_VMR_inst = NO2_VMR_inst * vmr2ppm


if (nspec GT 5) then begin
	HNO3_VMR_inst = HNO3_VMR_inst * vmr2ppm
	HO2NO2_VMR_inst = HO2NO2_VMR_inst * vmr2ppm
	H2O2_VMR_inst = H2O2_VMR_inst * vmr2ppm
	PAN_VMR_inst = PAN_VMR_inst * vmr2ppm
	CH4_VMR_inst = CH4_VMR_inst * vmr2ppm
	CH3CHO_VMR_inst = CH3CHO_VMR_inst * vmr2ppm
	C2H6_VMR_inst = C2H6_VMR_inst * vmr2ppm
	ISOP_VMR_inst = ISOP_VMR_inst * vmr2ppm
	TOLUENE_VMR_inst = TOLUENE_VMR_inst * vmr2ppm
endif

; Definition of some variables required for the horizontal interpolation
dims = SIZE(mm5lon, /DIMENSIONS)
ncols = dims[0] - 1
nrows = dims[1] - 1
nhours = dims[3]
t_mm5lon = FLTARR(ncols, nrows, 1, nhours)
indexlon = FLTARR(ncols, nrows)
indexlat = FLTARR(ncols, nrows)

; Reading the dimensions of MOZART output
dimsmoz = SIZE(T, /DIMENSIONS)
ncolsmoz = dimsmoz[0]
nrowsmoz = dimsmoz[1]
nlevsmoz = dimsmoz[2]
ntime = dimsmoz[3]

; The x and y dimensions are reduced by 1
mm5lonr = mm5lon[0:ncols-1, 0:nrows-1, *, *]
mm5latr = mm5lat[0:ncols-1, 0:nrows-1, *, *]

; Cleaning up some trash
cmd = 'rm -f LONGICRS LONGICRS.hdr LATITCRS LATITCRS.hdr SIGMAH SIGMAH.hdr PSTARCRS PSTARCRS.hdr PP PP.hdr'
spawn, cmd

print,'Reading MM5 output...'

; Read MM5CAMx input file
; Note that free format reading might not work
; if the height and pressure values have such a number of digits
; that their field is full and different fields are not separated
; by space. In that case a format specification is required.
; This format depends on the converter that was used to produce the
; ascii height/pressure files and possibly on the platform.
OPENR, lun, mm5camxinfile, /GET_LUN
height = FLTARR(ncols,nrows,nlevs,24)
pres = FLTARR(ncols,nrows,nlevs,24)
height2d = FLTARR(ncols,nrows)
pres2d = FLTARR(ncols,nrows)
datemm5camx = 0.0
timemm5camx = 0

FOR t = 0, 23 DO BEGIN
	FOR k = 0, 13 DO BEGIN
		READF, lun, FORMAT = formatdt, datemm5camx, timemm5camx
		READF, lun, height2d
		READF, lun, FORMAT = formatdt, datemm5camx, timemm5camx
		READF, lun, pres2d
		
		height[*,*,k,t] = height2d
		pres[*,*,k,t] = pres2d
		
	ENDFOR
ENDFOR

FREE_LUN, lun

; In order to be able to loop over species, an array containing all of them is created
allspecs = FLTARR(ncolsmoz, nrowsmoz, nlevsmoz, ntime, nspec)

mspec = STRARR(1,nspec)


; Here we call an external procedure that was created by CAMxRunner
; to perform the renaming of the variables and to create the list of species

print,'Mapping species...'

@remap_species

; In this extra line, which does not exist in camxbound.pro, all elements of the matrix that
; should contain the MOZART data are set equal to 10.0 for testing purposes (see
; description at the beginning).
allspecs[*,*,*,*,*] = 10.0

print,'Horizontal Interpolation...'

; Creation of the grid indices (indexlon,indexlat) for horizontal interpolation
FOR i = 0, ncols - 1 DO BEGIN
	FOR j = 0, nrows - 1 DO BEGIN
		; The MM5 longtitude is converted from the [-180,180] range to the
		; [0,360] range for compatibility with the MOZART longtitude
		t_mm5lon[i,j,0,1] = (mm5lonr[i,j,0,1] + 360.0) MOD 360.0
		
		; The decimal grid indices in the MOZART grid, which coincide with
		; the MM5 cross grid points are calculated
		indexlon[i,j] = t_mm5lon[i,j,0,1] / 1.87500  ; 1.875 is the lon step in MOZART
		indexlat[i,j] = ((mm5latr[i,j,0,1] - 0.947368) / 1.89474) + 48 ; 1.89474 is the lat step in MOZART
	ENDFOR
ENDFOR

; Horizontal interpolation
allspecinterp = FLTARR(ncols, nrows, nlevsmoz, ntime, nspec)
FOR ispec = 0, nspec - 1 DO BEGIN
	FOR k = 0, nlevsmoz - 1 DO BEGIN
		FOR t = 0, ntime - 1 DO BEGIN
			allspecred = allspecs[*,*,k,t,ispec]
			allspecinterp2d = INTERPOLATE(allspecred, indexlon, indexlat)
			allspecinterp[*,*,k,t,ispec] = allspecinterp2d
		ENDFOR
	ENDFOR
ENDFOR

print,'Vertical interpolation...'

; Vertical interpolation, linear in pressure
; This is the target array
allspecinterpv = FLTARR(ncols,nrows,nlevs,ntime,nspec)
; Switches which will be used in order to ensure that certain warnings are printed only once
l = 1 
m = 1

FOR ispec = 0, nspec - 1 DO BEGIN
	FOR t = 0, ntime - 1 DO BEGIN
		FOR i = 0, ncols - 1 DO BEGIN
			FOR j = 0, nrows - 1 DO BEGIN
				; we have the concentrations (that is our function of p)
				; in allspecinterp[i,j,ilev,t,ispec]
				; the pressures at which we want to sample are in pres[i,j,k,t * 3]
				; the pressures at which the function is defined are in lev
				interpolated = INTERPOL(allspecinterp[i,j,*,t,ispec],lev,pres[i,j,*,t * 3])

				; If there are CAMx pressure levels below the lowest MOZART level, those CAMx levels
				; get the value which corresponds to the lowest level of MOZART. This is done
				; in order to avoid negative, zero or too low values which occured as a result of
				; the interpolation below the lowest MOZART level.
				below_mozart_level = WHERE(pres[i,j,*] GT lev[0], count)
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

; Here it is tested whether the resulting matrix has the correct values (10.0 everywhere)
; after the interpolation.
ne10 = WHERE(allspecinterpv NE 10.0, count)
PRINT,"********************************************************"
IF (count NE 0) THEN PRINT, "Test on array filled with identical values failed!"
IF (count EQ 0) THEN PRINT, "Test on array filled with identical values was successful."
PRINT,"********************************************************"

END

