*=============================================================================*

      SUBROUTINE read1(nw,wl,f)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Read extra-terrestrial flux data.  Re-grid data to match specified       =*
*=  working wavelength grid.                                                 =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  F      - REAL, spectral irradiance at the top of the atmosphere at    (O)=*
*=           each specified wavelength                                       =*
*-----------------------------------------------------------------------------*

      IMPLICIT NONE
      INCLUDE 'params'

* input: (wavelength grid)
      INTEGER nw
      REAL wl(kw)

* output: (extra terrestrial solar flux)
      REAL f(kw)

* local:

      REAL lambda_hi(10000),irrad_hi(10000)
      REAL lambda
      INTEGER ierr
      INTEGER i, j, n
      CHARACTER*40 FIL

*_______________________________________________________________________

******* SUSIM irradiance 
*_______________________________________________________________________
* VanHoosier, M. E., J.-D. F. Bartoe, G. E. Brueckner, and
* D. K. Prinz, Absolute solar spectral irradiance 120 nm -
* 400 nm (Results from the Solar Ultraviolet Spectral Irradiance
* Monitor - SUSIM- Experiment on board Spacelab 2), 
* Astro. Lett. and Communications, 1988, vol. 27, pp. 163-168.
*     SUSIM SL2 high resolution (0.15nm) Solar Irridance data.
*     Irradiance values are given in milliwatts/m^2/nanomenters
*     and are listed at 0.05nm intervals.  The wavelength given is
*     the center wavelength of the 0.15nm triangular bandpass.
*     Normalized to 1 astronomical unit.
*  DATA for wavelengths > 350 nm are unreliable
* (Van Hoosier, personal communication, 1994).
*_______________________________________________________________________

** high resolution

      fil = 'DATAE1/SUN/susim_hi.flx'
      OPEN(UNIT=kin,FILE=fil,STATUS='old')
      DO 11, i = 1, 7
         READ(kin,*)
   11 CONTINUE
      DO 12, i = 1, 559
         READ(kin,*)lambda,(irrad_hi(10*(i-1)+j), j=1, 10)
   12 CONTINUE
      CLOSE (kin)

* compute wavelengths, convert from mW to W

      n = 559*10
      DO 13, i = 1, n
         lambda_hi(i)=120.5 + FLOAT(i-1)*.05
         irrad_hi(i) = irrad_hi(i)  /  1000.
   13 CONTINUE
*_______________________________________________________________________

      CALL addpnt(lambda_hi,irrad_hi,10000,n,
     >            lambda_hi(1)*(1.-deltax),0.)
      CALL addpnt(lambda_hi,irrad_hi,10000,n,                 0.,0.)
      CALL addpnt(lambda_hi,irrad_hi,10000,n,
     >            lambda_hi(n)*(1.+deltax),0.)
      CALL addpnt(lambda_hi,irrad_hi,10000,n,              1.e38,0.)
      CALL inter2(nw,wl,f,n,lambda_hi,irrad_hi,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, fil
         STOP
      ENDIF

      RETURN
      END

