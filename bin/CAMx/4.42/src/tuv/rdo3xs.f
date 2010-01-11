      SUBROUTINE rdo3xs(nw,wl,xso3,s226,s263,s298)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Read ozone molecular absorption cross section.  Re-grid data to match    =*
*=  specified wavelength working grid.                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  XSO3   - REAL, molecular absoprtion cross section (cm^2) of O3 at     (O)=*
*=           each specified wavelength (WMO value at 273)                    =*
*=  S226   - REAL, molecular absoprtion cross section (cm^2) of O3 at     (O)=*
*=           each specified wavelength (value from Molina and Molina at 226K)=*
*=  S263   - REAL, molecular absoprtion cross section (cm^2) of O3 at     (O)=*
*=           each specified wavelength (value from Molina and Molina at 263K)=*
*=  S298   - REAL, molecular absoprtion cross section (cm^2) of O3 at     (O)=*
*=           each specified wavelength (value from Molina and Molina at 298K)=*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  02/97  Changed offset for grid-end interpolation to relative number      =*
*=         (x * (1 +- deltax))                                               =*
*-----------------------------------------------------------------------------*
*= This program is free software;  you can redistribute it and/or modify     =*
*= it under the terms of the GNU General Public License as published by the  =*
*= Free Software Foundation;  either version 2 of the license, or (at your   =*
*= option) any later version.                                                =*
*= The TUV package is distributed in the hope that it will be useful, but    =*
*= WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTIBI-  =*
*= LITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public     =*
*= License for more details.                                                 =*
*= To obtain a copy of the GNU General Public License, write to:             =*
*= Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.   =*
*-----------------------------------------------------------------------------*
*= To contact the authors, please mail to:                                   =*
*= Sasha Madronich, NCAR/ACD, P.O.Box 3000, Boulder, CO, 80307-3000, USA  or =*
*= send email to:  sasha@ucar.edu                                            =*
*-----------------------------------------------------------------------------*
*= Copyright (C) 1994,95,96  University Corporation for Atmospheric Research =*
*-----------------------------------------------------------------------------*

      IMPLICIT NONE
      INCLUDE 'params'

      INTEGER kdata
      PARAMETER(kdata=1000)

* input: (altitude working grid)
      INTEGER nw
      REAL wl(kw)

* output:
* ozone absorption cross section at three different 
* temperatures: 226, 263, 298 Kelvin.  Can interpolate
* to different temperatures. Units are cm2 molecule-1

      REAL xso3(kw), s226(kw),s263(kw),s298(kw)

* local:
      REAL x1(kdata),x2(kdata),x3(kdata)
      REAL y1(kdata),y2(kdata),y3(kdata)
      REAL yg(kw)
      REAL a1, a2, dum
      INTEGER ierr
      INTEGER i, iw, n, idum, n1, n2, n3

      character*40 fil

*_______________________________________________________________________


************ from WMO 1985 Ozone Assessment
* from 175.439 to 847.500 nm
* use value at 273 K

      fil = 'DATAE1/wmo85'
      OPEN(UNIT=kin,FILE='DATAE1/wmo85',STATUS='old')
      DO 11, i = 1, 3
         read(kin,*)
   11 CONTINUE
      n = 158
      DO 12, i = 1, n
         READ(kin,*) idum, a1, a2, dum, dum, dum, dum, y1(i)
         x1(i) = (a1+a2)/2.
   12 CONTINUE
      CLOSE (kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,      1.e+38,0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, fil
         STOP
      ENDIF
            
      DO 13, iw = 1, nw-1
         xso3(iw) = yg(iw)
   13 CONTINUE

************* ozone absorption cross sections:
* For Hartley aand Huggins bands, use temperature-dependent values from
* Molina, L. T., and M. J. Molina, Absolute absorption cross sections
* of ozone in the 185- to 350-nm wavelength range,
* J. Geophys. Res., vol. 91, 14501-14508, 1986.

      fil = 'DATAE1/O3/O3.molina.abs'
      OPEN(UNIT=kin,FILE='DATAE1/O3/O3.molina.abs',STATUS='old')
      DO 14, i = 1, 5
         READ(kin,*)
   14 CONTINUE

      n1 = 220
      n2 = 220
      n3 = 220
      DO 15, i = 1, n1
         READ(kin,*) x1(i), y1(i), y2(i), y3(i)
         x2(i) = x1(i)
         x3(i) = x1(i)
   15 CONTINUE
      CLOSE (kin)

      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,           0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,       1.e+38,0.)
      CALL inter2(nw,wl,yg,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, fil
         STOP
      ENDIF
            
      DO 16, iw = 1, nw-1
         s226(iw) = yg(iw)*1.E-20
   16 CONTINUE

      CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,           0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,       1.e+38,0.)
      CALL inter2(nw,wl,yg,n2,x2,y2,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, fil
         STOP
      ENDIF
      DO 17, iw = 1, nw-1
         s263(iw) = yg(iw)*1.E-20
   17 CONTINUE

      CALL addpnt(x3,y3,kdata,n3,x3(1)*(1.-deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,           0.,0.)
      CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,       1.e+38,0.)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, fil
         STOP
      ENDIF
      CALL inter2(nw,wl,yg,n3,x3,y3,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, fil
         STOP
      ENDIF
                  
      DO 18, iw = 1, nw-1
         s298(iw) = yg(iw)*1.E-20
   18 CONTINUE


*_______________________________________________________________________

      RETURN
      END
