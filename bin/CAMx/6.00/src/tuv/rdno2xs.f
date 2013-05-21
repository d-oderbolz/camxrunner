      SUBROUTINE rdno2xs(nw,wl,xsno2)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Read NO2 molecular absorption cross section.  Re-grid data to match      =*
*=  specified wavelength working grid.                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  XSNO2  - REAL, molecular absoprtion cross section (cm^2) of NO2 at    (O)=*
*=           each specified wavelength                                       =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  02/97  Changed offset for grid-end interpolation to relative number      =*
*=         (x * (1 +- deltax)                                                =*
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

      REAL xsno2(kw)

* local:
      REAL x1(kdata)
      REAL y1(kdata),y2(kdata),y3(kdata)
      REAL yg(kw)
      REAL a1, a2, dum
      INTEGER ierr
      INTEGER i, l, n, idum
      CHARACTER*40 fil
*_______________________________________________________________________

************* absorption cross sections:
*     measurements of Davidson et al. (198x) at 273K
*     from 263.8 to 648.8 nm in approximately 0.5 nm intervals

      fil = 'DATAE1/NO2/NO2_ncar_00.abs'
      OPEN(UNIT=kin,FILE=fil,STATUS='old')
      n = 750
      DO i = 1, n
         READ(kin,*) x1(i), y1(i), dum, dum, idum
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,      1.e+38,0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, fil
         STOP
      ENDIF

      DO 13, l = 1, nw-1
         xsno2(l) = yg(l)
   13 CONTINUE

*_______________________________________________________________________

      RETURN
      END
