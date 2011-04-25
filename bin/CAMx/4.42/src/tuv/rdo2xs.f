      SUBROUTINE rdo2xs(nw,wl,xso2)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Read oxygen molecular absorption cross section (not icluding SR bands)   =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  XSO2   - REAL, molecular absoprtion cross section (cm^2) of O2 at     (O)=*
*=           each specified wavelength                                       =*
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
* O2 absorption cross section. Units are cm2 molecule-1

      REAL xso2(kw)

* local:
      REAL x1(kdata), x
      REAL y1(kdata), y
      REAL yg(kw)
      INTEGER i, iw, n, ifirst
      INTEGER icount
      INTEGER ierr
      CHARACTER*40 fil

*_______________________________________________________________________

************* O2 absorption cross sections:
* from 116 nm to 245 nm, including Schumann-Runge continumm
* from Brasseur and Solomon 1986.

      fil =  'DATAE1/O2/O2_src.abs'
      OPEN(UNIT=kin,FILE='DATAE1/O2/O2_src.abs')
      READ(kin,*) ifirst, n
      DO i = 1, ifirst-2
        READ(kin,*)
      ENDDO
      icount = 0
      DO i = 1, n
        READ(kin,*) x, y
        IF (x .LT. 204.) THEN
          icount = icount+1
          x1(icount) = x
          y1(icount) = y
        ENDIF
      ENDDO
      CLOSE(kin)

* overwrite from 204 to 241 nm (Herzberg continuum)

      OPEN(UNIT=kin,FILE='DATAE1/O2/O2_jpl94.abs',STATUS='old')
      read(kin,*)
      n = 40
      DO i = 1, n
         icount = icount+1
         READ(kin,*) y
         y1(icount) = y*1E-24
         x1(icount) = 204. + FLOAT(i-1)
      END DO
      CLOSE (kin)

         CALL addpnt(x1,y1,kdata,icount,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,icount,               0.,0.)
         CALL addpnt(x1,y1,kdata,icount,x1(icount)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,icount,            1.e38,0.)

      CALL inter2(nw,wl,yg,icount,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, fil
         STOP
      ENDIF

      DO iw = 1, nw-1
         xso2(iw) = yg(iw)
      END DO

      END
