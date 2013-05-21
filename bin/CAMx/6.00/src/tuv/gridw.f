      SUBROUTINE gridw(nw,wl,wc,wu)
cgy modified for camx by greg yarwood, 19 Jan 05

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Create the altitude grid for all interpolations and radiative transfer   =*
*=  calculations.  Grid may be irregularly spaced.  Wavelengths are in nm.   =*
*=  No gaps are allowed within the wavelength grid.                          =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW  - INTEGER, number of wavelength grid _points_                     (O)=*
*=  WL  - REAL, vector carrying the lower limit of each wavel. interval   (O)=*
*=  WC  - REAL, vector carrying the center wavel of each wavel. interval  (O)=*
*=              (wc(i) = 0.5*(wl(i)+wu(i), i = 1..NW-1)                      =*
*=  WU  - REAL, vector carrying the upper limit of each wavel. interval   (O)=*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  Original                                                                 =*
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

* output:

      REAL wl(kw), wc(kw), wu(kw)
      INTEGER nw

* local:

      REAL wincr
      INTEGER iw
      LOGICAL ok
      INTEGER idum
      REAL dum
      INTEGER mopt

*_______________________________________________________________________

**** chose wavelengths

* some pre-set options
*     mopt = 1    equal spacing
*     mopt = 2    Isaksen's grid
*     mopt = 3    combined Kockarts/Isaksen grid + Lyman-Alpha
*     mopt = 4    user-defined
cgy   mopt = 5    camx option

      mopt = 5

      IF (mopt .EQ. 1) GO TO 1
      IF (mopt .EQ. 2) GO TO 2
      IF (mopt .EQ. 3) GO TO 3
      IF (mopt .EQ. 4) GO TO 4
cgy
      IF (mopt .EQ. 5) GO TO 5
cgy

 1    CONTINUE
      nw = 140 + 1
      wincr = 1.0
      DO 10, iw = 1, nw-1
         wl(iw) = 280. + wincr*FLOAT(iw-1)
         wu(iw) = wl(iw) + wincr
         wc(iw) = ( wl(iw) + wu(iw) )/2.
   10 CONTINUE
      wl(nw) = wu(nw-1)
      GO TO 9

 2    CONTINUE
      nw = 0
      OPEN(unit=kin,file='DATAE1/GRIDS/isaksen.grid',status='old')
      DO iw = 1, 2 
         READ(kin,*)
      ENDDO
      DO iw = 1, 130
         nw = nw + 1
         READ(kin,*) idum, dum, wc(nw), wl(nw), wu(nw)
      ENDDO
      CLOSE(kin)
      nw = nw + 1
      wl(nw) = wu(nw-1)
      GO TO 9

*** grid for strat photolysis calculations, extended at short wavelengths

 3    CONTINUE
      nw = 1
* include Lyman-Alpha wavelengths ([120.,121.4],[121.4,121.9],[123.,-])
      wl(nw) = 120.0
      wu(nw) = 121.4
      wc(nw) = (wl(nw)+wu(nw))*0.5
      nw = nw+1
      wl(nw) = wu(nw-1)
      wu(nw) = 121.9
      wc(nw) = (wl(nw)+wu(nw))*0.5
      nw = nw+1
      wl(nw) = wu(nw-1)
      wu(nw) = 123.0
      wc(nw) = (wl(nw)+wu(nw))*0.5
      nw = nw+1
      wl(nw) = wu(nw-1)
      OPEN(unit=kin,file='DATAE1/GRIDS/kockarts.grid',status='old')
      DO iw = 1, 16
         nw = nw + 1
         READ(kin,*) wl(nw), wu(nw)
         wc(nw) = ( wl(nw) + wu(nw) ) / 2.
         IF (iw	.eq. 1) THEN
             wu(nw-1) = wl(nw)
             wc(nw-1) = (wl(nw-1) + wu(nw-1))*0.5
         ENDIF
      ENDDO
      CLOSE(kin)

      OPEN(unit=kin,file='DATAE1/GRIDS/isaksen.grid',status='old')
      DO iw = 1, 2 + 10
         READ(kin,*)
      ENDDO
      DO iw = 11, 130
         nw = nw + 1
         READ(kin,*) idum, dum, wc(nw), wl(nw), wu(nw)
      ENDDO
      CLOSE(kin)
      nw = nw + 1
      wl(nw) = wu(nw-1)
      GO TO 9

 4    CONTINUE
* define wavelength intervals of width 1 nm from 250 - 420 nm:
      nw = 1
      wl(1) = 250.
      DO iw = 251, 420
        wu(nw) = Float(iw)
        wc(nw) = (wl(nw) + wu(nw))/2.
        nw = nw+1
        wl(nw) = Float(iw)
      ENDDO
* define wavelength intervals of width 10 nm from 420 - 700 nm:
      DO iw = 430, 700, 10
        wu(nw) = Float(iw)
        wc(nw) = (wl(nw) + wu(nw))/2.
        nw = nw+1
        wl(nw) = Float(iw)
      ENDDO
      GO TO 9

cgy user defined for camx input file prep

 5    CONTINUE
      nw = 1
      wl(1) = 280.
      DO iw = 281, 420
        wu(nw) = Float(iw)
        wc(nw) = (wl(nw) + wu(nw))/2.
        nw = nw+1
        wl(nw) = Float(iw)
      ENDDO
      DO iw = 430, 700, 10
        wu(nw) = Float(iw)
        wc(nw) = (wl(nw) + wu(nw))/2.
        nw = nw+1
        wl(nw) = Float(iw)
      ENDDO
cgy

 9    CONTINUE

***
* write to record

      WRITE(kout,*)'w-grid:',nw,wl(1),wl(nw)

* check grid for assorted improprieties:

      CALL gridck(kw,nw,wl,ok)

      IF (.NOT. ok) THEN
         WRITE(kout,*)'STOP in GRIDW:  The w-grid does not make sense'
         STOP
      ENDIF

*_______________________________________________________________________

      RETURN
      END
