      SUBROUTINE read2(nw,wl,f)

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

* input: (wavelength grid)
      INTEGER nw
      REAL wl(kw)
      REAL yg(kw)

*
      INTEGER iw

* output: (extra terrestrial solar flux)
      REAL f(kw)

* local:

      REAL x1(1000), y1(1000) 
      REAL x2(1000), y2(1000)
      REAL x3(1000), y3(1000)
      INTEGER i, n
      REAL DUM
      INTEGER IDUM

*_______________________________________________________________________

*********WMO 85 irradiance

      OPEN(UNIT=kin,FILE='DATAE1/SUN/wmo85.flx',STATUS='old')
      DO 11, i = 1, 3
         READ(kin,*)
   11 CONTINUE
      n = 158
      DO 12, i = 1, n
         READ(kin,*) idum, x1(i),x2(i),y1(i), dum, dum, dum
         x3(i) = 0.5 * (x1(i) + x2(i))

C average value needs to be calculated only if inter2 is
C used to interpolate onto wavelength grid (see below)
C        y1(i) =  y1(i) / (x2(i) - x1(i)) 

   12 CONTINUE
      CLOSE (kin)

      x1(n+1) = x2(n)

C inter2: INPUT : average value in each bin 
C         OUTPUT: average value in each bin
C inter3: INPUT : total area in each bin
C         OUTPUT: total area in each bin

      CALL inter3(nw,wl,yg, n+1,x1,y1,0)
C      CALL inter2(nw,wl,yg,n,x3,y1,ierr)

      DO 10,  iw = 1, nw-1
* from quanta s-1 cm-2 bin-1 to  watts m-2 nm-1
* 1.e4 * ([hc =] 6.62E-34 * 2.998E8)/(wc*1e-9) 
         
C the scaling by bin width needs to be done only if
C inter3 is used for interpolation

         yg(iw) = yg(iw) / (wl(iw+1)-wl(iw))
         f(iw) = yg(iw) * 1.e4 * (6.62E-34 * 2.998E8) / 
     $        ( 0.5 * (wl(iw+1)+wl(iw)) * 1.e-9)

   10 CONTINUE
      
*_______________________________________________________________________

      RETURN
      END
