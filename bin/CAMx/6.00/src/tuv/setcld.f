      SUBROUTINE setcld(nz,z,nw,wl,dtcld,omcld,gcld)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Set cloud properties for each specified altitude layer.  Properties      =*
*=  may be wavelength dependent.                                             =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NZ      - INTEGER, number of specified altitude levels in the working (I)=*
*=            grid                                                           =*
*=  Z       - REAL, specified altitude working grid (km)                  (I)=*
*=  NW      - INTEGER, number of specified intervals + 1 in working       (I)=*
*=            wavelength grid                                                =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  DTCLD   - REAL, optical depth due to absorption by clouds at each     (O)=*
*=            altitude and wavelength                                        =*
*=  OMCLD   - REAL, single scattering albedo due to clouds at each        (O)=*
*=            defined altitude and wavelength                                =*
*=  GCLD    - REAL, cloud asymmetry factor at each defined altitude and   (O)=*
*=            wavelength                                                     =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  12/94  Bug fix                                                           =*
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
      PARAMETER(kdata=51)

* input: (grids)
      REAL wl(kw)
      REAL z(kz)
      INTEGER nz
      INTEGER nw

* Output: 
      REAL dtcld(kz,kw), omcld(kz,kw), gcld(kz,kw)

* local:

* specified data:
      REAL zd(kdata), cd(kdata), omd(kdata), gd(kdata)
      REAL womd(kdata), wgd(kdata)

* other:
      REAL cz(kz)
      REAL omz(kz)
      REAL gz(kz)
      INTEGER i, iw, n

* External functions:
      REAL fsum
      EXTERNAL fsum
*_______________________________________________________________________


* cloud properties are set for each layer (not each level)

* Set as many clouds as want here:
* First choose a cloud grid, zd(n), in km above sea level
* Can allow altitude variation of omega, g:

      n = 4
      
      zd(1) = 5.
      cd(1) = 0.
      omd(1) = .9999
      gd(1) = .85

      zd(2) = 7.
      cd(2) = 0.
      omd(2) = .5
      gd(2) = .5

      zd(3) = 9.
      cd(3) = 0.
      omd(3) = .9999
      gd(3) = .85

      zd(4) = 11.

******************

* compute integrals and averages over grid layers:
* for g and omega, use averages weigthed by optical depth

C     DO 11, i = 1, n    !***** CHANGED!!See header!!*****
      DO 11, i = 1, n-1
         womd(i) = omd(i) * cd(i)
         wgd(i) = gd(i) * cd(i)
   11 CONTINUE
      CALL inter3(nz,z,cz,  n, zd,cd, 0)
      CALL inter3(nz,z,omz, n, zd,womd, 0)
      CALL inter3(nz,z,gz , n, zd,wgd, 0)

      DO 15, i = 1, nz-1
         IF (cz(i) .GT. 0.) THEN
            omz(i) = omz(i)/cz(i)
            gz(i)  = gz(i) /cz(i)
         ELSE
            omz(i) = 1.
            gz(i) = 0.
         ENDIF
   15 CONTINUE

      WRITE(kout,*) 'Cloud: ', n, 'levels, tot opt. dep. = ', 
     $     fsum(nz-1,cz)

* assign at all wavelengths
* (can move wavelength loop outside if want to vary with wavelength)

      DO 17, iw = 1, nw-1
         DO 16, i = 1, nz-1
            dtcld(i,iw) = cz(i)
            omcld(i,iw) = omz(i)
            gcld (i,iw) = gz(i)
   16    CONTINUE
   17 CONTINUE
*_______________________________________________________________________

      RETURN
      END
