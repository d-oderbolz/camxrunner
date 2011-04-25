      SUBROUTINE setso2(so2new,
     $     nz,z,nw,wl,
     $     xsso2, tlay,
     $     dtso2)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Set up an altitude profile of SO2 molecules, and corresponding absorption=*
*=  optical depths.  Subroutine includes a shape-conserving scaling method   =*
*=  that allows scaling of the entire profile to a given overhead SO2        =*
*=  column amount.                                                           =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  SO2NEW - REAL, overhead SO2 column amount (molec/cm^2) to which       (I)=*
*=           profile should be scaled.  If SO2NEW < 0, no scaling is done    =*
*=  NZ     - INTEGER, number of specified altitude levels in the working  (I)=*
*=           grid                                                            =*
*=  Z      - REAL, specified altitude working grid (km)                   (I)=*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  XSSO2  - REAL, molecular absoprtion cross section (cm^2) of O2 at     (I)=*
*=           each specified wavelength                                       =*
*=  TLAY   - REAL, temperature (K) at each specified altitude layer       (I)=*
*=  DTSO2  - REAL, optical depth due to SO2 absorption at each            (O)=*
*=           specified altitude at each specified wavelength                 =*
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

      INTEGER kdata
      PARAMETER(kdata=51)

********
* input:
********

* grids:

      REAL wl(kw)
      REAL z(kz)
      INTEGER nw
      INTEGER nz
      REAL so2new

* mid-layer temperature:

      REAL tlay(kz) 

********
* output:
********

      REAL dtso2(kz,kw)

********
* local:
********

* absorption cross sections 

      REAL xsso2(kw)
      REAL cz(kz)

* sulfur dioxide profile data:

      REAL zd(kdata), so2(kdata)
      REAL cd(kdata)
      REAL hscale
      REAL colold, colnew
      REAL scale
      REAL sso2

* other:

      INTEGER i, l, nd

********
* External functions:
********
      REAL fsum
      EXTERNAL fsum

*_______________________________________________________________________
* Data input:

* Example:  set to 1 ppb in lowest 1 km, set to zero above that.
* - do by specifying concentration at 3 altitudes.

      write(kout,*) 'SO2:  1 ppb in lowest 1 km, 0 above'

      nd = 3
      zd(1) = 0.
      so2(1) = 1. * 2.69e10

      zd(2) = 1.
      so2(2) = 1. * 2.69e10

      zd(3) = zd(2)* 1.000001
      so2(3) = 0.

C     zd(4) = zd(3)*1.1 
C     so2(4) = 0.

* compute column increments (alternatively, can specify these directly)

      DO 11, i = 1, nd - 1
         cd(i) = (so2(i+1)+so2(i)) * 1.E5 * (zd(i+1)-zd(i)) / 2. 
   11 CONTINUE

* Include exponential tail integral from top level to infinity.
* fold tail integral into top layer
* specify scale height near top of data (use ozone value)

      hscale = 4.50e5
      cd(nd-1) = cd(nd-1) + hscale * so2(nd)

***********
*********** end data input.

* Compute column increments on standard z-grid.  

      CALL inter3(nz,z,cz, nd,zd,cd, 1)

* scale values of cz(i) 

      colold = fsum(nz-1,cz)
      WRITE(kout,100) colold, colold/2.687E16
  100 FORMAT(5x,'old SO2 Column = ', 1pe11.4,1x,'# cm-2  = ',
     $     0pf8.2, '  Dobson Units ')

      if ( (so2new .lt. 0.)  .or.  (colold .le. 0.) ) then
         scale = 1.
      else
         scale =  2.687e16*so2new/colold
      endif

      do i = 1, nz-1
         cz(i) = cz(i) * scale
      enddo
      colnew = fsum(nz-1,cz)
      WRITE(kout,105) colnew, colnew/2.687E16
  105 format(5x,'new SO2 Column = ', 1pe11.4,1x,'# cm-2  = ',
     $     0pf8.2, '  Dobson Units ')

************************************
* calculate sulfur optical depth for each layer, with temperature 
* correction.  Output, dtso2(kz,kw)

      DO 20, l = 1, nw-1
         sso2 = xsso2(l)
         DO 10, i = 1, nz - 1

c Leaving this part in in case i want to interpolate between 
c the 221K and 298K data.
c
c            IF ( wl(l) .GT. 240.5  .AND. wl(l+1) .LT. 350. ) THEN
c               IF (tlay(i) .LT. 263.) THEN
c                  sso2 = s221(l) + (s263(l)-s226(l)) / (263.-226.) *
c     $                 (tlay(i)-226.)
c               ELSE
c                  sso2 = s263(l) + (s298(l)-s263(l)) / (298.-263.) *
c     $              (tlay(i)-263.)
c               ENDIF
c            ENDIF

            dtso2(i,l) = cz(i)*sso2

   10    CONTINUE
   20 CONTINUE
*_______________________________________________________________________

      RETURN
      END
