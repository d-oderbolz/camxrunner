      SUBROUTINE setno2(no2new,
     $     nz,z,nw,wl,
     $     xsno2, tlay,
     $     dtno2)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Set up an altitude profile of NO2 molecules, and corresponding absorption=*
*=  optical depths.  Subroutine includes a shape-conserving scaling method   =*
*=  that allows scaling of the entire profile to a given overhead NO2        =*
*=  column amount.                                                           =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NO2NEW - REAL, overhead NO2 column amount (molec/cm^2) to which       (I)=*
*=           profile should be scaled.  If NO2NEW < 0, no scaling is done    =*
*=  NZ     - INTEGER, number of specified altitude levels in the working  (I)=*
*=           grid                                                            =*
*=  Z      - REAL, specified altitude working grid (km)                   (I)=*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  XSNO2  - REAL, molecular absoprtion cross section (cm^2) of O2 at     (I)=*
*=           each specified wavelength                                       =*
*=  TLAY   - REAL, temperature (K) at each specified altitude layer       (I)=*
*=  DTNO2  - REAL, optical depth due to NO2 absorption at each            (O)=*
*=           specified altitude at each specified wavelength                 =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  03/97  fix DO-10 loop                                                    =*
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
      REAL no2new

* mid-layer temperature:

      REAL tlay(kz) 

********
* output:
********

      REAL dtno2(kz,kw)

********
* local:
********

* absorption cross sections 

      REAL xsno2(kw)
      REAL cz(kz)

* nitrogen dioxide profile data:

      REAL zd(kdata), no2(kdata)
      REAL cd(kdata)
      REAL hscale
      REAL colold, colnew
      REAL scale
      REAL sno2

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

      write(kout,*) 'NO2:  1 ppb in lowest 1 km, 0 above'

      nd = 3
      zd(1) = 0.
      no2(1) = 1. * 2.69e10

      zd(2) = 1.
      no2(2) = 1. * 2.69e10

      zd(3) = zd(2)* 1.000001
      no2(3) = 0.

C     zd(4) = zd(3)*1.1 
C     no2(4) = 0.

* compute column increments (alternatively, can specify these directly)

      DO 11, i = 1, nd - 1
         cd(i) = (no2(i+1)+no2(i)) * 1.E5 * (zd(i+1)-zd(i)) / 2. 
   11 CONTINUE

* Include exponential tail integral from top level to infinity.
* fold tail integral into top layer
* specify scale height near top of data (use ozone value)

      hscale = 4.50e5
      cd(nd-1) = cd(nd-1) + hscale * no2(nd)

***********
*********** end data input.

* Compute column increments on standard z-grid.  

      CALL inter3(nz,z,cz, nd,zd,cd, 1)

* scale values of cz(i) 

      colold = fsum(nz-1,cz)
      WRITE(kout,100) colold, colold/2.687E16
  100 FORMAT(5x,'old NO2 Column = ', 1pe11.4,1x,'# cm-2  = ',
     $     0pf8.2, '  Dobson Units ')

      if ( (no2new .lt. 0.)  .or.  (colold .le. 0.) ) then
         scale = 1.
      else
         scale =  2.687e16*no2new/colold
      endif

      do i = 1, nz-1
         cz(i) = cz(i) * scale
      enddo
      colnew = fsum(nz-1,cz)
      WRITE(kout,105) colnew, colnew/2.687E16
  105 format(5x,'new NO2 Column = ', 1pe11.4,1x,'# cm-2  = ',
     $     0pf8.2, '  Dobson Units ')

************************************
* calculate optical depth for each layer, with temperature 
* correction.  Output, dtno2(kz,kw)

      DO 20, l = 1, nw-1
         sno2 = xsno2(l)
         DO 10, i = 1, nz-1
            dtno2(i,l) = cz(i)*sno2
   10    CONTINUE
   20 CONTINUE
*_______________________________________________________________________

      RETURN
      END
