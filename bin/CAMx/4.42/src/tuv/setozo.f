      SUBROUTINE setozo(dobnew,
     $     nz,z,nw,wl,
     $     xso3,s226,s263,s298,tlay,
     $     dto3)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Set up an altitude profile of ozone, and corresponding absorption        =*
*=  optical depths.  Subroutine includes a shape-conserving scaling method   =*
*=  that allows scaling of the entire profile to a given overhead ozone      =*
*=  column amount.                                                           =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  DOBNEW - REAL, overhead ozone column amount (DU) to which profile     (I)=*
*=           should be scaled.  If DOBNEW < 0, no scaling is done            =*
*=  NZ     - INTEGER, number of specified altitude levels in the working  (I)=*
*=           grid                                                            =*
*=  Z      - REAL, specified altitude working grid (km)                   (I)=*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  XSO3   - REAL, molecular absoprtion cross section (cm^2) of O3 at     (I)=*
*=           each specified wavelength (WMO value at 273)                    =*
*=  S226   - REAL, molecular absoprtion cross section (cm^2) of O3 at     (I)=*
*=           each specified wavelength (value from Molina and Molina at 226K)=*
*=  S263   - REAL, molecular absoprtion cross section (cm^2) of O3 at     (I)=*
*=           each specified wavelength (value from Molina and Molina at 263K)=*
*=  S298   - REAL, molecular absoprtion cross section (cm^2) of O3 at     (I)=*
*=           each specified wavelength (value from Molina and Molina at 298K)=*
*=  TLAY   - REAL, temperature (K) at each specified altitude layer       (I)=*
*=  DTO3   - REAL, optical depth due to ozone absorption at each          (O)=*
*=           specified altitude at each specified wavelength                 =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  02/97  Read in profile from an input file                                =*
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
      PARAMETER(kdata=150)

********
* input:
********

* grids:
      REAL wl(kw)
      REAL z(kz)
      INTEGER nw
      INTEGER nz
* ozone absorption cross sections at 226, 263, and 298 K:
      REAL xso3(kw), s226(kw),s263(kw),s298(kw)
      REAL dobnew
* mid-layer temperature:
      REAL tlay(kz) 

********
* output:
********
      REAL dto3(kz,kw)


********
* local:
********

      REAL cz(kz)

* ozone profile data:

      REAL zd(kdata), o3(kdata)
      REAL cd(kdata)
      REAL hscale
      REAL dobold, scale
      REAL colold, colnew
      REAL so3

* other:
      INTEGER i, iw, nd

********
* External functions:
********
      REAL fsum
      EXTERNAL fsum
*_______________________________________________________________________


* read in ozone profile

      WRITE(kout,*) 'ozone profile: USSA, 1976'

      OPEN(kin,FILE='DATAE1/ATM/ussa.ozone',STATUS='old')
      DO i = 1, 7
        READ(kin,*)
      ENDDO
      nd = 1
 4    CONTINUE
         READ(kin,*,END=5) zd(nd), o3(nd)
         nd = nd+1
         GOTO 4
 5    CONTINUE
      CLOSE(kin)
      nd = nd-1

* compute column increments

      DO 11, i = 1, nd - 1
         cd(i) = (o3(i+1)+o3(i)) * 1.E5 * (zd(i+1)-zd(i)) / 2. 
   11 CONTINUE

* Include exponential tail integral from infinity to 50 km,
* fold tail integral into top layer
* specify scale height near top of data.

      hscale = 4.50e5
      cd(nd-1) = cd(nd-1) + hscale * o3(nd)

* alternative input ozone concentration data could include, e.g., 
* a read file here:

***********
*********** end data input.

* Compute column increments on standard z-grid.  

      CALL inter3(nz,z,cz, nd,zd,cd, 1)

* scale values of cz(i) by any dobson unit

      colold = fsum(nz-1,cz)
      dobold = colold/2.687e16
      WRITE(kout,100) colold, dobold
  100 FORMAT(5x,'old O3 Column = ', 1pe11.4,1x,'# cm-2  = ',
     $     0pf8.2, '  Dobson Units ')

      if (dobnew .lt. 0.) then
         scale = 1.
      else
         scale = dobnew/dobold
      endif

      do i = 1, nz-1
         cz(i) = cz(i) * scale
      enddo
      colnew = fsum(nz-1,cz)
      WRITE(kout,105) colnew, colnew/2.687E16
  105 format(5x,'new O3 Column = ', 1pe11.4,1x,'# cm-2  = ',
     $     0pf8.2, '  Dobson Units ')

************************************
* calculate ozone optical depth for each layer, with temperature 
* correction.  Output, dto3(kz,kw)

      DO 20, iw = 1, nw-1
         so3 = xso3(iw)
         DO 10, i = 1, nz - 1

            IF ( wl(iw) .GT. 240.5  .AND. wl(iw+1) .LT. 350. ) THEN
               IF (tlay(i) .LT. 263.) THEN
                  so3 = s226(iw) + (s263(iw)-s226(iw)) / (263.-226.) *
     $                 (tlay(i)-226.)
               ELSE
                  so3 = s263(iw) + (s298(iw)-s263(iw)) / (298.-263.) *
     $              (tlay(i)-263.)
               ENDIF
            ENDIF

            dto3(i,iw) = cz(i)*so3

   10    CONTINUE
   20 CONTINUE
*_______________________________________________________________________

      RETURN
      END
