      SUBROUTINE setair(pmbnew,
     $     nz,z,nw,wl,
     $     airlev,dtrl,cz)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Set up an altitude profile of air molecules.  Subroutine includes a      =*
*=  shape-conserving scaling method that allows scaling of the entire        =*
*=  profile to a given sea-level pressure.                                   =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  PMBNEW  - REAL, sea-level pressure (mb) to which profile should be    (I)=*
*=            scaled.  If PMBNEW < 0, no scaling is done                     =*
*=  NZ      - INTEGER, number of specified altitude levels in the working (I)=*
*=            grid                                                           =*
*=  Z       - REAL, specified altitude working grid (km)                  (I)=*
*=  NW      - INTEGER, number of specified intervals + 1 in working       (I)=*
*=            wavelength grid                                                =*
*=  WL      - REAL, vector of lower limits of wavelength intervals in     (I)=*
*=            working wavelength grid                                        =*
*=  AIRLEV  - REAL, air density (molec/cc) at each specified altitude     (O)=* 
*=  DTRL    - REAL, Rayleigh optical depth at each specified altitude     (O)=*
*=            and each specified wavelength                                  =*
*=  CZ      - REAL, number of air molecules per cm^2 at each specified    (O)=*
*=            altitude layer                                                 =*
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

* input: (grids)
      REAL wl(kw)
      REAL z(kz)
      INTEGER nw
      INTEGER nz
      REAL  pmbnew

* output:
* air density (molec cm-3) at each grid level
* Rayleigh optical depths

      REAL airlev(kz)
      REAL dtrl(kz,kw)

* local:
      REAL scale
      real airnew(kdata)
      REAL colold, colnew, pmbold
      REAL pconv
      PARAMETER(pconv = 980.665 * 1.E-3 * 28.9644 / 6.022169E23)
* specified data:
      REAL zd(kdata), air(kdata)
      REAL hscale
      REAL cd(kdata)

* other:
      REAL cz(kz)
      REAL srayl(kw)
      REAL deltaz
      REAL colz, pressz
      REAL wc, wmicrn, xx 
      INTEGER i, iw, nd
      

* External functions:
      REAL fsum
      EXTERNAL fsum

*_______________________________________________________________________

* read in air density profile

      WRITE(kout,*) 'air density: USSA, 1976'

      OPEN(kin,FILE='DATAE1/ATM/ussa.dens',STATUS='old')
      DO i = 1, 3
         READ(kin,*)
      ENDDO
      nd = 1
 4    CONTINUE
        READ(kin,*,END=5) zd(nd), air(nd)
        nd = nd+1
        GOTO 4
 5    CONTINUE
      CLOSE(kin)
      nd = nd-1

* compute column increments (logarithmic integrals)

      DO 6, i = 1, nd - 1
         deltaz = 1.E5 * (zd(i+1)-zd(i)) 
         cd(i) =  (air(i+1)-air(i)) /ALOG(air(i+1)/air(i)) * deltaz
C         cd(i) = (air(i+1)+air(i)) * deltaz / 2. 
    6 CONTINUE

* Include exponential tail integral from infinity to 50 km,
* fold tail integral into top layer
* specify scale height near top of data.

      hscale = 8.05e5
      cd(nd-1) = cd(nd-1) + hscale * air(nd)

* alternative input air density data could include, e.g., a read file here:

* If want, can rescale to any total pressure:

      colold = fsum(nd-1,cd)
      pmbold = colold * pconv 
      WRITE(kout,100) colold, pmbold
  100 FORMAT(5x,'old sea level air column = ', 1pe11.4,1x,'# cm-2  = ',
     $     0pf8.2,' mbar')

* assign new sea level pressure

      if (pmbnew .lt. 0.) then
         scale = 1.
      else
         scale = pmbnew/pmbold
      endif

      DO i = 1, nd-1
         cd(i) = cd(i) * scale
         airnew(i) = air(i) * scale
      ENDDO
      airnew(nd) = air(nd) * scale
      
      colnew = fsum(nd-1,cd)
      WRITE(kout,105) colnew, colnew * pconv
  105 FORMAT(5x,'new sea level air column = ', 1pe11.4,1x,'# cm-2  = ',
     $     0pf8.2,' mbar')

********************** end data input.

* Compute air density at each level

      CALL inter1(nz,z,airlev,nd,zd,airnew)

* Compute column increments on standard z-grid.  

      CALL inter3(nz,z,cz, nd,zd,cd, 1)
      
      colz = fsum(nz-1,cz)
      pressz =  colz * pconv
      write(kout,110) colz, pressz
 110  FORMAT(5x,'surface air column = ', 1pe11.4,1x,'# cm-2  = ',
     $     0pf8.2,' mbar')


* compute Rayleigh cross sections and depths:

      DO 30, iw = 1, nw - 1
         wc = (wl(iw) + wl(iw+1))/2.

* Rayleigh scattering cross section from WMO 1985 (originally from
* Nicolet, M., On the molecular scattering in the terrestrial atmosphere:
* An empirical formula for its calculation in the homoshpere, Planet.
* Space Sci., 32, 1467-1468, 1984.

         wmicrn =  wc/1.E3
         IF( wmicrn .LE. 0.55) THEN
            xx = 3.6772 + 0.389*wmicrn + 0.09426/wmicrn
         ELSE
            xx = 4. + 0.04
         ENDIF
         srayl(iw) = 4.02e-28/(wmicrn)**xx

* alternate (older) expression from
* Frohlich and Shaw, Appl.Opt. v.11, p.1773 (1980).
C     xx = 3.916 + 0.074*wmicrn + 0.050/wmicrn
C     srayl(iw) = 3.90e-28/(wmicrn)**xx

         DO 40, i = 1, nz - 1
            dtrl(i,iw) = cz(i)*srayl(iw)
   40    CONTINUE

   30 CONTINUE
*_______________________________________________________________________

      RETURN
      END
