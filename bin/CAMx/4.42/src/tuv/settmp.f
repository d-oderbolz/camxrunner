      SUBROUTINE settmp(nz,z,tlev,tlay)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Set up an altitude profile of temperatures.  Temperature values are      =*
*=  needed to compute some cross sections and quantum yields.  Distinguish   =*
*=  between temperature at levels and layers.                                =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NZ      - INTEGER, number of specified altitude levels in the working (I)=*
*=            grid                                                           =*
*=  Z       - REAL, specified altitude working grid (km)                  (I)=*
*=  TLEV    - REAL, temperature (K) at each specified altitude level      (O)=*
*=  TLAY    - REAL, temperature (K) at each specified altitude layer      (O)=*
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

* input: (altitude working grid)
      REAL z(kz)
      INTEGER nz

* output:
      REAL tlev(kz), tlay(kz)

* local:
      REAL zd(kdata), td(kdata)
      INTEGER i, nd
*_______________________________________________________________________


* read in temperature profile

      WRITE(kout,*) 'air temperature: USSA, 1976'

      OPEN(kin,FILE='DATAE1/ATM/ussa.temp',STATUS='old')
      DO i = 1, 3
         READ(kin,*)
      ENDDO
      nd = 1
 4    CONTINUE
         READ(kin,*,END=5) zd(nd), td(nd) 
         nd = nd+1
         GOTO 4
 5    CONTINUE
      CLOSE(kin)
      nd = nd-1

* use constant temperature to infinity:  

      zd(nd) = 1.E10

* alternative input temperature data could include, e.g., a read file here:

***********
*********** end data input.

* interpolate onto z-grid

      CALL inter1(nz,z,tlev,nd,zd,td)

* compute layer-averages

      DO 20, i = 1, nz - 1
         tlay(i) = (tlev(i+1) + tlev(i))/2.
 20   CONTINUE
*_______________________________________________________________________
      
      RETURN
      END
