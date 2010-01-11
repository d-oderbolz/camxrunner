      SUBROUTINE gridz(nz,z)
cgy modified for camx by greg yarwood 6/4/99

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Create the altitude grid for all interpolations and radiative transfer   =*
*=  calculations.  Grid may be irregularly spaced.  All altitudes are in     =*
*=  kilometers (km).  The altitude at index 1 specifies the surface elevation=*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NZ  - INTEGER, number of altitude points (levels)                     (O)=*
*=  Z   - REAL, vector of altitude levels (in km)                         (O)=*
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

* output: altitude working grid:

      REAL z(kz)
      INTEGER nz

* local:

      REAL zincr
      INTEGER i
      LOGICAL ok
*_______________________________________________________________________

* set vertical grid of the atmosphere.  All values should be in km.
* User specifies upright grid (surface at lowest km value, increasing
* upwards:
*     -  NZ = total number of user levels
*     -  Z(I) = altitude in km for each level.
* Note "levels" are vertical points
*      "layers" are vertical distances between levels

* set atmospheric level altitudes (in real km), including 
* top-most level.
* non-uniform spacing is possible 

      z(1) = 0.
      nz = 1
      zincr = 0.2
      do i = 1, 10
         nz = nz + 1
         z(nz) = z(nz-1) + zincr
      enddo
      do i = 1, 20
         nz = nz + 1
         z(nz) = z(nz-1) + zincr*2.
      enddo
      do i = 1, 20
         nz = nz + 1
         z(nz) = z(nz-1) + zincr*5.
      enddo
      do i = 1, 10
         nz = nz + 1
         z(nz) = z(nz-1) + zincr*10.
      enddo

* write to record:

      WRITE(kout,*)'z-grid:',nz,z(1),z(nz)

* check grid for assorted improprieties:

      CALL gridck(kz,nz,z,ok)

      IF (.NOT. ok) THEN
         WRITE(kout,*)'STOP in GRIDZ:  The z-grid does not make sense'
         STOP
      ENDIF
*_______________________________________________________________________

      RETURN
      END
