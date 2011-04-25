       SUBROUTINE sjo2(nz,nw,xso2,nj,sq)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Update the weighting function (cross section x quantum yield) for O2     =*
*=  photolysis.  Effective O2 cross section depends upon solar zenith angle  =*
*=  (Schuman-Runge bands), so needs to be update at each time step.          =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  XSO2   - REAL, molecular absorption cross section in SR bands at      (I)=*
*=           each specified altitude and wavelength.  Includes Herzberg      =*
*=            continuum.                                                     =*
*=  NJ     - INTEGER, index of O2 photolysis in array SQ                  (I)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  08/96  Original                                                          =*
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

* calling parameters

      INTEGER nz, nw, nj
      REAL xso2(kz,kw)
      REAL sq(kj,kz,kw)

* local

      REAL qy
      INTEGER iw, iz
*______________________________________________________________________________

* O2 + hv -> O + O
* quantum yield assumed to be unity
* assign cross section values at all wavelengths and at all altitudes

      qy = 1.
      DO iw = 1, nw-1
        DO iz = 1, nz
          sq(nj,iz,iw) = qy * xso2(iz,iw)
        ENDDO
      ENDDO
*______________________________________________________________________________

      RETURN
      END
