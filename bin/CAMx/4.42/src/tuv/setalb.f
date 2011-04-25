      SUBROUTINE setalb(nw,wl,albedo)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Set the albedo of the surface.  The albedo is assumed to be Lambertian,  =*
*=  i.e., the reflected light is isotropic, and idependt of the direction    =*
*=  of incidence of light.  Albedo can be chosen to be wavelength dependent. =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW      - INTEGER, number of specified intervals + 1 in working       (I)=*
*=            wavelength grid                                                =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  ALBEDO  - REAL, surface albedo at each specified wavelength           (O)=*
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

* input: (wavelength working grid data)
      INTEGER nw
      REAL wl(kw)

* output:
      REAL albedo(kw)

* local:
      INTEGER iw
      REAL alb
*_______________________________________________________________________

* set

      alb = 0.10
      WRITE(kout,*)'wavelength-independent albedo = ', alb
      DO 10, iw = 1, nw - 1

         albedo(iw) = alb

   10 CONTINUE
*_______________________________________________________________________

      RETURN
      END
