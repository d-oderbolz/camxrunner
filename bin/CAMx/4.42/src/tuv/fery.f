      FUNCTION fery(w)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Calculate the action spectrum value for erythema at a given wavelength   =*
*=  according to: McKinlay, A.F and B.L.Diffey, A reference action spectrum  =*
*=  for ultraviolet induced erythema in human skin, CIE Journal, vol 6,      =*
*=  pp 17-22, 1987.                                                          =*
*=  Value at 300 nm = 0.6486                                                 =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  W - REAL, wavelength (nm)                                             (I)=*
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

* input:
      REAL w 

* function value:
      REAL fery
*_______________________________________________________________________

      IF (w .LT. 250.) THEN
          fery = 1.
C outside the ery spectrum range
      ELSEIF ((w .GE. 250.) .AND. (w .LT. 298)) THEN
          fery = 1.
      ELSEIF ((w .GE. 298.) .AND. (w .LT. 328.)) THEN
          fery = 10.**( 0.094*(298.-w) )
      ELSEIF ((w .GE. 328.) .AND. (w .LT. 400.)) THEN
          fery = 10.**( 0.015*(139.-w) )
      ELSE
         fery = 1.E-36
C outside the ery spectrum range
      ENDIF

*_______________________________________________________________________

      RETURN
      END
