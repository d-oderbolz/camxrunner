      FUNCTION fsum(n,x)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Compute the sum of the first N elements of a floating point vector.      =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  N  - INTEGER, number of elements to sum                               (I)=*
*=  X  - REAL, vector whose components are to be summed                   (I)=*
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
      INTEGER n
      REAL x(n)

* function value:
      REAL fsum

* local:
      INTEGER i
*_______________________________________________________________________

      fsum = 0.
      DO 10, i = 1, n
         fsum=fsum+x(i)
   10 CONTINUE
*_______________________________________________________________________

      RETURN
      END
