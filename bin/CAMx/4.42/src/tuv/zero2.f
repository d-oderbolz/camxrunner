      subroutine zero2(x,m,n)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Initialize all elements of a 2D floating point array with zero.          =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  X  - REAL, array to be initialized                                    (O)=*
*=  M  - INTEGER, number of elements along the first dimension of X,      (I)=*
*=       exactly as specified in the calling program                         =*
*=  N  - INTEGER, number of elements along the second dimension of X,     (I)=*
*=       exactly as specified in the calling program                         =*
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

      implicit none
* m,n : dimensions of x, exactly as specified in the calling program
      integer i, j, m, n
      real x(m,n)
      do 1 j = 1, n
         do 2 i = 1, m
            x(i,j) = 0.
 2       continue
 1    continue
      return
      end
