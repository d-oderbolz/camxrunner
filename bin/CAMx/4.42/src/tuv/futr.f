      FUNCTION futr(w)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Calculate the action spectrum value for skin cancer of albino hairless   =*
*=  mice at a given wavelength according to:  deGRuijl, F.R., H.J.C.M.Steren-=*
*=  borg, P.D.Forbes, R.E.Davies, C.Colse, G.Kelfkens, H.vanWeelden,         =*
*=  and J.C.van der Leun, Wavelength dependence of skin cancer induction by  =*
*=  ultraviolet irradiation of albino hairless mice, Cancer Research, vol 53,=*
*=  pp. 53-60, 1993                                                          =*
*=  (Action spectrum for carcinomas)                                         =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  W  - REAL, wavelength (nm)                                            (I)=*
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
      REAL futr

* local:
      REAL a1, a2, a3, a4, a5,
     >     x1, x2, x3, x4, x5,
     >     t1, t2, t3, t4, t5,
     >     b1, b2, b3, b4, b5,
     >     p
*_______________________________________________________________________

      a1 = -10.91
      a2 = - 0.86
      a3 = - 8.60
      a4 = - 9.36
      a5 = -13.15

      x1 = 270.
      x2 = 302.
      x3 = 334.
      x4 = 367.
      x5 = 400.

      t1 = (w-x2)*(w-x3)*(w-x4)*(w-x5)
      t2 = (w-x1)*(w-x3)*(w-x4)*(w-x5)
      t3 = (w-x1)*(w-x2)*(w-x4)*(w-x5)
      t4 = (w-x1)*(w-x2)*(w-x3)*(w-x5)
      t5 = (w-x1)*(w-x2)*(w-x3)*(w-x4)

      b1 = (x1-x2)*(x1-x3)*(x1-x4)*(x1-x5)
      b2 = (x2-x1)*(x2-x3)*(x2-x4)*(x2-x5)
      b3 = (x3-x1)*(x3-x2)*(x3-x4)*(x3-x5)
      b4 = (x4-x1)*(x4-x2)*(x4-x3)*(x4-x5)
      b5 = (x5-x1)*(x5-x2)*(x5-x3)*(x5-x4)

      p = a1*t1/b1 + a2*t2/b2 + a3*t3/b3 + a4*t4/b4 + a5*t5/b5

      futr  = EXP(p)
*_______________________________________________________________________

      RETURN
      END
