      SUBROUTINE sundis(idate,esrm2)
cgy added checks on date

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Calculate Earth-Sun distance variation for a given date.  Based on       =*
*=  Fourier coefficients originally from:  Spencer, J.W., 1971, Fourier      =*
*=  series representation of the position of the sun, Search, 2:172          =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  IDATE  - INTEGER, specification of the date, from YYMMDD              (I)=*
*=  ESRM2  - REAL, variation of the Earth-sun distance                    (O)=*
*=           ESRM2 = (average e/s dist)^2 / (e/s dist on day IDATE)^2        =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  01/95  Changed computation of trig function values                       =*
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
      INTEGER idate

* output:
      REAL esrm2

* internal:
      INTEGER iyear, imonth, iday, mday, month, jday
      REAL dayn, thet0
      REAL sinth, costh, sin2th, cos2th
      INTEGER imn(12)

      REAL pi
      PARAMETER(pi=3.1415926535898)
*_______________________________________________________________________

      DATA imn/31,28,31,30,31,30,31,31,30,31,30,31/             
*_______________________________________________________________________

* parse date to find day number (Julian day)
cgy
      if (idate.lt.1.or.idate.gt.991231) then
         write(*,*) 'Date must be between 000001 and 991231'
         write(*,*) 'date = ', idate
         write(*,*) 'Enter 2000 as 00, etc.'
         stop
      endif
cgy

      iyear = int(idate/10000)
      imonth = int( (idate-10000*iyear)/100 )
      iday = idate - (10000*iyear + 100*imonth)

cgy
      if (imonth.gt.12) then
         write(*,*) 'Month in date exceeds 12'
         write(*,*) 'date = ', idate
         write(*,*) 'month = ', imonth
         stop
      endif
cgy

      IF ( MOD(iyear,4) .EQ. 0) THEN
         imn(2) = 29
      ELSE
         imn(2) = 28
      ENDIF

cgy
      if (iday.gt.imn(imonth)) then
         write(*,*) 'Day in date exceeds days in month'
         write(*,*) 'date = ', idate
         write(*,*) 'day = ', iday
         stop
      endif
cgy

      mday = 0
      DO 12, month = 1, imonth-1
         mday = mday + imn(month)	  	   
   12 CONTINUE
      jday = mday + iday
      dayn = FLOAT(jday - 1) + 0.5

* define angular day number and compute esrm2:

      thet0 = 2.*pi*dayn/365.

* calculate SIN(2*thet0), COS(2*thet0) from
* addition theoremes for trig functions for better
* performance;  the computation of sin2th, cos2th
* is about 5-6 times faster than the evaluation
* of the intrinsic functions SIN and COS
*
      sinth = SIN(thet0)
      costh = COS(thet0)
      sin2th = 2.*sinth*costh
      cos2th = costh*costh - sinth*sinth
      esrm2  = 1.000110 + 
     $         0.034221*costh  +  0.001280*sinth + 
     $         0.000719*cos2th +  0.000077*sin2th
*_______________________________________________________________________

      RETURN
      END
