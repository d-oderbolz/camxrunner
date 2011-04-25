cgy modified by Greg Yarwood 6/4/99, see comments below
      SUBROUTINE r1(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)
*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide the product of (cross section) x (quantum yield) for the two     =*
*=  O3 photolysis reactions:                                                 =*
*=             (a) O3 + hv -> O2 + O(1D)                                     =*
*=             (b) O3 + hv -> O2 + O(3P)                                     =*
*=  Cross section:  Combined data from WMO 85 Ozone Assessment (use 273K     =*
*=                  value from 175.439-847.5 nm) and data from Molina and    =*
*=                  Molina (use in Hartley and Huggins bans (240.5-350 nm)   =*
*=  Quantum yield:  Choice between                                           =*
*=                   (1) data from Michelsen et al, 1994                     =*
*=                   (2) JPL 87 recommendation                               =*
*=                   (3) JPL 90/92 recommendation (no "tail")                =*
*=                   (4) data from Shetter et al., 1996                      =*
*=                   (5) JPL 97 recommendation                               =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)

      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER n1, n2, n3, n4, n5
      INTEGER kdata
      PARAMETER (kdata = 250)
      REAL x1(kdata), x2(kdata), x3(kdata), x4(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata), y4(kdata)

* local

      REAL yg(kw), yg1(kw), yg2(kw), yg3(kw), yg4(kw)
      REAL xso3(kw),s226(kw),s263(kw),s298(kw)
      REAL qy1d, qy3p
      REAL tau, tau2, tau3
      REAL a, b, c
      REAL a0, a1, a2, a3, a4, a5, a6, a7
      REAL xl, xl0
      REAL so3
      REAL dum
      INTEGER myld
      INTEGER kmich, kjpl87, kjpl92, kshet, kjpl97
      INTEGER i, iw, n, idum
      INTEGER ierr

****************************************************************

*************       jlabel(j) = 'O3 -> O2 + O(1D)'
*************       jlabel(j) = 'O3 -> O2 + O(3P)'

      j = j + 1
      jlabel(j) = 'O3 -> O2 + O(1D)'
      
      j = j + 1
      jlabel(j) = 'O3 -> O2 + O(3P)'

* cross sections:
* from WMO 1985 Ozone Assessment
* from 175.439 to 847.500 nm
* use value at 273 K

      OPEN(UNIT=kin,FILE='DATAE1/wmo85',STATUS='old')
      DO i = 1, 3
         read(kin,*)
      ENDDO
      n = 158
      DO i = 1, n
         READ(kin,*) idum, a1, a2, dum, dum, dum, dum, y1(i)
         x1(i) = (a1+a2)/2.
      ENDDO
      CLOSE (kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,               0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

      DO iw = 1, nw-1
         xso3(iw) = yg(iw)
      ENDDO

* For Hartley and Huggins bands, use temperature-dependent values from
* Molina, L. T., and M. J. Molina, Absolute absorption cross sections
* of ozone in the 185- to 350-nm wavelength range,
* J. Geophys. Res., vol. 91, 14501-14508, 1986.

      OPEN(UNIT=kin,FILE='DATAE1/O3/1986Molina.txt',STATUS='old')
      DO i = 1, 121
         READ(kin,*)
      ENDDO
      n1 = 220
      n2 = 220
      n3 = 220
      DO i = 1, n1
         READ(kin,*) x1(i), y1(i), y2(i), y3(i)
         x2(i) = x1(i)
         x3(i) = x1(i)
      ENDDO
      CLOSE (kin)

      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,               0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,            1.e+38,0.)
      CALL inter2(nw,wl,yg,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF
      DO iw = 1, nw-1
         s226(iw) = yg(iw)
      ENDDO

      CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,               0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,            1.e+38,0.)
      CALL inter2(nw,wl,yg,n2,x2,y2,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF
      DO iw = 1, nw-1
         s263(iw) = yg(iw)
      ENDDO

      CALL addpnt(x3,y3,kdata,n3,x3(1)*(1.-deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,               0.,0.)
      CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,            1.e+38,0.)
      CALL inter2(nw,wl,yg,n3,x3,y3,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF
      DO iw = 1, nw-1
         s298(iw) = yg(iw)
      ENDDO

* quantum yield:

      kmich = 1
      kjpl87 = 2
      kjpl92 = 3
      kshet = 4
      kjpl97 = 5

* choose quantum yield recommendation:
*    kjpl87:  JPL recommendation 1987                - JPL 87, 90, 92 do not "tail"
*    kjpl92:  JPL recommendations 1990/92 (identical) - still with no "tail"
*    kjpl97:  JPL recommendation 1997, includes tail, similar to Shetter et al.
*    kmich :  Michelsen et al., 1994
*    kshet :  Shetter et al., 1996

      myld = kjpl87
      myld = kjpl92
      myld = kshet
      myld = kmich
      myld = kjpl97

* read parameters from JPL'97

      IF (myld .EQ. kjpl97) THEN
        OPEN(UNIT=kin,FILE='DATAJ1/YLD/O3.param_jpl97.yld',STATUS='old')
        READ(kin,*)
        READ(kin,*)
        READ(kin,*)
        n1 = 21
        n2 = n1
        DO i = 1, n1
           READ(kin,*) x1(i), y1(i), y2(i)
           x2(i) = x1(i)
        ENDDO
        CLOSE(kin)

        CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),y1(1))
        CALL addpnt(x1,y1,kdata,n1,               0.,y1(1))
        CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),y1(n1))
        CALL addpnt(x1,y1,kdata,n1,            1.e+38,y1(n1))
        CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)
        IF (ierr .NE. 0) THEN
           WRITE(*,*) ierr, jlabel(j)
           STOP
        ENDIF

        CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),y2(1))
        CALL addpnt(x2,y2,kdata,n2,               0.,y2(1))
        CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),y2(n2))
        CALL addpnt(x2,y2,kdata,n2,            1.e+38,y2(n2))
        CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
        IF (ierr .NE. 0) THEN
           WRITE(*,*) ierr, jlabel(j)
           STOP
        ENDIF
      ENDIF

* read parameters from Michelsen, H. A., R.J. Salawitch, P. O. Wennber, 
* and J. G. Anderson, Geophys. Res. Lett., 21, 2227-2230, 1994.

      IF (myld .EQ. kmich) THEN
        OPEN(UNIT=kin,FILE='DATAJ1/YLD/O3.param.yld',STATUS='old')
        READ(kin,*)
        READ(kin,*)
        READ(kin,*)
        n1 = 21
        n2 = n1
        DO i = 1, n1
           READ(kin,*) x1(i), y1(i), y2(i)
           x2(i) = x1(i)
        ENDDO
        CLOSE(kin)

        CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),y1(1))
        CALL addpnt(x1,y1,kdata,n1,               0.,y1(1))
        CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),y1(n1))
        CALL addpnt(x1,y1,kdata,n1,            1.e+38,y1(n1))
        CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)
        IF (ierr .NE. 0) THEN
           WRITE(*,*) ierr, jlabel(j)
           STOP
        ENDIF

        CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),y2(1))
        CALL addpnt(x2,y2,kdata,n2,               0.,y2(1))
        CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),y2(n2))
        CALL addpnt(x2,y2,kdata,n2,            1.e+38,y2(n2))
        CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
        IF (ierr .NE. 0) THEN
           WRITE(*,*) ierr, jlabel(j)
           STOP
        ENDIF
      ENDIF

* quantum yield data from 
* Shetter et al, J.Geophys.Res., v 101 (D9), pg. 14,631-14,641, June 20, 1996

      IF (myld .EQ. kshet) THEN
        OPEN(UNIT=kin,FILE='DATAJ1/YLD/O3_shetter.yld',STATUS='OLD')
        READ(kin,*) idum, n
        DO i = 1, idum-2
          READ(kin,*)
        ENDDO
        n = n-2
        DO i = 1, n
          READ(kin,*) x1(i),y3(i),y4(i),y1(i),y2(i)
          x2(i) = x1(i)
          x3(i) = x1(i)
          x4(i) = x1(i)
        ENDDO
        DO i = n+1, n+2
           READ(kin,*) x3(i),y3(i),y4(i)
           x4(i) = x3(i)
        ENDDO
        CLOSE(kin)

        n1 = n
        n2 = n
        n3 = n+2
        n4 = n+2

* coefficients for exponential fit:

        CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax), y1(1))
        CALL addpnt(x1,y1,kdata,n1,                0., y1(1))
        CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
        CALL addpnt(x1,y1,kdata,n1,              1E38,0.)

        CALL inter2(nw,wl,yg1, n1,x1,y1, ierr)
        IF (ierr .NE. 0) THEN
           WRITE(*,*) ierr, jlabel(j)
           STOP
        ENDIF

        CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),y2(1))
        CALL addpnt(x2,y2,kdata,n2,               0.,y2(1))
        CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
        CALL addpnt(x2,y2,kdata,n2,              1E38,0.)

        CALL inter2(nw,wl,yg2, n2,x2,y2, ierr)
        IF (ierr .NE. 0) THEN
           WRITE(*,*) ierr, jlabel(j)
           STOP
        ENDIF

* phi data at 298 and 230 K

        CALL addpnt(x3,y3,kdata,n3,x3(1)*(1.-deltax),y3(1))
        CALL addpnt(x3,y3,kdata,n3,               0.,y3(1))
        CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
        CALL addpnt(x3,y3,kdata,n3,              1E38,0.)

        CALL inter2(nw,wl,yg3, n3,x3,y3, ierr)
        IF (ierr .NE. 0) THEN
           WRITE(*,*) ierr,jlabel(j)
           STOP
        ENDIF

        CALL addpnt(x4,y4,kdata,n4,x4(1)*(1.-deltax),y4(1))
        CALL addpnt(x4,y4,kdata,n4,               0.,y4(1))
        CALL addpnt(x4,y4,kdata,n4,x4(n4)*(1.+deltax),0.)
        CALL addpnt(x4,y4,kdata,n4,              1E38,0.)

        CALL inter2(nw,wl,yg4, n4,x4,y4, ierr)
        IF (ierr .NE. 0) THEN
           WRITE(*,*) ierr,jlabel(j)
           STOP
        ENDIF
      ENDIF

* compute cross sections and yields at different wavelengths, altitudes:

      DO iw = 1, nw-1

         so3 = xso3(iw)

         DO i = 1, nz

            IF ( wl(iw) .GT. 240.5  .AND. wl(iw+1) .LT. 350. ) THEN
               IF (tlev(i) .LT. 263.) THEN
                  so3 = s226(iw) + (s263(iw)-s226(iw)) / (263.-226.) *
     $                 (tlev(i)-226.)
               ELSE
                  so3 = s263(iw) + (s298(iw)-s263(iw)) / (298.-263.) *
     $              (tlev(i)-263.)
               ENDIF
            ENDIF

* quantum yields
* coefficients from jpl 87:

             IF (myld .EQ. kjpl87) THEN
               tau = tlev(i) - 230.
               tau2 = tau*tau
               tau3 = tau2*tau
               xl = wc(iw)
               xl0 = 308.2 + 4.4871e-2*tau + 6.938e-5*tau2 -
     >               2.5452e-6*tau3
               a = 0.9*(0.369 + 2.85e-4*tau + 1.28e-5*tau2 + 
     >                  2.57e-8*tau3)
               b     = -0.575 + 5.59e-3*tau - 1.439e-5*tau2 - 
     >                  3.27e-8*tau3
               c = 0.9*(0.518 + 9.87e-4*tau - 3.94e-5*tau2 + 
     >                  3.91e-7*tau3)
               qy1d = a*atan(b*(xl-xl0)) + c
               qy1d = amax1(0.,qy1d)
               qy1d = amin1(0.9,qy1d)
             ENDIF

* from jpl90, jpl92:
* (caution: error in JPL92 for first term of a3)

             IF (myld .EQ. kjpl92) THEN
               tau = 298. - tlev(i)
               tau2 = tau*tau
               xl0 = wc(iw) - 305.
               a0 =   .94932   - 1.7039e-4*tau + 1.4072E-6*tau2
               a1 = -2.4052e-2 + 1.0479e-3*tau - 1.0655e-5*tau2
               a2 =  1.8771e-2 - 3.6401e-4*tau - 1.8587e-5*tau2
               a3 = -1.4540e-2 - 4.7787e-5*tau + 8.1277e-6*tau2
               a4 =  2.3287e-3 + 1.9891e-5*tau - 1.1801e-6*tau2
               a5 = -1.4471e-4 - 1.7188e-6*tau + 7.2661e-8*tau2
               a6 =  3.1830e-6 + 4.6209e-8*tau - 1.6266e-9*tau2
               qy1d = a0 + a1*xl0 + a2*(xl0)**2 + a3*(xl0)**3 +
     >                a4*(xl0)**4 + a5*(xl0)**5 + a6*(xl0)**6
               IF (wc(iw) .LT. 305.) qy1d = 0.95
               IF (wc(iw) .GT. 320.) qy1d = 0.
               IF (qy1d .LT. 0.02) qy1d = 0.
             ENDIF

* from JPL'97

           IF (myld .EQ. kjpl97) THEN
             IF (wc(iw) .LT. 271.) THEN
                qy1d = 0.87
             ELSE IF (wc(iw) .GE. 271. .AND. wc(iw) .LT. 290.) THEN
                qy1d = 0.87 + (wc(iw)-271.)*(.95-.87)/(290.-271.)
             ELSE IF (wc(iw) .GE. 290. .AND. wc(iw) .LT. 305.) THEN
                qy1d = 0.95
             ELSE IF (wc(iw) .GE. 305. .AND. wc(iw) .LE. 325.) THEN
                qy1d = yg1(iw) * EXP ( -yg2(iw) /tlev(i) )
             ELSE
                qy1d = 0.
             ENDIF
           ENDIF
 
* from Michelsen, H. A., R.J. Salawitch, P. O. Wennber, and J. G. Anderson
* Geophys. Res. Lett., 21, 2227-2230, 1994.

           IF (myld .EQ. kmich) THEN
             IF (wc(iw) .LT. 271.) THEN
                qy1d = 0.87
             ELSE IF (wc(iw) .GE. 271. .AND. wc(iw) .LT. 305.) THEN
                qy1d = 1.98 - 301./wc(iw)
             ELSE IF (wc(iw) .GE. 305. .AND. wc(iw) .LE. 325.) THEN
                qy1d = yg1(iw) * EXP (-yg2(iw) /(0.6951*tlev(i)))
             ELSE
                qy1d = 0.
             ENDIF
           ENDIF
 
* Shetter et al.:
* phi = A * exp(-B/T), A and B are based on meas. at 298 and 230 K
* do linear interpolation between phi(298) and phi(230) for wavelengths > 321
* as phi(230)=0. for those wavelengths, so there are no A and B factors

           IF (myld .EQ. kshet) THEN
             IF (wl(iw+1) .LE. 321.) THEN
               qy1d = yg1(iw) * EXP(-1. * yg2(iw)/tlev(i))
             ELSE
               qy1d = (yg3(iw) - yg4(iw))/(298.-230.) * (tlev(i)-230.) +
     >                 yg4(iw)
             ENDIF
           ENDIF

           sq(j-1,i,iw) = qy1d*so3
           qy3p = 1.0 - qy1d
           sq(j,i,iw) = qy3p*so3

         ENDDO
      ENDDO

      END
      SUBROUTINE r10(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product of (cross section) x (quantum yield) for CH2O photolysis =*
*=        (a) CH2O + hv -> H + HCO                                           =*
*=        (b) CH2O + hv -> H2 + CO                                           =*
*=  Cross section: Choice between                                            =*
*=                 1) Bass et al., 1980 (resolution: 0.025 nm)               =*
*=                 2) Moortgat and Schneider (resolution: 1 nm)              =*
*=                 3) Cantrell et al. (orig res.) for > 301 nm,              =*
*=                    IUPAC 92, 97 elsewhere                                 =*
*=                 4) Cantrell et al. (2.5 nm res.) for > 301 nm,            =*
*=                    IUPAC 92, 97 elsewhere                                 =*
*=                 5) Rogers et al., 1990                                    =*
*=                 6) new NCAR recommendation, based on averages of          =*
*=                    Cantrell et al., Moortgat and Schneider, and Rogers    =*
*=                    et al.                                                 =*
*=  Quantum yield: Choice between                                            =*
*=                 1) Evaluation by Madronich 1991 (unpublished)             =*
*=                 2) IUPAC 89, 92, 97                                       =*
*=                 3) Madronich, based on 1), updated 1998.                  =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

      INTEGER kdata
      PARAMETER(kdata=16000)

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:

      INTEGER j, iz, iw

* data arrays

      INTEGER n
      real x(kdata), y(kdata)
      real xl(kdata), xc(kdata), xu(kdata)
      INTEGER n1, n2, n3, n4, n5
      REAL x1(kdata), x2(kdata), x3(kdata), x4(kdata), x5(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata), y4(kdata), y5(kdata)

* local

      REAL yg(kw), yg1(kw), yg2(kw), yg3(kw), yg4(kw), yg5(kw)
      REAL a, b, c
      REAL a0, a1, a2, a3, a4, a5, a6, a7
      REAL b0, b1, b2, b3, b4
      REAL phi1, phi2, phi20, ak300, akt
      REAL qy, qy1, qy2, qy3

      REAL sigma, sig, slope
      REAL xs
      REAL t
      REAL dum
      INTEGER idum

      INTEGER i
      INTEGER irow, icol, irev
      INTEGER ierr

      INTEGER mopt1, mopt2

*_______________________________________________________________________

      DO 5, iw = 1, nw - 1
         wc(iw) = (wl(iw) + wl(iw+1))/2.
 5    CONTINUE

****************************************************************
**************** CH2O photodissociatation

      j = j+1
      jlabel(j) = 'CH2O -> H + HCO' 

      j = j+1
      jlabel(j) = 'CH2O -> H2 + CO'

* working grid arrays:
*     yg1 = cross section at a specific temperature
*     yg2, yg3 = cross sections at different temp or slope, for calculating
*                temperature depedence
*     yg4 = quantum yield data for radical channel
*     yg5 = quantum yield data for molecular channel

* Input data options:
* mopt1 for absorption:
* 1:  DATAJ1/CH2O/CH2O_nbs.abs'
*     from Bass et al., Planet. Space. Sci. 28, 675, 1980.
*     over 258.750-359.525 in 0.025 nm steps
* 2:  DATAJ1/CH2O_iupac1.abs 
*     Moortgat and Schneider, personal communication as reported in IUPAC 89, 92, 97
*     at 285K.  Over 240-360 nm in 1 nm bins (note that IUPAC 89,92,97 incorectly 
*     claims 0.5 nm intervals in footnote)
* 3:  DATAJ1/CH2O/ch2o_can_hr.abs for wc > 301 nm, temperature dependent
*     DATAJ1/CH2O/ch2o_iupac1.abs elsewhere
*     from Cantrell et al. 1990 for wc > 301 nm.  Original data from Cantrell,
*     at high resolution
* 4:  DATAJ1/CH2O/CH2O_can_lr.abs for wc > 301 nm, temperature dependent
*     DATAJ1/CH2O/CH2O_iupac1.abs elsewhere
*     from Cantrell et al. 1990 for wc > 301 nm.  Data from Cantrell et al., as
*     reported by IUPAC'92,'97.  On 2.5 nm intervals.
* 5:  DATAJ1/CH2O/CH2O_rog.abs'
*     from Rogers et al., J. Phys. Chem. 94, 4011, 1990.
* 6:  DATAJ2/CH2O_ncar.abs
*     new NCAR recommendation, based on averages of Moortgat and Schneider, Cantrell et al.,
*     and Rogers.
* mopt2 for quantum yields:
* 1:  DATAJ1/CH2O/CH2O_i_mad.yld and 
*     DATAJ1/CH2O/CH2O_ii_mad.yld
*     evaluated by Madronich, 1991, unpublished
* 2:  DATAJ1/CH2O/CH2O_iupac.yld
*     from IUPAC'89, '92, '97
* 3:  DATAJ1/CH2O/CH2O_jpl97.dat'
*     based on Madronich 1991 unpublished evaluation, updated Jan 1998.

      mopt1 = 6
      mopt2 = 1

      IF (mopt1 .EQ. 1) THEN

* read NBS/Bass data

         OPEN(UNIT=kin,FILE='DATAJ1/CH2O/CH2O_nbs.abs'
     $        ,STATUS='old')
         n = 4032
         DO i = 1, n
            READ(kin,*) x(i), y(i)
         ENDDO
         CALL addpnt(x,y,kdata,n,x(1)*(1.-deltax),0.)
         CALL addpnt(x,y,kdata,n,               0.,0.)
         CALL addpnt(x,y,kdata,n,x(n)*(1.+deltax),0.)
         CALL addpnt(x,y,kdata,n,           1.e+38,0.)

         CALL inter2(nw,wl,yg1,n,x,y,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j-1)
            STOP
         ENDIF

      ELSEIF (mopt1 .EQ. 2 .OR. mopt1 .EQ. 3 .OR. mopt1 .EQ. 4) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH2O_iupac1.abs',STATUS='old')
         DO i = 1, 4
            READ(kin,*)
         ENDDO
         n = 121
         DO i = 1, n
            READ(kin,*) x(i), y(i)
            y(i) = y(i) * 1.e-20
         ENDDO
         CLOSE(kin)
         CALL addpnt(x,y,kdata,n,x(1)*(1.-deltax),0.)
         CALL addpnt(x,y,kdata,n,               0.,0.)
         CALL addpnt(x,y,kdata,n,x(n)*(1.+deltax),0.)
         CALL addpnt(x,y,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg1,n,x,y,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j-1)
            STOP
         ENDIF

         IF(mopt1 .EQ. 3) THEN

* data are on wavenumber grid (cm-1), so convert to wavelength in nm:
* grid was on increasing wavenumbers, so need to reverse to get increasing
* wavelengths
* cross section assumed to be zero for wavelengths longer than 360 nm
* if y1 < 0, then make = 0 (some negative cross sections, actually 273 K intercepts
* are in the original data,  Here, make equal to zero)

         OPEN(kin,FILE='DATAJ1/CH2O/CH2O_can_hr.abs',STATUS='old')
         READ(kin,*) idum, n
         DO i = 1, idum-2
            READ(kin,*)
         ENDDO
         DO i = 1, n
            READ(kin,*) x1(i), y1(i), y2(i)
            x1(i) = 1./x1(i) * 1E7
            IF (x1(i) .GT. 360.) THEN
               y1(i) = 0.
               y2(i) = 0.
            ENDIF
         ENDDO
         CLOSE(kin)

         DO i = 1, n/2
            irev = n+1-i
            dum = x1(i)
            x1(i) = x1(irev)
            x1(irev) = dum
            dum = y1(i)
            y1(i) = y1(irev)
            y1(irev) = dum
            dum = y2(i)
            y2(i) = y2(irev)
            y2(irev) = dum
         ENDDO
         DO i = 1, n
            x2(i) = x1(i)
            y1(i) = max(y1(i),0.)
         ENDDO
         n1 = n
         n2 = n

         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,               0.,0.)
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,             1E38,0.)
         CALL inter2(nw,wl,yg2,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

         CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
         CALL addpnt(x2,y2,kdata,n2,               0.,0.)
         CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
         CALL addpnt(x2,y2,kdata,n2,              1E38,0.)
         CALL inter2(nw,wl,yg3,n2,x2,y2,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF(mopt1 .eq. 4) THEN

            OPEN(UNIT=kin,FILE='DATAJ1/CH2O/CH2O_can_lr.abs',
     $        STATUS='old')
            DO i = 1, 4
               READ(kin,*)
            ENDDO
            n = 23
            DO i = 1, n
               READ(kin,*) x2(i), y2(i), y3(i), dum, dum
               x3(i) = x2(i)
            ENDDO
            CLOSE(kin)
            n2 = n
            n3 = n

            CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
            CALL addpnt(x2,y2,kdata,n2,               0.,0.)
            CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
            CALL addpnt(x2,y2,kdata,n2,             1E38,0.)
            CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
            IF (ierr .NE. 0) THEN
               WRITE(*,*) ierr, jlabel(j)
               STOP
            ENDIF

            CALL addpnt(x3,y3,kdata,n3,x3(1)*(1.-deltax),0.)
            CALL addpnt(x3,y3,kdata,n3,               0.,0.)
            CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
            CALL addpnt(x3,y3,kdata,n3,              1E38,0.)
            CALL inter2(nw,wl,yg3,n3,x3,y3,ierr)
            IF (ierr .NE. 0) THEN
               WRITE(*,*) ierr, jlabel(j)
               STOP
            ENDIF

         ENDIF

      ELSEIF (mopt1 .EQ. 5) THEN

* read Rodgers data

         OPEN(UNIT=kin,FILE='DATAJ1/CH2O/CH2O_rog.abs'
     $        ,STATUS='old')
         DO i = 1, 10
            READ(kin,*)
         ENDDO
         n = 261
         DO i = 1, n
            READ(kin,*) x(i), y(i), dum
            y(i) = y(i) * 1.e-20
         ENDDO
         CALL addpnt(x,y,kdata,n,x(1)*(1.-deltax),0.)
         CALL addpnt(x,y,kdata,n,               0.,0.)
         CALL addpnt(x,y,kdata,n,x(n)*(1.+deltax),0.)
         CALL addpnt(x,y,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg1,n,x,y,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j-1)
            STOP
         ENDIF

      ELSEIF(mopt1 .EQ. 6) THEN

            OPEN(UNIT=kin,FILE='DATAJ1/CH2O/CH2O_ncar.abs',STATUS='old')
            DO i = 1, 3
               READ(kin,*)
            ENDDO
            n = 126
            DO i = 1, n
               READ(kin,*) x2(i), y2(i), y3(i)
               x3(i) = x2(i)
            ENDDO
            CLOSE(kin)
            n2 = n
            n3 = n

            CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
            CALL addpnt(x2,y2,kdata,n2,               0.,0.)
            CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
            CALL addpnt(x2,y2,kdata,n2,             1E38,0.)
            CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
            IF (ierr .NE. 0) THEN
               WRITE(*,*) ierr, jlabel(j)
               STOP
            ENDIF

            CALL addpnt(x3,y3,kdata,n3,x3(1)*(1.-deltax),0.)
            CALL addpnt(x3,y3,kdata,n3,               0.,0.)
            CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
            CALL addpnt(x3,y3,kdata,n3,              1E38,0.)
            CALL inter2(nw,wl,yg3,n3,x3,y3,ierr)
            IF (ierr .NE. 0) THEN
               WRITE(*,*) ierr, jlabel(j)
               STOP
            ENDIF

      ENDIF
      
* quantum yield

      IF (mopt2 .EQ. 1) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH2O/CH2O_i_mad.yld',STATUS='old')
         DO i = 1, 11
            READ(kin,*)
         ENDDO
         n = 20
         DO i = 1, n
            READ(kin,*) x(i), y(i)
         ENDDO
         CLOSE(kin)
         CALL addpnt(x,y,kdata,n,x(1)*(1.-deltax),y(1))
         CALL addpnt(x,y,kdata,n,               0.,y(1))
         CALL addpnt(x,y,kdata,n,x(n)*(1.+deltax),0.)
         CALL addpnt(x,y,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg4,n,x,y,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j-1)
            STOP
         ENDIF

         OPEN(UNIT=kin,FILE='DATAJ1/CH2O/CH2O_ii_mad.yld',STATUS='old')
         DO i = 1, 9
            READ(kin,*)
         ENDDO
         n = 33
         DO i = 1, n
            READ(kin,*) x(i), y(i)
         ENDDO
         CLOSE(kin)
         CALL addpnt(x,y,kdata,n,x(1)*(1.-deltax),y(1))
         CALL addpnt(x,y,kdata,n,               0.,y(1))
         CALL addpnt(x,y,kdata,n,x(n)*(1.+deltax),0.)
         CALL addpnt(x,y,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg5,n,x,y,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF(mopt2 .EQ. 2) then

         OPEN(UNIT=kin,FILE='DATAJ1/CH2O/CH2O_iupac.yld',STATUS='old')
         DO i = 1, 7
            READ(kin,*) 
         ENDDO
         n = 13
         DO i = 1, n
            READ(kin,*) x1(i), y1(i), y2(i)
            x2(i) = x1(i)
         ENDDO
         CLOSE(kin)
         n1 = n
         n2 = n

         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),y1(1))
         CALL addpnt(x1,y1,kdata,n1,               0.,y1(1))
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
         CALL inter2(nw,wl,yg4,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

         CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),y2(1))
         CALL addpnt(x2,y2,kdata,n2,               0.,y2(1))
         CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
         CALL addpnt(x2,y2,kdata,n2,           1.e+38,0.)
         CALL inter2(nw,wl,yg5,n2,x2,y2,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

*! box-filling interpolation.  
c         DO i = 1, n
c            READ(kin,*) x1(i), y1(i), y2(i)
c            x1(i) = x1(i) - 5.0
c            x2(i) = x1(i)
c         ENDDO
c         n = n + 1
c         x1(n) = x1(n-1) + 5.0
c         x2(n) = x1(n)
c         CLOSE(kin)
c         DO i = 1, n-1
c            y1(i) = y1(i) * (x1(i+1)-x1(i))
c         ENDDO
c         CALL inter3(nw,wl,yg4,n,x1,y1,0)
c         DO iw = 1, nw-1
c            yg4(iw) = yg4(iw)/(wl(iw+1)-wl(iw))
c         ENDDO
c         DO i = 1, n-1
c            y2(i) = y2(i) * (x2(i+1)-x2(i))
c         ENDDO
c         CALL inter3(nw,wl,yg5,n,x2,y2,0)
c         DO iw = 1, nw-1
c            yg5(iw) = yg5(iw)/(wl(iw+1)-wl(iw))
c         ENDDO

      ELSE IF(mopt2 .EQ. 3) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH2O/CH2O_jpl97.dat',STATUS='old')
         DO i = 1, 4
            READ(kin,*) 
         ENDDO
         n = 23
         DO i = 1, n
            READ(kin,*) x1(i), dum, dum, dum, dum, y1(i), y2(i)
            x2(i) = x1(i)
         ENDDO
         CLOSE(kin)
         n1 = n
         n2 = n

         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),y1(1))
         CALL addpnt(x1,y1,kdata,n1,               0.,y1(1))
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
         CALL inter2(nw,wl,yg4,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

         CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),y2(1))
         CALL addpnt(x2,y2,kdata,n2,               0.,y2(1))
         CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
         CALL addpnt(x2,y2,kdata,n2,           1.e+38,0.)
         CALL inter2(nw,wl,yg5,n2,x2,y2,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ENDIF

* combine
* y1 = xsect
* y2 = xsect(223), Cantrell et al.
* y3 = xsect(293), Cantrell et al.
* y4 = qy for radical channel
* y5 = qy for molecular channel
* pressure and temperature dependent for w > 330.

      DO iw = 1, nw - 1

         sig = yg1(iw)

         DO i = 1, nz

* correct cross section for temperature dependence for > 301. nm
         
            IF (wl(iw) .GE. 301.) THEN 
               t = MAX(223.15, MIN(tlev(i), 293.15))
               IF (mopt1 .EQ. 3 .OR. mopt1 .EQ. 6) THEN
                  sig = yg2(iw) + yg3(iw) * (t - 273.15)

               ELSEIF (mopt1 .EQ. 4) THEN
                  slope = (yg3(iw) - yg2(iw)) / (293. - 223.)
                  sig = yg2(iw) + slope * (t - 223.)

               ENDIF

            ENDIF
            sig = MAX(sig, 0.)

* quantum yields:
* temperature and pressure dependence beyond 330 nm

            qy1 = yg4(iw)
            IF ( (wc(iw) .GE. 330.) .AND. (yg5(iw) .GT. 0.) ) THEN
               phi1 = yg4(iw)
               phi2 = yg5(iw)
               phi20 = 1. - phi1
               ak300=((1./phi2)-(1./phi20))/2.54E+19
               akt=ak300*(1.+61.69*(1.-tlev(i)/300.)*(wc(iw)/329.-1.))
               qy2 = 1. / ( (1./phi20) + airlev(i)*akt)

            ELSE
               qy2 = yg5(iw)
            ENDIF
            qy2 = MAX(0.,qy2)
            qy2 = MIN(1.,qy2)
            
            sq(j-1,i,iw) = sig * qy1
            sq(j  ,i,iw) = sig * qy2

         ENDDO
      ENDDO

      END
      SUBROUTINE r11(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CH3CHO photolysis: =*
*=      (a)  CH3CHO + hv -> CH3 + HCO                                        =*
*=      (b)  CH3CHO + hv -> CH4 + CO                                         =*
*=      (c)  CH3CHO + hv -> CH3CO + H                                        =*
*=  Cross section:  Choice between                                           =*
*=                   (1) IUPAC 97 data, from Martinez et al.                 =*
*=                   (2) Calvert and Pitts                                   =*
*=                   (3) Martinez et al., Table 1 scanned from paper         =*
*=                   (4) KFA tabulations                                     =*
*=  Quantum yields: Choice between                                           =*
*=                   (1) IUPAC 97, pressure correction using Horowith and    =*
*=                                 Calvert, 1982                             =*
*=                   (2) NCAR data file, from Moortgat, 1986                 =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:

      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=150)

      INTEGER i, n
      INTEGER n1, n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

* local

      REAL yg(kw), yg1(kw), yg2(kw), yg3(kw), yg4(kw)
      REAL qy1, qy2, qy3
      REAL sig
      REAL dum
      INTEGER ierr
      INTEGER  iz, iw

      INTEGER mabs, myld

****************************************************************
************************* CH3CHO photolysis
* 1:  CH3 + HCO
* 2:  CH4 + CO
* 3:  CH3CO + H


      j = j+1
      jlabel(j) = 'CH3CHO -> CH3 + HCO'
      j = j+1
      jlabel(j) = 'CH3CHO -> CH4 + CO'
      j = j+1
      jlabel(j) = 'CH3CHO -> CH3CO + H'

* options
* mabs for cross sections
* myld for quantum yields

* Absorption:
* 1:  IUPAC-97 data, from Martinez et al.
* 2:  Calvert and Pitts
* 3:  Martinez et al., Table 1 scanned from paper
* 4:  KFA tabulations, 6 choices, see file OPEN statements

* Quantum yield
* 1:  DATAJ1/CH3CHO/CH3CHO_iup.yld
* pressure correction using Horowitz and Calvert 1982, based on slope/intercepth
* of Stern-Volmer plots

* 2:  ncar data file, from Moortgat 1986.
*     DATAJ1/CH3CHO/d021_i.yld
*     DATAJ1/CH3CHO/d021_i.yld
*     DATAJ1/CH3CHO/d021_i.yld

      mabs = 3
      myld = 1

      IF (mabs .EQ. 1) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3CHO/CH3CHO_iup.abs',STATUS='old')
         do i = 1, 4
            read(kin,*)
         enddo
         n = 106
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
            y1(i) = y1(i) * 1.e-20
         ENDDO
         CLOSE(kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF(mabs .EQ. 2) THEN

* cross section from Calvert and  Pitts
         
         OPEN(UNIT=kin,FILE='DATAJ1/CH3CHO/d021_cp.abs',STATUS='old')
         DO i = 1, 14
            READ(kin,*)
         ENDDO
         n = 54
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
            x1(i) = x1(i)/10.
            y1(i) = y1(i) * 3.82E-21
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF(mabs .EQ. 3) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3CHO/CH3CHO_mar.abs',STATUS='old')
         DO i = 1, 3
            READ(kin,*)
         ENDDO
         n = 106
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF(mabs .EQ. 4) THEN

* cross section from KFA tables
* ch3cho.001 - Calvert and Pitts 1966
* ch3cho.002 - Meyrahn thesis 1984
* ch3cho.003 - Schneider and Moortgat, priv comm. MPI Mainz 1989, 0.012 nm resol.
* ch3cho.004 - Schneider and Moortgat, priv comm. MPI Mainz 1989, 0.08  nm resol.
* ch3cho.005 - IUPAC'92
* ch3cho.006 - Libuda, thesis Wuppertal 1992
         
c         OPEN(UNIT=kin,FILE='DATAJ2/KFA/ch3cho.001',STATUS='old')
C         n = 217
c         OPEN(UNIT=kin,FILE='DATAJ2/KFA/ch3cho.002',STATUS='old')
c         n = 63
c         OPEN(UNIT=kin,FILE='DATAJ2/KFA/ch3cho.003',STATUS='old')
c         n = 13738
c         OPEN(UNIT=kin,FILE='DATAJ2/KFA/ch3cho.004',STATUS='old')
c         n = 2053
         OPEN(UNIT=kin,FILE='DATAJ2/KFA/ch3cho.005',STATUS='old')
         n = 18
c         OPEN(UNIT=kin,FILE='DATAJ2/KFA/ch3cho.006',STATUS='old')
c         n = 1705

         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ENDIF

* quantum yields

      IF (myld .EQ. 1) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3CHO/CH3CHO_iup.yld',STATUS='old')
         do i = 1, 4
            read(kin,*)
         enddo
         n = 12
         DO i = 1, n
            READ(kin,*) x1(i), y2(i), y1(i)
            x2(i) = x1(i)
         ENDDO
         CLOSE(kin)
         n1 = n
         n2 = n

         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,               0.,0.)
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
         CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

         CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
         CALL addpnt(x2,y2,kdata,n2,               0.,0.)
         CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
         CALL addpnt(x2,y2,kdata,n2,           1.e+38,0.)
         CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

         DO iw = 1, nw-1
            yg3(iw) = 0.
         ENDDO

      ELSEIF (myld .EQ. 2) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3CHO/d021_i.yld',STATUS='old')
         DO i = 1, 18
            READ(kin,*)
         ENDDO
         n = 10
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),y1(1))
         CALL addpnt(x1,y1,kdata,n,               0.,y1(1))
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg1,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF
      
         OPEN(UNIT=kin,FILE='DATAJ1/CH3CHO/d021_ii.yld',STATUS='old')
         DO i = 1, 10
            READ(kin,*)
         ENDDO
         n = 9
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),y1(1))
         CALL addpnt(x1,y1,kdata,n,               0.,y1(1))
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg2,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

         OPEN(UNIT=kin,FILE='DATAJ1/CH3CHO/d021_iii.yld',STATUS='old')
         DO i = 1, 10
            READ(kin,*)
         ENDDO
         n = 9
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),y1(1))
         CALL addpnt(x1,y1,kdata,n,               0.,y1(1))
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg3,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ENDIF

* pressure-dependence parameters
      
         OPEN(UNIT=kin,FILE='DATAJ1/CH3CHO/CH3CHO_press.yld',
     $     STATUS='old')
         do i = 1, 4
            read(kin,*)
         enddo
         n = 5
         DO i = 1, n
            READ(kin,*) x1(i), dum, dum, y1(i)
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg4,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

* combine:

      DO iw = 1, nw - 1
         DO i = 1, nz

            sig = yg(iw)

* quantum yields:

            qy1 = yg1(iw)
            qy2 = yg2(iw)
            qy3 = yg3(iw)

* pressure correction for channel 1, CH3 + CHO
* based on Horowitz and Calvert 1982.

            qy1 = qy1 * (1. + yg4(iw))/(1. + yg4(iw)*airlev(i)/2.465E19)
            qy1 = MIN(1., qy1)
            qy1 = MAX(0., qy1)

            sq(j-2,i,iw) = sig * qy1
            sq(j-1,i,iw) = sig * qy2
            sq(j  ,i,iw) = sig * qy3

         ENDDO
      ENDDO

      END
      SUBROUTINE r12(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide the product (cross section) x (quantum yield) for C2H5CHO        =*
*=  photolysis:                                                              =*
*=         C2H5CHO + hv -> C2H5 + HCO                                        =*
*=                                                                           =*
*=  Cross section:  Choice between                                           =*
*=                   (1) IUPAC 97 data, from Martinez et al.                 =*
*=                   (2) Calvert and Pitts, as tabulated by KFA              =*
*=  Quantum yield:  IUPAC 97 recommendation                                  =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:

      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=150)

      INTEGER i, n
      INTEGER n1
      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw), yg1(kw)
      REAL qy1
      REAL sig
      INTEGER ierr
      INTEGER iw

      INTEGER mabs, myld

************************* C2H5CHO photolysis
* 1:  C2H5 + HCO

      j = j+1
      jlabel(j) = 'C2H5CHO -> C2H5 + HCO'

* options
* mabs for cross sections
* myld for quantum yields

* Absorption:
* 1:  IUPAC-97 data, from Martinez et al.
* 2:  Calvert and Pitts, as tabulated by KFA.

* Quantum yield
* 1:  IUPAC-97 data

      mabs = 1
      myld = 1

      IF (mabs .EQ. 1) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/C2H5CHO/C2H5CHO_iup.abs',
     $        STATUS='old')
         do i = 1, 4
            read(kin,*)
         enddo
         n = 106
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
            y1(i) = y1(i) * 1.e-20
         ENDDO
         CLOSE(kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF(mabs .EQ. 2) THEN

* cross section from KFA tables
* c2h5cho.001 - Calvert and Pitts 1966
         
         OPEN(UNIT=kin,FILE='DATAJ2/KFA/c2h5cho.001',STATUS='old')
         n = 83

         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ENDIF

* quantum yields

      IF (myld .EQ. 1) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/C2H5CHO/C2H5CHO_iup.yld',
     $        STATUS='old')
         do i = 1, 4
            read(kin,*)
         enddo
         n = 5
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE(kin)
         n1 = n

         CALL addpnt(x1,y1,kdata,n1,340.,0.)

         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,               0.,0.)
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
         CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF (myld .EQ. 2) THEN

         STOP

      ENDIF

* combine:

      DO iw = 1, nw - 1
         DO i = 1, nz

            sig = yg(iw)

* quantum yields:

            qy1 = yg1(iw)

            sq(j  ,i,iw) = sig * qy1

         ENDDO
      ENDDO

      END
      SUBROUTINE r13(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide the product (cross section) x (quantum yield) for CHOCHO         =*
*=  photolysis:                                                              =*
*=              CHOCHO + hv -> Products                                      =*
*=                                                                           =*
*=  Cross section: Choice between                                            =*
*=                  (1) Plum et al., as tabulated by IUPAC 97                =*
*=                  (2) Plum et al., as tabulated by KFA.                    =*
*=  Quantum yield: IUPAC 97 recommendation                                   =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:

      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=250)

      INTEGER i, n
      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      REAL sig
      INTEGER ierr
      INTEGER iw

      INTEGER mabs, myld

************************* CHOCHO photolysis
* 1:  CHOCHO

      j = j+1
      jlabel(j) = 'CHOCHO -> products'

* options
* mabs for cross sections
* myld for quantum yields

* Absorption:
* 1:  Plum et al., as tabulated by IUPAC-97
* 2:  Plum et al., as tabulated by KFA.

* Quantum yield
* 1:  IUPAC-97 data

      mabs = 1
      myld = 1

      IF (mabs .EQ. 1) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CHOCHO/CHOCHO_iup.abs',
     $        STATUS='old')
         do i = 1, 4
            read(kin,*)
         enddo
         n = 110
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
            y1(i) = y1(i) * 1.e-20
         ENDDO
         CLOSE(kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF(mabs .EQ. 2) THEN

* cross section from KFA tables
* chocho.001 - Plum et al. 1983
         
         OPEN(UNIT=kin,FILE='DATAJ2/KFA/chocho.001',STATUS='old')
         n = 219

         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ENDIF

* quantum yields

* combine:

      DO iw = 1, nw - 1

         DO i = 1, nz

            sig = yg(iw)

* quantum yields:

            if(wc(iw) .lt. 325.) then
               qy = 0.4
            else
               qy = 0.029
            endif

            sq(j,i,iw) = sig * qy

         ENDDO
      ENDDO

      END
      SUBROUTINE r14(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide the product (cross section) x (quantum yield) for CH3COCHO       =*
*=  photolysis:                                                              =*
*=           CH3COCHO + hv -> Products                                       =*
*=                                                                           =*
*=  Cross section: Choice between                                            =*
*=                  (1) from Meller et al., 1991, as tabulated by IUPAC 97   =*
*=                         5 nm resolution (table 1) for < 402 nm            =*
*=                         2 nm resolution (table 2) for > 402 nm            =*
*=                  (2) average at 1 nm of Staffelbach et al., 1995, and     =*
*=                      Meller et al., 1991                                  =*
*=                  (3) Plum et al., 1983, as tabulated by KFA	             =*
*=                  (4) Meller et al., 1991 (0.033 nm res.), as tab. by KFA  =*
*=                  (5) Meller et al., 1991 (1.0 nm res.), as tab. by KFA    =*
*=                  (6) Staffelbach et al., 1995, as tabulated by KFA        =*
*=  Quantum yield: Choice between                                            =*
*=                  (1) Plum et al., fixed at 0.107                          =*
*=                  (2) Plum et al., divided by 2, fixed at 0.0535           =*
*=                  (3) Staffelbach et al., 0.45 for < 300 nm, 0 for > 430 nm=*
*=                      linear interp. in between                            =*
*=                  (4) Koch and Moortgat, prv. comm., 1997                  =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:

      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=300)

      INTEGER i, n
      INTEGER n1, n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

* local

      REAL yg(kw), yg1(kw), yg2(kw)
      REAL qy
      REAL sig
      INTEGER ierr
      INTEGER iw

      INTEGER mabs, myld

************************* CH3COCHO photolysis
* 1:  CH3COCHO

      j = j+1
      jlabel(j) = 'CH3COCHO -> products'

* options
* mabs for cross sections
* myld for quantum yields

* Absorption:
* 1:  from Meller et al. (1991), as tabulated by IUPAC-97
*     for wc < 402, use coarse data (5 nm, table 1)
*     for wc > 402, use finer data (2 nm, table 2)
* 2: average at 1nm of  Staffelbach et al. 1995 and Meller et al. 1991
*     Cross section from KFA tables:
* 3: ch3cocho.001 - Plum et al. 1983
* 4: ch3cocho.002 - Meller et al. 1991, 0.033 nm resolution
* 5: ch3cocho.003 - Meller et al. 1991, 1.0   nm resolution
* 6: ch3cocho.004 - Staffelbach et al. 1995

* Quantum yield
* 1:  Plum et al., 0.107
* 2:  Plum et al., divided by two = 0.0535
* 3:  Staffelbach et al., 0.45 at wc .le. 300, 0 for wc .gt. 430, linear 
*     interpl in between
* 4:  Koch and Moortgat, prv. comm. 1997. - pressure-dependent

      mabs = 2
      myld = 3

      IF (mabs .EQ. 1) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3COCHO/CH3COCHO_iup1.abs',
     $        STATUS='old')
         do i = 1, 4
            read(kin,*)
         enddo
         n = 38
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
            y1(i) = y1(i) * 1.e-20
         ENDDO
         CLOSE(kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg1,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

         OPEN(UNIT=kin,FILE='DATAJ1/CH3COCHO/CH3COCHO_iup2.abs',
     $        STATUS='old')
         do i = 1, 4
            read(kin,*)
         enddo
         n = 75
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
            y1(i) = y1(i) * 1.e-20
         ENDDO
         CLOSE(kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg2,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

         DO iw = 1, nw-1 
            IF(wc(iw) .LT. 402.) THEN
               yg(iw) = yg1(iw)
            ELSE
               yg(iw) = yg2(iw)
            ENDIF               
         ENDDO

      ELSEIF(mabs .EQ. 2) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3COCHO/CH3COCHO_ncar.abs',
     $        STATUS='old')
         n = 271
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE(kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF(mabs .GT. 2) THEN

* cross section from KFA tables
* ch3cocho.001 - Plum et al. 1983
* ch3cocho.002 - Meller et al. 1991, 0.033 nm resolution
* ch3cocho.003 - Meller et al. 1991, 1.0   nm resolution
* ch3cocho.004 - Staffelbach et al. 1995
         
         IF(mabs .EQ. 3) THEN
            OPEN(UNIT=kin,FILE='DATAJ2/KFA/ch3cocho.001',STATUS='old')
            n = 136
         ELSEIF(mabs .EQ. 4) THEN
            OPEN(UNIT=kin,FILE='DATAJ2/KFA/ch3cocho.002',STATUS='old')
            n = 8251
         ELSEIF(mabs .EQ. 5) THEN
            OPEN(UNIT=kin,FILE='DATAJ2/KFA/ch3cocho.003',STATUS='old')
            n = 275
         ELSEIF(mabs .EQ. 6) THEN
            OPEN(UNIT=kin,FILE='DATAJ2/KFA/ch3cocho.004',STATUS='old')
            n = 162
         ENDIF
         
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ENDIF

* quantum yields

         IF(myld .EQ. 4) THEN
            OPEN(UNIT=kin,FILE='DATAJ1/CH3COCHO/CH3COCHO_km.yld',
     $           STATUS='old')
            DO i = 1, 5
               READ(kin,*)
            ENDDO
            n = 5
            DO i = 1, n
               READ(kin,*) x1(i), y1(i), y2(i)
               x2(i) = x1(i)
            ENDDO
            CLOSE (kin)
            n1 = n
            n2 = n

            CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),1.)
            CALL addpnt(x1,y1,kdata,n1,               0.,1.)
            CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
            CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
            CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)
            IF (ierr .NE. 0) THEN
               WRITE(*,*) ierr, jlabel(j)
               STOP
            ENDIF

            CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),1.)
            CALL addpnt(x2,y2,kdata,n2,               0.,1.)
            CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
            CALL addpnt(x2,y2,kdata,n2,           1.e+38,0.)
            CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
            IF (ierr .NE. 0) THEN
               WRITE(*,*) ierr, jlabel(j)
               STOP
            ENDIF

         ENDIF

* combine:

      DO iw = 1, nw - 1

         sig = yg(iw)

         DO i = 1, nz

* quantum yields:

            IF (myld .EQ. 1) THEN
               qy = 0.107

            ELSEIF(myld .EQ. 2) THEN
               qy = 0.107/2.

            ELSEIF(myld .EQ. 3) THEN
               IF(wc(iw) .LE. 300.) THEN
                  qy = 0.45
               ELSE IF (wc(iw) .GE. 430.) THEN 
                  qy = 0.
               ELSE
                  qy = 0.45 + (0-0.45)*(wc(iw)-300.)/(430.-300.)
               ENDIF

            ELSEIF(myld .EQ. 4) THEN

               IF (yg1(iw) .GT. 0.) THEN

                  qy = yg2(iw)/( 1. + (airlev(i)/2.465E19) 
     $                 * ( (yg2(iw)/yg1(iw)) - 1.))

               ELSE
                  qy = 0.
               ENDIF
               
            ENDIF

            sq(j,i,iw) = sig * qy

         ENDDO
      ENDDO

      END
      SUBROUTINE r15(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CH3COCH3 photolysis=*
*=          CH3COCH3 + hv -> Products                                        =*
*=                                                                           =*
*=  Cross section:  Choice between                                           =*
*=                   (1) Calvert and Pitts                                   =*
*=                   (2) Martinez et al., 1991, alson in IUPAC 97            =*
*=                   (3) NOAA, 1998, unpublished as of 01/98                 =*
*=  Quantum yield:  Choice between                                           =*
*=                   (1) Gardiner et al, 1984                                =*
*=                   (2) IUPAC 97                                            =*
*=                   (3) McKeen et al., 1997                                 =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:

      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=150)

      INTEGER i, n
      INTEGER n1, n2, n3
      REAL x1(kdata), x2(kdata), x3(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata)

* local

      REAL yg(kw), yg1(kw), yg2(kw), yg3(kw)
      REAL qy
      REAL sig
      INTEGER ierr
      INTEGER iw

      REAL a, b, t
      INTEGER mabs, myld
**************** CH3COCH3 photodissociation

      j = j + 1
      jlabel(j) = 'CH3COCH3 '

* options
* mabs for cross sections
* myld for quantum yields

* Absorption:
* 1:  cross section from Calvert and  Pitts
* 2:  Martinez et al. 1991, also in IUPAC'97
* 3:  NOAA 1998, unpublished as of Jan 98.

* Quantum yield
* 1:  Gardiner et al. 1984
* 2:  IUPAC 97
* 3:  McKeen, S. A., T. Gierczak, J. B. Burkholder, P. O. Wennberg, T. F. Hanisco,
*       E. R. Keim, R.-S. Gao, S. C. Liu, A. R. Ravishankara, and D. W. Fahey, 
*       The photochemistry of acetone in the upper troposphere:  a source of 
*       odd-hydrogen radicals, Geophys. Res. Lett., 24, 3177-3180, 1997.

      mabs = 2
      myld = 3

      IF (mabs .EQ. 1) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3COCH3/CH3COCH3_cp.abs',
     $        STATUS='old')
         DO i = 1, 6
            READ(kin,*)
         ENDDO
         n = 35
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
            y1(i) = y1(i) * 3.82E-21
         ENDDO
         CLOSE (kin)
         
         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF(mabs .EQ. 2) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3COCH3/CH3COCH3_iup.abs',
     $        STATUS='old')
         DO i = 1, 4
            READ(kin,*)
         ENDDO
         n = 96
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
            y1(i) = y1(i) * 1.e-20
         ENDDO
         CLOSE (kin)
         
         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF(mabs .EQ. 3) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3COCH3/CH3COCH3_noaa.abs',
     $        STATUS='old')
         DO i = 1, 12
            READ(kin,*)
         ENDDO
         n = 135
         DO i = 1, n
            READ(kin,*) x1(i), y1(i), y2(i), y3(i)
            x2(i) = x1(i)
            x3(i) = x1(i)
         ENDDO
         CLOSE (kin)
         n1 = n
         n2 = n
         n3 = n
         
         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,               0.,0.)
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

         CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
         CALL addpnt(x2,y2,kdata,n2,               0.,0.)
         CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
         CALL addpnt(x2,y2,kdata,n2,           1.e+38,0.)
         CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF


         CALL addpnt(x3,y3,kdata,n3,x3(1)*(1.-deltax),0.)
         CALL addpnt(x3,y3,kdata,n3,               0.,0.)
         CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
         CALL addpnt(x3,y3,kdata,n3,           1.e+38,0.)
         CALL inter2(nw,wl,yg3,n3,x3,y3,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ENDIF

      IF (myld .EQ. 2) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3COCH3/CH3COCH3_iup.yld',
     $        STATUS='old')
         DO i = 1, 4
            READ(kin,*)
         ENDDO
         n = 9
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)
         
         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg1,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ENDIF

      DO iw = 1, nw - 1

         DO i = 1, nz

            sig = yg(iw)

            IF(mabs .EQ. 3) THEN
               t = 298. - tlev(i)
               t = MIN(t, 298.-235.)
               t = MAX(t, 0.)

               sig = yg(iw)*(1. + yg2(iw)*t + yg3(iw)*t*t)

            ENDIF

            IF (myld .EQ. 1) THEN
               qy = 0.0766 + 0.09415*EXP(-airlev(i)/3.222e18)

            ELSEIF (myld .EQ. 2) THEN
               qy = yg1(iw)

            ELSEIF (myld .EQ. 3) THEN
               IF (wc(iw) .LE. 292.) THEN
                  qy = 1.
               ELSEIF (wc(iw) .GE. 292.  .AND. wc(iw) .LT. 308. ) THEN
                  a = -15.696 + 0.05707*wc(iw)
                  b = EXP(-88.81+0.15161*wc(iw))
                  qy = 1./(a + b*airlev(i))
               ELSEIF (wc(iw) .GE. 308.  .AND. wc(iw) .LT. 337. ) THEN
                  a = -130.2 + 0.42884*wc(iw)
                  b = EXP(-55.947+0.044913*wc(iw))
                  qy = 1./(a + b*airlev(i))
               ELSEIF (wc(iw) .GE. 337.) THEN
                  qy = 0.
               ENDIF

               qy = max(0., qy)
               qy = min(1., qy)

            ENDIF

            sq(j,i,iw) = sig*qy

         ENDDO
      ENDDO

      END
      SUBROUTINE r16(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CH3OOH photolysis: =*
*=         CH3OOH + hv -> CH3O + OH                                          =*
*=                                                                           =*
*=  Cross section: Choice between                                            =*
*=                  (1) JPL 97 recommendation (based on Vaghjiana and        =*
*=                      Ravishankara, 1989), 10 nm resolution                =*
*=                  (2) IUPAC 97 (from Vaghjiana and Ravishankara, 1989),    =*
*=                      5 nm resolution                                      =*
*=                  (3) Cox and Tyndall, 1978; only for wavelengths < 280 nm =*
*=                  (4) Molina and Arguello, 1979;  might be 40% too high    =*
*=  Quantum yield: Assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:

      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      INTEGER i, n
      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER ierr
      INTEGER idum
      INTEGER iw

      INTEGER mabs


**************** CH3OOH photodissociation
         j = j + 1
         jlabel(j) = 'CH3OOH -> CH3O + OH'

* mabs: Absorption cross section options:
* 1:  JPL data base (1985,92,94,97). 1997 is from  Vaghjiani and Ravishankara (1989), at
*     10 nm resolution
* 2:  IUPAC97 (from  Vaghjiani and Ravishankara (1989) at 5 nm resolution).
* 3:  Cox and Tyndall (1978), only for wavelengths < 280 nm
* 4:  Molina and Arguello (1979).  According to Vaghjiani and Ravishankara (1989), 
*     Molina and Arguello had a problem measuring CH3OOH, cross sections 40% too high.

      mabs = 2

      IF (mabs .EQ. 1) THEN

c         OPEN(UNIT=kin,FILE='DATAJ1/CH3OOH/CH3OOH_jpl85.abs',
c     $        STATUS='old')
c         OPEN(UNIT=kin,FILE='DATAJ1/CH3OOH/CH3OOH_jpl92.abs',
c     $        STATUS='old')
c         OPEN(UNIT=kin,FILE='DATAJ1/CH3OOH/CH3OOH_jpl94.abs',
c     $        STATUS='old')
         OPEN(UNIT=kin,FILE='DATAJ1/CH3OOH/CH3OOH_jpl94.abs',
     $        STATUS='old')
         READ(kin,*) idum, n
         DO i = 1, idum-2
            READ(kin,*)
         ENDDO
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
            y1(i) = y1(i) * 1.E-20
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF (mabs .EQ. 2) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3OOH/CH3OOH_iup.abs',
     $        STATUS='old')
         DO i = 1, 4
            READ(kin,*)
         ENDDO
         n = 32
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
            y1(i) = y1(i) * 1.E-20
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF (mabs .EQ. 3) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3OOH/CH3OOH_ct.abs',
     $        STATUS='old')
         DO i = 1, 4
            READ(kin,*)
         ENDDO
         n = 12
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF (mabs .EQ. 4) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/CH3OOH/CH3OOH_ma.abs',
     $        STATUS='old')
         DO i = 1, 4
            READ(kin,*)
         ENDDO
         n = 15
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n,               0.,0.)
         CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ENDIF

* quantum yield = 1

      qy = 1.
      DO iw = 1, nw - 1

         DO i = 1, nz
            sq(j,i,iw) = yg(iw)*qy
         ENDDO
      ENDDO

      END
      SUBROUTINE r17(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CH3ONO2            =*
*=  photolysis:                                                              =*
*=          CH3ONO2 + hv -> CH3O + NO2                                       =*
*=                                                                           =*
*=  Cross section: Choice between                                            =*
*=                  (1) Calvert and Pitts, 1966                              =*
*=                  (2) Talukdar, Burkholder, Hunter, Gilles, Roberts,       =*
*=                      Ravishankara, 1997                                   =*
*=                  (3) IUPAC 97, table of values for 198K                   =*
*=                  (4) IUPAC 97, temperature-dependent equation             =*
*=                  (5) Taylor et al, 1980                                   =*
*=                  (6) fit from Roberts and Fajer, 1989                     =*
*=                  (7) Rattigan et al., 1992                                =*
*=                  (8) Libuda and Zabel, 1995                               =*
*=  Quantum yield: Assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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


      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:

      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER (kdata = 2000)

      INTEGER i, n
      INTEGER iw
      INTEGER n1, n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

* local

      REAL yg(kw), yg1(kw)
      REAL qy
      REAL sig
      INTEGER ierr

      INTEGER mabs, myld

**************** CH3ONO2 photodissociation

      j = j + 1
      jlabel(j) = 'CH3ONO2 -> CH3O+NO2'

* mabs: absorption cross section options:
* 1:  Calvert and  Pitts 1966
* 2:  Talukdar, Burkholder, Hunter, Gilles, Roberts, Ravishankara, 1997.
* 3:  IUPAC-97, table of values for 298K.
* 4:  IUPAC-97, temperature-dependent equation
* 5:  Taylor et al. 1980
* 6:  fit from Roberts and Fajer, 1989
* 7:  Rattigan et al. 1992
* 8:  Libuda and Zabel 1995

      mabs = 2

      IF (mabs .EQ. 1) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/RONO2/CH3ONO2_cp.abs',STATUS='old')
         DO i = 1, 3
            READ(kin,*)
         ENDDO
         n = 15
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         n1 = n
         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,               0.,0.)
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF(mabs .EQ. 2) THEN

*        sigma(T,lambda) = sigma(298,lambda) * exp(B * (T-298))

         OPEN(UNIT=kin,FILE='DATAJ1/RONO2/CH3ONO2_tal.abs',STATUS='old')
         DO i = 1, 4
            READ(kin,*)
         ENDDO
         n = 55
         DO i = 1, n
            READ(kin,*) x1(i), y1(i), y2(i)
            x2(i) = x1(i)
            y1(i) = y1(i) * 1.e-20
         ENDDO
         CLOSE (kin)

         n1 = n
         n2 = n
         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,               0.,0.)
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

         CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),y2(1))
         CALL addpnt(x2,y2,kdata,n2,               0.,y2(1))
         CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),y2(n2))
         CALL addpnt(x2,y2,kdata,n2,            1.e+38,y2(n2))
         CALL inter2(nw,wl,yg1,n2,x2,y2,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF (mabs .EQ. 3) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/RONO2/CH3ONO2_iup1.abs',
     $        STATUS='old')
         DO i = 1, 4
            READ(kin,*)
         ENDDO
         n = 13
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
            y1(i) = y1(i)*1e-20
         ENDDO
         CLOSE (kin)

         n1 = n
         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,               0.,0.)
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF(mabs .EQ. 4) THEN

*        sigma(T,lambda) = sigma(298,lambda) * 10**(B * T)

         OPEN(UNIT=kin,FILE='DATAJ1/RONO2/CH3ONO2_iup2.abs',
     $        STATUS='old')
         DO i = 1, 4
            READ(kin,*)
         ENDDO
         n = 7
         DO i = 1, n
            READ(kin,*) x1(i), y1(i), y2(i)
            x2(i) = x1(i)
            y1(i) = y1(i) * 1.e-21
            y2(i) = y2(i) * 1.e-3
         ENDDO
         CLOSE (kin)

         n1 = n
         n2 = n
         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),-36.)
         CALL addpnt(x1,y1,kdata,n1,               0.,-36.)
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),-36.)
         CALL addpnt(x1,y1,kdata,n1,           1.e+38,-36.)
         CALL inter2(nw,wl,yg,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

         CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),y2(1))
         CALL addpnt(x2,y2,kdata,n2,               0.,y2(1))
         CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),y2(n2))
         CALL addpnt(x2,y2,kdata,n2,            1.e+38,y2(n2))
         CALL inter2(nw,wl,yg1,n2,x2,y2,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF (mabs .EQ. 5) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/RONO2/CH3ONO2_tay.abs',
     $        STATUS='old')
         DO i = 1, 4
            READ(kin,*)
         ENDDO
         n = 13
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         n1 = n
         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,               0.,0.)
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF (mabs .EQ. 6) THEN

         DO iw = 1, nw-1
            IF(wc(iw) .GT. 284.) THEN
               yg(iw) = EXP(-1.044e-3*wc(iw)*wc(iw) + 
     $              0.5309*wc(iw) - 112.4)
            ELSE
               yg(iw) = 0.
            ENDIF
         ENDDO

      ELSEIF (mabs .EQ. 7) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/RONO2/CH3ONO2_rat.abs',
     $        STATUS='old')
         DO i = 1, 4
            READ(kin,*)
         ENDDO
         n = 24
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         n1 = n
         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,               0.,0.)
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ELSEIF (mabs .EQ. 8) THEN

         OPEN(UNIT=kin,FILE='DATAJ1/RONO2/CH3ONO2_lib.abs',
     $        STATUS='old')
         DO i = 1, 4
            READ(kin,*)
         ENDDO
         n = 1638
         DO i = 1, n
            READ(kin,*) x1(i), y1(i)
         ENDDO
         CLOSE (kin)

         n1 = n
         CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,               0.,0.)
         CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
         CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
         CALL inter2(nw,wl,yg,n1,x1,y1,ierr)
         IF (ierr .NE. 0) THEN
            WRITE(*,*) ierr, jlabel(j)
            STOP
         ENDIF

      ENDIF

* quantum yield = 1

      qy = 1.

      DO iw = 1, nw - 1
         sig = yg(iw)

         DO i = 1, nz
            
            IF(mabs .EQ. 2) THEN
               sig = yg(iw) * exp (yg1(iw) * (tlev(i)-298.))

            ELSEIF (mabs .EQ. 4) THEN
               sig = yg(iw)*10.**(yg1(iw)*tlev(i))

            ENDIF

            sq(j,i,iw) = qy * sig

         ENDDO
      ENDDO

      END
      SUBROUTINE r18(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for PAN photolysis:    =*
*=       PAN + hv -> Products                                                =*
*=                                                                           =*
*=  Cross section: from Talukdar et al., 1995                                =*
*=  Quantum yield: Assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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


      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:

      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      INTEGER iw
      INTEGER i, n
      INTEGER n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

* local

      REAL yg(kw), yg2(kw)
      REAL qy
      REAL sig
      INTEGER ierr

**************** PAN photodissociation

      j = j+1
      jlabel(j) = 'PAN + hv -> products'

* cross section from Senum et al., 1984, J.Phys.Chem. 88/7, 1269-1270

C     OPEN(UNIT=kin,FILE='DATAJ1/RONO2/PAN_senum.abs',STATUS='OLD')
C     DO i = 1, 14
C        READ(kin,*)
C     ENDDO
C     n = 21
C     DO i = 1, n
C        READ(kin,*) x1(i), y1(i)
C        y1(i) = y1(i) * 1.E-20
C     ENDDO
C     CLOSE(kin)

C      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
C      CALL addpnt(x1,y1,kdata,n,               0.,0.)
C      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
C      CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
C      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
C      IF (ierr .NE. 0) THEN
C         WRITE(*,*) ierr, jlabel(j)
C         STOP
C      ENDIF

* cross section from 
*      Talukdar et al., 1995, J.Geophys.Res. 100/D7, 14163-14174

      OPEN(UNIT=kin,FILE='DATAJ1/RONO2/PAN_talukdar.abs',STATUS='OLD')
      DO i = 1, 14
         READ(kin,*)
      ENDDO
      n = 78
      DO i = 1, n
         READ(kin,*) x1(i), y1(i), y2(i)
         y1(i) = y1(i) * 1.E-20
         y2(i) = y2(i) * 1E-3
         x2(i) = x1(i)
      ENDDO
      n2 = n
      CLOSE(kin)
 
      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,               0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

      CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,          0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,      1.e+38,0.)
      CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

* quantum yield
* yet unknown, but assumed to be 1.0 (Talukdar et al., 1995)

      qy = 1.0

      DO iw = 1, nw-1
        DO i = 1, nz

          sig = yg(iw) * EXP(yg2(iw)*(tlev(i)-298.))

          sq(j,i,iw) = qy * sig

        ENDDO
      ENDDO 

      END
      SUBROUTINE r19(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CCl2O photolysis:  =*
*=        CCl2O + hv -> Products                                             =*
*=                                                                           =*
*=  Cross section: JPL 94 recommendation                                     =*
*=  Quantum yield: Unity (Calvert and Pitts)                                 =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

************* CCl2O photodissociation

      j = j+1
      jlabel(j) = 'CCl2O + hv -> Products'

*** cross sections from JPL94 recommendation

      OPEN(kin,FILE='DATAJ1/ABS/CCl2O_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

*** quantum yield unity (Calvert and Pitts)
      qy = 1.
      DO iw = 1, nw-1
        DO iz = 1, nz
           sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r2(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide the product (cross section) x (quantum yield) for NO2            =*
*=  photolysis:                                                              =*
*=         NO2 + hv -> NO + O(3P)                                            =*
*=  Cross section from JPL94 (can also have Davidson et al.)                 =*
*=  Quantum yield from Gardiner, Sperry, and Calvert                         =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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


      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      INTEGER n1
      REAL x1(kdata), x2(kdata), x3(kdata)
      REAL y1(kdata), y2(kdata)

* local

      REAL yg1(kw), yg2(kw)
      REAL xsno2(kz,kw)
      REAL dum
      INTEGER i, iw, n, idum, ierr


**************** NO2 photodissociation

      j = j + 1
      jlabel(j) = 'NO2 -> NO + O(3P)'

* cross section

*------------NEED TO CHANGE kdata = 1000 FOR DAVIDSON ET AL. DATA---------
*     measurements of Davidson et al. (198x) at 273K
*     from 263.8 to 648.8 nm in approximately 0.5 nm intervals
C     OPEN(UNIT=kin,FILE='DATAE1/NO2/NO2_ncar_00.abs',STATUS='old')
C     n = 750
C     DO i = 1, n
C        READ(kin,*) x1(i), y1(i), dum, dum, idum
C     ENDDO
C     CLOSE(kin)

C     CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
C     CALL addpnt(x1,y1,kdata,n,               0.,0.)
C     CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
C     CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
C     CALL inter2(nw,wl,yg,n,x1,y1,ierr)
C     IF (ierr .NE. 0) THEN
C        WRITE(*,*) ierr, jlabel(j)
C        STOP
C     ENDIF

* cross section data from JPL 94 recommendation
* JPL 97 recommendation is identical

      OPEN(UNIT=kin,FILE='DATAE1/NO2/NO2_jpl94.abs',STATUS='old')
      READ(kin,*) idum, n
      DO i = 1, idum-2
         READ(kin,*)
      ENDDO 

* read in wavelength bins, cross section at T0 and temperature correction
* coefficient a;  see input file for details.
* data need to be scaled to total area per bin so that they can be used with
* inter3

      DO i = 1, n
         READ(kin,*) x1(i), x3(i), y1(i), dum, y2(i)
         y1(i) = (x3(i)-x1(i)) * y1(i)*1.E-20
         y2(i) = (x3(i)-x1(i)) * y2(i)*1.E-22
         x2(i) = x1(i) 
      ENDDO
      CLOSE(kin)

      x1(n+1) = x3(n)
      x2(n+1) = x3(n)
      n = n+1
      n1 = n

      CALL inter3(nw,wl,yg1,n,x1,y1,0)

      CALL inter3(nw,wl,yg2,n1,x2,y2,0)

* yg1, yg2 are per nm, so rescale by bin widths

      DO iw = 1, nw-1
        yg1(iw) = yg1(iw)/(wl(iw+1)-wl(iw))
        yg2(iw) = yg2(iw)/(wl(iw+1)-wl(iw))
      ENDDO

      DO iw = 1, nw-1
        DO i = 1, nz
           xsno2(i,iw) = yg1(iw) + yg2(iw)*(tlev(i)-273.15)
        ENDDO
      ENDDO 

* quantum yield
* from Gardiner, Sperry, and Calvert

      OPEN(UNIT=kin,FILE='DATAJ1/YLD/NO2_calvert.yld',STATUS='old')
      DO i = 1, 8
         READ(kin,*) 
      ENDDO
      n = 66
      DO i = 1, n
         READ(kin,*) x1(i),y1(i)
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),y1(1))
      CALL addpnt(x1,y1,kdata,n,               0.,y1(1))
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),   0.)
      CALL addpnt(x1,y1,kdata,n,           1.e+38,   0.)
      CALL inter2(nw,wl,yg1,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

* combine

      DO iw = 1, nw - 1
         DO i = 1, nz
            sq(j,i,iw) = xsno2(i,iw)*yg1(iw)
         ENDDO
      ENDDO

      END
      SUBROUTINE r20(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CCl4 photolysis:   =*
*=      CCl4 + hv -> Products                                                =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CCl4 photodissociation
      
      j = j+1
      jlabel(j) = 'CCl4 + hv -> Products'

*** cross sections from JPL97 recommendation (identical to 94 data)

      OPEN(kin,FILE='DATAJ1/ABS/CCl4_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

*** quantum yield assumed to be unity
      qy = 1.
      DO iw = 1, nw-1
        DO iz = 1, nz
           sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r21(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CClFO photolysis:  =*
*=         CClFO + hv -> Products                                            =*
*=  Cross section: from JPL 97                                               =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CClFO photodissociation

      j = j+1
      jlabel(j) = 'CClFO + hv -> Products'

*** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/CClFO_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

*** quantum yield unity
      qy = 1.
      DO iw = 1, nw-1
        DO iz = 1, nz
          sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r22(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CF2O photolysis:   =*
*=        CF2O + hv -> Products                                              =*
*=  Cross section:  from JPL 97 recommendation                               =*
*=  Quantum yield:  unity (Nolle et al.)                                     =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CF2O photodissociation

      j = j+1
      jlabel(j) = 'CF2O + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/CF2O_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

*** quantum yield unity (Nolle et al.)
      qy = 1.
      DO iw = 1, nw-1
        DO iz = 1, nz
           sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r23(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CFC-113 photolysis:=*
*=          CF2ClCFCl2 + hv -> Products                                      =*
*=  Cross section:  from JPL 97 recommendation, linear interp. between       =*
*=                  values at 210 and 295K                                   =*
*=  Quantum yield:  assumed to be unity                                      =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      INTEGER n1, n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

* local

      REAL yg1(kw), yg2(kw)
      REAL qy
      REAL t
      INTEGER i, iw, n, idum
      INTEGER iz
      INTEGER ierr
      REAL slope

**************************************************************
************* CF2ClCFCl2 (CFC-113) photodissociation

      j = j+1
      jlabel(j) = 'CF2ClCFCl2 (CFC-113) + hv -> Products'

*** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/CFC-113_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i), y2(i)
        y1(i) = y1(i) * 1E-20
        y2(i) = y2(i) * 1E-20
        x2(i) = x1(i)
      ENDDO
      CLOSE(kin)

      n1 = n
      n2 = n

** sigma @ 295 K

      CALL addpnt(x1,y1,kdata,n1, x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,           0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,         1E38,0.)

      CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

* sigma @ 210 K

      CALL addpnt(x2,y2,kdata,n2, x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,           0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,         1E38,0.)

      CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
 
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

*** quantum yield assumed to be unity
      qy = 1.

      DO iz = 1, nz
        t = MAX(210.,MIN(tlev(iz),295.))
        slope = (t-210.)/(295.-210.)
        DO iw = 1, nw-1
            sq(j,iz,iw) = qy * (yg2(iw) + slope*(yg1(iw)-yg2(iw)))
        ENDDO
      ENDDO

      END
      SUBROUTINE r24(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CFC-144 photolysis:=*
*=              CF2ClCF2Cl + hv -> Products                                  =*
*=  Cross section: from JPL 97 recommendation, linear interp. between values =*
*=                 at 210 and 295K                                           =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      INTEGER n1, n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

* local

      REAL yg1(kw), yg2(kw)
      REAL qy
      REAL t
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz
      REAL slope

**************************************************************
************* CF2ClCF2Cl (CFC-114) photodissociation

      j = j+1
      jlabel(j) = 'CF2ClCF2Cl (CFC-114) + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/CFC-114_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i), y2(i)
        y1(i) = y1(i) * 1E-20
        y2(i) = y2(i) * 1E-20
        x2(i) = x1(i)
      ENDDO
      CLOSE(kin)

      n1 = n
      n2 = n

** sigma @ 295 K

      CALL addpnt(x1,y1,kdata,n1, x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,           0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,         1E38,0.)

      CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

* sigma @ 210 K

      CALL addpnt(x2,y2,kdata,n2, x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,           0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,         1E38,0.)

      CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)

      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

*** quantum yield assumed to be unity
      qy = 1.

      DO iz = 1, nz
        t = MAX(210.,MIN(tlev(iz),295.))
        slope = (t-210.)/(295.-210.)
        DO iw = 1, nw-1
            sq(j,iz,iw) = qy * (yg2(iw) + slope*(yg1(iw)-yg2(iw)))
        ENDDO
      ENDDO

      END
      SUBROUTINE r25(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CFC-115 photolysis =*
*=             CF3CF2Cl + hv -> Products                                     =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CF3CF2Cl (CFC-115) photodissociation
      
      j = j+1
      jlabel(j) = 'CF3CF2Cl (CFC-115) + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/CFC-115_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
    
      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.

      DO iw = 1, nw-1
        DO iz = 1, nz
          sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r26(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CFC-111 photolysis =*
*=          CCl3F + hv -> Products                                           =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      REAL t
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CCl3F (CFC-11) photodissociation
      
      j = j+1
      jlabel(j) = 'CCl3F (CFC-11) + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/CFC-11_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

** sigma @ 298 K

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF 

**** quantum yield assumed to be unity
      qy = 1.

      DO iz = 1, nz
        t = 1E-04 * (tlev(iz)-298.)
        DO iw = 1, nw-1
          sq(j,iz,iw) = qy * yg(iw) * EXP((wc(iw)-184.9) * t)
        ENDDO
      ENDDO

      END
      SUBROUTINE r27(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CFC-112 photolysis:=*
*=         CCl2F2 + hv -> Products                                           =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      REAL t
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CCl2F2 (CFC-12) photodissociation
      
      j = j+1
      jlabel(j) = 'CCl2F2 (CFC-12) + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/CFC-12_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

** sigma @ 298 K

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.

      DO iz = 1, nz
        t = 1E-04 * (tlev(i)-298.) 
        DO iw = 1, nw-1
          sq(j,iz,iw) = qy * yg(iw) * EXP((wc(iw)-184.9) * t)
        ENDDO
      ENDDO

      END
      SUBROUTINE r28(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CH3Br photolysis:  =*
*=         CH3Br + hv -> Products                                            =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CH3Br photodissociation

* data from JPL97 (identical to 94 recommendation)
      
      j = j+1
      jlabel(j) = 'CH3Br + hv -> Products'
      OPEN(kin,FILE='DATAJ1/ABS/CH3Br_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
  
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.

      DO iw = 1, nw-1
        DO iz = 1, nz
          sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r29(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CH3CCl3 photolysis =*
*=           CH3CCl3 + hv -> Products                                        =*
*=  Cross section: from JPL 97 recommendation, piecewise linear interp.      =*
*=                 of data at 210, 250, and 295K                             =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      INTEGER n1, n2, n3
      REAL x1(kdata), x2(kdata), x3(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata)

* local

      REAL yg(kw), yg1(kw), yg2(kw), yg3(kw)
      REAL qy
      REAL t
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz
      REAL slope

**************************************************************
************* CH3CCl3 photodissociation
      
      j = j+1
      jlabel(j) = 'CH3CCl3 + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/CH3CCl3_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i), y2(i), y3(i)
        y1(i) = y1(i) * 1E-20
        y2(i) = y2(i) * 1E-20
        y3(i) = y3(i) * 1E-20
        x2(i) = x1(i)
        x3(i) = x1(i)
      ENDDO
      CLOSE(kin)

      n1 = n
      n2 = n
      n3 = n

** sigma @ 295 K

      CALL addpnt(x1,y1,kdata,n1, x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,           0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,         1E38,0.)

      CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

** sigma @ 250 K
      
      CALL addpnt(x2,y2,kdata,n2, x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,           0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,         1E38,0.)

      CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
      
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

** sigma @ 210 K

      CALL addpnt(x3,y3,kdata,n3, x3(1)*(1.-deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,           0.,0.)
      CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,         1E38,0.)

      CALL inter2(nw,wl,yg3,n3,x3,y3,ierr)

      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.

      DO iz = 1, nz
        t = MIN(295.,MAX(tlev(iz),210.))
        IF (t .LE. 250.) THEN
          slope = (t-210.)/(250.-210.)
          DO iw = 1, nw-1
            sq(j,iz,iw) = qy * (yg3(iw) + slope*(yg2(iw)-yg3(iw)))
          ENDDO
        ELSE
          slope = (t-250.)/(295.-250.)
          DO iw = 1, nw-1
            sq(j,iz,iw) = qy * (yg2(iw) + slope*(yg1(iw)-yg2(iw)))
          ENDDO
        ENDIF
      ENDDO

      END
      SUBROUTINE r3(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide the product (absorptioon cross section) x (quantum yield) for    =*
*=  both channels of NO3 photolysis:                                         =*
*=          (a) NO3 + hv -> NO2 + O(3P)                                      =*
*=          (b) NO3 + hv -> NO + O2                                          =*
*=  Cross section combined from Graham and Johnston (<600 nm) and JPL 94     =*
*=  Quantum yield from Madronich (1988)                                      =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=350)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw), yg1(kw)
      REAL qy
      INTEGER irow, icol
      INTEGER i, iw, n, idum
      INTEGER ierr

****************      jlabel(j) = 'NO3 -> NO2 + O(3P)'
****************      jlabel(j) = 'NO3 -> NO + O2'

* cross section
*     measurements of Graham and Johnston 1978

      OPEN(UNIT=kin,FILE='DATAJ1/ABS/NO3_gj78.abs',STATUS='old')
      DO i = 1, 9
         READ(kin,*)
      ENDDO
      n = 305
      DO irow = 1, 30
         READ(kin,*) ( y1(10*(irow-1) + icol), icol =  1, 10 )
      ENDDO
      READ(kin,*) ( y1(300 + icol), icol = 1, 5 )
      CLOSE (kin)
      DO i = 1, n
         y1(i) =  y1(i) * 1.E-19
         x1(i) = 400. + 1.*FLOAT(i-1)
      ENDDO

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,               0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

*     cross section from JPL94:

      OPEN(UNIT=kin,FILE='DATAJ1/ABS/NO3_jpl94.abs',STATUS='old')
      READ(kin,*) idum, n
      DO i = 1, idum-2
         READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i)*1E-20
      ENDDO 
      CLOSE (kin)
      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,               0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
      CALL inter2(nw,wl,yg1,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

* use JPL94 for wavelengths longer than 600 nm

      DO iw = 1, nw-1
         IF(wl(iw) .GT. 600.) yg(iw) = yg1(iw)
      ENDDO

* quantum yield:
* from Madronich (1988) see CEC NO3 book.

* for   NO3 ->NO+O2

      j = j + 1
      jlabel(j) = 'NO3 -> NO + O2'
      DO iw = 1, nw - 1
         IF (wc(iw).LT.584.) THEN 
            qy = 0.
         ELSEIF (wc(iw).GE.640.) THEN
            qy = 0.
         ELSEIF (wc(iw).GE.595.) THEN 
            qy = 0.35*(1.-(wc(iw)-595.)/45.)
         ELSE
            qy = 0.35*(wc(iw)-584.)/11.
         ENDIF
         DO i = 1, nz
            sq(j,i,iw) = yg(iw)*qy
         ENDDO
      ENDDO

* for  NO3 ->NO2+O
      j = j + 1
      jlabel(j) = 'NO3 -> NO2 + O(3P)'
      DO iw = 1, nw - 1
         IF (wc(iw).LT.584.) THEN
            qy = 1.
         ELSEIF (wc(iw).GT.640.) THEN
            qy = 0.
         ELSEIF (wc(iw).GT.595.) THEN
            qy = 0.65*(1-(wc(iw)-595.)/45.)
         ELSE
            qy = 1.-0.35*(wc(iw)-584.)/11.
         ENDIF
         DO i = 1, nz
            sq(j,i,iw) = yg(iw)*qy
         ENDDO
      ENDDO

      END
      SUBROUTINE r30(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for CH3Cl photolysis:  =*
*=            CH3Cl + hv -> Products                                         =*
*=  Cross section: from JPL 97 recommendation, piecewise linear interp.      =*
*=                 from values at 255, 279, and 296K                         =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      INTEGER n1, n2, n3
      REAL x1(kdata), x2(kdata), x3(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata)

* local

      REAL yg(kw), yg1(kw), yg2(kw), yg3(kw)
      REAL qy
      REAL t
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz
      REAL slope

**************************************************************
************* CH3Cl photodissociation

      j = j+1
      jlabel(j) = 'CH3Cl + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/CH3Cl_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i), y2(i), y3(i)
        y1(i) = y1(i) * 1E-20
        y2(i) = y2(i) * 1E-20
        y3(i) = y3(i) * 1E-20
        x2(i) = x1(i)
        x3(i) = x1(i)
      ENDDO
      CLOSE(kin)

      n1 = n
      n2 = n
      n3 = n

** sigma @ 296 K

      CALL addpnt(x1,y1,kdata,n1, x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,           0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,         1E38,0.)

      CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

** sigma @ 279 K
  
      CALL addpnt(x2,y2,kdata,n2, x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,           0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,         1E38,0.)

      CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)

      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

** sigma @ 255 K

      CALL addpnt(x3,y3,kdata,n3, x3(1)*(1.-deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,           0.,0.)
      CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,         1E38,0.)

      CALL inter2(nw,wl,yg3,n3,x3,y3,ierr)

      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.

      DO iz = 1, nz
        t = MAX(255.,MIN(tlev(i),296.))
        IF (t .LE. 279.) THEN
          slope = (t-255.)/(279.-255.)
          DO iw = 1, nw-1
            sq(j,iz,iw) = qy * (yg3(iw)+slope*(yg2(iw)-yg3(iw)))
          ENDDO
        ELSE
          slope = (t-279.)/(296.-279.)
          DO iw = 1, nw-1
            sq(j,iz,iw) = qy * (yg2(iw)+slope*(yg1(iw)-yg2(iw)))
          ENDDO
        ENDIF
      ENDDO

      END
      SUBROUTINE r31(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for ClOO photolysis:   =*
*=          ClOO + hv -> Products                                            =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

C     INTEGER n1, n2, n3, n4, n5
      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* ClOO photodissociation

      j = j+1
      jlabel(j) = 'ClOO + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/ClOO_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
 
      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.

      DO iw = 1, nw-1
        DO iz = 1, nz
          sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r32(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for HCFC-123 photolysis=*
*=       CF3CHCl2 + hv -> Products                                           =*
*=  Cross section: from Orlando et al., 1991                                 =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* local

      REAL qy
      REAL t
      INTEGER i, iw, idum
      INTEGER iz, k
      REAL lambda, sum
      CHARACTER*120 inline

      REAL coeff(4,3), TBar, LBar

**************************************************************
************* CF3CHCl2 (HCFC-123) photodissociation
      
      j = j+1
      jlabel(j) = 'CF3CHCl2 (HCFC-123) + hv -> Products'

**** cross sections from JPL94 recommendation

C     OPEN(kin,FILE='DATAJ1/ABS/HCFC-123_jpl94.abs',STATUS='OLD')
C     READ(kin,*) idum, n
C     DO i = 1, idum-2
C       READ(kin,*)
C     ENDDO
C     DO i = 1, n
C       READ(kin,*) x1(i), y1(i)
C       y1(i) = y1(i) * 1E-20
C     ENDDO
C     CLOSE(kin)

C     CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
C     CALL addpnt(x1,y1,kdata,n,          0.,0.)
C     CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
C     CALL addpnt(x1,y1,kdata,n,        1E38,0.)

C     CALL inter2(nw,wl,yg,n,x1,y1,ierr)

C     IF (ierr .NE. 0) THEN
C        WRITE(*,*) ierr, jlabel(j)
C        STOP
C     ENDIF

**** quantum yield assumed to be unity
C     qy = 1.

C     DO iw = 1, nw-1
C       DO iz = 1, nz
C         sq(j,iz,iw) = qy * yg(iw)
C       ENDDO
C     ENDDO


**** cross section from Orlando et al., 1991

      OPEN(kin,FILE='DATAJ1/ABS/HCFCs_orl.abs',STATUS='OLD')
      READ(kin,*) idum
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      READ(kin,'(A)') inline
      READ(inline(6:),*) TBar,i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      CLOSE(kin)

      LBar = 206.214

**** quantum yield assumed to be unity

      qy = 1. 

      DO iw = 1, nw-1

        lambda = wc(iw)

C use parameterization only up to 220 nm, as the error bars associated with
C the measurements beyond 220 nm are very large (Orlando, priv.comm.)

        IF (lambda .GE. 190. .AND. lambda .LE. 220.) THEN
          DO iz = 1, nz
             t = MIN(295.,MAX(tlev(i),203.))-TBar
             sum = 0.
             DO i = 1, 4
                sum = (coeff(i,1)+t*(coeff(i,2)+t*coeff(i,3))) *
     >                (lambda-LBar)**(i-1) + sum
             ENDDO 
             sq(j,iz,iw) = qy * EXP(sum)
          ENDDO
        ELSE
          DO iz = 1, nz
            sq(j,iz,iw) = 0.
          ENDDO
        ENDIF
      ENDDO

      END
      SUBROUTINE r33(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for HCFC-124 photolysis=*
*=        CF3CHFCl + hv -> Products                                          =*
*=  Cross section: from Orlando et al., 1991                                 =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays


* local

      REAL qy
      REAL t
      INTEGER i, iw, n, idum
      INTEGER iz, k
      REAL lambda, sum
      CHARACTER*120 inline

      REAL coeff(4,3), TBar, LBar

**************************************************************
************* CF3CHFCl (HCFC-124) photodissociation
      
      j = j+1
      jlabel(j) = 'CF3CHFCl (HCFC-124) + hv -> Products'

**** cross sections from JPL94 recommendation

C     OPEN(kin,FILE='DATAJ1/ABS/HCFC-124_jpl94.abs',STATUS='OLD')
C     READ(kin,*) idum, n
C     DO i = 1, idum-2
C       READ(kin,*)
C     ENDDO
C     DO i = 1, n
C       READ(kin,*) x1(i), y1(i)
C       y1(i) = y1(i) * 1E-20
C     ENDDO
C     CLOSE(kin)

C     CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
C     CALL addpnt(x1,y1,kdata,n,          0.,0.)
C     CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
C     CALL addpnt(x1,y1,kdata,n,        1E38,0.)

C     CALL inter2(nw,wl,yg,n,x1,y1,ierr)

C     IF (ierr .NE. 0) THEN
C       WRITE(*,*) ierr, jlabel(j)
C       STOP
C     ENDIF

**** quantum yield assumed to be unity
C     qy = 1.

C     DO iw = 1, nw-1
C       DO iz = 1, nz
C         sq(j,iz,iw) = qy * yg(iw)
C       ENDDO
C     ENDDO

**** cross section from Orlando et al., 1991

      OPEN(kin,FILE='DATAJ1/ABS/HCFCs_orl.abs',STATUS='OLD')
      READ(kin,*) idum
      idum = idum+5
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      READ(kin,'(A)') inline
      READ(inline(6:),*) TBar,i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      CLOSE(kin)

      LBar = 206.214

**** quantum yield assumed to be unity

      qy = 1. 

      DO iw = 1, nw-1
        lambda = wc(iw)
        IF (lambda .GE. 190. .AND. lambda .LE. 230.) THEN
          DO iz = 1, nz
             t = MIN(295.,MAX(tlev(i),203.))-TBar
             sum = 0.
             DO i = 1, 4
                sum = (coeff(i,1)+t*(coeff(i,2)+t*coeff(i,3))) *
     >                (lambda-LBar)**(i-1) + sum
             ENDDO
             sq(j,iz,iw) = qy * EXP(sum)
          ENDDO
        ELSE
          DO iz = 1, nz
            sq(j,iz,iw) = 0.
          ENDDO
        ENDIF
      ENDDO

      END
      SUBROUTINE r34(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for HCFC-141b          =*
*=  photolysis:                                                              =*
*=         CH3CFCl2 + hv -> Products                                         =*
*=  Cross section: from JPL97 recommendation                                 =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CH3CFCl2 (HCFC-141b) photodissociation

      j = j+1
      jlabel(j) = 'CH3CFCl2 (HCFC-141b) + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/HCFC-141b_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.

      DO iw = 1, nw-1
        DO iz = 1, nz
          sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r35(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for HCFC-142b          =*
*=  photolysis:                                                              =*
*=          CH3CF2Cl + hv -> Products                                        =*
*=  Cross section: from Orlando et al., 1991                                 =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* local

      REAL qy
      REAL t
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz, k
      REAL lambda, sum
      CHARACTER*120 inline

      REAL coeff(4,3), TBar, LBar

**************************************************************
************* CH3CF2Cl (HCFC-142b) photodissociation

      j = j+1
      jlabel(j) = 'CH3CF2Cl (HCFC-142b) + hv -> Products'

**** cross sections from JPL94 recommendation

C     OPEN(kin,FILE='DATAJ1/ABS/HCFC-142b_jpl94.abs',STATUS='OLD')
C     READ(kin,*) idum, n
C     DO i = 1, idum-2
C       READ(kin,*)
C     ENDDO
C     DO i = 1, n
C       READ(kin,*) x1(i), y1(i)
C       y1(i) = y1(i) * 1E-20
C     ENDDO
C     CLOSE(kin)

C     CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
C     CALL addpnt(x1,y1,kdata,n,          0.,0.)
C     CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
C     CALL addpnt(x1,y1,kdata,n,        1E38,0.)

C     CALL inter2(nw,wl,yg,n,x1,y1,ierr)

C     IF (ierr .NE. 0) THEN
C       WRITE(*,*) ierr, jlabel(j)
C       STOP
C     ENDIF

**** quantum yield assumed to be unity
C     qy = 1.

C     DO iw = 1, nw-1
C       DO iz = 1, nz
C         sq(j,iz,iw) = qy * yg(iw)
C       ENDDO
C     ENDDO

**** cross section from Orlando et al., 1991

      OPEN(kin,FILE='DATAJ1/ABS/HCFCs_orl.abs',STATUS='OLD')
      READ(kin,*) idum
      idum = idum+10
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      READ(kin,'(A)') inline
      READ(inline(6:),*) TBar,i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      READ(kin,*)           i,(coeff(i,k),k=1,3)
      CLOSE(kin)

      LBar = 206.214

**** quantum yield assumed to be unity

      qy = 1.

      DO iw = 1, nw-1
        lambda = wc(iw)
        IF (lambda .GE. 190. .AND. lambda .LE. 230.) THEN
          DO iz = 1, nz
             t = MIN(295.,MAX(tlev(i),203.))-TBar
             sum = 0.
             DO i = 1, 4
                sum = (coeff(i,1)+t*(coeff(i,2)+t*coeff(i,3))) *
     >                (lambda-LBar)**(i-1) + sum
             ENDDO
             sq(j,iz,iw) = qy * EXP(sum)
          ENDDO
        ELSE
          DO iz = 1, nz
            sq(j,iz,iw) = 0.
          ENDDO
        ENDIF
      ENDDO

      END
      SUBROUTINE r36(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for HCFC-225ca         =*
*=  photolysis:                                                              =*
*=           CF3CF2CHCl2 + hv -> Products                                    =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CF3CF2CHCl2 (HCFC-225ca) photodissociation
       
      j = j+1
      jlabel(j) = 'CF3CF2CHCl2 (HCFC-225ca) + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/HCFC-225ca_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.

      DO iw = 1, nw-1
        DO iz = 1, nz
          sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r37(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for HCFC-225cb         =*
*=  photolysis:                                                              =*
*=          CF2ClCF2CHFCl + hv -> Products                                   =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CF2ClCF2CHFCl (HCFC-225cb) photodissociation

      j = j+1
      jlabel(j) = 'CF2ClCF2CHFCl (HCFC-225cb) + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/HCFC-225cb_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.

      DO iw = 1, nw-1
        DO iz = 1, nz
          sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r38(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for HCFC-22 photolysis =*
*=          CHClF2 + hv -> Products                                          =*
*=  Cross section: from JPL 97 recommendation, piecewise linear interp.      =*
*=                 from values at 210, 230, 250, 279, and 295 K              =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      INTEGER n1, n2, n3, n4, n5
      REAL x1(kdata), x2(kdata), x3(kdata), x4(kdata), x5(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata), y4(kdata), y5(kdata)

* local

      REAL yg(kw), yg1(kw), yg2(kw), yg3(kw), yg4(kw), yg5(kw)
      REAL qy
      REAL t
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz
      REAL slope

**************************************************************
************* CHClF2 (HCFC-22) photodissociation
       
      j = j+1
      jlabel(j) = 'CHClF2 (HCFC-22) + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/HCFC-22_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i), y2(i), y3(i), y4(i), y5(i)
        y1(i) = y1(i) * 1E-20
        y2(i) = y2(i) * 1E-20
        y3(i) = y3(i) * 1E-20
        y4(i) = y4(i) * 1E-20
        y5(i) = y5(i) * 1E-20
        x2(i) = x1(i)
        x3(i) = x1(i)
        x4(i) = x1(i)
        x5(i) = x1(i)
      ENDDO
      CLOSE(kin)

      n1 = n
      n2 = n
      n3 = n
      n4 = n
      n5 = n

** sigma @ 295 K

      CALL addpnt(x1,y1,kdata,n1, x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,           0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,         1E38,0.)

      CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

** sigma @ 270 K

      CALL addpnt(x2,y2,kdata,n2, x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,           0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,         1E38,0.)

      CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

** sigma @ 250 K

      CALL addpnt(x3,y3,kdata,n3, x3(1)*(1.-deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,           0.,0.)
      CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,         1E38,0.)

      CALL inter2(nw,wl,yg3,n3,x3,y3,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

** sigma @ 230 K

      CALL addpnt(x4,y4,kdata,n4, x4(1)*(1.-deltax),0.)
      CALL addpnt(x4,y4,kdata,n4,           0.,0.)
      CALL addpnt(x4,y4,kdata,n4,x4(n4)*(1.+deltax),0.)
      CALL addpnt(x4,y4,kdata,n4,         1E38,0.)

      CALL inter2(nw,wl,yg4,n4,x4,y4,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

** sigma @ 210 K

      CALL addpnt(x5,y5,kdata,n5, x5(1)*(1.-deltax),0.)
      CALL addpnt(x5,y5,kdata,n5,           0.,0.)
      CALL addpnt(x5,y5,kdata,n5,x5(n5)*(1.+deltax),0.)
      CALL addpnt(x5,y5,kdata,n5,         1E38,0.)

      CALL inter2(nw,wl,yg5,n5,x5,y5,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.

      DO iz = 1, nz
         t = MIN(295.,MAX(tlev(iz),210.))
         IF (t .LE. 230.) THEN
            slope = (t-210.)/(230.-210.)
            DO iw = 1, nw-1
              sq(j,iz,iw) = qy * (yg5(iw)+slope*(yg4(iw)-yg5(iw)))
            ENDDO
         ELSEIF (t .LE. 250.) THEN
            slope = (t-230.)/(250.-230.)
            DO iw = 1, nw-1
              sq(j,iz,iw) = qy * (yg4(iw)+slope*(yg3(iw)-yg4(iw)))
            ENDDO
         ELSEIF (t .LE. 270.) THEN
            slope = (t-250.)/(270.-250.)
            DO iw = 1, nw-1
              sq(j,iz,iw) = qy * (yg3(iw)+slope*(yg2(iw)-yg3(iw)))
            ENDDO
         ELSE
            slope = (t-270.)/(295.-270.)
            DO iw = 1, nw-1
              sq(j,iz,iw) = qy * (yg2(iw)+slope*(yg1(iw)-yg2(iw)))
            ENDDO
         ENDIF
      ENDDO

      END
      SUBROUTINE r39(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for HO2 photolysis:    =*
*=          HO2 + hv -> OH + O                                               =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed shape based on work by Lee, 1982; normalized      =*
*=                 to unity at 248 nm                                        =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* HO2 photodissociation

      j = j+1
      jlabel(j) = 'HO2 + hv -> OH + O'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/HO2_jpl94.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
  
      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

**** quantum yield:  absolute quantum yield has not been reported yet, but
****                 Lee measured a quantum yield for O(1D) production at 248
****                 nm that was 15 time larger than at 193 nm
**** here:  a quantum yield of unity is assumed at 248 nm and beyond, for
****        shorter wavelengths a linear decrease with lambda is assumed

      DO iw = 1, nw-1
         IF (wc(iw) .GE. 248.) THEN
            qy = 1.
         ELSE
            qy = 1./15. + (wc(iw)-193.)*(14./15.)/(248.-193.)
            qy = MAX(qy,0.)
         ENDIF
         DO iz = 1, nz
           sq(j,iz,iw) = qy * yg(iw)
         ENDDO
      ENDDO

      END
      SUBROUTINE r4(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product of (cross section) x (quantum yiels) for N2O5 photolysis =*
*=  reactions:                                                               =*
*=       (a) N2O5 + hv -> NO3 + NO + O(3P)                                   =*
*=       (b) N2O5 + hv -> NO3 + NO2                                          =*
*=  Cross section from JPL97: use tabulated values up to 280 nm, use expon.  =*
*=                            expression for >285nm, linearly interpolate    =*
*=                            between s(280) and s(285,T) in between         =*
*=  Quantum yield: Analysis of data in JPL94 (->DATAJ1/YLD/N2O5.qy)          =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      REAL xs, xst285, xs280
      REAL t
      INTEGER i, iw, n, idum
      INTEGER ierr

**************** N2O5 photodissociation

      j = j + 1
      jlabel(j) = 'N2O5 -> NO3 + NO + O(3P)'

      j = j + 1
      jlabel(j) = 'N2O5 -> NO3 + NO2'

* cross section from jpl97, table up to 280 nm

      OPEN(UNIT=kin,FILE='DATAJ1/ABS/N2O5_jpl97.abs',STATUS='old')
      READ(kin,*) idum, n
      DO i = 1, idum-2
         READ(kin,*)
      ENDDO
      DO i = 1,  n
         READ(kin,*) x1(i), y1(i)
         y1(i) = y1(i) * 1.E-20
      ENDDO
      xs280 = y1(n)
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata, n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata, n,               0.,0.)
      CALL addpnt(x1,y1,kdata, n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata, n,            1.E36,0.)

      CALL inter2(nw,wl,yg, n,x1,y1, ierr)
      IF (ierr .NE. 0) THEN
         WRITE(0,*) ierr,jlabel(j)
         STOP
      ENDIF 

* quantum yield : see DATAJ1/YLD/N2O5.qy for explanation
* correct for T-dependence of cross section

      DO iw = 1, nw - 1

         qy = MIN( 1., 3.832441 - 0.012809638 * wc(iw) )
         qy = MAX( 0., qy )

         DO i = 1, nz

* temperature dependence only valid for 225 - 300 K.

            t = MAX(225.,MIN(tlev(i),300.))

* evaluation of exponential
            IF (wl(iw) .GE. 285. .AND. wl(iw+1) .LE. 380.) THEN
               sq(j-1,i,iw) = qy *
     $            1.E-20*EXP( 2.735 + (4728.5-17.127*wc(iw)) / t )
               sq(j,i,iw) = (1.-qy) * 
     $            1.E-20*EXP( 2.735 + (4728.5-17.127*wc(iw)) / t )

* between 280 and 285, interpolate between temperature evaluated exponential
* at 285 nm and the tabulated value at 280 nm.
            ELSEIF (wl(iw) .GE. 280. .AND. wl(iw+1) .LE. 285.) THEN
               xst285 = 1.E-20*
     >                  EXP( 2.735 + (4728.5-17.127*285.) / t )
               xs = xs280 + (wc(iw)-280.)*(xst285-xs280)/(285.-280.)
C               write(23,*) wc(iw),xs280,xst285,xs
               sq(j-1,i,iw) = qy * xs
               sq(j,i,iw) = (1.-qy) * xs

* use tabulated values
            ELSEIF (wl(iw) .LE. 280.) THEN
               sq(j-1,i,iw) = qy * yg(iw)
               sq(j,i,iw) = (1.-qy) * yg(iw)

* beyond 380 nm, set to zero
            ELSE
               sq(j-1,i,iw) = 0.
               sq(j,i,iw) = 0.
            ENDIF
         ENDDO
      ENDDO

      END
      SUBROUTINE r40(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) Halon-1202 photolysis: =*
*=         CF2Br2 + hv -> Products                                           =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: unity (Molina and Molina)                                 =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CF2Br2 (Halon-1202) photodissociation
      
      j = j+1
      jlabel(j) = 'CF2Br2 (Halon-1202) + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/Halon-1202_jpl97.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)

      CALL inter2(nw,wl,yg,n,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

**** quantum yield unity (Molina and Molina)
      qy = 1.
     
      DO iw = 1, nw-1
        DO iz = 1, nz
           sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r41(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for Halon-1211         =*
*=  photolysis:                                                              =*
*=           CF2ClBr + hv -> Products                                        =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CF2BrCl (Halon-1211) photodissociation

      j = j+1
      jlabel(j) = 'CF2BrCl (Halon-1211) + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/Halon-1211_jpl97.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)
     
      CALL inter2(nw,wl,yg,n,x1,y1,ierr) 

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.
     
      DO iw = 1, nw-1
        DO iz = 1, nz
           sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r42(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for Halon-1301         =*
*=  photolysis:                                                              =*
*=         CF3Br + hv -> Products                                            =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CF3Br (Halon-1301) photodissociation

      j = j+1
      jlabel(j) = 'CF3Br (Halon-1301) + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/Halon-1301_jpl97.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)
    
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.
     
      DO iw = 1, nw-1
        DO iz = 1, nz
           sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r43(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for Halon-2402         =*
*=  photolysis:                                                              =*
*=           CF2BrCF2Br + hv -> Products                                     =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

**************************************************************
************* CF2BrCF2Br (Halon-2402) photodissociation

      j = j+1
      jlabel(j) = 'CF2BrCF2Br (Halon-2402) + hv -> Products'

**** cross sections from JPL97 recommendation (identical to 94 recommendation)

      OPEN(kin,FILE='DATAJ1/ABS/Halon-2402_jpl97.abs',STATUS='OLD')
      READ(kin,*) idum, n
      DO i = 1, idum-2
        READ(kin,*)
      ENDDO
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,          0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)+deltax,0.)
      CALL addpnt(x1,y1,kdata,n,        1E38,0.)
    
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)

      IF (ierr .NE. 0) THEN
        WRITE(*,*) ierr, jlabel(j)
        STOP
      ENDIF

**** quantum yield assumed to be unity
      qy = 1.
     
      DO iw = 1, nw-1
        DO iz = 1, nz
           sq(j,iz,iw) = qy * yg(iw)
        ENDDO
      ENDDO

      END
      SUBROUTINE r44(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for N2O photolysis:    =*
*=              N2O + hv -> N2 + O(1D)                                       =*
*=  Cross section: from JPL 97 recommendation                                =*
*=  Quantum yield: assumed to be unity, based on Greenblatt and Ravishankara =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* local

      REAL qy
      REAL a, b, c
      REAL a0, a1, a2, a3, a4
      REAL b0, b1, b2, b3
      REAL t
      INTEGER iw, iz
      REAL lambda

**************************************************************
************* N2O photodissociation

      j = j+1
      jlabel(j) = 'N2O + hv -> N2 + O(1D)'

**** cross sections according to JPL97 recommendation (identical to 94 rec.)
**** see file DATAJ1/ABS/N2O_jpl94.abs for detail

      A0 = 68.21023                
      A1 = -4.071805               
      A2 = 4.301146E-02            
      A3 = -1.777846E-04           
      A4 = 2.520672E-07

      B0 = 123.4014
      B1 = -2.116255
      B2 = 1.111572E-02
      B3 = -1.881058E-05

**** quantum yield of N(4s) and NO(2Pi) is less than 1% (Greenblatt and
**** Ravishankara), so quantum yield of O(1D) is assumed to be unity
      qy = 1.

      DO iw = 1, nw-1
         lambda = wc(iw)   
         IF (lambda .GE. 173. .AND. lambda .LE. 240.) THEN
           DO iz = 1, nz
             t = MAX(194.,MIN(tlev(iz),320.))
             A = (((A4*lambda+A3)*lambda+A2)*lambda+A1)*lambda+A0
             B = (((B3*lambda+B2)*lambda+B1)*lambda+B0)
             B = (t-300.)*EXP(B)
             sq(j,iz,iw) = qy * EXP(A+B)
           ENDDO
         ELSE
           DO iz = 1, nz
             sq(j,iz,iw) = 0.
           ENDDO 
         ENDIF
      ENDDO

      END
      SUBROUTINE r5(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide the product (cross section) x (quantum yield) for HNO2 photolysis=*
*=     HNO2 + hv -> NO + OH                                                  =*
*=  Cross section:  from JPL97                                               =*
*=  Quantum yield:  assumed to be unity                                      =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n
      INTEGER ierr

**************** HNO2 photodissociation
* cross section from JPL92
* (from Bongartz et al., identical to JPL94, JPL97 recommendation)

      j = j + 1
      jlabel(j) = 'HNO2 -> OH + NO'
      OPEN(UNIT=kin,FILE='DATAJ1/ABS/HNO2_jpl92.abs',STATUS='old')
      DO i = 1, 13
         READ(kin,*)
      ENDDO
      n = 91
      DO i = 1, n
         READ(kin,*) x1(i), y1(i)
         y1(i) = y1(i) * 1.E-20
      ENDDO
      CLOSE (kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,               0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

* quantum yield = 1

      qy = 1.
      DO iw = 1, nw - 1
         DO i = 1, nz
            sq(j,i,iw) = yg(iw)*qy
         ENDDO
      ENDDO

      END
      SUBROUTINE r6(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product of (cross section) x (quantum yield) for HNO3 photolysis =*
*=        HNO3 + hv -> OH + NO2                                              =*
*=  Cross section: Burkholder et al., 1993                                   =*
*=  Quantum yield: Assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      INTEGER n1, n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

* local

      REAL yg1(kw), yg2(kw)
      INTEGER i, iw
      INTEGER ierr

**************** HNO3 photodissociation

       j = j + 1
       jlabel(j) = 'HNO3 -> OH + NO2'

C* cross section from JPL85
C
C      OPEN(UNIT=kin,FILE='DATAJ1/ABS/HNO3.abs',STATUS='old')
C      DO i = 1, 9
C         READ(kin,*)
C      ENDDO
C      n = 29
C      DO i = 1, n
C         READ(kin,*) x1(i), y1(i)
C         y1(i) = y1(i) * 1.E-20
C      ENDDO
C      CLOSE (kin)
C
C      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
C      CALL addpnt(x1,y1,kdata,n,               0.,0.)
C      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
C      CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
C      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
C      IF (ierr .NE. 0) THEN
C         WRITE(*,*) ierr, jlabel(j)
C         STOP
C      ENDIF
C
C* quantum yield = 1
C
C      qy = 1.
C      DO iw = 1, nw - 1
C         DO i = 1, nz
C            sq(j,i,iw) = yg(iw)*qy
C         ENDDO
C      ENDDO


* HNO3 cross section parameters from Burkholder et al. 1993

      OPEN(UNIT=kin,FILE='DATAJ1/ABS/HNO3_burk.abs',STATUS='old')
      DO i = 1, 6
         READ(kin,*)
      END DO
      n1 =  83
      n2 = n1
      DO i = 1, n1
         READ(kin,*) y1(i), y2(i)
         x1(i) = 184. + i*2.
         x2(i) = x1(i)
      END DO
      CLOSE (kin)

      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,               0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,            1.e+38,0.)
      CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF


      CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),y2(1))
      CALL addpnt(x2,y2,kdata,n2,               0.,y2(1))
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),y2(n2))
      CALL addpnt(x2,y2,kdata,n2,            1.e+38,y2(n2))
      CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

* quantum yield = 1
* correct for temperature dependence

      DO iw = 1, nw - 1
         DO i = 1, nz
            sq(j,i,iw) = yg1(iw) * 1.E-20
     $           * exp( yg2(iw)/1.e3*(tlev(i)-298.) )
         ENDDO
      ENDDO

      END
      SUBROUTINE r7(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product of (cross section) x (quantum yield) for HNO4 photolysis =*
*=       HNO4 + hv -> HO2 + NO2                                              =*
*=  Cross section:  from JPL97                                               =*
*=  Quantum yield:  Assumed to be unity                                      =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)
 
C* local
 
      REAL yg(kw)
      REAL qy
      INTEGER i, iw, n
      INTEGER ierr

**************** HNO4 photodissociation

* cross section from JPL85 (identical to JPL92 and JPL94 and JPL97)

      j = j + 1
      jlabel(j) = 'HNO4 -> HO2 + NO2'
C      OPEN(UNIT=kin,FILE='DATAJ1/ABS/HNO4.abs',STATUS='old')
      OPEN(UNIT=kin,FILE='DATAJ1/ABS/HNO4_jpl92.abs',STATUS='old')
      DO i = 1, 4
         READ(kin,*)
      ENDDO
      n = 31
      DO i = 1, n
         READ(kin,*) x1(i), y1(i)
         y1(i) = y1(i) * 1.E-20
      ENDDO
      CLOSE (kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,               0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

* quantum yield = 1

      qy = 1.
      DO iw = 1, nw - 1
         DO i = 1, nz
            sq(j,i,iw) = yg(iw)*qy
         ENDDO
      ENDDO

      END
      SUBROUTINE r8(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product of (cross section) x (quantum yield) for H2O2 photolysis =*
*=         H2O2 + hv -> 2 OH                                                 =*
*=  Cross section:  From JPL97, tabulated values @ 298K for <260nm, T-depend.=*
*=                  parameterization for 260-350nm                           =*
*=  Quantum yield:  Assumed to be unity                                      =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

C     INTEGER n1, n2, n3, n4, n5
      REAL x1(kdata)
      REAL y1(kdata)

* local

      REAL yg(kw)
      REAL qy
      REAL a0, a1, a2, a3, a4, a5, a6, a7
      REAL b0, b1, b2, b3, b4
      REAL xs
      REAL t
      INTEGER i, iw, n, idum
      INTEGER ierr
      REAL lambda
      REAL sumA, sumB, chi

**************** H2O2 photodissociation

* cross section from Lin et al. 1978

      j = j + 1
      jlabel(j) = 'H2O2 -> 2 OH'
C     OPEN(UNIT=kin,FILE='DATAJ1/ABS/H2O2_lin.abs',STATUS='old')
C     DO i = 1, 7
C        READ(kin,*)
C     ENDDO
C     n = 32
C     DO i = 1, n
C        READ(kin,*) x1(i), y1(i)
C        y1(i) = y1(i) * 1.E-20
C     ENDDO
C     CLOSE (kin)
C
C      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
C      CALL addpnt(x1,y1,kdata,n,               0.,0.)
C      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
C      CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
C      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
C      IF (ierr .NE. 0) THEN
C         WRITE(*,*) ierr, jlabel(j)
C         STOP
C      ENDIF

* cross section from JPL94 (identical to JPL97)
* tabulated data up to 260 nm

      OPEN(UNIT=kin,FILE='DATAJ1/ABS/H2O2_jpl94.abs',STATUS='old')
      READ(kin,*) idum,n
      DO i = 1, idum-2
         READ(kin,*)
      ENDDO
      DO i = 1, n
         READ(kin,*) x1(i), y1(i)
         y1(i) = y1(i) * 1.E-20
      ENDDO
      CLOSE (kin)


      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n,               0.,0.)
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n,           1.e+38,0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

      A0 = 6.4761E+04            
      A1 = -9.2170972E+02        
      A2 = 4.535649              
      A3 = -4.4589016E-03        
      A4 = -4.035101E-05         
      A5 = 1.6878206E-07
      A6 = -2.652014E-10
      A7 = 1.5534675E-13

      B0 = 6.8123E+03
      B1 = -5.1351E+01
      B2 = 1.1522E-01
      B3 = -3.0493E-05
      B4 = -1.0924E-07

* quantum yield = 1

      qy = 1.

      DO iw = 1, nw - 1

* Parameterization (JPL94)
* Range 260-350 nm; 200-400 K

         IF ((wl(iw) .GE. 260.) .AND. (wl(iw) .LT. 350.)) THEN

           lambda = wc(iw)
           sumA = ((((((A7*lambda + A6)*lambda + A5)*lambda + 
     >                  A4)*lambda +A3)*lambda + A2)*lambda + 
     >                  A1)*lambda + A0
           sumB = (((B4*lambda + B3)*lambda + B2)*lambda + 
     >               B1)*lambda + B0

           DO i = 1, nz
              t = MIN(MAX(tlev(i),200.),400.)            
              chi = 1./(1.+EXP(-1265./t))
              xs = (chi * sumA + (1.-chi)*sumB)*1E-21
              sq(j,i,iw) = xs*qy
           ENDDO
         ELSE
           DO i = 1, nz
              sq(j,i,iw) = yg(iw)*qy
           ENDDO
         ENDIF

      ENDDO

      END
      SUBROUTINE r9(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product of (cross section) x (quantum yield) for CHBr3 photolysis=*
*=          CHBr3 + hv -> Products                                           =*
*=  Cross section: Choice of data from Atlas (?Talukdar???) or JPL97         =*
*=  Quantum yield: Assumed to be unity                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original, adapted from former JSPEC1 subroutine                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:

      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=200)

      INTEGER n1, n2, n3, n4, n5
      REAL x1(kdata), x2(kdata), x3(kdata), x4(kdata), x5(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata), y4(kdata), y5(kdata)

* local

      REAL yg(kw), yg1(kw), yg2(kw), yg3(kw), yg4(kw), yg5(kw)

      real t
      real qy

      INTEGER i, iw, n
      INTEGER ierr
      INTEGER iz

      integer kopt


*_______________________________________________________________________

      DO 5, iw = 1, nw - 1
         wc(iw) = (wl(iw) + wl(iw+1))/2.
 5    CONTINUE



**************** CHBr3 photodissociation

      j = j + 1
      jlabel(j) = 'CHBr3'

* option:

* kopt = 1:  cross section from Elliot Atlas, 1997
* kopt = 2:  cross section from JPL 1997

      kopt = 2
      if (kopt .eq. 1) then

      OPEN(UNIT=kin,FILE='DATAJ1/ABS/CHBr3.abs',STATUS='old')
      DO i = 1, 5
         READ(kin,*)
      ENDDO

      n5 = 25
      n4 = 27
      n3 = 29
      n2 = 31
      n1 = 39
      DO i = 1, n5
         READ(kin,*) x1(i), y1(i), y2(i), y3(i), y4(i), y5(i)
      ENDDO
      do i = n5 + 1, n4
         READ(kin,*) x1(i), y1(i), y2(i), y3(i), y4(i)
      enddo
      do i = n4 + 1, n3
         READ(kin,*) x1(i), y1(i), y2(i), y3(i)
      enddo
      do i = n3 + 1, n2
         READ(kin,*) x1(i), y1(i), y2(i)
      enddo
      do i = n2 + 1, n1
         READ(kin,*) x1(i), y1(i)
      enddo
      CLOSE (kin)

      do i = 1, n1
         y1(i) = y1(i) * 1.e-23
      enddo
      do i = 1, n2
         x2(i) = x1(i)
         y2(i) = y2(i) * 1.e-23
      enddo
      do i = 1, n3
         x3(i) = x1(i)
         y3(i) = y3(i) * 1.e-23
      enddo
      do i = 1, n4
         x4(i) = x1(i)
         y4(i) = y4(i) * 1.e-23
      enddo
      do i = 1, n5
         x5(i) = x1(i)
         y5(i) = y5(i) * 1.e-23
      enddo

      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),y1(1))
      CALL addpnt(x1,y1,kdata,n1,               0.,y1(1))
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
      CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

      CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),y2(1))
      CALL addpnt(x2,y2,kdata,n2,               0.,y2(1))
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,           1.e+38,0.)
      CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

      CALL addpnt(x3,y3,kdata,n3,x3(1)*(1.-deltax),y3(1))
      CALL addpnt(x3,y3,kdata,n3,               0.,y3(1))
      CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,           1.e+38,0.)
      CALL inter2(nw,wl,yg3,n3,x3,y3,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)

      ENDIF

      CALL addpnt(x4,y4,kdata,n4,x4(1)*(1.-deltax),y4(1))
      CALL addpnt(x4,y4,kdata,n4,               0.,y4(1))
      CALL addpnt(x4,y4,kdata,n4,x4(n4)*(1.+deltax),0.)
      CALL addpnt(x4,y4,kdata,n4,           1.e+38,0.)
      CALL inter2(nw,wl,yg4,n4,x4,y4,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

      CALL addpnt(x5,y5,kdata,n5,x5(1)*(1.-deltax),y5(1))
      CALL addpnt(x5,y5,kdata,n5,               0.,y5(1))
      CALL addpnt(x5,y5,kdata,n5,x5(n5)*(1.+deltax),0.)
      CALL addpnt(x5,y5,kdata,n5,           1.e+38,0.)
      CALL inter2(nw,wl,yg5,n5,x5,y5,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF


* quantum yield = 1

      qy = 1.
      DO iw = 1, nw - 1
         DO iz = 1, nz

            t = tlev(iz)

            if (t .ge. 296.) then
               yg(iw) = yg1(iw)

            else if(t .ge. 286.) then
               yg(iw) = yg1(iw) + (t-286.)*(yg2(iw)-yg1(iw))/10.

            else if(t .ge. 276.) then
               yg(iw) = yg2(iw) + (t-276.)*(yg3(iw)-yg2(iw))/10.

            else if(t .ge. 266.) then
               yg(iw) = yg3(iw) + (t-266.)*(yg4(iw)-yg3(iw))/10.

            else if(t .ge. 256.) then
               yg(iw) = yg4(iw) + (t-256.)*(yg5(iw)-yg4(iw))/10.

            else if(t .lt. 256.) then
               yg(iw) = yg5(iw)

            endif

            sq(j,iz,iw) = yg(iw)*qy

         ENDDO
      ENDDO

* jpl97, with temperature dependence formula,
*w = 290 nm to 340 nm, 
*T = 210K to 300 K
*sigma, cm2 = exp((0.06183-0.000241*w)*(273.-T)-(2.376+0.14757*w))

      ELSEIF (kopt .EQ. 2) THEN

      OPEN(UNIT=kin,FILE='DATAJ1/ABS/CHBr3.jpl97',STATUS='old')
      DO i = 1, 6
         READ(kin,*)
      ENDDO
      n1 = 87
      DO i = 1, n1
         READ(kin,*) x1(i), y1(i)
         y1(i) = y1(i) * 1.e-20
      ENDDO
      CLOSE(kin)

      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),y1(1))
      CALL addpnt(x1,y1,kdata,n1,               0.,y1(1))
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
      CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

* quantum yield = 1

      qy = 1.
      DO iw = 1, nw - 1
         DO iz = 1, nz

            t = tlev(iz)
            yg(iw) = yg1(iw)

            IF (wc(iw) .GT. 290. .AND. wc(iw) .LT. 340. 
     $           .AND. t .GT. 210 .AND. t .LT. 300) THEN
               yg(iw) = EXP((0.06183-0.000241*wc(iw))*(273.-T)-
     $              (2.376+0.14757*wc(iw)))
            ENDIF

            sq(j,iz,iw) = yg(iw)*qy
         ENDDO
      ENDDO

      ENDIF

      END
      SUBROUTINE r45(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for ClONO2 photolysis: =*
*=        ClONO2 + hv -> Products                                            =*
*=                                                                           =*
*=  Cross section: JPL 97 recommendation                                     =*
*=  Quantum yield: JPL 97 recommendation                                     =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  09/98  Original, by sm                                                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=150)

      REAL x1(kdata),x2(kdata),x3(kdata)
      REAL y1(kdata),y2(kdata),y3(kdata)
      INTEGER n1, n2, n3

* local

      REAL yg1(kw), yg2(kw), yg3(kw)
      REAL qy1, qy2
      REAL xs 
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

************* ClONO2 photodissociation

      j = j+1
      jlabel(j) = 'ClONO2 + hv -> Cl + NO3'

*** cross sections from JPL97 recommendation

      OPEN(kin,FILE='DATAJ1/ABS/ClONO2_jpl97.abs',STATUS='OLD')
      n = 119
      DO i = 1, n
        READ(kin,*) x1(i), y1(i), y2(i), y3(i)
        y1(i) = y1(i) * 1E-20
        x2(i) = x1(i)
        x3(i) = x1(i)
      ENDDO
      CLOSE(kin)

      n1 = n
      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,          0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,        1E38,0.)
      CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

      n2 = n
      CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,          0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,        1E38,0.)
      CALL inter2(nw,wl,yg2,n2,x2,y2,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

      n3 = n
      CALL addpnt(x3,y3,kdata,n3,x3(1)*(1.-deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,          0.,0.)
      CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,        1E38,0.)
      CALL inter2(nw,wl,yg3,n3,x3,y3,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

      DO iw = 1, nw-1

*** quantum yields (from jpl97)

         IF( wc(iw) .LT. 308.) THEN
            qy1 = 0.6
         ELSEIF( (wc(iw) .GE. 308) .AND. (wc(iw) .LE. 364.) ) THEN
            qy1 = 7.143e-3 * wc(iw) - 1.6
         ELSEIF( wc(iw) .GT. 364. ) THEN
            qy1 = 1.0
         ENDIF
         qy2 = 1. - qy1
         
* compute T-dependent cross section

         DO iz = 1, nz
            xs = yg1(iw)*( 1. + 
     $           yg2(iw)*(tlev(iz)-296) + 
     $           yg3(iw)*(tlev(iz)-296)*(tlev(iz)-296))
            sq(j,iz,iw) = qy1 * xs
            sq(j+1,iz,iw) = qy2 * xs

C            if(iz .eq. 1) write(33,333) wc(iw), xs
C 333        format(0pf8.3,1pe11.4)

         ENDDO
      ENDDO

      j = j+1
      jlabel(j) = 'ClONO2 + hv -> ClO + NO2'

      END
      SUBROUTINE r46(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for BrONO2 photolysis: =*
*=        BrONO2 + hv -> Products                                            =*
*=                                                                           =*
*=  Cross section: JPL 97 recommendation                                     =*
*=  Quantum yield: JPL 97 recommendation                                     =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  09/98  Original, by sm                                                   =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=100)

      REAL x1(kdata)
      REAL y1(kdata)
      INTEGER n1, n2, n3

* local

      REAL yg1(kw)
      REAL qy1, qy2
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

************* BrONO2 photodissociation

      j = j+1
      jlabel(j) = 'BrONO2 + hv -> Br + NO3'

*** cross sections from JPL97 recommendation

      OPEN(kin,FILE='DATAJ1/ABS/BrONO2_jpl97.abs',STATUS='OLD')
      n = 61
      DO i = 1, n
        READ(kin,*) x1(i), y1(i)
        y1(i) = y1(i) * 1E-20
      ENDDO
      CLOSE(kin)

      n1 = n
      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,          0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,        1E38,0.)
      CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

*** quantum yields (from jpl97)

      qy1 = 0.29
      qy2 = 0.71

      DO iw = 1, nw-1
         DO iz = 1, nz
            sq(j,iz,iw) = qy1 * yg1(iw)
            sq(j+1,iz,iw) = qy2 * yg1(iw)

c            if(iz .eq. 1) write(34,333) wc(iw), yg1(iw)
c 333        format(0pf8.3,1pe11.4)


         ENDDO
      ENDDO

      j = j+1
      jlabel(j) = 'BrONO2 + hv -> BrO + NO2'

      END

cgy r99 added for camx application by greg yarwood 6/4/99
      SUBROUTINE r99(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for ISPD   photolysis: =*
*=        ISPD   + hv -> Products                                            =*
*=                                                                           =*
*=  Cross section:  Isoprene product photolysis properties from Carter and   =*
*=  Atkinson, 1996 IJCK.  Same as absorption cross sections for Acrolein from=*
*=  E. P. Gardner, P. D. Sperry, and J. G. Calvert, JPC _91_, 1922 (1987)    =*
*=  Digitized from small figure.                                             =*
*=  Quantum yield:  yet unknown, but assumed to be 0.0036                    =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  09/98  Original, by sm                                                   =*
*=  06/99  r99 created by Greg Yarwood                                       =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=150)

      REAL x1(kdata)
      REAL y1(kdata)
      INTEGER n1, n2, n3

* local

      REAL yg1(kw)
      REAL qy
      INTEGER i, iw, n, idum
      INTEGER ierr
      INTEGER iz

************* ISPD photodissociation

      j = j+1
      jlabel(j) = 'ISPD + hv -> products'

*** cross sections 

      OPEN(UNIT=kin,FILE='DATAJ1/ABS/ISPD_carter.abs',STATUS='OLD')
      DO i = 1, 6
         READ(kin,*)
      ENDDO
      n = 131
      DO i = 1, n
         READ(kin,*) x1(i), y1(i)
         y1(i) = y1(i) * 1.E-20
      ENDDO
      CLOSE(kin)

      n1 = n
      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,          0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,        1E38,0.)
      CALL inter2(nw,wl,yg1,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, jlabel(j)
         STOP
      ENDIF

*** quantum yields 

      qy = 0.0036

      DO iw = 1, nw-1
        DO i = 1, nz
          sq(j,i,iw) = qy*yg1(iw)
        ENDDO
      ENDDO


      END

c
cgy subroutine to read SAPRC data in Bill Carter's data format.
c   Data reads and error checking get quite complicated to
c   ensure that the ASCII data read in correctly on multiple platforms.
c   This version works on DEC, SGI, Sun and Linux(PGF77).
c   If you are having problems, or add new data, try turning on the
c   diagnostic output by setting idb=1
c
      SUBROUTINE rsap(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,rname,sapvrs)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for SAPRC photolysis:  =*
*=  Reaction name passed in rname                                            =*
*=                                                                           =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*=  SAPVRS - CHARACTER*7, version number of SAPRC (97, 99, etc.)
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  09/98  Original, by sm                                                   =*
*=  06/99  modified by Greg Yarwood                                          =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)
      CHARACTER*7 sapvrs
      CHARACTER*12 rname

* weighting functions

      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j
      CHARACTER*40 jlabel(kj)

* data arrays

      INTEGER kdata
      PARAMETER(kdata=999)

      REAL x1(kdata)
      REAL y1(kdata)
      REAL q1(kdata)
      REAL a1(kdata)
      INTEGER n1, n2, n3

* local

      REAL ag1(kw)
      REAL factor
      REAL lfac
      INTEGER i, iw, n, idum, idb, iloc
      INTEGER ierr
      INTEGER iz
      CHARACTER*80 fname, line, blank

* data

      data blank /' '/

************* 
c
c --- toggle switch (0/1) to turn on dbug statements
c
      idb = 0
c
c --- increment the weighting function counter (j) 
c
      j = j+1
      jlabel(j) = sapvrs//': '//rname
      fname='DATAJ1/'//sapvrs//'/'//rname
      if (idb.eq.1) write(*,'(a,a2,a,a)') 'Reading '//sapvrs//
     &                                    ' file: ', fname
      iloc=1
      open(unit=kin,file=fname,status='old',err=999)
      iloc=2
      read(kin,'(a80)',err=999,end=999) line
      if (idb.eq.1) write(*,'(a,a)') 'First line: ', line
c
c --- read header - comment lines begin with !
c
      do 10 i=1,99
        iloc=3
        read(kin,'(a80)',err=999,end=999) line
        if (line.eq.blank) goto 40
        if (idb.eq.1) write(*,'(a,a)') 'Test header: ', line
        if (line(1:1).eq.'!') goto 10
        goto 20
 10   continue
c
c --- read and count data lines, check for a factor to scale sigma
c
 20   n = 1
      factor = 1.0
      lfac = 1.0
      if (idb.eq.1) write(*,'(a)') 'Get data...'
      do i = 1, 999
        if(line(1:3).eq.'FAC') then
          iloc=4
          read(line(5:),*,err=999) factor
          if (idb.eq.1) write(*,*) 'factor =', factor
          read(kin,'(a80)',err=40,end=40) line
          if (idb.eq.1) write(*,'(a80)') line
          if (line.eq.blank) goto 40
        elseif(line(1:4).eq.'LFAC') then
          iloc=41
          read(line(6:),*,err=999) lfac
          if (idb.eq.1) write(*,*) 'lambda factor =', lfac
          read(kin,'(a80)',err=40,end=40) line
          if (idb.eq.1) write(*,'(a80)') line
          if (line.eq.blank) goto 40
        else
          read(line,*,err=30,end=30) x1(n), y1(n), q1(n)
          goto 50
c
c --- no phi specified, must be 1.0
c
 30         q1(n) = 1.0
            read(line,*,err=70,end=70) x1(n), y1(n)
c
c --- calculate sigma*phi and convert lamda to nm
c
 50       a1(n) = y1(n) * factor * q1(n)
          x1(n) = 1000*x1(n)*lfac
          if (idb.eq.1) write(*,*) x1(n), y1(n), q1(n)
          read(kin,'(a80)',err=40,end=40) line
          if (idb.eq.1) write(*,'(a80)') line
          if (line.eq.blank) goto 40
          n=n+1
          if (n.gt.kdata) then
            write(*,*) ' In RSAP n > kdata, recompile with kdata =', n
            stop
          endif
        endif
      enddo
c
c --- done reading data
c
 70   n=n-1
 40   close(kin)
      if (idb.eq.1) write(*,*) 'Counted ', n, ' data points'
c
c --- put data onto the modeling wavelength grid
c
      n1 = n
      CALL addpnt(x1,a1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,a1,kdata,n1,          0.,0.)
      CALL addpnt(x1,a1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,a1,kdata,n1,        1E38,0.)
      CALL inter2(nw,wl,ag1,n1,x1,a1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) 'cross-section',ierr, jlabel(j)
         STOP
      ENDIF
c
      DO iw = 1, nw-1
        DO i = 1, nz
          sq(j,i,iw) = ag1(iw)
        ENDDO
      ENDDO
c
      return
c
 999  write(*,*) 'Fatal error reading SAPRC data file:'
      write(*,*) fname
      write(*,*) 'at location = ', iloc , ' look for iloc in rn.f'
      stop
c
      END

c


c   subroutine to read chlorine data in DATAJ1/CHLORINE directory
c   Data reads and error checking get quite complicated to
c   ensure that the ASCII data read in correctly on multiple platforms.
c   This version works on DEC, SGI, Sun and Linux(PGF77).
c   If you are having problems, or add new data, try turning on the
c   diagnostic output by setting idb=1
c
      SUBROUTINE rcl(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,rname)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for chlorine photolysis=*
*=  Reaction name passed in rname                                            =*
*=                                                                           =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  09/98  Original, by sm                                                   =*
*=  06/01  modified by Greg Yarwood                                          =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      CHARACTER*12 rname
      CHARACTER*80 fname, line, blank
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* data arrays

      INTEGER kdata
      PARAMETER(kdata=999)

      REAL x1(kdata)
      REAL y1(kdata)
      REAL q1(kdata)
      REAL a1(kdata)
      INTEGER n1, n2, n3

* local

      REAL ag1(kw)
      REAL factor
      INTEGER i, iw, n, idum, idb, iloc
      INTEGER ierr
      INTEGER iz

* data

      data blank /' '/

************* 
c
c --- toggle switch (0/1) to turn on dbug statements
c
      idb = 0
c
c --- increment the weighting function counter (j) 
c
      j = j+1
      jlabel(j) = 'CHLORINE: '//rname
      fname='DATAJ1/CHLORINE/'//rname
      if (idb.eq.1) write(*,'(a,a)') 'Reading CHLORINE file: ', fname

      iloc=1
      open(unit=kin,file=fname,status='old',err=999)
      iloc=2
      read(kin,'(a80)',err=999,end=999) line
      if (idb.eq.1) write(*,'(a,a)') 'First line: ', line
c
c --- read header - comment lines begin with !
c
      do 10 i=1,99
        iloc=3
        read(kin,'(a80)',err=999,end=999) line
        if (line.eq.blank) goto 40
        if (idb.eq.1) write(*,'(a,a)') 'Test header: ', line
        if (line(1:1).eq.'!') goto 10
        goto 20
 10   continue
c
c --- read and count data lines, check for a factor to scale sigma
c
 20   n = 1
      factor = 1.0
      if (idb.eq.1) write(*,'(a)') 'Get data...'
      do i = 1, 999
        if(line(1:3).eq.'FAC') then
          iloc=4
          read(line(5:),*,err=999) factor
          if (idb.eq.1) write(*,*) 'factor =', factor
          read(kin,'(a80)',err=40,end=40) line
          if (idb.eq.1) write(*,'(a80)') line
          if (line.eq.blank) goto 40
        else
          read(line,*,err=30,end=30) x1(n), y1(n), q1(n)
          goto 50
c
c --- no phi specified, must be 1.0
c
 30         q1(n) = 1.0
            read(line,*,err=70,end=70) x1(n), y1(n)
c
c --- calculate sigma*phi and convert lamda to nm
c
 50       a1(n) = y1(n) * factor * q1(n)
          x1(n) = 1000*x1(n)
          if (idb.eq.1) write(*,*) x1(n), y1(n), q1(n)
          read(kin,'(a80)',err=40,end=40) line
          if (idb.eq.1) write(*,'(a80)') line
          if (line.eq.blank) goto 40
          n=n+1
          if (n.gt.kdata) then
            write(*,*) ' In RCL n > kdata, recompile with kdata =', n
            stop
          endif
        endif
      enddo
c
c --- done reading data
c
 70   n=n-1
 40   close(kin)
      if (idb.eq.1) write(*,*) 'Counted ', n, ' data points'
c
c --- put data onto the modeling wavelength grid
c
      n1 = n
      CALL addpnt(x1,a1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,a1,kdata,n1,          0.,0.)
      CALL addpnt(x1,a1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,a1,kdata,n1,        1E38,0.)
      CALL inter2(nw,wl,ag1,n1,x1,a1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) 'cross-section',ierr, jlabel(j)
         STOP
      ENDIF
c
      DO iw = 1, nw-1
        DO i = 1, nz
          sq(j,i,iw) = ag1(iw)
        ENDDO
      ENDDO
c
      return
c
 999  write(*,*) 'Fatal error reading CHLORINE data file:'
      write(*,*) fname
      write(*,*) 'at location = ', iloc , ' look for iloc in rn.f'
      stop
c
      END

c
cgy subroutine to read data from IUPAC where sigma and phi are seperated
c
      SUBROUTINE riup(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,namsig,namphi,
     &           iinterp)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Provide product (cross section) x (quantum yield) for IUPAC photolysis:  =*
*=  Reaction name passed in rname                                            =*
*=                                                                           =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of center points of wavelength intervals in     (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*=  iinterp- INTEGER, control how cross-sections are interpolate             =*
*=              1 cross-sections are point values                            =*
*=              2 cross-sections are binned                                  =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  09/98  Original, by sm                                                   =*
*=  01/05  modified by Greg Yarwood                                          =*
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

      IMPLICIT NONE
      INCLUDE 'params'

* input

      INTEGER nw
      REAL wl(kw), wc(kw)
      
      INTEGER nz, iinterp

      REAL tlev(kz)
      REAL airlev(kz)
      CHARACTER*12 namsig, namphi

* weighting functions

      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j
      CHARACTER*40 jlabel(kj)

* data arrays

      INTEGER kdata
      PARAMETER(kdata=999)

      REAL x1(kdata)
      REAL y1(kdata)
      REAL q1(kdata)
      REAL a1(kdata)
      INTEGER n1, n2, n3

* local

      REAL ag1(kw), ag2(kw)
      REAL factor
      REAL lfac
      INTEGER i, iw, n, idum, idb, iloc
      INTEGER ierr
      INTEGER iz
      CHARACTER*80 fname, line, blank

* data

      data blank /' '/

************* 
c
c --- toggle switch (0/1) to turn on dbug statements
c
      idb = 1
c
c --- increment the weighting function counter (j) 
c
      j = j+1
      jlabel(j) = 'IUPAC04: '//namphi
c
c --- first, read the sigma values
c
      fname='DATAJ1/IUPAC04/raw_data/'//namsig
      if (idb.eq.1) write(*,'(a,a)') 'Reading ', fname
      iloc=1
      open(unit=kin,file=fname,status='old',err=999)
      iloc=2
      read(kin,'(a80)',err=999,end=999) line
      if (idb.eq.1) write(*,'(a,a)') 'First line: ', line
c
c --- read header - comment lines begin with !
c
      do 10 i=1,99
        iloc=3
        read(kin,'(a80)',err=999,end=999) line
        if (line.eq.blank) goto 40
        if (idb.eq.1) write(*,'(a,a)') 'Test header: ', line
        if (line(1:1).eq.'!') goto 10
        goto 20
 10   continue
c
c --- read and count data lines, check for a factor to scale sigma
c
 20   n = 1
      factor = 1.0
      lfac = 1.0
      if (idb.eq.1) write(*,'(a)') 'Get data...'
      do i = 1, 999
        if(line(1:3).eq.'FAC') then
          iloc=4
          read(line(5:),*,err=999) factor
          if (idb.eq.1) write(*,*) 'factor =', factor
          read(kin,'(a80)',err=40,end=40) line
          if (idb.eq.1) write(*,'(a80)') line
          if (line.eq.blank) goto 40
        elseif(line(1:4).eq.'LFAC') then
          iloc=41
          read(line(6:),*,err=999) lfac
          if (idb.eq.1) write(*,*) 'lambda factor =', lfac
          read(kin,'(a80)',err=40,end=40) line
          if (idb.eq.1) write(*,'(a80)') line
          if (line.eq.blank) goto 40
        else
          read(line,*,err=70,end=70) x1(n), y1(n)
c
c --- calculate sigma*factor and convert lamda to nm
c
          a1(n) = y1(n) * factor
          x1(n) = 1000*x1(n)*lfac
          if (idb.eq.1) write(*,*) x1(n), y1(n)
          read(kin,'(a80)',err=40,end=40) line
          if (idb.eq.1) write(*,'(a80)') line
          if (line.eq.blank) goto 40
          n=n+1
          if (n.gt.kdata) then
            write(*,*) ' In RIUP n > kdata, recompile with kdata =', n
            stop
          endif
        endif
      enddo
c
c --- done reading data
c
 70   n=n-1
 40   close(kin)
      if (idb.eq.1) write(*,*) 'Counted ', n, ' data points'
c
c --- put data onto the modeling wavelength grid
c
      n1 = n
      CALL addpnt(x1,a1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,a1,kdata,n1,          0.,0.)
      CALL addpnt(x1,a1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,a1,kdata,n1,        1E38,0.)
      if (iinterp.eq.2) then
        do iw = 1, n1-1
          a1(iw) = a1(iw)*(x1(iw+1)-x1(iw))
        enddo
        CALL inter3(nw,wl,ag1,n1,x1,a1,0)
      elseif (iinterp.eq.1) then
        CALL inter1(nw,wl,ag1,n1,x1,a1)
      else
        write(*,*) 'Error in RIUP (look in RN)'
        write(*,*) 'Unkown value for iinterp', iinterp
        STOP
      endif
c
c --- second, read the phi values
c
      fname='DATAJ1/IUPAC04/raw_data/'//namphi
      if (idb.eq.1) write(*,'(a,a)') 'Reading ', fname
      iloc=11
      open(unit=kin,file=fname,status='old',err=999)
      iloc=12
      read(kin,'(a80)',err=999,end=999) line
      if (idb.eq.1) write(*,'(a,a)') 'First line: ', line
c
c --- read header - comment lines begin with !
c
      do 110 i=1,99
        iloc=13
        read(kin,'(a80)',err=999,end=999) line
        if (line.eq.blank) goto 140
        if (idb.eq.1) write(*,'(a,a)') 'Test header: ', line
        if (line(1:1).eq.'!') goto 110
        goto 120
 110  continue
c
c --- read and count data lines, check for a factor to scale phi
c
 120  n = 1
      factor = 1.0
      lfac = 1.0
      if (idb.eq.1) write(*,'(a)') 'Get data...'
      do i = 1, 999
        if(line(1:3).eq.'FAC') then
          iloc=14
          read(line(5:),*,err=999) factor
          if (idb.eq.1) write(*,*) 'factor =', factor
          read(kin,'(a80)',err=140,end=140) line
          if (idb.eq.1) write(*,'(a80)') line
          if (line.eq.blank) goto 140
        elseif(line(1:4).eq.'LFAC') then
          iloc=141
          read(line(6:),*,err=999) lfac
          if (idb.eq.1) write(*,*) 'lambda factor =', lfac
          read(kin,'(a80)',err=140,end=140) line
          if (idb.eq.1) write(*,'(a80)') line
          if (line.eq.blank) goto 140
        else
          read(line,*,err=170,end=170) x1(n), y1(n)
c
c --- calculate sigma*factor and convert lamda to nm
c
          a1(n) = y1(n) * factor
          x1(n) = 1000*x1(n)*lfac
          if (idb.eq.1) write(*,*) x1(n), y1(n)
          read(kin,'(a80)',err=140,end=140) line
          if (idb.eq.1) write(*,'(a80)') line
          if (line.eq.blank) goto 140
          n=n+1
          if (n.gt.kdata) then
            write(*,*) ' In RIUP n > kdata, recompile with kdata =', n
            stop
          endif
        endif
      enddo
c
c --- done reading data
c
 170  n=n-1
 140  close(kin)
      if (idb.eq.1) write(*,*) 'Counted ', n, ' data points'
c
c --- put data onto the modeling wavelength grid
c
      n1 = n
      CALL addpnt(x1,a1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,a1,kdata,n1,          0.,0.)
      CALL addpnt(x1,a1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,a1,kdata,n1,        1E38,0.)
      if (iinterp .eq. 2) then
        CALL inter1(nw-1,wc,ag2,n1,x1,a1)
      else
        CALL inter1(nw,wl,ag2,n1,x1,a1)
      endif
c
c --- multiply sigma and phi
c
      DO iw = 1, nw
        if (idb.eq.1) then
          if (iinterp.eq.2) then
             write(*,'(f7.5,1p2e12.4)') 
     &                        wc(iw)/1000., ag1(iw), ag2(iw)
          else
             write(*,'(f7.5,1p2e12.4)') 
     &                        wl(iw)/1000., ag1(iw), ag2(iw)
          endif
        endif
c
c Interpolations are set up to output data on new grid,
c   not put data onto photolysis calculation grid
c        DO i = 1, nz
c          sq(j,i,iw) = ag1(iw)*ag2(iw)
c        ENDDO
      ENDDO
c
      return
c
 999  write(*,*) 'Fatal error reading IUPAC data file:'
      write(*,*) fname
      write(*,*) 'at location = ', iloc , ' look for iloc in rn.f'
      stop
c
      end
