      subroutine hr_pan9(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c----CAMx v6.00 130506
c
c     HR_PAN9 solves PAN and RCO3 using Hertel's equations
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c     Created by the CMC version 5.2
c
c --- Subroutines Called:
c        none
c
c --- Called by:
c        EBISOLV
c
c --- Argument definitions:
c        y0  - initial Conc for this step (ppm)
c        yh  - current Conc iteration (ppm)
c        y1  - next Conc iteration (ppm)
c        H2O - water vapor Conc (ppm)
c        M   - total gas Conc (ppm)
c        O2  - oxygen Conc (ppm)
c        CH4 - methane Conc (ppm)
c        H2  - hydrogen Conc (ppm)
c        ny  - dimension of y0, y1 and yh
c        rk  - rate constants (ppm-n hr-1)
c        r   - reaction rates (hr-1)
c        nr  - dimension of rk and r
c        dt  - time step (hr)
c
c --- Includes:
      include "camx.prm"
      include "chmdat.inc"
      include "ddmchm.inc"
c
c --- Arguments:
      integer ny, nr
      real y0(ny+1), y1(ny+1), yh(ny+1)
      real rk(nr), r(nr)
      real H2O, M, O2, CH4, H2, dt
c
c --- Local variables:
      real N2
      real fwd, bck, self, A, B, C, Q
      real newRCO3, lsPAN, lsRCO3
c
c --- Entry Point
c
      N2 = M - O2
c
      newRCO3 = 
     &       +          r( 84)
     &       +          r( 85)
     &       +          r( 86)
     &       +          r( 96)
     &       +          r(138)
     &       +          r(139)
     &       + ( 0.620)*r(140)
     &       +          r(142)
     &       +          r(143)
     &       + ( 0.210)*r(149)
     &       + ( 0.114)*r(150)
     &       + ( 0.967)*r(152)
     &       +          r(183)
     &       +          r(222) 
c
      lsRCO3 = 
     &       +          rk( 88)*yh(lNO)
     &       +          rk( 89)*yh(lNO2)
     &       +          rk( 92)*yh(lHO2)
     &       +          rk( 93)*yh(lMEO2)
     &       +          rk( 94)*yh(lXO2)
     &       +          rk(112)*yh(lCXO3) 
c
      lsPAN = 
     &       + rk( 90)
     &       + rk( 91)
c
      fwd  = dt*( rk( 89) ) * yh(lNO2)
      bck  = dt*( rk( 90) + rk( 91) )
      self = dt*rk( 95)
      lsPAN  = 1.0 + dt*lsPAN
      lsRCO3 = 1.0 + dt*lsRCO3
c
c-----Solve for peroxyacetyl radical
c
      A = 2.0 * self * lsPAN
      B = ( lsPAN*lsRCO3) - (fwd*bck)
      C = lsPAN*( y0(lC2O3) + newRCO3*dt ) + bck*y0(lPAN)
      Q = -0.5 * ( B + SIGN(1.0,B)*SQRT(B*B + 4.0*A*C) )
c
c --- Update Concentrations
c
      y1(lC2O3) = MAX(1.0E-25, MAX(Q/A, -C/Q) )
      y1(lPAN)  = MAX(1.0E-25, (y0(lPAN) + fwd*y1(lC2O3)) / lsPAN )
c
      return
      end

