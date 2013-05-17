      subroutine hr_pan5(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c----CAMx v5.41 121109
c
c     HR_PAN5 solves PAN and RCO3 using Hertel's equations
c
c     Copyright 1996 - 2012
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
     &       +          r(104)
     &       +          r(106)
     &       +          r(112)
     &       +          r(113)
     &       + ( 2.000)*r(114)
     &       +          r(130)
     &       +          r(132)
     &       +          r(136)
     &       +          r(137)
     &       + ( 0.492)*r(138)
     &       +          r(139)
     &       +          r(149)
     &       +          r(150)
     &       +          r(151)
     &       + ( 2.000)*r(152)
     &       + ( 0.670)*r(165)
     &       + ( 0.675)*r(166)
     &       + ( 0.467)*r(173)
     &       + ( 0.029)*r(174)
     &       + ( 0.667)*r(175)
     &       +          r(180)
     &       + ( 0.500)*r(181)
     &       +          r(182)
     &       + ( 0.500)*r(183)
     &       + ( 0.123)*r(194)
     &       + ( 0.011)*r(200)
     &       + ( 0.137)*r(209) 
c
      lsRCO3 = 
     &       +          rk( 69)*yh(lNO2)
     &       +          rk( 71)*yh(lNO)
     &       +          rk( 72)*yh(lHO2)
     &       +          rk( 73)*yh(lNO3)
     &       +          rk( 74)*yh(lCXO2)
     &       +          rk( 75)*yh(lRO2R)
     &       +          rk( 77)*yh(lRO2N)
     &       +          rk( 88)*yh(lRCO3)
     &       +          rk( 99)*yh(lBZCO) 
c
      lsPAN = 
     &       + rk( 70)
c
      fwd  = dt*( rk( 69) ) * yh(lNO2)
      bck  = dt*( rk( 70) )
      self = dt*rk( 78)
      lsPAN  = 1.0 + dt*lsPAN
      lsRCO3 = 1.0 + dt*lsRCO3
c
c-----Solve for peroxyacetyl radical
c
      A = 2.0 * self * lsPAN
      B = ( lsPAN*lsRCO3) - (fwd*bck)
      C = lsPAN*( y0(lCCO3) + newRCO3*dt ) + bck*y0(lPAN)
      Q = -0.5 * ( B + SIGN(1.0,B)*SQRT(B*B + 4.0*A*C) )
c
c --- Update Concentrations
c
      y1(lCCO3) = MAX(1.0E-25, MAX(Q/A, -C/Q) )
      y1(lPAN)  = MAX(1.0E-25, (y0(lPAN) + fwd*y1(lCCO3)) / lsPAN )
c
      return
      end

