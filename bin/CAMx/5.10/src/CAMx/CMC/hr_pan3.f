      subroutine hr_pan3(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c-----CAMx v5.10 090918
c
c     HR_PAN3 solves PAN and RCO3 using Hertel's equations
c
c     Copyright 1996 - 2009
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
      include "chmdat.com"
      include "ddmchm.com"
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
     &       +          r( 42)
     &       +          r( 43)
     &       +          r( 44)
     &       +          r( 69)
     &       +          r( 70)
     &       + ( 0.620)*r( 71)
     &       +          r( 73)
     &       +          r( 74)
     &       + ( 0.250)*r( 75)
     &       + ( 0.200)*r( 77)
     &       + ( 0.498)*r( 92)
     &       + ( 0.114)*r( 93)
     &       + ( 0.075)*r( 94)
     &       + ( 0.967)*r( 95) 
c
      lsRCO3 = 
     &       +          rk( 46)*yh(lNO)
     &       +          rk( 47)*yh(lNO2)
     &       +          rk( 50)*yh(lHO2) 
c
      lsPAN = 
     &       + rk( 48)
c
      fwd  = dt*( rk( 47) ) * yh(lNO2)
      bck  = dt*( rk( 48) )
      self = dt*rk( 49)
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

