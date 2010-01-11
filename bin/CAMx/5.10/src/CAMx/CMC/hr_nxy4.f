      subroutine hr_nxy4(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c-----CAMx v5.10 090918
c
c     HR_NXY4 solves NO3 and N2O5 using Hertel's equations
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
      real self, fwd, bck
      real newNO3, lsNO3, lsN2O5, totNO3, A, B, C, Q, t1
c
c --- Entry Point
c
      N2 = M - O2
c
      newNO3 = 
     &       +          r(  5)
     &       +          r(  7)
     &       +          r( 27)
     &       + ( 0.390)*r(113) 
c
      lsN2O5 = 
     &       +          rk( 18)*H2O
     &       +          rk( 19)
     &       +          rk(111) 
c
      lsNO3 = 
     &       +          rk( 14)
     &       +          rk( 15)*yh(lNO)
     &       +          rk( 16)*yh(lNO2)
     &       +          rk( 17)*yh(lNO2)
     &       +          rk( 41)*yh(lFORM)
     &       +          rk( 44)*yh(lALD2)
     &       +          rk( 59)*yh(lOLE)
     &       +          rk( 67)*yh(lCRES)
     &       +          rk( 78)*yh(lISOP)
     &       +          rk( 94)*yh(lISPD)
     &       +          rk(104)*yh(lO)
     &       +          rk(105)*yh(lOH)
     &       +          rk(106)*yh(lHO2)
     &       +          rk(107)*yh(lO3) 
c
      self = dt*rk(108)
      fwd = dt*( rk( 17) ) * yh(lNO2)
      bck = dt*( rk( 19) + rk(111) )
c
c --- Collect terms
c
      t1 = 1.0 + dt*lsN2O5
      A = 2.0*fwd*t1
      B = t1*( 1.0 + dt*lsNO3 ) - bck*fwd
      C = t1*( y0(lNO3) + dt*newNO3 ) + bck*y0(lN2O5)
      Q = -0.5*(B + SIGN(1.0,B)*SQRT(B*B + 4.0*A*C))
      Q = MAX(Q/A ,-C/Q)
c
c --- Update Concentrations
c
      y1(lNO3)  = MAX(1.0E-25, Q)
      y1(lN2O5) = MAX(1.0E-25, ( y0(lN2O5) + fwd*y1(lNO3) )/t1)
c
      return
      end

