      subroutine hr_nxy7(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c----CAMx v6.00 130506
c
c     HR_NXY7 solves NO3 and N2O5 using Hertel's equations
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
      real self, fwd, bck
      real newNO3, lsNO3, lsN2O5, A, B, C, Q, t1
c
c --- Entry Point
c
      N2 = M - O2
c
      newNO3 = 
     &       +          r(  6)
     &       +          r( 26)
     &       +          r( 46)
     &       + ( 0.410)*r( 50)
     &       + ( 0.400)*r( 56)
     &       + ( 0.400)*r( 64)
     &       + ( 0.185)*r(167) 
c
      lsN2O5 = 
     &       +          rk( 37)
     &       +          rk( 38)
     &       +          rk( 39)*H2O 
c
      lsNO3 = 
     &       +          rk( 27)
     &       +          rk( 28)
     &       +          rk( 29)*yh(lNO)
     &       +          rk( 30)*yh(lNO2)
     &       +          rk( 31)*yh(lO)
     &       +          rk( 32)*yh(lOH)
     &       +          rk( 33)*yh(lHO2)
     &       +          rk( 34)*yh(lO3)
     &       +          rk( 36)*yh(lNO2)
     &       +          rk(100)*yh(lFORM)
     &       +          rk(107)*yh(lALD2)
     &       +          rk(111)*yh(lALDX)
     &       +          rk(115)*yh(lGLYD)
     &       +          rk(118)*yh(lGLY)
     &       +          rk(120)*yh(lMGLY)
     &       +          rk(140)*yh(lETH)
     &       +          rk(144)*yh(lOLE)
     &       +          rk(148)*yh(lIOLE)
     &       +          rk(156)*yh(lISOP)
     &       +          rk(159)*yh(lISPD)
     &       +          rk(171)*yh(lTERP)
     &       +          rk(188)*yh(lCRES)
     &       +          rk(192)*yh(lCRON)
     &       +          rk(202)*yh(lXOPN)
     &       +          rk(206)*yh(lOPEN)
     &       +          rk(208)*yh(lCAT1) 
c
      self = dt*rk( 35)
      fwd = dt*( rk( 36) ) * yh(lNO2)
      bck = dt*( rk( 37) + rk( 38) )
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

