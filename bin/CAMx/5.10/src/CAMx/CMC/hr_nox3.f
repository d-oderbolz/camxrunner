      subroutine hr_nox3(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c-----CAMx v5.10 090918
c
c     HR_NOX3 solves the NOx family using Hertel's equations
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
      real jO3_O1D, rNO2_O3P, rO3P_O3, rO3wNO
      real totO1D, totNO, totNO2, totO3P, totO3
      real lsO1D, lsNO, lsNO2, lsO3P, lsO3
      real yldO3P, rNO_NO2, rNO2_NO
      real t1, t2, t3, t4, t5, f1, f2, f3, f4, s1, s2, A, B, C, Q
c
c --- Entry Point
c
      N2 = M - O2
c
c --- O1D loss
c
      lsO1D = 
     &       + rk( 10)
     &       + rk( 11)*H2O
c
c --- Yield of O3P from O1D
c
      yldO3P  = ( rk( 10) ) / lsO1D
c
c --- Conversion of NO2 to NO
c     (rNO2_NO = rate of NO2 to NO)
c
      rNO2_NO  = dt*(
     &       +          rk(  1)
     &       +          rk(  4)*yh(lO)
     &       + ( 0.200)*rk( 96)*yh(lISOP) )
c
c --- NO production terms except rNO2_NO
c     (totNO = initial NO + new production)
c
      totNO = y0(lNO) + dt*(
     &       + ( 0.110)*r( 14)
     &       +          r( 16)
     &       +          r( 23)
     &       +          r( 25) )
c
c --- Net loss of NO
c     (net either NO or NO2 produced)
c
      lsNO = 1.0 + dt*(
     &       +          rk( 21)*yh(lNO2)*H2O
     &       +          rk( 22)*yh(lOH)
     &       + ( 0.100)*rk( 64)*yh(lTO2)
     &       +          rk( 81)*yh(lXO2N) )
c
c --- Conversion of NO to NO2 except O3+NO (rO3wNO)
c
      rNO_NO2 = dt*(
     &       +          rk(  6)*yh(lO)
     &       +          rk( 15)*yh(lNO3)
     &       + ( 2.000)*rk( 20)*yh(lNO)
     &       +          rk( 28)*yh(lHO2)
     &       +          rk( 46)*yh(lC2O3)
     &       + ( 0.900)*rk( 64)*yh(lTO2)
     &       +          rk( 79)*yh(lXO2) )
c
c --- Remaining NO2 production
c
      totNO2 = y0(lNO2) + dt*(
     &       + ( 0.890)*r( 14)
     &       +          r( 15)
     &       +          r( 19)
     &       +          r( 24)
     &       +          r( 25)
     &       +          r( 30)
     &       +          r( 31)
     &       +          r( 48)
     &       +          r( 59)
     &       + ( 0.200)*r( 78) )
c
c --- Net loss of NO2
c     (net either NO or NO2 produced)
c
      lsNO2 = 1.0 + dt*(
     &       +          rk(  5)*yh(lO)
     &       +          rk(  7)*yh(lO3)
     &       +          rk( 17)*yh(lNO3)
     &       +          rk( 21)*yh(lNO)*H2O
     &       +          rk( 26)*yh(lOH)
     &       +          rk( 29)*yh(lHO2)
     &       +          rk( 47)*yh(lC2O3)
     &       +          rk( 55)*yh(lROR)
     &       +          rk( 68)*yh(lCRO)
     &       + ( 0.800)*rk( 96)*yh(lISOP) )
c
c --- Production of O3P except NO2+hv
c
      totO3P = y0(lO) + dt*(
     &       +          r(  8)
     &       +          r(  9)*yldO3P
     &       + ( 0.890)*r( 14) )
c
c --- Net loss of O3P
c
      lsO3P = 1.0 + dt*(
     &       +          rk(  2)
     &       +          rk(  4)*yh(lNO2)
     &       +          rk(  5)*yh(lNO2)
     &       +          rk(  6)*yh(lNO)
     &       +          rk( 40)*yh(lFORM)
     &       +          rk( 42)*yh(lALD2)
     &       +          rk( 56)*yh(lOLE)
     &       +          rk( 60)*yh(lETH)
     &       +          rk( 75)*yh(lISOP) )
c
c --- Production of O3 except O3P+O2
c
      totO3 = y0(lO3)
c
c --- Net loss of O3 except O3+NO (rO3wNO)
c
      lsO3 = 1.0 + dt*(
     &       +          rk(  7)*yh(lNO2)
     &       +          rk(  8)
     &       +          rk(  9)
     &       +          rk( 12)*yh(lOH)
     &       +          rk( 13)*yh(lHO2)
     &       +          rk( 58)*yh(lOLE)
     &       +          rk( 62)*yh(lETH)
     &       +          rk( 71)*yh(lOPEN)
     &       +          rk( 77)*yh(lISOP)
     &       +          rk( 93)*yh(lISPD) )
c
c --- Specific reactions
c
      jO3_O1D  = rk(  9)
      rNO2_O3P = dt*rk(  1)
      rO3P_O3  = dt*rk(  2)
      rO3wNO   = dt*rk(  3)
c
c --- Collect common terms
c
      t1 = rNO2_O3P / lsNO2
      t2 = rNO2_NO / lsNO2
      t3 = rNO_NO2 / lsNO
      t4 = rO3P_O3  / lsO3P
      t5 = t3*totNO - t2*totNO2
      f1 = 1.0 + t2 + t3
      f2 = t1*t4
      f3 = lsO3*lsNO + rO3wNO*totNO
      f4 = totO3 + totO3P * t4
c
c --- Solve for change in NO and NO2 
c
      A = rO3wNO * (f1 - f2)
      B = f1*f3 + rO3wNO*(f2*(totNO2 - totNO) + f4 + t5)
      C = rO3wNO*totNO*(f4 + totNO2 * f2) + f3*t5
      Q = -0.5 * (B + SIGN(1.0,B)*SQRT(B*B - 4.0*A*C))
      Q = MAX(Q/A ,C/Q)
c
c --- Update Concentrations
c
      y1(lNO)  = MAX(1.0E-25, (totNO + Q) / lsNO )
      y1(lNO2) = MAX(1.0E-25, (totNO2 - Q) / lsNO2 )
      s1 = totO3P + rNO2_O3P*y1(lNO2)
      s2 = t4*s1
      y1(lO3)  = MAX(1.0E-25, (totO3 + s2) / 
     &                             ( lsO3 + rO3wNO * y1(lNO) ) )
      y1(lO)   = MAX(1.0E-35, s1 / lsO3P )
      totO1D = jO3_O1D*y1(lO3)
      y1(lO1D) = MAX(1.0E-35, totO1D / lsO1D )
c
      return
      end

