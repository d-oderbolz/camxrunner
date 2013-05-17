      subroutine hr_nox5(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c----CAMx v5.41 121109
c
c     HR_NOX5 solves the NOx family using Hertel's equations
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
     &       + rk( 19)*H2O
     &       + rk( 20)*M
c
c --- Yield of O3P from O1D
c
      yldO3P  = ( rk( 20)*M ) / lsO1D
c
c --- Conversion of NO2 to NO
c     (rNO2_NO = rate of NO2 to NO)
c
      rNO2_NO  = dt*(
     &       +          rk(  1)
     &       +          rk(  5)*yh(lO) )
c
c --- NO production terms except rNO2_NO
c     (totNO = initial NO + new production)
c
      totNO = y0(lNO) + dt*(
     &       +          r( 14)
     &       +          r( 15)
     &       +          r( 22) )
c
c --- Net loss of NO
c     (net either NO or NO2 produced)
c
      lsNO = 1.0 + dt*(
     &       +          rk( 21)*yh(lOH)
     &       +          rk( 62)*yh(lRO2N) )
c
c --- Conversion of NO to NO2 except O3+NO (rO3wNO)
c
      rNO_NO2 = dt*(
     &       +          rk(  4)*yh(lO)*M
     &       +          rk(  9)*yh(lNO3)
     &       + ( 2.000)*rk( 10)*yh(lNO)*O2
     &       +          rk( 31)*yh(lHO2)
     &       +          rk( 46)*yh(lCXO2)
     &       +          rk( 51)*yh(lRO2R)
     &       +          rk( 56)*yh(lR2O2)
     &       +          rk( 71)*yh(lCCO3)
     &       +          rk( 81)*yh(lRCO3)
     &       +          rk( 92)*yh(lBZCO)
     &       +          rk(104)*yh(lMCO3)
     &       +          rk(128)*yh(lHCO3) )
c
c --- Remaining NO2 production
c
      totNO2 = y0(lNO2) + dt*(
     &       +          r(  9)
     &       +          r( 12)
     &       +          r( 16)
     &       +          r( 23)
     &       +          r( 24)
     &       +          r( 26)
     &       +          r( 28)
     &       +          r( 33)
     &       + ( 0.610)*r( 34)
     &       +          r( 35)
     &       + ( 0.800)*r( 39)
     &       + ( 2.000)*r( 40)
     &       +          r( 48)
     &       +          r( 53)
     &       +          r( 58)
     &       +          r( 65)
     &       +          r( 70)
     &       +          r( 73)
     &       +          r( 80)
     &       +          r( 83)
     &       +          r( 91)
     &       +          r( 94)
     &       +          r(103)
     &       +          r(106)
     &       + ( 0.338)*r(176)
     &       +          r(177)
     &       + ( 0.187)*r(191)
     &       + ( 0.474)*r(195)
     &       + ( 0.391)*r(210) )
c
c --- Net loss of NO2
c     (net either NO or NO2 produced)
c
      lsNO2 = 1.0 + dt*(
     &       +          rk(  6)*yh(lO)
     &       +          rk(  8)*yh(lO3)
     &       +          rk( 11)*yh(lNO3)
     &       +          rk( 25)*yh(lOH)
     &       +          rk( 32)*yh(lHO2)
     &       +          rk( 69)*yh(lCCO3)
     &       +          rk( 79)*yh(lRCO3)
     &       +          rk( 90)*yh(lBZCO)
     &       +          rk(102)*yh(lMCO3)
     &       +          rk(115)*yh(lTBUO)
     &       +          rk(117)*yh(lBZO)
     &       +          rk(120)*yh(lBZNO) )
c
c --- Production of O3P except NO2+hv
c
      totO3P = y0(lO) + dt*(
     &       +          r( 16)
     &       +          r( 17)
     &       +          r( 18)*yldO3P )
c
c --- Net loss of O3P
c
      lsO3P = 1.0 + dt*(
     &       +          rk(  2)*O2*M
     &       +          rk(  3)*yh(lO3)
     &       +          rk(  4)*yh(lNO)*M
     &       +          rk(  5)*yh(lNO2)
     &       +          rk(  6)*yh(lNO2)
     &       +          rk(164)*yh(lMETH)
     &       +          rk(168)*yh(lMVK)
     &       +          rk(188)*yh(lETHE)
     &       +          rk(192)*yh(lISOP)
     &       +          rk(196)*yh(lTERP)
     &       +          rk(207)*yh(lOLE1)
     &       +          rk(211)*yh(lOLE2)
     &       +          rk(217)*yh(lMBUT) )
c
c --- Production of O3 except O3P+O2
c
      totO3 = y0(lO3) + dt*(
     &       + ( 0.250)*r( 72)
     &       + ( 0.250)*r( 82)
     &       + ( 0.250)*r( 93)
     &       + ( 0.250)*r(105) )
c
c --- Net loss of O3 except O3+NO (rO3wNO)
c
      lsO3 = 1.0 + dt*(
     &       +          rk(  3)*yh(lO)
     &       +          rk(  8)*yh(lNO2)
     &       +          rk( 17)
     &       +          rk( 18)
     &       +          rk( 30)*yh(lOH)
     &       +          rk( 36)*yh(lHO2)
     &       +          rk(162)*yh(lMETH)
     &       +          rk(167)*yh(lMVK)
     &       +          rk(171)*yh(lISPD)
     &       +          rk(179)*yh(lDCB1)
     &       +          rk(186)*yh(lETHE)
     &       +          rk(190)*yh(lISOP)
     &       +          rk(194)*yh(lTERP)
     &       +          rk(205)*yh(lOLE1)
     &       +          rk(209)*yh(lOLE2)
     &       +          rk(215)*yh(lMBUT) )
c
c --- Specific reactions
c
      jO3_O1D  = rk( 18)
      rNO2_O3P = dt*rk(  1)
      rO3P_O3  = dt*rk(  2)*O2*M
      rO3wNO   = dt*rk(  7)
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

