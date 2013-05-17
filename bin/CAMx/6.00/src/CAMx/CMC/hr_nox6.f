      subroutine hr_nox6(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c----CAMx v6.00 130506
c
c     HR_NOX6 solves the NOx family using Hertel's equations
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
     &       + rk( 10)*M
     &       + rk( 11)*H2O
     &       + rk( 38)*H2
c
c --- Yield of O3P from O1D
c
      yldO3P  = ( rk( 10)*M ) / lsO1D
c
c --- Conversion of NO2 to NO
c     (rNO2_NO = rate of NO2 to NO)
c
      rNO2_NO  = dt*(
     &       +          rk(  1)
     &       +          rk(  4)*yh(lO)
     &       + ( 0.200)*rk(148)*yh(lISOP) )
c
c --- NO production terms except rNO2_NO
c     (totNO = initial NO + new production)
c
      totNO = y0(lNO) + dt*(
     &       +          r( 15)
     &       +          r( 17)
     &       +          r( 25)
     &       +          r( 27) )
c
c --- Net loss of NO
c     (net either NO or NO2 produced)
c
      lsNO = 1.0 + dt*(
     &       +          rk( 23)*yh(lNO2)*H2O
     &       +          rk( 24)*yh(lOH)
     &       +          rk( 55)*yh(lXO2N)
     &       + ( 0.100)*rk(132)*yh(lTO2) )
c
c --- Conversion of NO to NO2 except O3+NO (rO3wNO)
c
      rNO_NO2 = dt*(
     &       +          rk(  6)*yh(lO)
     &       +          rk( 16)*yh(lNO3)
     &       + ( 2.000)*rk( 22)*yh(lNO)*O2
     &       +          rk( 30)*yh(lHO2)
     &       +          rk( 54)*yh(lXO2)
     &       +          rk( 68)*yh(lMEO2)
     &       +          rk( 81)*yh(lHCO3)
     &       +          rk( 88)*yh(lC2O3)
     &       +          rk(103)*yh(lCXO3)
     &       + ( 0.900)*rk(132)*yh(lTO2) )
c
c --- Remaining NO2 production
c
      totNO2 = y0(lNO2) + dt*(
     &       +          r( 14)
     &       +          r( 16)
     &       +          r( 21)
     &       +          r( 26)
     &       +          r( 27)
     &       +          r( 32)
     &       +          r( 33)
     &       +          r( 46)
     &       +          r( 47)
     &       +          r( 49)
     &       + ( 2.000)*r( 50)
     &       + ( 0.610)*r( 51)
     &       +          r( 52)
     &       +          r( 53)
     &       +          r( 62)
     &       +          r( 90)
     &       +          r( 91)
     &       +          r(105)
     &       +          r(106)
     &       +          r(107)
     &       +          r(122)
     &       +          r(126)
     &       +          r(130)
     &       + ( 0.200)*r(147)
     &       + ( 0.470)*r(156) )
c
c --- Net loss of NO2
c     (net either NO or NO2 produced)
c
      lsNO2 = 1.0 + dt*(
     &       +          rk(  5)*yh(lO)
     &       +          rk(  7)*yh(lO3)
     &       +          rk( 18)*yh(lNO3)
     &       +          rk( 23)*yh(lNO)*H2O
     &       +          rk( 28)*yh(lOH)
     &       +          rk( 31)*yh(lHO2)
     &       +          rk( 89)*yh(lC2O3)
     &       +          rk(104)*yh(lCXO3)
     &       +          rk(118)*yh(lROR)
     &       +          rk(136)*yh(lCRO)
     &       + ( 0.800)*rk(148)*yh(lISOP) )
c
c --- Production of O3P except NO2+hv
c
      totO3P = y0(lO) + dt*(
     &       +          r(  8)
     &       +          r(  9)*yldO3P
     &       +          r( 14)
     &       +          r( 41)
     &       + ( 0.500)*r(129) )
c
c --- Net loss of O3P
c
      lsO3P = 1.0 + dt*(
     &       +          rk(  2)*O2*M
     &       +          rk(  4)*yh(lNO2)
     &       +          rk(  5)*yh(lNO2)
     &       +          rk(  6)*yh(lNO)
     &       +          rk( 40)*yh(lOH)
     &       +          rk( 44)*yh(lHO2)
     &       +          rk( 45)*yh(lH2O2)
     &       +          rk( 46)*yh(lNO3)
     &       +          rk( 77)*yh(lFORM)
     &       +          rk( 84)*yh(lALD2)
     &       +          rk( 99)*yh(lALDX)
     &       +          rk(119)*yh(lOLE)
     &       +          rk(123)*yh(lETH)
     &       +          rk(127)*yh(lIOLE)
     &       +          rk(144)*yh(lISOP)
     &       +          rk(153)*yh(lTERP) )
c
c --- Production of O3 except O3P+O2
c
      totO3 = y0(lO3) + dt*(
     &       + ( 0.200)*r( 92)
     &       + ( 0.200)*r(108) )
c
c --- Net loss of O3 except O3+NO (rO3wNO)
c
      lsO3 = 1.0 + dt*(
     &       +          rk(  7)*yh(lNO2)
     &       +          rk(  8)
     &       +          rk(  9)
     &       +          rk( 12)*yh(lOH)
     &       +          rk( 13)*yh(lHO2)
     &       +          rk( 49)*yh(lNO3)
     &       +          rk(121)*yh(lOLE)
     &       +          rk(125)*yh(lETH)
     &       +          rk(129)*yh(lIOLE)
     &       +          rk(140)*yh(lOPEN)
     &       +          rk(146)*yh(lISOP)
     &       +          rk(150)*yh(lISPD)
     &       +          rk(155)*yh(lTERP) )
c
c --- Specific reactions
c
      jO3_O1D  = rk(  9)
      rNO2_O3P = dt*rk(  1)
      rO3P_O3  = dt*rk(  2)*O2*M
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

