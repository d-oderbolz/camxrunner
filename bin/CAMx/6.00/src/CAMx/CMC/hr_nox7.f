      subroutine hr_nox7(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c----CAMx v6.00 130506
c
c     HR_NOX7 solves the NOx family using Hertel's equations
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
     &       +          rk(  5)*yh(lO) )
c
c --- NO production terms except rNO2_NO
c     (totNO = initial NO + new production)
c
      totNO = y0(lNO) + dt*(
     &       +          r( 28)
     &       +          r( 30)
     &       +          r( 42)
     &       +          r( 43)
     &       + ( 0.000)*r( 68) )
c
c --- Net loss of NO
c     (net either NO or NO2 produced)
c
      lsNO = 1.0 + dt*(
     &       +          rk( 40)*yh(lOH)
     &       +          rk( 41)*yh(lNO2)*H2O
     &       +          rk( 83)*yh(lXO2N)
     &       + ( 0.117)*rk(150)*yh(lISO2)
     &       + ( 0.082)*rk(173)*yh(lBZO2)
     &       + ( 0.140)*rk(178)*yh(lTO2)
     &       + ( 0.140)*rk(183)*yh(lXLO2)
     &       + ( 0.140)*rk(209)*yh(lCAO2) )
c
c --- Conversion of NO to NO2 except O3+NO (rO3wNO)
c
      rNO_NO2 = dt*(
     &       +          rk(  4)*yh(lO)*M
     &       + ( 2.000)*rk( 24)*yh(lNO)*O2
     &       +          rk( 25)*yh(lHO2)
     &       +          rk( 29)*yh(lNO3)
     &       +          rk( 53)*yh(lC2O3)
     &       +          rk( 61)*yh(lCXO3)
     &       +          rk( 71)*yh(lMEO2)
     &       +          rk( 75)*yh(lXO2H)
     &       +          rk( 79)*yh(lXO2)
     &       +          rk(103)*yh(lHCO3)
     &       + ( 0.883)*rk(150)*yh(lISO2)
     &       +          rk(164)*yh(lEPX2)
     &       + ( 0.918)*rk(173)*yh(lBZO2)
     &       + ( 0.860)*rk(178)*yh(lTO2)
     &       + ( 0.860)*rk(183)*yh(lXLO2)
     &       +          rk(195)*yh(lCRN2)
     &       + ( 0.860)*rk(209)*yh(lCAO2)
     &       +          rk(213)*yh(lOPO3) )
c
c --- Remaining NO2 production
c
      totNO2 = y0(lNO2) + dt*(
     &       +          r( 27)
     &       +          r( 29)
     &       +          r( 31)
     &       +          r( 32)
     &       +          r( 33)
     &       +          r( 34)
     &       + ( 2.000)*r( 35)
     &       +          r( 37)
     &       +          r( 38)
     &       +          r( 42)
     &       +          r( 44)
     &       +          r( 47)
     &       +          r( 49)
     &       + ( 0.590)*r( 50)
     &       +          r( 51)
     &       +          r( 55)
     &       + ( 0.600)*r( 56)
     &       +          r( 63)
     &       + ( 0.600)*r( 64)
     &       +          r( 92)
     &       + ( 0.500)*r(140)
     &       + ( 0.500)*r(144)
     &       + ( 0.500)*r(148)
     &       + ( 0.350)*r(156)
     &       + ( 0.444)*r(167)
     &       + ( 0.470)*r(171)
     &       + ( 0.500)*r(202)
     &       +          r(215) )
c
c --- Net loss of NO2
c     (net either NO or NO2 produced)
c
      lsNO2 = 1.0 + dt*(
     &       +          rk(  6)*yh(lO)
     &       +          rk( 26)*yh(lO3)
     &       +          rk( 36)*yh(lNO3)
     &       +          rk( 41)*yh(lNO)*H2O
     &       +          rk( 45)*yh(lOH)
     &       +          rk( 48)*yh(lHO2)
     &       +          rk( 54)*yh(lC2O3)
     &       +          rk( 62)*yh(lCXO3)
     &       +          rk(135)*yh(lROR)
     &       +          rk(189)*yh(lCRO)
     &       +          rk(193)*yh(lCRNO)
     &       +          rk(214)*yh(lOPO3) )
c
c --- Production of O3P except NO2+hv
c
      totO3P = y0(lO) + dt*(
     &       +          r(  8)
     &       +          r(  9)*yldO3P
     &       +          r( 16)
     &       +          r( 27) )
c
c --- Net loss of O3P
c
      lsO3P = 1.0 + dt*(
     &       +          rk(  2)*O2*M
     &       +          rk(  4)*yh(lNO)*M
     &       +          rk(  5)*yh(lNO2)
     &       +          rk(  6)*yh(lNO2)
     &       +          rk(  7)*yh(lO3)
     &       +          rk( 14)*yh(lOH)
     &       +          rk( 15)*yh(lHO2)
     &       +          rk( 23)*yh(lH2O2)
     &       +          rk( 31)*yh(lNO3)
     &       +          rk( 99)*yh(lFORM)
     &       +          rk(105)*yh(lALD2)
     &       +          rk(109)*yh(lALDX)
     &       +          rk(137)*yh(lETH)
     &       +          rk(141)*yh(lOLE)
     &       +          rk(145)*yh(lIOLE)
     &       +          rk(168)*yh(lTERP) )
c
c --- Production of O3 except O3P+O2
c
      totO3 = y0(lO3) + dt*(
     &       + ( 0.150)*r( 57)
     &       + ( 0.150)*r( 65)
     &       + ( 0.150)*r(216) )
c
c --- Net loss of O3 except O3+NO (rO3wNO)
c
      lsO3 = 1.0 + dt*(
     &       +          rk(  7)*yh(lO)
     &       +          rk(  8)
     &       +          rk(  9)
     &       +          rk( 12)*yh(lOH)
     &       +          rk( 13)*yh(lHO2)
     &       +          rk( 26)*yh(lNO2)
     &       +          rk( 34)*yh(lNO3)
     &       +          rk(139)*yh(lETH)
     &       +          rk(143)*yh(lOLE)
     &       +          rk(147)*yh(lIOLE)
     &       +          rk(155)*yh(lISOP)
     &       +          rk(158)*yh(lISPD)
     &       +          rk(170)*yh(lTERP)
     &       +          rk(194)*yh(lCRNO)
     &       +          rk(201)*yh(lXOPN)
     &       +          rk(205)*yh(lOPEN) )
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

