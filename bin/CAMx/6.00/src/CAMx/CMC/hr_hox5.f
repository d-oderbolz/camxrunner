      subroutine hr_hox5(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c----CAMx v6.00 130506
c
c     HR_HOX5 solves the HOx family using Hertel's equations
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
      real t1, t2, t3, A, B, C, Q
      real newOH, newHO2, newHONO, newPNA
      real lsOH, lsHO2, lsHONO, lsPNA, lsO1D, yldOH, self
      real rOH_HO2, rHO2_OH, rOH_HONO, rHONO_OH, rHO2_PNA, rPNA_HO2
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
c --- Yield of OH from O1D
c
      yldOH  = ( 
     &         ( 2.000)*rk( 19)*H2O ) / lsO1D
c
c --- Conversion of HONO to OH
c
      rHONO_OH  = dt*(
     &       +          rk( 22) )
c
c --- Conversion of HO2 to OH
c
      rHO2_OH  = dt*(
     &       +          rk( 31)*yh(lNO)
     &       +          rk( 36)*yh(lO3)
     &       + ( 0.800)*rk( 39)*yh(lNO3) )
c
c --- Other OH production terms
c
      newOH = y0(lOH) + dt*(
     &       +          r( 28)
     &       + ( 0.390)*r( 34)
     &       + ( 2.000)*r( 41)
     &       +          r(142)
     &       +          r(144)
     &       + ( 0.208)*r(162)
     &       + ( 0.330)*r(165)
     &       + ( 0.164)*r(167)
     &       + ( 0.285)*r(171)
     &       + ( 0.500)*r(179)
     &       + ( 0.120)*r(186)
     &       + ( 0.266)*r(190)
     &       + ( 0.567)*r(194)
     &       + ( 0.155)*r(205)
     &       + ( 0.378)*r(209)
     &       + ( 0.099)*r(215)
     &       +          r( 18)*yldOH )
c
c --- Conversion of PNA to HO2
c
      rPNA_HO2  = dt*(
     &       +          rk( 33)
     &       + ( 0.610)*rk( 34) )
c
c --- Conversion of OH to HO2
c
      rOH_HO2  = dt*(
     &       +          rk( 26)*yh(lNO3)
     &       +          rk( 29)*yh(lCO)
     &       +          rk( 30)*yh(lO3)
     &       +          rk( 42)*yh(lHO2H)
     &       +          rk( 44)*yh(lSO2)
     &       +          rk( 45)*H2
     &       +          rk(125)*yh(lHCHO)
     &       +          rk(140)*yh(lMEOH)
     &       + ( 0.630)*rk(147)*yh(lGLY)
     &       + ( 0.379)*rk(174)*yh(lPROD)
     &       + ( 0.113)*rk(176)*yh(lRNO3)
     &       + ( 0.121)*rk(198)*yh(lALK2)
     &       + ( 0.224)*rk(202)*yh(lARO1)
     &       + ( 0.187)*rk(203)*yh(lARO2)
     &       + ( 0.950)*rk(212)*yh(lETOH) )
c
c --- Other HO2 production terms
c
      newHO2 = y0(lHO2) + dt*(
     &       +          r( 23)
     &       +          r( 46)
     &       +          r( 48)
     &       + ( 2.000)*r( 50)
     &       +          r( 51)
     &       +          r( 53)
     &       +          r( 54)
     &       +          r( 55)
     &       +          r( 64)
     &       +          r( 65)
     &       +          r( 66)
     &       +          r( 68)
     &       + ( 2.000)*r(123)
     &       +          r(127)
     &       +          r(128)
     &       +          r(129)
     &       +          r(131)
     &       +          r(134)
     &       +          r(142)
     &       +          r(144)
     &       + ( 2.000)*r(145)
     &       + ( 0.630)*r(148)
     &       +          r(149)
     &       + ( 0.008)*r(162)
     &       + ( 0.340)*r(165)
     &       + ( 0.064)*r(167)
     &       + ( 0.400)*r(171)
     &       + ( 1.233)*r(173)
     &       + ( 0.341)*r(177)
     &       + ( 1.500)*r(179)
     &       + ( 0.500)*r(181)
     &       + ( 0.500)*r(183)
     &       + ( 0.120)*r(186)
     &       + ( 0.500)*r(188)
     &       + ( 0.033)*r(194)
     &       + ( 0.056)*r(205)
     &       + ( 0.003)*r(209)
     &       + ( 0.013)*r(211)
     &       + ( 0.099)*r(215) )
c
c
c --- Conversion of OH to HONO
c
      rOH_HONO  = dt*(
     &       +          rk( 21)*yh(lNO) )
c
c --- Other HONO production terms
c
      newHONO = y0(lHONO) 
c
c --- Conversion of HO2 to PNA
c
      rHO2_PNA  = dt*(
     &       +          rk( 32)*yh(lNO2) )
c
c --- Other PNA production terms
c
      newPNA = y0(lHNO4) 
c
c --- Net loss of OH
c
      lsOH = 1.0 + dt*(
     &       +          rk( 21)*yh(lNO)
     &       +          rk( 24)*yh(lHONO)
     &       +          rk( 25)*yh(lNO2)
     &       +          rk( 26)*yh(lNO3)
     &       +          rk( 27)*yh(lHNO3)
     &       +          rk( 29)*yh(lCO)
     &       +          rk( 30)*yh(lO3)
     &       +          rk( 35)*yh(lHNO4)
     &       +          rk( 42)*yh(lHO2H)
     &       +          rk( 43)*yh(lHO2)
     &       +          rk( 44)*yh(lSO2)
     &       +          rk( 45)*H2
     &       +          rk(125)*yh(lHCHO)
     &       +          rk(130)*yh(lCCHO)
     &       +          rk(133)*yh(lRCHO)
     &       +          rk(136)*yh(lACET)
     &       +          rk(138)*yh(lMEK)
     &       +          rk(140)*yh(lMEOH)
     &       + ( 0.650)*rk(141)*yh(lCOOH)
     &       + ( 0.340)*rk(143)*yh(lROOH)
     &       +          rk(147)*yh(lGLY)
     &       +          rk(150)*yh(lMGLY)
     &       +          rk(153)*yh(lPHEN)
     &       +          rk(155)*yh(lCRES)
     &       +          rk(158)*yh(lBALD)
     &       +          rk(161)*yh(lMETH)
     &       +          rk(166)*yh(lMVK)
     &       +          rk(170)*yh(lISPD)
     &       +          rk(174)*yh(lPROD)
     &       +          rk(176)*yh(lRNO3)
     &       +          rk(178)*yh(lDCB1)
     &       +          rk(180)*yh(lDCB2)
     &       +          rk(182)*yh(lDCB3)
     &       +          rk(184)*CH4
     &       +          rk(185)*yh(lETHE)
     &       +          rk(189)*yh(lISOP)
     &       +          rk(193)*yh(lTERP)
     &       +          rk(197)*yh(lALK1)
     &       + ( 0.754)*rk(198)*yh(lALK2)
     &       +          rk(199)*yh(lALK3)
     &       +          rk(200)*yh(lALK4)
     &       +          rk(201)*yh(lALK5)
     &       +          rk(202)*yh(lARO1)
     &       +          rk(203)*yh(lARO2)
     &       +          rk(204)*yh(lOLE1)
     &       +          rk(208)*yh(lOLE2)
     &       +          rk(212)*yh(lETOH)
     &       +          rk(213)*yh(lMTBE)
     &       +          rk(214)*yh(lMBUT) )
c
c --- Loss of HO2, excluding self-reaction
c     (net either HO2 or OH produced)
c
      lsHO2 = 1.0 + rHO2_OH + rHO2_PNA + dt*(
     &       + ( 0.200)*rk( 39)*yh(lNO3)
     &       +          rk( 43)*yh(lOH)
     &       +          rk( 47)*yh(lCXO2)
     &       +          rk( 52)*yh(lRO2R)
     &       +          rk( 63)*yh(lRO2N)
     &       +          rk( 72)*yh(lCCO3)
     &       +          rk( 82)*yh(lRCO3)
     &       +          rk( 93)*yh(lBZCO)
     &       +          rk(105)*yh(lMCO3)
     &       +          rk(118)*yh(lBZO)
     &       +          rk(121)*yh(lBZNO)
     &       +          rk(126)*yh(lHCHO) )
c
c --- HO2 self-reaction
c
      self = dt*2.0*( 
     &       +          rk( 37)
     &       +          rk( 38)*H2O )
c
c --- Loss of HONO
c
      lsHONO = 1.0 + dt*(
     &       +          rk( 22)
     &       +          rk( 23)
     &       +          rk( 24)*yh(lOH) )
c
c --- Loss of PNA
c
      lsPNA = 1.0 + dt*(
     &       +          rk( 33)
     &       +          rk( 34)
     &       +          rk( 35)*yh(lOH) )
c
c --- Collect common terms
c
      t1 = 1.0 / ( lsOH*lsHONO - rHONO_OH*rOH_HONO )
      t2 = rOH_HO2*t1
      t3 = rPNA_HO2 / lsPNA
c
c --- Solve for HO2
c
      A = self
      B = lsHO2 - t3*rHO2_PNA - t2*rHO2_OH*lsHONO
      C = newHO2 + t3 * newPNA + t2*( newOH*lsHONO + newHONO*rHONO_OH )
      Q = -0.5 * (B + SIGN(1.0,B)*SQRT(B*B + 4.0*A*C))
c
c --- Update Concentrations
c
      y1(lHO2)  = MAX(1.0E-25, MAX(Q/A ,-C/Q) )
      y1(lOH)   = MAX(1.0E-25, ( ( newOH + rHO2_OH*y1(lHO2) )*lsHONO + 
     &                                        rHONO_OH*newHONO ) * t1 )
      y1(lHNO4)  = MAX(1.0E-25, ( newPNA + rHO2_PNA*y1(lHO2) ) / lsPNA )
      y1(lHONO) = MAX(1.0E-25, ( newHONO + rOH_HONO*y1(lOH) ) / lsHONO )
c
      return
      end

