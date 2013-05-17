      subroutine hr_hox9(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c----CAMx v6.00 130506
c
c     HR_HOX9 solves the HOx family using Hertel's equations
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
     &       + rk( 10)*M
     &       + rk( 11)*H2O
     &       + rk( 38)*H2
c
c --- Yield of OH from O1D
c
      yldOH  = ( 
     &         ( 2.000)*rk( 11)*H2O
     &       +          rk( 38)*H2 ) / lsO1D
c
c --- Conversion of HONO to OH
c
      rHONO_OH  = dt*(
     &       +          rk( 25) )
c
c --- Conversion of HO2 to OH
c
      rHO2_OH  = dt*(
     &       +          rk( 13)*yh(lO3)
     &       +          rk( 30)*yh(lNO)
     &       +          rk( 44)*yh(lO) )
c
c --- Other OH production terms
c
      newOH = y0(lOH) + dt*(
     &       + ( 2.000)*r( 36)
     &       +          r( 45)
     &       + ( 0.390)*r( 51)
     &       +          r( 52)
     &       +          r( 65)
     &       +          r( 72)
     &       +          r( 77)
     &       +          r( 84)
     &       +          r( 97)
     &       +          r( 99)
     &       + ( 0.100)*r(119)
     &       + ( 0.100)*r(121)
     &       + ( 0.300)*r(123)
     &       + ( 0.130)*r(125)
     &       + ( 0.500)*r(129)
     &       + ( 0.080)*r(140)
     &       + ( 0.266)*r(146)
     &       + ( 0.268)*r(150)
     &       + ( 0.570)*r(155)
     &       +          r(158)
     &       +          r(215)
     &       +          r(  9)*yldOH )
c
c --- Conversion of PNA to HO2
c
      rPNA_HO2  = dt*(
     &       +          rk( 32)
     &       + ( 0.610)*rk( 51) )
c
c --- Conversion of OH to HO2
c
      rOH_HO2  = dt*(
     &       +          rk( 12)*yh(lO3)
     &       +          rk( 37)*yh(lH2O2)
     &       +          rk( 39)*H2
     &       +          rk( 40)*yh(lO)
     &       +          rk( 47)*yh(lNO3)
     &       +          rk( 61)*yh(lNTR)
     &       +          rk( 63)*yh(lSO2)
     &       +          rk( 66)*yh(lCO)
     &       + ( 0.300)*rk( 71)*yh(lMEPX)
     &       +          rk( 73)*yh(lMEOH)
     &       +          rk( 74)*yh(lFORM)
     &       +          rk( 83)*yh(lFACD)
     &       +          rk(113)*yh(lETHA)
     &       +          rk(114)*yh(lETOH)
     &       + ( 0.110)*rk(115)*yh(lPAR)
     &       + ( 0.950)*rk(120)*yh(lOLE)
     &       +          rk(124)*yh(lETH)
     &       +          rk(128)*yh(lIOLE)
     &       + ( 0.440)*rk(131)*yh(lTOL)
     &       + ( 0.600)*rk(134)*yh(lCRES)
     &       + ( 2.000)*rk(139)*yh(lOPEN)
     &       + ( 0.700)*rk(141)*yh(lXYL)
     &       + ( 0.912)*rk(145)*yh(lISOP)
     &       + ( 0.503)*rk(149)*yh(lISPD)
     &       + ( 0.750)*rk(154)*yh(lTERP)
     &       + ( 0.820)*rk(220)*yh(lNBUI)
     &       +          rk(221)*yh(lIBUO) )
c
c --- Other HO2 production terms
c
      newHO2 = y0(lHO2) + dt*(
     &       +          r( 38)
     &       +          r( 45)
     &       +          r( 62)
     &       +          r( 65)
     &       +          r( 68)
     &       + ( 0.740)*r( 70)
     &       +          r( 72)
     &       + ( 2.000)*r( 75)
     &       +          r( 77)
     &       +          r( 78)
     &       +          r( 80)
     &       +          r( 81)
     &       +          r( 87)
     &       + ( 0.900)*r( 93)
     &       +          r(102)
     &       +          r(103)
     &       +          r(109)
     &       + ( 2.000)*r(111)
     &       +          r(112)
     &       + ( 0.940)*r(116)
     &       +          r(117)
     &       + ( 0.300)*r(119)
     &       + ( 0.440)*r(121)
     &       + ( 1.700)*r(123)
     &       + ( 0.130)*r(125)
     &       + ( 0.100)*r(127)
     &       + ( 0.500)*r(129)
     &       +          r(130)
     &       + ( 0.900)*r(132)
     &       +          r(133)
     &       +          r(138)
     &       + ( 0.760)*r(140)
     &       +          r(143)
     &       + ( 0.250)*r(144)
     &       + ( 0.066)*r(146)
     &       + ( 0.800)*r(147)
     &       + ( 0.800)*r(148)
     &       + ( 0.154)*r(150)
     &       + ( 0.925)*r(151)
     &       + ( 1.033)*r(152)
     &       + ( 0.070)*r(155)
     &       + ( 0.280)*r(156)
     &       +          r(170)
     &       +          r(171)
     &       + ( 0.110)*r(173)
     &       +          r(174)
     &       +          r(175)
     &       +          r(176)
     &       +          r(177)
     &       + ( 0.920)*r(178)
     &       + ( 0.750)*r(179)
     &       + ( 0.880)*r(180)
     &       + ( 0.840)*r(181)
     &       +          r(182)
     &       +          r(185)
     &       +          r(186)
     &       +          r(218) )
c
c
c --- Conversion of OH to HONO
c
      rOH_HONO  = dt*(
     &       +          rk( 24)*yh(lNO) )
c
c --- Other HONO production terms
c
      newHONO = y0(lHONO) + dt*(
     &       + ( 2.000)*r( 23) ) 
c
c --- Conversion of HO2 to PNA
c
      rHO2_PNA  = dt*(
     &       +          rk( 31)*yh(lNO2) )
c
c --- Other PNA production terms
c
      newPNA = y0(lPNA) 
c
c --- Net loss of OH
c
      lsOH = 1.0 + dt*(
     &       +          rk( 12)*yh(lO3)
     &       +          rk( 24)*yh(lNO)
     &       +          rk( 26)*yh(lHONO)
     &       +          rk( 28)*yh(lNO2)
     &       +          rk( 29)*yh(lHNO3)
     &       +          rk( 33)*yh(lPNA)
     &       +          rk( 37)*yh(lH2O2)
     &       +          rk( 39)*H2
     &       +          rk( 40)*yh(lO)
     &       +          rk( 41)*yh(lOH)
     &       +          rk( 42)*yh(lOH)
     &       +          rk( 43)*yh(lHO2)
     &       +          rk( 47)*yh(lNO3)
     &       +          rk( 61)*yh(lNTR)
     &       +          rk( 63)*yh(lSO2)
     &       +          rk( 64)*yh(lROOH)
     &       +          rk( 66)*yh(lCO)
     &       +          rk( 67)*CH4
     &       +          rk( 71)*yh(lMEPX)
     &       +          rk( 73)*yh(lMEOH)
     &       +          rk( 74)*yh(lFORM)
     &       +          rk( 83)*yh(lFACD)
     &       +          rk( 85)*yh(lALD2)
     &       +          rk( 96)*yh(lPACD)
     &       +          rk( 98)*yh(lAACD)
     &       +          rk(100)*yh(lALDX)
     &       +          rk(107)*yh(lPANX)
     &       +          rk(113)*yh(lETHA)
     &       +          rk(114)*yh(lETOH)
     &       +          rk(115)*yh(lPAR)
     &       +          rk(120)*yh(lOLE)
     &       +          rk(124)*yh(lETH)
     &       +          rk(128)*yh(lIOLE)
     &       +          rk(131)*yh(lTOL)
     &       +          rk(134)*yh(lCRES)
     &       +          rk(139)*yh(lOPEN)
     &       +          rk(141)*yh(lXYL)
     &       +          rk(142)*yh(lMGLY)
     &       +          rk(145)*yh(lISOP)
     &       +          rk(149)*yh(lISPD)
     &       +          rk(154)*yh(lTERP)
     &       +          rk(168)*yh(lHCL)
     &       +          rk(169)*yh(lFMCL)
     &       +          rk(205)*yh(lI2)
     &       +          rk(209)*yh(lOIO)
     &       +          rk(216)*yh(lHOI)
     &       +          rk(217)*yh(lHI)
     &       +          rk(220)*yh(lNBUI)
     &       +          rk(221)*yh(lIBUO) )
c
c --- Loss of HO2, excluding self-reaction
c     (net either HO2 or OH produced)
c
      lsHO2 = 1.0 + rHO2_OH + rHO2_PNA + dt*(
     &       +          rk( 43)*yh(lOH)
     &       +          rk( 48)*yh(lNO3)
     &       +          rk( 56)*yh(lXO2)
     &       +          rk( 57)*yh(lXO2N)
     &       +          rk( 69)*yh(lMEO2)
     &       +          rk( 79)*yh(lFORM)
     &       +          rk( 82)*yh(lHCO3)
     &       +          rk( 92)*yh(lC2O3)
     &       +          rk(108)*yh(lCXO3)
     &       +          rk(137)*yh(lCRO)
     &       +          rk(162)*yh(lCLO)
     &       +          rk(191)*yh(lI)
     &       +          rk(200)*yh(lIO) )
c
c --- HO2 self-reaction
c
      self = dt*2.0*( 
     &       +          rk( 34)
     &       +          rk( 35)*H2O )
c
c --- Loss of HONO
c
      lsHONO = 1.0 + dt*(
     &       +          rk( 25)
     &       +          rk( 26)*yh(lOH)
     &       + ( 2.000)*rk( 27)*yh(lHONO) )
c
c --- Loss of PNA
c
      lsPNA = 1.0 + dt*(
     &       +          rk( 32)
     &       +          rk( 33)*yh(lOH)
     &       +          rk( 51) )
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
      y1(lPNA)  = MAX(1.0E-25, ( newPNA + rHO2_PNA*y1(lHO2) ) / lsPNA )
      y1(lHONO) = MAX(1.0E-25, ( newHONO + rOH_HONO*y1(lOH) ) / lsHONO )
c
      return
      end

