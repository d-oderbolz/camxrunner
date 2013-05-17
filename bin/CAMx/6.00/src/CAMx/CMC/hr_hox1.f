      subroutine hr_hox1(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c----CAMx v6.00 130506
c
c     HR_HOX1 solves the HOx family using Hertel's equations
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
c
c --- Yield of OH from O1D
c
      yldOH  = ( 
     &         ( 2.000)*rk( 11)*H2O ) / lsO1D
c
c --- Conversion of HONO to OH
c
      rHONO_OH  = dt*(
     &       +          rk( 43) )
c
c --- Conversion of HO2 to OH
c
      rHO2_OH  = dt*(
     &       +          rk( 13)*yh(lO3)
     &       +          rk( 15)*yh(lO)
     &       +          rk( 25)*yh(lNO)
     &       +          rk( 33)*yh(lNO3)
     &       + ( 0.440)*rk( 57)*yh(lC2O3)
     &       + ( 0.440)*rk( 65)*yh(lCXO3)
     &       + ( 0.200)*rk(104)*yh(lHCO3)
     &       + ( 0.120)*rk(151)*yh(lISO2)
     &       + ( 1.125)*rk(163)*yh(lEPX2)
     &       + ( 0.440)*rk(216)*yh(lOPO3) )
c
c --- Other OH production terms
c
      newOH = y0(lOH) + dt*(
     &       + ( 2.000)*r( 21)
     &       +          r( 23)
     &       +          r( 47)
     &       + ( 0.410)*r( 50)
     &       +          r( 88)
     &       +          r( 90)
     &       +          r( 99)
     &       +          r(105)
     &       +          r(109)
     &       + ( 0.190)*r(114)
     &       + ( 0.300)*r(137)
     &       + ( 0.160)*r(139)
     &       + ( 0.100)*r(141)
     &       + ( 0.334)*r(143)
     &       + ( 0.500)*r(147)
     &       + ( 0.040)*r(154)
     &       + ( 0.266)*r(155)
     &       + ( 0.268)*r(158)
     &       + ( 0.125)*r(164)
     &       + ( 0.100)*r(165)
     &       + ( 0.125)*r(166)
     &       + ( 0.570)*r(170)
     &       +          r(197)
     &       + ( 0.500)*r(201)
     &       + ( 0.500)*r(205)
     &       +          r(232)
     &       +          r(  9)*yldOH )
c
c --- Conversion of PNA to HO2
c
      rPNA_HO2  = dt*(
     &       +          rk( 49)
     &       + ( 0.590)*rk( 50) )
c
c --- Conversion of OH to HO2
c
      rOH_HO2  = dt*(
     &       +          rk( 12)*yh(lO3)
     &       +          rk( 14)*yh(lO)
     &       +          rk( 22)*yh(lH2O2)
     &       +          rk( 32)*yh(lNO3)
     &       +          rk( 52)*yh(lSO2)
     &       +          rk( 93)*yh(lFACD)
     &       +          rk( 96)*yh(lFORM)
     &       + ( 0.200)*rk(113)*yh(lGLYD)
     &       +          rk(116)*yh(lGLY)
     &       +          rk(122)*H2
     &       +          rk(123)*yh(lCO)
     &       +          rk(126)*yh(lMEOH)
     &       + ( 0.900)*rk(127)*yh(lETOH)
     &       + ( 0.300)*rk(136)*yh(lETHY)
     &       + ( 0.530)*rk(172)*yh(lBENZ)
     &       + ( 0.180)*rk(177)*yh(lTOL)
     &       + ( 0.155)*rk(182)*yh(lXYL)
     &       +          rk(187)*yh(lCRES) )
c
c --- Other HO2 production terms
c
      newHO2 = y0(lHO2) + dt*(
     &       +          r( 23)
     &       +          r( 71)
     &       + ( 0.900)*r( 73)
     &       + ( 0.370)*r( 74)
     &       +          r( 75)
     &       + ( 0.800)*r( 77)
     &       + ( 0.600)*r( 78)
     &       + ( 0.800)*r( 85)
     &       +          r( 90)
     &       + ( 2.000)*r( 97)
     &       +          r( 99)
     &       +          r(100)
     &       +          r(102)
     &       +          r(103)
     &       +          r(108)
     &       +          r(112)
     &       + ( 1.400)*r(114)
     &       + ( 2.000)*r(117)
     &       +          r(118)
     &       +          r(119)
     &       +          r(134)
     &       +          r(137)
     &       + ( 0.160)*r(139)
     &       + ( 0.100)*r(141)
     &       + ( 0.080)*r(143)
     &       + ( 0.803)*r(150)
     &       + ( 0.709)*r(152)
     &       + ( 0.803)*r(153)
     &       + ( 0.800)*r(154)
     &       + ( 0.066)*r(155)
     &       + ( 0.090)*r(158)
     &       + ( 0.850)*r(159)
     &       + ( 0.333)*r(160)
     &       + ( 0.825)*r(164)
     &       + ( 0.660)*r(165)
     &       + ( 0.825)*r(166)
     &       + ( 0.918)*r(173)
     &       +          r(174)
     &       +          r(176)
     &       + ( 0.860)*r(178)
     &       +          r(179)
     &       +          r(181)
     &       + ( 0.860)*r(183)
     &       +          r(185)
     &       +          r(186)
     &       + ( 0.700)*r(199)
     &       +          r(203)
     &       + ( 0.560)*r(205)
     &       + ( 1.200)*r(209)
     &       +          r(211)
     &       +          r(212) )
c
c
c --- Conversion of OH to HONO
c
      rOH_HONO  = dt*(
     &       +          rk( 40)*yh(lNO) )
c
c --- Other HONO production terms
c
      newHONO = y0(lHONO) + dt*(
     &       + ( 2.000)*r( 41) ) 
c
c --- Conversion of HO2 to PNA
c
      rHO2_PNA  = dt*(
     &       +          rk( 48)*yh(lNO2) )
c
c --- Other PNA production terms
c
      newPNA = y0(lPNA) 
c
c --- Net loss of OH
c
      lsOH = 1.0 + dt*(
     &       +          rk( 12)*yh(lO3)
     &       +          rk( 14)*yh(lO)
     &       +          rk( 16)*yh(lOH)
     &       +          rk( 17)*yh(lOH)
     &       +          rk( 18)*yh(lHO2)
     &       +          rk( 22)*yh(lH2O2)
     &       +          rk( 32)*yh(lNO3)
     &       +          rk( 40)*yh(lNO)
     &       +          rk( 44)*yh(lHONO)
     &       +          rk( 45)*yh(lNO2)
     &       +          rk( 46)*yh(lHNO3)
     &       +          rk( 51)*yh(lPNA)
     &       +          rk( 52)*yh(lSO2)
     &       + ( 0.600)*rk( 87)*yh(lMEPX)
     &       + ( 0.600)*rk( 89)*yh(lROOH)
     &       +          rk( 91)*yh(lNTR)
     &       +          rk( 93)*yh(lFACD)
     &       +          rk( 94)*yh(lAACD)
     &       +          rk( 95)*yh(lPACD)
     &       +          rk( 96)*yh(lFORM)
     &       +          rk(106)*yh(lALD2)
     &       +          rk(110)*yh(lALDX)
     &       +          rk(113)*yh(lGLYD)
     &       +          rk(116)*yh(lGLY)
     &       +          rk(121)*yh(lMGLY)
     &       +          rk(122)*H2
     &       +          rk(123)*yh(lCO)
     &       +          rk(124)*CH4
     &       +          rk(125)*yh(lETHA)
     &       +          rk(126)*yh(lMEOH)
     &       +          rk(127)*yh(lETOH)
     &       +          rk(130)*yh(lACET)
     &       +          rk(131)*yh(lPRPA)
     &       +          rk(132)*yh(lPAR)
     &       + ( 0.300)*rk(136)*yh(lETHY)
     &       +          rk(138)*yh(lETH)
     &       +          rk(142)*yh(lOLE)
     &       +          rk(146)*yh(lIOLE)
     &       +          rk(149)*yh(lISOP)
     &       +          rk(157)*yh(lISPD)
     &       + ( 0.067)*rk(161)*yh(lISPX)
     &       +          rk(162)*yh(lEPOX)
     &       +          rk(167)*yh(lINTR)
     &       +          rk(169)*yh(lTERP)
     &       + ( 0.882)*rk(172)*yh(lBENZ)
     &       + ( 0.900)*rk(177)*yh(lTOL)
     &       + ( 0.756)*rk(182)*yh(lXYL)
     &       +          rk(187)*yh(lCRES)
     &       +          rk(191)*yh(lCRON)
     &       +          rk(198)*yh(lCRPX)
     &       +          rk(200)*yh(lXOPN)
     &       +          rk(204)*yh(lOPEN)
     &       +          rk(207)*yh(lCAT1)
     &       +          rk(224)*yh(lI2)
     &       +          rk(226)*yh(lHI)
     &       +          rk(233)*yh(lHOI)
     &       +          rk(236)*yh(lOIO) )
c
c --- Loss of HO2, excluding self-reaction
c     (net either HO2 or OH produced)
c
      lsHO2 = 1.0 + rHO2_OH + rHO2_PNA + dt*(
     &       +          rk( 18)*yh(lOH)
     &       + ( 0.560)*rk( 57)*yh(lC2O3)
     &       + ( 0.560)*rk( 65)*yh(lCXO3)
     &       +          rk( 72)*yh(lMEO2)
     &       +          rk( 76)*yh(lXO2H)
     &       +          rk( 80)*yh(lXO2)
     &       +          rk( 84)*yh(lXO2N)
     &       +          rk(101)*yh(lFORM)
     &       + ( 0.600)*rk(104)*yh(lHCO3)
     &       + ( 0.760)*rk(151)*yh(lISO2)
     &       +          rk(175)*yh(lBZO2)
     &       +          rk(180)*yh(lTO2)
     &       +          rk(184)*yh(lXLO2)
     &       +          rk(190)*yh(lCRO)
     &       +          rk(196)*yh(lCRN2)
     &       +          rk(210)*yh(lCAO2)
     &       + ( 0.560)*rk(216)*yh(lOPO3)
     &       +          rk(220)*yh(lI)
     &       +          rk(230)*yh(lIO) )
c
c --- HO2 self-reaction
c
      self = dt*2.0*( 
     &       +          rk( 19)
     &       +          rk( 20)*H2O )
c
c --- Loss of HONO
c
      lsHONO = 1.0 + dt*(
     &       + ( 2.000)*rk( 42)*yh(lHONO)
     &       +          rk( 43)
     &       +          rk( 44)*yh(lOH) )
c
c --- Loss of PNA
c
      lsPNA = 1.0 + dt*(
     &       +          rk( 49)
     &       +          rk( 50)
     &       +          rk( 51)*yh(lOH) )
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

