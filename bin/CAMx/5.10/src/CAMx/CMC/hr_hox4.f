      subroutine hr_hox4(y0,y1,yh,H2O,M,O2,CH4,H2,ny,rk,r,nr,dt)
      implicit none
c
c-----CAMx v5.10 090918
c
c     HR_HOX4 solves the HOx family using Hertel's equations
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
     &       + rk( 10)
     &       + rk( 11)*H2O
     &       + rk( 97)*H2
c
c --- Yield of OH from O1D
c
      yldOH  = ( 
     &         ( 2.000)*rk( 11)*H2O
     &       +          rk( 97)*H2 ) / lsO1D
c
c --- Conversion of HONO to OH
c
      rHONO_OH  = dt*(
     &       +          rk( 23) )
c
c --- Conversion of HO2 to OH
c
      rHO2_OH  = dt*(
     &       +          rk( 13)*yh(lO3)
     &       +          rk( 28)*yh(lNO)
     &       + ( 0.790)*rk( 50)*yh(lC2O3)
     &       +          rk(102)*yh(lO) )
c
c --- Other OH production terms
c
      newOH = y0(lOH) + dt*(
     &       + ( 2.000)*r( 34)
     &       +          r( 40)
     &       +          r( 42)
     &       + ( 0.200)*r( 56)
     &       + ( 0.100)*r( 58)
     &       + ( 0.300)*r( 60)
     &       + ( 0.080)*r( 71)
     &       + ( 0.266)*r( 77)
     &       + ( 0.268)*r( 93)
     &       +          r(103)
     &       +          r(110)
     &       + ( 0.390)*r(113)
     &       +          r(  9)*yldOH )
c
c --- Conversion of PNA to HO2
c
      rPNA_HO2  = dt*(
     &       +          rk( 30)
     &       + ( 0.610)*rk(113) )
c
c --- Conversion of OH to HO2
c
      rOH_HO2  = dt*(
     &       +          rk( 12)*yh(lO3)
     &       +          rk( 35)*yh(lH2O2)
     &       +          rk( 36)*yh(lCO)
     &       +          rk( 37)*yh(lFORM)
     &       +          rk( 51)
     &       + ( 0.110)*rk( 52)*yh(lPAR)
     &       +          rk( 57)*yh(lOLE)
     &       +          rk( 61)*yh(lETH)
     &       + ( 0.440)*rk( 63)*yh(lTOL)
     &       + ( 0.600)*rk( 66)*yh(lCRES)
     &       + ( 2.000)*rk( 70)*yh(lOPEN)
     &       + ( 0.700)*rk( 72)*yh(lXYL)
     &       + ( 0.912)*rk( 76)*yh(lISOP)
     &       +          rk( 82)*yh(lSO2)
     &       +          rk( 84)*yh(lMEOH)
     &       +          rk( 85)*yh(lETOH)
     &       + ( 0.503)*rk( 92)*yh(lISPD)
     &       +          rk( 98)*H2
     &       +          rk( 99)*yh(lO)
     &       +          rk(105)*yh(lNO3) )
c
c --- Other HO2 production terms
c
      newHO2 = y0(lHO2) + dt*(
     &       + ( 2.000)*r( 38)
     &       +          r( 40)
     &       +          r( 41)
     &       + ( 2.000)*r( 45)
     &       +          r( 46)
     &       + ( 2.000)*r( 49)
     &       + ( 0.940)*r( 53)
     &       +          r( 54)
     &       + ( 0.380)*r( 56)
     &       + ( 0.440)*r( 58)
     &       + ( 1.700)*r( 60)
     &       + ( 0.120)*r( 62)
     &       + ( 0.900)*r( 64)
     &       +          r( 65)
     &       +          r( 69)
     &       + ( 0.760)*r( 71)
     &       +          r( 74)
     &       + ( 0.250)*r( 75)
     &       + ( 0.066)*r( 77)
     &       + ( 0.800)*r( 78)
     &       + ( 0.154)*r( 93)
     &       + ( 0.925)*r( 94)
     &       + ( 1.033)*r( 95)
     &       + ( 0.800)*r( 96)
     &       +          r( 97)
     &       +          r(103) )
c
c
c --- Conversion of OH to HONO
c
      rOH_HONO  = dt*(
     &       +          rk( 22)*yh(lNO) )
c
c --- Other HONO production terms
c
      newHONO = y0(lHONO) + dt*(
     &       + ( 2.000)*r( 21) ) 
c
c --- Conversion of HO2 to PNA
c
      rHO2_PNA  = dt*(
     &       +          rk( 29)*yh(lNO2) )
c
c --- Other PNA production terms
c
      newPNA = y0(lPNA) 
c
c --- Net loss of OH
c
      lsOH = 1.0 + dt*(
     &       +          rk( 12)*yh(lO3)
     &       +          rk( 22)*yh(lNO)
     &       +          rk( 24)*yh(lHONO)
     &       +          rk( 26)*yh(lNO2)
     &       +          rk( 27)*yh(lHNO3)
     &       +          rk( 31)*yh(lPNA)
     &       +          rk( 35)*yh(lH2O2)
     &       +          rk( 36)*yh(lCO)
     &       +          rk( 37)*yh(lFORM)
     &       +          rk( 43)*yh(lALD2)
     &       +          rk( 51)
     &       +          rk( 52)*yh(lPAR)
     &       +          rk( 57)*yh(lOLE)
     &       +          rk( 61)*yh(lETH)
     &       +          rk( 63)*yh(lTOL)
     &       +          rk( 66)*yh(lCRES)
     &       +          rk( 70)*yh(lOPEN)
     &       +          rk( 72)*yh(lXYL)
     &       +          rk( 73)*yh(lMGLY)
     &       +          rk( 76)*yh(lISOP)
     &       +          rk( 82)*yh(lSO2)
     &       +          rk( 84)*yh(lMEOH)
     &       +          rk( 85)*yh(lETOH)
     &       +          rk( 90)*yh(lHO2)
     &       +          rk( 92)*yh(lISPD)
     &       +          rk( 98)*H2
     &       +          rk( 99)*yh(lO)
     &       +          rk(100)*yh(lOH)
     &       +          rk(101)*yh(lOH)
     &       +          rk(105)*yh(lNO3) )
c
c --- Loss of HO2, excluding self-reaction
c     (net either HO2 or OH produced)
c
      lsHO2 = 1.0 + rHO2_OH + rHO2_PNA + dt*(
     &       +          rk( 86)*yh(lXO2)
     &       +          rk( 87)*yh(lXO2N)
     &       +          rk( 90)*yh(lOH)
     &       +          rk(106)*yh(lNO3) )
c
c --- HO2 self-reaction
c
      self = dt*2.0*( 
     &       +          rk( 32)
     &       +          rk( 33)*H2O )
c
c --- Loss of HONO
c
      lsHONO = 1.0 + dt*(
     &       +          rk( 23)
     &       +          rk( 24)*yh(lOH)
     &       + ( 2.000)*rk( 25)*yh(lHONO) )
c
c --- Loss of PNA
c
      lsPNA = 1.0 + dt*(
     &       +          rk( 30)
     &       +          rk( 31)*yh(lOH)
     &       +          rk(113) )
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

