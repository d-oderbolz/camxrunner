      subroutine ebirxn4(ny,nr,yh,H2O,M,O2,CH4,H2,rk,r)
      implicit none
c
c-----CAMx v5.10 090918
c
c     EBIRXN4 computes reaction rates for the EBI solver
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
c        ny  - dimension of y
c        nr  - dimension of r and rk
c        yh  - current y (ppm)
c        H2O - water vapor Concentration (ppm)
c        M   - total gas Concentration (ppm)
c        O2  - oxygen Concentration (ppm)
c        CH4 - methane Concentration (ppm)
c        H2  - hydrogen Concentration (ppm)
c        rk  - rate constants (units ppm hr)
c        r   - reaction rates (hr-1)
c
c --- Includes:
      include "camx.prm"
      include "chmdat.com"
      include "ddmchm.com"
c
c --- Arguments:
      integer ny, nr
      real    yh(ny+1)
      real    r(nr), rk(nr)
      real    H2O,M,O2,CH4,H2
c
c --- Local variables:
      real    N2
c
c --- Entry point
c
      N2  = M - O2
c
c --- Calculate reaction rates
c
      r(  1) = rk(  1)*yh(lNO2)
      r(  2) = rk(  2)*yh(lO)
      r(  3) = rk(  3)*yh(lO3)*yh(lNO)
      r(  4) = rk(  4)*yh(lO)*yh(lNO2)
      r(  5) = rk(  5)*yh(lO)*yh(lNO2)
      r(  6) = rk(  6)*yh(lO)*yh(lNO)
      r(  7) = rk(  7)*yh(lNO2)*yh(lO3)
      r(  8) = rk(  8)*yh(lO3)
      r(  9) = rk(  9)*yh(lO3)
      r( 10) = rk( 10)*yh(lO1D)
      r( 11) = rk( 11)*yh(lO1D)*H2O
      r( 12) = rk( 12)*yh(lO3)*yh(lOH)
      r( 13) = rk( 13)*yh(lO3)*yh(lHO2)
      r( 14) = rk( 14)*yh(lNO3)
      r( 15) = rk( 15)*yh(lNO3)*yh(lNO)
      r( 16) = rk( 16)*yh(lNO3)*yh(lNO2)
      r( 17) = rk( 17)*yh(lNO3)*yh(lNO2)
      r( 18) = rk( 18)*yh(lN2O5)*H2O
      r( 19) = rk( 19)*yh(lN2O5)
      r( 20) = rk( 20)*yh(lNO)*yh(lNO)
      r( 21) = rk( 21)*yh(lNO)*yh(lNO2)*H2O
      r( 22) = rk( 22)*yh(lNO)*yh(lOH)
      r( 23) = rk( 23)*yh(lHONO)
      r( 24) = rk( 24)*yh(lOH)*yh(lHONO)
      r( 25) = rk( 25)*yh(lHONO)*yh(lHONO)
      r( 26) = rk( 26)*yh(lNO2)*yh(lOH)
      r( 27) = rk( 27)*yh(lOH)*yh(lHNO3)
      r( 28) = rk( 28)*yh(lHO2)*yh(lNO)
      r( 29) = rk( 29)*yh(lHO2)*yh(lNO2)
      r( 30) = rk( 30)*yh(lPNA)
      r( 31) = rk( 31)*yh(lOH)*yh(lPNA)
      r( 32) = rk( 32)*yh(lHO2)*yh(lHO2)
      r( 33) = rk( 33)*yh(lHO2)*yh(lHO2)*H2O
      r( 34) = rk( 34)*yh(lH2O2)
      r( 35) = rk( 35)*yh(lOH)*yh(lH2O2)
      r( 36) = rk( 36)*yh(lOH)*yh(lCO)
      r( 37) = rk( 37)*yh(lFORM)*yh(lOH)
      r( 38) = rk( 38)*yh(lFORM)
      r( 39) = rk( 39)*yh(lFORM)
      r( 40) = rk( 40)*yh(lFORM)*yh(lO)
      r( 41) = rk( 41)*yh(lFORM)*yh(lNO3)
      r( 42) = rk( 42)*yh(lALD2)*yh(lO)
      r( 43) = rk( 43)*yh(lALD2)*yh(lOH)
      r( 44) = rk( 44)*yh(lALD2)*yh(lNO3)
      r( 45) = rk( 45)*yh(lALD2)
      r( 46) = rk( 46)*yh(lC2O3)*yh(lNO)
      r( 47) = rk( 47)*yh(lC2O3)*yh(lNO2)
      r( 48) = rk( 48)*yh(lPAN)
      r( 49) = rk( 49)*yh(lC2O3)*yh(lC2O3)
      r( 50) = rk( 50)*yh(lC2O3)*yh(lHO2)
      r( 51) = rk( 51)*yh(lOH)
      r( 52) = rk( 52)*yh(lPAR)*yh(lOH)
      r( 53) = rk( 53)*yh(lROR)
      r( 54) = rk( 54)*yh(lROR)
      r( 55) = rk( 55)*yh(lROR)*yh(lNO2)
      r( 56) = rk( 56)*yh(lO)*yh(lOLE)
      r( 57) = rk( 57)*yh(lOH)*yh(lOLE)
      r( 58) = rk( 58)*yh(lO3)*yh(lOLE)
      r( 59) = rk( 59)*yh(lNO3)*yh(lOLE)
      r( 60) = rk( 60)*yh(lO)*yh(lETH)
      r( 61) = rk( 61)*yh(lOH)*yh(lETH)
      r( 62) = rk( 62)*yh(lO3)*yh(lETH)
      r( 63) = rk( 63)*yh(lTOL)*yh(lOH)
      r( 64) = rk( 64)*yh(lTO2)*yh(lNO)
      r( 65) = rk( 65)*yh(lTO2)
      r( 66) = rk( 66)*yh(lOH)*yh(lCRES)
      r( 67) = rk( 67)*yh(lCRES)*yh(lNO3)
      r( 68) = rk( 68)*yh(lCRO)*yh(lNO2)
      r( 69) = rk( 69)*yh(lOPEN)
      r( 70) = rk( 70)*yh(lOPEN)*yh(lOH)
      r( 71) = rk( 71)*yh(lOPEN)*yh(lO3)
      r( 72) = rk( 72)*yh(lOH)*yh(lXYL)
      r( 73) = rk( 73)*yh(lOH)*yh(lMGLY)
      r( 74) = rk( 74)*yh(lMGLY)
      r( 75) = rk( 75)*yh(lO)*yh(lISOP)
      r( 76) = rk( 76)*yh(lOH)*yh(lISOP)
      r( 77) = rk( 77)*yh(lO3)*yh(lISOP)
      r( 78) = rk( 78)*yh(lNO3)*yh(lISOP)
      r( 79) = rk( 79)*yh(lXO2)*yh(lNO)
      r( 80) = rk( 80)*yh(lXO2)*yh(lXO2)
      r( 81) = rk( 81)*yh(lXO2N)*yh(lNO)
      r( 82) = rk( 82)*yh(lSO2)*yh(lOH)
      r( 83) = rk( 83)*yh(lSO2)
      r( 84) = rk( 84)*yh(lMEOH)*yh(lOH)
      r( 85) = rk( 85)*yh(lETOH)*yh(lOH)
      r( 86) = rk( 86)*yh(lXO2)*yh(lHO2)
      r( 87) = rk( 87)*yh(lXO2N)*yh(lHO2)
      r( 88) = rk( 88)*yh(lXO2N)*yh(lXO2N)
      r( 89) = rk( 89)*yh(lXO2)*yh(lXO2N)
      r( 90) = rk( 90)*yh(lOH)*yh(lHO2)
      r( 91) = rk( 91)*yh(lCRO)
      r( 92) = rk( 92)*yh(lOH)*yh(lISPD)
      r( 93) = rk( 93)*yh(lO3)*yh(lISPD)
      r( 94) = rk( 94)*yh(lNO3)*yh(lISPD)
      r( 95) = rk( 95)*yh(lISPD)
      r( 96) = rk( 96)*yh(lNO2)*yh(lISOP)
      r( 97) = rk( 97)*yh(lO1D)*H2
      r( 98) = rk( 98)*yh(lOH)*H2
      r( 99) = rk( 99)*yh(lOH)*yh(lO)
      r(100) = rk(100)*yh(lOH)*yh(lOH)
      r(101) = rk(101)*yh(lOH)*yh(lOH)
      r(102) = rk(102)*yh(lHO2)*yh(lO)
      r(103) = rk(103)*yh(lH2O2)*yh(lO)
      r(104) = rk(104)*yh(lNO3)*yh(lO)
      r(105) = rk(105)*yh(lNO3)*yh(lOH)
      r(106) = rk(106)*yh(lNO3)*yh(lHO2)
      r(107) = rk(107)*yh(lNO3)*yh(lO3)
      r(108) = rk(108)*yh(lNO3)*yh(lNO3)
      r(109) = rk(109)*yh(lPAN)
      r(110) = rk(110)*yh(lHNO3)
      r(111) = rk(111)*yh(lN2O5)
      r(112) = rk(112)*yh(lNTR)
      r(113) = rk(113)*yh(lPNA)
c
      return
      end

