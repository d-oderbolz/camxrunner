      subroutine lsrxn1(H2O,M,O2,CH4,H2,y,dbrk,r)
c
c----CAMx v4.51 080522
c
c     LSRXN1 computes double precision fluxes for each reaction
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c     Routines Called:
c        none
c
c     Called by:
c        LSRATE1
c        LSJAC1
c
      implicit none
      include "camx.prm"
      include "ddmchm.com"
c
      double precision y(mxradcl+mxspec+1), dbrk(mxrxn), r(mxrxn)
      double precision H2O, M, O2, CH4, H2
c
      r(  1) = dbrk(  1)*y(lNO2)
      r(  2) = dbrk(  2)*y(lO)
      r(  3) = dbrk(  3)*y(lO3)*y(lNO)
      r(  4) = dbrk(  4)*y(lO)*y(lNO2)
      r(  5) = dbrk(  5)*y(lO)*y(lNO2)
      r(  6) = dbrk(  6)*y(lO)*y(lNO)
      r(  7) = dbrk(  7)*y(lNO2)*y(lO3)
      r(  8) = dbrk(  8)*y(lO3)
      r(  9) = dbrk(  9)*y(lO3)
      r( 10) = dbrk( 10)*y(lO1D)
      r( 11) = dbrk( 11)*y(lO1D)*H2O
      r( 12) = dbrk( 12)*y(lO3)*y(lOH)
      r( 13) = dbrk( 13)*y(lO3)*y(lHO2)
      r( 14) = dbrk( 14)*y(lNO3)
      r( 15) = dbrk( 15)*y(lNO3)*y(lNO)
      r( 16) = dbrk( 16)*y(lNO3)*y(lNO2)
      r( 17) = dbrk( 17)*y(lNO3)*y(lNO2)
      r( 18) = dbrk( 18)*y(lN2O5)*H2O
      r( 19) = dbrk( 19)*y(lN2O5)
      r( 20) = dbrk( 20)*y(lNO)*y(lNO)
      r( 21) = dbrk( 21)*y(lNO)*y(lNO2)*H2O
      r( 22) = dbrk( 22)*y(lNO)*y(lOH)
      r( 23) = dbrk( 23)*y(lHONO)
      r( 24) = dbrk( 24)*y(lOH)*y(lHONO)
      r( 25) = dbrk( 25)*y(lHONO)*y(lHONO)
      r( 26) = dbrk( 26)*y(lNO2)*y(lOH)
      r( 27) = dbrk( 27)*y(lOH)*y(lHNO3)
      r( 28) = dbrk( 28)*y(lHO2)*y(lNO)
      r( 29) = dbrk( 29)*y(lHO2)*y(lNO2)
      r( 30) = dbrk( 30)*y(lPNA)
      r( 31) = dbrk( 31)*y(lOH)*y(lPNA)
      r( 32) = dbrk( 32)*y(lHO2)*y(lHO2)
      r( 33) = dbrk( 33)*y(lHO2)*y(lHO2)*H2O
      r( 34) = dbrk( 34)*y(lH2O2)
      r( 35) = dbrk( 35)*y(lOH)*y(lH2O2)
      r( 36) = dbrk( 36)*y(lOH)*y(lCO)
      r( 37) = dbrk( 37)*y(lFORM)*y(lOH)
      r( 38) = dbrk( 38)*y(lFORM)
      r( 39) = dbrk( 39)*y(lFORM)
      r( 40) = dbrk( 40)*y(lFORM)*y(lO)
      r( 41) = dbrk( 41)*y(lFORM)*y(lNO3)
      r( 42) = dbrk( 42)*y(lALD2)*y(lO)
      r( 43) = dbrk( 43)*y(lALD2)*y(lOH)
      r( 44) = dbrk( 44)*y(lALD2)*y(lNO3)
      r( 45) = dbrk( 45)*y(lALD2)
      r( 46) = dbrk( 46)*y(lC2O3)*y(lNO)
      r( 47) = dbrk( 47)*y(lC2O3)*y(lNO2)
      r( 48) = dbrk( 48)*y(lPAN)
      r( 49) = dbrk( 49)*y(lC2O3)*y(lC2O3)
      r( 50) = dbrk( 50)*y(lC2O3)*y(lHO2)
      r( 51) = dbrk( 51)*y(lOH)
      r( 52) = dbrk( 52)*y(lPAR)*y(lOH)
      r( 53) = dbrk( 53)*y(lROR)
      r( 54) = dbrk( 54)*y(lROR)
      r( 55) = dbrk( 55)*y(lROR)*y(lNO2)
      r( 56) = dbrk( 56)*y(lO)*y(lOLE)
      r( 57) = dbrk( 57)*y(lOH)*y(lOLE)
      r( 58) = dbrk( 58)*y(lO3)*y(lOLE)
      r( 59) = dbrk( 59)*y(lNO3)*y(lOLE)
      r( 60) = dbrk( 60)*y(lO)*y(lETH)
      r( 61) = dbrk( 61)*y(lOH)*y(lETH)
      r( 62) = dbrk( 62)*y(lO3)*y(lETH)
      r( 63) = dbrk( 63)*y(lTOL)*y(lOH)
      r( 64) = dbrk( 64)*y(lTO2)*y(lNO)
      r( 65) = dbrk( 65)*y(lTO2)
      r( 66) = dbrk( 66)*y(lOH)*y(lCRES)
      r( 67) = dbrk( 67)*y(lCRES)*y(lNO3)
      r( 68) = dbrk( 68)*y(lCRO)*y(lNO2)
      r( 69) = dbrk( 69)*y(lOPEN)
      r( 70) = dbrk( 70)*y(lOPEN)*y(lOH)
      r( 71) = dbrk( 71)*y(lOPEN)*y(lO3)
      r( 72) = dbrk( 72)*y(lOH)*y(lXYL)
      r( 73) = dbrk( 73)*y(lOH)*y(lMGLY)
      r( 74) = dbrk( 74)*y(lMGLY)
      r( 75) = dbrk( 75)*y(lO)*y(lISOP)
      r( 76) = dbrk( 76)*y(lOH)*y(lISOP)
      r( 77) = dbrk( 77)*y(lO3)*y(lISOP)
      r( 78) = dbrk( 78)*y(lNO3)*y(lISOP)
      r( 79) = dbrk( 79)*y(lXO2)*y(lNO)
      r( 80) = dbrk( 80)*y(lXO2)*y(lXO2)
      r( 81) = dbrk( 81)*y(lXO2N)*y(lNO)
      r( 82) = dbrk( 82)*y(lSO2)*y(lOH)
      r( 83) = dbrk( 83)*y(lSO2)
      r( 84) = dbrk( 84)*y(lMEOH)*y(lOH)
      r( 85) = dbrk( 85)*y(lETOH)*y(lOH)
      r( 86) = dbrk( 86)*y(lXO2)*y(lHO2)
      r( 87) = dbrk( 87)*y(lXO2N)*y(lHO2)
      r( 88) = dbrk( 88)*y(lXO2N)*y(lXO2N)
      r( 89) = dbrk( 89)*y(lXO2)*y(lXO2N)
      r( 90) = dbrk( 90)*y(lOH)*y(lHO2)
      r( 91) = dbrk( 91)*y(lCRO)
      r( 92) = dbrk( 92)*y(lOH)*y(lISPD)
      r( 93) = dbrk( 93)*y(lO3)*y(lISPD)
      r( 94) = dbrk( 94)*y(lNO3)*y(lISPD)
      r( 95) = dbrk( 95)*y(lISPD)
      r( 96) = dbrk( 96)*y(lNO2)*y(lISOP)
      r( 97) = dbrk( 97)*y(lCL2)
      r( 98) = dbrk( 98)*y(lHOCL)
      r( 99) = dbrk( 99)*y(lCL)*y(lO3)
      r(100) = dbrk(100)*y(lCLO)*y(lNO)
      r(101) = dbrk(101)*y(lCLO)*y(lHO2)
      r(102) = dbrk(102)*y(lCL)*y(lPAR)
      r(103) = dbrk(103)*y(lCL)*y(lOLE)
      r(104) = dbrk(104)*y(lCL)
      r(105) = dbrk(105)*y(lCL)*y(lETH)
      r(106) = dbrk(106)*y(lCL)*y(lISOP)
      r(107) = dbrk(107)*y(lOH)*y(lICL1)
      r(108) = dbrk(108)*y(lCL)*y(lBUTA)
      r(109) = dbrk(109)*y(lOH)*y(lBCL1)
      r(110) = dbrk(110)*y(lCLO)*y(lCLO)
c
      return
      end
