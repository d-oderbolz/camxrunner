      subroutine ierxn3(y,ny,r,rrk,nk)
      implicit none
c
c-----CAMx v5.10 090918
c
c     IERXN3 computes IEH solver fluxes for each reaction
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c     Created by the CMC version 5.2
c
c     Routines Called:
c        none
c
c     Called by:
c        IEHSOLV
c        IERATE3
c        IEJAC3
c
      include "camx.prm"
      include "chmdat.com"
      include "iehchem.com"
c
      integer ny, nk
      real H2O, M, O2, CH4, H2, N2
      real y(ny+6)
      real r(nk)
      real rrk(nk)
c
c --- Entry point
c
      H2O = y(ny+2)
      M   = y(ny+3)
      O2  = y(ny+4)
      CH4 = y(ny+5)
      H2  = y(ny+6)
      N2  = M - O2
c
c --- Calculate reaction rates
c
      r(  1) = rrk(  1)*y(iNO2)
      r(  2) = rrk(  2)*y(iO)
      r(  3) = rrk(  3)*y(iO3)*y(iNO)
      r(  4) = rrk(  4)*y(iO)*y(iNO2)
      r(  5) = rrk(  5)*y(iO)*y(iNO2)
      r(  6) = rrk(  6)*y(iO)*y(iNO)
      r(  7) = rrk(  7)*y(iNO2)*y(iO3)
      r(  8) = rrk(  8)*y(iO3)
      r(  9) = rrk(  9)*y(iO3)
      r( 10) = rrk( 10)*y(iO1D)
      r( 11) = rrk( 11)*y(iO1D)*H2O
      r( 12) = rrk( 12)*y(iO3)*y(iOH)
      r( 13) = rrk( 13)*y(iO3)*y(iHO2)
      r( 14) = rrk( 14)*y(iNO3)
      r( 15) = rrk( 15)*y(iNO3)*y(iNO)
      r( 16) = rrk( 16)*y(iNO3)*y(iNO2)
      r( 17) = rrk( 17)*y(iNO3)*y(iNO2)
      r( 18) = rrk( 18)*y(iN2O5)*H2O
      r( 19) = rrk( 19)*y(iN2O5)
      r( 20) = rrk( 20)*y(iNO)*y(iNO)
      r( 21) = rrk( 21)*y(iNO)*y(iNO2)*H2O
      r( 22) = rrk( 22)*y(iNO)*y(iOH)
      r( 23) = rrk( 23)*y(iHONO)
      r( 24) = rrk( 24)*y(iOH)*y(iHONO)
      r( 25) = rrk( 25)*y(iHONO)*y(iHONO)
      r( 26) = rrk( 26)*y(iNO2)*y(iOH)
      r( 27) = rrk( 27)*y(iOH)*y(iHNO3)
      r( 28) = rrk( 28)*y(iHO2)*y(iNO)
      r( 29) = rrk( 29)*y(iHO2)*y(iNO2)
      r( 30) = rrk( 30)*y(iPNA)
      r( 31) = rrk( 31)*y(iOH)*y(iPNA)
      r( 32) = rrk( 32)*y(iHO2)*y(iHO2)
      r( 33) = rrk( 33)*y(iHO2)*y(iHO2)*H2O
      r( 34) = rrk( 34)*y(iH2O2)
      r( 35) = rrk( 35)*y(iOH)*y(iH2O2)
      r( 36) = rrk( 36)*y(iOH)*y(iCO)
      r( 37) = rrk( 37)*y(iFORM)*y(iOH)
      r( 38) = rrk( 38)*y(iFORM)
      r( 39) = rrk( 39)*y(iFORM)
      r( 40) = rrk( 40)*y(iFORM)*y(iO)
      r( 41) = rrk( 41)*y(iFORM)*y(iNO3)
      r( 42) = rrk( 42)*y(iALD2)*y(iO)
      r( 43) = rrk( 43)*y(iALD2)*y(iOH)
      r( 44) = rrk( 44)*y(iALD2)*y(iNO3)
      r( 45) = rrk( 45)*y(iALD2)
      r( 46) = rrk( 46)*y(iC2O3)*y(iNO)
      r( 47) = rrk( 47)*y(iC2O3)*y(iNO2)
      r( 48) = rrk( 48)*y(iPAN)
      r( 49) = rrk( 49)*y(iC2O3)*y(iC2O3)
      r( 50) = rrk( 50)*y(iC2O3)*y(iHO2)
      r( 51) = rrk( 51)*y(iOH)
      r( 52) = rrk( 52)*y(iPAR)*y(iOH)
      r( 53) = rrk( 53)*y(iROR)
      r( 54) = rrk( 54)*y(iROR)
      r( 55) = rrk( 55)*y(iROR)*y(iNO2)
      r( 56) = rrk( 56)*y(iO)*y(iOLE)
      r( 57) = rrk( 57)*y(iOH)*y(iOLE)
      r( 58) = rrk( 58)*y(iO3)*y(iOLE)
      r( 59) = rrk( 59)*y(iNO3)*y(iOLE)
      r( 60) = rrk( 60)*y(iO)*y(iETH)
      r( 61) = rrk( 61)*y(iOH)*y(iETH)
      r( 62) = rrk( 62)*y(iO3)*y(iETH)
      r( 63) = rrk( 63)*y(iTOL)*y(iOH)
      r( 64) = rrk( 64)*y(iTO2)*y(iNO)
      r( 65) = rrk( 65)*y(iTO2)
      r( 66) = rrk( 66)*y(iOH)*y(iCRES)
      r( 67) = rrk( 67)*y(iCRES)*y(iNO3)
      r( 68) = rrk( 68)*y(iCRO)*y(iNO2)
      r( 69) = rrk( 69)*y(iOPEN)
      r( 70) = rrk( 70)*y(iOPEN)*y(iOH)
      r( 71) = rrk( 71)*y(iOPEN)*y(iO3)
      r( 72) = rrk( 72)*y(iOH)*y(iXYL)
      r( 73) = rrk( 73)*y(iOH)*y(iMGLY)
      r( 74) = rrk( 74)*y(iMGLY)
      r( 75) = rrk( 75)*y(iO)*y(iISOP)
      r( 76) = rrk( 76)*y(iOH)*y(iISOP)
      r( 77) = rrk( 77)*y(iO3)*y(iISOP)
      r( 78) = rrk( 78)*y(iNO3)*y(iISOP)
      r( 79) = rrk( 79)*y(iXO2)*y(iNO)
      r( 80) = rrk( 80)*y(iXO2)*y(iXO2)
      r( 81) = rrk( 81)*y(iXO2N)*y(iNO)
      r( 82) = rrk( 82)*y(iSO2)*y(iOH)
      r( 83) = rrk( 83)*y(iSO2)
      r( 84) = rrk( 84)*y(iMEOH)*y(iOH)
      r( 85) = rrk( 85)*y(iETOH)*y(iOH)
      r( 86) = rrk( 86)*y(iXO2)*y(iHO2)
      r( 87) = rrk( 87)*y(iXO2N)*y(iHO2)
      r( 88) = rrk( 88)*y(iXO2N)*y(iXO2N)
      r( 89) = rrk( 89)*y(iXO2)*y(iXO2N)
      r( 90) = rrk( 90)*y(iOH)*y(iHO2)
      r( 91) = rrk( 91)*y(iCRO)
      r( 92) = rrk( 92)*y(iOH)*y(iISPD)
      r( 93) = rrk( 93)*y(iO3)*y(iISPD)
      r( 94) = rrk( 94)*y(iNO3)*y(iISPD)
      r( 95) = rrk( 95)*y(iISPD)
      r( 96) = rrk( 96)*y(iNO2)*y(iISOP)
c
      return
      end
