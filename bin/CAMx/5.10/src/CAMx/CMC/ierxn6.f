      subroutine ierxn6(y,ny,r,rrk,nk)
      implicit none
c
c-----CAMx v5.10 090918
c
c     IERXN6 computes IEH solver fluxes for each reaction
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
c        IERATE6
c        IEJAC6
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
      r(  2) = rrk(  2)*y(iO)*O2*M
      r(  3) = rrk(  3)*y(iO3)*y(iNO)
      r(  4) = rrk(  4)*y(iO)*y(iNO2)
      r(  5) = rrk(  5)*y(iO)*y(iNO2)
      r(  6) = rrk(  6)*y(iO)*y(iNO)
      r(  7) = rrk(  7)*y(iNO2)*y(iO3)
      r(  8) = rrk(  8)*y(iO3)
      r(  9) = rrk(  9)*y(iO3)
      r( 10) = rrk( 10)*y(iO1D)*M
      r( 11) = rrk( 11)*y(iO1D)*H2O
      r( 12) = rrk( 12)*y(iO3)*y(iOH)
      r( 13) = rrk( 13)*y(iO3)*y(iHO2)
      r( 14) = rrk( 14)*y(iNO3)
      r( 15) = rrk( 15)*y(iNO3)
      r( 16) = rrk( 16)*y(iNO3)*y(iNO)
      r( 17) = rrk( 17)*y(iNO3)*y(iNO2)
      r( 18) = rrk( 18)*y(iNO3)*y(iNO2)
      r( 19) = rrk( 19)*y(iN2O5)*H2O
      r( 20) = rrk( 20)*y(iN2O5)*H2O*H2O
      r( 21) = rrk( 21)*y(iN2O5)
      r( 22) = rrk( 22)*y(iNO)*y(iNO)*O2
      r( 23) = rrk( 23)*y(iNO)*y(iNO2)*H2O
      r( 24) = rrk( 24)*y(iNO)*y(iOH)
      r( 25) = rrk( 25)*y(iHONO)
      r( 26) = rrk( 26)*y(iOH)*y(iHONO)
      r( 27) = rrk( 27)*y(iHONO)*y(iHONO)
      r( 28) = rrk( 28)*y(iNO2)*y(iOH)
      r( 29) = rrk( 29)*y(iOH)*y(iHNO3)
      r( 30) = rrk( 30)*y(iHO2)*y(iNO)
      r( 31) = rrk( 31)*y(iHO2)*y(iNO2)
      r( 32) = rrk( 32)*y(iPNA)
      r( 33) = rrk( 33)*y(iOH)*y(iPNA)
      r( 34) = rrk( 34)*y(iHO2)*y(iHO2)
      r( 35) = rrk( 35)*y(iHO2)*y(iHO2)*H2O
      r( 36) = rrk( 36)*y(iH2O2)
      r( 37) = rrk( 37)*y(iOH)*y(iH2O2)
      r( 38) = rrk( 38)*y(iO1D)*H2
      r( 39) = rrk( 39)*y(iOH)*H2
      r( 40) = rrk( 40)*y(iOH)*y(iO)
      r( 41) = rrk( 41)*y(iOH)*y(iOH)
      r( 42) = rrk( 42)*y(iOH)*y(iOH)
      r( 43) = rrk( 43)*y(iOH)*y(iHO2)
      r( 44) = rrk( 44)*y(iHO2)*y(iO)
      r( 45) = rrk( 45)*y(iH2O2)*y(iO)
      r( 46) = rrk( 46)*y(iNO3)*y(iO)
      r( 47) = rrk( 47)*y(iNO3)*y(iOH)
      r( 48) = rrk( 48)*y(iNO3)*y(iHO2)
      r( 49) = rrk( 49)*y(iNO3)*y(iO3)
      r( 50) = rrk( 50)*y(iNO3)*y(iNO3)
      r( 51) = rrk( 51)*y(iPNA)
      r( 52) = rrk( 52)*y(iHNO3)
      r( 53) = rrk( 53)*y(iN2O5)
      r( 54) = rrk( 54)*y(iXO2)*y(iNO)
      r( 55) = rrk( 55)*y(iXO2N)*y(iNO)
      r( 56) = rrk( 56)*y(iXO2)*y(iHO2)
      r( 57) = rrk( 57)*y(iXO2N)*y(iHO2)
      r( 58) = rrk( 58)*y(iXO2)*y(iXO2)
      r( 59) = rrk( 59)*y(iXO2N)*y(iXO2N)
      r( 60) = rrk( 60)*y(iXO2)*y(iXO2N)
      r( 61) = rrk( 61)*y(iNTR)*y(iOH)
      r( 62) = rrk( 62)*y(iNTR)
      r( 63) = rrk( 63)*y(iSO2)*y(iOH)
      r( 64) = rrk( 64)*y(iROOH)*y(iOH)
      r( 65) = rrk( 65)*y(iROOH)
      r( 66) = rrk( 66)*y(iOH)*y(iCO)
      r( 67) = rrk( 67)*y(iOH)*CH4
      r( 68) = rrk( 68)*y(iMEO2)*y(iNO)
      r( 69) = rrk( 69)*y(iMEO2)*y(iHO2)
      r( 70) = rrk( 70)*y(iMEO2)*y(iMEO2)
      r( 71) = rrk( 71)*y(iMEPX)*y(iOH)
      r( 72) = rrk( 72)*y(iMEPX)
      r( 73) = rrk( 73)*y(iMEOH)*y(iOH)
      r( 74) = rrk( 74)*y(iFORM)*y(iOH)
      r( 75) = rrk( 75)*y(iFORM)
      r( 76) = rrk( 76)*y(iFORM)
      r( 77) = rrk( 77)*y(iFORM)*y(iO)
      r( 78) = rrk( 78)*y(iFORM)*y(iNO3)
      r( 79) = rrk( 79)*y(iFORM)*y(iHO2)
      r( 80) = rrk( 80)*y(iHCO3)
      r( 81) = rrk( 81)*y(iHCO3)*y(iNO)
      r( 82) = rrk( 82)*y(iHCO3)*y(iHO2)
      r( 83) = rrk( 83)*y(iFACD)*y(iOH)
      r( 84) = rrk( 84)*y(iALD2)*y(iO)
      r( 85) = rrk( 85)*y(iALD2)*y(iOH)
      r( 86) = rrk( 86)*y(iALD2)*y(iNO3)
      r( 87) = rrk( 87)*y(iALD2)
      r( 88) = rrk( 88)*y(iC2O3)*y(iNO)
      r( 89) = rrk( 89)*y(iC2O3)*y(iNO2)
      r( 90) = rrk( 90)*y(iPAN)
      r( 91) = rrk( 91)*y(iPAN)
      r( 92) = rrk( 92)*y(iC2O3)*y(iHO2)
      r( 93) = rrk( 93)*y(iC2O3)*y(iMEO2)
      r( 94) = rrk( 94)*y(iC2O3)*y(iXO2)
      r( 95) = rrk( 95)*y(iC2O3)*y(iC2O3)
      r( 96) = rrk( 96)*y(iPACD)*y(iOH)
      r( 97) = rrk( 97)*y(iPACD)
      r( 98) = rrk( 98)*y(iAACD)*y(iOH)
      r( 99) = rrk( 99)*y(iALDX)*y(iO)
      r(100) = rrk(100)*y(iALDX)*y(iOH)
      r(101) = rrk(101)*y(iALDX)*y(iNO3)
      r(102) = rrk(102)*y(iALDX)
      r(103) = rrk(103)*y(iCXO3)*y(iNO)
      r(104) = rrk(104)*y(iCXO3)*y(iNO2)
      r(105) = rrk(105)*y(iPANX)
      r(106) = rrk(106)*y(iPANX)
      r(107) = rrk(107)*y(iPANX)*y(iOH)
      r(108) = rrk(108)*y(iCXO3)*y(iHO2)
      r(109) = rrk(109)*y(iCXO3)*y(iMEO2)
      r(110) = rrk(110)*y(iCXO3)*y(iXO2)
      r(111) = rrk(111)*y(iCXO3)*y(iCXO3)
      r(112) = rrk(112)*y(iCXO3)*y(iC2O3)
      r(113) = rrk(113)*y(iOH)*y(iETHA)
      r(114) = rrk(114)*y(iOH)*y(iETOH)
      r(115) = rrk(115)*y(iPAR)*y(iOH)
      r(116) = rrk(116)*y(iROR)
      r(117) = rrk(117)*y(iROR)
      r(118) = rrk(118)*y(iROR)*y(iNO2)
      r(119) = rrk(119)*y(iO)*y(iOLE)
      r(120) = rrk(120)*y(iOH)*y(iOLE)
      r(121) = rrk(121)*y(iO3)*y(iOLE)
      r(122) = rrk(122)*y(iNO3)*y(iOLE)
      r(123) = rrk(123)*y(iO)*y(iETH)
      r(124) = rrk(124)*y(iOH)*y(iETH)
      r(125) = rrk(125)*y(iO3)*y(iETH)
      r(126) = rrk(126)*y(iNO3)*y(iETH)
      r(127) = rrk(127)*y(iIOLE)*y(iO)
      r(128) = rrk(128)*y(iIOLE)*y(iOH)
      r(129) = rrk(129)*y(iIOLE)*y(iO3)
      r(130) = rrk(130)*y(iIOLE)*y(iNO3)
      r(131) = rrk(131)*y(iTOL)*y(iOH)
      r(132) = rrk(132)*y(iTO2)*y(iNO)
      r(133) = rrk(133)*y(iTO2)
      r(134) = rrk(134)*y(iOH)*y(iCRES)
      r(135) = rrk(135)*y(iCRES)*y(iNO3)
      r(136) = rrk(136)*y(iCRO)*y(iNO2)
      r(137) = rrk(137)*y(iCRO)*y(iHO2)
      r(138) = rrk(138)*y(iOPEN)
      r(139) = rrk(139)*y(iOPEN)*y(iOH)
      r(140) = rrk(140)*y(iOPEN)*y(iO3)
      r(141) = rrk(141)*y(iOH)*y(iXYL)
      r(142) = rrk(142)*y(iOH)*y(iMGLY)
      r(143) = rrk(143)*y(iMGLY)
      r(144) = rrk(144)*y(iO)*y(iISOP)
      r(145) = rrk(145)*y(iOH)*y(iISOP)
      r(146) = rrk(146)*y(iO3)*y(iISOP)
      r(147) = rrk(147)*y(iNO3)*y(iISOP)
      r(148) = rrk(148)*y(iNO2)*y(iISOP)
      r(149) = rrk(149)*y(iOH)*y(iISPD)
      r(150) = rrk(150)*y(iO3)*y(iISPD)
      r(151) = rrk(151)*y(iNO3)*y(iISPD)
      r(152) = rrk(152)*y(iISPD)
      r(153) = rrk(153)*y(iTERP)*y(iO)
      r(154) = rrk(154)*y(iTERP)*y(iOH)
      r(155) = rrk(155)*y(iTERP)*y(iO3)
      r(156) = rrk(156)*y(iTERP)*y(iNO3)
c
      return
      end
