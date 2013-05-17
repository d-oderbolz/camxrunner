      subroutine ierxn5(y,ny,r,rrk,nk)
      implicit none
c
c----CAMx v5.41 121109
c
c     IERXN5 computes IEH solver fluxes for each reaction
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c     Created by the CMC version 5.2
c
c     Routines Called:
c        none
c
c     Called by:
c        IEHSOLV
c        IERATE5
c        IEJAC5
c
      include "camx.prm"
      include "chmdat.inc"
      include "iehchem.inc"
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
      r(  3) = rrk(  3)*y(iO)*y(iO3)
      r(  4) = rrk(  4)*y(iO)*y(iNO)*M
      r(  5) = rrk(  5)*y(iO)*y(iNO2)
      r(  6) = rrk(  6)*y(iO)*y(iNO2)
      r(  7) = rrk(  7)*y(iO3)*y(iNO)
      r(  8) = rrk(  8)*y(iO3)*y(iNO2)
      r(  9) = rrk(  9)*y(iNO)*y(iNO3)
      r( 10) = rrk( 10)*y(iNO)*y(iNO)*O2
      r( 11) = rrk( 11)*y(iNO2)*y(iNO3)
      r( 12) = rrk( 12)*y(iN2O5)
      r( 13) = rrk( 13)*y(iN2O5)*H2O
      r( 14) = rrk( 14)*y(iNO2)*y(iNO3)
      r( 15) = rrk( 15)*y(iNO3)
      r( 16) = rrk( 16)*y(iNO3)
      r( 17) = rrk( 17)*y(iO3)
      r( 18) = rrk( 18)*y(iO3)
      r( 19) = rrk( 19)*y(iO1D)*H2O
      r( 20) = rrk( 20)*y(iO1D)*M
      r( 21) = rrk( 21)*y(iOH)*y(iNO)
      r( 22) = rrk( 22)*y(iHONO)
      r( 23) = rrk( 23)*y(iHONO)
      r( 24) = rrk( 24)*y(iOH)*y(iHONO)
      r( 25) = rrk( 25)*y(iOH)*y(iNO2)
      r( 26) = rrk( 26)*y(iOH)*y(iNO3)
      r( 27) = rrk( 27)*y(iOH)*y(iHNO3)
      r( 28) = rrk( 28)*y(iHNO3)
      r( 29) = rrk( 29)*y(iOH)*y(iCO)
      r( 30) = rrk( 30)*y(iOH)*y(iO3)
      r( 31) = rrk( 31)*y(iHO2)*y(iNO)
      r( 32) = rrk( 32)*y(iHO2)*y(iNO2)
      r( 33) = rrk( 33)*y(iHNO4)
      r( 34) = rrk( 34)*y(iHNO4)
      r( 35) = rrk( 35)*y(iHNO4)*y(iOH)
      r( 36) = rrk( 36)*y(iHO2)*y(iO3)
      r( 37) = rrk( 37)*y(iHO2)*y(iHO2)
      r( 38) = rrk( 38)*y(iHO2)*y(iHO2)*H2O
      r( 39) = rrk( 39)*y(iNO3)*y(iHO2)
      r( 40) = rrk( 40)*y(iNO3)*y(iNO3)
      r( 41) = rrk( 41)*y(iHO2H)
      r( 42) = rrk( 42)*y(iHO2H)*y(iOH)
      r( 43) = rrk( 43)*y(iOH)*y(iHO2)
      r( 44) = rrk( 44)*y(iOH)*y(iSO2)
      r( 45) = rrk( 45)*y(iOH)*H2
      r( 46) = rrk( 46)*y(iCXO2)*y(iNO)
      r( 47) = rrk( 47)*y(iCXO2)*y(iHO2)
      r( 48) = rrk( 48)*y(iCXO2)*y(iNO3)
      r( 49) = rrk( 49)*y(iCXO2)*y(iCXO2)
      r( 50) = rrk( 50)*y(iCXO2)*y(iCXO2)
      r( 51) = rrk( 51)*y(iRO2R)*y(iNO)
      r( 52) = rrk( 52)*y(iRO2R)*y(iHO2)
      r( 53) = rrk( 53)*y(iRO2R)*y(iNO3)
      r( 54) = rrk( 54)*y(iRO2R)*y(iCXO2)
      r( 55) = rrk( 55)*y(iRO2R)*y(iRO2R)
      r( 56) = rrk( 56)*y(iR2O2)*y(iNO)
      r( 57) = rrk( 57)*y(iR2O2)*y(iHO2)
      r( 58) = rrk( 58)*y(iR2O2)*y(iNO3)
      r( 59) = rrk( 59)*y(iR2O2)*y(iCXO2)
      r( 60) = rrk( 60)*y(iR2O2)*y(iRO2R)
      r( 61) = rrk( 61)*y(iR2O2)*y(iR2O2)
      r( 62) = rrk( 62)*y(iRO2N)*y(iNO)
      r( 63) = rrk( 63)*y(iRO2N)*y(iHO2)
      r( 64) = rrk( 64)*y(iRO2N)*y(iCXO2)
      r( 65) = rrk( 65)*y(iRO2N)*y(iNO3)
      r( 66) = rrk( 66)*y(iRO2N)*y(iRO2R)
      r( 67) = rrk( 67)*y(iRO2N)*y(iR2O2)
      r( 68) = rrk( 68)*y(iRO2N)*y(iRO2N)
      r( 69) = rrk( 69)*y(iCCO3)*y(iNO2)
      r( 70) = rrk( 70)*y(iPAN)
      r( 71) = rrk( 71)*y(iCCO3)*y(iNO)
      r( 72) = rrk( 72)*y(iCCO3)*y(iHO2)
      r( 73) = rrk( 73)*y(iCCO3)*y(iNO3)
      r( 74) = rrk( 74)*y(iCCO3)*y(iCXO2)
      r( 75) = rrk( 75)*y(iCCO3)*y(iRO2R)
      r( 76) = rrk( 76)*y(iCCO3)*y(iR2O2)
      r( 77) = rrk( 77)*y(iCCO3)*y(iRO2N)
      r( 78) = rrk( 78)*y(iCCO3)*y(iCCO3)
      r( 79) = rrk( 79)*y(iRCO3)*y(iNO2)
      r( 80) = rrk( 80)*y(iPAN2)
      r( 81) = rrk( 81)*y(iRCO3)*y(iNO)
      r( 82) = rrk( 82)*y(iRCO3)*y(iHO2)
      r( 83) = rrk( 83)*y(iRCO3)*y(iNO3)
      r( 84) = rrk( 84)*y(iRCO3)*y(iCXO2)
      r( 85) = rrk( 85)*y(iRCO3)*y(iRO2R)
      r( 86) = rrk( 86)*y(iRCO3)*y(iR2O2)
      r( 87) = rrk( 87)*y(iRCO3)*y(iRO2N)
      r( 88) = rrk( 88)*y(iRCO3)*y(iCCO3)
      r( 89) = rrk( 89)*y(iRCO3)*y(iRCO3)
      r( 90) = rrk( 90)*y(iBZCO)*y(iNO2)
      r( 91) = rrk( 91)*y(iPBZN)
      r( 92) = rrk( 92)*y(iBZCO)*y(iNO)
      r( 93) = rrk( 93)*y(iBZCO)*y(iHO2)
      r( 94) = rrk( 94)*y(iBZCO)*y(iNO3)
      r( 95) = rrk( 95)*y(iBZCO)*y(iCXO2)
      r( 96) = rrk( 96)*y(iBZCO)*y(iRO2R)
      r( 97) = rrk( 97)*y(iBZCO)*y(iR2O2)
      r( 98) = rrk( 98)*y(iBZCO)*y(iRO2N)
      r( 99) = rrk( 99)*y(iBZCO)*y(iCCO3)
      r(100) = rrk(100)*y(iBZCO)*y(iRCO3)
      r(101) = rrk(101)*y(iBZCO)*y(iBZCO)
      r(102) = rrk(102)*y(iMCO3)*y(iNO2)
      r(103) = rrk(103)*y(iMPAN)
      r(104) = rrk(104)*y(iMCO3)*y(iNO)
      r(105) = rrk(105)*y(iMCO3)*y(iHO2)
      r(106) = rrk(106)*y(iMCO3)*y(iNO3)
      r(107) = rrk(107)*y(iMCO3)*y(iCXO2)
      r(108) = rrk(108)*y(iMCO3)*y(iRO2R)
      r(109) = rrk(109)*y(iMCO3)*y(iR2O2)
      r(110) = rrk(110)*y(iMCO3)*y(iRO2N)
      r(111) = rrk(111)*y(iMCO3)*y(iCCO3)
      r(112) = rrk(112)*y(iMCO3)*y(iRCO3)
      r(113) = rrk(113)*y(iMCO3)*y(iBZCO)
      r(114) = rrk(114)*y(iMCO3)*y(iMCO3)
      r(115) = rrk(115)*y(iTBUO)*y(iNO2)
      r(116) = rrk(116)*y(iTBUO)
      r(117) = rrk(117)*y(iBZO)*y(iNO2)
      r(118) = rrk(118)*y(iBZO)*y(iHO2)
      r(119) = rrk(119)*y(iBZO)
      r(120) = rrk(120)*y(iBZNO)*y(iNO2)
      r(121) = rrk(121)*y(iBZNO)*y(iHO2)
      r(122) = rrk(122)*y(iBZNO)
      r(123) = rrk(123)*y(iHCHO)
      r(124) = rrk(124)*y(iHCHO)
      r(125) = rrk(125)*y(iHCHO)*y(iOH)
      r(126) = rrk(126)*y(iHCHO)*y(iHO2)
      r(127) = rrk(127)*y(iHCO3)
      r(128) = rrk(128)*y(iHCO3)*y(iNO)
      r(129) = rrk(129)*y(iHCHO)*y(iNO3)
      r(130) = rrk(130)*y(iCCHO)*y(iOH)
      r(131) = rrk(131)*y(iCCHO)
      r(132) = rrk(132)*y(iCCHO)*y(iNO3)
      r(133) = rrk(133)*y(iRCHO)*y(iOH)
      r(134) = rrk(134)*y(iRCHO)
      r(135) = rrk(135)*y(iRCHO)*y(iNO3)
      r(136) = rrk(136)*y(iACET)*y(iOH)
      r(137) = rrk(137)*y(iACET)
      r(138) = rrk(138)*y(iMEK)*y(iOH)
      r(139) = rrk(139)*y(iMEK)
      r(140) = rrk(140)*y(iMEOH)*y(iOH)
      r(141) = rrk(141)*y(iCOOH)*y(iOH)
      r(142) = rrk(142)*y(iCOOH)
      r(143) = rrk(143)*y(iROOH)*y(iOH)
      r(144) = rrk(144)*y(iROOH)
      r(145) = rrk(145)*y(iGLY)
      r(146) = rrk(146)*y(iGLY)
      r(147) = rrk(147)*y(iGLY)*y(iOH)
      r(148) = rrk(148)*y(iGLY)*y(iNO3)
      r(149) = rrk(149)*y(iMGLY)
      r(150) = rrk(150)*y(iMGLY)*y(iOH)
      r(151) = rrk(151)*y(iMGLY)*y(iNO3)
      r(152) = rrk(152)*y(iBACL)
      r(153) = rrk(153)*y(iPHEN)*y(iOH)
      r(154) = rrk(154)*y(iPHEN)*y(iNO3)
      r(155) = rrk(155)*y(iCRES)*y(iOH)
      r(156) = rrk(156)*y(iCRES)*y(iNO3)
      r(157) = rrk(157)*y(iNPHE)*y(iNO3)
      r(158) = rrk(158)*y(iBALD)*y(iOH)
      r(159) = rrk(159)*y(iBALD)
      r(160) = rrk(160)*y(iBALD)*y(iNO3)
      r(161) = rrk(161)*y(iMETH)*y(iOH)
      r(162) = rrk(162)*y(iMETH)*y(iO3)
      r(163) = rrk(163)*y(iMETH)*y(iNO3)
      r(164) = rrk(164)*y(iMETH)*y(iO)
      r(165) = rrk(165)*y(iMETH)
      r(166) = rrk(166)*y(iMVK)*y(iOH)
      r(167) = rrk(167)*y(iMVK)*y(iO3)
      r(168) = rrk(168)*y(iMVK)*y(iO)
      r(169) = rrk(169)*y(iMVK)
      r(170) = rrk(170)*y(iISPD)*y(iOH)
      r(171) = rrk(171)*y(iISPD)*y(iO3)
      r(172) = rrk(172)*y(iISPD)*y(iNO3)
      r(173) = rrk(173)*y(iISPD)
      r(174) = rrk(174)*y(iPROD)*y(iOH)
      r(175) = rrk(175)*y(iPROD)
      r(176) = rrk(176)*y(iRNO3)*y(iOH)
      r(177) = rrk(177)*y(iRNO3)
      r(178) = rrk(178)*y(iDCB1)*y(iOH)
      r(179) = rrk(179)*y(iDCB1)*y(iO3)
      r(180) = rrk(180)*y(iDCB2)*y(iOH)
      r(181) = rrk(181)*y(iDCB2)
      r(182) = rrk(182)*y(iDCB3)*y(iOH)
      r(183) = rrk(183)*y(iDCB3)
      r(184) = rrk(184)*CH4*y(iOH)
      r(185) = rrk(185)*y(iETHE)*y(iOH)
      r(186) = rrk(186)*y(iETHE)*y(iO3)
      r(187) = rrk(187)*y(iETHE)*y(iNO3)
      r(188) = rrk(188)*y(iETHE)*y(iO)
      r(189) = rrk(189)*y(iISOP)*y(iOH)
      r(190) = rrk(190)*y(iISOP)*y(iO3)
      r(191) = rrk(191)*y(iISOP)*y(iNO3)
      r(192) = rrk(192)*y(iISOP)*y(iO)
      r(193) = rrk(193)*y(iTERP)*y(iOH)
      r(194) = rrk(194)*y(iTERP)*y(iO3)
      r(195) = rrk(195)*y(iTERP)*y(iNO3)
      r(196) = rrk(196)*y(iTERP)*y(iO)
      r(197) = rrk(197)*y(iALK1)*y(iOH)
      r(198) = rrk(198)*y(iALK2)*y(iOH)
      r(199) = rrk(199)*y(iALK3)*y(iOH)
      r(200) = rrk(200)*y(iALK4)*y(iOH)
      r(201) = rrk(201)*y(iALK5)*y(iOH)
      r(202) = rrk(202)*y(iARO1)*y(iOH)
      r(203) = rrk(203)*y(iARO2)*y(iOH)
      r(204) = rrk(204)*y(iOLE1)*y(iOH)
      r(205) = rrk(205)*y(iOLE1)*y(iO3)
      r(206) = rrk(206)*y(iOLE1)*y(iNO3)
      r(207) = rrk(207)*y(iOLE1)*y(iO)
      r(208) = rrk(208)*y(iOLE2)*y(iOH)
      r(209) = rrk(209)*y(iOLE2)*y(iO3)
      r(210) = rrk(210)*y(iOLE2)*y(iNO3)
      r(211) = rrk(211)*y(iOLE2)*y(iO)
      r(212) = rrk(212)*y(iETOH)*y(iOH)
      r(213) = rrk(213)*y(iMTBE)*y(iOH)
      r(214) = rrk(214)*y(iMBUT)*y(iOH)
      r(215) = rrk(215)*y(iMBUT)*y(iO3)
      r(216) = rrk(216)*y(iMBUT)*y(iNO3)
      r(217) = rrk(217)*y(iMBUT)*y(iO)
c
      return
      end
