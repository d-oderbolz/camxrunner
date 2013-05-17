      subroutine ebirxn2(ny,nr,yh,H2O,M,O2,CH4,H2,rk,r)
      implicit none
c
c----CAMx v5.41 121109
c
c     EBIRXN2 computes reaction rates for the EBI solver
c
c     Copyright 1996 - 2012
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
      include "chmdat.inc"
      include "ddmchm.inc"
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
      r(  2) = rk(  2)*yh(lO)*O2*M
      r(  3) = rk(  3)*yh(lO3)*yh(lNO)
      r(  4) = rk(  4)*yh(lO)*yh(lNO)*M
      r(  5) = rk(  5)*yh(lO)*yh(lNO2)
      r(  6) = rk(  6)*yh(lO)*yh(lNO2)
      r(  7) = rk(  7)*yh(lO)*yh(lO3)
      r(  8) = rk(  8)*yh(lO3)
      r(  9) = rk(  9)*yh(lO3)
      r( 10) = rk( 10)*yh(lO1D)*M
      r( 11) = rk( 11)*yh(lO1D)*H2O
      r( 12) = rk( 12)*yh(lO3)*yh(lOH)
      r( 13) = rk( 13)*yh(lO3)*yh(lHO2)
      r( 14) = rk( 14)*yh(lOH)*yh(lO)
      r( 15) = rk( 15)*yh(lHO2)*yh(lO)
      r( 16) = rk( 16)*yh(lOH)*yh(lOH)
      r( 17) = rk( 17)*yh(lOH)*yh(lOH)
      r( 18) = rk( 18)*yh(lOH)*yh(lHO2)
      r( 19) = rk( 19)*yh(lHO2)*yh(lHO2)
      r( 20) = rk( 20)*yh(lHO2)*yh(lHO2)*H2O
      r( 21) = rk( 21)*yh(lH2O2)
      r( 22) = rk( 22)*yh(lH2O2)*yh(lOH)
      r( 23) = rk( 23)*yh(lH2O2)*yh(lO)
      r( 24) = rk( 24)*yh(lNO)*yh(lNO)*O2
      r( 25) = rk( 25)*yh(lHO2)*yh(lNO)
      r( 26) = rk( 26)*yh(lNO2)*yh(lO3)
      r( 27) = rk( 27)*yh(lNO3)
      r( 28) = rk( 28)*yh(lNO3)
      r( 29) = rk( 29)*yh(lNO3)*yh(lNO)
      r( 30) = rk( 30)*yh(lNO3)*yh(lNO2)
      r( 31) = rk( 31)*yh(lNO3)*yh(lO)
      r( 32) = rk( 32)*yh(lNO3)*yh(lOH)
      r( 33) = rk( 33)*yh(lNO3)*yh(lHO2)
      r( 34) = rk( 34)*yh(lNO3)*yh(lO3)
      r( 35) = rk( 35)*yh(lNO3)*yh(lNO3)
      r( 36) = rk( 36)*yh(lNO3)*yh(lNO2)
      r( 37) = rk( 37)*yh(lN2O5)
      r( 38) = rk( 38)*yh(lN2O5)
      r( 39) = rk( 39)*yh(lN2O5)*H2O
      r( 40) = rk( 40)*yh(lNO)*yh(lOH)
      r( 41) = rk( 41)*yh(lNO)*yh(lNO2)*H2O
      r( 42) = rk( 42)*yh(lHONO)*yh(lHONO)
      r( 43) = rk( 43)*yh(lHONO)
      r( 44) = rk( 44)*yh(lHONO)*yh(lOH)
      r( 45) = rk( 45)*yh(lNO2)*yh(lOH)
      r( 46) = rk( 46)*yh(lHNO3)*yh(lOH)
      r( 47) = rk( 47)*yh(lHNO3)
      r( 48) = rk( 48)*yh(lHO2)*yh(lNO2)
      r( 49) = rk( 49)*yh(lPNA)
      r( 50) = rk( 50)*yh(lPNA)
      r( 51) = rk( 51)*yh(lPNA)*yh(lOH)
      r( 52) = rk( 52)*yh(lSO2)*yh(lOH)
      r( 53) = rk( 53)*yh(lC2O3)*yh(lNO)
      r( 54) = rk( 54)*yh(lC2O3)*yh(lNO2)
      r( 55) = rk( 55)*yh(lPAN)
      r( 56) = rk( 56)*yh(lPAN)
      r( 57) = rk( 57)*yh(lC2O3)*yh(lHO2)
      r( 58) = rk( 58)*yh(lC2O3)*yh(lRO2)
      r( 59) = rk( 59)*yh(lC2O3)*yh(lC2O3)
      r( 60) = rk( 60)*yh(lC2O3)*yh(lCXO3)
      r( 61) = rk( 61)*yh(lCXO3)*yh(lNO)
      r( 62) = rk( 62)*yh(lCXO3)*yh(lNO2)
      r( 63) = rk( 63)*yh(lPANX)
      r( 64) = rk( 64)*yh(lPANX)
      r( 65) = rk( 65)*yh(lCXO3)*yh(lHO2)
      r( 66) = rk( 66)*yh(lCXO3)*yh(lRO2)
      r( 67) = rk( 67)*yh(lCXO3)*yh(lCXO3)
      r( 68) = rk( 68)*yh(lRO2)*yh(lNO)
      r( 69) = rk( 69)*yh(lRO2)*yh(lHO2)
      r( 70) = rk( 70)*yh(lRO2)*yh(lRO2)
      r( 71) = rk( 71)*yh(lMEO2)*yh(lNO)
      r( 72) = rk( 72)*yh(lMEO2)*yh(lHO2)
      r( 73) = rk( 73)*yh(lMEO2)*yh(lC2O3)
      r( 74) = rk( 74)*yh(lMEO2)*yh(lRO2)
      r( 75) = rk( 75)*yh(lXO2H)*yh(lNO)
      r( 76) = rk( 76)*yh(lXO2H)*yh(lHO2)
      r( 77) = rk( 77)*yh(lXO2H)*yh(lC2O3)
      r( 78) = rk( 78)*yh(lXO2H)*yh(lRO2)
      r( 79) = rk( 79)*yh(lXO2)*yh(lNO)
      r( 80) = rk( 80)*yh(lXO2)*yh(lHO2)
      r( 81) = rk( 81)*yh(lXO2)*yh(lC2O3)
      r( 82) = rk( 82)*yh(lXO2)*yh(lRO2)
      r( 83) = rk( 83)*yh(lXO2N)*yh(lNO)
      r( 84) = rk( 84)*yh(lXO2N)*yh(lHO2)
      r( 85) = rk( 85)*yh(lXO2N)*yh(lC2O3)
      r( 86) = rk( 86)*yh(lXO2N)*yh(lRO2)
      r( 87) = rk( 87)*yh(lMEPX)*yh(lOH)
      r( 88) = rk( 88)*yh(lMEPX)
      r( 89) = rk( 89)*yh(lROOH)*yh(lOH)
      r( 90) = rk( 90)*yh(lROOH)
      r( 91) = rk( 91)*yh(lNTR)*yh(lOH)
      r( 92) = rk( 92)*yh(lNTR)
      r( 93) = rk( 93)*yh(lFACD)*yh(lOH)
      r( 94) = rk( 94)*yh(lAACD)*yh(lOH)
      r( 95) = rk( 95)*yh(lPACD)*yh(lOH)
      r( 96) = rk( 96)*yh(lFORM)*yh(lOH)
      r( 97) = rk( 97)*yh(lFORM)
      r( 98) = rk( 98)*yh(lFORM)
      r( 99) = rk( 99)*yh(lFORM)*yh(lO)
      r(100) = rk(100)*yh(lFORM)*yh(lNO3)
      r(101) = rk(101)*yh(lFORM)*yh(lHO2)
      r(102) = rk(102)*yh(lHCO3)
      r(103) = rk(103)*yh(lHCO3)*yh(lNO)
      r(104) = rk(104)*yh(lHCO3)*yh(lHO2)
      r(105) = rk(105)*yh(lALD2)*yh(lO)
      r(106) = rk(106)*yh(lALD2)*yh(lOH)
      r(107) = rk(107)*yh(lALD2)*yh(lNO3)
      r(108) = rk(108)*yh(lALD2)
      r(109) = rk(109)*yh(lALDX)*yh(lO)
      r(110) = rk(110)*yh(lALDX)*yh(lOH)
      r(111) = rk(111)*yh(lALDX)*yh(lNO3)
      r(112) = rk(112)*yh(lALDX)
      r(113) = rk(113)*yh(lGLYD)*yh(lOH)
      r(114) = rk(114)*yh(lGLYD)
      r(115) = rk(115)*yh(lGLYD)*yh(lNO3)
      r(116) = rk(116)*yh(lGLY)*yh(lOH)
      r(117) = rk(117)*yh(lGLY)
      r(118) = rk(118)*yh(lGLY)*yh(lNO3)
      r(119) = rk(119)*yh(lMGLY)
      r(120) = rk(120)*yh(lMGLY)*yh(lNO3)
      r(121) = rk(121)*yh(lMGLY)*yh(lOH)
      r(122) = rk(122)*H2*yh(lOH)
      r(123) = rk(123)*yh(lCO)*yh(lOH)
      r(124) = rk(124)*CH4*yh(lOH)
      r(125) = rk(125)*yh(lETHA)*yh(lOH)
      r(126) = rk(126)*yh(lMEOH)*yh(lOH)
      r(127) = rk(127)*yh(lETOH)*yh(lOH)
      r(128) = rk(128)*yh(lKET)
      r(129) = rk(129)*yh(lACET)
      r(130) = rk(130)*yh(lACET)*yh(lOH)
      r(131) = rk(131)*yh(lPRPA)*yh(lOH)
      r(132) = rk(132)*yh(lPAR)*yh(lOH)
      r(133) = rk(133)*yh(lROR)
      r(134) = rk(134)*yh(lROR)*O2
      r(135) = rk(135)*yh(lROR)*yh(lNO2)
      r(136) = rk(136)*yh(lETHY)*yh(lOH)
      r(137) = rk(137)*yh(lETH)*yh(lO)
      r(138) = rk(138)*yh(lETH)*yh(lOH)
      r(139) = rk(139)*yh(lETH)*yh(lO3)
      r(140) = rk(140)*yh(lETH)*yh(lNO3)
      r(141) = rk(141)*yh(lOLE)*yh(lO)
      r(142) = rk(142)*yh(lOLE)*yh(lOH)
      r(143) = rk(143)*yh(lOLE)*yh(lO3)
      r(144) = rk(144)*yh(lOLE)*yh(lNO3)
      r(145) = rk(145)*yh(lIOLE)*yh(lO)
      r(146) = rk(146)*yh(lIOLE)*yh(lOH)
      r(147) = rk(147)*yh(lIOLE)*yh(lO3)
      r(148) = rk(148)*yh(lIOLE)*yh(lNO3)
      r(149) = rk(149)*yh(lISOP)*yh(lOH)
      r(150) = rk(150)*yh(lISOP)*yh(lO)
      r(151) = rk(151)*yh(lISO2)*yh(lNO)
      r(152) = rk(152)*yh(lISO2)*yh(lHO2)
      r(153) = rk(153)*yh(lISO2)*yh(lC2O3)
      r(154) = rk(154)*yh(lISO2)*yh(lRO2)
      r(155) = rk(155)*yh(lISO2)
      r(156) = rk(156)*yh(lISOP)*yh(lO3)
      r(157) = rk(157)*yh(lISOP)*yh(lNO3)
      r(158) = rk(158)*yh(lISPD)*yh(lOH)
      r(159) = rk(159)*yh(lISPD)*yh(lO3)
      r(160) = rk(160)*yh(lISPD)*yh(lNO3)
      r(161) = rk(161)*yh(lISPD)
      r(162) = rk(162)*yh(lISPX)*yh(lOH)
      r(163) = rk(163)*yh(lHPLD)
      r(164) = rk(164)*yh(lHPLD)*yh(lNO3)
      r(165) = rk(165)*yh(lEPOX)*yh(lOH)
      r(166) = rk(166)*yh(lEPX2)*yh(lHO2)
      r(167) = rk(167)*yh(lEPX2)*yh(lNO)
      r(168) = rk(168)*yh(lEPX2)*yh(lC2O3)
      r(169) = rk(169)*yh(lEPX2)*yh(lRO2)
      r(170) = rk(170)*yh(lINTR)*yh(lOH)
      r(171) = rk(171)*yh(lTERP)*yh(lO)
      r(172) = rk(172)*yh(lTERP)*yh(lOH)
      r(173) = rk(173)*yh(lTERP)*yh(lO3)
      r(174) = rk(174)*yh(lTERP)*yh(lNO3)
      r(175) = rk(175)*yh(lBENZ)*yh(lOH)
      r(176) = rk(176)*yh(lBZO2)*yh(lNO)
      r(177) = rk(177)*yh(lBZO2)*yh(lC2O3)
      r(178) = rk(178)*yh(lBZO2)*yh(lHO2)
      r(179) = rk(179)*yh(lBZO2)*yh(lRO2)
      r(180) = rk(180)*yh(lTOL)*yh(lOH)
      r(181) = rk(181)*yh(lTO2)*yh(lNO)
      r(182) = rk(182)*yh(lTO2)*yh(lC2O3)
      r(183) = rk(183)*yh(lTO2)*yh(lHO2)
      r(184) = rk(184)*yh(lTO2)*yh(lRO2)
      r(185) = rk(185)*yh(lXYL)*yh(lOH)
      r(186) = rk(186)*yh(lXLO2)*yh(lNO)
      r(187) = rk(187)*yh(lXLO2)*yh(lHO2)
      r(188) = rk(188)*yh(lXLO2)*yh(lC2O3)
      r(189) = rk(189)*yh(lXLO2)*yh(lRO2)
      r(190) = rk(190)*yh(lCRES)*yh(lOH)
      r(191) = rk(191)*yh(lCRES)*yh(lNO3)
      r(192) = rk(192)*yh(lCRO)*yh(lNO2)
      r(193) = rk(193)*yh(lCRO)*yh(lHO2)
      r(194) = rk(194)*yh(lCRON)*yh(lOH)
      r(195) = rk(195)*yh(lCRON)*yh(lNO3)
      r(196) = rk(196)*yh(lCRON)
      r(197) = rk(197)*yh(lXOPN)
      r(198) = rk(198)*yh(lXOPN)*yh(lOH)
      r(199) = rk(199)*yh(lXOPN)*yh(lO3)
      r(200) = rk(200)*yh(lXOPN)*yh(lNO3)
      r(201) = rk(201)*yh(lOPEN)
      r(202) = rk(202)*yh(lOPEN)*yh(lOH)
      r(203) = rk(203)*yh(lOPEN)*yh(lO3)
      r(204) = rk(204)*yh(lOPEN)*yh(lNO3)
      r(205) = rk(205)*yh(lCAT1)*yh(lOH)
      r(206) = rk(206)*yh(lCAT1)*yh(lNO3)
      r(207) = rk(207)*yh(lOPO3)*yh(lNO)
      r(208) = rk(208)*yh(lOPO3)*yh(lNO2)
      r(209) = rk(209)*yh(lOPAN)
      r(210) = rk(210)*yh(lOPO3)*yh(lHO2)
      r(211) = rk(211)*yh(lOPO3)*yh(lC2O3)
      r(212) = rk(212)*yh(lOPO3)*yh(lRO2)
      r(213) = rk(213)*yh(lOPAN)*yh(lOH)
c
      return
      end

