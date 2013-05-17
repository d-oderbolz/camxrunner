      subroutine ebirxn9(ny,nr,yh,H2O,M,O2,CH4,H2,rk,r)
      implicit none
c
c----CAMx v6.00 130506
c
c     EBIRXN9 computes reaction rates for the EBI solver
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
      r(  4) = rk(  4)*yh(lO)*yh(lNO2)
      r(  5) = rk(  5)*yh(lO)*yh(lNO2)
      r(  6) = rk(  6)*yh(lO)*yh(lNO)
      r(  7) = rk(  7)*yh(lNO2)*yh(lO3)
      r(  8) = rk(  8)*yh(lO3)
      r(  9) = rk(  9)*yh(lO3)
      r( 10) = rk( 10)*yh(lO1D)*M
      r( 11) = rk( 11)*yh(lO1D)*H2O
      r( 12) = rk( 12)*yh(lO3)*yh(lOH)
      r( 13) = rk( 13)*yh(lO3)*yh(lHO2)
      r( 14) = rk( 14)*yh(lNO3)
      r( 15) = rk( 15)*yh(lNO3)
      r( 16) = rk( 16)*yh(lNO3)*yh(lNO)
      r( 17) = rk( 17)*yh(lNO3)*yh(lNO2)
      r( 18) = rk( 18)*yh(lNO3)*yh(lNO2)
      r( 19) = rk( 19)*yh(lN2O5)*H2O
      r( 20) = rk( 20)*yh(lN2O5)*H2O*H2O
      r( 21) = rk( 21)*yh(lN2O5)
      r( 22) = rk( 22)*yh(lNO)*yh(lNO)*O2
      r( 23) = rk( 23)*yh(lNO)*yh(lNO2)*H2O
      r( 24) = rk( 24)*yh(lNO)*yh(lOH)
      r( 25) = rk( 25)*yh(lHONO)
      r( 26) = rk( 26)*yh(lOH)*yh(lHONO)
      r( 27) = rk( 27)*yh(lHONO)*yh(lHONO)
      r( 28) = rk( 28)*yh(lNO2)*yh(lOH)
      r( 29) = rk( 29)*yh(lOH)*yh(lHNO3)
      r( 30) = rk( 30)*yh(lHO2)*yh(lNO)
      r( 31) = rk( 31)*yh(lHO2)*yh(lNO2)
      r( 32) = rk( 32)*yh(lPNA)
      r( 33) = rk( 33)*yh(lOH)*yh(lPNA)
      r( 34) = rk( 34)*yh(lHO2)*yh(lHO2)
      r( 35) = rk( 35)*yh(lHO2)*yh(lHO2)*H2O
      r( 36) = rk( 36)*yh(lH2O2)
      r( 37) = rk( 37)*yh(lOH)*yh(lH2O2)
      r( 38) = rk( 38)*yh(lO1D)*H2
      r( 39) = rk( 39)*yh(lOH)*H2
      r( 40) = rk( 40)*yh(lOH)*yh(lO)
      r( 41) = rk( 41)*yh(lOH)*yh(lOH)
      r( 42) = rk( 42)*yh(lOH)*yh(lOH)
      r( 43) = rk( 43)*yh(lOH)*yh(lHO2)
      r( 44) = rk( 44)*yh(lHO2)*yh(lO)
      r( 45) = rk( 45)*yh(lH2O2)*yh(lO)
      r( 46) = rk( 46)*yh(lNO3)*yh(lO)
      r( 47) = rk( 47)*yh(lNO3)*yh(lOH)
      r( 48) = rk( 48)*yh(lNO3)*yh(lHO2)
      r( 49) = rk( 49)*yh(lNO3)*yh(lO3)
      r( 50) = rk( 50)*yh(lNO3)*yh(lNO3)
      r( 51) = rk( 51)*yh(lPNA)
      r( 52) = rk( 52)*yh(lHNO3)
      r( 53) = rk( 53)*yh(lN2O5)
      r( 54) = rk( 54)*yh(lXO2)*yh(lNO)
      r( 55) = rk( 55)*yh(lXO2N)*yh(lNO)
      r( 56) = rk( 56)*yh(lXO2)*yh(lHO2)
      r( 57) = rk( 57)*yh(lXO2N)*yh(lHO2)
      r( 58) = rk( 58)*yh(lXO2)*yh(lXO2)
      r( 59) = rk( 59)*yh(lXO2N)*yh(lXO2N)
      r( 60) = rk( 60)*yh(lXO2)*yh(lXO2N)
      r( 61) = rk( 61)*yh(lNTR)*yh(lOH)
      r( 62) = rk( 62)*yh(lNTR)
      r( 63) = rk( 63)*yh(lSO2)*yh(lOH)
      r( 64) = rk( 64)*yh(lROOH)*yh(lOH)
      r( 65) = rk( 65)*yh(lROOH)
      r( 66) = rk( 66)*yh(lOH)*yh(lCO)
      r( 67) = rk( 67)*yh(lOH)*CH4
      r( 68) = rk( 68)*yh(lMEO2)*yh(lNO)
      r( 69) = rk( 69)*yh(lMEO2)*yh(lHO2)
      r( 70) = rk( 70)*yh(lMEO2)*yh(lMEO2)
      r( 71) = rk( 71)*yh(lMEPX)*yh(lOH)
      r( 72) = rk( 72)*yh(lMEPX)
      r( 73) = rk( 73)*yh(lMEOH)*yh(lOH)
      r( 74) = rk( 74)*yh(lFORM)*yh(lOH)
      r( 75) = rk( 75)*yh(lFORM)
      r( 76) = rk( 76)*yh(lFORM)
      r( 77) = rk( 77)*yh(lFORM)*yh(lO)
      r( 78) = rk( 78)*yh(lFORM)*yh(lNO3)
      r( 79) = rk( 79)*yh(lFORM)*yh(lHO2)
      r( 80) = rk( 80)*yh(lHCO3)
      r( 81) = rk( 81)*yh(lHCO3)*yh(lNO)
      r( 82) = rk( 82)*yh(lHCO3)*yh(lHO2)
      r( 83) = rk( 83)*yh(lFACD)*yh(lOH)
      r( 84) = rk( 84)*yh(lALD2)*yh(lO)
      r( 85) = rk( 85)*yh(lALD2)*yh(lOH)
      r( 86) = rk( 86)*yh(lALD2)*yh(lNO3)
      r( 87) = rk( 87)*yh(lALD2)
      r( 88) = rk( 88)*yh(lC2O3)*yh(lNO)
      r( 89) = rk( 89)*yh(lC2O3)*yh(lNO2)
      r( 90) = rk( 90)*yh(lPAN)
      r( 91) = rk( 91)*yh(lPAN)
      r( 92) = rk( 92)*yh(lC2O3)*yh(lHO2)
      r( 93) = rk( 93)*yh(lC2O3)*yh(lMEO2)
      r( 94) = rk( 94)*yh(lC2O3)*yh(lXO2)
      r( 95) = rk( 95)*yh(lC2O3)*yh(lC2O3)
      r( 96) = rk( 96)*yh(lPACD)*yh(lOH)
      r( 97) = rk( 97)*yh(lPACD)
      r( 98) = rk( 98)*yh(lAACD)*yh(lOH)
      r( 99) = rk( 99)*yh(lALDX)*yh(lO)
      r(100) = rk(100)*yh(lALDX)*yh(lOH)
      r(101) = rk(101)*yh(lALDX)*yh(lNO3)
      r(102) = rk(102)*yh(lALDX)
      r(103) = rk(103)*yh(lCXO3)*yh(lNO)
      r(104) = rk(104)*yh(lCXO3)*yh(lNO2)
      r(105) = rk(105)*yh(lPANX)
      r(106) = rk(106)*yh(lPANX)
      r(107) = rk(107)*yh(lPANX)*yh(lOH)
      r(108) = rk(108)*yh(lCXO3)*yh(lHO2)
      r(109) = rk(109)*yh(lCXO3)*yh(lMEO2)
      r(110) = rk(110)*yh(lCXO3)*yh(lXO2)
      r(111) = rk(111)*yh(lCXO3)*yh(lCXO3)
      r(112) = rk(112)*yh(lCXO3)*yh(lC2O3)
      r(113) = rk(113)*yh(lOH)*yh(lETHA)
      r(114) = rk(114)*yh(lOH)*yh(lETOH)
      r(115) = rk(115)*yh(lPAR)*yh(lOH)
      r(116) = rk(116)*yh(lROR)
      r(117) = rk(117)*yh(lROR)
      r(118) = rk(118)*yh(lROR)*yh(lNO2)
      r(119) = rk(119)*yh(lO)*yh(lOLE)
      r(120) = rk(120)*yh(lOH)*yh(lOLE)
      r(121) = rk(121)*yh(lO3)*yh(lOLE)
      r(122) = rk(122)*yh(lNO3)*yh(lOLE)
      r(123) = rk(123)*yh(lO)*yh(lETH)
      r(124) = rk(124)*yh(lOH)*yh(lETH)
      r(125) = rk(125)*yh(lO3)*yh(lETH)
      r(126) = rk(126)*yh(lNO3)*yh(lETH)
      r(127) = rk(127)*yh(lIOLE)*yh(lO)
      r(128) = rk(128)*yh(lIOLE)*yh(lOH)
      r(129) = rk(129)*yh(lIOLE)*yh(lO3)
      r(130) = rk(130)*yh(lIOLE)*yh(lNO3)
      r(131) = rk(131)*yh(lTOL)*yh(lOH)
      r(132) = rk(132)*yh(lTO2)*yh(lNO)
      r(133) = rk(133)*yh(lTO2)
      r(134) = rk(134)*yh(lOH)*yh(lCRES)
      r(135) = rk(135)*yh(lCRES)*yh(lNO3)
      r(136) = rk(136)*yh(lCRO)*yh(lNO2)
      r(137) = rk(137)*yh(lCRO)*yh(lHO2)
      r(138) = rk(138)*yh(lOPEN)
      r(139) = rk(139)*yh(lOPEN)*yh(lOH)
      r(140) = rk(140)*yh(lOPEN)*yh(lO3)
      r(141) = rk(141)*yh(lOH)*yh(lXYL)
      r(142) = rk(142)*yh(lOH)*yh(lMGLY)
      r(143) = rk(143)*yh(lMGLY)
      r(144) = rk(144)*yh(lO)*yh(lISOP)
      r(145) = rk(145)*yh(lOH)*yh(lISOP)
      r(146) = rk(146)*yh(lO3)*yh(lISOP)
      r(147) = rk(147)*yh(lNO3)*yh(lISOP)
      r(148) = rk(148)*yh(lNO2)*yh(lISOP)
      r(149) = rk(149)*yh(lOH)*yh(lISPD)
      r(150) = rk(150)*yh(lO3)*yh(lISPD)
      r(151) = rk(151)*yh(lNO3)*yh(lISPD)
      r(152) = rk(152)*yh(lISPD)
      r(153) = rk(153)*yh(lTERP)*yh(lO)
      r(154) = rk(154)*yh(lTERP)*yh(lOH)
      r(155) = rk(155)*yh(lTERP)*yh(lO3)
      r(156) = rk(156)*yh(lTERP)*yh(lNO3)
      r(157) = rk(157)*yh(lCL2)
      r(158) = rk(158)*yh(lHOCL)
      r(159) = rk(159)*yh(lCL)*yh(lO3)
      r(160) = rk(160)*yh(lCLO)*yh(lCLO)
      r(161) = rk(161)*yh(lCLO)*yh(lNO)
      r(162) = rk(162)*yh(lCLO)*yh(lHO2)
      r(163) = rk(163)*yh(lCLO)*yh(lNO2)
      r(164) = rk(164)*yh(lN3CL)
      r(165) = rk(165)*yh(lN3CL)
      r(166) = rk(166)*yh(lN3CL)
      r(167) = rk(167)*yh(lCL)*yh(lN3CL)
      r(168) = rk(168)*yh(lOH)*yh(lHCL)
      r(169) = rk(169)*yh(lOH)*yh(lFMCL)
      r(170) = rk(170)*yh(lFMCL)
      r(171) = rk(171)*yh(lCL)*H2
      r(172) = rk(172)*yh(lCL)*CH4
      r(173) = rk(173)*yh(lCL)*yh(lPAR)
      r(174) = rk(174)*yh(lCL)*yh(lETHA)
      r(175) = rk(175)*yh(lCL)*yh(lETH)
      r(176) = rk(176)*yh(lCL)*yh(lOLE)
      r(177) = rk(177)*yh(lCL)*yh(lIOLE)
      r(178) = rk(178)*yh(lCL)*yh(lISOP)
      r(179) = rk(179)*yh(lCL)*yh(lTERP)
      r(180) = rk(180)*yh(lCL)*yh(lTOL)
      r(181) = rk(181)*yh(lCL)*yh(lXYL)
      r(182) = rk(182)*yh(lCL)*yh(lFORM)
      r(183) = rk(183)*yh(lCL)*yh(lALD2)
      r(184) = rk(184)*yh(lCL)*yh(lALDX)
      r(185) = rk(185)*yh(lCL)*yh(lMEOH)
      r(186) = rk(186)*yh(lCL)*yh(lETOH)
      r(187) = rk(187)*yh(lN2O5)*yh(lHCL)
      r(188) = rk(188)*yh(lNTCL)
      r(189) = rk(189)*yh(lSS)*yh(lHNO3)
      r(190) = rk(190)*yh(lI)*yh(lO3)
      r(191) = rk(191)*yh(lI)*yh(lHO2)
      r(192) = rk(192)*yh(lI)*yh(lNO)
      r(193) = rk(193)*yh(lI)*yh(lNO2)
      r(194) = rk(194)*yh(lI)*yh(lNO3)
      r(195) = rk(195)*yh(lI)*yh(lINO)
      r(196) = rk(196)*yh(lI)*yh(lINO2)
      r(197) = rk(197)*yh(lIO)
      r(198) = rk(198)*yh(lIO)*yh(lNO)
      r(199) = rk(199)*yh(lIO)*yh(lNO2)
      r(200) = rk(200)*yh(lIO)*yh(lHO2)
      r(201) = rk(201)*yh(lIO)*yh(lIO)
      r(202) = rk(202)*yh(lIO)*yh(lIO)
      r(203) = rk(203)*yh(lIO)*yh(lIXOY)
      r(204) = rk(204)*yh(lI2)
      r(205) = rk(205)*yh(lI2)*yh(lOH)
      r(206) = rk(206)*yh(lI2)*yh(lNO3)
      r(207) = rk(207)*yh(lOIO)
      r(208) = rk(208)*yh(lOIO)*yh(lNO)
      r(209) = rk(209)*yh(lOIO)*yh(lOH)
      r(210) = rk(210)*yh(lOIO)*yh(lIXOY)
      r(211) = rk(211)*yh(lINO2)
      r(212) = rk(212)*yh(lINO2)
      r(213) = rk(213)*yh(lINO3)
      r(214) = rk(214)*yh(lINO3)
      r(215) = rk(215)*yh(lHOI)
      r(216) = rk(216)*yh(lHOI)*yh(lOH)
      r(217) = rk(217)*yh(lHI)*yh(lOH)
      r(218) = rk(218)*yh(lCH3I)
      r(219) = rk(219)*yh(lNBUI)
      r(220) = rk(220)*yh(lNBUI)*yh(lOH)
      r(221) = rk(221)*yh(lIBUO)*yh(lOH)
      r(222) = rk(222)*yh(lIBAC)
c
      return
      end

