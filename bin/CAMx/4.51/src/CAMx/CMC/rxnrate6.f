      subroutine rxnrate6(H2O,M,O2,CH4,H2,cncrad,conc,r)
c
c----CAMx v4.51 080522
c
c     RXNRATE computes fluxes for each reaction
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c     Routines Called:
c        none
c
c     Called by:
c        RADINIT
c        TRAP
c
      implicit none
      include "camx.prm"
      include "chmstry.com"
c
      real H2O, M, O2, CH4, H2
      real cncrad(MXRADCL),conc(MXSPEC+1),r(MXRXN)
c
      r(  1) = rk(  1)*conc(kNO2)
      r(  2) = rk(  2)*cncrad(kO)*O2*M
      r(  3) = rk(  3)*conc(kO3)*conc(kNO)
      r(  4) = rk(  4)*cncrad(kO)*conc(kNO2)
      r(  5) = rk(  5)*cncrad(kO)*conc(kNO2)
      r(  6) = rk(  6)*cncrad(kO)*conc(kNO)
      r(  7) = rk(  7)*conc(kNO2)*conc(kO3)
      r(  8) = rk(  8)*conc(kO3)
      r(  9) = rk(  9)*conc(kO3)
      r( 10) = rk( 10)*cncrad(kO1D)*M
      r( 11) = rk( 11)*cncrad(kO1D)*H2O
      r( 12) = rk( 12)*conc(kO3)*cncrad(kOH)
      r( 13) = rk( 13)*conc(kO3)*cncrad(kHO2)
      r( 14) = rk( 14)*cncrad(kNO3)
      r( 15) = rk( 15)*cncrad(kNO3)
      r( 16) = rk( 16)*cncrad(kNO3)*conc(kNO)
      r( 17) = rk( 17)*cncrad(kNO3)*conc(kNO2)
      r( 18) = rk( 18)*cncrad(kNO3)*conc(kNO2)
      r( 19) = rk( 19)*cncrad(kN2O5)*H2O
      r( 20) = rk( 20)*cncrad(kN2O5)*H2O*H2O
      r( 21) = rk( 21)*cncrad(kN2O5)
      r( 22) = rk( 22)*conc(kNO)*conc(kNO)*O2
      r( 23) = rk( 23)*conc(kNO)*conc(kNO2)*H2O
      r( 24) = rk( 24)*conc(kNO)*cncrad(kOH)
      r( 25) = rk( 25)*conc(kHONO)
      r( 26) = rk( 26)*cncrad(kOH)*conc(kHONO)
      r( 27) = rk( 27)*conc(kHONO)*conc(kHONO)
      r( 28) = rk( 28)*conc(kNO2)*cncrad(kOH)
      r( 29) = rk( 29)*cncrad(kOH)*conc(kHNO3)
      r( 30) = rk( 30)*cncrad(kHO2)*conc(kNO)
      r( 31) = rk( 31)*cncrad(kHO2)*conc(kNO2)
      r( 32) = rk( 32)*conc(kPNA)
      r( 33) = rk( 33)*cncrad(kOH)*conc(kPNA)
      r( 34) = rk( 34)*cncrad(kHO2)*cncrad(kHO2)
      r( 35) = rk( 35)*cncrad(kHO2)*cncrad(kHO2)*H2O
      r( 36) = rk( 36)*conc(kH2O2)
      r( 37) = rk( 37)*cncrad(kOH)*conc(kH2O2)
      r( 38) = rk( 38)*cncrad(kO1D)*H2
      r( 39) = rk( 39)*cncrad(kOH)*H2
      r( 40) = rk( 40)*cncrad(kOH)*cncrad(kO)
      r( 41) = rk( 41)*cncrad(kOH)*cncrad(kOH)
      r( 42) = rk( 42)*cncrad(kOH)*cncrad(kOH)
      r( 43) = rk( 43)*cncrad(kOH)*cncrad(kHO2)
      r( 44) = rk( 44)*cncrad(kHO2)*cncrad(kO)
      r( 45) = rk( 45)*conc(kH2O2)*cncrad(kO)
      r( 46) = rk( 46)*cncrad(kNO3)*cncrad(kO)
      r( 47) = rk( 47)*cncrad(kNO3)*cncrad(kOH)
      r( 48) = rk( 48)*cncrad(kNO3)*cncrad(kHO2)
      r( 49) = rk( 49)*cncrad(kNO3)*conc(kO3)
      r( 50) = rk( 50)*cncrad(kNO3)*cncrad(kNO3)
      r( 51) = rk( 51)*conc(kPNA)
      r( 52) = rk( 52)*conc(kHNO3)
      r( 53) = rk( 53)*cncrad(kN2O5)
      r( 54) = rk( 54)*cncrad(kXO2)*conc(kNO)
      r( 55) = rk( 55)*cncrad(kXO2N)*conc(kNO)
      r( 56) = rk( 56)*cncrad(kXO2)*cncrad(kHO2)
      r( 57) = rk( 57)*cncrad(kXO2N)*cncrad(kHO2)
      r( 58) = rk( 58)*cncrad(kXO2)*cncrad(kXO2)
      r( 59) = rk( 59)*cncrad(kXO2N)*cncrad(kXO2N)
      r( 60) = rk( 60)*cncrad(kXO2)*cncrad(kXO2N)
      r( 61) = rk( 61)*conc(kNTR)*cncrad(kOH)
      r( 62) = rk( 62)*conc(kNTR)
      r( 63) = rk( 63)*conc(kSO2)*cncrad(kOH)
      r( 64) = rk( 64)*conc(kROOH)*cncrad(kOH)
      r( 65) = rk( 65)*conc(kROOH)
      r( 66) = rk( 66)*cncrad(kOH)*conc(kCO)
      r( 67) = rk( 67)*cncrad(kOH)*CH4
      r( 68) = rk( 68)*cncrad(kMEO2)*conc(kNO)
      r( 69) = rk( 69)*cncrad(kMEO2)*cncrad(kHO2)
      r( 70) = rk( 70)*cncrad(kMEO2)*cncrad(kMEO2)
      r( 71) = rk( 71)*conc(kMEPX)*cncrad(kOH)
      r( 72) = rk( 72)*conc(kMEPX)
      r( 73) = rk( 73)*conc(kMEOH)*cncrad(kOH)
      r( 74) = rk( 74)*conc(kFORM)*cncrad(kOH)
      r( 75) = rk( 75)*conc(kFORM)
      r( 76) = rk( 76)*conc(kFORM)
      r( 77) = rk( 77)*conc(kFORM)*cncrad(kO)
      r( 78) = rk( 78)*conc(kFORM)*cncrad(kNO3)
      r( 79) = rk( 79)*conc(kFORM)*cncrad(kHO2)
      r( 80) = rk( 80)*cncrad(kHCO3)
      r( 81) = rk( 81)*cncrad(kHCO3)*conc(kNO)
      r( 82) = rk( 82)*cncrad(kHCO3)*cncrad(kHO2)
      r( 83) = rk( 83)*conc(kFACD)*cncrad(kOH)
      r( 84) = rk( 84)*conc(kALD2)*cncrad(kO)
      r( 85) = rk( 85)*conc(kALD2)*cncrad(kOH)
      r( 86) = rk( 86)*conc(kALD2)*cncrad(kNO3)
      r( 87) = rk( 87)*conc(kALD2)
      r( 88) = rk( 88)*cncrad(kC2O3)*conc(kNO)
      r( 89) = rk( 89)*cncrad(kC2O3)*conc(kNO2)
      r( 90) = rk( 90)*conc(kPAN)
      r( 91) = rk( 91)*conc(kPAN)
      r( 92) = rk( 92)*cncrad(kC2O3)*cncrad(kHO2)
      r( 93) = rk( 93)*cncrad(kC2O3)*cncrad(kMEO2)
      r( 94) = rk( 94)*cncrad(kC2O3)*cncrad(kXO2)
      r( 95) = rk( 95)*cncrad(kC2O3)*cncrad(kC2O3)
      r( 96) = rk( 96)*conc(kPACD)*cncrad(kOH)
      r( 97) = rk( 97)*conc(kPACD)
      r( 98) = rk( 98)*conc(kAACD)*cncrad(kOH)
      r( 99) = rk( 99)*conc(kALDX)*cncrad(kO)
      r(100) = rk(100)*conc(kALDX)*cncrad(kOH)
      r(101) = rk(101)*conc(kALDX)*cncrad(kNO3)
      r(102) = rk(102)*conc(kALDX)
      r(103) = rk(103)*cncrad(kCXO3)*conc(kNO)
      r(104) = rk(104)*cncrad(kCXO3)*conc(kNO2)
      r(105) = rk(105)*conc(kPANX)
      r(106) = rk(106)*conc(kPANX)
      r(107) = rk(107)*conc(kPANX)*cncrad(kOH)
      r(108) = rk(108)*cncrad(kCXO3)*cncrad(kHO2)
      r(109) = rk(109)*cncrad(kCXO3)*cncrad(kMEO2)
      r(110) = rk(110)*cncrad(kCXO3)*cncrad(kXO2)
      r(111) = rk(111)*cncrad(kCXO3)*cncrad(kCXO3)
      r(112) = rk(112)*cncrad(kCXO3)*cncrad(kC2O3)
      r(113) = rk(113)*cncrad(kOH)*conc(kETHA)
      r(114) = rk(114)*cncrad(kOH)*conc(kETOH)
      r(115) = rk(115)*conc(kPAR)*cncrad(kOH)
      r(116) = rk(116)*cncrad(kROR)
      r(117) = rk(117)*cncrad(kROR)
      r(118) = rk(118)*cncrad(kROR)*conc(kNO2)
      r(119) = rk(119)*cncrad(kO)*conc(kOLE)
      r(120) = rk(120)*cncrad(kOH)*conc(kOLE)
      r(121) = rk(121)*conc(kO3)*conc(kOLE)
      r(122) = rk(122)*cncrad(kNO3)*conc(kOLE)
      r(123) = rk(123)*cncrad(kO)*conc(kETH)
      r(124) = rk(124)*cncrad(kOH)*conc(kETH)
      r(125) = rk(125)*conc(kO3)*conc(kETH)
      r(126) = rk(126)*cncrad(kNO3)*conc(kETH)
      r(127) = rk(127)*conc(kIOLE)*cncrad(kO)
      r(128) = rk(128)*conc(kIOLE)*cncrad(kOH)
      r(129) = rk(129)*conc(kIOLE)*conc(kO3)
      r(130) = rk(130)*conc(kIOLE)*cncrad(kNO3)
      r(131) = rk(131)*conc(kTOL)*cncrad(kOH)
      r(132) = rk(132)*cncrad(kTO2)*conc(kNO)
      r(133) = rk(133)*cncrad(kTO2)
      r(134) = rk(134)*cncrad(kOH)*conc(kCRES)
      r(135) = rk(135)*conc(kCRES)*cncrad(kNO3)
      r(136) = rk(136)*cncrad(kCRO)*conc(kNO2)
      r(137) = rk(137)*cncrad(kCRO)*cncrad(kHO2)
      r(138) = rk(138)*conc(kOPEN)
      r(139) = rk(139)*conc(kOPEN)*cncrad(kOH)
      r(140) = rk(140)*conc(kOPEN)*conc(kO3)
      r(141) = rk(141)*cncrad(kOH)*conc(kXYL)
      r(142) = rk(142)*cncrad(kOH)*conc(kMGLY)
      r(143) = rk(143)*conc(kMGLY)
      r(144) = rk(144)*cncrad(kO)*conc(kISOP)
      r(145) = rk(145)*cncrad(kOH)*conc(kISOP)
      r(146) = rk(146)*conc(kO3)*conc(kISOP)
      r(147) = rk(147)*cncrad(kNO3)*conc(kISOP)
      r(148) = rk(148)*conc(kNO2)*conc(kISOP)
      r(149) = rk(149)*cncrad(kOH)*conc(kISPD)
      r(150) = rk(150)*conc(kO3)*conc(kISPD)
      r(151) = rk(151)*cncrad(kNO3)*conc(kISPD)
      r(152) = rk(152)*conc(kISPD)
      r(153) = rk(153)*conc(kTERP)*cncrad(kO)
      r(154) = rk(154)*conc(kTERP)*cncrad(kOH)
      r(155) = rk(155)*conc(kTERP)*conc(kO3)
      r(156) = rk(156)*conc(kTERP)*cncrad(kNO3)
c
      return
      end
