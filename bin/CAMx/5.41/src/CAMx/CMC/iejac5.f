      subroutine iejac5(neq,t,y,ml,mu,jac,njac)
      implicit none
c
c----CAMx v5.41 121109
c
c     IEJAC5 computes a Jacobian for the IEH solver
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c     Created by the CMC version 5.2
c
c     Routines Called:
c        IERXN5
c
c     Called by:
c        LSODE
c
      include "camx.prm"
      include "chmdat.inc"
      include "iehchem.inc"
      include "lsbox.inc"
c
      integer ml, mu, njac, neq(3), ny, nk, i, j, l
      real t, tmp, H2O, M, O2, CH4, H2, N2
      real y(neq(2)+6)
      real jac(neq(1),neq(1))
      real r(neq(3))
      real loss(MXSPEC+1)
      real gain(MXSPEC+1)
c
c --- Entry point
c
      ny = neq(2)
      nk = neq(3)
      H2O = y(ny+2)
      M   = y(ny+3)
      O2  = y(ny+4)
      CH4 = y(ny+5)
      H2  = y(ny+6)
      N2  = M - O2
c
      do l=1,ny
        Loss(l) = 0.0
        Gain(l) = 0.0
      enddo
c
c --- Get the reaction rates
c
      call ierxn5(y,ny,r,rrk,nk)
c
c --- Solve the steady state species
c
c
c   O1D
c
        Loss(iO1D  )= +( 1.000)*r( 19)+( 1.000)*r( 20)
c
        Gain(iO1D  )= +( 1.000)*r( 18)
c
      if (loss(iO1D).gt.1.0e-25.or.loss(iO1D).lt.-1.0e-25) then
        y(iO1D) = gain(iO1D)/loss(iO1D)*y(iO1D)
      else
        y(iO1D) = 0.0
      endif
      r( 19) = rrk( 19)*y(iO1D)*H2O
      r( 20) = rrk( 20)*y(iO1D)*M
c
c     O
c
        Loss(iO    )= +( 1.000)*r(  2)+( 1.000)*r(  3)+( 1.000)*r(  4)
     &                +( 1.000)*r(  5)+( 1.000)*r(  6)+( 1.000)*r(164)
     &                +( 1.000)*r(168)+( 1.000)*r(188)+( 1.000)*r(192)
     &                +( 1.000)*r(196)+( 1.000)*r(207)+( 1.000)*r(211)
     &                +( 1.000)*r(217)
c
        Gain(iO    )= +( 1.000)*r(  1)+( 1.000)*r( 16)+( 1.000)*r( 17)
     &                +( 1.000)*r( 20)
c
      if (loss(iO).gt.1.0e-25.or.loss(iO).lt.-1.0e-25) then
        y(iO) = gain(iO)/loss(iO)*y(iO)
      else
        y(iO) = 0.0
      endif
      r(  2) = rrk(  2)*y(iO)*O2*M
      r(  3) = rrk(  3)*y(iO)*y(iO3)
      r(  4) = rrk(  4)*y(iO)*y(iNO)*M
      r(  5) = rrk(  5)*y(iO)*y(iNO2)
      r(  6) = rrk(  6)*y(iO)*y(iNO2)
      r(164) = rrk(164)*y(iMETH)*y(iO)
      r(168) = rrk(168)*y(iMVK)*y(iO)
      r(188) = rrk(188)*y(iETHE)*y(iO)
      r(192) = rrk(192)*y(iISOP)*y(iO)
      r(196) = rrk(196)*y(iTERP)*y(iO)
      r(207) = rrk(207)*y(iOLE1)*y(iO)
      r(211) = rrk(211)*y(iOLE2)*y(iO)
      r(217) = rrk(217)*y(iMBUT)*y(iO)
c
c --- Calculate the Jacobian
c
c
c  N2O5   NO3    OH   HO2  RO2R  R2O2  RO2N  CCO3  RCO3  MCO3
c  BZCO  CXO2  HCO3  TBUO   BZO  BZNO    NO   NO2    O3   PAN
c  PAN2  MPAN  PBZN  NPHE  CRES  DCB2  DCB3  RNO3  HNO4
c

          jac(iN2O5,iN2O5)= +( 1.000)*r( 12)+( 1.000)*r( 13)
          jac(iNO3 ,iN2O5)= +(-1.000)*r( 12)
          jac(iNO2 ,iN2O5)= +(-1.000)*r( 12)

          jac(iN2O5,iNO3 )= +(-1.000)*r( 11)
          jac(iNO3 ,iNO3 )= +( 1.000)*r(  9)+( 1.000)*r( 11)
     &                      +( 1.000)*r( 14)+( 1.000)*r( 15)
     &                      +( 1.000)*r( 16)+( 1.000)*r( 26)
     &                      +( 1.000)*r( 39)+( 4.000)*r( 40)
     &                      +( 1.000)*r( 48)+( 1.000)*r( 53)
     &                      +( 1.000)*r( 58)+( 1.000)*r( 65)
     &                      +( 1.000)*r( 73)+( 1.000)*r( 83)
     &                      +( 1.000)*r( 94)+( 1.000)*r(106)
     &                      +( 1.000)*r(129)+( 1.000)*r(132)
          jac(iNO3 ,iNO3 )=jac(iNO3 ,iNO3 )
     &                      +( 1.000)*r(135)+( 1.000)*r(148)
     &                      +( 1.000)*r(151)+( 1.000)*r(154)
     &                      +( 1.000)*r(156)+( 1.000)*r(157)
     &                      +( 1.000)*r(160)+( 1.000)*r(163)
     &                      +( 1.000)*r(172)+( 1.000)*r(187)
     &                      +( 1.000)*r(191)+( 1.000)*r(195)
     &                      +( 1.000)*r(206)+( 1.000)*r(210)
     &                      +( 1.000)*r(216)
          jac(iOH  ,iNO3 )= +( 1.000)*r( 26)+(-0.800)*r( 39)
          jac(iHO2 ,iNO3 )= +(-1.000)*r( 26)+( 1.000)*r( 39)
     &                      +(-1.000)*r( 48)+(-1.000)*r( 53)
     &                      +(-1.000)*r( 65)+(-1.000)*r(129)
     &                      +(-0.630)*r(148)
          jac(iRO2R,iNO3 )= +( 1.000)*r( 53)+(-1.000)*r( 83)
     &                      +(-0.500)*r(163)+(-0.799)*r(172)
     &                      +(-1.000)*r(187)+(-0.749)*r(191)
     &                      +(-0.276)*r(195)+(-0.824)*r(206)
     &                      +(-0.442)*r(210)+(-0.935)*r(216)
          jac(iR2O2,iNO3 )= +( 1.000)*r( 58)+(-1.000)*r( 94)
     &                      +(-0.187)*r(191)+(-0.750)*r(195)
     &                      +(-0.488)*r(206)+(-0.711)*r(210)
          jac(iRO2N,iNO3 )= +( 1.000)*r( 65)+(-0.051)*r(172)
     &                      +(-0.064)*r(191)+(-0.250)*r(195)
     &                      +(-0.176)*r(206)+(-0.136)*r(210)
     &                      +(-0.065)*r(216)
          jac(iCCO3,iNO3 )= +( 1.000)*r( 73)+(-1.000)*r(106)
     &                      +(-1.000)*r(132)+(-1.000)*r(151)
          jac(iRCO3,iNO3 )= +( 1.000)*r( 83)+(-1.000)*r(135)
     &                      +(-0.370)*r(148)
          jac(iMCO3,iNO3 )= +( 1.000)*r(106)+(-0.500)*r(163)
     &                      +(-0.150)*r(172)
          jac(iBZCO,iNO3 )= +( 1.000)*r( 94)+(-1.000)*r(160)
          jac(iCXO2,iNO3 )= +( 1.000)*r( 48)+(-1.000)*r( 73)
     &                      +(-0.030)*r(210)
          jac(iBZO ,iNO3 )= +(-1.000)*r( 94)+(-1.000)*r(154)
     &                      +(-1.000)*r(156)
          jac(iBZNO,iNO3 )= +(-1.000)*r(157)
          jac(iNO  ,iNO3 )= +( 1.000)*r(  9)+(-1.000)*r( 14)
     &                      +(-1.000)*r( 15)
          jac(iNO2 ,iNO3 )= +(-2.000)*r(  9)+( 1.000)*r( 11)
     &                      +( 1.000)*r( 14)+(-1.000)*r( 14)
     &                      +(-1.000)*r( 16)+(-1.000)*r( 26)
     &                      +(-0.800)*r( 39)+(-4.000)*r( 40)
     &                      +(-1.000)*r( 48)+(-1.000)*r( 53)
     &                      +(-1.000)*r( 58)+(-1.000)*r( 65)
     &                      +(-1.000)*r( 73)+(-1.000)*r( 83)
     &                      +(-1.000)*r( 94)+(-1.000)*r(106)
     &                      +(-0.187)*r(191)+(-0.474)*r(195)
          jac(iNO2 ,iNO3 )=jac(iNO2 ,iNO3 )
     &                      +(-0.391)*r(210)
          jac(iNPHE,iNO3 )= +( 1.000)*r(157)
          jac(iCRES,iNO3 )= +( 1.000)*r(156)
          jac(iRNO3,iNO3 )= +(-0.572)*r(172)+(-0.813)*r(191)
     &                      +(-0.276)*r(195)+(-0.511)*r(206)
     &                      +(-0.321)*r(210)+(-1.000)*r(216)

          jac(iNO3 ,iOH  )= +( 1.000)*r( 26)+(-1.000)*r( 27)
          jac(iOH  ,iOH  )= +( 1.000)*r( 21)+( 1.000)*r( 24)
     &                      +( 1.000)*r( 25)+( 1.000)*r( 26)
     &                      +( 1.000)*r( 27)+( 1.000)*r( 29)
     &                      +( 1.000)*r( 30)+( 1.000)*r( 35)
     &                      +( 1.000)*r( 42)+( 1.000)*r( 43)
     &                      +( 1.000)*r( 44)+( 1.000)*r( 45)
     &                      +( 1.000)*r(125)+( 1.000)*r(130)
     &                      +( 1.000)*r(133)+( 1.000)*r(136)
     &                      +( 1.000)*r(138)+( 1.000)*r(140)
          jac(iOH  ,iOH  )=jac(iOH  ,iOH  )
     &                      +( 1.000)*r(141)+(-0.350)*r(141)
     &                      +( 1.000)*r(143)+(-0.660)*r(143)
     &                      +( 1.000)*r(147)+( 1.000)*r(150)
     &                      +( 1.000)*r(153)+( 1.000)*r(155)
     &                      +( 1.000)*r(158)+( 1.000)*r(161)
     &                      +( 1.000)*r(166)+( 1.000)*r(170)
     &                      +( 1.000)*r(174)+( 1.000)*r(176)
     &                      +( 1.000)*r(178)+( 1.000)*r(180)
          jac(iOH  ,iOH  )=jac(iOH  ,iOH  )
     &                      +( 1.000)*r(182)+( 1.000)*r(184)
     &                      +( 1.000)*r(185)+( 1.000)*r(189)
     &                      +( 1.000)*r(193)+( 1.000)*r(197)
     &                      +( 1.000)*r(198)+(-0.246)*r(198)
     &                      +( 1.000)*r(199)+( 1.000)*r(200)
     &                      +( 1.000)*r(201)+( 1.000)*r(202)
     &                      +( 1.000)*r(203)+( 1.000)*r(204)
     &                      +( 1.000)*r(208)+( 1.000)*r(212)
          jac(iOH  ,iOH  )=jac(iOH  ,iOH  )
     &                      +( 1.000)*r(213)+( 1.000)*r(214)
          jac(iHO2 ,iOH  )= +(-1.000)*r( 26)+(-1.000)*r( 29)
     &                      +(-1.000)*r( 30)+(-1.000)*r( 42)
     &                      +( 1.000)*r( 43)+(-1.000)*r( 44)
     &                      +(-1.000)*r( 45)+(-1.000)*r(125)
     &                      +(-1.000)*r(140)+(-0.630)*r(147)
     &                      +(-0.379)*r(174)+(-0.113)*r(176)
     &                      +(-0.121)*r(198)+(-0.224)*r(202)
     &                      +(-0.187)*r(203)+(-0.950)*r(212)
          jac(iRO2R,iOH  )= +(-0.034)*r(133)+(-0.370)*r(138)
     &                      +(-0.340)*r(143)+(-0.760)*r(153)
     &                      +(-0.760)*r(155)+(-0.500)*r(161)
     &                      +(-0.300)*r(166)+(-0.670)*r(170)
     &                      +(-0.473)*r(174)+(-0.376)*r(176)
     &                      +(-1.000)*r(178)+(-1.000)*r(185)
     &                      +(-0.907)*r(189)+(-0.750)*r(193)
     &                      +(-1.000)*r(197)+(-0.612)*r(198)
     &                      +(-0.695)*r(199)+(-0.835)*r(200)
          jac(iRO2R,iOH  )=jac(iRO2R,iOH  )
     &                      +(-0.653)*r(201)+(-0.765)*r(202)
     &                      +(-0.804)*r(203)+(-0.910)*r(204)
     &                      +(-0.918)*r(208)+(-0.050)*r(212)
     &                      +(-0.743)*r(213)+(-0.935)*r(214)
          jac(iR2O2,iOH  )= +(-1.000)*r(136)+(-0.616)*r(138)
     &                      +(-0.675)*r(166)+(-0.596)*r(176)
     &                      +(-1.000)*r(180)+(-1.000)*r(182)
     &                      +(-0.079)*r(189)+(-0.500)*r(193)
     &                      +(-0.559)*r(199)+(-0.936)*r(200)
     &                      +(-0.948)*r(201)+(-0.205)*r(204)
     &                      +(-0.001)*r(208)+(-0.381)*r(213)
          jac(iRO2N,iOH  )= +(-0.001)*r(133)+(-0.042)*r(138)
     &                      +(-0.025)*r(166)+(-0.041)*r(170)
     &                      +(-0.070)*r(174)+(-0.173)*r(176)
     &                      +(-0.093)*r(189)+(-0.250)*r(193)
     &                      +(-0.021)*r(198)+(-0.070)*r(199)
     &                      +(-0.143)*r(200)+(-0.347)*r(201)
     &                      +(-0.011)*r(202)+(-0.009)*r(203)
     &                      +(-0.090)*r(204)+(-0.082)*r(208)
     &                      +(-0.078)*r(213)+(-0.065)*r(214)
          jac(iCCO3,iOH  )= +(-1.000)*r(130)+(-1.000)*r(136)
     &                      +(-0.492)*r(138)+(-1.000)*r(150)
     &                      +(-0.675)*r(166)+(-0.029)*r(174)
     &                      +(-1.000)*r(180)+(-1.000)*r(182)
     &                      +(-0.011)*r(200)
          jac(iRCO3,iOH  )= +(-0.965)*r(133)+(-0.096)*r(138)
     &                      +(-0.370)*r(147)+(-0.049)*r(174)
          jac(iMCO3,iOH  )= +(-0.500)*r(161)+(-0.289)*r(170)
          jac(iBZCO,iOH  )= +(-1.000)*r(158)
          jac(iCXO2,iOH  )= +(-0.650)*r(141)+(-1.000)*r(184)
     &                      +(-0.011)*r(200)+(-0.162)*r(213)
          jac(iTBUO,iOH  )= +(-0.236)*r(199)+(-0.016)*r(213)
          jac(iBZO ,iOH  )= +(-0.240)*r(153)+(-0.240)*r(155)
          jac(iNO  ,iOH  )= +( 1.000)*r( 21)
          jac(iNO2 ,iOH  )= +(-1.000)*r( 24)+( 1.000)*r( 25)
     &                      +(-1.000)*r( 26)+(-1.000)*r( 35)
     &                      +(-0.338)*r(176)
          jac(iO3  ,iOH  )= +( 1.000)*r( 30)
          jac(iCRES,iOH  )= +( 1.000)*r(155)+(-0.207)*r(202)
     &                      +(-0.187)*r(203)
          jac(iDCB2,iOH  )= +( 1.000)*r(180)+(-0.108)*r(202)
     &                      +(-0.099)*r(203)
          jac(iDCB3,iOH  )= +( 1.000)*r(182)+(-0.051)*r(202)
     &                      +(-0.093)*r(203)
          jac(iRNO3,iOH  )= +( 1.000)*r(176)+(-0.310)*r(176)
          jac(iHNO4,iOH  )= +( 1.000)*r( 35)

          jac(iNO3 ,iHO2 )= +( 1.000)*r( 39)
          jac(iOH  ,iHO2 )= +(-1.000)*r( 31)+(-1.000)*r( 36)
     &                      +(-0.800)*r( 39)+( 1.000)*r( 43)
          jac(iHO2 ,iHO2 )= +( 1.000)*r( 31)+( 1.000)*r( 32)
     &                      +( 1.000)*r( 36)+( 4.000)*r( 37)
     &                      +( 4.000)*r( 38)+( 1.000)*r( 39)
     &                      +( 1.000)*r( 43)+( 1.000)*r( 47)
     &                      +( 1.000)*r( 52)+( 1.000)*r( 57)
     &                      +(-1.000)*r( 57)+( 1.000)*r( 63)
     &                      +( 1.000)*r( 72)+( 1.000)*r( 82)
     &                      +( 1.000)*r( 93)+( 1.000)*r(105)
     &                      +( 1.000)*r(118)+( 1.000)*r(121)
          jac(iHO2 ,iHO2 )=jac(iHO2 ,iHO2 )
     &                      +( 1.000)*r(126)
          jac(iRO2R,iHO2 )= +( 1.000)*r( 52)
          jac(iR2O2,iHO2 )= +( 1.000)*r( 57)
          jac(iRO2N,iHO2 )= +( 1.000)*r( 63)
          jac(iCCO3,iHO2 )= +( 1.000)*r( 72)
          jac(iRCO3,iHO2 )= +( 1.000)*r( 82)
          jac(iMCO3,iHO2 )= +( 1.000)*r(105)
          jac(iBZCO,iHO2 )= +( 1.000)*r( 93)
          jac(iCXO2,iHO2 )= +( 1.000)*r( 47)
          jac(iHCO3,iHO2 )= +(-1.000)*r(126)
          jac(iBZO ,iHO2 )= +( 1.000)*r(118)
          jac(iBZNO,iHO2 )= +( 1.000)*r(121)
          jac(iNO  ,iHO2 )= +( 1.000)*r( 31)
          jac(iNO2 ,iHO2 )= +(-1.000)*r( 31)+( 1.000)*r( 32)
     &                      +(-0.800)*r( 39)
          jac(iO3  ,iHO2 )= +( 1.000)*r( 36)+(-0.250)*r( 72)
     &                      +(-0.250)*r( 82)+(-0.250)*r( 93)
     &                      +(-0.250)*r(105)
          jac(iNPHE,iHO2 )= +(-1.000)*r(121)
          jac(iHNO4,iHO2 )= +(-1.000)*r( 32)

          jac(iNO3 ,iRO2R)= +( 1.000)*r( 53)
          jac(iHO2 ,iRO2R)= +(-1.000)*r( 51)+( 1.000)*r( 52)
     &                      +(-1.000)*r( 53)+(-1.000)*r( 54)
     &                      +(-2.000)*r( 55)+(-1.000)*r( 66)
          jac(iRO2R,iRO2R)= +( 1.000)*r( 51)+( 1.000)*r( 52)
     &                      +( 1.000)*r( 53)+( 1.000)*r( 54)
     &                      +( 4.000)*r( 55)+( 1.000)*r( 60)
     &                      +(-1.000)*r( 60)+( 1.000)*r( 66)
     &                      +( 1.000)*r( 75)+( 1.000)*r( 85)
     &                      +( 1.000)*r( 96)+( 1.000)*r(108)
          jac(iR2O2,iRO2R)= +( 1.000)*r( 60)
          jac(iRO2N,iRO2R)= +( 1.000)*r( 66)
          jac(iCCO3,iRO2R)= +( 1.000)*r( 75)
          jac(iRCO3,iRO2R)= +( 1.000)*r( 85)
          jac(iMCO3,iRO2R)= +( 1.000)*r(108)
          jac(iBZCO,iRO2R)= +( 1.000)*r( 96)
          jac(iCXO2,iRO2R)= +( 1.000)*r( 54)
          jac(iNO  ,iRO2R)= +( 1.000)*r( 51)
          jac(iNO2 ,iRO2R)= +(-1.000)*r( 51)+(-1.000)*r( 53)

          jac(iNO3 ,iR2O2)= +( 1.000)*r( 58)
          jac(iHO2 ,iR2O2)= +( 1.000)*r( 57)+(-1.000)*r( 57)
          jac(iRO2R,iR2O2)= +( 1.000)*r( 60)+(-1.000)*r( 60)
          jac(iR2O2,iR2O2)= +( 1.000)*r( 56)+( 1.000)*r( 57)
     &                      +( 1.000)*r( 58)+( 1.000)*r( 59)
     &                      +( 1.000)*r( 60)+( 4.000)*r( 61)
     &                      +( 1.000)*r( 67)+( 1.000)*r( 76)
     &                      +( 1.000)*r( 86)+( 1.000)*r( 97)
     &                      +( 1.000)*r(109)
          jac(iRO2N,iR2O2)= +( 1.000)*r( 67)+(-1.000)*r( 67)
          jac(iCCO3,iR2O2)= +( 1.000)*r( 76)+(-1.000)*r( 76)
          jac(iRCO3,iR2O2)= +( 1.000)*r( 86)+(-1.000)*r( 86)
          jac(iMCO3,iR2O2)= +( 1.000)*r(109)+(-1.000)*r(109)
          jac(iBZCO,iR2O2)= +( 1.000)*r( 97)+(-1.000)*r( 97)
          jac(iCXO2,iR2O2)= +( 1.000)*r( 59)+(-1.000)*r( 59)
          jac(iNO  ,iR2O2)= +( 1.000)*r( 56)
          jac(iNO2 ,iR2O2)= +(-1.000)*r( 56)+(-1.000)*r( 58)

          jac(iNO3 ,iRO2N)= +( 1.000)*r( 65)
          jac(iHO2 ,iRO2N)= +( 1.000)*r( 63)+(-1.000)*r( 64)
     &                      +(-1.000)*r( 65)+(-1.000)*r( 66)
     &                      +(-2.000)*r( 68)
          jac(iRO2R,iRO2N)= +( 1.000)*r( 66)
          jac(iR2O2,iRO2N)= +( 1.000)*r( 67)
          jac(iRO2N,iRO2N)= +( 1.000)*r( 62)+( 1.000)*r( 63)
     &                      +( 1.000)*r( 64)+( 1.000)*r( 65)
     &                      +( 1.000)*r( 66)+( 1.000)*r( 67)
     &                      +(-1.000)*r( 67)+( 4.000)*r( 68)
     &                      +( 1.000)*r( 77)+( 1.000)*r( 87)
     &                      +( 1.000)*r( 98)+( 1.000)*r(110)
          jac(iCCO3,iRO2N)= +( 1.000)*r( 77)
          jac(iRCO3,iRO2N)= +( 1.000)*r( 87)
          jac(iMCO3,iRO2N)= +( 1.000)*r(110)
          jac(iBZCO,iRO2N)= +( 1.000)*r( 98)
          jac(iCXO2,iRO2N)= +( 1.000)*r( 64)
          jac(iNO  ,iRO2N)= +( 1.000)*r( 62)
          jac(iNO2 ,iRO2N)= +(-1.000)*r( 65)
          jac(iRNO3,iRO2N)= +(-1.000)*r( 62)

          jac(iNO3 ,iCCO3)= +( 1.000)*r( 73)
          jac(iHO2 ,iCCO3)= +( 1.000)*r( 72)
          jac(iRO2R,iCCO3)= +( 1.000)*r( 75)+(-1.000)*r( 88)
          jac(iR2O2,iCCO3)= +( 1.000)*r( 76)+(-1.000)*r( 99)
          jac(iRO2N,iCCO3)= +( 1.000)*r( 77)
          jac(iCCO3,iCCO3)= +( 1.000)*r( 69)+( 1.000)*r( 71)
     &                      +( 1.000)*r( 72)+( 1.000)*r( 73)
     &                      +( 1.000)*r( 74)+( 1.000)*r( 75)
     &                      +( 1.000)*r( 76)+(-1.000)*r( 76)
     &                      +( 1.000)*r( 77)+( 4.000)*r( 78)
     &                      +( 1.000)*r( 88)+( 1.000)*r( 99)
     &                      +( 1.000)*r(111)+(-1.000)*r(111)
          jac(iRCO3,iCCO3)= +( 1.000)*r( 88)
          jac(iMCO3,iCCO3)= +( 1.000)*r(111)
          jac(iBZCO,iCCO3)= +( 1.000)*r( 99)
          jac(iCXO2,iCCO3)= +(-1.000)*r( 71)+(-1.000)*r( 73)
     &                      +( 1.000)*r( 74)+(-4.000)*r( 78)
     &                      +(-1.000)*r( 88)+(-1.000)*r( 99)
     &                      +(-1.000)*r(111)
          jac(iBZO ,iCCO3)= +(-1.000)*r( 99)
          jac(iNO  ,iCCO3)= +( 1.000)*r( 71)
          jac(iNO2 ,iCCO3)= +( 1.000)*r( 69)+(-1.000)*r( 71)
     &                      +(-1.000)*r( 73)
          jac(iO3  ,iCCO3)= +(-0.250)*r( 72)
          jac(iPAN ,iCCO3)= +(-1.000)*r( 69)

          jac(iNO3 ,iRCO3)= +( 1.000)*r( 83)
          jac(iHO2 ,iRCO3)= +( 1.000)*r( 82)
          jac(iRO2R,iRCO3)= +(-1.000)*r( 81)+(-1.000)*r( 83)
     &                      +( 1.000)*r( 85)+(-1.000)*r( 88)
     &                      +(-4.000)*r( 89)+(-1.000)*r(100)
     &                      +(-1.000)*r(112)
          jac(iR2O2,iRCO3)= +( 1.000)*r( 86)+(-1.000)*r(100)
          jac(iRO2N,iRCO3)= +( 1.000)*r( 87)
          jac(iCCO3,iRCO3)= +( 1.000)*r( 88)+(-1.000)*r(112)
          jac(iRCO3,iRCO3)= +( 1.000)*r( 79)+( 1.000)*r( 81)
     &                      +( 1.000)*r( 82)+( 1.000)*r( 83)
     &                      +( 1.000)*r( 84)+( 1.000)*r( 85)
     &                      +( 1.000)*r( 86)+(-1.000)*r( 86)
     &                      +( 1.000)*r( 87)+( 1.000)*r( 88)
     &                      +( 4.000)*r( 89)+( 1.000)*r(100)
     &                      +( 1.000)*r(112)
          jac(iMCO3,iRCO3)= +( 1.000)*r(112)
          jac(iBZCO,iRCO3)= +( 1.000)*r(100)
          jac(iCXO2,iRCO3)= +( 1.000)*r( 84)+(-1.000)*r( 88)
          jac(iBZO ,iRCO3)= +(-1.000)*r(100)
          jac(iNO  ,iRCO3)= +( 1.000)*r( 81)
          jac(iNO2 ,iRCO3)= +( 1.000)*r( 79)+(-1.000)*r( 81)
     &                      +(-1.000)*r( 83)
          jac(iO3  ,iRCO3)= +(-0.250)*r( 82)
          jac(iPAN2,iRCO3)= +(-1.000)*r( 79)

          jac(iNO3 ,iMCO3)= +( 1.000)*r(106)
          jac(iHO2 ,iMCO3)= +( 1.000)*r(105)
          jac(iRO2R,iMCO3)= +( 1.000)*r(108)+(-1.000)*r(112)
          jac(iR2O2,iMCO3)= +( 1.000)*r(109)+(-1.000)*r(113)
          jac(iRO2N,iMCO3)= +( 1.000)*r(110)
          jac(iCCO3,iMCO3)= +(-1.000)*r(104)+(-1.000)*r(106)
     &                      +( 1.000)*r(111)+(-1.000)*r(111)
     &                      +(-1.000)*r(112)+(-1.000)*r(113)
     &                      +(-4.000)*r(114)
          jac(iRCO3,iMCO3)= +( 1.000)*r(112)
          jac(iMCO3,iMCO3)= +( 1.000)*r(102)+( 1.000)*r(104)
     &                      +( 1.000)*r(105)+( 1.000)*r(106)
     &                      +( 1.000)*r(107)+( 1.000)*r(108)
     &                      +( 1.000)*r(109)+(-1.000)*r(109)
     &                      +( 1.000)*r(110)+( 1.000)*r(111)
     &                      +( 1.000)*r(112)+( 1.000)*r(113)
     &                      +( 4.000)*r(114)
          jac(iBZCO,iMCO3)= +( 1.000)*r(113)
          jac(iCXO2,iMCO3)= +( 1.000)*r(107)+(-1.000)*r(111)
          jac(iBZO ,iMCO3)= +(-1.000)*r(113)
          jac(iNO  ,iMCO3)= +( 1.000)*r(104)
          jac(iNO2 ,iMCO3)= +( 1.000)*r(102)+(-1.000)*r(104)
     &                      +(-1.000)*r(106)
          jac(iO3  ,iMCO3)= +(-0.250)*r(105)
          jac(iMPAN,iMCO3)= +(-1.000)*r(102)

          jac(iNO3 ,iBZCO)= +( 1.000)*r( 94)
          jac(iHO2 ,iBZCO)= +( 1.000)*r( 93)
          jac(iRO2R,iBZCO)= +( 1.000)*r( 96)+(-1.000)*r(100)
          jac(iR2O2,iBZCO)= +(-1.000)*r( 92)+(-1.000)*r( 94)
     &                      +( 1.000)*r( 97)+(-1.000)*r( 99)
     &                      +(-1.000)*r(100)+(-4.000)*r(101)
     &                      +(-1.000)*r(113)
          jac(iRO2N,iBZCO)= +( 1.000)*r( 98)
          jac(iCCO3,iBZCO)= +( 1.000)*r( 99)+(-1.000)*r(113)
          jac(iRCO3,iBZCO)= +( 1.000)*r(100)
          jac(iMCO3,iBZCO)= +( 1.000)*r(113)
          jac(iBZCO,iBZCO)= +( 1.000)*r( 90)+( 1.000)*r( 92)
     &                      +( 1.000)*r( 93)+( 1.000)*r( 94)
     &                      +( 1.000)*r( 95)+( 1.000)*r( 96)
     &                      +( 1.000)*r( 97)+(-1.000)*r( 97)
     &                      +( 1.000)*r( 98)+( 1.000)*r( 99)
     &                      +( 1.000)*r(100)+( 4.000)*r(101)
     &                      +( 1.000)*r(113)
          jac(iCXO2,iBZCO)= +( 1.000)*r( 95)+(-1.000)*r( 99)
          jac(iBZO ,iBZCO)= +(-1.000)*r( 92)+(-1.000)*r( 94)
     &                      +(-1.000)*r( 99)+(-1.000)*r(100)
     &                      +(-4.000)*r(101)+(-1.000)*r(113)
          jac(iNO  ,iBZCO)= +( 1.000)*r( 92)
          jac(iNO2 ,iBZCO)= +( 1.000)*r( 90)+(-1.000)*r( 92)
     &                      +(-1.000)*r( 94)
          jac(iO3  ,iBZCO)= +(-0.250)*r( 93)
          jac(iPBZN,iBZCO)= +(-1.000)*r( 90)

          jac(iNO3 ,iCXO2)= +( 1.000)*r( 48)
          jac(iHO2 ,iCXO2)= +(-1.000)*r( 46)+( 1.000)*r( 47)
     &                      +(-1.000)*r( 48)+(-4.000)*r( 50)
     &                      +(-1.000)*r( 54)+(-1.000)*r( 64)
          jac(iRO2R,iCXO2)= +( 1.000)*r( 54)
          jac(iR2O2,iCXO2)= +( 1.000)*r( 59)
          jac(iRO2N,iCXO2)= +( 1.000)*r( 64)
          jac(iCCO3,iCXO2)= +( 1.000)*r( 74)
          jac(iRCO3,iCXO2)= +( 1.000)*r( 84)
          jac(iMCO3,iCXO2)= +( 1.000)*r(107)
          jac(iBZCO,iCXO2)= +( 1.000)*r( 95)
          jac(iCXO2,iCXO2)= +( 1.000)*r( 46)+( 1.000)*r( 47)
     &                      +( 1.000)*r( 48)+( 4.000)*r( 49)
     &                      +( 4.000)*r( 50)+( 1.000)*r( 54)
     &                      +( 1.000)*r( 59)+(-1.000)*r( 59)
     &                      +( 1.000)*r( 64)+( 1.000)*r( 74)
     &                      +( 1.000)*r( 84)+( 1.000)*r( 95)
     &                      +( 1.000)*r(107)
          jac(iNO  ,iCXO2)= +( 1.000)*r( 46)
          jac(iNO2 ,iCXO2)= +(-1.000)*r( 46)+(-1.000)*r( 48)

          jac(iHO2 ,iHCO3)= +(-1.000)*r(127)+(-1.000)*r(128)
          jac(iHCO3,iHCO3)= +( 1.000)*r(127)+( 1.000)*r(128)
          jac(iNO  ,iHCO3)= +( 1.000)*r(128)
          jac(iNO2 ,iHCO3)= +(-1.000)*r(128)

          jac(iCXO2,iTBUO)= +(-1.000)*r(116)
          jac(iTBUO,iTBUO)= +( 1.000)*r(115)+( 1.000)*r(116)
          jac(iNO2 ,iTBUO)= +( 1.000)*r(115)
          jac(iRNO3,iTBUO)= +(-1.000)*r(115)

          jac(iHO2 ,iBZO )= +( 1.000)*r(118)
          jac(iBZO ,iBZO )= +( 1.000)*r(117)+( 1.000)*r(118)
     &                      +( 1.000)*r(119)
          jac(iNO2 ,iBZO )= +( 1.000)*r(117)
          jac(iNPHE,iBZO )= +(-1.000)*r(117)

          jac(iHO2 ,iBZNO)= +( 1.000)*r(121)
          jac(iBZNO,iBZNO)= +( 1.000)*r(120)+( 1.000)*r(121)
     &                      +( 1.000)*r(122)
          jac(iNO2 ,iBZNO)= +( 1.000)*r(120)
          jac(iNPHE,iBZNO)= +(-1.000)*r(121)+(-1.000)*r(122)

          jac(iNO3 ,iNO  )= +( 1.000)*r(  9)
          jac(iOH  ,iNO  )= +( 1.000)*r( 21)+(-1.000)*r( 31)
          jac(iHO2 ,iNO  )= +( 1.000)*r( 31)+(-1.000)*r( 46)
     &                      +(-1.000)*r( 51)+(-1.000)*r(128)
          jac(iRO2R,iNO  )= +( 1.000)*r( 51)+(-1.000)*r( 81)
          jac(iR2O2,iNO  )= +( 1.000)*r( 56)+(-1.000)*r( 92)
          jac(iRO2N,iNO  )= +( 1.000)*r( 62)
          jac(iCCO3,iNO  )= +( 1.000)*r( 71)+(-1.000)*r(104)
          jac(iRCO3,iNO  )= +( 1.000)*r( 81)
          jac(iMCO3,iNO  )= +( 1.000)*r(104)
          jac(iBZCO,iNO  )= +( 1.000)*r( 92)
          jac(iCXO2,iNO  )= +( 1.000)*r( 46)+(-1.000)*r( 71)
          jac(iHCO3,iNO  )= +( 1.000)*r(128)
          jac(iBZO ,iNO  )= +(-1.000)*r( 92)
          jac(iNO  ,iNO  )= +( 1.000)*r(  4)+( 1.000)*r(  7)
     &                      +( 1.000)*r(  9)+( 4.000)*r( 10)
     &                      +( 1.000)*r( 21)+( 1.000)*r( 31)
     &                      +( 1.000)*r( 46)+( 1.000)*r( 51)
     &                      +( 1.000)*r( 56)+( 1.000)*r( 62)
     &                      +( 1.000)*r( 71)+( 1.000)*r( 81)
     &                      +( 1.000)*r( 92)+( 1.000)*r(104)
     &                      +( 1.000)*r(128)
          jac(iNO2 ,iNO  )= +(-1.000)*r(  4)+(-1.000)*r(  7)
     &                      +(-2.000)*r(  9)+(-4.000)*r( 10)
     &                      +(-1.000)*r( 31)+(-1.000)*r( 46)
     &                      +(-1.000)*r( 51)+(-1.000)*r( 56)
     &                      +(-1.000)*r( 71)+(-1.000)*r( 81)
     &                      +(-1.000)*r( 92)+(-1.000)*r(104)
     &                      +(-1.000)*r(128)
          jac(iO3  ,iNO  )= +( 1.000)*r(  7)
          jac(iRNO3,iNO  )= +(-1.000)*r( 62)

          jac(iN2O5,iNO2 )= +(-1.000)*r( 11)
          jac(iNO3 ,iNO2 )= +(-1.000)*r(  6)+(-1.000)*r(  8)
     &                      +( 1.000)*r( 11)+( 1.000)*r( 14)
          jac(iOH  ,iNO2 )= +( 1.000)*r( 25)
          jac(iHO2 ,iNO2 )= +( 1.000)*r( 32)
          jac(iCCO3,iNO2 )= +( 1.000)*r( 69)
          jac(iRCO3,iNO2 )= +( 1.000)*r( 79)
          jac(iMCO3,iNO2 )= +( 1.000)*r(102)
          jac(iBZCO,iNO2 )= +( 1.000)*r( 90)
          jac(iTBUO,iNO2 )= +( 1.000)*r(115)
          jac(iBZO ,iNO2 )= +( 1.000)*r(117)
          jac(iBZNO,iNO2 )= +( 1.000)*r(120)
          jac(iNO  ,iNO2 )= +(-1.000)*r(  1)+(-1.000)*r(  5)
     &                      +(-1.000)*r( 14)
          jac(iNO2 ,iNO2 )= +( 1.000)*r(  1)+( 1.000)*r(  5)
     &                      +( 1.000)*r(  6)+( 1.000)*r(  8)
     &                      +( 1.000)*r( 11)+( 1.000)*r( 14)
     &                      +(-1.000)*r( 14)+( 1.000)*r( 25)
     &                      +( 1.000)*r( 32)+( 1.000)*r( 69)
     &                      +( 1.000)*r( 79)+( 1.000)*r( 90)
     &                      +( 1.000)*r(102)+( 1.000)*r(115)
     &                      +( 1.000)*r(117)+( 1.000)*r(120)
          jac(iO3  ,iNO2 )= +( 1.000)*r(  8)
          jac(iPAN ,iNO2 )= +(-1.000)*r( 69)
          jac(iPAN2,iNO2 )= +(-1.000)*r( 79)
          jac(iMPAN,iNO2 )= +(-1.000)*r(102)
          jac(iPBZN,iNO2 )= +(-1.000)*r( 90)
          jac(iNPHE,iNO2 )= +(-1.000)*r(117)
          jac(iRNO3,iNO2 )= +(-1.000)*r(115)
          jac(iHNO4,iNO2 )= +(-1.000)*r( 32)

          jac(iNO3 ,iO3  )= +(-1.000)*r(  8)
          jac(iOH  ,iO3  )= +( 1.000)*r( 30)+(-1.000)*r( 36)
     &                      +(-0.208)*r(162)+(-0.164)*r(167)
     &                      +(-0.285)*r(171)+(-0.500)*r(179)
     &                      +(-0.120)*r(186)+(-0.266)*r(190)
     &                      +(-0.567)*r(194)+(-0.155)*r(205)
     &                      +(-0.378)*r(209)+(-0.099)*r(215)
          jac(iHO2 ,iO3  )= +(-1.000)*r( 30)+( 1.000)*r( 36)
     &                      +(-0.008)*r(162)+(-0.064)*r(167)
     &                      +(-0.400)*r(171)+(-1.500)*r(179)
     &                      +(-0.120)*r(186)+(-0.033)*r(194)
     &                      +(-0.056)*r(205)+(-0.003)*r(209)
     &                      +(-0.099)*r(215)
          jac(iRO2R,iO3  )= +(-0.100)*r(162)+(-0.050)*r(167)
     &                      +(-0.048)*r(171)+(-0.066)*r(190)
     &                      +(-0.031)*r(194)+(-0.022)*r(205)
     &                      +(-0.033)*r(209)
          jac(iR2O2,iO3  )= +(-0.126)*r(190)+(-0.729)*r(194)
     &                      +(-0.137)*r(209)
          jac(iRO2N,iO3  )= +(-0.008)*r(190)+(-0.180)*r(194)
     &                      +(-0.001)*r(205)+(-0.002)*r(209)
          jac(iCCO3,iO3  )= +(-0.123)*r(194)+(-0.137)*r(209)
          jac(iRCO3,iO3  )= +(-0.100)*r(162)+(-0.050)*r(167)
     &                      +(-0.048)*r(171)+(-0.201)*r(194)
     &                      +(-0.006)*r(209)
          jac(iMCO3,iO3  )= +(-0.192)*r(190)
          jac(iCXO2,iO3  )= +(-0.076)*r(205)+(-0.197)*r(209)
          jac(iNO  ,iO3  )= +( 1.000)*r(  7)
          jac(iNO2 ,iO3  )= +(-1.000)*r(  7)+( 1.000)*r(  8)
          jac(iO3  ,iO3  )= +( 1.000)*r(  3)+( 1.000)*r(  7)
     &                      +( 1.000)*r(  8)+( 1.000)*r( 17)
     &                      +( 1.000)*r( 18)+( 1.000)*r( 30)
     &                      +( 1.000)*r( 36)+( 1.000)*r(162)
     &                      +( 1.000)*r(167)+( 1.000)*r(171)
     &                      +( 1.000)*r(179)+( 1.000)*r(186)
     &                      +( 1.000)*r(190)+( 1.000)*r(194)
     &                      +( 1.000)*r(205)+( 1.000)*r(209)
     &                      +( 1.000)*r(215)

          jac(iCCO3,iPAN )= +(-1.000)*r( 70)
          jac(iNO2 ,iPAN )= +(-1.000)*r( 70)
          jac(iPAN ,iPAN )= +( 1.000)*r( 70)

          jac(iRCO3,iPAN2)= +(-1.000)*r( 80)
          jac(iNO2 ,iPAN2)= +(-1.000)*r( 80)
          jac(iPAN2,iPAN2)= +( 1.000)*r( 80)

          jac(iMCO3,iMPAN)= +(-1.000)*r(103)
          jac(iNO2 ,iMPAN)= +(-1.000)*r(103)
          jac(iMPAN,iMPAN)= +( 1.000)*r(103)

          jac(iBZCO,iPBZN)= +(-1.000)*r( 91)
          jac(iNO2 ,iPBZN)= +(-1.000)*r( 91)
          jac(iPBZN,iPBZN)= +( 1.000)*r( 91)

          jac(iNO3 ,iNPHE)= +( 1.000)*r(157)
          jac(iBZNO,iNPHE)= +(-1.000)*r(157)
          jac(iNPHE,iNPHE)= +( 1.000)*r(157)

          jac(iNO3 ,iCRES)= +( 1.000)*r(156)
          jac(iOH  ,iCRES)= +( 1.000)*r(155)
          jac(iRO2R,iCRES)= +(-0.760)*r(155)
          jac(iBZO ,iCRES)= +(-0.240)*r(155)+(-1.000)*r(156)
          jac(iCRES,iCRES)= +( 1.000)*r(155)+( 1.000)*r(156)

          jac(iOH  ,iDCB2)= +( 1.000)*r(180)
          jac(iHO2 ,iDCB2)= +(-0.500)*r(181)
          jac(iRO2R,iDCB2)= +(-1.000)*r(181)
          jac(iR2O2,iDCB2)= +(-1.000)*r(180)+(-1.000)*r(181)
          jac(iCCO3,iDCB2)= +(-1.000)*r(180)+(-0.500)*r(181)
          jac(iDCB2,iDCB2)= +( 1.000)*r(180)+( 1.000)*r(181)

          jac(iOH  ,iDCB3)= +( 1.000)*r(182)
          jac(iHO2 ,iDCB3)= +(-0.500)*r(183)
          jac(iRO2R,iDCB3)= +(-1.000)*r(183)
          jac(iR2O2,iDCB3)= +(-1.000)*r(182)+(-1.000)*r(183)
          jac(iCCO3,iDCB3)= +(-1.000)*r(182)+(-0.500)*r(183)
          jac(iDCB3,iDCB3)= +( 1.000)*r(182)+( 1.000)*r(183)

          jac(iOH  ,iRNO3)= +( 1.000)*r(176)
          jac(iHO2 ,iRNO3)= +(-0.113)*r(176)+(-0.341)*r(177)
          jac(iRO2R,iRNO3)= +(-0.376)*r(176)+(-0.564)*r(177)
          jac(iR2O2,iRNO3)= +(-0.596)*r(176)+(-0.152)*r(177)
          jac(iRO2N,iRNO3)= +(-0.173)*r(176)+(-0.095)*r(177)
          jac(iNO2 ,iRNO3)= +(-0.338)*r(176)+(-1.000)*r(177)
          jac(iRNO3,iRNO3)= +( 1.000)*r(176)+(-0.310)*r(176)
     &                      +( 1.000)*r(177)

          jac(iNO3 ,iHNO4)= +(-0.390)*r( 34)
          jac(iOH  ,iHNO4)= +(-0.390)*r( 34)+( 1.000)*r( 35)
          jac(iHO2 ,iHNO4)= +(-1.000)*r( 33)+(-0.610)*r( 34)
          jac(iNO2 ,iHNO4)= +(-1.000)*r( 33)+(-0.610)*r( 34)
     &                      +(-1.000)*r( 35)
          jac(iHNO4,iHNO4)= +( 1.000)*r( 33)+( 1.000)*r( 34)
     &                      +( 1.000)*r( 35)
c
c --- Put the Jacobian in right form for LSODE
c
      do j = 1,neq(1)
        tmp = y(j)
        if( tmp.LT.1.0e-25 .AND. tmp.GT.-1.0e-25 ) then
          tmp = 1.0
        endif
        do i = 1,neq(1)
          jac(i,j) = -jac(i,j)/tmp
        enddo
      enddo
c
      return
      end
