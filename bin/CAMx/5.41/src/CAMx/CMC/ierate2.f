      subroutine ierate2(neq,t,y,rate,nr,r)
      implicit none
c
c----CAMx v5.41 121109
c
c     IERATE2 computes rates for IEH solver fast species
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c     Created by the CMC version 5.2
c
c     Routines Called:
c        IERXN2
c
c     Called by:
c        LSODE
c
      include "camx.prm"
      include "chmdat.inc"
      include "iehchem.inc"
      include "lsbox.inc"
c
      integer neq(3), ny, nr, nk, l
      real t, H2O, M, O2, CH4, H2, N2
      real y(neq(2)+6)
      real rate(neq(1))
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
      call ierxn2(y,ny,r,rrk,nk)
c
c --- Solve the steady state species
c
c
c   O1D
c
        Loss(iO1D  )= +( 1.000)*r( 10)+( 1.000)*r( 11)
c
        Gain(iO1D  )= +( 1.000)*r(  9)
c
      if (loss(iO1D).gt.1.0e-25.or.loss(iO1D).lt.-1.0e-25) then
        y(iO1D) = gain(iO1D)/loss(iO1D)*y(iO1D)
      else
        y(iO1D) = 0.0
      endif
      r( 10) = rrk( 10)*y(iO1D)*M
      r( 11) = rrk( 11)*y(iO1D)*H2O
c
c     O
c
        Loss(iO    )= +( 1.000)*r(  2)+( 1.000)*r(  4)+( 1.000)*r(  5)
     &                +( 1.000)*r(  6)+( 1.000)*r(  7)+( 1.000)*r( 14)
     &                +( 1.000)*r( 15)+( 1.000)*r( 23)+( 1.000)*r( 31)
     &                +( 1.000)*r( 99)+( 1.000)*r(105)+( 1.000)*r(109)
     &                +( 1.000)*r(137)+( 1.000)*r(141)+( 1.000)*r(145)
     &                +( 1.000)*r(150)+( 1.000)*r(171)
c
        Gain(iO    )= +( 1.000)*r(  1)+( 1.000)*r(  8)+( 1.000)*r( 10)
     &                +( 1.000)*r( 16)+( 1.000)*r( 27)
c
      if (loss(iO).gt.1.0e-25.or.loss(iO).lt.-1.0e-25) then
        y(iO) = gain(iO)/loss(iO)*y(iO)
      else
        y(iO) = 0.0
      endif
      r(  2) = rrk(  2)*y(iO)*O2*M
      r(  4) = rrk(  4)*y(iO)*y(iNO)*M
      r(  5) = rrk(  5)*y(iO)*y(iNO2)
      r(  6) = rrk(  6)*y(iO)*y(iNO2)
      r(  7) = rrk(  7)*y(iO)*y(iO3)
      r( 14) = rrk( 14)*y(iOH)*y(iO)
      r( 15) = rrk( 15)*y(iHO2)*y(iO)
      r( 23) = rrk( 23)*y(iH2O2)*y(iO)
      r( 31) = rrk( 31)*y(iNO3)*y(iO)
      r( 99) = rrk( 99)*y(iFORM)*y(iO)
      r(105) = rrk(105)*y(iALD2)*y(iO)
      r(109) = rrk(109)*y(iALDX)*y(iO)
      r(137) = rrk(137)*y(iETH)*y(iO)
      r(141) = rrk(141)*y(iOLE)*y(iO)
      r(145) = rrk(145)*y(iIOLE)*y(iO)
      r(150) = rrk(150)*y(iISOP)*y(iO)
      r(171) = rrk(171)*y(iTERP)*y(iO)
c
c --- Calculate the species rates
c
c
c  BZO2  C2O3   CRO  CXO3  EPX2  HCO3   HO2  ISO2  MEO2  N2O5
c    NO   NO2   NO3    O3    OH  OPAN  OPO3   PAN  PANX   PNA
c   RO2   ROR   TO2  XLO2   XO2  XO2H  XO2N
c
        Loss(iBZO2 )= +( 1.000)*r(176)+( 1.000)*r(177)+( 1.000)*r(178)
     &                +( 1.000)*r(179)
c
        Gain(iBZO2 )= +( 0.352)*r(175)
c
        Loss(iC2O3 )= +( 1.000)*r( 53)+( 1.000)*r( 54)+( 1.000)*r( 57)
     &                +( 1.000)*r( 58)+( 2.000)*r( 59)+( 1.000)*r( 60)
     &                +( 1.000)*r( 73)+( 1.000)*r( 77)+( 1.000)*r( 81)
     &                +( 1.000)*r( 85)+( 1.000)*r(153)+( 1.000)*r(168)
     &                +( 1.000)*r(177)+( 1.000)*r(182)+( 1.000)*r(188)
     &                +( 1.000)*r(211)
c
        Gain(iC2O3 )= +( 1.000)*r( 55)+( 0.600)*r( 56)+( 1.000)*r( 58)
     &                +( 1.000)*r( 95)+( 1.000)*r(105)+( 1.000)*r(106)
     &                +( 1.000)*r(107)+( 0.800)*r(113)+( 1.000)*r(115)
     &                +( 1.000)*r(119)+( 1.000)*r(120)+( 1.000)*r(121)
     &                +( 0.500)*r(128)+( 0.620)*r(129)+( 1.000)*r(130)
     &                +( 0.230)*r(158)+( 0.143)*r(159)+( 0.208)*r(161)
     &                +( 0.300)*r(197)+( 0.600)*r(199)+( 0.120)*r(203)
c
        Loss(iCRO  )= +( 1.000)*r(192)+( 1.000)*r(193)
c
        Gain(iCRO  )= +( 0.200)*r(190)+( 0.300)*r(191)+( 0.500)*r(194)
     &                +( 0.500)*r(195)+( 0.500)*r(205)+( 1.000)*r(206)
c
        Loss(iCXO3 )= +( 1.000)*r( 60)+( 1.000)*r( 61)+( 1.000)*r( 62)
     &                +( 1.000)*r( 65)+( 1.000)*r( 66)+( 2.000)*r( 67)
c
        Gain(iCXO3 )= +( 1.000)*r( 63)+( 0.600)*r( 64)+( 1.000)*r(109)
     &                +( 1.000)*r(110)+( 1.000)*r(111)+( 0.500)*r(128)
     &                +( 0.250)*r(150)+( 0.200)*r(156)+( 0.117)*r(158)
     &                +( 0.717)*r(160)+( 0.390)*r(173)+( 0.200)*r(207)
c
        Loss(iEPX2 )= +( 1.000)*r(166)+( 1.000)*r(167)+( 1.000)*r(168)
     &                +( 1.000)*r(169)
c
        Gain(iEPX2 )= +( 1.000)*r(165)
c
        Loss(iHCO3 )= +( 1.000)*r(102)+( 1.000)*r(103)+( 1.000)*r(104)
c
        Gain(iHCO3 )= +( 1.000)*r(101)
c
        Loss(iHO2  )= +( 1.000)*r( 13)+( 1.000)*r( 15)+( 1.000)*r( 18)
     &                +( 2.000)*r( 19)+( 2.000)*r( 20)+( 1.000)*r( 25)
     &                +( 1.000)*r( 33)+( 1.000)*r( 48)+( 1.000)*r( 57)
     &                +( 1.000)*r( 65)+( 1.000)*r( 69)+( 1.000)*r( 72)
     &                +( 1.000)*r( 76)+( 1.000)*r( 80)+( 1.000)*r( 84)
     &                +( 1.000)*r(101)+( 1.000)*r(104)+( 1.000)*r(152)
     &                +( 1.000)*r(166)+( 1.000)*r(178)+( 1.000)*r(183)
     &                +( 1.000)*r(187)+( 1.000)*r(193)+( 1.000)*r(210)
c
        Gain(iHO2  )= +( 1.000)*r( 12)+( 1.000)*r( 14)+( 1.000)*r( 22)
     &                +( 1.000)*r( 23)+( 1.000)*r( 32)+( 1.000)*r( 49)
     &                +( 0.590)*r( 50)+( 1.000)*r( 52)+( 1.000)*r( 69)
     &                +( 1.000)*r( 71)+( 0.900)*r( 73)+( 0.370)*r( 74)
     &                +( 1.000)*r( 75)+( 0.800)*r( 77)+( 0.600)*r( 78)
     &                +( 0.800)*r( 85)+( 1.000)*r( 90)+( 1.000)*r( 93)
     &                +( 1.000)*r( 96)+( 2.000)*r( 97)+( 1.000)*r( 99)
     &                +( 1.000)*r(100)+( 1.000)*r(102)+( 1.000)*r(103)
     &                +( 0.200)*r(104)+( 1.000)*r(108)+( 1.000)*r(112)
        Gain(iHO2  ) = Gain(iHO2  )
     &                +( 0.200)*r(113)+( 1.400)*r(114)+( 1.000)*r(116)
     &                +( 2.000)*r(117)+( 1.000)*r(118)+( 1.000)*r(119)
     &                +( 1.000)*r(122)+( 1.000)*r(123)+( 1.000)*r(126)
     &                +( 0.900)*r(127)+( 1.000)*r(134)+( 0.300)*r(136)
     &                +( 1.000)*r(137)+( 0.160)*r(139)+( 0.100)*r(141)
     &                +( 0.080)*r(143)+( 0.250)*r(150)+( 0.818)*r(151)
     &                +( 0.120)*r(152)+( 0.728)*r(153)+( 0.728)*r(154)
     &                +( 1.000)*r(155)+( 0.066)*r(156)+( 0.137)*r(158)
        Gain(iHO2  ) = Gain(iHO2  )
     &                +( 0.398)*r(159)+( 0.760)*r(161)+( 1.000)*r(163)
     &                +( 0.825)*r(166)+( 0.825)*r(167)+( 0.660)*r(168)
     &                +( 0.825)*r(169)+( 0.530)*r(175)+( 0.918)*r(176)
     &                +( 1.000)*r(177)+( 1.000)*r(179)+( 0.180)*r(180)
     &                +( 0.860)*r(181)+( 1.000)*r(182)+( 1.000)*r(184)
     &                +( 0.155)*r(185)+( 0.860)*r(186)+( 1.000)*r(188)
     &                +( 1.000)*r(189)+( 1.000)*r(190)+( 1.000)*r(196)
     &                +( 0.700)*r(197)+( 1.000)*r(201)+( 0.560)*r(203)
        Gain(iHO2  ) = Gain(iHO2  )
     &                +( 0.200)*r(205)+( 0.800)*r(207)
c
        Loss(iISO2 )= +( 1.000)*r(151)+( 1.000)*r(152)+( 1.000)*r(153)
     &                +( 1.000)*r(154)+( 1.000)*r(155)
c
        Gain(iISO2 )= +( 1.000)*r(149)+( 0.067)*r(162)
c
        Loss(iMEO2 )= +( 1.000)*r( 71)+( 1.000)*r( 72)+( 1.000)*r( 73)
     &                +( 1.000)*r( 74)
c
        Gain(iMEO2 )= +( 1.000)*r( 53)+( 0.400)*r( 56)+( 0.440)*r( 57)
     &                +( 2.000)*r( 59)+( 1.000)*r( 60)+( 0.900)*r( 73)
     &                +( 0.800)*r( 77)+( 0.800)*r( 81)+( 0.800)*r( 85)
     &                +( 0.600)*r( 87)+( 1.000)*r( 88)+( 1.000)*r( 94)
     &                +( 1.000)*r(108)+( 1.000)*r(124)+( 0.500)*r(128)
     &                +( 1.380)*r(129)+( 0.800)*r(153)+( 0.273)*r(158)
     &                +( 0.340)*r(161)+( 0.800)*r(168)+( 1.000)*r(177)
     &                +( 1.000)*r(182)+( 1.000)*r(188)+( 1.000)*r(211)
c
        Loss(iN2O5 )= +( 1.000)*r( 37)+( 1.000)*r( 38)+( 1.000)*r( 39)
c
        Gain(iN2O5 )= +( 1.000)*r( 36)
c
        Loss(iNO   )= +( 1.000)*r(  3)+( 1.000)*r(  4)+( 2.000)*r( 24)
     &                +( 1.000)*r( 25)+( 1.000)*r( 29)+( 1.000)*r( 40)
     &                +( 1.000)*r( 41)+( 1.000)*r( 53)+( 1.000)*r( 61)
     &                +( 1.000)*r( 68)+( 1.000)*r( 71)+( 1.000)*r( 75)
     &                +( 1.000)*r( 79)+( 1.000)*r( 83)+( 1.000)*r(103)
     &                +( 1.000)*r(151)+( 1.000)*r(167)+( 1.000)*r(176)
     &                +( 1.000)*r(181)+( 1.000)*r(186)+( 1.000)*r(207)
c
        Gain(iNO   )= +( 1.000)*r(  1)+( 1.000)*r(  5)+( 1.000)*r( 28)
     &                +( 1.000)*r( 30)+( 1.000)*r( 42)+( 1.000)*r( 43)
     &                +( 1.000)*r( 68)
c
        Loss(iNO2  )= +( 1.000)*r(  1)+( 1.000)*r(  5)+( 1.000)*r(  6)
     &                +( 1.000)*r( 26)+( 1.000)*r( 30)+( 1.000)*r( 36)
     &                +( 1.000)*r( 41)+( 1.000)*r( 45)+( 1.000)*r( 48)
     &                +( 1.000)*r( 54)+( 1.000)*r( 62)+( 1.000)*r(135)
     &                +( 1.000)*r(192)+( 1.000)*r(208)
c
        Gain(iNO2  )= +( 1.000)*r(  3)+( 1.000)*r(  4)+( 2.000)*r( 24)
     &                +( 1.000)*r( 25)+( 1.000)*r( 27)+( 2.000)*r( 29)
     &                +( 1.000)*r( 30)+( 1.000)*r( 31)+( 1.000)*r( 32)
     &                +( 1.000)*r( 33)+( 1.000)*r( 34)+( 2.000)*r( 35)
     &                +( 1.000)*r( 37)+( 1.000)*r( 38)+( 1.000)*r( 42)
     &                +( 1.000)*r( 44)+( 1.000)*r( 47)+( 1.000)*r( 49)
     &                +( 0.590)*r( 50)+( 1.000)*r( 51)+( 1.000)*r( 53)
     &                +( 1.000)*r( 55)+( 0.600)*r( 56)+( 1.000)*r( 61)
     &                +( 1.000)*r( 63)+( 0.600)*r( 64)+( 1.000)*r( 71)
        Gain(iNO2  ) = Gain(iNO2  )
     &                +( 1.000)*r( 75)+( 1.000)*r( 79)+( 1.000)*r( 91)
     &                +( 1.000)*r(103)+( 0.500)*r(140)+( 0.500)*r(144)
     &                +( 0.500)*r(148)+( 0.900)*r(151)+( 0.350)*r(157)
     &                +( 0.142)*r(160)+( 1.000)*r(167)+( 0.444)*r(170)
     &                +( 0.470)*r(174)+( 0.918)*r(176)+( 0.860)*r(181)
     &                +( 0.860)*r(186)+( 0.500)*r(200)+( 1.000)*r(207)
     &                +( 1.000)*r(209)+( 1.000)*r(213)
c
        Loss(iNO3  )= +( 1.000)*r( 27)+( 1.000)*r( 28)+( 1.000)*r( 29)
     &                +( 1.000)*r( 30)+( 1.000)*r( 31)+( 1.000)*r( 32)
     &                +( 1.000)*r( 33)+( 1.000)*r( 34)+( 2.000)*r( 35)
     &                +( 1.000)*r( 36)+( 1.000)*r(100)+( 1.000)*r(107)
     &                +( 1.000)*r(111)+( 1.000)*r(115)+( 1.000)*r(118)
     &                +( 1.000)*r(120)+( 1.000)*r(140)+( 1.000)*r(144)
     &                +( 1.000)*r(148)+( 1.000)*r(157)+( 1.000)*r(160)
     &                +( 1.000)*r(164)+( 1.000)*r(174)+( 1.000)*r(191)
     &                +( 1.000)*r(195)+( 1.000)*r(200)+( 1.000)*r(204)
        Loss(iNO3  ) = Loss(iNO3  )
     &                +( 1.000)*r(206)
c
        Gain(iNO3  )= +( 1.000)*r(  6)+( 1.000)*r( 26)+( 1.000)*r( 37)
     &                +( 1.000)*r( 38)+( 1.000)*r( 46)+( 0.410)*r( 50)
     &                +( 0.400)*r( 56)+( 0.400)*r( 64)+( 0.185)*r(170)
c
        Loss(iO3   )= +( 1.000)*r(  3)+( 1.000)*r(  7)+( 1.000)*r(  8)
     &                +( 1.000)*r(  9)+( 1.000)*r( 12)+( 1.000)*r( 13)
     &                +( 1.000)*r( 26)+( 1.000)*r( 34)+( 1.000)*r(139)
     &                +( 1.000)*r(143)+( 1.000)*r(147)+( 1.000)*r(156)
     &                +( 1.000)*r(159)+( 1.000)*r(173)+( 1.000)*r(199)
     &                +( 1.000)*r(203)
c
        Gain(iO3   )= +( 1.000)*r(  2)+( 0.150)*r( 57)+( 0.150)*r( 65)
     &                +( 0.150)*r(210)
c
        Loss(iOH   )= +( 1.000)*r( 12)+( 1.000)*r( 14)+( 2.000)*r( 16)
     &                +( 2.000)*r( 17)+( 1.000)*r( 18)+( 1.000)*r( 22)
     &                +( 1.000)*r( 32)+( 1.000)*r( 40)+( 1.000)*r( 44)
     &                +( 1.000)*r( 45)+( 1.000)*r( 46)+( 1.000)*r( 51)
     &                +( 1.000)*r( 52)+( 1.000)*r( 87)+( 1.000)*r( 89)
     &                +( 1.000)*r( 91)+( 1.000)*r( 93)+( 1.000)*r( 94)
     &                +( 1.000)*r( 95)+( 1.000)*r( 96)+( 1.000)*r(106)
     &                +( 1.000)*r(110)+( 1.000)*r(113)+( 1.000)*r(116)
     &                +( 1.000)*r(121)+( 1.000)*r(122)+( 1.000)*r(123)
        Loss(iOH   ) = Loss(iOH   )
     &                +( 1.000)*r(124)+( 1.000)*r(125)+( 1.000)*r(126)
     &                +( 1.000)*r(127)+( 1.000)*r(130)+( 1.000)*r(131)
     &                +( 1.000)*r(132)+( 1.000)*r(136)+( 1.000)*r(138)
     &                +( 1.000)*r(142)+( 1.000)*r(146)+( 1.000)*r(149)
     &                +( 1.000)*r(158)+( 1.000)*r(162)+( 1.000)*r(165)
     &                +( 1.000)*r(170)+( 1.000)*r(172)+( 1.000)*r(175)
     &                +( 1.000)*r(180)+( 1.000)*r(185)+( 1.000)*r(190)
     &                +( 1.000)*r(194)+( 1.000)*r(198)+( 1.000)*r(202)
        Loss(iOH   ) = Loss(iOH   )
     &                +( 1.000)*r(205)+( 1.000)*r(213)
c
        Gain(iOH   )= +( 2.000)*r( 11)+( 1.000)*r( 13)+( 1.000)*r( 15)
     &                +( 2.000)*r( 21)+( 1.000)*r( 23)+( 1.000)*r( 25)
     &                +( 1.000)*r( 33)+( 1.000)*r( 43)+( 1.000)*r( 47)
     &                +( 0.410)*r( 50)+( 0.440)*r( 57)+( 0.440)*r( 65)
     &                +( 0.400)*r( 87)+( 1.000)*r( 88)+( 0.400)*r( 89)
     &                +( 1.000)*r( 90)+( 1.000)*r( 99)+( 0.200)*r(104)
     &                +( 1.000)*r(105)+( 1.000)*r(109)+( 0.190)*r(114)
     &                +( 0.700)*r(136)+( 0.300)*r(137)+( 0.160)*r(139)
     &                +( 0.100)*r(141)+( 0.334)*r(143)+( 0.500)*r(147)
        Gain(iOH   ) = Gain(iOH   )
     &                +( 0.120)*r(152)+( 0.266)*r(156)+( 0.461)*r(159)
     &                +( 0.933)*r(162)+( 1.000)*r(163)+( 1.125)*r(166)
     &                +( 0.125)*r(167)+( 0.100)*r(168)+( 0.125)*r(169)
     &                +( 0.570)*r(173)+( 0.118)*r(175)+( 0.100)*r(180)
     &                +( 0.244)*r(185)+( 0.500)*r(199)+( 0.500)*r(203)
     &                +( 0.440)*r(210)
c
        Loss(iOPAN )= +( 1.000)*r(209)+( 1.000)*r(213)
c
        Gain(iOPAN )= +( 1.000)*r(208)
c
        Loss(iOPO3 )= +( 1.000)*r(207)+( 1.000)*r(208)+( 1.000)*r(210)
     &                +( 1.000)*r(211)+( 1.000)*r(212)
c
        Gain(iOPO3 )= +( 0.480)*r(191)+( 1.000)*r(201)+( 0.600)*r(202)
     &                +( 1.000)*r(204)+( 1.000)*r(209)
c
        Loss(iPAN  )= +( 1.000)*r( 55)+( 1.000)*r( 56)
c
        Gain(iPAN  )= +( 1.000)*r( 54)
c
        Loss(iPANX )= +( 1.000)*r( 63)+( 1.000)*r( 64)
c
        Gain(iPANX )= +( 1.000)*r( 62)
c
        Loss(iPNA  )= +( 1.000)*r( 49)+( 1.000)*r( 50)+( 1.000)*r( 51)
c
        Gain(iPNA  )= +( 1.000)*r( 48)
c
        Loss(iRO2  )= +( 1.000)*r( 58)+( 1.000)*r( 66)+( 1.000)*r( 68)
     &                +( 1.000)*r( 69)+( 2.000)*r( 70)+( 1.000)*r( 74)
     &                +( 1.000)*r( 78)+( 1.000)*r( 82)+( 1.000)*r( 86)
     &                +( 1.000)*r(154)+( 1.000)*r(169)+( 1.000)*r(179)
     &                +( 1.000)*r(184)+( 1.000)*r(189)+( 1.000)*r(212)
c
        Gain(iRO2  )= +( 1.000)*r( 53)+( 0.400)*r( 56)+( 0.440)*r( 57)
     &                +( 2.000)*r( 59)+( 2.000)*r( 60)+( 1.000)*r( 61)
     &                +( 0.400)*r( 64)+( 0.440)*r( 65)+( 0.800)*r( 66)
     &                +( 2.000)*r( 67)+( 0.900)*r( 73)+( 1.000)*r( 74)
     &                +( 0.800)*r( 77)+( 1.000)*r( 78)+( 0.800)*r( 81)
     &                +( 1.000)*r( 82)+( 0.800)*r( 85)+( 1.000)*r( 86)
     &                +( 0.600)*r( 87)+( 1.000)*r( 88)+( 0.600)*r( 89)
     &                +( 1.000)*r( 91)+( 1.000)*r( 92)+( 1.000)*r( 94)
     &                +( 1.000)*r(108)+( 1.000)*r(112)+( 0.110)*r(114)
        Gain(iRO2  ) = Gain(iRO2  )
     &                +( 0.200)*r(116)+( 0.500)*r(118)+( 1.000)*r(120)
     &                +( 1.000)*r(124)+( 1.000)*r(125)+( 0.100)*r(127)
     &                +( 1.000)*r(128)+( 1.380)*r(129)+( 1.000)*r(130)
     &                +( 1.000)*r(131)+( 1.000)*r(132)+( 0.980)*r(133)
     &                +( 0.700)*r(137)+( 1.000)*r(138)+( 1.000)*r(140)
     &                +( 0.210)*r(141)+( 1.195)*r(142)+( 0.150)*r(143)
     &                +( 1.000)*r(144)+( 0.100)*r(145)+( 1.000)*r(146)
     &                +( 0.300)*r(147)+( 1.000)*r(148)+( 1.000)*r(149)
        Gain(iRO2  ) = Gain(iRO2  )
     &                +( 0.250)*r(150)+( 0.082)*r(151)+( 0.872)*r(153)
     &                +( 0.072)*r(154)+( 0.200)*r(156)+( 1.000)*r(157)
     &                +( 1.088)*r(158)+( 0.284)*r(160)+( 0.840)*r(161)
     &                +( 0.067)*r(162)+( 1.000)*r(165)+( 0.800)*r(168)
     &                +( 1.000)*r(169)+( 1.000)*r(170)+( 1.500)*r(172)
     &                +( 0.940)*r(173)+( 1.280)*r(174)+( 0.352)*r(175)
     &                +( 1.000)*r(177)+( 1.000)*r(179)+( 0.720)*r(180)
     &                +( 1.000)*r(182)+( 1.000)*r(184)+( 0.602)*r(185)
        Gain(iRO2  ) = Gain(iRO2  )
     &                +( 1.000)*r(188)+( 1.000)*r(189)+( 0.020)*r(190)
     &                +( 0.700)*r(191)+( 2.000)*r(198)+( 0.300)*r(199)
     &                +( 1.000)*r(200)+( 0.400)*r(202)+( 0.440)*r(210)
     &                +( 2.000)*r(211)+( 1.800)*r(212)
c
        Loss(iROR  )= +( 1.000)*r(133)+( 1.000)*r(134)+( 1.000)*r(135)
c
        Gain(iROR  )= +( 0.760)*r(132)+( 0.020)*r(133)
c
        Loss(iTO2  )= +( 1.000)*r(181)+( 1.000)*r(182)+( 1.000)*r(183)
     &                +( 1.000)*r(184)
c
        Gain(iTO2  )= +( 0.650)*r(180)
c
        Loss(iXLO2 )= +( 1.000)*r(186)+( 1.000)*r(187)+( 1.000)*r(188)
     &                +( 1.000)*r(189)
c
        Gain(iXLO2 )= +( 0.544)*r(185)
c
        Loss(iXO2  )= +( 1.000)*r( 79)+( 1.000)*r( 80)+( 1.000)*r( 81)
     &                +( 1.000)*r( 82)
c
        Gain(iXO2  )= +( 1.000)*r( 91)+( 0.200)*r(116)+( 0.500)*r(118)
     &                +( 1.000)*r(120)+( 1.000)*r(130)+( 0.760)*r(132)
     &                +( 0.500)*r(140)+( 0.195)*r(142)+( 0.480)*r(144)
     &                +( 0.480)*r(148)+( 0.250)*r(150)+( 0.200)*r(156)
     &                +( 0.330)*r(157)+( 0.521)*r(158)+( 0.142)*r(160)
     &                +( 0.160)*r(161)+( 0.630)*r(170)+( 0.500)*r(172)
     &                +( 0.690)*r(173)+( 0.750)*r(174)+( 0.480)*r(191)
     &                +( 0.450)*r(200)+( 1.000)*r(211)
c
        Loss(iXO2H )= +( 1.000)*r( 75)+( 1.000)*r( 76)+( 1.000)*r( 77)
     &                +( 1.000)*r( 78)
c
        Gain(iXO2H )= +( 1.000)*r( 60)+( 1.000)*r( 61)+( 0.400)*r( 64)
     &                +( 0.440)*r( 65)+( 0.800)*r( 66)+( 2.000)*r( 67)
     &                +( 0.540)*r( 89)+( 1.000)*r( 92)+( 1.000)*r(112)
     &                +( 0.110)*r(114)+( 0.991)*r(125)+( 0.100)*r(127)
     &                +( 0.500)*r(128)+( 0.970)*r(131)+( 0.110)*r(132)
     &                +( 0.940)*r(133)+( 0.700)*r(137)+( 1.000)*r(138)
     &                +( 0.500)*r(140)+( 0.200)*r(141)+( 0.976)*r(142)
     &                +( 0.150)*r(143)+( 0.480)*r(144)+( 0.100)*r(145)
     &                +( 1.000)*r(146)+( 0.300)*r(147)+( 0.480)*r(148)
        Gain(iXO2H ) = Gain(iXO2H )
     &                +( 0.082)*r(151)+( 0.072)*r(153)+( 0.072)*r(154)
     &                +( 0.640)*r(157)+( 0.238)*r(158)+( 0.142)*r(160)
     &                +( 0.340)*r(161)+( 0.370)*r(170)+( 0.750)*r(172)
     &                +( 0.070)*r(173)+( 0.280)*r(174)+( 0.070)*r(180)
     &                +( 0.058)*r(185)+( 0.120)*r(191)+( 1.000)*r(197)
     &                +( 2.000)*r(198)+( 0.300)*r(199)+( 0.450)*r(200)
     &                +( 0.400)*r(202)+( 0.440)*r(210)+( 0.800)*r(212)
c
        Loss(iXO2N )= +( 1.000)*r( 83)+( 1.000)*r( 84)+( 1.000)*r( 85)
     &                +( 1.000)*r( 86)
c
        Gain(iXO2N )= +( 0.060)*r( 89)+( 0.009)*r(125)+( 0.030)*r(131)
     &                +( 0.130)*r(132)+( 0.040)*r(133)+( 0.010)*r(141)
     &                +( 0.024)*r(142)+( 0.040)*r(144)+( 0.040)*r(148)
     &                +( 0.030)*r(157)+( 0.056)*r(158)+( 0.250)*r(172)
     &                +( 0.180)*r(173)+( 0.250)*r(174)+( 0.020)*r(190)
     &                +( 0.100)*r(191)+( 0.100)*r(200)
c
c
      do l=1,neq(1)
        rate(l) = gain(l) -loss(l)
      enddo
c
      return
      end
