      subroutine ieslow5(r,rate,gain,loss,nr,ny,n1,n2)
c
c----CAMx v4.42 070603
c
c     IESLOW5 computes reaction rates for slow state species
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Routines Called:
c        none
c
c     Called by:
c        IEHSOLV
c
      implicit none
      include "camx.prm"
      include "iehchem.com"
c
      integer ny, nr, n1, n2, l
      real loss(ny+1),gain(ny+1),rate(ny+1),r(nr)
c
c --- Entry point
c
      do l=n1,n2
        Loss(l) = 0.0
        Gain(l) = 0.0
      enddo
c
c --- Calculate the species rates
c
c
c  HONO  HNO3    XN  HO2H  HCHO  CCHO  RCHO  BALD  BACL  PROD
c  DCB1  PHEN  ISOP  ISPD   MVK  METH  MGLY   GLY  TERP  HC2H
c  CO2H  CO3H  RC2H  RC3H  ACET   MEK  MEOH  COOH  ROOH    CO
c  ETHE  ALK1  ALK2  ALK3  ALK4  ALK5  ARO1  ARO2  OLE1  OLE2
c   SO2  SULF  ETOH  MTBE  MBUT
c
        Loss(iHONO )= +( 1.000)*r( 22)+( 1.000)*r( 23)+( 1.000)*r( 24)
        Gain(iHONO )= +( 1.000)*r( 21)
        Loss(iHNO3 )= +( 1.000)*r( 27)+( 1.000)*r( 28)
        Gain(iHNO3 )= +( 2.000)*r( 13)+( 1.000)*r( 25)+( 0.200)*r( 39)
     &                +( 1.000)*r(129)+( 1.000)*r(132)+( 1.000)*r(135)
     &                +( 1.000)*r(148)+( 1.000)*r(151)+( 1.000)*r(154)
     &                +( 1.000)*r(156)+( 1.000)*r(157)+( 1.000)*r(160)
     &                +( 0.500)*r(163)+( 0.150)*r(172)

        Gain(iXN   )= +( 2.000)*r(120)+( 0.500)*r(163)+( 0.278)*r(172)
     &                +( 0.352)*r(176)+( 1.000)*r(187)+( 0.250)*r(195)
     &                +( 0.489)*r(206)+( 0.288)*r(210)
        Loss(iHO2H )= +( 1.000)*r( 41)+( 1.000)*r( 42)
        Gain(iHO2H )= +( 1.000)*r( 37)+( 1.000)*r( 38)
        Loss(iHCHO )= +( 1.000)*r(123)+( 1.000)*r(124)+( 1.000)*r(125)
     &                +( 1.000)*r(126)+( 1.000)*r(129)
        Gain(iHCHO )= +( 1.000)*r( 46)+( 1.000)*r( 48)+( 1.000)*r( 49)
     &                +( 2.000)*r( 50)+( 0.750)*r( 54)+( 0.750)*r( 64)
     &                +( 1.000)*r( 74)+( 1.000)*r( 84)+( 1.000)*r( 95)
     &                +( 1.000)*r(104)+( 1.000)*r(106)+( 1.000)*r(107)
     &                +( 1.000)*r(111)+( 1.000)*r(112)+( 1.000)*r(113)
     &                +( 2.000)*r(114)+( 1.000)*r(127)+( 1.000)*r(136)
     &                +( 0.115)*r(138)+( 1.000)*r(140)+( 0.350)*r(141)
     &                +( 1.000)*r(142)+( 1.000)*r(146)+( 0.084)*r(161)
     &                +( 0.200)*r(162)+( 0.670)*r(165)+( 0.300)*r(166)
        Gain(iHCHO ) = Gain(iHCHO )
     &                +( 0.100)*r(167)+( 0.055)*r(170)+( 0.125)*r(171)
     &                +( 0.227)*r(172)+( 0.300)*r(173)+( 0.213)*r(174)
     &                +( 0.506)*r(175)+( 0.010)*r(176)+( 0.134)*r(177)
     &                +( 1.610)*r(185)+( 1.000)*r(186)+( 0.191)*r(188)
     &                +( 0.624)*r(189)+( 0.592)*r(190)+( 0.240)*r(192)
     &                +( 0.276)*r(193)+( 0.235)*r(194)+( 0.039)*r(198)
     &                +( 0.026)*r(199)+( 0.024)*r(200)+( 0.026)*r(201)
     &                +( 0.732)*r(204)+( 0.500)*r(205)+( 0.244)*r(208)
        Gain(iHCHO ) = Gain(iHCHO )
     &                +( 0.269)*r(209)+( 0.079)*r(210)+( 0.081)*r(212)
     &                +( 0.234)*r(213)+( 0.311)*r(214)+( 0.300)*r(215)
        Loss(iCCHO )= +( 1.000)*r(130)+( 1.000)*r(131)+( 1.000)*r(132)
        Gain(iCCHO )= +( 1.000)*r( 81)+( 1.000)*r( 83)+( 1.000)*r( 88)
     &                +( 2.000)*r( 89)+( 1.000)*r(100)+( 1.000)*r(112)
     &                +( 0.034)*r(133)+( 1.000)*r(134)+( 0.482)*r(138)
     &                +( 1.000)*r(139)+( 0.129)*r(170)+( 0.047)*r(171)
     &                +( 0.467)*r(173)+( 0.084)*r(174)+( 0.246)*r(175)
     &                +( 0.439)*r(176)+( 0.431)*r(177)+( 0.195)*r(185)
     &                +( 0.250)*r(188)+( 1.000)*r(197)+( 0.445)*r(199)
     &                +( 0.455)*r(200)+( 0.099)*r(201)+( 0.294)*r(204)
     &                +( 0.154)*r(205)+( 0.009)*r(206)+( 0.732)*r(208)
        Gain(iCCHO ) = Gain(iCCHO )
     &                +( 0.456)*r(209)+( 0.507)*r(210)+( 0.960)*r(212)
     &                +( 0.624)*r(214)
        Loss(iRCHO )= +( 1.000)*r(133)+( 1.000)*r(134)+( 1.000)*r(135)
        Gain(iRCHO )= +( 0.370)*r(138)+( 1.000)*r(143)+( 1.000)*r(144)
     &                +( 1.000)*r(164)+( 0.675)*r(166)+( 0.450)*r(168)
     &                +( 0.013)*r(170)+( 0.218)*r(172)+( 0.558)*r(174)
     &                +( 0.710)*r(175)+( 0.213)*r(176)+( 0.147)*r(177)
     &                +( 1.000)*r(178)+( 1.000)*r(180)+( 1.000)*r(182)
     &                +( 1.000)*r(187)+( 0.474)*r(193)+( 0.205)*r(194)
     &                +( 0.474)*r(195)+( 0.147)*r(196)+( 0.155)*r(198)
     &                +( 0.122)*r(199)+( 0.244)*r(200)+( 0.204)*r(201)
     &                +( 0.497)*r(204)+( 0.363)*r(205)+( 0.037)*r(206)
        Gain(iRCHO ) = Gain(iRCHO )
     &                +( 0.450)*r(207)+( 0.511)*r(208)+( 0.305)*r(209)
     &                +( 0.151)*r(210)+( 0.069)*r(211)+( 0.311)*r(214)
     &                +( 0.700)*r(215)+( 0.935)*r(216)+( 0.450)*r(217)
        Loss(iBALD )= +( 1.000)*r(158)+( 1.000)*r(159)+( 1.000)*r(160)
        Gain(iBALD )= +( 0.059)*r(202)+( 0.050)*r(203)+( 0.061)*r(208)
     &                +( 0.042)*r(209)+( 0.015)*r(210)
        Loss(iBACL )= +( 1.000)*r(152)
        Gain(iBACL )= +( 0.031)*r(194)+( 0.087)*r(203)
        Loss(iPROD )= +( 1.000)*r(174)+( 1.000)*r(175)
        Gain(iPROD )= +( 0.500)*r( 64)+( 0.500)*r( 66)+( 1.000)*r( 68)
     &                +( 1.000)*r( 77)+( 1.000)*r( 87)+( 1.000)*r( 98)
     &                +( 0.700)*r(169)+( 0.332)*r(170)+( 0.329)*r(174)
     &                +( 0.048)*r(176)+( 0.435)*r(177)+( 0.100)*r(190)
     &                +( 0.750)*r(192)+( 0.276)*r(193)+( 0.276)*r(194)
     &                +( 0.853)*r(196)+( 0.125)*r(200)+( 0.417)*r(201)
     &                +( 0.055)*r(202)+( 0.119)*r(204)+( 0.215)*r(205)
     &                +( 0.113)*r(207)+( 0.006)*r(209)+( 0.259)*r(211)
     &                +( 0.007)*r(213)
        Loss(iDCB1 )= +( 1.000)*r(178)+( 1.000)*r(179)
        Gain(iDCB1 )= +( 0.491)*r(202)+( 0.561)*r(203)
        Loss(iPHEN )= +( 1.000)*r(153)+( 1.000)*r(154)
        Gain(iPHEN )= +( 1.000)*r(118)+( 1.000)*r(119)+( 0.017)*r(202)
        Loss(iISOP )= +( 1.000)*r(189)+( 1.000)*r(190)+( 1.000)*r(191)
     &                +( 1.000)*r(192)

        Loss(iISPD )= +( 1.000)*r(170)+( 1.000)*r(171)+( 1.000)*r(172)
     &                +( 1.000)*r(173)
        Gain(iISPD )= +( 0.357)*r(189)+( 0.936)*r(191)+( 0.025)*r(208)
        Loss(iMVK  )= +( 1.000)*r(166)+( 1.000)*r(167)+( 1.000)*r(168)
     &                +( 1.000)*r(169)
        Gain(iMVK  )= +( 0.320)*r(189)+( 0.160)*r(190)+( 0.048)*r(210)
        Loss(iMETH )= +( 1.000)*r(161)+( 1.000)*r(162)+( 1.000)*r(163)
     &                +( 1.000)*r(164)+( 1.000)*r(165)
        Gain(iMETH )= +( 0.230)*r(189)+( 0.390)*r(190)+( 0.025)*r(208)
     &                +( 0.026)*r(209)+( 0.012)*r(211)
        Loss(iMGLY )= +( 1.000)*r(149)+( 1.000)*r(150)+( 1.000)*r(151)
        Gain(iMGLY )= +( 0.230)*r(155)+( 0.084)*r(161)+( 0.900)*r(162)
     &                +( 0.300)*r(166)+( 0.950)*r(167)+( 0.174)*r(170)
     &                +( 0.742)*r(171)+( 0.008)*r(172)+( 0.500)*r(181)
     &                +( 0.500)*r(183)+( 0.119)*r(202)+( 0.287)*r(203)
        Loss(iGLY  )= +( 1.000)*r(145)+( 1.000)*r(146)+( 1.000)*r(147)
     &                +( 1.000)*r(148)
        Gain(iGLY  )= +( 0.230)*r(153)+( 0.150)*r(170)+( 0.023)*r(171)
     &                +( 1.000)*r(179)+( 0.500)*r(181)+( 0.500)*r(183)
     &                +( 0.009)*r(188)+( 0.001)*r(194)+( 0.248)*r(198)
     &                +( 0.118)*r(202)+( 0.097)*r(203)
        Loss(iTERP )= +( 1.000)*r(193)+( 1.000)*r(194)+( 1.000)*r(195)
     &                +( 1.000)*r(196)


        Gain(iHC2H )= +( 1.000)*r(128)+( 0.333)*r(162)+( 0.351)*r(167)
     &                +( 0.100)*r(171)+( 0.370)*r(186)+( 0.204)*r(190)
     &                +( 0.103)*r(194)+( 0.121)*r(198)+( 0.185)*r(205)
     &                +( 0.073)*r(209)+( 0.259)*r(215)

        Gain(iCO2H )= +( 0.250)*r( 72)+( 1.000)*r( 74)+( 1.000)*r( 75)
     &                +( 1.000)*r( 77)+( 0.050)*r(205)+( 0.129)*r(209)

        Gain(iCO3H )= +( 0.750)*r( 72)

        Gain(iRC2H )= +( 0.250)*r( 82)+( 1.000)*r( 84)+( 1.000)*r( 85)
     &                +( 1.000)*r( 87)+( 0.250)*r( 93)+( 1.000)*r( 95)
     &                +( 1.000)*r( 96)+( 1.000)*r( 98)+( 0.250)*r(105)
     &                +( 1.000)*r(107)+( 1.000)*r(108)+( 2.000)*r(110)
     &                +( 0.372)*r(171)+( 0.150)*r(190)+( 0.189)*r(194)
     &                +( 0.119)*r(205)+( 0.303)*r(209)+( 0.285)*r(215)

        Gain(iRC3H )= +( 0.750)*r( 82)+( 0.750)*r( 93)+( 0.750)*r(105)
        Loss(iACET )= +( 1.000)*r(136)+( 1.000)*r(137)
        Gain(iACET )= +( 1.000)*r(116)+( 0.006)*r(176)+( 0.020)*r(177)
     &                +( 0.130)*r(194)+( 0.417)*r(198)+( 0.024)*r(199)
     &                +( 0.452)*r(200)+( 0.072)*r(201)+( 0.005)*r(204)
     &                +( 0.001)*r(205)+( 0.024)*r(206)+( 0.127)*r(208)
     &                +( 0.045)*r(209)+( 0.102)*r(210)+( 0.024)*r(213)
     &                +( 0.624)*r(214)+( 0.015)*r(215)+( 0.934)*r(216)
        Loss(iMEK  )= +( 1.000)*r(138)+( 1.000)*r(139)
        Gain(iMEK  )= +( 0.500)*r( 64)+( 1.000)*r( 65)+( 0.500)*r( 66)
     &                +( 1.000)*r( 68)+( 0.416)*r(161)+( 0.550)*r(168)
     &                +( 0.150)*r(170)+( 0.210)*r(171)+( 0.233)*r(173)
     &                +( 0.115)*r(174)+( 0.177)*r(176)+( 0.243)*r(177)
     &                +( 0.332)*r(199)+( 0.110)*r(200)+( 0.089)*r(201)
     &                +( 0.437)*r(207)+( 0.072)*r(208)+( 0.026)*r(209)
     &                +( 0.001)*r(210)+( 0.659)*r(211)+( 0.719)*r(213)
     &                +( 0.550)*r(217)
        Loss(iMEOH )= +( 1.000)*r(140)
        Gain(iMEOH )= +( 1.000)*r( 49)+( 0.250)*r( 54)+( 0.250)*r( 64)
        Loss(iCOOH )= +( 1.000)*r(141)+( 1.000)*r(142)
        Gain(iCOOH )= +( 1.000)*r( 47)
        Loss(iROOH )= +( 1.000)*r(143)+( 1.000)*r(144)
        Gain(iROOH )= +( 1.000)*r( 52)+( 1.000)*r( 63)
        Loss(iCO   )= +( 1.000)*r( 29)
        Gain(iCO   )= +( 1.000)*r(123)+( 1.000)*r(124)+( 1.000)*r(125)
     &                +( 1.000)*r(129)+( 1.000)*r(131)+( 0.034)*r(133)
     &                +( 1.000)*r(134)+( 2.000)*r(145)+( 1.000)*r(146)
     &                +( 1.260)*r(147)+( 1.260)*r(148)+( 1.000)*r(149)
     &                +( 1.000)*r(150)+( 1.000)*r(151)+( 0.416)*r(161)
     &                +( 0.450)*r(162)+( 0.500)*r(163)+( 0.670)*r(165)
     &                +( 0.475)*r(167)+( 0.700)*r(169)+( 0.336)*r(170)
     &                +( 0.498)*r(171)+( 0.572)*r(172)+( 1.233)*r(173)
     &                +( 1.000)*r(178)+( 1.500)*r(179)+( 1.000)*r(181)
        Gain(iCO   ) = Gain(iCO   )
     &                +( 1.000)*r(183)+( 0.500)*r(186)+( 0.491)*r(188)
     &                +( 0.275)*r(190)+( 0.157)*r(194)+( 0.160)*r(198)
     &                +( 0.002)*r(200)+( 0.345)*r(205)+( 0.265)*r(209)
     &                +( 0.012)*r(211)+( 0.365)*r(215)
        Loss(iETHE )= +( 1.000)*r(185)+( 1.000)*r(186)+( 1.000)*r(187)
     &                +( 1.000)*r(188)

        Loss(iALK1 )= +( 1.000)*r(197)

        Loss(iALK2 )= +( 1.000)*r(198)

        Loss(iALK3 )= +( 1.000)*r(199)

        Loss(iALK4 )= +( 1.000)*r(200)

        Loss(iALK5 )= +( 1.000)*r(201)

        Loss(iARO1 )= +( 1.000)*r(202)

        Loss(iARO2 )= +( 1.000)*r(203)

        Loss(iOLE1 )= +( 1.000)*r(204)+( 1.000)*r(205)+( 1.000)*r(206)
     &                +( 1.000)*r(207)

        Loss(iOLE2 )= +( 1.000)*r(208)+( 1.000)*r(209)+( 1.000)*r(210)
     &                +( 1.000)*r(211)

        Loss(iSO2  )= +( 1.000)*r( 44)


        Gain(iSULF )= +( 1.000)*r( 44)
        Loss(iETOH )= +( 1.000)*r(212)

        Loss(iMTBE )= +( 1.000)*r(213)

        Loss(iMBUT )= +( 1.000)*r(214)+( 1.000)*r(215)+( 1.000)*r(216)
     &                +( 1.000)*r(217)

c
      do l=n1,n2
        rate(l) = gain(l) -loss(l)
      enddo
c
      return
      end
