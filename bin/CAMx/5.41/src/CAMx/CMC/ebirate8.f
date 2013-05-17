      subroutine ebirate8(ny,nr,r,gain,loss)
      implicit none
c
c----CAMx v5.41 121109
c
c     EBIRATE8 computes species production and loss
c     for the EBI solver
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
c        ny   - dimension of gain and loss
c        nr   - dimension of r
c        r    - reaction rates (hr-1)
c        gain - species production (ppm/hr)
c        loss - species destruction (ppm/hr)
c
c --- Includes:
      include "camx.prm"
      include "chmdat.inc"
      include "ddmchm.inc"
c
c --- Arguments:
      integer ny, nr
      real    loss(ny+1), gain(ny+1)
      real    r(nr)
c
c --- Entry Point:
c
c
c-----Calculate the species rates
c
c
c   NO2    NO     O    O3   NO3  N2O5  HNO3   O1D    OH  HONO
c   HO2    CO  HNO4  HO2H   SO2  SULF  CXO2  HCHO  COOH  MEOH
c  RO2R  ROOH  R2O2  RO2N  RNO3   MEK  PROD  CCO3   PAN  CO3H
c  CO2H  RCO3  PAN2  CCHO  RC3H  RC2H  BZCO  PBZN   BZO  MCO3
c  MPAN  TBUO  ACET  NPHE  PHEN  BZNO    XN  HCO3  HC2H  RCHO
c   GLY  MGLY  BACL  CRES  BALD  METH   MVK  ISPD  DCB1  DCB2
c  DCB3  ETHE  ISOP  TERP  ALK1  ALK2  ALK3  ALK4  ALK5  ARO1
c  ARO2  OLE1  OLE2  ETOH  MTBE  MBUT   CL2    CL  HOCL   CLO
c  N3CL   HCL  FMCL  NTCL    SS     I    IO    HI   INO  INO2
c    I2  INO3   HOI   OIO  IXOY  HIO3  CH3I  NBUI  IBUO  IBAC
c
c
        Loss(lNO2  )= +( 1.000)*r(  1)+( 1.000)*r(  5)+( 1.000)*r(  6)
     &                +( 1.000)*r(  8)+( 1.000)*r( 11)+( 1.000)*r( 14)
     &                +( 1.000)*r( 25)+( 1.000)*r( 32)+( 1.000)*r( 69)
     &                +( 1.000)*r( 79)+( 1.000)*r( 90)+( 1.000)*r(102)
     &                +( 1.000)*r(115)+( 1.000)*r(117)+( 1.000)*r(120)
     &                +( 1.000)*r(224)+( 1.000)*r(259)+( 1.000)*r(265)
c
        Gain(lNO2  )= +( 1.000)*r(  4)+( 1.000)*r(  7)+( 2.000)*r(  9)
     &                +( 2.000)*r( 10)+( 1.000)*r( 12)+( 1.000)*r( 14)
     &                +( 1.000)*r( 16)+( 1.000)*r( 23)+( 1.000)*r( 24)
     &                +( 1.000)*r( 26)+( 1.000)*r( 28)+( 1.000)*r( 31)
     &                +( 1.000)*r( 33)+( 0.610)*r( 34)+( 1.000)*r( 35)
     &                +( 0.800)*r( 39)+( 2.000)*r( 40)+( 1.000)*r( 46)
     &                +( 1.000)*r( 48)+( 1.000)*r( 51)+( 1.000)*r( 53)
     &                +( 1.000)*r( 56)+( 1.000)*r( 58)+( 1.000)*r( 65)
     &                +( 1.000)*r( 70)+( 1.000)*r( 71)+( 1.000)*r( 73)
        Gain(lNO2  ) = Gain(lNO2  )
     &                +( 1.000)*r( 80)+( 1.000)*r( 81)+( 1.000)*r( 83)
     &                +( 1.000)*r( 91)+( 1.000)*r( 92)+( 1.000)*r( 94)
     &                +( 1.000)*r(103)+( 1.000)*r(104)+( 1.000)*r(106)
     &                +( 1.000)*r(128)+( 0.338)*r(176)+( 1.000)*r(177)
     &                +( 0.187)*r(191)+( 0.474)*r(195)+( 0.391)*r(210)
     &                +( 1.000)*r(222)+( 1.000)*r(225)+( 1.000)*r(226)
     &                +( 1.000)*r(254)+( 1.000)*r(260)+( 1.000)*r(262)
     &                +( 1.000)*r(264)+( 1.000)*r(274)+( 0.500)*r(277)
        Gain(lNO2  ) = Gain(lNO2  )
     &                +( 1.000)*r(278)+( 0.500)*r(279)+( 1.000)*r(280)
c
        Loss(lNO   )= +( 1.000)*r(  4)+( 1.000)*r(  7)+( 1.000)*r(  9)
     &                +( 2.000)*r( 10)+( 1.000)*r( 21)+( 1.000)*r( 31)
     &                +( 1.000)*r( 46)+( 1.000)*r( 51)+( 1.000)*r( 56)
     &                +( 1.000)*r( 62)+( 1.000)*r( 71)+( 1.000)*r( 81)
     &                +( 1.000)*r( 92)+( 1.000)*r(104)+( 1.000)*r(128)
     &                +( 1.000)*r(222)+( 1.000)*r(258)+( 1.000)*r(264)
     &                +( 1.000)*r(274)
c
        Gain(lNO   )= +( 1.000)*r(  1)+( 1.000)*r(  5)+( 1.000)*r( 14)
     &                +( 1.000)*r( 15)+( 1.000)*r( 22)+( 1.000)*r(261)
     &                +( 0.500)*r(277)
c
        Loss(lO    )= +( 1.000)*r(  2)+( 1.000)*r(  3)+( 1.000)*r(  4)
     &                +( 1.000)*r(  5)+( 1.000)*r(  6)+( 1.000)*r(164)
     &                +( 1.000)*r(168)+( 1.000)*r(188)+( 1.000)*r(192)
     &                +( 1.000)*r(196)+( 1.000)*r(207)+( 1.000)*r(211)
     &                +( 1.000)*r(217)
c
        Gain(lO    )= +( 1.000)*r(  1)+( 1.000)*r( 16)+( 1.000)*r( 17)
     &                +( 1.000)*r( 20)+( 1.000)*r(263)
c
        Loss(lO3   )= +( 1.000)*r(  3)+( 1.000)*r(  7)+( 1.000)*r(  8)
     &                +( 1.000)*r( 17)+( 1.000)*r( 18)+( 1.000)*r( 30)
     &                +( 1.000)*r( 36)+( 1.000)*r(162)+( 1.000)*r(167)
     &                +( 1.000)*r(171)+( 1.000)*r(179)+( 1.000)*r(186)
     &                +( 1.000)*r(190)+( 1.000)*r(194)+( 1.000)*r(205)
     &                +( 1.000)*r(209)+( 1.000)*r(215)+( 1.000)*r(220)
     &                +( 1.000)*r(256)
c
        Gain(lO3   )= +( 1.000)*r(  2)+( 0.250)*r( 72)+( 0.250)*r( 82)
     &                +( 0.250)*r( 93)+( 0.250)*r(105)
c
        Loss(lNO3  )= +( 1.000)*r(  9)+( 1.000)*r( 11)+( 1.000)*r( 14)
     &                +( 1.000)*r( 15)+( 1.000)*r( 16)+( 1.000)*r( 26)
     &                +( 1.000)*r( 39)+( 2.000)*r( 40)+( 1.000)*r( 48)
     &                +( 1.000)*r( 53)+( 1.000)*r( 58)+( 1.000)*r( 65)
     &                +( 1.000)*r( 73)+( 1.000)*r( 83)+( 1.000)*r( 94)
     &                +( 1.000)*r(106)+( 1.000)*r(129)+( 1.000)*r(132)
     &                +( 1.000)*r(135)+( 1.000)*r(148)+( 1.000)*r(151)
     &                +( 1.000)*r(154)+( 1.000)*r(156)+( 1.000)*r(157)
     &                +( 1.000)*r(160)+( 1.000)*r(163)+( 1.000)*r(172)
        Loss(lNO3  ) = Loss(lNO3  )
     &                +( 1.000)*r(187)+( 1.000)*r(191)+( 1.000)*r(195)
     &                +( 1.000)*r(206)+( 1.000)*r(210)+( 1.000)*r(216)
     &                +( 1.000)*r(260)+( 1.000)*r(272)
c
        Gain(lNO3  )= +( 1.000)*r(  6)+( 1.000)*r(  8)+( 1.000)*r( 12)
     &                +( 1.000)*r( 27)+( 0.390)*r( 34)+( 1.000)*r(227)
     &                +( 1.000)*r(228)+( 0.500)*r(279)
c
        Loss(lN2O5 )= +( 1.000)*r( 12)+( 1.000)*r( 13)+( 1.000)*r(253)
c
        Gain(lN2O5 )= +( 1.000)*r( 11)
c
        Loss(lHNO3 )= +( 1.000)*r( 27)+( 1.000)*r( 28)+( 1.000)*r(255)
c
        Gain(lHNO3 )= +( 2.000)*r( 13)+( 1.000)*r( 25)+( 0.200)*r( 39)
     &                +( 1.000)*r(129)+( 1.000)*r(132)+( 1.000)*r(135)
     &                +( 1.000)*r(148)+( 1.000)*r(151)+( 1.000)*r(154)
     &                +( 1.000)*r(156)+( 1.000)*r(157)+( 1.000)*r(160)
     &                +( 0.500)*r(163)+( 0.150)*r(172)+( 1.000)*r(253)
c
        Loss(lO1D  )= +( 1.000)*r( 19)+( 1.000)*r( 20)
c
        Gain(lO1D  )= +( 1.000)*r( 18)
c
        Loss(lOH   )= +( 1.000)*r( 21)+( 1.000)*r( 24)+( 1.000)*r( 25)
     &                +( 1.000)*r( 26)+( 1.000)*r( 27)+( 1.000)*r( 29)
     &                +( 1.000)*r( 30)+( 1.000)*r( 35)+( 1.000)*r( 42)
     &                +( 1.000)*r( 43)+( 1.000)*r( 44)+( 1.000)*r( 45)
     &                +( 1.000)*r(125)+( 1.000)*r(130)+( 1.000)*r(133)
     &                +( 1.000)*r(136)+( 1.000)*r(138)+( 1.000)*r(140)
     &                +( 1.000)*r(141)+( 1.000)*r(143)+( 1.000)*r(147)
     &                +( 1.000)*r(150)+( 1.000)*r(153)+( 1.000)*r(155)
     &                +( 1.000)*r(158)+( 1.000)*r(161)+( 1.000)*r(166)
        Loss(lOH   ) = Loss(lOH   )
     &                +( 1.000)*r(170)+( 1.000)*r(174)+( 1.000)*r(176)
     &                +( 1.000)*r(178)+( 1.000)*r(180)+( 1.000)*r(182)
     &                +( 1.000)*r(184)+( 1.000)*r(185)+( 1.000)*r(189)
     &                +( 1.000)*r(193)+( 1.000)*r(197)+( 1.000)*r(198)
     &                +( 1.000)*r(199)+( 1.000)*r(200)+( 1.000)*r(201)
     &                +( 1.000)*r(202)+( 1.000)*r(203)+( 1.000)*r(204)
     &                +( 1.000)*r(208)+( 1.000)*r(212)+( 1.000)*r(213)
     &                +( 1.000)*r(214)+( 1.000)*r(229)+( 1.000)*r(230)
        Loss(lOH   ) = Loss(lOH   )
     &                +( 1.000)*r(271)+( 1.000)*r(275)+( 1.000)*r(282)
     &                +( 1.000)*r(283)+( 1.000)*r(286)+( 1.000)*r(287)
c
        Gain(lOH   )= +( 2.000)*r( 19)+( 1.000)*r( 22)+( 1.000)*r( 28)
     &                +( 1.000)*r( 31)+( 0.390)*r( 34)+( 1.000)*r( 36)
     &                +( 0.800)*r( 39)+( 2.000)*r( 41)+( 0.350)*r(141)
     &                +( 1.000)*r(142)+( 0.660)*r(143)+( 1.000)*r(144)
     &                +( 0.208)*r(162)+( 0.330)*r(165)+( 0.164)*r(167)
     &                +( 0.285)*r(171)+( 0.500)*r(179)+( 0.120)*r(186)
     &                +( 0.266)*r(190)+( 0.567)*r(194)+( 0.246)*r(198)
     &                +( 0.155)*r(205)+( 0.378)*r(209)+( 0.099)*r(215)
     &                +( 1.000)*r(219)+( 0.246)*r(235)+( 1.000)*r(281)
c
        Loss(lHONO )= +( 1.000)*r( 22)+( 1.000)*r( 23)+( 1.000)*r( 24)
c
        Gain(lHONO )= +( 1.000)*r( 21)
c
        Loss(lHO2  )= +( 1.000)*r( 31)+( 1.000)*r( 32)+( 1.000)*r( 36)
     &                +( 2.000)*r( 37)+( 2.000)*r( 38)+( 1.000)*r( 39)
     &                +( 1.000)*r( 43)+( 1.000)*r( 47)+( 1.000)*r( 52)
     &                +( 1.000)*r( 57)+( 1.000)*r( 63)+( 1.000)*r( 72)
     &                +( 1.000)*r( 82)+( 1.000)*r( 93)+( 1.000)*r(105)
     &                +( 1.000)*r(118)+( 1.000)*r(121)+( 1.000)*r(126)
     &                +( 1.000)*r(223)+( 1.000)*r(257)+( 1.000)*r(266)
c
        Gain(lHO2  )= +( 1.000)*r( 23)+( 1.000)*r( 26)+( 1.000)*r( 29)
     &                +( 1.000)*r( 30)+( 1.000)*r( 33)+( 0.610)*r( 34)
     &                +( 1.000)*r( 42)+( 1.000)*r( 44)+( 1.000)*r( 45)
     &                +( 1.000)*r( 46)+( 1.000)*r( 48)+( 2.000)*r( 50)
     &                +( 1.000)*r( 51)+( 1.000)*r( 53)+( 1.000)*r( 54)
     &                +( 1.000)*r( 55)+( 1.000)*r( 57)+( 1.000)*r( 64)
     &                +( 1.000)*r( 65)+( 1.000)*r( 66)+( 1.000)*r( 68)
     &                +( 2.000)*r(123)+( 1.000)*r(125)+( 1.000)*r(127)
     &                +( 1.000)*r(128)+( 1.000)*r(129)+( 1.000)*r(131)
        Gain(lHO2  ) = Gain(lHO2  )
     &                +( 1.000)*r(134)+( 1.000)*r(140)+( 1.000)*r(142)
     &                +( 1.000)*r(144)+( 2.000)*r(145)+( 0.630)*r(147)
     &                +( 0.630)*r(148)+( 1.000)*r(149)+( 0.008)*r(162)
     &                +( 0.340)*r(165)+( 0.064)*r(167)+( 0.400)*r(171)
     &                +( 1.233)*r(173)+( 0.379)*r(174)+( 0.113)*r(176)
     &                +( 0.341)*r(177)+( 1.500)*r(179)+( 0.500)*r(181)
     &                +( 0.500)*r(183)+( 0.120)*r(186)+( 0.500)*r(188)
     &                +( 0.033)*r(194)+( 0.121)*r(198)+( 0.224)*r(202)
        Gain(lHO2  ) = Gain(lHO2  )
     &                +( 0.187)*r(203)+( 0.056)*r(205)+( 0.003)*r(209)
     &                +( 0.013)*r(211)+( 0.950)*r(212)+( 0.099)*r(215)
     &                +( 1.000)*r(231)+( 1.000)*r(232)+( 0.121)*r(235)
     &                +( 1.000)*r(251)+( 1.000)*r(252)+( 1.000)*r(284)
c
        Loss(lCO   )= +( 1.000)*r( 29)
c
        Gain(lCO   )= +( 1.000)*r(123)+( 1.000)*r(124)+( 1.000)*r(125)
     &                +( 1.000)*r(129)+( 1.000)*r(131)+( 0.034)*r(133)
     &                +( 1.000)*r(134)+( 2.000)*r(145)+( 1.000)*r(146)
     &                +( 1.260)*r(147)+( 1.260)*r(148)+( 1.000)*r(149)
     &                +( 1.000)*r(150)+( 1.000)*r(151)+( 0.416)*r(161)
     &                +( 0.450)*r(162)+( 0.500)*r(163)+( 0.670)*r(165)
     &                +( 0.475)*r(167)+( 0.700)*r(169)+( 0.336)*r(170)
     &                +( 0.498)*r(171)+( 0.572)*r(172)+( 1.233)*r(173)
     &                +( 1.000)*r(178)+( 1.500)*r(179)+( 1.000)*r(181)
        Gain(lCO   ) = Gain(lCO   )
     &                +( 1.000)*r(183)+( 0.500)*r(186)+( 0.491)*r(188)
     &                +( 0.275)*r(190)+( 0.157)*r(194)+( 0.160)*r(198)
     &                +( 0.002)*r(200)+( 0.345)*r(205)+( 0.265)*r(209)
     &                +( 0.012)*r(211)+( 0.365)*r(215)+( 1.000)*r(230)
     &                +( 1.000)*r(231)+( 0.160)*r(235)+( 0.002)*r(237)
c
        Loss(lHNO4 )= +( 1.000)*r( 33)+( 1.000)*r( 34)+( 1.000)*r( 35)
c
        Gain(lHNO4 )= +( 1.000)*r( 32)
c
        Loss(lHO2H )= +( 1.000)*r( 41)+( 1.000)*r( 42)
c
        Gain(lHO2H )= +( 1.000)*r( 37)+( 1.000)*r( 38)
c
        Loss(lSO2  )= +( 1.000)*r( 44)
c
        Gain(lSO2  )= 0.0
c
        Loss(lSULF )= 0.0
c
        Gain(lSULF )= +( 1.000)*r( 44)
c
        Loss(lCXO2 )= +( 1.000)*r( 46)+( 1.000)*r( 47)+( 1.000)*r( 48)
     &                +( 2.000)*r( 49)+( 2.000)*r( 50)+( 1.000)*r( 54)
     &                +( 1.000)*r( 59)+( 1.000)*r( 64)+( 1.000)*r( 74)
     &                +( 1.000)*r( 84)+( 1.000)*r( 95)+( 1.000)*r(107)
c
        Gain(lCXO2 )= +( 1.000)*r( 59)+( 1.000)*r( 71)+( 1.000)*r( 73)
     &                +( 2.000)*r( 78)+( 1.000)*r( 88)+( 1.000)*r( 99)
     &                +( 1.000)*r(111)+( 1.000)*r(116)+( 1.000)*r(131)
     &                +( 1.000)*r(137)+( 0.650)*r(141)+( 0.300)*r(169)
     &                +( 1.000)*r(184)+( 0.300)*r(188)+( 0.250)*r(192)
     &                +( 0.011)*r(200)+( 0.076)*r(205)+( 0.197)*r(209)
     &                +( 0.030)*r(210)+( 0.162)*r(213)+( 1.000)*r(233)
     &                +( 0.011)*r(237)+( 1.000)*r(284)
c
        Loss(lHCHO )= +( 1.000)*r(123)+( 1.000)*r(124)+( 1.000)*r(125)
     &                +( 1.000)*r(126)+( 1.000)*r(129)+( 1.000)*r(246)
c
        Gain(lHCHO )= +( 1.000)*r( 46)+( 1.000)*r( 48)+( 1.000)*r( 49)
     &                +( 2.000)*r( 50)+( 0.750)*r( 54)+( 0.750)*r( 64)
     &                +( 1.000)*r( 74)+( 1.000)*r( 84)+( 1.000)*r( 95)
     &                +( 1.000)*r(104)+( 1.000)*r(106)+( 1.000)*r(107)
     &                +( 1.000)*r(111)+( 1.000)*r(112)+( 1.000)*r(113)
     &                +( 2.000)*r(114)+( 1.000)*r(127)+( 1.000)*r(136)
     &                +( 0.115)*r(138)+( 1.000)*r(140)+( 0.350)*r(141)
     &                +( 1.000)*r(142)+( 1.000)*r(146)+( 0.084)*r(161)
     &                +( 0.200)*r(162)+( 0.670)*r(165)+( 0.300)*r(166)
        Gain(lHCHO ) = Gain(lHCHO )
     &                +( 0.100)*r(167)+( 0.055)*r(170)+( 0.125)*r(171)
     &                +( 0.227)*r(172)+( 0.300)*r(173)+( 0.213)*r(174)
     &                +( 0.506)*r(175)+( 0.010)*r(176)+( 0.134)*r(177)
     &                +( 1.610)*r(185)+( 1.000)*r(186)+( 0.191)*r(188)
     &                +( 0.624)*r(189)+( 0.592)*r(190)+( 0.240)*r(192)
     &                +( 0.276)*r(193)+( 0.235)*r(194)+( 0.039)*r(198)
     &                +( 0.026)*r(199)+( 0.024)*r(200)+( 0.026)*r(201)
     &                +( 0.732)*r(204)+( 0.500)*r(205)+( 0.244)*r(208)
        Gain(lHCHO ) = Gain(lHCHO )
     &                +( 0.269)*r(209)+( 0.079)*r(210)+( 0.081)*r(212)
     &                +( 0.234)*r(213)+( 0.311)*r(214)+( 0.300)*r(215)
     &                +( 0.039)*r(235)+( 0.026)*r(236)+( 0.024)*r(237)
     &                +( 0.026)*r(238)+( 1.000)*r(239)+( 1.000)*r(249)
     &                +( 0.065)*r(250)+( 1.000)*r(251)+( 1.000)*r(288)
c
        Loss(lCOOH )= +( 1.000)*r(141)+( 1.000)*r(142)
c
        Gain(lCOOH )= +( 1.000)*r( 47)
c
        Loss(lMEOH )= +( 1.000)*r(140)+( 1.000)*r(251)
c
        Gain(lMEOH )= +( 1.000)*r( 49)+( 0.250)*r( 54)+( 0.250)*r( 64)
c
        Loss(lRO2R )= +( 1.000)*r( 51)+( 1.000)*r( 52)+( 1.000)*r( 53)
     &                +( 1.000)*r( 54)+( 2.000)*r( 55)+( 1.000)*r( 60)
     &                +( 1.000)*r( 66)+( 1.000)*r( 75)+( 1.000)*r( 85)
     &                +( 1.000)*r( 96)+( 1.000)*r(108)
c
        Gain(lRO2R )= +( 1.000)*r( 60)+( 1.000)*r( 81)+( 1.000)*r( 83)
     &                +( 1.000)*r( 88)+( 2.000)*r( 89)+( 1.000)*r(100)
     &                +( 1.000)*r(112)+( 0.034)*r(133)+( 1.000)*r(134)
     &                +( 0.370)*r(138)+( 1.000)*r(139)+( 0.340)*r(143)
     &                +( 0.760)*r(153)+( 0.760)*r(155)+( 0.500)*r(161)
     &                +( 0.100)*r(162)+( 0.500)*r(163)+( 0.330)*r(165)
     &                +( 0.300)*r(166)+( 0.050)*r(167)+( 0.670)*r(170)
     &                +( 0.048)*r(171)+( 0.799)*r(172)+( 0.473)*r(174)
     &                +( 0.960)*r(175)+( 0.376)*r(176)+( 0.564)*r(177)
        Gain(lRO2R ) = Gain(lRO2R )
     &                +( 1.000)*r(178)+( 1.000)*r(181)+( 1.000)*r(183)
     &                +( 1.000)*r(185)+( 1.000)*r(187)+( 0.200)*r(188)
     &                +( 0.907)*r(189)+( 0.066)*r(190)+( 0.749)*r(191)
     &                +( 0.750)*r(193)+( 0.031)*r(194)+( 0.276)*r(195)
     &                +( 1.000)*r(197)+( 0.612)*r(198)+( 0.695)*r(199)
     &                +( 0.835)*r(200)+( 0.653)*r(201)+( 0.765)*r(202)
     &                +( 0.804)*r(203)+( 0.910)*r(204)+( 0.022)*r(205)
     &                +( 0.824)*r(206)+( 0.918)*r(208)+( 0.033)*r(209)
        Gain(lRO2R ) = Gain(lRO2R )
     &                +( 0.442)*r(210)+( 0.012)*r(211)+( 0.050)*r(212)
     &                +( 0.743)*r(213)+( 0.935)*r(214)+( 0.935)*r(216)
     &                +( 1.000)*r(234)+( 0.612)*r(235)+( 0.695)*r(236)
     &                +( 0.835)*r(237)+( 0.653)*r(238)+( 1.000)*r(239)
     &                +( 0.910)*r(240)+( 0.900)*r(241)+( 0.920)*r(242)
     &                +( 0.750)*r(243)+( 0.880)*r(244)+( 0.840)*r(245)
     &                +( 0.100)*r(248)+( 1.000)*r(249)+( 0.820)*r(286)
     &                +( 1.000)*r(287)
c
        Loss(lROOH )= +( 1.000)*r(143)+( 1.000)*r(144)
c
        Gain(lROOH )= +( 1.000)*r( 52)+( 1.000)*r( 63)
c
        Loss(lR2O2 )= +( 1.000)*r( 56)+( 1.000)*r( 57)+( 1.000)*r( 58)
     &                +( 1.000)*r( 59)+( 1.000)*r( 60)+( 2.000)*r( 61)
     &                +( 1.000)*r( 67)+( 1.000)*r( 76)+( 1.000)*r( 86)
     &                +( 1.000)*r( 97)+( 1.000)*r(109)
c
        Gain(lR2O2 )= +( 1.000)*r( 92)+( 1.000)*r( 94)+( 1.000)*r( 99)
     &                +( 1.000)*r(100)+( 2.000)*r(101)+( 1.000)*r(113)
     &                +( 1.000)*r(136)+( 0.616)*r(138)+( 0.675)*r(166)
     &                +( 0.515)*r(175)+( 0.596)*r(176)+( 0.152)*r(177)
     &                +( 1.000)*r(180)+( 1.000)*r(181)+( 1.000)*r(182)
     &                +( 1.000)*r(183)+( 0.079)*r(189)+( 0.126)*r(190)
     &                +( 0.187)*r(191)+( 0.240)*r(192)+( 0.500)*r(193)
     &                +( 0.729)*r(194)+( 0.750)*r(195)+( 0.559)*r(199)
     &                +( 0.936)*r(200)+( 0.948)*r(201)+( 0.205)*r(204)
        Gain(lR2O2 ) = Gain(lR2O2 )
     &                +( 0.488)*r(206)+( 0.001)*r(208)+( 0.137)*r(209)
     &                +( 0.711)*r(210)+( 0.381)*r(213)+( 0.559)*r(236)
     &                +( 0.936)*r(237)+( 0.948)*r(238)+( 1.000)*r(239)
     &                +( 0.730)*r(240)+( 0.680)*r(241)+( 0.780)*r(242)
     &                +( 0.450)*r(243)+( 0.135)*r(250)+( 1.000)*r(285)
     &                +( 0.180)*r(286)+( 2.000)*r(288)
c
        Loss(lRO2N )= +( 1.000)*r( 62)+( 1.000)*r( 63)+( 1.000)*r( 64)
     &                +( 1.000)*r( 65)+( 1.000)*r( 66)+( 1.000)*r( 67)
     &                +( 2.000)*r( 68)+( 1.000)*r( 77)+( 1.000)*r( 87)
     &                +( 1.000)*r( 98)+( 1.000)*r(110)
c
        Gain(lRO2N )= +( 1.000)*r( 67)+( 0.001)*r(133)+( 0.042)*r(138)
     &                +( 0.025)*r(166)+( 0.041)*r(170)+( 0.051)*r(172)
     &                +( 0.070)*r(174)+( 0.040)*r(175)+( 0.173)*r(176)
     &                +( 0.095)*r(177)+( 0.093)*r(189)+( 0.008)*r(190)
     &                +( 0.064)*r(191)+( 0.010)*r(192)+( 0.250)*r(193)
     &                +( 0.180)*r(194)+( 0.250)*r(195)+( 0.021)*r(198)
     &                +( 0.070)*r(199)+( 0.143)*r(200)+( 0.347)*r(201)
     &                +( 0.011)*r(202)+( 0.009)*r(203)+( 0.090)*r(204)
     &                +( 0.001)*r(205)+( 0.176)*r(206)+( 0.082)*r(208)
        Gain(lRO2N ) = Gain(lRO2N )
     &                +( 0.002)*r(209)+( 0.136)*r(210)+( 0.001)*r(211)
     &                +( 0.078)*r(213)+( 0.065)*r(214)+( 0.065)*r(216)
     &                +( 0.021)*r(235)+( 0.070)*r(236)+( 0.143)*r(237)
     &                +( 0.347)*r(238)+( 0.090)*r(240)+( 0.100)*r(241)
     &                +( 0.080)*r(242)+( 0.250)*r(243)+( 0.120)*r(244)
     &                +( 0.160)*r(245)
c
        Loss(lRNO3 )= +( 1.000)*r(176)+( 1.000)*r(177)
c
        Gain(lRNO3 )= +( 1.000)*r( 62)+( 1.000)*r(115)+( 0.572)*r(172)
     &                +( 0.310)*r(176)+( 0.813)*r(191)+( 0.276)*r(195)
     &                +( 0.511)*r(206)+( 0.321)*r(210)+( 1.000)*r(216)
     &                +( 1.000)*r(255)
c
        Loss(lMEK  )= +( 1.000)*r(138)+( 1.000)*r(139)+( 1.000)*r(250)
c
        Gain(lMEK  )= +( 0.500)*r( 64)+( 1.000)*r( 65)+( 0.500)*r( 66)
     &                +( 1.000)*r( 68)+( 0.416)*r(161)+( 0.550)*r(168)
     &                +( 0.150)*r(170)+( 0.210)*r(171)+( 0.233)*r(173)
     &                +( 0.115)*r(174)+( 0.177)*r(176)+( 0.243)*r(177)
     &                +( 0.332)*r(199)+( 0.110)*r(200)+( 0.089)*r(201)
     &                +( 0.437)*r(207)+( 0.072)*r(208)+( 0.026)*r(209)
     &                +( 0.001)*r(210)+( 0.659)*r(211)+( 0.719)*r(213)
     &                +( 0.550)*r(217)+( 0.332)*r(236)+( 0.110)*r(237)
     &                +( 0.089)*r(238)
c
        Loss(lPROD )= +( 1.000)*r(174)+( 1.000)*r(175)
c
        Gain(lPROD )= +( 0.500)*r( 64)+( 0.500)*r( 66)+( 1.000)*r( 68)
     &                +( 1.000)*r( 77)+( 1.000)*r( 87)+( 1.000)*r( 98)
     &                +( 0.700)*r(169)+( 0.332)*r(170)+( 0.329)*r(174)
     &                +( 0.048)*r(176)+( 0.435)*r(177)+( 0.100)*r(190)
     &                +( 0.750)*r(192)+( 0.276)*r(193)+( 0.276)*r(194)
     &                +( 0.853)*r(196)+( 0.125)*r(200)+( 0.417)*r(201)
     &                +( 0.055)*r(202)+( 0.119)*r(204)+( 0.215)*r(205)
     &                +( 0.113)*r(207)+( 0.006)*r(209)+( 0.259)*r(211)
     &                +( 0.007)*r(213)+( 0.125)*r(237)+( 0.417)*r(238)
        Gain(lPROD ) = Gain(lPROD )
     &                +( 0.300)*r(243)+( 0.210)*r(244)+( 0.220)*r(245)
c
        Loss(lCCO3 )= +( 1.000)*r( 69)+( 1.000)*r( 71)+( 1.000)*r( 72)
     &                +( 1.000)*r( 73)+( 1.000)*r( 74)+( 1.000)*r( 75)
     &                +( 1.000)*r( 76)+( 1.000)*r( 77)+( 2.000)*r( 78)
     &                +( 1.000)*r( 88)+( 1.000)*r( 99)+( 1.000)*r(111)
c
        Gain(lCCO3 )= +( 1.000)*r( 70)+( 1.000)*r( 76)+( 1.000)*r(104)
     &                +( 1.000)*r(106)+( 1.000)*r(111)+( 1.000)*r(112)
     &                +( 1.000)*r(113)+( 2.000)*r(114)+( 1.000)*r(130)
     &                +( 1.000)*r(132)+( 1.000)*r(136)+( 1.000)*r(137)
     &                +( 0.492)*r(138)+( 1.000)*r(139)+( 1.000)*r(149)
     &                +( 1.000)*r(150)+( 1.000)*r(151)+( 2.000)*r(152)
     &                +( 0.670)*r(165)+( 0.675)*r(166)+( 0.467)*r(173)
     &                +( 0.029)*r(174)+( 0.667)*r(175)+( 1.000)*r(180)
     &                +( 0.500)*r(181)+( 1.000)*r(182)+( 0.500)*r(183)
        Gain(lCCO3 ) = Gain(lCCO3 )
     &                +( 0.123)*r(194)+( 0.011)*r(200)+( 0.137)*r(209)
     &                +( 0.011)*r(237)+( 1.000)*r(249)+( 0.085)*r(250)
     &                +( 1.000)*r(288)
c
        Loss(lPAN  )= +( 1.000)*r( 70)
c
        Gain(lPAN  )= +( 1.000)*r( 69)
c
        Loss(lCO3H )= 0.0
c
        Gain(lCO3H )= +( 0.750)*r( 72)
c
        Loss(lCO2H )= 0.0
c
        Gain(lCO2H )= +( 0.250)*r( 72)+( 1.000)*r( 74)+( 1.000)*r( 75)
     &                +( 1.000)*r( 77)+( 0.050)*r(205)+( 0.129)*r(209)
c
        Loss(lRCO3 )= +( 1.000)*r( 79)+( 1.000)*r( 81)+( 1.000)*r( 82)
     &                +( 1.000)*r( 83)+( 1.000)*r( 84)+( 1.000)*r( 85)
     &                +( 1.000)*r( 86)+( 1.000)*r( 87)+( 1.000)*r( 88)
     &                +( 2.000)*r( 89)+( 1.000)*r(100)+( 1.000)*r(112)
c
        Gain(lRCO3 )= +( 1.000)*r( 80)+( 1.000)*r( 86)+( 0.965)*r(133)
     &                +( 1.000)*r(135)+( 0.096)*r(138)+( 0.370)*r(147)
     &                +( 0.370)*r(148)+( 0.100)*r(162)+( 0.050)*r(167)
     &                +( 0.048)*r(171)+( 0.300)*r(173)+( 0.049)*r(174)
     &                +( 0.333)*r(175)+( 0.201)*r(194)+( 0.006)*r(209)
     &                +( 0.900)*r(248)+( 0.036)*r(250)
c
        Loss(lPAN2 )= +( 1.000)*r( 80)
c
        Gain(lPAN2 )= +( 1.000)*r( 79)
c
        Loss(lCCHO )= +( 1.000)*r(130)+( 1.000)*r(131)+( 1.000)*r(132)
     &                +( 1.000)*r(247)
c
        Gain(lCCHO )= +( 1.000)*r( 81)+( 1.000)*r( 83)+( 1.000)*r( 88)
     &                +( 2.000)*r( 89)+( 1.000)*r(100)+( 1.000)*r(112)
     &                +( 0.034)*r(133)+( 1.000)*r(134)+( 0.482)*r(138)
     &                +( 1.000)*r(139)+( 0.129)*r(170)+( 0.047)*r(171)
     &                +( 0.467)*r(173)+( 0.084)*r(174)+( 0.246)*r(175)
     &                +( 0.439)*r(176)+( 0.431)*r(177)+( 0.195)*r(185)
     &                +( 0.250)*r(188)+( 1.000)*r(197)+( 0.445)*r(199)
     &                +( 0.455)*r(200)+( 0.099)*r(201)+( 0.294)*r(204)
     &                +( 0.154)*r(205)+( 0.009)*r(206)+( 0.732)*r(208)
        Gain(lCCHO ) = Gain(lCCHO )
     &                +( 0.456)*r(209)+( 0.507)*r(210)+( 0.960)*r(212)
     &                +( 0.624)*r(214)+( 1.000)*r(234)+( 0.445)*r(236)
     &                +( 0.455)*r(237)+( 0.099)*r(238)+( 0.500)*r(240)
     &                +( 0.230)*r(241)+( 0.070)*r(250)+( 1.000)*r(252)
c
        Loss(lRC3H )= 0.0
c
        Gain(lRC3H )= +( 0.750)*r( 82)+( 0.750)*r( 93)+( 0.750)*r(105)
c
        Loss(lRC2H )= 0.0
c
        Gain(lRC2H )= +( 0.250)*r( 82)+( 1.000)*r( 84)+( 1.000)*r( 85)
     &                +( 1.000)*r( 87)+( 0.250)*r( 93)+( 1.000)*r( 95)
     &                +( 1.000)*r( 96)+( 1.000)*r( 98)+( 0.250)*r(105)
     &                +( 1.000)*r(107)+( 1.000)*r(108)+( 2.000)*r(110)
     &                +( 0.372)*r(171)+( 0.150)*r(190)+( 0.189)*r(194)
     &                +( 0.119)*r(205)+( 0.303)*r(209)+( 0.285)*r(215)
c
        Loss(lBZCO )= +( 1.000)*r( 90)+( 1.000)*r( 92)+( 1.000)*r( 93)
     &                +( 1.000)*r( 94)+( 1.000)*r( 95)+( 1.000)*r( 96)
     &                +( 1.000)*r( 97)+( 1.000)*r( 98)+( 1.000)*r( 99)
     &                +( 1.000)*r(100)+( 2.000)*r(101)+( 1.000)*r(113)
c
        Gain(lBZCO )= +( 1.000)*r( 91)+( 1.000)*r( 97)+( 1.000)*r(158)
     &                +( 1.000)*r(160)
c
        Loss(lPBZN )= +( 1.000)*r( 91)
c
        Gain(lPBZN )= +( 1.000)*r( 90)
c
        Loss(lBZO  )= +( 1.000)*r(117)+( 1.000)*r(118)+( 1.000)*r(119)
c
        Gain(lBZO  )= +( 1.000)*r( 92)+( 1.000)*r( 94)+( 1.000)*r( 99)
     &                +( 1.000)*r(100)+( 2.000)*r(101)+( 1.000)*r(113)
     &                +( 0.240)*r(153)+( 1.000)*r(154)+( 0.240)*r(155)
     &                +( 1.000)*r(156)
c
        Loss(lMCO3 )= +( 1.000)*r(102)+( 1.000)*r(104)+( 1.000)*r(105)
     &                +( 1.000)*r(106)+( 1.000)*r(107)+( 1.000)*r(108)
     &                +( 1.000)*r(109)+( 1.000)*r(110)+( 1.000)*r(111)
     &                +( 1.000)*r(112)+( 1.000)*r(113)+( 2.000)*r(114)
c
        Gain(lMCO3 )= +( 1.000)*r(103)+( 1.000)*r(109)+( 0.500)*r(161)
     &                +( 0.500)*r(163)+( 0.330)*r(165)+( 0.300)*r(169)
     &                +( 0.289)*r(170)+( 0.150)*r(172)+( 0.192)*r(190)
     &                +( 0.240)*r(192)
c
        Loss(lMPAN )= +( 1.000)*r(103)
c
        Gain(lMPAN )= +( 1.000)*r(102)
c
        Loss(lTBUO )= +( 1.000)*r(115)+( 1.000)*r(116)
c
        Gain(lTBUO )= +( 0.236)*r(199)+( 0.016)*r(213)+( 0.236)*r(236)
c
        Loss(lACET )= +( 1.000)*r(136)+( 1.000)*r(137)+( 1.000)*r(249)
c
        Gain(lACET )= +( 1.000)*r(116)+( 0.006)*r(176)+( 0.020)*r(177)
     &                +( 0.130)*r(194)+( 0.417)*r(198)+( 0.024)*r(199)
     &                +( 0.452)*r(200)+( 0.072)*r(201)+( 0.005)*r(204)
     &                +( 0.001)*r(205)+( 0.024)*r(206)+( 0.127)*r(208)
     &                +( 0.045)*r(209)+( 0.102)*r(210)+( 0.024)*r(213)
     &                +( 0.624)*r(214)+( 0.015)*r(215)+( 0.934)*r(216)
     &                +( 0.417)*r(235)+( 0.024)*r(236)+( 0.452)*r(237)
     &                +( 0.072)*r(238)
c
        Loss(lNPHE )= +( 1.000)*r(157)
c
        Gain(lNPHE )= +( 1.000)*r(117)+( 1.000)*r(121)+( 1.000)*r(122)
c
        Loss(lPHEN )= +( 1.000)*r(153)+( 1.000)*r(154)
c
        Gain(lPHEN )= +( 1.000)*r(118)+( 1.000)*r(119)+( 0.017)*r(202)
c
        Loss(lBZNO )= +( 1.000)*r(120)+( 1.000)*r(121)+( 1.000)*r(122)
c
        Gain(lBZNO )= +( 1.000)*r(157)
c
        Loss(lXN   )= 0.0
c
        Gain(lXN   )= +( 2.000)*r(120)+( 0.500)*r(163)+( 0.278)*r(172)
     &                +( 0.352)*r(176)+( 1.000)*r(187)+( 0.250)*r(195)
     &                +( 0.489)*r(206)+( 0.288)*r(210)
c
        Loss(lHCO3 )= +( 1.000)*r(127)+( 1.000)*r(128)
c
        Gain(lHCO3 )= +( 1.000)*r(126)
c
        Loss(lHC2H )= 0.0
c
        Gain(lHC2H )= +( 1.000)*r(128)+( 0.333)*r(162)+( 0.351)*r(167)
     &                +( 0.100)*r(171)+( 0.370)*r(186)+( 0.204)*r(190)
     &                +( 0.103)*r(194)+( 0.121)*r(198)+( 0.185)*r(205)
     &                +( 0.073)*r(209)+( 0.259)*r(215)+( 0.121)*r(235)
c
        Loss(lRCHO )= +( 1.000)*r(133)+( 1.000)*r(134)+( 1.000)*r(135)
     &                +( 1.000)*r(248)
c
        Gain(lRCHO )= +( 0.370)*r(138)+( 1.000)*r(143)+( 1.000)*r(144)
     &                +( 1.000)*r(164)+( 0.675)*r(166)+( 0.450)*r(168)
     &                +( 0.013)*r(170)+( 0.218)*r(172)+( 0.558)*r(174)
     &                +( 0.710)*r(175)+( 0.213)*r(176)+( 0.147)*r(177)
     &                +( 1.000)*r(178)+( 1.000)*r(180)+( 1.000)*r(182)
     &                +( 1.000)*r(187)+( 0.474)*r(193)+( 0.205)*r(194)
     &                +( 0.474)*r(195)+( 0.147)*r(196)+( 0.155)*r(198)
     &                +( 0.122)*r(199)+( 0.244)*r(200)+( 0.204)*r(201)
     &                +( 0.497)*r(204)+( 0.363)*r(205)+( 0.037)*r(206)
        Gain(lRCHO ) = Gain(lRCHO )
     &                +( 0.450)*r(207)+( 0.511)*r(208)+( 0.305)*r(209)
     &                +( 0.151)*r(210)+( 0.069)*r(211)+( 0.311)*r(214)
     &                +( 0.700)*r(215)+( 0.935)*r(216)+( 0.450)*r(217)
     &                +( 0.155)*r(235)+( 0.122)*r(236)+( 0.244)*r(237)
     &                +( 0.204)*r(238)+( 0.300)*r(240)+( 0.450)*r(241)
     &                +( 0.450)*r(243)+( 0.840)*r(250)+( 1.000)*r(285)
     &                +( 0.180)*r(286)
c
        Loss(lGLY  )= +( 1.000)*r(145)+( 1.000)*r(146)+( 1.000)*r(147)
     &                +( 1.000)*r(148)
c
        Gain(lGLY  )= +( 0.230)*r(153)+( 0.150)*r(170)+( 0.023)*r(171)
     &                +( 1.000)*r(179)+( 0.500)*r(181)+( 0.500)*r(183)
     &                +( 0.009)*r(188)+( 0.001)*r(194)+( 0.248)*r(198)
     &                +( 0.118)*r(202)+( 0.097)*r(203)+( 0.248)*r(235)
c
        Loss(lMGLY )= +( 1.000)*r(149)+( 1.000)*r(150)+( 1.000)*r(151)
c
        Gain(lMGLY )= +( 0.230)*r(155)+( 0.084)*r(161)+( 0.900)*r(162)
     &                +( 0.300)*r(166)+( 0.950)*r(167)+( 0.174)*r(170)
     &                +( 0.742)*r(171)+( 0.008)*r(172)+( 0.500)*r(181)
     &                +( 0.500)*r(183)+( 0.119)*r(202)+( 0.287)*r(203)
c
        Loss(lBACL )= +( 1.000)*r(152)
c
        Gain(lBACL )= +( 0.031)*r(194)+( 0.087)*r(203)
c
        Loss(lCRES )= +( 1.000)*r(155)+( 1.000)*r(156)
c
        Gain(lCRES )= +( 0.207)*r(202)+( 0.187)*r(203)
c
        Loss(lBALD )= +( 1.000)*r(158)+( 1.000)*r(159)+( 1.000)*r(160)
c
        Gain(lBALD )= +( 0.059)*r(202)+( 0.050)*r(203)+( 0.061)*r(208)
     &                +( 0.042)*r(209)+( 0.015)*r(210)+( 0.670)*r(244)
     &                +( 0.620)*r(245)
c
        Loss(lMETH )= +( 1.000)*r(161)+( 1.000)*r(162)+( 1.000)*r(163)
     &                +( 1.000)*r(164)+( 1.000)*r(165)
c
        Gain(lMETH )= +( 0.230)*r(189)+( 0.390)*r(190)+( 0.025)*r(208)
     &                +( 0.026)*r(209)+( 0.012)*r(211)+( 0.200)*r(242)
c
        Loss(lMVK  )= +( 1.000)*r(166)+( 1.000)*r(167)+( 1.000)*r(168)
     &                +( 1.000)*r(169)
c
        Gain(lMVK  )= +( 0.320)*r(189)+( 0.160)*r(190)+( 0.048)*r(210)
     &                +( 0.270)*r(242)
c
        Loss(lISPD )= +( 1.000)*r(170)+( 1.000)*r(171)+( 1.000)*r(172)
     &                +( 1.000)*r(173)
c
        Gain(lISPD )= +( 0.357)*r(189)+( 0.936)*r(191)+( 0.025)*r(208)
     &                +( 0.150)*r(240)+( 0.250)*r(241)+( 0.450)*r(242)
c
        Loss(lDCB1 )= +( 1.000)*r(178)+( 1.000)*r(179)
c
        Gain(lDCB1 )= +( 0.491)*r(202)+( 0.561)*r(203)
c
        Loss(lDCB2 )= +( 1.000)*r(180)+( 1.000)*r(181)
c
        Gain(lDCB2 )= +( 0.108)*r(202)+( 0.099)*r(203)
c
        Loss(lDCB3 )= +( 1.000)*r(182)+( 1.000)*r(183)
c
        Gain(lDCB3 )= +( 0.051)*r(202)+( 0.093)*r(203)
c
        Loss(lETHE )= +( 1.000)*r(185)+( 1.000)*r(186)+( 1.000)*r(187)
     &                +( 1.000)*r(188)+( 1.000)*r(239)
c
        Gain(lETHE )= 0.0
c
        Loss(lISOP )= +( 1.000)*r(189)+( 1.000)*r(190)+( 1.000)*r(191)
     &                +( 1.000)*r(192)+( 1.000)*r(242)
c
        Gain(lISOP )= 0.0
c
        Loss(lTERP )= +( 1.000)*r(193)+( 1.000)*r(194)+( 1.000)*r(195)
     &                +( 1.000)*r(196)+( 1.000)*r(243)
c
        Gain(lTERP )= 0.0
c
        Loss(lALK1 )= +( 1.000)*r(197)+( 1.000)*r(234)
c
        Gain(lALK1 )= 0.0
c
        Loss(lALK2 )= +( 1.000)*r(198)+( 1.000)*r(235)
c
        Gain(lALK2 )= 0.0
c
        Loss(lALK3 )= +( 1.000)*r(199)+( 1.000)*r(236)
c
        Gain(lALK3 )= 0.0
c
        Loss(lALK4 )= +( 1.000)*r(200)+( 1.000)*r(237)
c
        Gain(lALK4 )= 0.0
c
        Loss(lALK5 )= +( 1.000)*r(201)+( 1.000)*r(238)
c
        Gain(lALK5 )= 0.0
c
        Loss(lARO1 )= +( 1.000)*r(202)+( 1.000)*r(244)
c
        Gain(lARO1 )= 0.0
c
        Loss(lARO2 )= +( 1.000)*r(203)+( 1.000)*r(245)
c
        Gain(lARO2 )= 0.0
c
        Loss(lOLE1 )= +( 1.000)*r(204)+( 1.000)*r(205)+( 1.000)*r(206)
     &                +( 1.000)*r(207)+( 1.000)*r(240)
c
        Gain(lOLE1 )= 0.0
c
        Loss(lOLE2 )= +( 1.000)*r(208)+( 1.000)*r(209)+( 1.000)*r(210)
     &                +( 1.000)*r(211)+( 1.000)*r(241)
c
        Gain(lOLE2 )= 0.0
c
        Loss(lETOH )= +( 1.000)*r(212)+( 1.000)*r(252)
c
        Gain(lETOH )= 0.0
c
        Loss(lMTBE )= +( 1.000)*r(213)
c
        Gain(lMTBE )= 0.0
c
        Loss(lMBUT )= +( 1.000)*r(214)+( 1.000)*r(215)+( 1.000)*r(216)
     &                +( 1.000)*r(217)
c
        Gain(lMBUT )= 0.0
c
        Loss(lCL2  )= +( 1.000)*r(218)
c
        Gain(lCL2  )= +( 0.300)*r(221)+( 1.000)*r(228)
c
        Loss(lCL   )= +( 1.000)*r(220)+( 1.000)*r(228)+( 1.000)*r(232)
     &                +( 1.000)*r(233)+( 1.000)*r(234)+( 1.000)*r(235)
     &                +( 1.000)*r(236)+( 1.000)*r(237)+( 1.000)*r(238)
     &                +( 1.000)*r(239)+( 1.000)*r(240)+( 1.000)*r(241)
     &                +( 1.000)*r(242)+( 1.000)*r(243)+( 1.000)*r(244)
     &                +( 1.000)*r(245)+( 1.000)*r(246)+( 1.000)*r(247)
     &                +( 1.000)*r(248)+( 1.000)*r(249)+( 1.000)*r(250)
     &                +( 1.000)*r(251)+( 1.000)*r(252)
c
        Gain(lCL   )= +( 2.000)*r(218)+( 1.000)*r(219)+( 1.400)*r(221)
     &                +( 1.000)*r(222)+( 1.000)*r(227)+( 1.000)*r(229)
     &                +( 1.000)*r(230)+( 1.000)*r(231)+( 1.000)*r(254)
c
        Loss(lHOCL )= +( 1.000)*r(219)
c
        Gain(lHOCL )= +( 1.000)*r(223)
c
        Loss(lCLO  )= +( 2.000)*r(221)+( 1.000)*r(222)+( 1.000)*r(223)
     &                +( 1.000)*r(224)
c
        Gain(lCLO  )= +( 1.000)*r(220)+( 1.000)*r(225)+( 1.000)*r(226)
c
        Loss(lN3CL )= +( 1.000)*r(225)+( 1.000)*r(226)+( 1.000)*r(227)
     &                +( 1.000)*r(228)
c
        Gain(lN3CL )= +( 1.000)*r(224)
c
        Loss(lHCL  )= +( 1.000)*r(229)+( 1.000)*r(253)
c
        Gain(lHCL  )= +( 1.000)*r(232)+( 1.000)*r(233)+( 1.000)*r(234)
     &                +( 1.000)*r(235)+( 1.000)*r(236)+( 1.000)*r(237)
     &                +( 1.000)*r(238)+( 0.200)*r(240)+( 0.250)*r(241)
     &                +( 0.150)*r(242)+( 0.400)*r(243)+( 1.000)*r(244)
     &                +( 1.000)*r(245)+( 1.000)*r(246)+( 1.000)*r(247)
     &                +( 1.000)*r(248)+( 1.000)*r(249)+( 1.000)*r(250)
     &                +( 1.000)*r(251)+( 1.000)*r(252)+( 1.000)*r(255)
c
        Loss(lFMCL )= +( 1.000)*r(230)+( 1.000)*r(231)
c
        Gain(lFMCL )= +( 1.000)*r(239)+( 0.800)*r(240)+( 0.750)*r(241)
     &                +( 0.850)*r(242)+( 0.600)*r(243)
c
        Loss(lNTCL )= +( 1.000)*r(254)
c
        Gain(lNTCL )= +( 1.000)*r(253)
c
        Loss(lSS   )= +( 1.000)*r(255)
c
        Gain(lSS   )= 0.0
c
        Loss(lI    )= +( 1.000)*r(256)+( 1.000)*r(257)+( 1.000)*r(258)
     &                +( 1.000)*r(259)+( 1.000)*r(260)+( 1.000)*r(261)
     &                +( 1.000)*r(262)
c
        Gain(lI    )= +( 1.000)*r(263)+( 1.000)*r(264)+( 1.000)*r(267)
     &                +( 2.000)*r(270)+( 1.000)*r(271)+( 1.000)*r(272)
     &                +( 1.000)*r(273)+( 0.500)*r(277)+( 1.000)*r(278)
     &                +( 0.500)*r(279)+( 1.000)*r(281)+( 1.000)*r(283)
     &                +( 1.000)*r(284)+( 1.000)*r(285)+( 0.180)*r(286)
     &                +( 1.000)*r(288)
c
        Loss(lIO   )= +( 1.000)*r(263)+( 1.000)*r(264)+( 1.000)*r(265)
     &                +( 1.000)*r(266)+( 2.000)*r(267)+( 2.000)*r(268)
     &                +( 1.000)*r(269)
c
        Gain(lIO   )= +( 1.000)*r(256)+( 1.000)*r(260)+( 1.000)*r(274)
     &                +( 0.500)*r(277)+( 0.500)*r(279)+( 1.000)*r(280)
     &                +( 1.000)*r(282)
c
        Loss(lHI   )= +( 1.000)*r(283)
c
        Gain(lHI   )= +( 1.000)*r(257)
c
        Loss(lINO  )= +( 1.000)*r(261)
c
        Gain(lINO  )= +( 1.000)*r(258)
c
        Loss(lINO2 )= +( 1.000)*r(262)+( 1.000)*r(277)+( 1.000)*r(278)
c
        Gain(lINO2 )= +( 1.000)*r(259)
c
        Loss(lI2   )= +( 1.000)*r(270)+( 1.000)*r(271)+( 1.000)*r(272)
c
        Gain(lI2   )= +( 1.000)*r(261)+( 1.000)*r(262)
c
        Loss(lINO3 )= +( 1.000)*r(279)+( 1.000)*r(280)
c
        Gain(lINO3 )= +( 1.000)*r(265)+( 1.000)*r(272)
c
        Loss(lHOI  )= +( 1.000)*r(281)+( 1.000)*r(282)
c
        Gain(lHOI  )= +( 1.000)*r(266)+( 1.000)*r(271)
c
        Loss(lOIO  )= +( 1.000)*r(273)+( 1.000)*r(274)+( 1.000)*r(275)
     &                +( 1.000)*r(276)
c
        Gain(lOIO  )= +( 1.000)*r(267)
c
        Loss(lIXOY )= +( 1.000)*r(269)+( 1.000)*r(276)
c
        Gain(lIXOY )= +( 1.000)*r(268)+( 1.500)*r(269)+( 1.500)*r(276)
c
        Loss(lHIO3 )= 0.0
c
        Gain(lHIO3 )= +( 1.000)*r(275)
c
        Loss(lCH3I )= +( 1.000)*r(284)
c
        Gain(lCH3I )= 0.0
c
        Loss(lNBUI )= +( 1.000)*r(285)+( 1.000)*r(286)
c
        Gain(lNBUI )= 0.0
c
        Loss(lIBUO )= +( 1.000)*r(287)
c
        Gain(lIBUO )= +( 0.820)*r(286)
c
        Loss(lIBAC )= +( 1.000)*r(288)
c
        Gain(lIBAC )= +( 1.000)*r(287)
c
c
      return
      end

