      subroutine cpamech7( r, rk, dtfact, nr, pa, npa, npa_init, ldark )
      use filunit
      use tracer
      use procan
c
c----CAMx v6.00 130506
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
C***********************************************************************
C  Called by CHEMDRIV, uses integrated reaction rates (R) to calculate
C  important chemical process rates (PA)
C
C    *** NOTE:  This code is hardwired for the CAMx CB6 mechanism 7 ***
C
C  Arguments:  R should be the average rate during the most recent
C       chemistry time step, multipled by the time step, eg:
C       the chem solver calculates 0.5*dt*(R+Rold) and passes it
C       to this routine. It's OK to use simply R*dt, but this
C       is a slightly less accurate form of the integration.
C       Important reaction rates are summed below to calculate PA
C       which is passed back to the Chem routine and accumulated for
C       one hour and then output.
C       RK is the rate constants in ppm-n hr-1.
C       DTFACT is the ratio of time step to averaging interval.
C       NR is the number of reactions.
C       NPA is the number of chemical process rates calculated below.
C       NPA_INIT sets num of outputs on first dummy call from pasetup.
C       LDARK is flag set true at night.
C       R is assumed to be ppm.
C       PA is converted to ppb.
C
C** REVISION HISTORY:
C**   NO.    DATE   WHO  WHAT
C**   ---    -----  ---  --------------------------------------------
C**   01     11/07  GY  Created
C**
C***********************************************************************
C
      IMPLICIT NONE
C
      include 'camx.prm'
C
C... Local parameter sets the cut-point between NOx and VOC sensitive
C    O3 production.  Default = 0.35.  Recommended range is 0.15 to 0.35.
C
      REAL  ratio_cut
      PARAMETER (ratio_cut = 0.35)
C
C... Factor to convert PPM to PPB
C
      REAL  ppbfact
      PARAMETER (ppbfact = 1000.0)
C
C... Argument declarations
      INTEGER  NR, NPA, NPA_INIT
      REAL     R(NR), PA(NPA), RK(NR)
      REAL     DTFACT
      LOGICAL  LDARK
C
C... Local variable declarations
      REAL   cyc_HONO_p, cyc_HONO_l
      REAL   cyc_H2O2_p, cyc_H2O2_l
      REAL   cyc_ROOH_p, cyc_ROOH_l
      REAL   cyc_MEPX_p, cyc_MEPX_l
      REAL   cyc_HNO4_p, cyc_HNO4_l
      REAL   cyc_HCO3_p, cyc_HCO3_l
      REAL   cyc_PAN_p,  cyc_PAN_l
      REAL   cyc_PANX_p, cyc_PANX_l
      REAL   cyc_OPAN_p, cyc_OPAN_l
      REAL   OH_new, HO2_new, HOx_new, RO2_new
      REAL   OH_reacted, HO2_reacted, HOx_reacted
      REAL   OH_term, HO2_term, HOx_term, RO2_term
      REAL   OHwHC
      REAL   other_OH_prop
      REAL   hcho_from_eth, hcho_from_ole, hcho_from_iole
      REAL   hcho_from_terp, hcho_from_open, hcho_from_xopn
      REAL   hcho_from_isop, hcho_from_ispd
      REAL   POx_net, ratio_ind, total_ho2_prod 
      REAL   prod_h2o2, prod_hno3, o3_prod, o3_loss 
      REAL   Y_OH_per_HO2, OH_from_HO2
      REAL   ho2_frac_meo2, meo2_frac_c2o3, c2o3_frac_cxo3
      REAL   ho2_frac_xo2h, xo2h_frac_cxo3, xo2h_frac_opo3
      REAL   sum, tmp, ho2tmp
      INTEGER   n, nn
C
C***********************************************************************
C
C  { calculation of these PA outputs is currently hardwired below }
C
C  { Oxidant and Ozone Production and Loss }
C
C     OxProd
C     OxLoss
C     PO3_net
C     PO3_VOCsns
C     PO3_NOxsns
C     PH2O2_PHN3
C     O3_dest
C
C  { Radical Initiation }
C
C     OH_new
C     HO2_new
C     HOx_new
C     newOH_O1D
C     newOH_HONO
C     nOH_O3_OLE
C     nwHO2_HCHO
C     RO2_new
C
C  { Radical Propagation }
C
C     OHw_CO
C     OHw_CH4
C     OHw_ETHA
C     OHw_PRPA
C     OHw_PAR
C     OHw_BENZ
C     OHw_TOL
C     OHw_XYL
C     OHw_ETH
C     OHw_ETHA
C     OHw_OLE
C     OHw_IOLE
C     OHw_ISOP
C     OHw_TERP
C     OHw_ETHY
C     OHw_all_HC
C     ISOPwOx
C     TERPwOx
C     OH_rctd
C     HO2_rctd
C     HOx_rctd
C     RO2_rctd
C     OHfromHO2
C     Y_OHperHO2
C
C  { Radical Termination and Chain Length }
C
C     OH_term
C     HO2_term
C     HOx_term
C     RO2_term
C     HOx_CL
C
C  { Formaldehyde Chemistry }
C
C     HCHOp_eth
C     HCHOp_ole
C     HCHOp_iole
C     HCHOp_terp
C     HCHOp_isop
C     HCHOp_ispd
C     HCHOp_Tot
C
C  { NOy Chemistry }
C
C     HNO3_OHNO2
C     HNO3_NO3HC
C     HNO3_N2O5
C     PANprodNet
C     PANlossNet
C     RNO3_prod
C     NOxrecycl
C     NOw_HO2
C     NOw_RO2s
C     NOw_RCO3s
C
C  { Photolysis Rates }
C
C     J_NO2
C     J_O3O1D
C
C***********************************************************************
C
C
C...  { Oxidant and Ozone Production and Loss }
C     Ox = O3, NO2, NO3*2, O, O1D, HNO3, PAN, PANX, OPAN, PNA, N2O5*3, NTR, INTR, CRON, CRPX, CRNO, CRN2
      nn =  1
      ptname(nn)  = 'OxProd'
      PA(nn) =  
     &      +        r(16)  ! OH + OH = O
     &      +(2.000)*r(24)  ! NO + NO + O2 = 2 NO2
     &      +        r(25)  ! HO2 + NO = OH + NO2
     &      +        r(42)  ! HONO + HONO = NO + NO2
     &      +        r(44)  ! HONO + OH = NO2
     &      +        r(46)  ! HNO3 + OH = NO3
     &      +(0.410)*r(50)  ! PNA = 0.59 HO2 + 0.59 NO2 + 0.41 OH + 0.41 NO3
     &      +        r(53)  ! C2O3 + NO = NO2 + MEO2 + RO2
     &      +(0.400)*r(56)  ! PAN = 0.6 NO2 + 0.6 C2O3 + 0.4 NO3 + 0.4 MEO2 + 0.4 RO2
     &      +(0.150)*r(57)  ! C2O3 + HO2 = 0.41 PACD + 0.15 AACD + 0.15 O3 + 0.44 MEO2 + 0.44 RO2 + 0.44 OH
     &      +        r(61)  ! CXO3 + NO = NO2 + ALD2 + XO2H + RO2
     &      +(0.400)*r(64)  ! PANX = 0.6 NO2 0.6 CXO3 + 0.4 NO3 + 0.4 ALD2 + 0.4 XO2H + 0.4 RO2
     &      +(0.150)*r(65)  ! CXO3 + HO2 = 0.41 PACD + 0.15 AACD + 0.15 O3 + 0.44 ALD2 + 0.44 XO2H + 0.44 RO2
     &      +        r(71)  ! MEO2 + NO  = FORM + HO2 + NO2
     &      +        r(75)  ! XO2H + NO  = NO2 + HO2
     &      +        r(79)  ! XO2 + NO  = NO2
     &      +        r(83)  ! XO2N + NO  = NTR
     &      +        r(103)  ! HCO3 + NO = FACD + NO2 + HO2
     &      +        r(173)  ! BZO2 + NO = 0.918 NO2 + 0.082 NTR + 0.918 GLY  + 0.918 OPEN + 0.918 HO2
     &      +        r(178)  ! TO2 + NO = 0.86 NO2 + 0.14 NTR + 0.417 GLY + 0.443 MGLY + 0.66 OPEN + 0.2 XOPN +
     &      +        r(183)  ! XLO2 + NO = 0.86 NO2 + 0.14 NTR + 0.221 GLY + 0.675 MGLY + 0.3 OPEN + 0.56 XOPN
     &      +(0.150)*r(216)  ! OPO3 + HO2 = 0.41 PACD + 0.15 AACD + 0.15 O3 + 0.44 ALDX + 0.44 XO2H + 0.44 RO2
C
      nn = nn + 1
      ptname(nn)  = 'OxLoss'
      PA(nn) =  
     &      +(2.000)*r(5)  ! O + NO2 = NO
     &      +(2.000)*r(7)  ! O + O3 =
     &      +        r(11)  ! O1D + H2O = 2 OH
     &      +        r(12)  ! O3 + OH = HO2
     &      +        r(13)  ! O3 + HO2 = OH
     &      +        r(14)  ! OH + O = HO2
     &      +        r(15)  ! HO2 + O = OH
     &      +        r(23)  ! H2O2 + O = OH + HO2
     &      +(2.000)*r(28)  ! NO3 = NO
     &      +(2.000)*r(30)  ! NO3 + NO2 = NO + NO2
     &      +(2.000)*r(31)  ! NO3 + O = NO2
     &      +        r(32)  ! NO3 + OH = HO2 + NO2
     &      +        r(33)  ! NO3 + HO2 = OH + NO2
     &      +(2.000)*r(34)  ! NO3 + O3 = NO2
     &      +(2.000)*r(35)  ! NO3 + NO3 = 2 NO2
     &      +        r(39)  ! N2O5 + H2O = 2 HNO3
     &      +        r(41)  ! NO + NO2 + H2O = 2 HONO
     &      +        r(99)  ! FORM + O = OH + HO2 + CO
     &      +        r(100)  ! FORM + NO3 = HNO3 + HO2 + CO
     &      +        r(105)  ! ALD2 + O = C2O3 + OH
     &      +        r(107)  ! ALD2 + NO3 = C2O3 + HNO3
     &      +        r(109)  ! ALDX + O = CXO3 + OH
     &      +        r(111)  ! ALDX + NO3 = CXO3 + HNO3
     &      +        r(115)  ! GLYD + NO3 = HNO3 + C2O3
     &      +        r(118)  ! GLY + NO3 = HNO3 + CO + HO2 + XO2 + RO2
     &      +        r(120)  ! MGLY + NO3 = HNO3 + C2O3 + XO2 + RO2
     &      +        r(137)  ! ETH + O = FORM + HO2 + CO + 0.7 XO2H + 0.7 RO2 + 0.3 OH
     &      +        r(139)  ! ETH + O3 = FORM + 0.51 CO + 0.16 HO2 + 0.16 OH + 0.37 FACD
     &      +        r(140)  ! ETH + NO3 = 0.5 NO2 + 0.5 NTR + 0.5 XO2H + 0.5 XO2 + RO2 + 1.125 FORM
     &      +        r(141)  ! OLE + O = 0.2 ALD2 + 0.3 ALDX + 0.1 HO2 + 0.2 XO2H + 0.2 CO + 0.2 FORM +
     &      +        r(143)  ! OLE + O3 = 0.295 ALD2 + 0.555 FORM + 0.27 ALDX + 0.15 XO2H + 0.15 RO2 + 0.334 OH +
     &      +        r(144)  ! OLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.500
     &      +        r(145)  ! IOLE + O = 1.24 ALD2 + 0.66 ALDX + 0.1 XO2H + 0.1 RO2 + 0.1 CO + 0.1 PAR
     &      +        r(147)  ! IOLE + O3 = 0.732 ALD2 + 0.442 ALDX + 0.128 FORM + 0.245 CO + 0.5 OH + 0.3 XO2H +
     &      +        r(148)  ! IOLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.50
     &      +        r(150)  ! ISO2 + NO = 0.117 INTR  + 0.883 NO2 + 0.803 HO2  + 0.66 FORM + 0.66 ISPD + 0.08 XO
     &      +        r(155)  ! ISOP + O3 = 0.6 FORM + 0.65 ISPD + 0.15 ALDX + 0.2 CXO3 + 0.35 PAR + 0.266 OH
     &      +        r(156)  ! ISOP + NO3 = 0.35 NO2 + 0.65 INTR + 0.64 XO2H + 0.33 XO2 + 0.03 XO2N + RO2 + 0.3
     &      +        r(158)  ! ISPD + O3 = 0.02 ALD2 + 0.15 FORM + 0.225 CO + 0.85 MGLY + 0.36 PAR + 0.114 C2O3
     &      +        r(159)  ! ISPD + NO3 = 0.643 CO + 0.282 FORM + 0.357 ALDX + 1.282 PAR + 0.85 HO2 + 0.075 CXO3
     &      +        r(164)  ! EPX2 + NO = 0.275 GLYD + 0.275 GLY + 0.275 MGLY + 0.125 OH + 0.825 HO2 + 0.375 FORM +
     &      +        r(168)  ! TERP + O = 0.15 ALDX + 5.12 PAR
     &      +        r(170)  ! TERP + O3 = 0.57 OH + 0.07 XO2H + 0.69 XO2 + 0.18 XO2N + 0.94 RO2 + 0.24 FORM +
     &      +        r(171)  ! TERP + NO3 = 0.47 NO2 + 0.28 XO2H + 0.75 XO2 + 0.25 XO2N + 1.28 RO2 + 0.47 ALDX
     &      +        r(188)  ! CRES + NO3 = 0.3 CRO + HNO3 + 0.24 XO2 + 0.36 XO2H + 0.48 ALDX + 0.24 FORM + 0.
     &      +        r(194)  ! CRNO + O3 = CRN2
     &      +        r(201)  ! XOPN + O3 = 1.2 MGLY + 0.5 OH + 0.6 C2O3 + 0.1 ALD2 + 0.5 CO + 0.3 XO2H +
     &      +        r(202)  ! XOPN + NO3 = 0.5 NO2 + 0.5 NTR + 0.45 XO2H + 0.45 XO2 + 0.1 XO2N + RO2 + 0.25
     &      +        r(205)  ! OPEN + O3 = 1.4 GLY + 0.24 MGLY + 0.5 OH + 0.12 C2O3 + 0.08 FORM + 0.02 ALD2
     &      +        r(206)  ! OPEN + NO3 = OPO3 + HNO3
     &      +        r(208)  ! CAT1 + NO3 = CRO + HNO3
C
C...  calculate ratio of P(H2O2)/P(HNO3) to use as indicator
C     of NOX or VOC sensitivity
      prod_h2o2 =  R(19) + R(20)
      prod_hno3 =  R(45)
      if (prod_hno3 .GT. 0.0) then
        ratio_ind =  prod_h2o2/prod_hno3
      else
        ratio_ind = 999.
      end if
C
C.....{ Net Ox Prod sensitive to VOC or NOx}
c      nn =  nn + 2
c      ptname(nn-1)   = 'POx_VOCsns'
c      ptname(nn) = 'POx_NOxsns'
c      POx_net = PA(1) - PA(2)
c      if (POx_net .LT. 0.0) POx_net = 0.0
C
C....increment POx
c      if (ratio_ind .LT. ratio_cut ) then
c        PA(nn-1)   = POx_net
c        PA(nn) = 0.0
c      else
c        PA(nn-1)   = 0.0
c        PA(nn) = POx_net
c      end if
C
C
C...  { Net O3 Production }
      nn =  nn + 1
      ptname(nn)   = 'PO3_net'
      O3_prod =
     &      +         r(  2) !   2  O + O2 + M = O3 +
     &      +( 0.150)*r( 57) !  57  C2O3 + HO2 = 0.41 PACD +
     &      +( 0.150)*r( 65) !  65  CXO3 + HO2 = 0.41 PACD +
     &      +( 0.150)*r(216) ! 216  OPO3 + HO2 = 0.41 PACD +
c
      O3_loss =
     &      +         r(  3) !   3  O3 + NO = NO2 +
     &      +         r(  7) !   7  O + O3 =
     &      +         r(  8) !   8  O3 = O +
     &      +         r(  9) !   9  O3 = O1D +
     &      +         r( 12) !  12  O3 + OH = HO2 +
     &      +         r( 13) !  13  O3 + HO2 = OH +
     &      +         r( 26) !  26  NO2 + O3 = NO3 +
     &      +         r( 34) !  34  NO3 + O3 = NO2 +
     &      +         r(139) ! 139  ETH + O3 = FORM +
     &      +         r(143) ! 143  OLE + O3 = 0.295 ALD2 +
     &      +         r(147) ! 147  IOLE + O3 = 0.732 ALD2 +
     &      +         r(155) ! 155  ISOP + O3 = 0.6 FORM +
     &      +         r(158) ! 158  ISPD + O3 = 0.02 ALD2 +
     &      +         r(170) ! 170  TERP + O3 = 0.57 OH +
     &      +         r(194) ! 194  CRNO + O3 = CRN2 +
     &      +         r(201) ! 201  XOPN + O3 = 1.2 MGLY +
     &      +         r(205) ! 205  OPEN + O3 = 1.4 GLY +
c
      PA(nn) = O3_prod - O3_loss
C
C...  { Net O3 Prod sensitive to VOC or NOx}
      nn =  nn + 2
      ptname(nn-1) = 'PO3_VOCsns'
      ptname(nn)   = 'PO3_NOxsns'
      if (O3_prod - O3_loss .GT. 0.0) then
        if (ratio_ind .LT. ratio_cut ) then
          PA(nn-1) = O3_prod - O3_loss
          PA(nn)   = 0.0
        else
          PA(nn-1) = 0.0
          PA(nn)   = O3_prod - O3_loss
        endif
      else
        PA(nn-1) = 0.0
        PA(nn)   = 0.0
      endif
C
C...  { Value of PHNO3/PH2O2 used as VOC vs NOx indicator}
      if(.NOT. lcpacum) then
        nn =  nn + 1
        ptname(nn)   = 'PH2O2_PHN3'
        if (ldark) then
          PA(nn) = -999. * (dtfact/ppbfact)
        else
          PA(nn) = ratio_ind * (dtfact/ppbfact)
        endif
      endif
C
C...  { O3 destruction by several pathways }
C
      nn =  nn + 1
      ptname(nn)   = 'O3_dest'
C...  O1D + water
      PA(nn) =
     &      -( 1.000)*r( 11) !  11  O1D + H2O = 2 OH
C
C...  HO2 + O3 (assuming no OH is recycled via OH+H2O2)
      PA(nn) =  PA(nn)
     &      -( 1.000)*r( 13) !  13  O3 + HO2 = OH +
C
C...  OH + O3 (accounting for any NO2 formed)
C
C     ho2tmp is HO2 destruction without NO2 production
      ho2tmp = 
     &      +         r(13)  ! O3 + HO2 = OH
     &      +         r(15)  ! HO2 + O = OH
     &      +         r(18)  ! OH + HO2 =
     &      +( 2.000)*r(19)  ! HO2 + HO2 = H2O2
     &      +( 2.000)*r(20)  ! HO2 + HO2 + H2O = H2O2
     &      +         r(57)  ! C2O3 + HO2 = 0.41 PACD + 0.15 AACD + 0.15 O3 + 0.44 MEO2 + 0.44 RO2 + 0.44 OH
     &      +         r(65)  ! CXO3 + HO2 = 0.41 PACD + 0.15 AACD + 0.15 O3 + 0.44 ALD2 + 0.44 XO2H + 0.44 RO2
     &      +         r(72)  ! MEO2 + HO2  = 0.9 MEPX + 0.1 FORM
     &      +         r(76)  ! XO2H + HO2  = ROOH
     &      +         r(80)  ! XO2 + HO2  = ROOH
     &      +         r(84)  ! XO2N + HO2  = ROOH
     &      +         r(101)  ! FORM + HO2 = HCO3
     &      +( 0.800)*r(104)  ! HCO3 + HO2 = 0.5 MEPX + 0.5 FACD + 0.2 OH + 0.2 HO2
     &      +( 0.880)*r(151)  ! ISO2 + HO2 = 0.88 ISPX + 0.12 OH + 0.12 HO2 + 0.12 FORM + 0.12 ISPD
C     &      +( 0.175)*r(163)  ! EPX2 + HO2 = 0.275 GLYD + 0.275 GLY + 0.275 MGLY + 1.125 OH + 0.825 HO2 + 0.375 FORM
     &      +         r(175)  ! BZO2 + HO2 =
     &      +         r(180)  ! TO2 + HO2 =
     &      +         r(184)  ! XLO2 + HO2 =
     &      +         r(190)  ! CRO + HO2 = CRES
     &      +         r(196)  ! CRN2 + HO2 = CRPX
     &      +         r(210)  ! CAO2 + HO2 =
     &      +         r(216)  ! OPO3 + HO2 = 0.41 PACD + 0.15 AACD + 0.15 O3 + 0.44 ALDX + 0.44 XO2H + 0.44 RO2
C
C      12  O3 + OH = HO2
C      25 HO2 + NO = OH + NO2
C      33 NO3 + HO2 = OH + NO2
C
      PA(nn) =  PA(nn)
     &       - R(12) * ho2tmp
     &         / ( ho2tmp+R(25)+R(33) )
C
C...  O(3P) + VOC
      PA(nn) =  PA(nn)
     &      -         r( 99) !  99  FORM + O = OH +
     &      -         r(105) ! 105  ALD2 + O = C2O3 +
     &      -         r(109) ! 109  ALDX + O = CXO3 +
     &      -         r(137) ! 137  ETH + O = FORM +
     &      -         r(141) ! 141  OLE + O = 0.2 ALD2 +
     &      -         r(145) ! 145  IOLE + O = 1.24 ALD2 +
     &      -         r(168) ! 168  TERP + O = 0.15 ALDX +
C...  O3 + VOC
      PA(nn) =  PA(nn)
     &      -         r(139) ! 139  ETH + O3 = FORM +
     &      -         r(143) ! 143  OLE + O3 = 0.295 ALD2 +
     &      -         r(147) ! 147  IOLE + O3 = 0.732 ALD2 +
     &      -         r(155) ! 155  ISOP + O3 = 0.6 FORM +
     &      -         r(158) ! 158  ISPD + O3 = 0.02 ALD2 +
     &      -         r(170) ! 170  TERP + O3 = 0.57 OH +
     &      -         r(194) ! 194  CRNO + O3 = CRN2 +
     &      -         r(201) ! 201  XOPN + O3 = 1.2 MGLY +
     &      -         r(205) ! 205  OPEN + O3 = 1.4 GLY +
c
      PA(nn) = amin1( PA(nn), (O3_prod - O3_loss) )
c
c
C--------------------End of Ox & O3-----------------------
c
c
C...  { Radical Reservoirs: HONO, HNO4, H2O2  etc }
C     We will call these termination rxns if there is net production
C     during a time step, or initiation if there is net release of HOx.
c
c    40  NO + OH = HONO
c    43  HONO = NO +  OH
c
      sum =  R(40) - R(43)          ! HONO cycle
      if (sum.GT.0.) then
         cyc_HONO_p = sum
         cyc_HONO_l = 0.0
      else
         cyc_HONO_p =  0.0
         cyc_HONO_l = -sum
      end if
c
c     48  HO2 + NO2 = PNA
c     49  PNA = HO2 + NO2
c     50  PNA = 0.59 HO2 + 0.59 NO2 + 0.41 OH + 0.41 NO3
c     51  PNA + OH = NO2
c
      sum =  R(48) - R(49) - 0.59*R(50) - R(51)  ! HNO4 cycle
      if (sum.GT.0.) then
         cyc_HNO4_p = sum
         cyc_HNO4_l = 0.0
      else
         cyc_HNO4_p =  0.0
         cyc_HNO4_l = -sum
      end if
c
c     19  HO2 + HO2 = H2O2
c     20  HO2 + HO2 + H2O = H2O2
c     17  OH + OH = H2O2
c     21  H2O2 = 2 OH
c     22  H2O2 + OH = HO2
c     23  H2O2 + O = OH + HO2
c
      sum =  R(17) + R(19) + R(20) - R(21) - 0.5*R(22) - 0.5*R(23)  ! H2O2 cycle
      if (sum.GT.0.) then
         cyc_H2O2_p = 2.*sum
         cyc_H2O2_l = 0.0
      else
         cyc_H2O2_p =  0.0
         cyc_H2O2_l = -2.0*sum
      end if
c
c     101  FORM + HO2 = HCO3
c     102  HCO3 = FORM + HO2
c
      sum =  R(101) - R(102)     ! HCO3 cycle
      if (sum.GT.0.) then
         cyc_HCO3_p = sum
         cyc_HCO3_l = 0.0
      else
         cyc_HCO3_p =  0.0
         cyc_HCO3_l = -sum
      end if
c
c     72  MEO2 + HO2  = 0.9 MEPX + 0.1 FORM
c     87  MEPX + OH  = 0.6 MEO2 + 0.6 RO2 + 0.4 FORM + 0.4 OH
c     88  MEPX  = MEO2 + RO2 + OH
c
      sum =  0.9*R(72) - R(87) - R(88)   ! MEPX cycle
      if (sum.GT.0.) then
         cyc_MEPX_p = sum
         cyc_MEPX_l = 0.0
      else
         cyc_MEPX_p = 0.0
         cyc_MEPX_l = sum
      end if
c
c     76  XO2H + HO2  = ROOH
c     80  XO2 + HO2  = ROOH
c     84  XO2N + HO2  = ROOH
c     89  ROOH + OH  = 0.54 XO2H + 0.06 XO2N + 0.6 RO2 + 0.4 OH
c     90  ROOH  = HO2 + OH
c
      sum =  R(76) + R(80) + R(84) - R(89) - R(90)   ! ROOH cycle
      if (sum.GT.0.) then
         cyc_ROOH_p = sum
         cyc_ROOH_l = 0.0
      else
         cyc_ROOH_p = 0.0
         cyc_ROOH_l = sum
      end if
c
c     54  C2O3 + NO2 = PAN
c     55  PAN = NO2 + C2O3
c     56  PAN = 0.6 NO2 + 0.6 C2O3 + 0.4 NO3 + 0.4 MEO2 + 0.4 RO2
c
      sum =  R(55) - R(56) - R(54)       ! PAN Cycle
      if (sum.GT.0.) then
         cyc_PAN_p = sum
         cyc_PAN_l = 0.0
      else
         cyc_PAN_p =  0.0
         cyc_PAN_l = -sum
      end if
c
c     62  CXO3 + NO2 = PANX
c     63  PANX = NO2 + CXO3
c     64  PANX = 0.6 NO2 0.6 CXO3 + 0.4 NO3 + 0.4 ALD2 + 0.4 XO2H + 0.4 RO2
c
      sum =  R(62) - R(63) - R(64)    ! PANX Cycle
      if (sum.GT.0.) then
         cyc_PANX_p = sum
         cyc_PANX_l = 0.0
      else
         cyc_PANX_p =  0.0
         cyc_PANX_l = -sum
      end if
c
c     214  OPO3 + NO2 = OPAN
c     215  OPAN = OPO3 + NO2
c
      sum =  R(214) - R(215)    ! OPAN Cycle
      if (sum.GT.0.) then
         cyc_OPAN_p = sum
         cyc_OPAN_l = 0.0
      else
         cyc_OPAN_p =  0.0
         cyc_OPAN_l = -sum
      end if
C
C------------------End of Radical Reservoirs----------------
C
C
C     {      R a d i c a l    I n i t i a t i o n       }
C
C
C     CB6 has recursive radical production that must be
C     considered when tracking radical initiation, i.e.:
C     MEO2 makes some HO2
C     XO2H makes some HO2
C     C2O3 makes some MEO2
C     skip CXO3 makes MEO2 ! 112 CXO3 C2O3 = MEO2 XO2 HO2 ALD2
C
C
C...  Fractional yield of HO2 from MEO2
c     71  MEO2 + NO  = FORM + HO2 + NO2
c     72  MEO2 + HO2  = 0.9 MEPX + 0.1 FORM
c     73  MEO2 + C2O3 = FORM + 0.9 HO2 + 0.9 MEO2 + 0.1 AACD + 0.9 RO2
c     74  MEO2 + RO2  = 0.685 FORM  + 0.315 MEOH + 0.37 HO2 + RO2
      ho2_frac_meo2 = ( r(71)+0.9*r(73)+0.37*r(74) ) /
     &                   (  r(71)+r(72)+r(73)+r(74) )
C...  Fractional yield of HO2 from XO2H
c     75  XO2H + NO  = NO2 + HO2
c     76  XO2H + HO2  = ROOH
c     77  XO2H + C2O3 = 0.8 HO2 + 0.8 MEO2 + 0.2 AACD + 0.8 RO2
c     78  XO2H + RO2  = 0.6 HO2 + RO2
      ho2_frac_xo2h = ( r(75)+0.8*r(77)+0.6*r(78) ) /
     &                   (  r(75)+r(76)+r(77)+r(78) )
C
C...  Fractional yield of MEO2 from C2O3
c     54  C2O3 + NO2 = PAN
c     57  C2O3 + HO2 = 0.41 PACD + 0.15 AACD + 0.15 O3 + 0.44 MEO2 + 0.44 RO2 + 0.44 OH
c     58  C2O3 + RO2 = C2O3
c     59  C2O3 + C2O3  = 2 MEO2 + 2 RO2
c     60  C2O3 + CXO3  = MEO2 + ALD2 + XO2H + 2 RO2
c     73  MEO2 + C2O3 = FORM + 0.9 HO2 + 0.9 MEO2 + 0.1 AACD + 0.9 RO2
c     77  XO2H + C2O3 = 0.8 HO2 + 0.8 MEO2 + 0.2 AACD + 0.8 RO2
c     81  XO2 + C2O3 = 0.8 MEO2 + 0.2 AACD + 0.8 RO2
c     85  XO2N + C2O3 = 0.8 HO2 + 0.8 MEO2 + 0.2 AACD + 0.8 RO2
c     152  ISO2 + C2O3 = 0.709 HO2 + 0.583 FORM + 0.583 ISPD + 0.071 XO2H + 0.044 IOLE + 0.037 G
c     165  EPX2 + C2O3 = 0.22 GLYD + 0.22 GLY + 0.22 MGLY + 0.1 OH + 0.66 HO2 + 0.3 FORM
c     174  BZO2 + C2O3 = GLY + OPEN + HO2 + MEO2 + RO2
c     179  TO2 + C2O3 = 0.48 GLY + 0.52 MGLY + 0.77 OPEN + 0.23 XOPN + HO2 + MEO2 + RO2
c     185  XLO2 + C2O3 = 0.26 GLY + 0.77 MGLY + 0.35 OPEN + 0.65 XOPN + HO2 + MEO2 + RO2
c     211  CAO2 + C2O3 = HO2 + 0.4 GLY + MEO2 + RO2
c     217  OPO3 + C2O3 = MEO2 + XO2 +ALDX + 2 RO2
C
      tmp =  r(53)+0.44*r(57)+2.0*r(59)+r(60)+0.9*r(73)+
     &        0.8*( r(77)+r(81)+r(85)+r(152)+r(165))+
     &           r(174)+r(179)+r(185)+r(211)+r(217) 
      meo2_frac_c2o3 = tmp / ( tmp+0.66*r(57)+0.1*r(73)+
     &        0.2*( r(77)+r(81)+r(85)+r(152)+r(165))+
     &           cyc_PAN_p )
C
C...  Fractional yield of XO2H from CXO3
c     60  C2O3 + CXO3  = MEO2 + ALD2 + XO2H + 2 RO2
c     61  CXO3 + NO = NO2 + ALD2 + XO2H + RO2
c     62  CXO3 + NO2 = PANX
c     65  CXO3 + HO2 = 0.41 PACD + 0.15 AACD + 0.15 O3 + 0.44 ALD2 + 0.44 XO2H + 0.44 RO2
c     67  CXO3 + CXO3  = 2 ALD2 + 2 XO2H + 2 RO2
C
      xo2h_frac_cxo3 = ( r(60)+r(61)+0.44*r(65)+r(67) ) /
     &           ( r(60)+r(61)+cyc_PANX_p+r(65)+r(67) )
C
C...  Fractional yield of XO2H from OPAN
c     213  OPO3 + NO = NO2 + XO2H + RO2 + ALDX
c     214  OPO3 + NO2 = OPAN
c     216  OPO3 + HO2 = 0.41 PACD + 0.15 AACD + 0.15 O3 + 0.44 ALDX + 0.44 XO2H + 0.44 RO2
c     217  OPO3 + C2O3 = MEO2 + XO2 +ALDX + 2 RO2
c     218  OPO3 + RO2 = 0.8 XO2H + 0.8 RO2 + 0.8 ALDX + 0.2 AACD
C
      xo2h_frac_opo3 = ( r(213)+0.44*r(216)+r(217)+0.8*r(218) ) /
     &              ( r(213)+cyc_OPAN_p+r(216)+r(217)+r(218) )
C
C
        nn = nn + 1
        ptname(nn)  = 'HO2frMEO2'
        PA(nn) = ho2_frac_meo2 * (dtfact/ppbfact)
C
        nn = nn + 1
        ptname(nn)  = 'MEO2frC2O3'
        PA(nn) = meo2_frac_c2o3 * (dtfact/ppbfact)
C
        nn = nn + 1
        ptname(nn)  = 'HO2frXO2H'
        PA(nn) = ho2_frac_xo2h * (dtfact/ppbfact)
C
        nn = nn + 1
        ptname(nn)  = 'XO2HfrCXO3'
        PA(nn) = xo2h_frac_cxo3 * (dtfact/ppbfact)
C
        nn = nn + 1
        ptname(nn)  = 'XO2HfrOPO3'
        PA(nn) = xo2h_frac_opo3 * (dtfact/ppbfact)
C
C
C...  { New HOx: OH or HO2 produced without HOx as a reactant }
C     new radicals come from photolysis and ozone reactions
C     include radicals from O(1D) and O atoms because they result from photolysis
C     include radicals from NO3 reactions because they result from O3 or O atoms
C     include HO2 from C2O3 from photolysis of MGLY or OPEN
C     don't include radicals from ROR and TO2 because they result from OH
C
C...  new OH
      OH_new =
     &      +( 2.000)*r(11)  ! O1D + H2O = 2 OH
     &      +( 2.000)*r(21)  ! H2O2 = 2 OH
     &      +( 1.000)*r(23)  ! H2O2 + O = OH + HO2
     &      +( 1.000)*r(43)  ! HONO = NO + OH
     &      +( 1.000)*r(47)  ! HNO3 = OH + NO2
     &      +( 0.410)*r(50)  ! PNA = 0.59 HO2 + 0.59 NO2 + 0.41 OH + 0.41 NO3
     &      +( 1.000)*r(88)  ! MEPX  = MEO2 + RO2 + OH
     &      +( 1.000)*r(90)  ! ROOH  = HO2 + OH
     &      +( 1.000)*r(99)  ! FORM + O = OH + HO2 + CO
     &      +( 1.000)*r(105)  ! ALD2 + O = C2O3 + OH
     &      +( 1.000)*r(109)  ! ALDX + O = CXO3 + OH
     &      +( 0.190)*r(114)  ! GLYD = 0.74 FORM + 0.89 CO + 1.4 HO2 + 0.15 MEOH + 0.19 OH + 0.11 GLY + 0.110
     &      +( 0.300)*r(137)  ! ETH + O = FORM + HO2 + CO + 0.7 XO2H + 0.7 RO2 + 0.3 OH
     &      +( 0.160)*r(139)  ! ETH + O3 = FORM + 0.51 CO + 0.16 HO2 + 0.16 OH + 0.37 FACD
     &      +( 0.100)*r(141)  ! OLE + O = 0.2 ALD2 + 0.3 ALDX + 0.1 HO2 + 0.2 XO2H + 0.2 CO + 0.2 FORM +
     &      +( 0.334)*r(143)  ! OLE + O3 = 0.295 ALD2 + 0.555 FORM + 0.27 ALDX + 0.15 XO2H + 0.15 RO2 + 0.334 OH +
     &      +( 0.500)*r(147)  ! IOLE + O3 = 0.732 ALD2 + 0.442 ALDX + 0.128 FORM + 0.245 CO + 0.5 OH + 0.3 XO2H +
     &      +( 0.266)*r(155)  ! ISOP + O3 = 0.6 FORM + 0.65 ISPD + 0.15 ALDX + 0.2 CXO3 + 0.35 PAR + 0.266 OH
     &      +( 0.268)*r(158)  ! ISPD + O3 = 0.02 ALD2 + 0.15 FORM + 0.225 CO + 0.85 MGLY + 0.36 PAR + 0.114 C2O3
     &      +( 0.570)*r(170)  ! TERP + O3 = 0.57 OH + 0.07 XO2H + 0.69 XO2 + 0.18 XO2N + 0.94 RO2 + 0.24 FORM +
     &      +( 1.000)*r(197)  ! CRPX = CRNO + OH
     &      +( 0.500)*r(201)  ! XOPN + O3 = 1.2 MGLY + 0.5 OH + 0.6 C2O3 + 0.1 ALD2 + 0.5 CO + 0.3 XO2H +
     &      +( 0.500)*r(205)  ! OPEN + O3 = 1.4 GLY + 0.24 MGLY + 0.5 OH + 0.12 C2O3 + 0.08 FORM + 0.02 ALD2
C...  new HO2
      HO2_new =
     &      +         r(23)  ! H2O2 + O = OH + HO2
c in cyc_HNO4_     &      +         r(49)  ! PNA = HO2 + NO2
     &      +         r(90)  ! ROOH  = HO2 + OH
     &      +( 2.000)*r(97)  ! FORM = 2 HO2 + CO
     &      +         r(99)  ! FORM + O = OH + HO2 + CO
     &      +         r(100)  ! FORM + NO3 = HNO3 + HO2 + CO
     &      +         r(102)  ! HCO3 = FORM + HO2
     &      +         r(103)  ! HCO3 + NO = FACD + NO2 + HO2
     &      +         r(108)  ! ALD2 = MEO2 + RO2 + CO + HO2
     &      +         r(112)  ! ALDX = ALD2 + XO2H + RO2 + CO + HO2
     &      +( 1.400)*r(114)  ! GLYD = 0.74 FORM + 0.89 CO + 1.4 HO2 + 0.15 MEOH + 0.19 OH + 0.11 GLY + 0.110
     &      +( 2.000)*r(117)  ! GLY = 2 HO2 + 2 CO
     &      +         r(118)  ! GLY + NO3 = HNO3 + CO + HO2 + XO2 + RO2
     &      +         r(119)  ! MGLY = C2O3 + HO2 + CO
     &      +         r(137)  ! ETH + O = FORM + HO2 + CO + 0.7 XO2H + 0.7 RO2 + 0.3 OH
     &      +( 0.160)*r(139)  ! ETH + O3 = FORM + 0.51 CO + 0.16 HO2 + 0.16 OH + 0.37 FACD
     &      +( 0.100)*r(141)  ! OLE + O = 0.2 ALD2 + 0.3 ALDX + 0.1 HO2 + 0.2 XO2H + 0.2 CO + 0.2 FORM +
     &      +( 0.080)*r(143)  ! OLE + O3 = 0.295 ALD2 + 0.555 FORM + 0.27 ALDX + 0.15 XO2H + 0.15 RO2 + 0.334 OH +
     &      +( 0.066)*r(155)  ! ISOP + O3 = 0.6 FORM + 0.65 ISPD + 0.15 ALDX + 0.2 CXO3 + 0.35 PAR + 0.266 OH
     &      +( 0.090)*r(158)  ! ISPD + O3 = 0.02 ALD2 + 0.15 FORM + 0.225 CO + 0.85 MGLY + 0.36 PAR + 0.114 C2O3
     &      +( 0.850)*r(159)  ! ISPD + NO3 = 0.643 CO + 0.282 FORM + 0.357 ALDX + 1.282 PAR + 0.85 HO2 + 0.075 CXO3
     &      +( 0.700)*r(199)  ! XOPN = CAO2 + 0.7 HO2 + 0.7 CO + 0.3 C2O3 + RO2
     &      +         r(203)  ! OPEN = OPO3 + HO2 + CO
     &      +( 0.560)*r(205)  ! OPEN + O3 = 1.4 GLY + 0.24 MGLY + 0.5 OH + 0.12 C2O3 + 0.08 FORM + 0.02 ALD2
C
C...  plus HO2 from decomposition of PNA,PAN,PANX, OPAN
C     &      +( 1.000)*r(49)  ! PNA = HO2 + NO2
C     &      +( 0.590)*r(50)  ! PNA = 0.59 HO2 + 0.59 NO2 + 0.41 OH + 0.41 NO3
C     90 PAN = C2O3 NO2
C     91 PAN = C2O3 NO2
C    105 PANX = CXO3 NO2
C    106 PANX = CXO3 NO2
C    107 PANX OH = ALD2 NO2
      HO2_new = HO2_new
     &                + cyc_HNO4_l
     &                +(cyc_PAN_l*ho2_frac_meo2*meo2_frac_c2o3)
     &                +(cyc_PANX_l*ho2_frac_xo2h*xo2h_frac_cxo3)
     &                +(cyc_OPAN_l*ho2_frac_xo2h*xo2h_frac_opo3)
C
C... new HO2 via MEO2 from photolysis reactions
c      +( 1.000)*r(88)  ! MEPX  = MEO2 + RO2 + OH
c      +( 1.000)*r(108)  ! ALD2 = MEO2 + RO2 + CO + HO2
c      +( 0.500)*r(128)  ! KET = 0.5 ALD2 + 0.5 C2O3 + 0.5 XO2H + 0.5 CXO3 + 0.5 MEO2 + RO2 + -2.5 P
c      +( 1.380)*r(129)  ! ACET = 0.38 CO + 1.38 MEO2 + 1.38 RO2 + 0.62 C2O3
      HO2_new = HO2_new
     & +( r(88)+r(108)+0.5*r(128)+r(129) )
     &               *ho2_frac_meo2
C
C... new HO2 via XO2H from photolysis reactions
c      +( 1.000)*r(92)  ! NTR = NO2 + XO2H + RO2
c      +( 1.000)*r(112)  ! ALDX = ALD2 + XO2H + RO2 + CO + HO2
c      +( 0.110)*r(114)  ! GLYD = 0.74 FORM + 0.89 CO + 1.4 HO2 + 0.15 MEOH + 0.19 OH + 0.11 GLY + 0.110
c      +( 0.500)*r(128)  ! KET = 0.5 ALD2 + 0.5 C2O3 + 0.5 XO2H + 0.5 CXO3 + 0.5 MEO2 + RO2 + -2.5 P
c      +( 0.700)*r(160)  ! ISPD = 0.333 CO + 0.067 ALD2 + 0.9 FORM + 0.832 PAR + 0.333 HO2 + 0.7 XO2H + 0.7
      HO2_new = HO2_new
     &     +( r(92)+r(112)+0.11*r(114)+0.5*r(128)
     &            +0.7*r(160) )
     &               *ho2_frac_xo2h
C
C... new HO2 via C2O3 from photolysis reactions
c      +( 1.000)*r(119)  ! MGLY = C2O3 + HO2 + CO
c      +( 0.500)*r(128)  ! KET = 0.5 ALD2 + 0.5 C2O3 + 0.5 XO2H + 0.5 CXO3 + 0.5 MEO2 + RO2 + -2.5 P
c      +( 0.620)*r(129)  ! ACET = 0.38 CO + 1.38 MEO2 + 1.38 RO2 + 0.62 C2O3
c      +( 0.967)*r(160)  ! ISPD = 0.333 CO + 0.067 ALD2 + 0.9 FORM + 0.832 PAR + 0.333 HO2 + 0.7 XO2H + 0.7
c      +( 0.300)*r(199)  ! XOPN = CAO2 + 0.7 HO2 + 0.7 CO + 0.3 C2O3 + RO2
      HO2_new = HO2_new
     &       +( r(119)+0.5*r(128)+0.62*r(129)
     &             +0.967*r(160)+0.3*r(199) )
     &               *ho2_frac_meo2*meo2_frac_c2o3
C
C... new HO2 via CXO3 from photolysis reactions
      HO2_new = HO2_new
c      +( 0.500)*r(128)  ! KET = 0.5 ALD2 + 0.5 C2O3 + 0.5 XO2H + 0.5 CXO3 + 0.5 MEO2 + RO2 + -2.5 P
      HO2_new = HO2_new
     &         +( 0.5*r(128) )
     &               *ho2_frac_xo2h*xo2h_frac_cxo3
C
C... new HO2 via MEO2 from NO3 reactions
C    none
C
C... new HO2 via XO2H from NO3 reactions
c      +( 0.500)*r(140)  ! ETH + NO3 = 0.5 NO2 + 0.5 NTR + 0.5 XO2H + 0.5 XO2 + RO2 + 1.125 FORM
c      +( 0.480)*r(144)  ! OLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.500
c      +( 0.480)*r(148)  ! IOLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.50
c      +( 0.640)*r(156)  ! ISOP + NO3 = 0.35 NO2 + 0.65 INTR + 0.64 XO2H + 0.33 XO2 + 0.03 XO2N + RO2 + 0.3
c      +( 0.075)*r(159)  ! ISPD + NO3 = 0.643 CO + 0.282 FORM + 0.357 ALDX + 1.282 PAR + 0.85 HO2 + 0.075 CXO3
c      +( 0.280)*r(171)  ! TERP + NO3 = 0.47 NO2 + 0.28 XO2H + 0.75 XO2 + 0.25 XO2N + 1.28 RO2 + 0.47 ALDX
c      +( 0.360)*r(188)  ! CRES + NO3 = 0.3 CRO + HNO3 + 0.24 XO2 + 0.36 XO2H + 0.48 ALDX + 0.24 FORM + 0.
c      +( 0.450)*r(202)  ! XOPN + NO3 = 0.5 NO2 + 0.5 NTR + 0.45 XO2H + 0.45 XO2 + 0.1 XO2N + RO2 + 0.25
      HO2_new = HO2_new
     &   +( 0.5*r(140)+0.48*r(144)+0.48*r(148)+0.64*r(156)
     &     +0.075*r(159)+0.28*r(171)+0.36*r(188)+0.45*r(202) )
     &               *ho2_frac_xo2h
C
C... new HO2 via C2O3 from NO3 reactions
c      +( 1.000)*r(107)  ! ALD2 + NO3 = C2O3 + HNO3
c      +( 1.000)*r(115)  ! GLYD + NO3 = HNO3 + C2O3
c      +( 1.000)*r(120)  ! MGLY + NO3 = HNO3 + C2O3 + XO2 + RO2
      HO2_new = HO2_new
     &    +( r(107)+r(115)+r(120) )
     &               *ho2_frac_meo2*meo2_frac_c2o3
C
C... new HO2 via CXO3 from NO3 reactions
c      +( 1.000)*r(111)  ! ALDX + NO3 = CXO3 + HNO3
c      +( 0.075)*r(159)  ! ISPD + NO3 = 0.643 CO + 0.282 FORM + 0.357 ALDX + 1.282 PAR + 0.85 HO2 + 0.075 CXO3
      HO2_new = HO2_new
     &          +( r(111)+0.075*r(159) )
     &               *ho2_frac_xo2h*xo2h_frac_cxo3
C
C... new HO2 via MEO2 from O3 reactions
C    none
C
C... new HO2 via XO2H from O3 reactions
c      +( 0.150)*r(143)  ! OLE + O3 = 0.295 ALD2 + 0.555 FORM + 0.27 ALDX + 0.15 XO2H + 0.15 RO2 + 0.334 OH +
c      +( 0.300)*r(147)  ! IOLE + O3 = 0.732 ALD2 + 0.442 ALDX + 0.128 FORM + 0.245 CO + 0.5 OH + 0.3 XO2H +
c      +( 0.064)*r(158)  ! ISPD + O3 = 0.02 ALD2 + 0.15 FORM + 0.225 CO + 0.85 MGLY + 0.36 PAR + 0.114 C2O3
c      +( 0.070)*r(170)  ! TERP + O3 = 0.57 OH + 0.07 XO2H + 0.69 XO2 + 0.18 XO2N + 0.94 RO2 + 0.24 FORM +
c      +( 0.300)*r(201)  ! XOPN + O3 = 1.2 MGLY + 0.5 OH + 0.6 C2O3 + 0.1 ALD2 + 0.5 CO + 0.3 XO2H +
      HO2_new = HO2_new
     &  +( 0.15*r(143)+0.3*r(147)+0.064*r(158)
     &          +0.07*r(170)+0.3*r(201) )
     &               *ho2_frac_xo2h
C
C... new HO2 via C2O3 from O3 reactions
c      +( 0.114)*r(158)  ! ISPD + O3 = 0.02 ALD2 + 0.15 FORM + 0.225 CO + 0.85 MGLY + 0.36 PAR + 0.114 C2O3
c      +( 0.600)*r(201)  ! XOPN + O3 = 1.2 MGLY + 0.5 OH + 0.6 C2O3 + 0.1 ALD2 + 0.5 CO + 0.3 XO2H +
c      +( 0.120)*r(205)  ! OPEN + O3 = 1.4 GLY + 0.24 MGLY + 0.5 OH + 0.12 C2O3 + 0.08 FORM + 0.02 ALD2
      HO2_new = HO2_new
     &          +( 0.114*r(158)+0.6*r(201)+0.12*r(205) )
     &               *ho2_frac_meo2*meo2_frac_c2o3
C
C... new HO2 via CXO3 from O3 reactions
c      +( 0.200)*r(155)  ! ISOP + O3 = 0.6 FORM + 0.65 ISPD + 0.15 ALDX + 0.2 CXO3 + 0.35 PAR + 0.266 OH
c      +( 0.390)*r(170)  ! TERP + O3 = 0.57 OH + 0.07 XO2H + 0.69 XO2 + 0.18 XO2N + 0.94 RO2 + 0.24 FORM +
      HO2_new = HO2_new
     &                +( 0.2*r(155)+0.39*r(170) )
     &               *ho2_frac_xo2h*xo2h_frac_cxo3
C
C... new HO2 via MEO2 from O atom reactions
C    none
C
C... new HO2 via XO2H from O atom reactions
c      +( 0.700)*r(137)  ! ETH + O = FORM + HO2 + CO + 0.7 XO2H + 0.7 RO2 + 0.3 OH
c      +( 0.200)*r(141)  ! OLE + O = 0.2 ALD2 + 0.3 ALDX + 0.1 HO2 + 0.2 XO2H + 0.2 CO + 0.2 FORM +
c      +( 0.100)*r(145)  ! IOLE + O = 1.24 ALD2 + 0.66 ALDX + 0.1 XO2H + 0.1 RO2 + 0.1 CO + 0.1 PAR
      HO2_new = HO2_new
     &      +( 0.7*r(137)+0.2*r(141)+0.1*r(145) )
     &               *ho2_frac_xo2h
C
C... new HO2 via C2O3 from O atom reactions
c      +( 1.000)*r(105)  ! ALD2 + O = C2O3 + OH
      HO2_new = HO2_new
     &          +( r(105) )
     &               *ho2_frac_meo2*meo2_frac_c2o3
C
C... new HO2 via CXO3 from O atom reactions
c      +( 1.000)*r(109)  ! ALDX + O = CXO3 + OH
      HO2_new = HO2_new
     &          +( r(109) )
     &               *ho2_frac_xo2h*xo2h_frac_cxo3
C
      HOx_new = OH_new + HO2_new
C
      nn = nn + 1
      ptname(nn)  = 'OH_new'
      PA(nn) = OH_new
C
      nn = nn + 1
      ptname(nn)  = 'HO2_new'
      PA(nn) = HO2_new
C
      nn = nn + 1
      ptname(nn)  = 'HOx_new'
      PA(nn) = HOx_new
C
C...  { New OH from O1D+H2O}
      nn = nn + 1
      ptname(nn)  = 'newOH_O1D'
      PA(nn) = 2.*R(11)   !O1D+H2O=2*OH
C
C...  { New OH from HONO }
      nn = nn + 1
      ptname(nn)  = 'newOH_HONO'
      PA(nn) = cyc_HONO_l  
C
C...  { New OH from O3 + olefins (only first generation OH) }
      nn = nn + 1
      ptname(nn)  = 'nOH_O3_OLE'
      PA(nn) = 
     &      +( 0.160)*r(139)  ! ETH + O3 = FORM + 0.51 CO + 0.16 HO2 + 0.16 OH + 0.37 FACD
     &      +( 0.334)*r(143)  ! OLE + O3 = 0.295 ALD2 + 0.555 FORM + 0.27 ALDX + 0.15 XO2H + 0.15 RO2 + 0.334 OH +
     &      +( 0.500)*r(147)  ! IOLE + O3 = 0.732 ALD2 + 0.442 ALDX + 0.128 FORM + 0.245 CO + 0.5 OH + 0.3 XO2H +
     &      +( 0.266)*r(155)  ! ISOP + O3 = 0.6 FORM + 0.65 ISPD + 0.15 ALDX + 0.2 CXO3 + 0.35 PAR + 0.266 OH
     &      +( 0.268)*r(158)  ! ISPD + O3 = 0.02 ALD2 + 0.15 FORM + 0.225 CO + 0.85 MGLY + 0.36 PAR + 0.114 C2O3
     &      +( 0.570)*r(170)  ! TERP + O3 = 0.57 OH + 0.07 XO2H + 0.69 XO2 + 0.18 XO2N + 0.94 RO2 + 0.24 FORM +
     &      +( 0.500)*r(201)  ! XOPN + O3 = 1.2 MGLY + 0.5 OH + 0.6 C2O3 + 0.1 ALD2 + 0.5 CO + 0.3 XO2H +
     &      +( 0.500)*r(205)  ! OPEN + O3 = 1.4 GLY + 0.24 MGLY + 0.5 OH + 0.12 C2O3 + 0.08 FORM + 0.02 ALD2
C
C...  { New HO2 from HCHO }
      nn = nn + 1
      ptname(nn)  = 'nwHO2_HCHO'
      PA(nn) = 
     &      +( 2.000)*r(97)  ! FORM = 2 HO2 + CO
     &      +( 1.000)*r(99)  ! FORM + O = OH + HO2 + CO
     &      +( 1.000)*r(100)  ! FORM + NO3 = HNO3 + HO2 + CO
C
C...  { New RO2 Production }
C
C...  Rad initiation for C2O3 
C     nn = nn + 1
C     ptname(nn)  = 'newC2O3'
C...  new C2O3
      RO2_new = cyc_PAN_l
     &      +         r(119)  ! MGLY = C2O3 + HO2 + CO
     &      +( 0.500)*r(128)  ! KET = 0.5 ALD2 + 0.5 C2O3 + 0.5 XO2H + 0.5 CXO3 + 0.5 MEO2 + RO2 + -2.5 P
     &      +( 0.620)*r(129)  ! ACET = 0.38 CO + 1.38 MEO2 + 1.38 RO2 + 0.62 C2O3
     &      +( 0.967)*r(160)  ! ISPD = 0.333 CO + 0.067 ALD2 + 0.9 FORM + 0.832 PAR + 0.333 HO2 + 0.7 XO2H + 0.7
     &      +( 0.300)*r(199)  ! XOPN = CAO2 + 0.7 HO2 + 0.7 CO + 0.3 C2O3 + RO2
     &      +( 0.120)*r(205)  ! OPEN + O3 = 1.4 GLY + 0.24 MGLY + 0.5 OH + 0.12 C2O3 + 0.08 FORM + 0.02 ALD2
     &      +         r(107)  ! ALD2 + NO3 = C2O3 + HNO3
     &      +         r(115)  ! GLYD + NO3 = HNO3 + C2O3
     &      +         r(120)  ! MGLY + NO3 = HNO3 + C2O3 + XO2 + RO2
     &      +( 0.114)*r(158)  ! ISPD + O3 = 0.02 ALD2 + 0.15 FORM + 0.225 CO + 0.85 MGLY + 0.36 PAR + 0.114 C2O3
     &      +( 0.600)*r(201)  ! XOPN + O3 = 1.2 MGLY + 0.5 OH + 0.6 C2O3 + 0.1 ALD2 + 0.5 CO + 0.3 XO2H +
     &      +( 0.120)*r(205)  ! OPEN + O3 = 1.4 GLY + 0.24 MGLY + 0.5 OH + 0.12 C2O3 + 0.08 FORM + 0.02 ALD2
     &      +         r(105)  ! ALD2 + O = C2O3 + OH
C
C...  Rad initiation for CXO3
C     nn = nn + 1
C     ptname(nn)  = 'newCXO3'
C...  new CXO3
      RO2_new = RO2_new
     &       + cyc_PANX_l
     &      +( 0.500)*r(128)  ! KET = 0.5 ALD2 + 0.5 C2O3 + 0.5 XO2H + 0.5 CXO3 + 0.5 MEO2 + RO2 + -2.5 P
     &      +         r(111)  ! ALDX + NO3 = CXO3 + HNO3
     &      +( 0.075)*r(159)  ! ISPD + NO3 = 0.643 CO + 0.282 FORM + 0.357 ALDX + 1.282 PAR + 0.85 HO2 + 0.075 CXO3
     &      +( 0.200)*r(155)  ! ISOP + O3 = 0.6 FORM + 0.65 ISPD + 0.15 ALDX + 0.2 CXO3 + 0.35 PAR + 0.266 OH
     &      +( 0.390)*r(170)  ! TERP + O3 = 0.57 OH + 0.07 XO2H + 0.69 XO2 + 0.18 XO2N + 0.94 RO2 + 0.24 FORM +
     &      +         r(105)  ! ALD2 + O = C2O3 + OH
C
C...  Rad initiation for MEO2
C     nn = nn + 1
C     ptname(nn)  = 'newMEO2'
C...  new MEO2
      RO2_new = RO2_new
     &      +         r(88)  ! MEPX  = MEO2 + RO2 + OH
     &      +         r(108)  ! ALD2 = MEO2 + RO2 + CO + HO2
     &      +( 0.500)*r(128)  ! KET = 0.5 ALD2 + 0.5 C2O3 + 0.5 XO2H + 0.5 CXO3 + 0.5 MEO2 + RO2 + -2.5 P
     &      +( 1.380)*r(129)  ! ACET = 0.38 CO + 1.38 MEO2 + 1.38 RO2 + 0.62 C2O3
C
C...  Rad initiation for XO2
C     nn = nn + 1
C     ptname(nn)  = 'newXO2H'
C...  new XO2H
      RO2_new = RO2_new
     &      +( 0.800)*r(66)  ! CXO3 + RO2 = 0.8 ALD2 + 0.8 XO2H + 0.8 RO2
     &      +         r(92)  ! NTR = NO2 + XO2H + RO2
     &      +         r(112)  ! ALDX = ALD2 + XO2H + RO2 + CO + HO2
     &      +( 0.110)*r(114)  ! GLYD = 0.74 FORM + 0.89 CO + 1.4 HO2 + 0.15 MEOH + 0.19 OH + 0.11 GLY + 0.110
     &      +( 0.500)*r(128)  ! KET = 0.5 ALD2 + 0.5 C2O3 + 0.5 XO2H + 0.5 CXO3 + 0.5 MEO2 + RO2 + -2.5 P
     &      +( 0.700)*r(160)  ! ISPD = 0.333 CO + 0.067 ALD2 + 0.9 FORM + 0.832 PAR + 0.333 HO2 + 0.7 XO2H + 0.7
     &      +( 0.500)*r(140)  ! ETH + NO3 = 0.5 NO2 + 0.5 NTR + 0.5 XO2H + 0.5 XO2 + RO2 + 1.125 FORM
     &      +( 0.480)*r(144)  ! OLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.500
     &      +( 0.480)*r(148)  ! IOLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.50
     &      +( 0.640)*r(156)  ! ISOP + NO3 = 0.35 NO2 + 0.65 INTR + 0.64 XO2H + 0.33 XO2 + 0.03 XO2N + RO2 + 0.3
     &      +( 0.075)*r(159)  ! ISPD + NO3 = 0.643 CO + 0.282 FORM + 0.357 ALDX + 1.282 PAR + 0.85 HO2 + 0.075 CXO3
     &      +( 0.280)*r(171)  ! TERP + NO3 = 0.47 NO2 + 0.28 XO2H + 0.75 XO2 + 0.25 XO2N + 1.28 RO2 + 0.47 ALDX
     &      +( 0.360)*r(188)  ! CRES + NO3 = 0.3 CRO + HNO3 + 0.24 XO2 + 0.36 XO2H + 0.48 ALDX + 0.24 FORM + 0.
     &      +( 0.450)*r(202)  ! XOPN + NO3 = 0.5 NO2 + 0.5 NTR + 0.45 XO2H + 0.45 XO2 + 0.1 XO2N + RO2 + 0.25
     &      +( 0.150)*r(143)  ! OLE + O3 = 0.295 ALD2 + 0.555 FORM + 0.27 ALDX + 0.15 XO2H + 0.15 RO2 + 0.334 OH +
     &      +( 0.300)*r(147)  ! IOLE + O3 = 0.732 ALD2 + 0.442 ALDX + 0.128 FORM + 0.245 CO + 0.5 OH + 0.3 XO2H +
     &      +( 0.064)*r(158)  ! ISPD + O3 = 0.02 ALD2 + 0.15 FORM + 0.225 CO + 0.85 MGLY + 0.36 PAR + 0.114 C2O3
     &      +( 0.070)*r(170)  ! TERP + O3 = 0.57 OH + 0.07 XO2H + 0.69 XO2 + 0.18 XO2N + 0.94 RO2 + 0.24 FORM +
     &      +( 0.300)*r(201)  ! XOPN + O3 = 1.2 MGLY + 0.5 OH + 0.6 C2O3 + 0.1 ALD2 + 0.5 CO + 0.3 XO2H +
     &      +( 0.700)*r(137)  ! ETH + O = FORM + HO2 + CO + 0.7 XO2H + 0.7 RO2 + 0.3 OH
     &      +( 0.200)*r(141)  ! OLE + O = 0.2 ALD2 + 0.3 ALDX + 0.1 HO2 + 0.2 XO2H + 0.2 CO + 0.2 FORM +
     &      +( 0.100)*r(145)  ! IOLE + O = 1.24 ALD2 + 0.66 ALDX + 0.1 XO2H + 0.1 RO2 + 0.1 CO + 0.1 PAR
C
C...  Rad initiation for XO2
C     nn = nn + 1
C     ptname(nn)  = 'newXO2H'
C...  new XO2H
      RO2_new = RO2_new
     &      +         r(118)  ! GLY + NO3 = HNO3 + CO + HO2 + XO2 + RO2
     &      +         r(120)  ! MGLY + NO3 = HNO3 + C2O3 + XO2 + RO2
     &      +( 0.500)*r(140)  ! ETH + NO3 = 0.5 NO2 + 0.5 NTR + 0.5 XO2H + 0.5 XO2 + RO2 + 1.125 FORM
     &      +( 0.480)*r(144)  ! OLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.500
     &      +( 0.480)*r(148)  ! IOLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.50
     &      +( 0.200)*r(155)  ! ISOP + O3 = 0.6 FORM + 0.65 ISPD + 0.15 ALDX + 0.2 CXO3 + 0.35 PAR + 0.266 OH
     &      +( 0.330)*r(156)  ! ISOP + NO3 = 0.35 NO2 + 0.65 INTR + 0.64 XO2H + 0.33 XO2 + 0.03 XO2N + RO2 + 0.3
     &      +( 0.690)*r(170)  ! TERP + O3 = 0.57 OH + 0.07 XO2H + 0.69 XO2 + 0.18 XO2N + 0.94 RO2 + 0.24 FORM +
     &      +( 0.750)*r(171)  ! TERP + NO3 = 0.47 NO2 + 0.28 XO2H + 0.75 XO2 + 0.25 XO2N + 1.28 RO2 + 0.47 ALDX
     &      +( 0.240)*r(188)  ! CRES + NO3 = 0.3 CRO + HNO3 + 0.24 XO2 + 0.36 XO2H + 0.48 ALDX + 0.24 FORM + 0.
     &      +( 0.450)*r(202)  ! XOPN + NO3 = 0.5 NO2 + 0.5 NTR + 0.45 XO2H + 0.45 XO2 + 0.1 XO2N + RO2 + 0.25
     &      +         r(217)  ! OPO3 + C2O3 = MEO2 + XO2 +ALDX + 2 RO2
C
C...  Rad initiation for XO2N
C     nn = nn + 1
C     ptname(nn)  = 'newxO2N'
C...  new XO2N
      RO2_new = RO2_new
     &      +( 0.040)*r(133)  ! ROR = 0.2 KET + 0.42 ACET + 0.74 ALD2 + 0.37 ALDX + 0.04 XO2N + 0.94 XO2H + 0
     &      +( 0.010)*r(141)  ! OLE + O = 0.2 ALD2 + 0.3 ALDX + 0.1 HO2 + 0.2 XO2H + 0.2 CO + 0.2 FORM +
     &      +( 0.040)*r(144)  ! OLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.500
     &      +( 0.040)*r(148)  ! IOLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.50
     &      +( 0.030)*r(156)  ! ISOP + NO3 = 0.35 NO2 + 0.65 INTR + 0.64 XO2H + 0.33 XO2 + 0.03 XO2N + RO2 + 0.3
     &      +( 0.180)*r(170)  ! TERP + O3 = 0.57 OH + 0.07 XO2H + 0.69 XO2 + 0.18 XO2N + 0.94 RO2 + 0.24 FORM +
     &      +( 0.250)*r(171)  ! TERP + NO3 = 0.47 NO2 + 0.28 XO2H + 0.75 XO2 + 0.25 XO2N + 1.28 RO2 + 0.47 ALDX
     &      +( 0.100)*r(188)  ! CRES + NO3 = 0.3 CRO + HNO3 + 0.24 XO2 + 0.36 XO2H + 0.48 ALDX + 0.24 FORM + 0.
     &      +( 0.100)*r(202)  ! XOPN + NO3 = 0.5 NO2 + 0.5 NTR + 0.45 XO2H + 0.45 XO2 + 0.1 XO2N + RO2 + 0.25
C
C...  Total RO2 Rad initiation
C
      nn =  nn + 1
      ptname(nn)  = 'RO2_new'
      PA(nn) =   RO2_new
C
C
C------------------End of Rad Initiation----------------
C
C
C        {    R a d i c a l     P r o p a g a t i o n    }
C
C
C...  OH+CO
      nn = nn + 1
      ptname(nn)  = 'OHw_CO'
      PA(nn) = R(123)
C
C...  OH+CH4
      nn = nn + 1
      ptname(nn)  = 'OHw_CH4'
      PA(nn) = R(124)
C
C...  OH+ETHA
      nn = nn + 1
      ptname(nn)  = 'OHw_ETHA'
      PA(nn) = R(125)
C
C...  OH+PRPA
      nn = nn + 1
      ptname(nn)  = 'OHw_PRPA'
      PA(nn) = R(131)
C
C...  OH+PAR
      nn = nn + 1
      ptname(nn)  = 'OHw_PAR'
      PA(nn) = R(132)
C
C...  OH+BENZ
      nn = nn + 1
      ptname(nn)  = 'OHw_BENZ'
      PA(nn) = R(172)
C
C...  OH+TOL
      nn = nn + 1
      ptname(nn)  = 'OHw_TOL'
      PA(nn) = R(177)
C
C...  OH+XYL
      nn = nn + 1
      ptname(nn)  = 'OHw_XYL'
      PA(nn) = R(182)
C
C...  OH+ETH
      nn = nn + 1
      ptname(nn)  = 'OHw_ETH'
      PA(nn) = R(138)
C
C...  OH+OLE
      nn = nn + 1
      ptname(nn)  = 'OHw_OLE'
      PA(nn) = R(142)
C
C...  OH+IOLE
      nn = nn + 1
      ptname(nn)  = 'OHw_IOLE'
      PA(nn) = R(146)
C
C...  OH+ISOP
      nn = nn + 1
      ptname(nn)  = 'OHw_ISOP'
      PA(nn) = R(149)
C
C...  OH+TERP
      nn = nn + 1
      ptname(nn)  = 'OHw_TERP'
      PA(nn) = R(169)
C
C...  OH+ETHY
      nn = nn + 1
      ptname(nn)  = 'OHw_ETHY'
      PA(nn) = R(136)
C
C...  { All OH rxns with organics }
      nn = nn + 1
      ptname(nn)  = 'OHw_all_HC'
c     93  FACD + OH = HO2
c     94  AACD + OH = MEO2 + RO2
c     95  PACD + OH = C2O3
c     96  FORM + OH = HO2 + CO
c     106  ALD2 + OH = C2O3
c     110  ALDX + OH = CXO3
c     113  GLYD + OH = 0.2 GLY + 0.2 HO2 + 0.8 C2O3
c     116  GLY + OH = 1.7 CO + 0.3 XO2 + 0.3 RO2 + HO2
c     121  MGLY + OH = C2O3 + CO
c     123  CO + OH = HO2
c     124  CH4 + OH = MEO2 + RO2
c     125  ETHA + OH = 0.991 ALD2 + 0.991 XO2H + 0.009 XO2N + RO2
c     126  MEOH + OH = FORM + HO2
c     127  ETOH + OH = 0.95 ALD2 + 0.9 HO2 + 0.1 XO2H + 0.1 RO2 + 0.078 FORM + 0.011 GLYD
c     130  ACET + OH = FORM + C2O3 + XO2 + RO2
c     131  PRPA + OH = 0.71 ACET + 0.26 ALDX + 0.26 PAR + 0.97 XO2H + 0.03 XO2N + RO2
c     132  PAR + OH = 0.11 ALDX + 0.76 ROR + 0.13 XO2N + 0.11 XO2H + 0.76 XO2 + RO2 + -0.11
c     136  ETHY + OH = 0.7 GLY + 0.7 OH + 0.3 FACD + 0.3 CO + 0.3 HO2
c     138  ETH + OH = XO2H + RO2 + 1.56 FORM + 0.22 GLYD
c     142  OLE + OH = 0.781 FORM + 0.488 ALD2 + 0.488 ALDX + 0.976 XO2H + 0.195 XO2 + 0.024 XO2N
c     146  IOLE + OH = 1.3 ALD2 + 0.7 ALDX + XO2H + RO2
c     149  ISOP + OH = ISO2 + RO2
c     157  ISPD + OH = 0.095 XO2N + 0.379 XO2 + 0.318 XO2H + 0.792 RO2 + 0.843 PAR + 0.379 C2O3
c     161  ISPX + OH = 0.904 EPOX + 0.933 OH + 0.067 ISO2 + 0.067 RO2 + 0.029 IOLE + 0.029 ALDX
c     162  EPOX + OH = EPX2 + RO2
c     167  INTR + OH = 0.63 XO2 + 0.37 XO2H + RO2 + 0.444 NO2 + 0.185 NO3 + 0.104 INTR + 0.592
c     169  TERP + OH = 0.75 XO2H + 0.5 XO2 + 0.25 XO2N + 1.5 RO2 + 0.28 FORM + 1.66 PAR
c     172  BENZ + OH = 0.53 CRES + 0.352 BZO2 + 0.352 RO2 + 0.118 OPEN + 0.118 OH + 0.53 HO2
c     177  TOL + OH = 0.18 CRES + 0.65 TO2 + 0.72 RO2 + 0.1 OPEN + 0.1 OH + 0.07 XO2H +
c     182  XYL + OH = 0.155 CRES + 0.544 XLO2 + 0.602 RO2 + 0.244 XOPN + 0.244 OH + 0.058 XO2H +
c     187  CRES + OH = 0.06 CRO + 0.12 XO2H + HO2 + 0.13 OPEN + 0.732 CAT1 + 0.06 CO + 0.060
c     191  CRON + OH = CRNO
c     198  CRPX + OH = CRN2
c     200  XOPN + OH = CAO2 +  MGLY +  XO2H +  RO2
c     204  OPEN + OH = 0.6 OPO3 + 0.4 CAO2 + 0.4 RO2
c     207  CAT1 + OH = CAO2 + RO2
      OHwHC  = R(93) + R(94) + R(95) + R(96) + R(106) 
     &       + R(110) + R(113) + R(116) + R(121)
     &       + R(123) + R(124) + R(125) + R(126)
     &       + R(127) + R(130) + R(131) + R(132)
     &       + R(136) + R(138) + R(142) + R(146)
     &       + R(149) + R(157) + R(161) + R(162)
     &       + R(167) + R(169) + R(172) + R(177)
     &       + R(182) + R(187) + R(191) + R(198)
     &       + R(200) + R(204) + R(207)
      PA(nn) = OHwHC
C
C...  { Isoprene with other oxidants }
      nn = nn + 1
      ptname(nn)  = 'ISOPwOx'
      PA(nn) = R(155)   !ISOP+O3
     &       + R(156)   !ISOP+NO3
C
C...  { Terpenes with other oxidants }
      nn = nn + 1
      ptname(nn)  = 'TERPwOx'
      PA(nn) = R(170)   !ISOP+O3
     &       + R(171)   !ISOP+NO3
     &       + R(168)   !ISOP+O
C
C...  { HOx reacted: reaction consuming OH or HO2 as reactant }
C
C...  OH reacted
      OH_reacted =
     &      +         r( 12)  ! O3 + OH
     &      +         r( 14)  ! OH + O
     &      +( 2.000)*r( 16)  ! OH + OH
     &      +( 2.000)*r( 17)  ! OH + OH
     &      +         r( 18)  ! OH + HO2
     &      +         r( 22)  ! H2O2 + OH
     &      +         r( 32)  ! NO3 + OH
     &      +         r( 40)  ! NO + OH
     &      +         r( 44)  ! HONO + OH
     &      +         r( 45)  ! NO2 + OH
     &      +         r( 46)  ! HNO3 + OH
     &      +         r( 51)  ! PNA + OH
     &      +         r( 52)  ! SO2 + OH
     &      +         r( 87)  ! MEPX + OH
     &      +         r( 89)  ! ROOH + OH
     &      +         r( 91)  ! NTR + OH
     &      +         r( 93)  ! FACD + OH
     &      +         r( 94)  ! AACD + OH
     &      +         r( 95)  ! PACD + OH
     &      +         r( 96)  ! FORM + OH
     &      +         r(106)  ! ALD2 + OH
     &      +         r(110)  ! ALDX + OH
     &      +         r(113)  ! GLYD + OH
     &      +         r(116)  ! GLY + OH
     &      +         r(121)  ! MGLY + OH
     &      +         r(122)  ! H2 + OH
     &      +         r(123)  ! CO + OH
     &      +         r(124)  ! CH4 + OH
     &      +         r(125)  ! ETHA + OH
     &      +         r(126)  ! MEOH + OH
     &      +         r(127)  ! ETOH + OH
     &      +         r(130)  ! ACET + OH
     &      +         r(131)  ! PRPA + OH
     &      +         r(132)  ! PAR + OH
     &      +         r(136)  ! ETHY + OH
     &      +         r(138)  ! ETH + OH
     &      +         r(142)  ! OLE + OH
     &      +         r(146)  ! IOLE + OH
     &      +         r(149)  ! ISOP + OH
     &      +         r(157)  ! ISPD + OH
     &      +         r(161)  ! ISPX + OH
     &      +         r(162)  ! EPOX + OH
     &      +         r(167)  ! INTR + OH
     &      +         r(169)  ! TERP + OH
     &      +         r(172)  ! BENZ + OH
     &      +         r(177)  ! TOL + OH
     &      +         r(182)  ! XYL + OH
     &      +         r(187)  ! CRES + OH
     &      +         r(191)  ! CRON + OH
     &      +         r(198)  ! CRPX + OH
     &      +         r(200)  ! XOPN + OH
     &      +         r(204)  ! OPEN + OH
     &      +         r(207)  ! CAT1 + OH
C
C...  HO2 reacted
      HO2_reacted =
     &      +         r( 13)  ! O3 + HO2
     &      +         r( 15)  ! HO2 + O
     &      +         r( 18)  ! OH + HO2
     &      + cyc_H2O2_p     ! HO2 + HO2; HO2 + HO2 + H2O; H2O2 destruction
     &      +         r( 25)  ! HO2 + NO
     &      +         r( 33)  ! NO3 + HO2
     &      + cyc_HNO4_p     ! HO2 + NO2; HNO4 destruction
     &      +         r( 57)  ! C2O3 + HO2
     &      +         r( 65)  ! CXO3 + HO2
     &      + cyc_ROOH_p     ! Sum of XO2s + HO2
     &      + cyc_MEPX_p     ! MEO2 + HO2; MEPX destruction
     &      + cyc_HCO3_p     ! FORM + HO2; HCO3 = FORM + HO2
     &      +         r(104)  ! HCO3 + HO2
     &      +         r(151)  ! ISO2 + HO2
     &      +         r(163)  ! EPX2 + HO2
     &      +         r(175)  ! BZO2 + HO2
     &      +         r(180)  ! TO2 + HO2
     &      +         r(184)  ! XLO2 + HO2
     &      +         r(190)  ! CRO + HO2
     &      +         r(196)  ! CRN2 + HO2
     &      +         r(210)  ! CAO2 + HO2
     &      +         r(216)  ! OPO3 + HO2
C
      HOx_reacted = HO2_reacted + OH_reacted
C
      nn = nn + 1
      ptname(nn)  = 'OH_rctd'
      PA(nn) = OH_reacted
C
      nn = nn + 1
      ptname(nn)  = 'HO2_rctd'
      PA(nn) = HO2_reacted
C
      nn = nn + 1
      ptname(nn)  = 'HOx_rctd'
      PA(nn) = HOx_reacted
C
c  {other OH propagation rxns }
c
c
c... { other OH prop rxns }
c      nn =  nn + 1
c      ptname(nn)  = 'OHpropmisc'
c
c      other_OH_prop = 
c     &      +( 1.000)*r( 12)  ! O3 + OH
c     &      +( 1.000)*r(122)  ! H2 + OH
c     &      +( 1.000)*r( 22)  ! H2O2 + OH
c     &      +( 1.000)*r( 52)  ! SO2 + OH
c     &      +( 1.000)*r( 32)  ! NO3 + OH
c      PA(nn) = other_OH_prop
C
C
C...  { RO2 reacted: reaction consuming C2O3, CXO3, OPO3, MEO2, XO2H, XO2, XO2N, 
C     TO2, XLO2,  BZO2, ISO2, EPX2, CRN2, CAO2 }
C
      nn = nn + 1
      ptname(nn)  = 'RO2_rctd'
C...  C2O3 reacted
      PA(nn) = 
     &      +         r( 53)  ! C2O3 + NO
     &      + cyc_PAN_p  ! C2O3 + NO2
     &      +         r( 57)  ! C2O3 + HO2
C     &      +         r( 58)  ! C2O3 + RO2
     &      +( 2.000)*r( 59)  ! C2O3 + C2O3
     &      +         r( 60)  ! C2O3 + CXO3
     &      +         r( 73)  ! MEO2 + C2O3
     &      +         r( 77)  ! XO2H + C2O3
     &      +         r( 81)  ! XO2 + C2O3
     &      +         r( 85)  ! XO2N + C2O3
     &      +         r(152)  ! ISO2 + C2O3
     &      +         r(165)  ! EPX2 + C2O3
     &      +         r(174)  ! BZO2 + C2O3
     &      +         r(179)  ! TO2 + C2O3
     &      +         r(185)  ! XLO2 + C2O3
     &      +         r(211)  ! CAO2 + C2O3
     &      +         r(217)  ! OPO3 + C2O3
C...  CXO3 reacted
      PA(nn) = PA(nn) 
     &      +         r( 60)  ! C2O3 + CXO3
     &      +         r( 61)  ! CXO3 + NO
     &      + cyc_PANX_p  ! CXO3 + NO2
     &      +         r( 65)  ! CXO3 + HO2
     &      +         r( 66)  ! CXO3 + RO2
     &      +( 2.000)*r( 67)  ! CXO3 + CXO3
C...  OPO3 reacted
      PA(nn) = PA(nn) 
     &      +         r(213)  ! OPO3 + NO
     &      + cyc_OPAN_p  ! OPO3 + NO2
     &      +         r(216)  ! OPO3 + HO2
     &      +         r(217)  ! OPO3 + C2O3
     &      +         r(218)  ! OPO3 + RO2
C...  MEO2 reacted
      PA(nn) = PA(nn)
     &      +         r( 71)  ! MEO2 + NO
     &      +         r( 72)  ! MEO2 + HO2
     &      +         r( 73)  ! MEO2 + C2O3
     &      +         r( 74)  ! MEO2 + RO2
C...  XO2H reacted
      PA(nn) = PA(nn)
     &      +         r( 75)  ! XO2H + NO
     &      +         r( 76)  ! XO2H + HO2
     &      +         r( 77)  ! XO2H + C2O3
     &      +         r( 78)  ! XO2H + RO2
C...  XO2 reacted
      PA(nn) = PA(nn)
     &      +         r( 79)  ! XO2 + NO
     &      +         r( 80)  ! XO2 + HO2
     &      +         r( 81)  ! XO2 + C2O3
     &      +         r( 82)  ! XO2 + RO2
C...  XO2N reacted
      PA(nn) = PA(nn)
     &      +         r( 83)  ! XO2N + NO
     &      +         r( 84)  ! XO2N + HO2
     &      +         r( 85)  ! XO2N + C2O3
     &      +         r( 86)  ! XO2N + RO2
C...  BZO2 reacted
      PA(nn) = PA(nn)
     &      +         r(173)  ! BZO2 + NO
     &      +         r(174)  ! BZO2 + C2O3
     &      +         r(175)  ! BZO2 + HO2
     &      +         r(176)  ! BZO2 + RO2
C...  TO2 reacted
      PA(nn) = PA(nn)
     &      +         r(178)  ! TO2 + NO
     &      +         r(179)  ! TO2 + C2O3
     &      +         r(180)  ! TO2 + HO2
     &      +         r(181)  ! TO2 + RO2
C...  XLO2 reacted
      PA(nn) = PA(nn)
     &      +         r(183)  ! XLO2 + NO
     &      +         r(184)  ! XLO2 + HO2
     &      +         r(185)  ! XLO2 + C2O3
     &      +         r(186)  ! XLO2 + RO2
C...  ISO2 reacted
      PA(nn) = PA(nn)
     &      +         r(150)  ! ISO2 + NO
     &      +         r(151)  ! ISO2 + HO2
     &      +         r(152)  ! ISO2 + C2O3
     &      +         r(153)  ! ISO2 + RO2
     &      +         r(154)  ! ISO2
C...  EPX2 reacted
      PA(nn) = PA(nn)
     &      +         r(163)  ! EPX2 + HO2
     &      +         r(164)  ! EPX2 + NO
     &      +         r(165)  ! EPX2 + C2O3
     &      +         r(166)  ! EPX2 + RO2
C...  CAO2 reacted
      PA(nn) = PA(nn)
     &      +( 1.200)*r(209)  ! CAO2 + NO = 0.86 NO2 + 0.14 NTR + 1.2 HO2 + 0.344 FORM + 0.344 CO
     &      +         r(210)  ! CAO2 + HO2 =
     &      +         r(211)  ! CAO2 + C2O3 = HO2 + 0.4 GLY + MEO2 + RO2
     &      +         r(212)  ! CAO2 + RO2 = HO2 + 0.4 GLY + RO2
C...  CRN2 reacted
      PA(nn) = PA(nn)
     &      +         r(195)  ! CRN2 + NO = CRNO + NO2
     &      +         r(196)  ! CRN2 + HO2 = CRPX
C
C...  { OH from HO2 }
C     Here we add up all rxns that convert HO2 to OH
      nn = nn + 1
      ptname(nn)  = 'OHfromHO2'
      OH_from_HO2 =
     &      +         r( 13)  ! O3 + HO2
     &      +         r( 15)  ! HO2 + O
     &      +         r( 25)  ! HO2 + NO
     &      +         r( 33)  ! NO3 + HO2
     &      +( 0.440)*r( 57)  ! C2O3 + HO2
     &      +( 0.440)*r( 65)  ! CXO3 + HO2
     &      +( 0.200)*r(104)  ! HCO3 + HO2
     &      +( 0.120)*r(151)  ! ISO2 + HO2
     &      +( 1.125)*r(163)  ! EPX2 + HO2
     &      +( 0.440)*r(216)  ! OPO3 + HO2
      PA(nn) = OH_from_HO2
C
C...  { Yield of OH per HO2 }
C
      if(.NOT. lcpacum) then
        nn = nn + 1
        ptname(nn)  = 'Y_OHperHO2'
        if (HO2_reacted .LE. 0.0) then
          Y_OH_per_HO2 = 0.0
        else
          Y_OH_per_HO2 =  OH_from_HO2/HO2_reacted
        end if
        PA(nn) = Y_OH_per_HO2 * (dtfact/ppbfact)
      endif
C
C
C---------------End of Radical Propagation-------------------
C
C
C          {    R a d i c a l    T e r m i n a t i o n    }
C
C
C
C...  { HOx termination: OH or HO2 reacted without HOx produced }
C     Do not count OH + VOCs that produce hidden HO2 via
C     C2O3,CXO3,MEO2
C
C...  OH terminated
      OH_term =
     &      +( 2.000)*r( 17)  ! OH + OH = H2O2
     &      +         r( 18)  ! OH + HO2 =
     &      + cyc_HONO_p  ! NO + OH = HONO
     &      +         r( 44)  ! HONO + OH = NO2
     &      +         r( 45)  ! NO2 + OH = HNO3
     &      +         r( 51)  ! PNA + OH = NO2
C
C...  HO2 terminated
      HO2_term =
     &      +         r( 18)  ! OH + HO2 =
     &      + cyc_H2O2_p  ! 19, 20  HO2 + HO2 = H2O2
     &      + cyc_HNO4_p  ! HO2 + NO2 = PNA
     &      + cyc_MEPX_p  ! MEO2 + HO2  = 0.9 MEPX + 0.1 FORM
     &      + cyc_ROOH_p  ! 76, 80, 84   XO2$ + HO2  = ROOH
     &      +         r( 80)  ! XO2 + HO2  = ROOH
     &      +         r( 84)  ! XO2N + HO2  = ROOH
     &      +         r(175)  ! BZO2 + HO2 =
     &      +         r(180)  ! TO2 + HO2 =
     &      +         r(184)  ! XLO2 + HO2 =
     &      +         r(190)  ! CRO + HO2 = CRES
     &      +         r(196)  ! CRN2 + HO2 = CRPX
     &      +         r(210)  ! CAO2 + HO2 =
C
      nn = nn + 1
      ptname(nn)  = 'OH_term'
      PA(nn) = OH_term
C
      nn = nn + 1
      ptname(nn)  = 'HO2_term'
      PA(nn) = HO2_term
C
C
      HOx_term = OH_term + HO2_term
      nn = nn + 1
      ptname(nn)  = 'HOx_term'
      PA(nn) = HOx_term
C
C...  { RO2 termination rxns }
C     RO2 radicals reacted are C2O3, CXO3, OPO3, MEO2, XO2H, 
C     XO2, XO2N, TO2, XLO2,  BZO2, ISO2, EPX2, CRN2, CAO2
      nn = nn + 1
      ptname(nn)  = 'RO2_term'
C...  C2O3
      PA(nn) =
     &      + cyc_PAN_p  ! C2O3 + NO2
     &      +         r( 57)  ! C2O3 + HO2
     &      +         r( 58)  ! C2O3 + RO2
     &      +( 2.000)*r( 59)  ! C2O3 + C2O3
     &      +         r( 60)  ! C2O3 + CXO3
     &      +         r( 73)  ! MEO2 + C2O3
     &      +         r( 77)  ! XO2H + C2O3
     &      +         r( 81)  ! XO2 + C2O3
     &      +         r( 85)  ! XO2N + C2O3
     &      +         r(152)  ! ISO2 + C2O3
     &      +         r(165)  ! EPX2 + C2O3
     &      +         r(174)  ! BZO2 + C2O3
     &      +         r(179)  ! TO2 + C2O3
     &      +         r(185)  ! XLO2 + C2O3
     &      +         r(211)  ! CAO2 + C2O3
     &      +         r(217)  ! OPO3 + C2O3
C...  CXO3 
      PA(nn) = PA(nn) 
     &      +         r( 60)  ! C2O3 + CXO3
     &      + cyc_PANX_p  ! CXO3 + NO2
     &      +         r( 65)  ! CXO3 + HO2
     &      +         r( 66)  ! CXO3 + RO2
     &      +( 2.000)*r( 67)  ! CXO3 + CXO3
C...  OPO3 
      PA(nn) = PA(nn) 
     &      + cyc_OPAN_p  ! OPO3 + NO2
     &      +         r(216)  ! OPO3 + HO2
     &      +         r(217)  ! OPO3 + C2O3
     &      +         r(218)  ! OPO3 + RO2
C...  MEO2 
      PA(nn) = PA(nn)
     &      +         r( 72)  ! MEO2 + HO2
     &      +         r( 73)  ! MEO2 + C2O3
     &      +         r( 74)  ! MEO2 + RO2
C...  XO2H 
      PA(nn) = PA(nn)
     &      +         r( 76)  ! XO2H + HO2
     &      +         r( 77)  ! XO2H + C2O3
     &      +         r( 78)  ! XO2H + RO2
C...  XO2 
      PA(nn) = PA(nn)
     &      +         r( 80)  ! XO2 + HO2
     &      +         r( 81)  ! XO2 + C2O3
     &      +         r( 82)  ! XO2 + RO2
C...  XO2N 
      PA(nn) = PA(nn)
     &      +         r( 84)  ! XO2N + HO2
     &      +         r( 85)  ! XO2N + C2O3
     &      +         r( 86)  ! XO2N + RO2
C...  BZO2 
      PA(nn) = PA(nn)
     &      +         r(174)  ! BZO2 + C2O3
     &      +         r(175)  ! BZO2 + HO2
     &      +         r(176)  ! BZO2 + RO2
C...  TO2 
      PA(nn) = PA(nn)
     &      +         r(179)  ! TO2 + C2O3
     &      +         r(180)  ! TO2 + HO2
     &      +         r(181)  ! TO2 + RO2
C...  XLO2 
      PA(nn) = PA(nn)
     &      +         r(184)  ! XLO2 + HO2
     &      +         r(185)  ! XLO2 + C2O3
     &      +         r(186)  ! XLO2 + RO2
C...  ISO2 
      PA(nn) = PA(nn)
     &      +         r(151)  ! ISO2 + HO2
     &      +         r(152)  ! ISO2 + C2O3
     &      +         r(153)  ! ISO2 + RO2
C...  EPX2 
      PA(nn) = PA(nn)
     &      +         r(163)  ! EPX2 + HO2
     &      +         r(165)  ! EPX2 + C2O3
     &      +         r(166)  ! EPX2 + RO2
C...  CRN2
      PA(nn) = PA(nn)
     &      +         r(196)  ! CRN2 + HO2 = CRPX
C...  CAO2
      PA(nn) = PA(nn)
     &      +         r(210)  ! CAO2 + HO2 =
     &      +         r(211)  ! CAO2 + C2O3 = HO2 + 0.4 GLY + MEO2 + RO2
     &      +         r(212)  ! CAO2 + RO2 = HO2 + 0.4 GLY + RO2
C
C
C-------------End of Radical Termination--------------
C
C
C    {     R a d i c a l     C h a i n   L e n g t h    }
C
C   Calculation of the HOx chain length.  Consider HOx together
C   because the chain reaction involves both OH and HO2.  Do not consider
C   other radicals to be part of the HOx chain because it is unclear
C   what this would mean.
C
C   HOx chain length = (HOx_rctd/2)/HOx_new
C   where:
C     HOx_rctd = sum of all OH and HO2 reactions
C     HOx_new  = sum of OH and HO2 production that is not recycling
C
C   HOx_rctd is divided by two because a chain length of 1 means.
C   a radical must react as both OH and HO2.
C
C...  { HOx chain length }
      if(.NOT. lcpacum) then
        nn = nn + 1
        ptname(nn)  = 'HOx_CL'
        if (ldark) then
          PA(nn) = -999. * (dtfact/ppbfact)
        else
          PA(nn) = (dtfact/ppbfact) * HOx_reacted
     &                        /  (2.0 * amax1(1.0e-6, HOx_new))
        endif
      endif
C
C
C-----------------End of Radical Chain Length---------------------------
C
C
C            {  F o r m a l d e h y d e   C h e m i s t r y  }
C
C
C     Only first generation HCHO is counted
C
C
C...  { HCHO Production from ethene }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_eth'
      hcho_from_eth =
     &      +         r(137)  ! ETH + O = FORM + HO2 + CO + 0.7 XO2H + 0.7 RO2 + 0.3 OH
     &      +( 1.560)*r(138)  ! ETH + OH = XO2H + RO2 + 1.56 FORM + 0.22 GLYD
     &      +         r(139)  ! ETH + O3 = FORM + 0.51 CO + 0.16 HO2 + 0.16 OH + 0.37 FACD
     &      +( 1.125)*r(140)  ! ETH + NO3 = 0.5 NO2 + 0.5 NTR + 0.5 XO2H + 0.5 XO2 + RO2 + 1.125 FORM
      PA(nn) = hcho_from_eth
C
C... { HCHO Production from OLE }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_ole'
      hcho_from_ole =
     &      +( 0.200)*r(141)  ! OLE + O = 0.2 ALD2 + 0.3 ALDX + 0.1 HO2 + 0.2 XO2H + 0.2 CO + 0.2 FORM +
     &      +( 0.781)*r(142)  ! OLE + OH = 0.781 FORM + 0.488 ALD2 + 0.488 ALDX + 0.976 XO2H + 0.195 XO2 + 0.024 XO2N
     &      +( 0.555)*r(143)  ! OLE + O3 = 0.295 ALD2 + 0.555 FORM + 0.27 ALDX + 0.15 XO2H + 0.15 RO2 + 0.334 OH +
     &      +( 0.500)*r(144)  ! OLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.500
      PA(nn) = hcho_from_ole
C
C... { HCHO Production from IOLE }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_iole'
      hcho_from_iole =
     &      +( 0.128)*r(147)  ! IOLE + O3 = 0.732 ALD2 + 0.442 ALDX + 0.128 FORM + 0.245 CO + 0.5 OH + 0.3 XO2H +
      PA(nn) = hcho_from_iole
C
C... { HCHO Production from OPEN }
cgy     nn = nn + 1
cgy     ptname(nn)  = 'HCHOp_open'
      hcho_from_open =
     &      +( 0.080)*r(205)  ! OPEN + O3 = 1.4 GLY + 0.24 MGLY + 0.5 OH + 0.12 C2O3 + 0.08 FORM + 0.02 ALD2
cgy     PA(nn) = hcho_from_open
C
C... { HCHO Production from XOPN }
cgy     nn = nn + 1
cgy     ptname(nn)  = 'HCHOp_xopn'
      hcho_from_xopn = 0.0
cgy     none
cgy     PA(nn) = hcho_from_open
C
C...  { HCHO Production from terpene }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_terp'
      hcho_from_terp =
     &      +( 0.280)*r(169)  ! TERP + OH = 0.75 XO2H + 0.5 XO2 + 0.25 XO2N + 1.5 RO2 + 0.28 FORM + 1.66 PAR
     &      +( 0.240)*r(170)  ! TERP + O3 = 0.57 OH + 0.07 XO2H + 0.69 XO2 + 0.18 XO2N + 0.94 RO2 + 0.24 FORM +
      PA(nn) = hcho_from_terp
C
C...  { HCHO Production from isoprene }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_isop'
      hcho_from_isop =
     &      +( 0.600)*r(155)  ! ISOP + O3 = 0.6 FORM + 0.65 ISPD + 0.15 ALDX + 0.2 CXO3 + 0.35 PAR + 0.266 OH
     &      +( 0.350)*r(156)  ! ISOP + NO3 = 0.35 NO2 + 0.65 INTR + 0.64 XO2H + 0.33 XO2 + 0.03 XO2N + RO2 + 0.3
     &      +( 0.660)*r(150)  ! ISO2 + NO = 0.117 INTR  + 0.883 NO2 + 0.803 HO2  + 0.66 FORM + 0.66 ISPD + 0.08 XO
     &      +( 0.120)*r(151)  ! ISO2 + HO2 = 0.88 ISPX + 0.12 OH + 0.12 HO2 + 0.12 FORM + 0.12 ISPD
     &      +( 0.583)*r(152)  ! ISO2 + C2O3 = 0.709 HO2 + 0.583 FORM + 0.583 ISPD + 0.071 XO2H + 0.044 IOLE + 0.037 G
     &      +( 0.660)*r(153)  ! ISO2 + RO2  = 0.803 HO2  + 0.66 FORM + 0.66 ISPD + 0.08 XO2H + 0.05 IOLE + 0.042
     &      +( 0.040)*r(154)  ! ISO2 = 0.8 HO2 + 0.04 OH + 0.04 FORM + 0.8 ISPD
      PA(nn) = hcho_from_isop
C
C...  { HCHO Production from isoprene products}
      nn = nn + 1
      ptname(nn)  = 'HCHOp_ispd'
      hcho_from_ispd =
     &      +( 0.592)*r(167)  ! INTR + OH = 0.63 XO2 + 0.37 XO2H + RO2 + 0.444 NO2 + 0.185 NO3 + 0.104 INTR + 0.592
     &      +( 0.240)*r(157)  ! ISPD + OH = 0.095 XO2N + 0.379 XO2 + 0.318 XO2H + 0.792 RO2 + 0.843 PAR + 0.379 C2O3
     &      +( 0.150)*r(158)  ! ISPD + O3 = 0.02 ALD2 + 0.15 FORM + 0.225 CO + 0.85 MGLY + 0.36 PAR + 0.114 C2O3
     &      +( 0.282)*r(159)  ! ISPD + NO3 = 0.643 CO + 0.282 FORM + 0.357 ALDX + 1.282 PAR + 0.85 HO2 + 0.075 CXO3
     &      +( 0.900)*r(160)  ! ISPD = 0.333 CO + 0.067 ALD2 + 0.9 FORM + 0.832 PAR + 0.333 HO2 + 0.7 XO2H + 0.7
     &      +( 0.375)*r(163)  ! EPX2 + HO2 = 0.275 GLYD + 0.275 GLY + 0.275 MGLY + 1.125 OH + 0.825 HO2 + 0.375 FORM
     &      +( 0.375)*r(164)  ! EPX2 + NO = 0.275 GLYD + 0.275 GLY + 0.275 MGLY + 0.125 OH + 0.825 HO2 + 0.375 FORM +
     &      +( 0.300)*r(165)  ! EPX2 + C2O3 = 0.22 GLYD + 0.22 GLY + 0.22 MGLY + 0.1 OH + 0.66 HO2 + 0.3 FORM
     &      +( 0.375)*r(166)  ! EPX2 + RO2 = 0.275 GLYD + 0.275 GLY + 0.275 MGLY + 0.125 OH + 0.825 HO2 + 0.375 FORM
      PA(nn) = hcho_from_ispd
C
C...  Total HCHO Production
      nn = nn + 1
      ptname(nn)  = 'HCHOp_Tot'
      PA(nn) =
     &      + hcho_from_eth
     &      + hcho_from_ole
     &      + hcho_from_iole
     &      + hcho_from_open
     &      + hcho_from_xopn
     &      + hcho_from_terp
     &      + hcho_from_isop
     &      + hcho_from_ispd
     &      +         r( 71)  ! MEO2 + NO  = FORM + HO2 + NO2
     &      +( 0.100)*r( 72)  ! MEO2 + HO2  = 0.9 MEPX + 0.1 FORM
     &      +         r( 73)  ! MEO2 + C2O3 = FORM + 0.9 HO2 + 0.9 MEO2 + 0.1 AACD + 0.9 RO2
     &      +( 0.685)*r( 74)  ! MEO2 + RO2  = 0.685 FORM  + 0.315 MEOH + 0.37 HO2 + RO2
     &      +( 0.400)*r( 87)  ! MEPX + OH  = 0.6 MEO2 + 0.6 RO2 + 0.4 FORM + 0.4 OH
     &      +         r(126)  ! MEOH + OH = FORM + HO2
     &      +cyc_HCO3_l  ! 102  HCO3 = FORM + HO2
     &      +( 0.740)*r(114)  ! GLYD = 0.74 FORM + 0.89 CO + 1.4 HO2 + 0.15 MEOH + 0.19 OH + 0.11 GLY + 0.110
     &      +( 0.078)*r(127)  ! ETOH + OH = 0.95 ALD2 + 0.9 HO2 + 0.1 XO2H + 0.1 RO2 + 0.078 FORM + 0.011 GLYD
     &      +         r(130)  ! ACET + OH = FORM + C2O3 + XO2 + RO2
     &      +( 0.060)*r(187)  ! CRES + OH = 0.06 CRO + 0.12 XO2H + HO2 + 0.13 OPEN + 0.732 CAT1 + 0.06 CO + 0.060
     &      +( 0.240)*r(188)  ! CRES + NO3 = 0.3 CRO + HNO3 + 0.24 XO2 + 0.36 XO2H + 0.48 ALDX + 0.24 FORM + 0.
     &      +( 0.344)*r(209)  ! CAO2 + NO = 0.86 NO2 + 0.14 NTR + 1.2 HO2 + 0.344 FORM + 0.344 CO
C
C
C-----------------End of Formaldehyde Chemistry-------------------------
C
C
C            {   N O y   C h e m i s t r y   }
C
C
C... { HNO3 production from OH+NO2 }
      nn = nn + 1
      ptname(nn)  = 'HNO3_OHNO2'
      PA(nn) = 
     &      +( 1.000)*r(45)  ! NO2 + OH = HNO3
C
C...  { HNO3 production from NO3 }
      nn = nn + 1
      ptname(nn)  = 'HNO3_NO3HC'
      PA(nn) = 
     &      +         r(100)  ! FORM + NO3 = HNO3 + HO2 + CO
     &      +         r(107)  ! ALD2 + NO3 = C2O3 + HNO3
     &      +         r(111)  ! ALDX + NO3 = CXO3 + HNO3
     &      +         r(115)  ! GLYD + NO3 = HNO3 + C2O3
     &      +         r(118)  ! GLY + NO3 = HNO3 + CO + HO2 + XO2 + RO2
     &      +         r(120)  ! MGLY + NO3 = HNO3 + C2O3 + XO2 + RO2
     &      +( 0.150)*r(159)  ! ISPD + NO3 = 0.643 CO + 0.282 FORM + 0.357 ALDX + 1.282 PAR + 0.85 HO2 + 0.075 CXO3
     &      +         r(188)  ! CRES + NO3 = 0.3 CRO + HNO3 + 0.24 XO2 + 0.36 XO2H + 0.48 ALDX + 0.24 FORM + 0.
     &      +         r(192)  ! CRON + NO3 = CRNO + HNO3
     &      +         r(206)  ! OPEN + NO3 = OPO3 + HNO3
     &      +         r(208)  ! CAT1 + NO3 = CRO + HNO3
C
C...  { HNO3 production from N2O5 }
      nn = nn + 1
      ptname(nn)  = 'HNO3_N2O5'
      PA(nn) = 
     &      +( 2.000)*r(39)  ! N2O5 + H2O = 2 HNO3
C
C...  { Other HNO3 production }
c      +         r(107)  ! ALD2 + NO3 = C2O3 + HNO3
c      +         r(100)  ! FORM + NO3 = HNO3 + HO2 + CO
c      +         r(91)  ! NTR + OH  = HNO3 + XO2H + RO2
c      +         r(111)  ! ALDX + NO3 = CXO3 + HNO3
c      +         r(115)  ! GLYD + NO3 = HNO3 + C2O3
c      +         r(118)  ! GLY + NO3 = HNO3 + CO + HO2 + XO2 + RO2
c      +         r(120)  ! MGLY + NO3 = HNO3 + C2O3 + XO2 + RO2
c      +( 0.150)*r(159)  ! ISPD + NO3 = 0.643 CO + 0.282 FORM + 0.357 ALDX + 1.282 PAR + 0.85 HO2 + 0.075 CXO3
c      +         r(188)  ! CRES + NO3 = 0.3 CRO + HNO3 + 0.24 XO2 + 0.36 XO2H + 0.48 ALDX + 0.24 FORM + 0.
c      +         r(206)  ! OPEN + NO3 = OPO3 + HNO3
c      +         r(208)  ! CAT1 + NO3 = CRO + HNO3
c      +         r(192)  ! CRON + NO3 = CRNO + HNO3
C
C...  { Net PAN prod }
      nn = nn + 1
      ptname(nn)  = 'PANprodNet'
      PA(nn) = cyc_PAN_p
C
C...  { Net PAN loss }
      nn = nn + 1
      ptname(nn)  = 'PANlossNet'
      PA(nn) = cyc_PAN_l
C
C...  { Organic Nitrate Production }
      nn = nn + 1
      ptname(nn)  = 'RNO3_prod'
      PA(nn) =  
     &      +         r(135)  ! ROR + NO2 = NTR
     &      +( 2.000)*r(193)  ! CRNO + NO2 = 2 NTR
     &      +         r( 83)  ! XO2N + NO  = NTR
     &      +( 0.082)*r(173)  ! BZO2 + NO = 0.918 NO2 + 0.082 NTR + 0.918 GLY  + 0.918 OPEN + 0.918 HO2
     &      +( 0.140)*r(178)  ! TO2 + NO = 0.86 NO2 + 0.14 NTR + 0.417 GLY + 0.443 MGLY + 0.66 OPEN + 0.2 XOPN +
     &      +( 0.140)*r(183)  ! XLO2 + NO = 0.86 NO2 + 0.14 NTR + 0.221 GLY + 0.675 MGLY + 0.3 OPEN + 0.56 XOPN
     &      +( 0.140)*r(209)  ! CAO2 + NO = 0.86 NO2 + 0.14 NTR + 1.2 HO2 + 0.344 FORM + 0.344 CO
     &      +( 0.500)*r(140)  ! ETH + NO3 = 0.5 NO2 + 0.5 NTR + 0.5 XO2H + 0.5 XO2 + RO2 + 1.125 FORM
     &      +( 0.500)*r(144)  ! OLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.500
     &      +( 0.500)*r(148)  ! IOLE + NO3 = 0.5 NO2 + 0.5 NTR + 0.48 XO2 + 0.48 XO2H + 0.04 XO2N + RO2 + 0.50
     &      +( 0.850)*r(159)  ! ISPD + NO3 = 0.643 CO + 0.282 FORM + 0.357 ALDX + 1.282 PAR + 0.85 HO2 + 0.075 CXO3
     &      +( 0.530)*r(171)  ! TERP + NO3 = 0.47 NO2 + 0.28 XO2H + 0.75 XO2 + 0.25 XO2N + 1.28 RO2 + 0.47 ALDX
     &      +( 0.500)*r(202)  ! XOPN + NO3 = 0.5 NO2 + 0.5 NTR + 0.45 XO2H + 0.45 XO2 + 0.1 XO2N + RO2 + 0.25
     &      +( 0.117)*r(150)  ! ISO2 + NO = 0.117 INTR  + 0.883 NO2 + 0.803 HO2  + 0.66 FORM + 0.66 ISPD + 0.08 XO
     &      +( 0.650)*r(156)  ! ISOP + NO3 = 0.35 NO2 + 0.65 INTR + 0.64 XO2H + 0.33 XO2 + 0.03 XO2N + RO2 + 0.3
C
C...  { NOz recycled to NOx }
C     potentially important NOx recycling are
C     OH reaction with nitric acid
C     Nitric acid photolysis
C     Organic nitrate photolysis
C     OH reaction with organic nitrates
      nn = nn + 1
      ptname(nn)  = 'NOxrecycl'
      PA(nn) =  
     &      +         r( 46)  ! HNO3 + OH = NO3
     &      +         r( 47)  ! HNO3 = OH + NO2
     &      +         r( 92)  ! NTR = NO2 + XO2H + RO2
     &      +( 0.629)*r(167)  ! INTR + OH = 0.63 XO2 + 0.37 XO2H + RO2 + 0.444 NO2 + 0.185 NO3 + 0.104 INTR + 0.592
C
C...  { NO to NO2 by HO2 }
      nn = nn + 1
      ptname(nn)  = 'NOw_HO2'
      PA(nn) =  
     &      +( 1.000)*r( 25)  ! HO2 + NO = OH + NO2
C
C...  { NO to NO2 by RO2s }
      nn = nn + 1
      ptname(nn)  = 'NOw_RO2s'
      PA(nn) =  
     &      +         r( 71)  ! MEO2 + NO  = FORM + HO2 + NO2
     &      +         r( 75)  ! XO2H + NO  = NO2 + HO2
     &      +         r( 79)  ! XO2 + NO  = NO2
     &      +( 0.883)*r(150)  ! ISO2 + NO = 0.117 INTR  + 0.883 NO2 + 0.803 HO2  + 0.66 FORM + 0.66 ISPD + 0.08 XO
     &      +         r(164)  ! EPX2 + NO = 0.275 GLYD + 0.275 GLY + 0.275 MGLY + 0.125 OH + 0.825 HO2 + 0.375 FORM +
     &      +( 0.918)*r(173)  ! BZO2 + NO = 0.918 NO2 + 0.082 NTR + 0.918 GLY  + 0.918 OPEN + 0.918 HO2
     &      +( 0.860)*r(178)  ! TO2 + NO = 0.86 NO2 + 0.14 NTR + 0.417 GLY + 0.443 MGLY + 0.66 OPEN + 0.2 XOPN +
     &      +( 0.860)*r(183)  ! XLO2 + NO = 0.86 NO2 + 0.14 NTR + 0.221 GLY + 0.675 MGLY + 0.3 OPEN + 0.56 XOPN
     &      +         r(195)  ! CRN2 + NO = CRNO + NO2
     &      +( 0.860)*r(209)  ! CAO2 + NO = 0.86 NO2 + 0.14 NTR + 1.2 HO2 + 0.344 FORM + 0.344 CO
C
C...  { NO to NO2 by RCO3s }
      nn = nn + 1
      ptname(nn)  = 'NOw_RCO3s'
      PA(nn) =  
     &      +         r( 53)  ! C2O3 + NO = NO2 + MEO2 + RO2
     &      +         r( 61)  ! CXO3 + NO = NO2 + ALD2 + XO2H + RO2
     &      +         r(213)  ! OPO3 + NO = NO2 + XO2H + RO2 + ALDX
C
C-----------------End of NOy Chemistry----------------------------------
C
C
C...  Convert to ppb/hr
      Do n = 1,nn
        PA(n) = ppbfact*PA(n)
      End do
C
C
C...  J(NO2) photolysis rate (per hour)
      nn = nn + 1
      ptname(nn)  = 'J_NO2'
      PA(nn) =      rk(1)*dtfact
C
C
C...  J(O3O1D) photolysis rate (per hour)
      nn = nn + 1
      ptname(nn)  = 'J_O3O1D'
      PA(nn) =      rk(9)*dtfact
C
C...  Ckeck that MXCPA is set large enough
C
      If( nn .GT. MXCPA ) Then
         write(iout,'(//,a)') 'ERROR in CPAMECH6'
         write(iout,'(a)')
     &           'Number of outputs requested (NN) exceeds limit.'
         write(iout,'(a)') 'Increase parameter MXCPA and try again.'
         write(iout,'(a,i5)') '  MXCPA =', MXCPA
         write(iout,'(a,i5)') '     NN =', nn
         call camxerr()
      End if
C
C...  Set num of outputs on first dummy call from pasetup
C
      NPA_INIT = nn
C
      Return
      End
