      subroutine cpamech5( r, rk, dtfact, nr, pa, npa, npa_init, ldark )
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
C    *** NOTE:  This code is hardwired for the CAMx SAPRC99 ***
C
C  Arguments:  R should be the average rate during the most recent
C              chemistry time step, multipled by the time step, eg:
C              the chem solver calculates 0.5*dt*(R+Rold) and passes it
C              to this routine. It's OK to use simply R*dt, but this
C              is a slightly less accurate form of the integration.
C              Important reaction rates are summed below to calculate PA
C              which is passed back to the Chem routine and accumulated for
C              one hour and then output.
C              RK is the rate constants in ppm-n hr-1.
C              DTFACT is the ratio of time step to averaging interval.
C              NR is the number of reactions.
C              NPA is the number of chemical process rates calculated below.
C              NPA_INIT sets num of outputs on first dummy call from pasetup.
C              LDARK is flag set true at night.
C              R is assumed to be ppm.
C              PA is converted to ppb.
C               
C** REVISION HISTORY:
C**   NO.    DATE   WHO  WHAT
C**   ---    -----  ---  --------------------------------------------
C**   01     12/02  GST  Created for use with the SAPRC99 mechanism
C**   02     08/03  GY   Added output of J(NO2) and J(O3 to O1D) in hr-1
C**   03     07/05  GST  Added new output variables
C**   04     08/05  GY   Revised radical prod/loss etc. and chain length
C**   05     08/07  GY   Added ETOH, MTBE and MBUT
C**   06     08/07  PP|GY  Corrected errors and added cyc_HCO3 and HCHOp
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
C
      REAL   cyc_HONO_p, cyc_HONO_l
      REAL   cyc_H2O2_p, cyc_H2O2_l
      REAL   cyc_ROOH_p, cyc_ROOH_l
      REAL   cyc_HNO4_p, cyc_HNO4_l
      REAL   cyc_HCO3_p, cyc_HCO3_l
      REAL   cyc_PAN_p,  cyc_PAN_l
      REAL   cyc_PAN2_p, cyc_PAN2_l
      REAL   cyc_MPAN_p, cyc_MPAN_l
      REAL   cyc_PBZN_p, cyc_PBZN_l
      REAL   OH_new, HO2_new, HOx_new, RO2_new
      REAL   OH_reacted, HO2_reacted, HOx_reacted
      REAL   OH_term, HO2_term, HOx_term
      REAL   OHwHC
      REAL   other_OH_prop
      REAL   POx_net, ratio_ind, total_ho2_prod 
      REAL   prod_h2o2, prod_hno3, o3_prod,o3_loss 
      REAL   Y_OH_per_HO2, OH_from_HO2
      REAL   total_new_HOx, total_new_HO2, rad_res_new_OH
      REAL   total_new_OH, OHnew_Ox_HC
      REAL   ho2_frac_ro2r, ho2_frac_cxo2, ho2tmp
      REAL   cxo2_frac_cco3, ro2r_frac_rco3
      REAL   cco3_frac_mco3
      REAL   hcho_from_ethe, hcho_from_ole1
      REAL   hcho_from_ole2, hcho_from_terp
      REAL   hcho_from_isop, hcho_from_ispd
      REAL   sum
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
C     OHw_ALK1
C     OHw_ALK2
C     OHw_ALK3
C     OHw_ALK4
C     OHw_ALK5
C     OHw_ARO1
C     OHw_ARO2
C     OHw_ETHE
C     OHw_OLE1
C     OHw_OLE2
C     OHw_ISOP
C     OHw_TERP
C     OHw_all_HC
C     ISOPwOx
C     TERPwOx
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
C     HCHOp_ethe
C     HCHOp_ole1
C     HCHOp_ole2
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
C     Ox =O3, NO2, NO3*2, O3P, O1D, HNO3, RNO3, PAN, PAN2, MPAN, HNO4,
C         N2O5*3, ORNIT, PBZN, XN
      nn =  1
      ptname(nn)  = 'OxProd'
      PA(nn) = 
     &      + 2.000*R( 10) !{NO+NO}
     &      + 1.000*R( 23) !{HONO}
     &      + 1.000*R( 24) !{OH+HONO}
     &      + 1.000*R( 27) !{OH+HNO3}
     &      + 1.000*R( 31) !{HO2+NO}
     &      + 0.390*R( 34) !{HNO4}
     &      + 1.000*R( 46) !{CXO2+NO}
     &      + 1.000*R( 51) !{RO2R+NO}
     &      + 1.000*R( 56) !{R2O2+NO}
     &      + 1.000*R( 62) !{RO2N+NO}
     &      + 1.000*R( 71) !{CCO3+NO}
     &      + 0.250*R( 72) !{CCO3+HO2}
     &      + 1.000*R( 81) !{RCO3+NO}
     &      + 0.250*R( 82) !{RCO3+HO2}
     &      + 1.000*R( 92) !{BZCO+NO}
     &      + 0.250*R( 93) !{BZCO+HO2}
     &      + 1.000*R(104) !{MCO3+NO}
     &      + 0.250*R(105) !{MCO3+HO2}
     &      + 1.000*R(120) !{BZNO+NO2}
     &      + 1.000*R(128) !{HCO3+NO}
C
C
      nn =  nn + 1
      ptname(nn)  = 'OxLoss'
      PA(nn) = 
     &      + 2.000*R(  3) !{O+O3}
     &      + 2.000*R(  5) !{O+NO2}
     &      + 1.000*R( 13) !{N2O5+H2O}
     &      + 2.000*R( 14) !{NO2+NO3}
     &      + 2.000*R( 15) !{NO3}
     &      + 1.000*R( 19) !{O1D+H2O}
     &      + 1.000*R( 26) !{OH+NO3}
     &      + 1.000*R( 30) !{OH+O3}
     &      + 1.000*R( 36) !{HO2+O3}
     &      + 1.000*R( 39) !{NO3+HO2}
     &      + 2.000*R( 40) !{NO3+NO3}
     &      + 1.000*R( 48) !{CXO2+NO3}
     &      + 1.000*R( 53) !{RO2R+NO3}
     &      + 1.000*R( 58) !{R2O2+NO3}
     &      + 1.000*R( 65) !{RO2N+NO3}
     &      + 1.000*R( 73) !{CCO3+NO3}
     &      + 1.000*R( 83) !{RCO3+NO3}
     &      + 1.000*R( 94) !{BZCO+NO3}
     &      + 1.000*R(106) !{MCO3+NO3}
     &      + 1.000*R(117) !{BZO+NO2}
     &      + 1.000*R(129) !{HCHO+NO3}
     &      + 1.000*R(132) !{CCHO+NO3}
     &      + 1.000*R(135) !{RCHO+NO3}
     &      + 1.000*R(148) !{GLY+NO3}
     &      + 1.000*R(151) !{MGLY+NO3}
     &      + 1.000*R(154) !{PHEN+NO3}
     &      + 1.000*R(156) !{CRES+NO3}
     &      + 1.000*R(157) !{NPHE+NO3}
     &      + 1.000*R(160) !{BALD+NO3}
     &      + 1.000*R(162) !{METH+O3}
     &      + 1.000*R(163) !{METH+NO3}
     &      + 1.000*R(164) !{METH+O}
     &      + 1.000*R(167) !{MVK+O3}
     &      + 1.000*R(168) !{MVK+O}
     &      + 1.000*R(171) !{ISPD+O3}
     &      + 1.000*R(172) !{ISPD+NO3}
     &      + 1.000*R(179) !{DCB1+O3}
     &      + 1.000*R(186) !{ETHE+O3}
     &      + 1.000*R(187) !{ETHE+NO3}
     &      + 1.000*R(188) !{ETHE+O}
     &      + 1.000*R(190) !{ISOP+O3}
     &      + 1.000*R(191) !{ISOP+NO3}
     &      + 1.000*R(192) !{ISOP+O}
     &      + 1.000*R(194) !{TERP+O3}
     &      + 1.000*R(195) !{TERP+NO3}
     &      + 1.000*R(196) !{TERP+O}
     &      + 1.000*R(205) !{OLE1+O3}
     &      + 1.000*R(206) !{OLE1+NO3}
     &      + 1.000*R(207) !{OLE1+O}
     &      + 1.000*R(209) !{OLE2+O3}
     &      + 1.000*R(210) !{OLE2+NO3}
     &      + 1.000*R(211) !{OLE2+O}
     &      + 1.000*R(215) !{MBUT+O3}
     &      + 1.000*R(216) !{MBUT+NO3}
     &      + 1.000*R(217) !{MBUT+O}
C
C
C...  calculate ratio of P(H2O2)/P(HNO3) to use as indicator 
C     of NOX or VOC sensitivity
      prod_h2o2 =  R(37) + R(38)
      prod_hno3 =  R(25)
      if (prod_hno3 .GT. 0.0) then
        ratio_ind =  prod_h2o2/prod_hno3
      else
        ratio_ind = 999.
      end if
cC
cC
cC.....{ Net Ox Prod sensitive to VOC or NOx}
c      nn =  nn + 1
c      ptname(nn)   = 'POx_VOCsns'
c      ptname(nn+1) = 'POx_NOxsns'
c      POx_net = PA(1) - PA(2)
c      if (POx_net .LT. 0.0) POx_net = 0.0
cC
cC....increment POx
c      if (ratio_ind .LT. ratio_cut ) then
c        PA(nn)   = POx_net
c        PA(nn+1) = 0.0
c      else
c        PA(nn)   = 0.0
c        PA(nn+1) = POx_net
c      end if
cC...increment counter for POx_NOx_sens (used above)
c      nn = nn + 1
C
C
C...  { Net O3 Production }
      nn =  nn + 1
      ptname(nn)   = 'PO3_net'
      O3_prod =      R(  2)  ! O3P + O2
     &        + 0.25*R( 72)  ! CCO3
     &        + 0.25*R( 82)  ! RCO3
     &        + 0.25*R( 93)  ! BZCO
     &        + 0.25*R(105)  ! MCO3
      O3_loss = R(  3)  ! O3+O3P
     &        + R(  7)  ! NO
     &        + R(  8)  ! NO2
     &        + R( 17)  ! hv
     &        + R( 18)  ! hv
     &        + R( 30)  ! OH
     &        + R( 36)  ! HO2
     &        + R(162)  ! METH
     &        + R(167)  ! MVK
     &        + R(171)  ! ISPD
     &        + R(179)  ! DCB1
     &        + R(186)  ! ETHE
     &        + R(190)  ! ISOP
     &        + R(194)  ! TERP
     &        + R(205)  ! OLE1
     &        + R(209)  ! OLE2
     &        + R(215)  ! MBUT
      PA(nn) = O3_prod - O3_loss
C
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
C
C...  { O3 destruction by several pathways }
C
      nn =  nn + 1
      ptname(nn)   = 'O3_dest'
C
C...  O1D + water
      PA(nn) =
     &       - R(19)
C...  HO2 + O3 (assuming no OH is recycled via OH+H2O2, OH+H2)
      PA(nn) =  PA(nn)
     &       - R(36)
C...  OH + O3 (accounting for fraction of HO2 recycled via NO)
C
C     ho2tmp is reaction of HO2 except with NO
      ho2tmp = R(43)+R(47)+R(52)+R(57)+R(63)+R(118)+R(121)+R(126)
     &              + 0.75 * ( R(72)+R(82)+R(93)+R(105) )
     &              + 2.0 * ( R(37)+R(38) )
C
      PA(nn) =  PA(nn) 
     &       - R(30) * ho2tmp 
     &               / ( ho2tmp + R(31) +0.8*R(39) )
C...  O(3P) + VOC
      PA(nn) =  PA(nn) 
     &       - ( R(164)+R(168)+R(188)+R(192)
     &                               +R(196)+R(207)+R(211)+R(217) )
C...  O3 + VOC
      PA(nn) =  PA(nn) 
     &       - ( R(162)+R(167)+R(171)+R(179)+R(186)
     &                        +R(190)+R(194)+R(205)+R(209)+R(215) )
C
      PA(nn) = amin1( PA(nn), O3_prod - O3_loss)
C
C--------------------End of Ox & O3-----------------------
C
C
C...  { Radical Reservoirs: HONO, HNO4, H2O2  etc }
C     We will call these termination rxns if there is net production
C     during a time step, or initiation if there is net release of HOx.
C
C  { 21} NO+OH=HONO
C  { 22} HONO=OH+NO
C
        sum =  R(21) - R(22)    ! HONO cycle
        if (sum.GT.0.) then
          cyc_HONO_p = sum
          cyc_HONO_l = 0.0
        else
          cyc_HONO_p =  0.0
          cyc_HONO_l = -sum
        end if
C
C  { 32} HO2+NO2=HNO4
C  { 33} HNO4=HO2+NO2
C  { 34} HNO4=0.610*HO2+0.610*NO2+0.390*OH+0.390*NO3
        sum =  R(32) - R(33) - 0.61*R(34)    ! HNO4 cycle
        if (sum.GT.0.) then
          cyc_HNO4_p = sum
          cyc_HNO4_l = 0.0
        else
          cyc_HNO4_p = 0.0
          cyc_HNO4_l = -sum
        end if
C
C
C{ 37} HO2+HO2=H2O2
C{ 38} HO2+HO2+M=H2O2
C{ 41} H2O2=2.00*OH
C
        sum =  R(37) + R(38) - R(41)     ! H2O2 cycle
        if (sum.GT.0.) then
         cyc_H2O2_p = 2.*sum
         cyc_H2O2_l = 0.0
        else
         cyc_H2O2_p =  0.0
         cyc_H2O2_l = -2.0*sum
        end if
C
C
C{126} HCHO + HO2 = HCO3
C{127} HCO3 = HO2 + HCHO
C
        sum =  R(126) - R(127)     ! HCO3 cycle
        if (sum.GT.0.) then
         cyc_HCO3_p = sum
         cyc_HCO3_l = 0.0
        else
         cyc_HCO3_p =  0.0
         cyc_HCO3_l = -sum
        end if
C
C
C{ ..} RO2+HO2=OOH
C{ ..} OOH=HO2+OH
C
        sum =  R( 52)   ! ROOH cycle
     &      +  R( 63)   ! ROOH cycle
     &      -  R(144)   ! ROOH cycle
     &      +  R( 47)   ! COOH cycle
     &      -  R(142)   ! COOH cycle
        if (sum.GT.0.) then
          cyc_ROOH_p = sum
          cyc_ROOH_l = 0.0
        else
          cyc_ROOH_p = 0.0
          cyc_ROOH_l = -sum
        end if
C
C
C  { 69} CCO3+NO2=PAN
C  { 70} PAN=CCO3+NO2
C
        sum = R(69) - R(70)    ! PAN Cycle
        if (sum.GT.0.) then
          cyc_PAN_p = sum
          cyc_PAN_l = 0.0
        else
          cyc_PAN_p =  0.0
          cyc_PAN_l = -sum
        end if
C
C
C  { 79} RCO3+NO2=PAN2
C  { 80} PAN2=RCO3+NO2
C
        sum = R(79) - R(80)    ! PAN2 Cycle
        if (sum.GT.0.) then
          cyc_PAN2_p = sum
          cyc_PAN2_l = 0.0
        else
          cyc_PAN2_p =  0.0
          cyc_PAN2_l = -sum
        end if
C
C  { 90} BZCO+NO2=PBZN
C  { 91} PBZN=BZCO+NO2        ! PBZN Cycle
        sum = R(90) - R(91)    
        if (sum.GT.0.) then
          cyc_PBZN_p = sum
          cyc_PBZN_l = 0.0
        else
          cyc_PBZN_p =  0.0
          cyc_PBZN_l = -sum
        end if
C
C  {102} MCO3+NO2=MPAN
C  {103} MPAN=MCO3+NO2
C
        sum =  R(102) - R(103) ! MPAN Cycle
        if (sum.GT.0.) then
          cyc_MPAN_p = sum
          cyc_MPAN_l = 0.0
        else
          cyc_MPAN_p =  0.0
          cyc_MPAN_l = -sum
        end if
C
C
C     {      R a d i c a l    I n i t i a t i o n       }
C
C     S99 has recursive radical production that must be
C     considered when tracking radical initiation, i.e.:
C     CXO2 makes some HO2
C     RO2R makes some HO2
C     CCO3 makes some CXO2
C     RCO3 makes some RO2R
C     MCO3 makes some CCO3
C
C...  Fractional yield of HO2 from CXO2
C     46 CXO2 NO = 1 NO2 1 HCHO 1 HO2
C     47 CXO2 HO2 = 1 COOH 1 O2
C     48 CXO2 NO3 = 1 HCHO 1 HO2 1 NO2
C     49 CXO2 CXO2 = 1 MEOH 1 HCHO 1 O2
C     50 CXO2 CXO2 = 2 HCHO 2 HO2
C     54 RO2R CXO2 = 1 HO2 0.75 HCHO 0.25 MEOH
C     59 R2O2 CXO2 = 1 CXO2
C     64 RO2N CXO2 = 1 HO2 0.25 MEOH 0.5 MEK
C     74 CCO3 CXO2 = 1 CO2H 1 HCHO
C     84 RCO3 CXO2 = 1 RC2H 1 HCHO
C     95 BZCO CXO2 = 1 RC2H 1 HCHO
C     107 MCO3 CXO2 = 1 RC2H 1 HCHO
      ho2_frac_cxo2 = ( r(46)+r(48)+2.0*r(50)+0.5*r(54)+0.5*r(64) ) /
     &                ( r(46)+r(47)+r(48)+2.0*r(49)+2.0*r(50)+r(54)
     &                       + r(59)+r(64)+r(74)+r(84)+r(95)+r(107) )
C
C...  Fractional yield of HO2 from RO2R
C     51 RO2R NO = 1 NO2 1 HO2
C     52 RO2R HO2 = 1 ROOH
C     53 RO2R NO3 = 1 NO2 1 O2 1 HO2
C     54 RO2R CXO2 = 1 HO2 0.75 HCHO 0.25 MEOH
C     55 RO2R RO2R = 1 HO2
C     60 R2O2 RO2R = 1 RO2R
C     66 RO2N RO2R = 1 HO2 0.5 MEK 0.5 PROD
C     75 CCO3 RO2R = 1 CO2H
C     85 RCO3 RO2R = 1 RC2H
C     96 BZCO RO2R = 1 RC2H
C     108 MCO3 RO2R = 1 RC2H
      ho2_frac_ro2r = ( r(51)+r(53)+0.5*r(54)+r(55)+0.5*r(66) ) /
     &                ( r(51)+r(52)+r(53)+r(54)+2.0*r(55)+r(60)+r(66)
     &                                   + r(75)+r(85)+r(96)+r(108) )
C
C...  Fractional yield of CXO2 from CCO3
C     69 CCO3 NO2 = 1 PAN
C     70 PAN = 1 CCO3 1 NO2
C     71 CCO3 NO = 1 CXO2 1 NO2
C     72 CCO3 HO2 = 0.75 CO3H 0.25 CO2H 0.25 O3
C     73 CCO3 NO3 = 1 CXO2 1 NO2
C     74 CCO3 CXO2 = 1 CO2H 1 HCHO
C     75 CCO3 RO2R = 1 CO2H
C     76 CCO3 R2O2 = 1 CCO3
C     77 CCO3 RO2N = 1 CO2H 1 PROD
C     78 CCO3 CCO3 = 2 CXO2
C     88 RCO3 CCO3 = 1 CXO2 1 CCHO 1 RO2R
C     99 BZCO CCO3 = 1 CXO2 1 BZO 1 R2O2
C     111 MCO3 CCO3 = 1 CXO2 1 HCHO 1 CCO3
      cxo2_frac_cco3 = ( r(71)+r(73)+2.0*r(78)+r(88)+r(99)+r(111) ) /
     &                 ( r(71)+r(72)+r(73)+r(74)+r(75)+r(76)
     &                   +r(77)+r(88)+r(99)+r(111)+2.0*r(78)+cyc_PAN_p )
C
C...  Fractional yield of RO2R from RCO3
C     79 RCO3 NO2 = 1 PAN2
C     80 PAN2 = 1 RCO3 1 NO2
C     81 RCO3 NO = 1 NO2 1 CCHO 1 RO2R
C     82 RCO3 HO2 = 0.75 RC3H 0.25 RC2H 0.25 O3
C     83 RCO3 NO3 = 1 NO2 1 CCHO 1 RO2R
C     84 RCO3 CXO2 = 1 RC2H 1 HCHO
C     85 RCO3 RO2R = 1 RC2H
C     86 RCO3 R2O2 = 1 RCO3
C     87 RCO3 RO2N = 1 RC2H 1 PROD
C     88 RCO3 CCO3 = 1 CXO2 1 CCHO 1 RO2R
C     89 RCO3 RCO3 = 2 CCHO 2 RO2R
C     100 BZCO RCO3 = 1 CCHO 1 RO2R 1 BZO
C     112 MCO3 RCO3 = 1 HCHO 1 CCO3 1 CCHO
      ro2r_frac_rco3 = ( r(81)+r(83)+r(88)+2.0*r(89)+r(100)+r(112) ) /
     &                 ( r(81)+r(82)+r(83)+r(84)+r(85)+r(86)
     &                 +r(87)+r(88)+r(100)+r(112)+2.0*r(89)+cyc_PAN2_p )
C
C...  Fractional yield of CCO3 from MCO3
C     102 MCO3 NO2 = 1 MPAN
C     103 MPAN = 1 MCO3 1 NO2
C     104 MCO3 NO = 1 NO2 1 HCHO 1 CCO3
C     105 MCO3 HO2 = 0.75 RC3H 0.25 RC2H 0.25 O3
C     106 MCO3 NO3 = 1 NO2 1 HCHO 1 CCO3
C     107 MCO3 CXO2 = 1 RC2H 1 HCHO
C     108 MCO3 RO2R = 1 RC2H
C     109 MCO3 R2O2 = 1 MCO3
C     110 MCO3 RO2N = 2 RC2H
C     111 MCO3 CCO3 = 1 CXO2 1 HCHO 1 CCO3
C     112 MCO3 RCO3 = 1 HCHO 1 CCO3 1 CCHO
C     113 MCO3 BZCO = 1 HCHO 1 CCO3 1 BZO
C     114 MCO3 MCO3 = 2 HCHO 2 CCO3
      cco3_frac_mco3 = ( r(104)+r(106)+r(111)+r(112)+
     &                   r(113)+2.0*r(114) ) /
     &                 ( r(104)+r(105)+r(106)+r(107)+r(108)+r(109)
     &              +r(110)+r(111)+r(112)+r(113)+2.0*r(114)+cyc_MPAN_p )
C
C
cgy      nn = nn + 1
cgy      ptname(nn)  = 'HO2frCXO2'
cgy      PA(nn) = ho2_frac_cxo2 * (dtfact/ppbfact)
C
cgy      nn = nn + 1
cgy      ptname(nn)  = 'HO2frRO2R'
cgy      PA(nn) = ho2_frac_ro2r * (dtfact/ppbfact)
C
cgy      nn = nn + 1
cgy      ptname(nn)  = 'CXO2frCCO3'
cgy      PA(nn) = cxo2_frac_cco3 * (dtfact/ppbfact)
C
cgy      nn = nn + 1
cgy      ptname(nn)  = 'RO2RfrRCO3'
cgy      PA(nn) = ro2r_frac_rco3 * (dtfact/ppbfact)
C
cgy      nn = nn + 1
cgy      ptname(nn)  = 'CCO3frMCO3'
cgy      PA(nn) = cco3_frac_mco3 * (dtfact/ppbfact)
C
C
C...  { New HOx: OH or HO2 produced without HOx as a reactant }
C     new HOx comes from photolysis and ozone reactions
C     include HOx from O(1D) and O atoms these are photolysis
C     include HOx from NO3 reactions because they result from O3 or O atoms
C     include HO2 from C2O3 from photolysis of carbonyls
C     don't include HOx from TBUO and most RO2s
C
C...  new OH
      OH_new =
     &                +( 2.000)*r( 19)!  19 O1D H2O = 2 OH
     &                +( 1.000)*r( 22)!  22 HONO = 1 OH 1 NO
     &                +( 1.000)*r( 28)!  28 HNO3 = 1 OH 1 NO2
     &                +( 0.390)*r( 34)!  34 HNO4 = 0.61 HO2 0.61 NO2 0.39 OH 0.39 NO3
     &                +( 2.000)*r( 41)!  41 HO2H = 2 OH
     &                +( 1.000)*r(142)! 142 COOH = 1 HCHO 1 HO2 1 OH
     &                +( 1.000)*r(144)! 144 ROOH = 1 RCHO 1 HO2 1 OH
     &                +( 0.208)*r(162)! 162 METH O3 = 0.008 HO2 0.1 RO2R 0.208 OH
     &                +( 0.330)*r(165)! 165 METH = 0.34 HO2 0.33 RO2R 0.33 OH
     &                +( 0.164)*r(167)! 167 MVK O3 = 0.064 HO2 0.05 RO2R 0.164 OH
     &                +( 0.285)*r(171)! 171 ISPD O3 = 0.4 HO2 0.048 RO2R 0.048 RCO3
     &                +( 0.500)*r(179)! 179 DCB1 O3 = 1.5 HO2 0.5 OH 1.5 CO
     &                +( 0.120)*r(186)! 186 ETHE O3 = 0.12 OH 0.12 HO2 0.5 CO
     &                +( 0.266)*r(190)! 190 ISOP O3 = 0.266 OH 0.066 RO2R 0.008 RO2N
     &                +( 0.567)*r(194)! 194 TERP O3 = 0.567 OH 0.033 HO2 0.031 RO2R
     &                +( 0.155)*r(205)! 205 OLE1 O3 = 0.155 OH 0.056 HO2 0.022 RO2R
     &                +( 0.378)*r(209)! 209 OLE2 O3 = 0.378 OH 0.003 HO2 0.033 RO2R
     &                +( 0.099)*r(215)! 215 MBUT O3 = 0.099 OH 0.099 HO2 0.365 CO 
C...  new HO2
      HO2_new =
     &                +( 1.000)*r( 23)!  23 HONO = 1 HO2 1 NO2
C in cyc_HNO4_l    &  +( 1.000)*r( 33)!  33 HNO4 = HO2 NO24
C in cyc_HNO4_l    &  +( 0.610)*r( 34)!  34 HNO4 = 0.61 HO2 0.61 NO2 0.39 OH
     &                +( 2.000)*r(123)! 123 HCHO = 2 HO2 1 CO
     &                + cyc_HCO3_l    ! 127 HCO3 = HO2 HCHO
     &                +( 1.000)*r(129)! 129 HCHO NO3 = 1 HNO3 1 HO2 1 CO
     &                +( 1.000)*r(131)! 131 CCHO = 1 CO 1 HO2 1 CXO2
     &                +( 1.000)*r(134)! 134 RCHO = 1 CCHO 1 RO2R 1 CO
     &                +( 1.000)*r(142)! 142 COOH = 1 HCHO 1 HO2 1 OH
     &                +( 1.000)*r(144)! 144 ROOH = 1 RCHO 1 HO2 1 OH
     &                +( 2.000)*r(145)! 145 GLY = 2 CO 2 HO2
     &                +( 0.630)*r(148)! 148 GLY NO3 = 1 HNO3 0.63 HO2 1.26 CO
     &                +( 1.000)*r(149)! 149 MGLY = 1 HO2 1 CO 1 CCO3
     &                +( 0.008)*r(162)! 162 METH O3 = 0.008 HO2 0.1 RO2R 0.208 OH
     &                +( 0.340)*r(165)! 165 METH = 0.34 HO2 0.33 RO2R 0.33 OH
     &                +( 0.064)*r(167)! 167 MVK O3 = 0.064 HO2 0.05 RO2R 0.164 OH
     &                +( 0.400)*r(171)! 171 ISPD O3 = 0.4 HO2 0.048 RO2R 0.048 RCO3
     &                +( 1.233)*r(173)! 173 ISPD = 1.233 HO2 0.467 CCO3 0.3 RCO3
     &                +( 0.341)*r(177)! 177 RNO3 = 1 NO2 0.341 HO2 0.564 RO2R
     &                +( 1.500)*r(179)! 179 DCB1 O3 = 1.5 HO2 0.5 OH 1.5 CO
     &                +( 0.500)*r(181)! 181 DCB2 = 1 RO2R 0.5 CCO3 0.5 HO2
     &                +( 0.500)*r(183)! 183 DCB3 = 1 RO2R 0.5 CCO3 0.5 HO2
     &                +( 0.120)*r(186)! 186 ETHE O3 = 0.12 OH 0.12 HO2 0.5 CO
     &                +( 0.500)*r(188)! 188 ETHE O = 0.5 HO2 0.2 RO2R 0.3 CXO2
     &                +( 0.033)*r(194)! 194 TERP O3 = 0.567 OH 0.033 HO2 0.031 RO2R
     &                +( 0.056)*r(205)! 205 OLE1 O3 = 0.155 OH 0.056 HO2 0.022 RO2R
     &                +( 0.003)*r(209)! 209 OLE2 O3 = 0.378 OH 0.003 HO2 0.033 RO2R
     &                +( 0.013)*r(211)! 211 OLE2 O = 0.013 HO2 0.012 RO2R 0.001 RO2N
     &                +( 0.099)*r(215)! 215 MBUT O3 = 0.099 OH 0.099 HO2 0.365 CO 
C
C... Decomposition of PAN compounds
C    33 HNO4 = HO2 NO2
C    34 HNO4 = 0.61 HO2 0.61 NO2 0.39 OH
C    70 PAN = 1 CCO3 1 NO2
C    80 PAN2 = 1 RCO3 1 NO2
C    91 PBZN = 1 BZCO 1 NO2
C    103 MPAN = 1 MCO3 1 NO2
      HO2_new = HO2_new 
     &            + cyc_HNO4_l
     &            + cyc_PAN_l * cxo2_frac_cco3 * ho2_frac_cxo2
     &            + cyc_PAN2_l * ro2r_frac_rco3 * ho2_frac_ro2r
     &            + cyc_PBZN_l * 0.0  ! no subsequent HOx from BZCO
     &            + cyc_MPAN_l * cco3_frac_mco3 * cxo2_frac_cco3
     &                                                 * ho2_frac_cxo2
C
C...new HO2 via CXO2 from CCHO, ACET, MVK photolysis
C    131 CCHO = 1 CO 1 HO2 1 CXO2
C    137 ACET = 1 CCO3 1 CXO2
C    169 MVK = 0.3 CXO2 0.7 CO 0.7 PROD
      HO2_new = HO2_new 
     &            + ( r(131)+r(137)+0.3*r(169) ) * ho2_frac_cxo2
C
C...new HO2 via CCO3 from ACET, MEK, MGLY, BACL, METH, ISPD, PROD, DCB2,
C   DCB3  photolysis
C    137 ACET = 1 CCO3 1 CXO2
C    139 MEK = 1 CCO3 1 CCHO 1 RO2R
C    149 MGLY = 1 HO2 1 CO 1 CCO3
C    152 BACL = 2 CCO3
C    165 METH = 0.34 HO2 0.33 RO2R 0.67 CCO3
C    173 ISPD = 1.233 HO2 0.467 CCO3 0.3 RCO3
C    175 PROD = 0.96 RO2R 0.04 RO2N 0.667 CCO3
C    181 DCB2 = 1 RO2R 0.5 CCO3 0.5 HO2
C    183 DCB3 = 1 RO2R 0.5 CCO3 0.5 HO2
      HO2_new = HO2_new 
     &       + ( r(137)+r(139)+r(149)+2.0*r(152)+0.67*r(165)
     &         + 0.467*r(173)+0.667*r(175)+0.5*r(181)+ 0.5*r(183) )
     &                        * ho2_frac_cxo2 * cxo2_frac_cco3
C
C...new HO2 via RO2R from RCHO, MEK, METH, PROD, DCB2, DCB3 photolysis
C    134 RCHO = 1 CCHO 1 RO2R 1 CO
C    139 MEK = 1 CCO3 1 CCHO 1 RO2R
C    165 METH = 0.34 HO2 0.33 RO2R 0.33 OH
C    175 PROD = 0.96 RO2R 0.04 RO2N 0.667 CCO3 
C    181 DCB2 = 1 RO2R 0.5 CCO3 0.5 HO2
C    183 DCB3 = 1 RO2R 0.5 CCO3 0.5 HO2
      HO2_new = HO2_new 
     &            + ( r(134)+r(139)+0.33*r(165)+0.96*r(175)
     &                  +   r(181)+r(183) ) * ho2_frac_ro2r
C
C...new HO2 via CXO2 from NO3 reactions
C    210 OLE2 NO3 = 0.391 NO2 0.442 RO2R 0.136 RO2N
      HO2_new = HO2_new 
     &            + ( 0.03*r(210) ) * ho2_frac_cxo2
C
C...new HO2 via CCO3 from NO3 reactions
C    132 CCHO NO3 = 1 HNO3 1 CCO3
C    151 MGLY NO3 = 1 HNO3 1 CO 1 CCO3
      HO2_new = HO2_new 
     &            + ( r(132)+r(151) ) * ho2_frac_cxo2 * cxo2_frac_cco3
C
C...new HO2 via RO2R from NO3 reactions
C    163 METH NO3 = 0.5 HNO3 0.5 RO2R 0.5 CO
C    172 ISPD NO3 = 0.799 RO2R 0.051 RO2N 0.15 MCO3
C    187 ETHE NO3 = 1 RO2R 1 RCHO 1 XN
C    191 ISOP NO3 = 0.187 NO2 0.749 RO2R 0.064 RO2N
C    195 TERP NO3 = 0.474 NO2 0.276 RO2R 0.25 RO2N
C    206 OLE1 NO3 = 0.824 RO2R 0.176 RO2N 0.488 R2O2
C    210 OLE2 NO3 = 0.391 NO2 0.442 RO2R 0.136 RO2N
C    216 MBUT NO3 = 0.935 RO2R 0.065 RO2N 0.935 RCHO
      HO2_new = HO2_new 
     &            + ( 0.5*r(163)+0.799*r(172)+r(187)
     &                   + 0.749*r(191)+0.276*r(195)+0.824*r(206)
     &                                + 0.442*r(210)+0.935*r(216) )
     &                                                 * ho2_frac_ro2r
C
C...new HO2 via CXO2 from O3 reactions
C    205 OLE1 O3 = 0.155 OH 0.056 HO2 0.022 RO2R 0.076 CXO2
C    209 OLE2 O3 = 0.378 OH 0.003 HO2 0.033 RO2R 0.197 CXO2
      HO2_new = HO2_new 
     &            + ( 0.076*r(205)+0.197*r(209) ) * ho2_frac_cxo2
C
C...new HO2 via CCO3 from O3 reactions
C    194 TERP O3 = 0.567 OH 0.033 HO2 0.031 RO2R
C    209 OLE2 O3 = 0.378 OH 0.003 HO2 0.033 RO2R
      HO2_new = HO2_new 
     &            + ( 0.123*r(194)+0.137*r(209) )
     &                                * ho2_frac_cxo2 * cxo2_frac_cco3
C
C...new HO2 via RO2R from O3 reactions
C    162 METH O3 = 0.008 HO2 0.1 RO2R 0.208 OH
C    167 MVK O3 = 0.064 HO2 0.05 RO2R 0.164 OH
C    171 ISPD O3 = 0.4 HO2 0.048 RO2R 0.048 RCO3
C    190 ISOP O3 = 0.266 OH 0.066 RO2R 0.008 RO2N
C    194 TERP O3 = 0.567 OH 0.033 HO2 0.031 RO2R
C    205 OLE1 O3 = 0.155 OH 0.056 HO2 0.022 RO2R
C    209 OLE2 O3 = 0.378 OH 0.003 HO2 0.033 RO2R
      HO2_new = HO2_new 
     &            + ( 0.100*r(162)+0.050*r(167)+0.048*r(171)
     &                 + 0.066*r(190)+0.031*r(194)+0.022*r(205)
     &                                + 0.033*r(209) ) * ho2_frac_ro2r
C
C...new HO2 via CXO2 from O atom reactions
C    188 ETHE O = 0.5 HO2 0.2 RO2R 0.3 CXO2
C    192 ISOP O = 0.01 RO2N 0.24 R2O2 0.25 CXO2
      HO2_new = HO2_new 
     &          + ( 0.300*r(188)+0.250*r(192) ) * ho2_frac_cxo2
C
C...new HO2 via CCO3 from O atom reactions
c    none
C
C...new HO2 via RO2R from O atom reactions
C    188 ETHE O = 0.5 HO2 0.2 RO2R 0.3 CXO2
C    211 OLE2 O = 0.013 HO2 0.012 RO2R 0.001 RO2N
      HO2_new = HO2_new 
     &          + ( 0.200*r(188)+0.012*r(211) ) * ho2_frac_ro2r
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
C
C...  { New OH from O1D+H2O }
      nn = nn + 1
      ptname(nn)  = 'newOH_O1D'
      PA(nn) = 2.*R(19)   !O1D+H2O->2*OH
C
C
C...  { New OH from HONO }
      nn = nn + 1
      ptname(nn)  = 'newOH_HONO'
      PA(nn) = cyc_HONO_l
C
C
C...  { New OH from O3 + olefins (only first generation OH) }
      nn = nn + 1
      ptname(nn)  = 'nOH_O3_OLE'
      PA(nn) =
     &       + ( 0.120)*r(186) ! 186 ETHE O3 = 0.12 OH 0.12 HO2 0.5 CO
     &       + ( 0.155)*r(205) ! 205 OLE1 O3 = 0.155 OH 0.056 HO2 0.022 RO2R
     &       + ( 0.378)*r(209) ! 209 OLE2 O3 = 0.378 OH 0.003 HO2 0.033 RO2R
     &       + ( 0.266)*r(190) ! 190 ISOP O3 = 0.266 OH 0.066 RO2R 0.008 RO2N
     &       + ( 0.567)*r(194) ! 194 TERP O3 = 0.567 OH 0.033 HO2 0.031 RO2R
     &       + ( 0.500)*r(179) ! 179 DCB1 O3 = 1.5 HO2 0.5 OH 1.5 CO
     &       + ( 0.208)*r(162) ! 162 METH O3 = 0.008 HO2 0.1 RO2R 0.208 OH
     &       + ( 0.164)*r(167) ! 167 MVK O3 = 0.064 HO2 0.05 RO2R 0.164 OH
     &       + ( 0.285)*r(171) ! 171 ISPD O3 = 0.4 HO2 0.048 RO2R 0.048 RCO3
     &       + ( 0.099)*r(215) ! 215 MBUT O3 = 0.099 OH 0.099 HO2 0.365 CO 
C
C...  { New HO2 from HCHO }
      nn = nn + 1
      ptname(nn)  = 'nwHO2_HCHO'
      PA(nn) = 2.*R(123) !HCHO=2*HO2+CO
     &       +    R(129) !HCHO+NO3
C
C
C...  { New RO2 Production }
C
C      rad initiation for CXO2
C      nn =  nn + 1
C      ptname(nn)  = 'newCXO2    '
C      PA(nn) = 
      RO2_new =
     &      + 1.000*R(131) !{CCHO}
     &      + 1.000*R(137) !{ACET}
     &      + 0.300*R(169) !{MVK}
     &      + 0.300*R(188) !{ETHE+O}
     &      + 0.250*R(192) !{ISOP+O}
     &      + 0.076*R(205) !{OLE1+O3}
     &      + 0.197*R(209) !{OLE2+O3}
     &      + 0.030*R(210) !{OLE2+NO3}
C
C
C...  Rad initiation for CCO3
C      nn =  nn + 1
C      ptname(nn)  = 'newCCO3    '
C      PA(nn) = 
      RO2_new = RO2_new
     &      + cyc_PAN_l    !{PAN}
     &      + 1.000*R(132) !{CCHO+NO3}
     &      + 1.000*R(137) !{ACET}
     &      + 1.000*R(139) !{MEK}
     &      + 1.000*R(149) !{MGLY}
     &      + 1.000*R(151) !{MGLY+NO3}
     &      + 2.000*R(152) !{BACL}
     &      + 0.670*R(165) !{METH}
     &      + 0.467*R(173) !{ISPD}
     &      + 0.667*R(175) !{PROD}
     &      + 0.500*R(181) !{DCB2}
     &      + 0.500*R(183) !{DCB3}
     &      + 0.123*R(194) !{TERP+O3}
     &      + 0.137*R(209) !{OLE2+O3}
C
C
C...  Rad initiation for RCO3
C      nn =  nn + 1
C      ptname(nn)  = 'newRCO3    '
C      PA(nn) = 
      RO2_new = RO2_new
     &      +  cyc_PAN2_l  !{PAN2}
     &      + 1.000*R(135) !{RCHO+NO3}
     &      + 0.370*R(148) !{GLY+NO3}
     &      + 0.100*R(162) !{METH+O3}
     &      + 0.050*R(167) !{MVK+O3}
     &      + 0.048*R(171) !{ISPD+O3}
     &      + 0.300*R(173) !{ISPD}
     &      + 0.333*R(175) !{PROD}
     &      + 0.201*R(194) !{TERP+O3}
     &      + 0.006*R(209) !{OLE2+O3}
C
C
C...  Rad initiation for RO2R
C      nn =  nn + 1
C      ptname(nn)  = 'newRO2R    '
C      PA(nn) = 
      RO2_new = RO2_new
     &      + 1.000*R(134) !{RCHO}
     &      + 1.000*R(139) !{MEK}
     &      + 0.100*R(162) !{METH+O3}
     &      + 0.500*R(163) !{METH+NO3}
     &      + 0.330*R(165) !{METH}
     &      + 0.050*R(167) !{MVK+O3}
     &      + 0.048*R(171) !{ISPD+O3}
     &      + 0.799*R(172) !{ISPD+NO3}
     &      + 0.960*R(175) !{PROD}
     &      + 0.564*R(177) !{RNO3}
     &      + 1.000*R(181) !{DCB2}
     &      + 1.000*R(183) !{DCB3}
     &      + 1.000*R(187) !{ETHE+NO3}
     &      + 0.200*R(188) !{ETHE+O}
     &      + 0.066*R(190) !{ISOP+O3}
     &      + 0.749*R(191) !{ISOP+NO3}
     &      + 0.031*R(194) !{TERP+O3}
     &      + 0.276*R(195) !{TERP+NO3}
     &      + 0.022*R(205) !{OLE1+O3}
     &      + 0.824*R(206) !{OLE1+NO3}
     &      + 0.033*R(209) !{OLE2+O3}
     &      + 0.442*R(210) !{OLE2+NO3}
     &      + 0.012*R(211) !{OLE2+O}
     &      + 0.935*R(216) !{MBUT+NO3}
C
C
C...  Rad initiation for RO2N
C      nn =  nn + 1
C      ptname(nn)  = 'newRO2N    '
C      PA(nn) = 
      RO2_new = RO2_new
     &      + 0.051*R(172) !{ISPD+NO3}
     &      + 0.040*R(175) !{PROD}
     &      + 0.095*R(177) !{RNO3}
     &      + 0.008*R(190) !{ISOP+O3}
     &      + 0.064*R(191) !{ISOP+NO3}
     &      + 0.010*R(192) !{ISOP+O}
     &      + 0.180*R(194) !{TERP+O3}
     &      + 0.250*R(195) !{TERP+NO3}
     &      + 0.001*R(205) !{OLE1+O3}
     &      + 0.176*R(206) !{OLE1+NO3}
     &      + 0.002*R(209) !{OLE2+O3}
     &      + 0.136*R(210) !{OLE2+NO3}
     &      + 0.001*R(211) !{OLE2+O}
     &      + 0.065*R(216) !{MBUT+NO3}
C
C
C...  Rad initiation for BZCO
C      nn =  nn + 1
C      ptname(nn)  = 'newBZCO    '
C      PA(nn) = 
      RO2_new = RO2_new
     &      + cyc_PBZN_l   !{PBZN}
     &      + 1.000*R(160) !{BALD+NO3}
C
C
C...  Rad initiation for MCO3
C      nn =  nn + 1
C      ptname(nn)  = 'newMCO3    '
C      PA(nn) = 
      RO2_new = RO2_new
     &      + cyc_MPAN_l   !{MPAN}
     &      + 0.500*R(163) !{METH+NO3}
     &      + 0.330*R(165) !{METH}
     &      + 0.300*R(169) !{MVK}
     &      + 0.150*R(172) !{ISPD+NO3}
     &      + 0.192*R(190) !{ISOP+O3}
     &      + 0.240*R(192) !{ISOP+O}
C
C
C...  Rad initiation for TBUO is zero
C
C...  Total RO2 Rad initiation
C
      nn =  nn + 1
      ptname(nn)  = 'RO2_new'
      PA(nn) =   RO2_new 
C
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
      PA(nn) = R(29)
C
C...  OH+CH4
      nn = nn + 1
      ptname(nn)  = 'OHw_CH4'
      PA(nn) = R(184)
C
C...  OH+ALK1
      nn = nn + 1
      ptname(nn)  = 'OHw_ALK1'
      PA(nn) = R(197)
C
C...  OH+ALK2
      nn = nn + 1
      ptname(nn)  = 'OHw_ALK2'
      PA(nn) = R(198)
C
C...  OH+ALK3
      nn = nn + 1
      ptname(nn)  = 'OHw_ALK3'
      PA(nn) = R(199)
C
C...  OH+ALK4
      nn = nn + 1
      ptname(nn)  = 'OHw_ALK4'
      PA(nn) = R(200)
C
C...  OH+ALK5
      nn = nn + 1
      ptname(nn)  = 'OHw_ALK5'
      PA(nn) = R(201)
C
C...  OH+ARO1
      nn = nn + 1
      ptname(nn)  = 'OHw_ARO1'
      PA(nn) = R(202)
C
C...  OH+ARO2
      nn = nn + 1
      ptname(nn)  = 'OHw_ARO2'
      PA(nn) = R(203)
C
C...  OH+ETHE
      nn = nn + 1
      ptname(nn)  = 'OHw_ETHE'
      PA(nn) = R(185)
C
C...  OH+OLE1
      nn = nn + 1
      ptname(nn)  = 'OHw_OLE1'
      PA(nn) = R(204)
C
C...  OH+OLE2
      nn = nn + 1
      ptname(nn)  = 'OHw_OLE2'
      PA(nn) = R(208)
C
C...  OH+ISOP
      nn = nn + 1
      ptname(nn)  = 'OHw_ISOP'
      PA(nn) = R(189)
C
C...  OH+TERP
      nn = nn + 1
      ptname(nn)  = 'OHw_TERP'
      PA(nn) = R(193)
C
C
C...  { All OH rxns with organics }
      nn =  nn + 1
      ptname(nn)  = 'OHw_all_HC'
      OHwHC = 
     &      +R( 29) !{OH+CO}
     &      +R(125) !{HCHO+OH}
     &      +R(130) !{CCHO+OH}
     &      +R(133) !{RCHO+OH}
     &      +R(136) !{ACET+OH}
     &      +R(138) !{MEK+OH}
     &      +R(140) !{MEOH+OH}
     &      +R(141) !{COOH+OH}
     &      +R(143) !{ROOH+OH}
     &      +R(147) !{GLY+OH}
     &      +R(150) !{MGLY+OH}
     &      +R(153) !{PHEN+OH}
     &      +R(155) !{CRES+OH}
     &      +R(158) !{BALD+OH}
     &      +R(161) !{METH+OH}
     &      +R(166) !{MVK+OH}
     &      +R(170) !{ISPD+OH}
     &      +R(174) !{PROD+OH}
     &      +R(176) !{RNO3+OH}
     &      +R(178) !{DCB1+OH}
     &      +R(180) !{DCB2+OH}
     &      +R(182) !{DCB3+OH}
     &      +R(184) !{CH4+OH}
     &      +R(185) !{ETHE+OH}
     &      +R(189) !{ISOP+OH}
     &      +R(193) !{TERP+OH}
     &      +R(197) !{ALK1+OH}
     &      +R(198) !{ALK2+OH}
     &      +R(199) !{ALK3+OH}
     &      +R(200) !{ALK4+OH}
     &      +R(201) !{ALK5+OH}
     &      +R(202) !{ARO1+OH}
     &      +R(203) !{ARO2+OH}
     &      +R(204) !{OLE1+OH}
     &      +R(208) !{OLE2+OH}
     &      +R(212) !{ETOH+OH}
     &      +R(213) !{MTBE+OH}
     &      +R(214) !{MBUT+OH}
      PA(nn) = OHwHC
C
C
C...  { Isoprene with other oxidants }
      nn = nn + 1
      ptname(nn)  = 'ISOPwOx'
      PA(nn) = 
     &       + R(190) !ISOP+O3
     &       + R(191) !ISOP+NO3
     &       + R(192) !ISOP+O
C
C...  { Terpene with other oxidants }
      nn = nn + 1
      ptname(nn)  = 'TERPwOx'
      PA(nn) =
     &       + R(194) !TERP+O3
     &       + R(195) !TERP+NO3
     &       + R(196) !TERP+O
C
C
C...  { HOx reacted: reactant OH or HO2 lost }
C
C...  OH reacted
      OH_reacted =
     &                +( 1.000)*r( 21)! 21 OH NO = 1 HONO
     &                +( 1.000)*r( 24)! 24 OH HONO = 1 NO2
     &                +( 1.000)*r( 25)! 25 OH NO2 = 1 HNO3
     &                +( 1.000)*r( 26)! 26 OH NO3 = 1 HO2 1 NO2
     &                +( 1.000)*r( 27)! 27 OH HNO3 = 1 NO3 1 H2O
     &                +( 1.000)*r( 29)! 29 OH CO = 1 HO2
     &                +( 1.000)*r( 30)! 30 OH O3 = 1 HO2
     &                +( 1.000)*r( 35)! 35 HNO4 OH = 1 NO2
     &                +( 1.000)*r( 42)! 42 HO2H OH = 1 HO2
     &                +( 1.000)*r( 43)! OH HO2 =
     &                +( 1.000)*r( 44)! 44 OH SO2 = 1 HO2 1 SULF
     &                +( 1.000)*r( 45)! 45 OH H2 = 1 HO2
     &                +( 1.000)*r(125)! 125 HCHO OH = 1 HO2 1 CO
     &                +( 1.000)*r(130)! 130 CCHO OH = 1 CCO3
     &                +( 1.000)*r(133)! 133 RCHO OH = 0.034 RO2R 0.001 RO2N 0.965 RCO3
     &                +( 1.000)*r(136)! 136 ACET OH = 1 HCHO 1 CCO3 1 R2O2
     &                +( 1.000)*r(138)! 138 MEK OH = 0.37 RO2R 0.042 RO2N 0.616 R2O2
     &                +( 1.000)*r(140)! 140 MEOH OH = 1 HCHO 1 HO2
     &                +( 1.000)*r(141)! 141 COOH OH = 0.35 HCHO 0.35 OH 0.65 CXO2
     &                +( 1.000)*r(143)! 143 ROOH OH = 1 RCHO 0.34 RO2R 0.66 OH
     &                +( 1.000)*r(147)! 147 GLY OH = 0.63 HO2 1.26 CO 0.37 RCO3
     &                +( 1.000)*r(150)! 150 MGLY OH = 1 CO 1 CCO3
     &                +( 1.000)*r(153)! 153 PHEN OH = 0.24 BZO 0.76 RO2R 0.23 GLY
     &                +( 1.000)*r(155)! 155 CRES OH = 0.24 BZO 0.76 RO2R 0.23 MGLY
     &                +( 1.000)*r(158)! 158 BALD OH = 1 BZCO
     &                +( 1.000)*r(161)! 161 METH OH = 0.5 RO2R 0.416 CO 0.084 HCHO
     &                +( 1.000)*r(166)! 166 MVK OH = 0.3 RO2R 0.025 RO2N 0.675 R2O2
     &                +( 1.000)*r(170)! ISPD OH = 0.67 RO2R 0.041 RO2N 0.289 MCO3
     &                +( 1.000)*r(174)! 174 PROD OH = 0.379 HO2 0.473 RO2R 0.07 RO2N
     &                +( 1.000)*r(176)! 176 RNO3 OH = 0.338 NO2 0.113 HO2 0.376 RO2R
     &                +( 1.000)*r(178)! 178 DCB1 OH = 1 RCHO 1 RO2R 1 CO
     &                +( 1.000)*r(180)! 180 DCB2 OH = 1 R2O2 1 RCHO 1 CCO3
     &                +( 1.000)*r(182)! 182 DCB3 OH = 1 R2O2 1 RCHO 1 CCO3
     &                +( 1.000)*r(184)! 184 CH4 OH = 1 CXO2
     &                +( 1.000)*r(185)! 185 ETHE OH = 1 RO2R 1.61 HCHO 0.195 CCHO
     &                +( 1.000)*r(189)! 189 ISOP OH = 0.907 RO2R 0.093 RO2N 0.079 R2O2
     &                +( 1.000)*r(193)! 193 TERP OH = 0.75 RO2R 0.25 RO2N 0.5 R2O2
     &                +( 1.000)*r(197)! 197 ALK1 OH = 1 RO2R 1 CCHO
     &                +( 1.000)*r(198)! 198 ALK2 OH = 0.246 OH 0.121 HO2 0.612 RO2R
     &                +( 1.000)*r(199)! 199 ALK3 OH = 0.695 RO2R 0.07 RO2N 0.559 R2O2
     &                +( 1.000)*r(200)! 200 ALK4 OH = 0.835 RO2R 0.143 RO2N 0.936 R2O2
     &                +( 1.000)*r(201)! 201 ALK5 OH = 0.653 RO2R 0.347 RO2N 0.948 R2O2
     &                +( 1.000)*r(202)! 202 ARO1 OH = 0.224 HO2 0.765 RO2R 0.011 RO2N
     &                +( 1.000)*r(203)! 203 ARO2 OH = 0.187 HO2 0.804 RO2R 0.009 RO2N
     &                +( 1.000)*r(204)! 204 OLE1 OH = 0.91 RO2R 0.09 RO2N 0.205 R2O2
     &                +( 1.000)*r(208)! 208 OLE2 OH = 0.918 RO2R 0.082 RO2N 0.001 R2O2
     &                +( 1.000)*r(212)! 212 ETOH OH
     &                +( 1.000)*r(213)! 213 MTBE OH
     &                +( 1.000)*r(214)! 214 MBUT OH
C
C...  HO2 reacted
      HO2_reacted = 
     &                +( 1.000)*r( 31)! 31 HO2 NO = 1 OH 1 NO2
     &                +( 1.000)*r( 32)! 32 HO2 NO2 = 1 HNO4
     &                +( 1.000)*r( 36)! 36 HO2 O3 = 1 OH
     &                +( 2.000)*r( 37)! 37 HO2 HO2 = 1 HO2H 1 O2
     &                +( 2.000)*r( 38)! 38 HO2 HO2 H2O = 1 HO2H 1 O2 1 H2O
     &                +( 1.000)*r( 39)! 39 NO3 HO2 = 0.8 OH 0.8 NO2 0.2 HNO3
     &                +( 1.000)*r( 43)! 43 OH HO2 =
     &                +( 1.000)*r( 47)! 47 CXO2 HO2 = 1 COOH 1 O2
     &                +( 1.000)*r( 52)! 52 RO2R HO2 = 1 ROOH
     &                +( 1.000)*r( 57)! 57 R2O2 HO2 = 1 HO2
     &                +( 1.000)*r( 63)! 63 RO2N HO2 = 1 ROOH
     &                +( 1.000)*r( 72)! 72 CCO3 HO2 = 0.75 CO3H 0.25 CO2H 0.25 O3
     &                +( 1.000)*r( 82)! 82 RCO3 HO2 = 0.75 RC3H 0.25 RC2H 0.25 O3
     &                +( 1.000)*r( 93)! 93 BZCO HO2 = 0.75 RC3H 0.25 RC2H 0.25 O3
     &                +( 1.000)*r(105)! 105 MCO3 HO2 = 0.75 RC3H 0.25 RC2H 0.25 O3
     &                +( 1.000)*r(118)! 118 BZO HO2 = 1 PHEN
     &                +( 1.000)*r(121)! 121 BZNO HO2 = 1 NPHE
     &                + cyc_HCO3_p    ! 126 HCHO HO2 = 1 HCO3
C
      HOx_reacted = HO2_reacted + OH_reacted
C
C
      nn = nn + 1
      ptname(nn)  = 'OH_rctd'
      PA(nn) = OH_reacted
C
C
      nn = nn + 1
      ptname(nn)  = 'HO2_rctd'
      PA(nn) = HO2_reacted
C
C
      nn = nn + 1
      ptname(nn)  = 'HOx_rctd'
      PA(nn) = HOx_reacted
C
C
cC  {other OH propagation rxns }
cC
cC
cC... { other OH prop rxns }
c      nn =  nn + 1
c      ptname(nn)  = 'OHpropmisc'
c
c      other_OH_prop = 
c     &      +R( 26) !{OH+NO3}
c     &      +R( 30) !{OH+O3}  ! treat this as a cycle? ...gst
c     &      +R( 42) !{HO2H+OH}
c     &      +R( 44) !{OH+SO2}
c     &      +R( 45) !{OH+H2}
c      PA(nn) = other_OH_prop
C
C
C...  { RO2 reacted: reaction consuming CXO2, RO2R, RO2N, CCO3, 
C     RCO3, MCO3 or BZCO as reactant }
C
      nn = nn + 1
      ptname(nn)  = 'RO2_rctd'
C...  CXO2 reacted
      PA(nn) = 
     &                +( 1.000)*r( 46)+( 1.000)*r( 47)+( 1.000)*r( 48)
     &                +( 2.000)*r( 49)+( 2.000)*r( 50)+( 1.000)*r( 54)
     &                +( 1.000)*r( 59)+( 1.000)*r( 64)+( 1.000)*r( 74)
     &                +( 1.000)*r( 84)+( 1.000)*r( 95)+( 1.000)*r(107)
C...  RO2R reacted
      PA(nn) = PA(nn)
     &                +( 1.000)*r( 51)+( 1.000)*r( 52)+( 1.000)*r( 53)
     &                +( 1.000)*r( 54)+( 2.000)*r( 55)+( 1.000)*r( 60)
     &                +( 1.000)*r( 66)+( 1.000)*r( 75)+( 1.000)*r( 85)
     &                +( 1.000)*r( 96)+( 1.000)*r(108)
C...  RO2N reacted
      PA(nn) = PA(nn)
     &                +( 1.000)*r( 62)+( 1.000)*r( 63)+( 1.000)*r( 64)
     &                +( 1.000)*r( 65)+( 1.000)*r( 66)+( 1.000)*r( 67)
     &                +( 2.000)*r( 68)+( 1.000)*r( 77)+( 1.000)*r( 87)
     &                +( 1.000)*r( 98)+( 1.000)*r(110)
C...  CCO3 reacted
      PA(nn) = PA(nn)
     &                +( 1.000)*r( 69)+( 1.000)*r( 71)+( 1.000)*r( 72)
     &                +( 1.000)*r( 73)+( 1.000)*r( 74)+( 1.000)*r( 75)
     &                +( 1.000)*r( 76)+( 1.000)*r( 77)+( 2.000)*r( 78)
     &                +( 1.000)*r( 88)+( 1.000)*r( 99)+( 1.000)*r(111)
C...  RCO3 reacted
      PA(nn) = PA(nn)
     &                +( 1.000)*r( 79)+( 1.000)*r( 81)+( 1.000)*r( 82)
     &                +( 1.000)*r( 83)+( 1.000)*r( 84)+( 1.000)*r( 85)
     &                +( 1.000)*r( 86)+( 1.000)*r( 87)+( 1.000)*r( 88)
     &                +( 2.000)*r( 89)+( 1.000)*r(100)+( 1.000)*r(112)
C...  MCO3 reacted
      PA(nn) = PA(nn)
     &                +( 1.000)*r(102)+( 1.000)*r(104)+( 1.000)*r(105)
     &                +( 1.000)*r(106)+( 1.000)*r(107)+( 1.000)*r(108)
     &                +( 1.000)*r(109)+( 1.000)*r(110)+( 1.000)*r(111)
     &                +( 1.000)*r(112)+( 1.000)*r(113)+( 2.000)*r(114)
C...  BZCO reacted
      PA(nn) = PA(nn)
     &                +( 1.000)*r( 90)+( 1.000)*r( 92)+( 1.000)*r( 93)
     &                +( 1.000)*r( 94)+( 1.000)*r( 95)+( 1.000)*r( 96)
     &                +( 1.000)*r( 97)+( 1.000)*r( 98)+( 1.000)*r( 99)
     &                +( 1.000)*r(100)+( 2.000)*r(101)+( 1.000)*r(113)
C
C
C...  { OH from HO2 }
C     Here we add up all rxns that convert HO2 to OH
      nn = nn + 1
      ptname(nn)  = 'OHfromHO2'
      OH_from_HO2 =
     &       +      R(31)      !HO2+NO
     &       +      R(36)      !HO2+O3
     &       + 0.80*R(39)      !HO2+NO3
      PA(nn) = OH_from_HO2
C
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
C     CXO2 or RO2R/RO2N
C     Do not count HO2 + HCHO
C
C...  OH terminated
      OH_term =
     &                + cyc_HONO_p    ! 21 OH NO = 1 HONO
     &                +( 1.000)*r( 24)! 24 OH HONO = 1 NO2
     &                +( 1.000)*r( 25)! 25 OH NO2 = 1 HNO3
     &                +( 1.000)*r( 27)! 27 OH HNO3 = 1 NO3 1 H2O
     &                +( 1.000)*r( 35)! 35 HNO4 OH = 1 NO2
     &                +( 1.000)*r( 43)! 43 OH HO2 =
     &                +( 1.000)*r(130)! 130 CCHO OH = 1 CCO3
     &                +( 1.000)*r(136)! 136 ACET OH = 1 HCHO 1 CCO3 1 R2O2
     &                +( 1.000)*r(150)! 150 MGLY OH = 1 CO 1 CCO3
     &                +( 1.000)*r(158)! 158 BALD OH = 1 BZCO
     &                +( 1.000)*r(180)! 182 DCB2 OH = 1 R2O2 1 RCHO 1 CCO3
     &                +( 1.000)*r(182)! 182 DCB3 OH = 1 R2O2 1 RCHO 1 CCO3
C
C...  HO2 terminated
      HO2_term =  
     &                + cyc_HNO4_p    ! 32 HO2 NO2 = 1 HNO4
     &                + cyc_H2O2_p    ! 37 HO2 HO2 and 38 HO2 HO2 H2O  
     &                +( 1.000)*r( 43)! 43 OH HO2 =
     &                + cyc_ROOH_p    ! 47 CXO2 HO2 and 52 RO2R HO2  and 63 RO2N HO2 
     &                +( 1.000)*r( 72)! 72 CCO3 HO2 = 0.75 CO3H 0.25 CO2H 0.25 O3
     &                +( 1.000)*r( 82)! 82 RCO3 HO2 = 0.75 RC3H 0.25 RC2H 0.25 O3
     &                +( 1.000)*r( 93)! 93 BZCO HO2 = 0.75 RC3H 0.25 RC2H 0.25 O3
     &                +( 1.000)*r(105)! 105 MCO3 HO2 = 0.75 RC3H 0.25 RC2H 0.25 O3
     &                +( 1.000)*r(118)! 118 BZO HO2 = 1 PHEN
     &                +( 1.000)*r(121)! 121 BZNO HO2 = 1 NPHE
C
C
      nn = nn + 1
      ptname(nn)  = 'OH_term'
      PA(nn) = OH_term
C
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
C
C...  { RO2 termination rxns }
C
      nn =  nn + 1
      ptname(nn)  = 'RO2_term'
C...  CXO2
      PA(nn) = 
     &      + 1.000*R( 47) !{CXO2+HO2}
     &      + 2.000*R( 49) !{CXO2+CXO2}
     &      + 2.000*R( 50) !{CXO2+CXO2}
     &      + 1.000*R( 54) !{RO2R+CXO2}
     &      + 1.000*R( 64) !{RO2N+CXO2}
     &      + 1.000*R( 74) !{CCO3+CXO2}
     &      + 1.000*R( 84) !{RCO3+CXO2}
     &      + 1.000*R( 95) !{BZCO+CXO2}
     &      + 1.000*R(107) !{MCO3+CXO2}
C... RO2R
      PA(nn) = PA(nn)
     &      + 1.000*R( 52) !{RO2R+HO2}
     &      + 1.000*R( 54) !{RO2R+CXO2}
     &      + 2.000*R( 55) !{RO2R+RO2R}
     &      + 1.000*R( 66) !{RO2N+RO2R}
     &      + 1.000*R( 75) !{CCO3+RO2R}
     &      + 1.000*R( 85) !{RCO3+RO2R}
     &      + 1.000*R( 96) !{BZCO+RO2R}
     &      + 1.000*R(108) !{MCO3+RO2R}
C... RO2N
      PA(nn) = PA(nn)
     &      + 1.000*R( 63) !{RO2N+HO2}
     &      + 1.000*R( 64) !{RO2N+CXO2}
     &      + 1.000*R( 66) !{RO2N+RO2R}
     &      + 2.000*R( 68) !{RO2N+RO2N}
     &      + 1.000*R( 77) !{CCO3+RO2N}
     &      + 1.000*R( 87) !{RCO3+RO2N}
     &      + 1.000*R( 98) !{BZCO+RO2N}
     &      + 1.000*R(110) !{MCO3+RO2N}
C..  RCO3
      PA(nn) = PA(nn)
     &      + cyc_PAN2_p
     &      + 1.000*R( 82) !{RCO3+HO2}
     &      + 1.000*R( 84) !{RCO3+CXO2}
     &      + 1.000*R( 85) !{RCO3+RO2R}
     &      + 1.000*R( 87) !{RCO3+RO2N}
     &      + 1.000*R( 88) !{RCO3+CCO3}
     &      + 2.000*R( 89) !{RCO3+RCO3}
     &      + 1.000*R(100) !{BZCO+RCO3}
     &      + 1.000*R(112) !{MCO3+RCO3}
C..  CCO3
      PA(nn) = PA(nn)
     &      + cyc_PAN_p
     &      + 1.000*R( 72) !{CCO3+HO2}
     &      + 1.000*R( 74) !{CCO3+CXO2}
     &      + 1.000*R( 75) !{CCO3+RO2R}
     &      + 1.000*R( 77) !{CCO3+RO2N}
     &      + 2.000*R( 78) !{CCO3+CCO3}
     &      + 1.000*R( 88) !{RCO3+CCO3}
     &      + 1.000*R( 99) !{BZCO+CCO3}
C..  BZCO
      PA(nn) = PA(nn)
     &      + cyc_PBZN_p
     &      + 1.000*R( 93) !{BZCO+HO2}
     &      + 1.000*R( 95) !{BZCO+CXO2}
     &      + 1.000*R( 96) !{BZCO+RO2R}
     &      + 1.000*R( 98) !{BZCO+RO2N}
     &      + 1.000*R( 99) !{BZCO+CCO3}
     &      + 1.000*R(100) !{BZCO+RCO3}
     &      + 2.000*R(101) !{BZCO+BZCO}
     &      + 1.000*R(113) !{MCO3+BZCO}
C..  MCO3
      PA(nn) = PA(nn)
     &      + cyc_MPAN_p
     &      + 1.000*R(105) !{MCO3+HO2}
     &      + 1.000*R(107) !{MCO3+CXO2}
     &      + 1.000*R(108) !{MCO3+RO2R}
     &      + 1.000*R(110) !{MCO3+RO2N}
     &      + 1.000*R(111) !{MCO3+CCO3}
     &      + 1.000*R(112) !{MCO3+RCO3}
     &      + 1.000*R(113) !{MCO3+BZCO}
     &      + 2.000*R(114) !{MCO3+MCO3}
C..  TBUO
      PA(nn) = PA(nn)
     &      + 1.000*R(115) !{TBUO+NO2}
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
C            {  F o r m a l d e h y d e     C h e m i s t r y      }
C
C
C     Only first generation HCHO is counted
C
C
C...  { HCHO Production from ethene }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_ethe'
      hcho_from_ethe =
     &       + 1.610*r(185) ! 185 ETHE OH = RO2R 1.61 HCHO 0.195 CCHO
     &       + 1.000*r(186) ! 186 ETHE O3 = 0.12 OH 0.12 HO2 0.5 CO HCHO 0.37 HC2H
     &       + 0.191*r(188) ! 188 ETHE O = 0.5 HO2 0.2 RO2R 0.3 CXO2 0.191 HCHO
      PA(nn) = hcho_from_ethe
C
C
C... { HCHO Production from OLE1 }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_ole1'
      hcho_from_ole1 =
     &       + 0.732*r(204) ! 204 OLE1 OH = 0.91 RO2R 0.09 RO2N 0.732 HCHO
     &       + 0.500*r(205) ! 205 OLE1 O3 = 0.155 OH 0.056 HO2 0.5 HCHO
      PA(nn) = hcho_from_ole1
C
C
C... { HCHO Production from OLE2 }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_ole2'
      hcho_from_ole2 =
     &       + 0.244*r(208) ! 208 OLE2 OH = 0.918 RO2R 0.244 HCHO
     &       + 0.269*r(209) ! 209 OLE2 O3 = 0.378 OH 0.269 HCHO
     &       + 0.079*r(210) ! 210 OLE2 NO3 = 0.391 NO2 0.079 HCHO
      PA(nn) = hcho_from_ole2
C
C
C... { HCHO Production from terpene }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_terp'
      hcho_from_terp =
     &       + 0.276*r(193) ! 193 TERP OH = 0.75 RO2R 0.25 RO2N 0.276 HCHO
     &       + 0.235*r(194) ! 194 TERP O3 = 0.567 OH 0.033 HO2 0.235 HCHO
      PA(nn) = hcho_from_terp
C
C
C...  { HCHO Production from isoprene }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_isop'
      hcho_from_isop =
     &       + 0.624*R(189) ! 189 ISOP OH = 0.907 RO2R 0.093 RO2N 0.624 HCHO
     &       + 0.592*R(190) ! 190 ISOP O3 = 0.266 OH 0.066 RO2R 0.592 HCHO
     &       + 0.240*R(192) ! 192 ISOP O = 0.01 RO2N 0.24 R2O2 0.24 HCHO
      PA(nn) = hcho_from_isop
C
C
C...  { HCHO Production from isoprene products}
      nn = nn + 1
      ptname(nn)  = 'HCHOp_ispd'
      hcho_from_ispd =
     &       + 0.055*R(170) ! 170 ISPD OH = 0.67 RO2R 0.041 RO2N 0.055 HCHO
     &       + 0.125*R(171) ! 171 ISPD O3 = 0.4 HO2 0.048 RO2R 0.125 HCHO
     &       + 0.227*R(172) ! 172 ISPD NO3 = 0.799 RO2R 0.051 RO2N 0.227 HCHO
     &       + 0.300*R(173) ! 173 ISPD = 1.233 HO2 0.467 CCO3 0.3 HCHO
     &       + 0.084*R(161) ! 161 METH OH 
     &       + 0.200*R(162) ! 162 METH O3
     &       + 0.670*R(165) ! 165 METH
     &       + 0.300*R(166) ! 166 MVK OH
     &       + 0.100*R(167) ! 167 MVK O3
      PA(nn) = hcho_from_ispd
C
C
C... { Total HCHO Production }
      nn =  nn + 1
      ptname(nn)  = 'HCHOp_Tot'
      PA(nn) =
     &      + hcho_from_ethe
     &      + hcho_from_ole1
     &      + hcho_from_ole2
     &      + hcho_from_terp
     &      + hcho_from_isop
     &      + hcho_from_ispd
     &      + 1.000*R( 46) !{CXO2+NO}
     &      + 1.000*R( 48) !{CXO2+NO3}
     &      + 1.000*R( 49) !{CXO2+CXO2}
     &      + 2.000*R( 50) !{CXO2+CXO2}
     &      + 0.750*R( 54) !{RO2R+CXO2}
     &      + 0.750*R( 64) !{RO2N+CXO2}
     &      + 1.000*R( 74) !{CCO3+CXO2}
     &      + 1.000*R( 84) !{RCO3+CXO2}
     &      + 1.000*R( 95) !{BZCO+CXO2}
     &      + 1.000*R(104) !{MCO3+NO}
     &      + 1.000*R(106) !{MCO3+NO3}
     &      + 1.000*R(107) !{MCO3+CXO2}
     &      + 1.000*R(111) !{MCO3+CCO3}
     &      + 1.000*R(112) !{MCO3+RCO3}
     &      + 1.000*R(113) !{MCO3+BZCO}
     &      + 2.000*R(114) !{MCO3+MCO3}
     &      + 1.000*R(127) !{HCO3}
     &      + 1.000*R(136) !{ACET+OH}
     &      + 0.115*R(138) !{MEK+OH}
     &      + 1.000*R(140) !{MEOH+OH}
     &      + 0.350*R(141) !{COOH+OH}
     &      + 1.000*R(142) !{COOH}
     &      + 1.000*R(146) !{GLY}
     &      + 0.213*R(174) !{PROD+OH}
     &      + 0.506*R(175) !{PROD}
     &      + 0.010*R(176) !{RNO3+OH}
     &      + 0.134*R(177) !{RNO3}
     &      + 0.039*R(198) !{ALK2+OH}
     &      + 0.026*R(199) !{ALK3+OH}
     &      + 0.024*R(200) !{ALK4+OH}
     &      + 0.026*R(201) !{ALK5+OH}
     &      + 0.081*R(212) !{ETOH+OH}
     &      + 0.234*R(213) !{MTBE+OH}
     &      + 0.311*R(214) !{MBUT+OH}
     &      + 0.300*R(215) !{MBUT+O3}
C
C
C-----------------End of Formaldehyde Chemistry-------------------------
C
C
C            {     N O y     C h e m i s t r y      }
C
C
C... { HNO3 production from OH+NO2}
      nn =  nn + 1
      ptname(nn)  = 'HNO3_OHNO2'
      PA(nn) =  1.000*R( 25) !{OH+NO2}
C
C
C...  { HNO3 production from NO3 }
      nn = nn + 1
      ptname(nn)  = 'HNO3_NO3HC'
      PA(nn)  = 0.200*R( 39) !{NO3+HO2}
     &        + 1.000*R(129) !{HCHO+NO3}
     &        + 1.000*R(132) !{CCHO+NO3}
     &        + 1.000*R(135) !{RCHO+NO3}
     &        + 1.000*R(148) !{GLY+NO3}
     &        + 1.000*R(151) !{MGLY+NO3}
     &        + 1.000*R(154) !{PHEN+NO3}
     &        + 1.000*R(156) !{CRES+NO3}
     &        + 1.000*R(157) !{NPHE+NO3}
     &        + 1.000*R(160) !{BALD+NO3}
     &        + 0.500*R(163) !{METH+NO3}
     &        + 0.150*R(172) !{ISPD+NO3}
C
C
C...  { Other HNO3 production }
      nn = nn + 1
      ptname(nn)  = 'HNO3_N2O5'
      PA(nn) =  2.000*R( 13) !{N2O5+H2O}
C
C
C...  { Net PAN prod }
      nn = nn + 1
      ptname(nn)  = 'PANSprdNet'
      PA(nn) = cyc_PAN_p + cyc_PAN2_p + cyc_MPAN_p + cyc_PBZN_p
C
C
C...  { Net PAN loss }
      nn = nn + 1
      ptname(nn)  = 'PANSlosNet'
      PA(nn) = cyc_PAN_l + cyc_PAN2_l + cyc_MPAN_l + cyc_PBZN_l
C
C
C...  { Organic Nitrate Production }
      nn =  nn + 1
      ptname(nn)  = 'RNO3_prod'
      PA(nn) = 
     &      + 1.000*R( 62) !{RO2N+NO}
     &      + 1.000*R(115) !{TBUO+NO2}
     &      + 0.572*R(172) !{ISPD+NO3}
     &      + 0.813*R(191) !{ISOP+NO3}
     &      + 0.276*R(195) !{TERP+NO3}
     &      + 0.511*R(206) !{OLE1+NO3}
     &      + 0.321*R(210) !{OLE2+NO3}
c     &      +  0.310*R(176)  !{176} RNO3+OH=0.310*RNO3 (recursive)
     &      + 1.000*R(216) !{MBUT+NO3}
cC
cC...  XN prod
c     combine XN with RNO3 to simplify
c      nn = nn + 1
c      ptname(nn)  = 'XNprod'
c      PA(nn) =     
     &      + 2.000*R(120) !{120} BZNO+NO2
     &      + 0.500*R(163) !{{163} METH+NO3
     &      + 0.278*R(172) !{172} ISPD+NO3=0.278*XN
     &      + 1.000*R(187) !{187} ETHE+NO3
     &      + 0.250*R(195) !{195} TERP+NO3
     &      + 0.489*R(206) !{206} OLE1+NO3
     &      + 0.288*R(210) !{210} OLE2+NO3C
c    &      + 0.352*R(176) !{176} RNO3+OH  (recursive)
C
C
C...  { NOz recycled to NOx } 
C     potentially important NOx recycling are
C     OH reaction with nitric acid
C     Nitric acid photolysis
C     Organic nitrate photolysis
C     OH reaction with organic nitrates
      nn = nn + 1
      ptname(nn)  = 'NOxrecycl'
      PA(nn)  =  1.000*R( 27)  !{ 27} OH+HNO3=NO3+H2O
     &        +  1.000*R( 28)  !{ 28} HNO3=OH+NO2
     &        +  0.338*R(176)  !{176} RNO3+OH=0.338*NO2+0.310*RNO3+0.352*XN
     &        +  1.000*R(177)  !{177} RNO3=NO2+0.341*HO2+0.564*RO2R+0.095*RO2N
C
C
C...  { NO to NO2 by HO2 } 
      nn = nn + 1
      ptname(nn)  = 'NOw_HO2'
      PA(nn)  =  1.000*R( 31)  ! 31 HO2 NO = 1 OH 1 NO2
C
C
C...  { NO to NO2 by RO2s } 
      nn = nn + 1
      ptname(nn)  = 'NOw_RO2s'
      PA(nn)  =  1.000*R( 46)  !  46 CXO2 NO = 1 NO2 1 HCHO 1 HO2
     &        +  1.000*R( 51)  !  51 RO2R NO = 1 NO2 1 HO2
     &        +  1.000*R( 56)  !  56 R2O2 NO = 1 NO2
     &        +  1.000*R(128)  ! 128 HCO3 NO = 1 HC2H 1 NO2 1 HO2
C
C
C...  { NO to NO2 by RCO3s }
      nn = nn + 1
      ptname(nn)  = 'NOw_RCO3s'
      PA(nn)  =  1.000*R( 71)  !  71 CCO3 NO = 1 CXO2 1 NO2
     &        +  1.000*R( 81)  !  81 RCO3 NO = 1 NO2 1 CCHO 1 RO2R
     &        +  1.000*R( 92)  !  92 BZCO NO = 1 NO2 1 BZO 1 R2O2
     &        +  1.000*R(104)  ! 104 MCO3 NO = 1 NO2 1 HCHO 1 CCO3
C
C-----------------End of NOy Chemistry----------------------------------
C
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
      PA(nn) =      rk(18)*dtfact
C
C...  Ckeck that MXCPA is set large enough
C
      If( nn .GT. MXCPA ) Then
         write(iout,'(//,a)') 'ERROR in CPAMECH5'
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
