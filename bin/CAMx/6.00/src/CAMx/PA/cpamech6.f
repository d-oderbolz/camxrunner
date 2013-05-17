      subroutine cpamech6( r, rk, dtfact, nr, pa, npa, npa_init, ldark )
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
C    *** NOTE:  This code is hardwired for the CAMx CB05 mechanism 6 ***
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
C**   01     08/07  PP|GY  Created
C**   02 04/20/2012 BK     Added TERPwOx
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
      REAL   OH_new, HO2_new, HOx_new, RO2_new
      REAL   OH_reacted, HO2_reacted, HOx_reacted
      REAL   OH_term, HO2_term, HOx_term, RO2_term
      REAL   OHwHC
      REAL   other_OH_prop
      REAL   hcho_from_eth, hcho_from_ole, hcho_from_iole
      REAL   hcho_from_terp, hcho_from_open
      REAL   hcho_from_isop, hcho_from_ispd
      REAL   POx_net, ratio_ind, total_ho2_prod 
      REAL   prod_h2o2, prod_hno3, o3_prod, o3_loss 
      REAL   Y_OH_per_HO2, OH_from_HO2
      REAL   ho2_frac_meo2, meo2_frac_c2o3, c2o3_frac_cxo3, ho2tmp
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
C     OHw_PAR
C     OHw_TOL
C     OHw_XYL
C     OHw_ETH
C     OHw_ETHA
C     OHw_OLE
C     OHw_IOLE
C     OHw_ISOP
C     OHw_TERP
C     OHw_all_HC
C     ISOPwOx
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
C     HCHOp_open
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
C     Ox =O3, NO2, NO3*2, O3P, O1D, HNO3, PAN, PANX, PNA, N2O5*3, NTR
      nn =  1
      ptname(nn)  = 'OxProd'
      PA(nn) =  2.0*R(22)  ! NO+NO+O2=2*NO2
     &       +      R(26)  ! OH+HONO=NO2
     &       +      R(27)  ! HONO+HONO=NO+NO2
     &       +      R(29)  ! OH+HNO3=NO3
     &       +      R(30)  ! HO2+NO=OH+NO2
     &       +      R(41)  ! OH+OH=O
     &       + 0.39*R(51)  ! PNA=0.61*NO2+0.39*NO3
     &       +      R(54)  ! XO2+NO=NO2
     &       +      R(55)  ! XO2N+NO=NTR
     &       +      R(68)  ! MEO2+NO=FORM+HO2+NO2
     &       +      R(81)  ! HCO3+NO=FACD+HO2+NO2
     &       +      R(88)  ! C2O3+NO=MEO2+NO2
     &       +  0.2*R(92)  ! C2O3+HO2=0.2*O3
     &       +      R(103) ! CXO3+NO=NO2
     &       +  0.2*R(108) ! CXO3+HO2=0.2*O3
     &       +      R(132) ! TO2+NO=0.9*NO2+0.1*NTR
C
C
      nn = nn + 1
      ptname(nn)  = 'OxLoss'
      PA(nn) =  2.0*R(4)   ! O3P+NO2=NO
     &       +      R(11)  ! O1D+H2O=2*OH
     &       +      R(12)  ! O3+OH=HO2
     &       +      R(13)  ! O3+HO2=OH
     &       +  2.0*R(15)  ! NO3=NO
     &       +  2.0*R(17)  ! NO3+NO2=NO+NO2
     &       +      R(19)  ! N2O5+H2O=2*HNO3
     &       +      R(20)  ! N2O5+H2O+H2O=2*HNO3
     &       +      R(23)  ! NO+NO2+H2O=2*HONO
     &       +      R(38)  ! O1D+H2
     &       +      R(40)  ! OH+O
     &       +      R(44)  ! HO2+O
     &       +      R(45)  ! H2O2+O
     &       +  2.0*R(46)  ! NO3+O=NO2
     &       +      R(47)  ! NO3+OH=NO2
     &       +      R(48)  ! NO3+HO2=HNO3
     &       +  2.0*R(49)  ! NO3+O3=NO2
     &       +  2.0*R(50)  ! NO3+NO3=2*NO2
     &       +      R(77)  ! FORM+O
     &       +      R(78)  ! FORM+NO3=HNO3
     &       +      R(84)  ! ALD2+O
     &       +      R(86)  ! ALD2+NO3=C2O3+HNO3
     &       +      R(99)  ! ALDX+O
     &       +      R(101) ! ALDX+NO3=CXO3+HNO3
     &       +      R(119) ! OLE+O
     &       +      R(121) ! OLE+O3
     &       +      R(122) ! OLE+NO3=NO2
     &       +      R(123) ! ETH+O
     &       +      R(125) ! ETH+O3
     &       +      R(126) ! ETH+NO3=NO2
     &       +      R(127) ! IOLE+O
     &       +  0.5*R(129) ! IOLE+O3=0.5*O
     &       +      R(130) ! IOLE+NO3=NO2
     &       +      R(135) ! CRES+NO3=NTR
     &       +      R(140) ! OPEN+O3
     &       +      R(144) ! ISOP+O
     &       +      R(146) ! ISOP+O3
     &       +      R(147) ! ISOP+NO3=0.8*NTR+0.2*NO2
     &       +  0.2*R(148) ! ISOP+NO2=0.8*NTR+0.2*NO
     &       +      R(150) ! ISPD+O3
     &       +      R(151) ! ISPD+NO3=0.85*NTR+0.15*HNO3
     &       +      R(153) ! TERP+O
     &       +      R(155) ! TERP+O3
     &       +      R(156) ! TERP+NO3=0.53*NTR+0.47*NO2
C
C
C...  calculate ratio of P(H2O2)/P(HNO3) to use as indicator
C     of NOX or VOC sensitivity
      prod_h2o2 =  R(34) + R(35)
      prod_hno3 =  R(28)
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
cC
cC...increment counter for POx_NOx_sens (used above)
c      nn = nn + 1
C
C
C...  { Net O3 Production }
      nn =  nn + 1
      ptname(nn)   = 'PO3_net'
      O3_prod =        R(  2)  ! O3P+O2=O3
     &        + 0.20 * R( 92)  ! C2O3+HO2=0.2*O3
     &        + 0.20 * R(108)  ! CXO3+HO2=0.2*O3
      O3_loss = R(  3)  ! O3+NO
     &        + R(  7)  ! O3+NO2
     &        + R(  8)  ! O3+hv=O3P
     &        + R(  9)  ! O3+hv=O1D
     &        + R( 12)  ! O3+OH
     &        + R( 13)  ! O3+HO2
     &        + R( 49)  ! O3+NO3
     &        + R(121)  ! O3+OLE
     &        + R(125)  ! O3+ETH
     &        + R(129)  ! O3+IOLE
     &        + R(140)  ! O3+OPEN
     &        + R(146)  ! O3+ISOP
     &        + R(150)  ! O3+ISPD
     &        + R(155)  ! O3+TERP
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
C...  O1D + water
      PA(nn) =
     &       - R(11)
C...  HO2 + O3 (assuming no OH is recycled via OH+H2O2)
      PA(nn) =  PA(nn)
     &       - R(13)
C...  OH + O3 (accounting for fraction of HO2 recycled via NO)
C
C     ho2tmp is reaction of HO2 with except with NO
      ho2tmp = R(43)+R(48)+R(56)+R(57)+R(69)+R(79)
     &       + R(82)+R(92)+R(108)+R(137)
     &       + 2.*(R(34)+R(35))
C
      PA(nn) =  PA(nn)
     &       - R(12) * ho2tmp
     &               / ( ho2tmp+R(30)+R(44) )
C...  O(3P) + VOC
      PA(nn) =  PA(nn)
     &       - ( R(77)+R(84)+R(99)+R(119)
     &                            +R(123)+R(127)+R(144)+R(153) )
C...  O3 + VOC
      PA(nn) =  PA(nn)
     &       - ( R(121)+R(125)+R(129)+R(140)+R(146)+R(150)+R(155) )
C
      PA(nn) = amin1( PA(nn), (O3_prod - O3_loss) )
C
C
C--------------------End of Ox & O3-----------------------
C
C
C...  { Radical Reservoirs: HONO, HNO4, H2O2  etc }
C     We will call these termination rxns if there is net production
C     during a time step, or initiation if there is net release of HOx.
C
C  { 25} HONO=OH+NO
C  { 24} NO+OH=HONO
C
      sum =  R(24) - R(25)          ! HONO cycle
      if (sum.GT.0.) then
         cyc_HONO_p = sum
         cyc_HONO_l = 0.0
      else
         cyc_HONO_p =  0.0
         cyc_HONO_l = -sum
      end if
C 
C  { 31} HO2+NO2=HNO4
C  { 32} HNO4=HO2+NO2
C  { 51} HNO4+hv=0.61{HO2+NO2}+0.39{OH+NO3}      
C
      sum =  R(31) - R(32) - 0.61*R(51)  ! HNO4 cycle
      if (sum.GT.0.) then
         cyc_HNO4_p = sum
         cyc_HNO4_l = 0.0
      else
         cyc_HNO4_p =  0.0
         cyc_HNO4_l = -sum
      end if
C
C { 34} HO2+HO2=H2O2
C { 35} HO2+HO2+H2O=H2O2
C { 36} H2O2=2.00*OH
C { 42} OH+OH=H2O2
C { 45} H2O2+O=OH+HO2
      sum =  R(34) + R(35) + R(42) - R(36) - 0.5*R(45)   ! H2O2 cycle
      if (sum.GT.0.) then
         cyc_H2O2_p = 2.*sum
         cyc_H2O2_l = 0.0
      else
         cyc_H2O2_p =  0.0
         cyc_H2O2_l = -2.0*sum
      end if
C
C{ 79} HCHO + HO2 = HCO3
C{ 80} HCO3 = HCHO + HO2
C
      sum =  R(79) - R(80)     ! HCO3 cycle
      if (sum.GT.0.) then
         cyc_HCO3_p = sum
         cyc_HCO3_l = 0.0
      else
         cyc_HCO3_p =  0.0
         cyc_HCO3_l = -sum
      end if
C
C { 69} MEO2 + HO2 = MEPX
C { 71} MEPX + OH = 0.7 MEO2 + 0.3 XO2 + 0.3 HO2
C { 72} MEPX = FORM + HO2 + OH
      sum =  R(69) - R(71) - R(72)   ! MEPX cycle
      if (sum.GT.0.) then
         cyc_MEPX_p = sum
         cyc_MEPX_l = 0.0
      else
         cyc_MEPX_p = 0.0
         cyc_MEPX_l = sum
      end if
C
C { 56} XO2 + HO2 = ROOH
C { 57} XO2N + HO2 = ROOH
C { 64} ROOH + OH = XO2 + 0.5 ALD2 + 0.5 ALDX
C { 65} ROOH = OH + HO2 + 0.5 ALD2 + 0.5 ALDX
      sum =  R(56) + R(57) - R(64) - R(65)   ! ROOH cycle
      if (sum.GT.0.) then
         cyc_ROOH_p = sum
         cyc_ROOH_l = 0.0
      else
         cyc_ROOH_p = 0.0
         cyc_ROOH_l = sum
      end if
C
C  { 89} C2O3+NO2=PAN
C  { 90} PAN=C2O3+NO2
C  { 91} PAN=C2O3+NO2
C
      sum =  R(89) - R(90) - R(91)       ! PAN Cycle
      if (sum.GT.0.) then
         cyc_PAN_p = sum
         cyc_PAN_l = 0.0
      else
         cyc_PAN_p =  0.0
         cyc_PAN_l = -sum
      end if
C
C  {104} CXO3+NO2=PANX
C  {105} PANX=CXO3+NO2
C  {106} PANX=CXO3+NO2
C  {107} PANX+OH=ALD2+NO2
C
      sum =  R(104) - R(105) - R(106)    ! PANX Cycle
      if (sum.GT.0.) then
         cyc_PANX_p = sum
         cyc_PANX_l = 0.0
      else
         cyc_PANX_p =  0.0
         cyc_PANX_l = -sum
      end if
C
C
C------------------End of Radical Reservoirs----------------
C
C
C     {      R a d i c a l    I n i t i a t i o n       }
C
C
C     CB05 has recursive radical production that must be
C     considered when tracking radical initiation, i.e.:
C     MEO2 makes some HO2
C     C2O3 makes some MEO2
C     skip CXO3 makes MEO2 ! 112 CXO3 C2O3 = MEO2 XO2 HO2 ALD2
C
C
C...  Fractional yield of HO2 from MEO2
C      68 MEO2 NO = FORM HO2 NO2
C      69 MEO2 HO2 = MEPX
C      70 MEO2 MEO2 = 1.37 FORM 0.74 HO2 0.63 MEOH
C      93 C2O3 MEO2 = 0.9 MEO2 0.9 HO2 FORM 0.1 AACD
C     109 CXO3 MEO2 = 0.9 ALD2 0.9 XO2 HO2 0.1 AACD 0.1 FORM  !to be reviewed
      ho2_frac_meo2 = ( r(68)+0.74*r(70)+0.9*r(93)+r(109) ) /
     &                   (  r(68)+r(69)+2.0*r(70)+r(93)+r(109) )
C
C...  Fractional yield of MEO2 from C2O3
C      88 C2O3 NO = MEO2 NO2
C      89 C2O3 NO2 = PAN
C      90 PAN = C2O3 NO2
C      91 PAN = C2O3 NO2
C      92 C2O3 HO2 = 0.8 PACD 0.2 AACD 0.2 O3
C      93 C2O3 MEO2 = 0.9 MEO2 0.9 HO2 FORM 0.1 AACD
C      94 C2O3 XO2 = 0.9 MEO2 0.1 AACD
C      95 C2O3 C2O3 = 2 MEO2
C     112 CXO3 C2O3 = MEO2 XO2 HO2 ALD2
      meo2_frac_c2o3 = ( r(88)+0.9*r(93)+0.9*r(94)+2.0*r(95)+r(112) ) /
     &            ( r(88)+r(92)+r(93)+r(94)+2.0*r(95)+r(112)+cyc_PAN_p )
C
C
cgy        nn = nn + 1
cgy        ptname(nn)  = 'HO2frMEO2'
cgy        PA(nn) = ho2_frac_meo2 * (dtfact/ppbfact)
C
cgy        nn = nn + 1
cgy        ptname(nn)  = 'MEO2frC2O3'
cgy        PA(nn) = meo2_frac_c2o3 * (dtfact/ppbfact)
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
     &                +( 2.000)*r( 11) !  11 O1D H2O = 2 OH
     &                +( 1.000)*r( 25) !  25 HONO = NO OH
     &                +( 2.000)*r( 36) !  36 H2O2 = 2 OH
     &                +( 1.000)*r( 38) !  38 O1D H2 = OH HO2
     &                +( 1.000)*r( 45) !  45 H2O2 O = OH HO2
     &                +( 0.390)*r( 51) !  51 PNA = 0.61 HO2 0.61 NO2 0.39 OH
     &                +( 1.000)*r( 52) !  52 HNO3 = OH NO2
     &                +( 1.000)*r( 65) !  65 ROOH = OH HO2 0.5 ALD2 0.5 ALDX
     &                +( 1.000)*r( 72) !  72 MEPX = FORM HO2 OH
     &                +( 1.000)*r( 77) !  77 FORM O = OH HO2 CO
     &                +( 1.000)*r( 84) !  84 ALD2 O = C2O3 OH
     &                +( 1.000)*r( 97) !  97 PACD = MEO2 OH
     &                +( 1.000)*r( 99) !  99 ALDX O = CXO3 OH
     &                +( 0.100)*r(119) ! 119 O OLE = 0.2 ALD2 0.3 HO2 0.1 OH
     &                +( 0.100)*r(121) ! 121 O3 OLE = 0.18 ALD2 2 XO2 0.1 OH
     &                +( 0.300)*r(123) ! 123 O ETH = FORM 1.7 HO2 0.3 OH
     &                +( 0.130)*r(125) ! 125 O3 ETH = FORM 0.13 HO2 0.13 OH
     &                +( 0.500)*r(129) ! 129 IOLE O3 = 0.65 ALD2 0.5 OH
     &                +( 0.080)*r(140) ! 140 OPEN O3 = 0.03 ALDX 0.08 OH
     &                +( 0.266)*r(146) ! 146 O3 ISOP = 0.65 ISPD 0.266 OH
     &                +( 0.268)*r(150) ! 150 O3 ISPD = 0.114 C2O3 0.268 OH
     &                +( 0.570)*r(155) ! 155 TERP O3 = 0.57 OH 0.07 HO2 XO2 
C...  new HO2
      HO2_new =
c in cyc_HNO4_l   &   +( 1.000)*r( 32) !  32 PNA = HO2 NO2
     &                +( 1.000)*r( 38) !  38 O1D H2 = OH HO2
     &                +( 1.000)*r( 45) !  45 H2O2 O = OH HO2
c in cyc_HNO4_l   &   +( 0.610)*r( 51) !  51 PNA = 0.61 HO2 0.61 NO2 0.39 OH
     &                +( 1.000)*r( 62) !  62 NTR = NO2 HO2 0.33 FORM 0.33 ALD2
     &                +( 1.000)*r( 65) !  65 ROOH = OH HO2 0.5 ALD2 0.5 ALDX
     &                +( 1.000)*r( 72) !  72 MEPX = FORM HO2 OH
     &                +( 2.000)*r( 75) !  75 FORM = 2 HO2 CO
     &                +( 1.000)*r( 77) !  77 FORM O = OH HO2 CO
     &                +( 1.000)*r( 78) !  78 FORM NO3 = HNO3 HO2 CO
     &                + cyc_HCO3_l     !  80 HCO3 = FORM HO2; 79 FORM HO2 = HCO3 
     &                +( 1.000)*r( 87) !  87 ALD2 = MEO2 CO HO2
     &                +( 1.000)*r(102) ! 102 ALDX = MEO2 CO HO2
     &                +( 0.300)*r(119) ! 119 O OLE = 0.2 ALD2 0.3 ALDX 0.3 HO2
     &                +( 0.440)*r(121) ! 121 O3 OLE = 0.18 ALD2 0.74 FORM 0.44 HO2
     &                +( 1.700)*r(123) ! 123 O ETH = FORM 1.7 HO2 CO 0.7 XO2 0.3 OH
     &                +( 0.130)*r(125) ! 125 O3 ETH = FORM 0.63 CO 0.13 HO2 0.13 OH
     &                +( 0.100)*r(127) ! 127 IOLE O = 1.24 ALD2 0.66 ALDX 0.1 HO2
     &                +( 0.500)*r(129) ! 129 IOLE O3 = 0.65 ALD2 0.35 ALDX 0.5 HO2
     &                +( 1.000)*r(130) ! 130 IOLE NO3 = 1.18 ALD2 0.64 ALDX HO2
     &                +( 1.000)*r(138) ! 138 OPEN = C2O3 HO2 CO
     &                +( 0.760)*r(140) ! 140 OPEN O3 = 0.03 ALDX 0.62 C2O3 0.76 HO2
     &                +( 1.000)*r(143) ! 143 MGLY = C2O3 HO2 CO
     &                +( 0.250)*r(144) ! 144 O ISOP = 0.75 ISPD 0.5 FORM 0.25 HO2
     &                +( 0.066)*r(146) ! 146 O3 ISOP = 0.65 ISPD 0.6 FORM 0.066 HO2
     &                +( 0.800)*r(147) ! 147 NO3 ISOP = 0.2 ISPD 0.8 NTR 0.8 HO2
     &                +( 0.800)*r(148) ! 148 NO2 ISOP = 0.2 ISPD 0.8 NTR XO2 0.8 HO2
     &                +( 0.154)*r(150) ! 150 O3 ISPD = 0.114 C2O3 0.15 FORM 0.154 HO2
     &                +( 0.925)*r(151) ! 151 NO3 ISPD = 0.357 ALDX 0.925 HO2
     &                +( 1.033)*r(152) ! 152 ISPD = 0.333 CO 0.067 ALD2 1.033 HO2
     &                +( 0.070)*r(155) ! 155 TERP O3 = 0.57 OH 0.07 HO2
     &                +( 0.280)*r(156) ! 156 TERP NO3 = 0.47 NO2 0.28 HO2
C
C...  plus HO2 from decomposition of PNA,PAN,PANX compounds
C     32 PNA = HO2 NO2
C     51 PNA = 0.61 HO2 0.61 NO2 0.39 OH 0.39 NO3
C     90 PAN = C2O3 NO2
C     91 PAN = C2O3 NO2
C    105 PANX = CXO3 NO2
C    106 PANX = CXO3 NO2
C    107 PANX OH = ALD2 NO2
      HO2_new = HO2_new
     &                + cyc_HNO4_l
     &                +(cyc_PAN_l*ho2_frac_meo2*meo2_frac_c2o3)
C
C... new HO2 via MEO2 from ALD2, PACD, ALDX photolysis
C     87 ALD2 = MEO2 CO HO2
C     97 PACD = MEO2 OH
C    102 ALDX = MEO2 CO HO2
      HO2_new = HO2_new
     &                +( r(87)+r(97)+r(102) )*ho2_frac_meo2
C
C... new HO2 via C2O3 from OPEN, MGLY, ISPD photolysis
C    138 OPEN = C2O3 HO2 CO
C    143 MGLY = C2O3 HO2 CO
C    152 ISPD = 0.333 CO 0.067 ALD2 0.9 FORM 1.033 HO2 0.7 XO2 0.967 C2O3
      HO2_new = HO2_new
     &                +( r(138)+r(143)+0.967*r(152) )
     &                     *ho2_frac_meo2*meo2_frac_c2o3
C
C... new HO2 via MEO2 from NO3 reactions
C    none
C
C... new HO2 via C2O3 from NO3 reactions
C     86 ALD2 NO3 = C2O3 HNO3
      HO2_new = HO2_new
     &                +r(86)*ho2_frac_meo2*meo2_frac_c2o3
C
C... new HO2 via MEO2 from O3 reactions
C    none
C
C... new HO2 via C2O3 from O3 reactions
C    140 OPEN O3 = 0.62 C2O3 0.03 XO2 0.08 OH 0.76 HO2
C    150 O3 ISPD = 0.114 C2O3 0.154 HO2 0.268 OH 0.064 XO2
      HO2_new = HO2_new
     &                +( 0.62*r(140)+0.114*r(150) )
     &                     *ho2_frac_meo2*meo2_frac_c2o3
C
C... new HO2 via MEO2 from O atom reactions
C    none
C
C... new HO2 via C2O3 from O atom reactions
C     84 ALD2 O = C2O3 OH
      HO2_new = HO2_new
     &                +r(84)*ho2_frac_meo2*meo2_frac_c2o3
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
C...  { New OH from O1D+H2O and O1D+H2}
      nn = nn + 1
      ptname(nn)  = 'newOH_O1D'
      PA(nn) = 2.*R(11)   !O1D+H2O=2*OH
     &       +    R(38)   !O1D+H2=OH+HO2
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
     &       + ( 0.100)*r(121) ! 121 O3 OLE = 0.18 ALD2 0.74 FORM 0.1 OH
     &       + ( 0.130)*r(125) ! 125 O3 ETH = FORM + 0.63 CO 0.13 OH
     &       + ( 0.500)*r(129) ! 129 IOLE O3 = 0.65 ALD2 0.35 ALDX 0.5 OH
     &       + ( 0.080)*r(140) ! 140 OPEN O3 = 0.03 ALDX 0.62 C2O3 0.08 OH
     &       + ( 0.266)*r(146) ! 146 O3 ISOP = 0.65 ISPD 0.6 FORM 0.266 OH
     &       + ( 0.268)*r(150) ! 150 O3 ISPD = 0.114 C2O3 0.15 FORM 0.268 OH
     &       + ( 0.570)*r(155) ! 155 TERP O3 = 0.57 OH 0.07 HO2 0.76 XO2
C
C
C...  { New HO2 from HCHO }
      nn = nn + 1
      ptname(nn)  = 'nwHO2_HCHO'
      PA(nn) = 2.*R(75) ! 75 FORM = 2 HO2 + CO
     &       +    R(77) ! 77 FORM O = OH HO2 CO
     &       +    R(78) ! 78 FORM NO3 = HNO3 HO2 CO
C
C
C...  { New RO2 Production }
C
C...  Rad initiation for C2O3 
C     nn = nn + 1
C     ptname(nn)  = 'newC2O3'
C...  new C2O3
      RO2_new = cyc_PAN_l
     &       + 1.000*r( 84) !  84 ALD2 O = C2O3 OH
     &       + 1.000*r( 86) !  86 ALD2 NO3 = C2O3 HNO3
     &       + 1.000*r(138) ! 138 OPEN = C2O3 HO2 CO
     &       + 0.620*r(140) ! 140 OPEN O3 = 0.03 ALDX 0.62 C2O3 0.7 FORM
     &       + 1.000*r(143) ! 143 MGLY = C2O3 HO2 CO
     &       + 0.114*r(150) ! 150 O3 ISPD = 0.114 C2O3 0.15 FORM 0.85 MGLY
     &       + 0.967*r(152) ! 152 ISPD = 0.333 CO 0.067 ALD2 0.967 C2O3
C
C
C...  Rad initiation for CXO3
C     nn = nn + 1
C     ptname(nn)  = 'newCXO3'
C...  new CXO3
      RO2_new = RO2_new
     &       + cyc_PANX_l
     &       + 1.000*r( 99) !  99 ALDX O = CXO3 OH
     &       + 1.000*r(101) ! 101 ALDX NO3 = CXO3 HNO3
     &       + 0.250*r(144) ! 144 O ISOP = 0.75 ISPD 0.5 FORM 0.25 CXO3
     &       + 0.200*r(146) ! 146 O3 ISOP = 0.65 ISPD 0.6 FORM 0.2 CXO3
     &       + 0.075*r(151) ! 151 NO3 ISPD = 0.357 ALDX 0.282 FORM 0.075 CXO3
     &       + 0.390*r(155) ! 155 TERP O3 = 0.57 OH 0.07 HO2 0.76 XO2 0 0.39 CXO3
C
C
C...  Rad initiation for MEO2
C     nn = nn + 1
C     ptname(nn)  = 'newMEO2'
C...  new MEO2
      RO2_new = RO2_new
     &       + 1.000*r( 87) !  87 ALD2 = MEO2 CO HO2
     &       + 1.000*r( 97) !  97 PACD = MEO2 OH
     &       + 1.000*r(102) ! 102 ALDX = MEO2 CO HO2
C
C
C...  Rad initiation for XO2
C     nn = nn + 1
C     ptname(nn)  = 'newxO2'
C...  new XO2
      RO2_new = RO2_new
     &       + 0.200*r(119) ! 119 O OLE = 0.2 ALD2 0.3 ALDX 0.3 HO2 0.2 XO2
     &       + 0.220*r(121) ! 121 O3 OLE = 0.18 ALD2 0.74 FORM 0.32 ALDX 0.22 XO2
     &       + 0.910*r(122) ! 122 NO3 OLE = NO2 FORM 0.91 XO2 0.09 XO2N
     &       + 0.700*r(123) ! 123 O ETH = FORM 1.7 HO2 CO 0.7 XO2 0.3 OH
     &       + 1.000*r(126) ! 126 NO3 ETH = NO2 XO2 2 FORM
     &       + 0.100*r(127) ! 127 IOLE O = 1.24 ALD2 0.66 ALDX 0.1 HO2 0.1 XO2
     &       + 0.030*r(140) ! 140 OPEN O3 = 0.03 ALDX 0.62 C2O3 0.7 FORM 0.03 XO2
     &       + 0.250*r(144) ! 144 O ISOP = 0.75 ISPD 0.5 FORM 0.25 XO2
     &       + 0.200*r(146) ! 146 O3 ISOP = 0.65 ISPD 0.6 FORM 0.2 XO2
     &       + 1.000*r(147) ! 147 NO3 ISOP = 0.2 ISPD 0.8 NTR XO2 0.8 HO2
     &       + 1.000*r(148) ! 148 NO2 ISOP = 0.2 ISPD 0.8 NTR XO2 0.8 HO2
     &       + 0.064*r(150) ! 150 O3 ISPD = 0.114 C2O3 0.15 FORM 0.064 XO2
     &       + 0.075*r(151) ! 151 NO3 ISPD = 0.357 ALDX 0.282 FORM 0.075 XO2
     &       + 0.700*r(152) ! 152 ISPD = 0.333 CO 0.067 ALD2 0.7 XO2
     &       + 0.760*r(155) ! 155 TERP O3 = 0.57 OH 0.07 HO2 0.76 XO2
     &       + 1.030*r(156) ! 156 TERP NO3 = 0.47 NO2 0.28 HO2 1.03 XO2
C
C
C...  Rad initiation for XO2N
C     nn = nn + 1
C     ptname(nn)  = 'newxO2N'
C...  new XO2N
      RO2_new = RO2_new
     &       + 0.010*r(119) ! 119 O OLE = 0.2 ALD2 0.3 ALDX 0.2 XO2 0.01 XO2N
     &       + 0.090*r(122) ! 122 NO3 OLE = NO2 FORM 0.91 XO2 0.09 XO2N
     &       + 0.180*r(155) ! 155 TERP O3 = 0.57 OH 0.07 HO2 0.76 XO2 0.18 XO2N
     &       + 0.250*r(156) ! 156 TERP NO3 = 0.47 NO2 0.28 HO2 1.03 XO2 0.25 XO2N
C
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
      PA(nn) = R(66)
C
C...  OH+CH4
      nn = nn + 1
      ptname(nn)  = 'OHw_CH4'
      PA(nn) = R(67)
C
C...  OH+PAR
      nn = nn + 1
      ptname(nn)  = 'OHw_PAR'
      PA(nn) = R(115)
C
C...  OH+TOL
      nn = nn + 1
      ptname(nn)  = 'OHw_TOL'
      PA(nn) = R(131)
C
C...  OH+XYL
      nn = nn + 1
      ptname(nn)  = 'OHw_XYL'
      PA(nn) = R(141)
C
C...  OH+ETH
      nn = nn + 1
      ptname(nn)  = 'OHw_ETH'
      PA(nn) = R(124)
C
C...  OH+ETHA
      nn = nn + 1
      ptname(nn)  = 'OHw_ETHA'
      PA(nn) = R(113)
C
C...  OH+OLE
      nn = nn + 1
      ptname(nn)  = 'OHw_OLE'
      PA(nn) = R(120)
C
C...  OH+IOLE
      nn = nn + 1
      ptname(nn)  = 'OHw_IOLE'
      PA(nn) = R(128)
C
C...  OH+ISOP
      nn = nn + 1
      ptname(nn)  = 'OHw_ISOP'
      PA(nn) = R(145)  !OH+ISOP
C
C...  OH+TERP
      nn = nn + 1
      ptname(nn)  = 'OHw_TERP'
      PA(nn) = R(154)  !OH+TERP
C
C
C...  { All OH rxns with organics }
      nn = nn + 1
      ptname(nn)  = 'OHw_all_HC'
      OHwHC  = R( 66)   !CO
     &       + R( 67)   !CH4
     &       + R( 71)   !MEPX
     &       + R( 73)   !MEOH
     &       + R( 74)   !FORM
     &       + R( 83)   !FACD
     &       + R( 85)   !ALD2
     &       + R( 96)   !PACD
     &       + R( 98)   !AACD
     &       + R(100)   !ALDX
     &       + R(113)   !ETHA
     &       + R(114)   !ETOH
     &       + R(115)   !PAR
     &       + R(120)   !OLE
     &       + R(124)   !ETH
     &       + R(128)   !IOLE
     &       + R(131)   !TOL
     &       + R(134)   !CRES
     &       + R(139)   !OPEN
     &       + R(141)   !XYL
     &       + R(142)   !MGLY
     &       + R(145)   !ISOP
     &       + R(149)   !ISPD
     &       + R(154)   !TERP
      PA(nn) = OHwHC
C
C
C
C...  { Isoprene with other oxidants }
      nn = nn + 1
      ptname(nn)  = 'ISOPwOx'
      PA(nn) = R(144)   !ISOP+O
     &       + R(146)   !ISOP+O3
     &       + R(147)   !ISOP+NO3
     &       + R(148)   !ISOP+NO2
C
C
C...  { Terpenes with other oxidants }
      nn = nn + 1
      ptname(nn)  = 'TERPwOx'
      PA(nn) = R(155)   !TERP+O3
     &       + R(156)   !TERP+NO3
     &       + R(153)   !TERP+O
C
C
C...  { HOx reacted: reaction consuming OH or HO2 as reactant }
C
C...  OH reacted
      OH_reacted =
     &                +( 1.000)*r( 12) !  12 O3 OH = HO2
     &                +( 1.000)*r( 24) !  24 NO OH = HONO
     &                +( 1.000)*r( 26) !  26 OH HONO = NO2
     &                +( 1.000)*r( 28) !  28 NO2 OH = HNO3
     &                +( 1.000)*r( 29) !  29 OH HNO3 = NO3
     &                +( 1.000)*r( 33) !  33 OH PNA = NO2
     &                +( 1.000)*r( 37) !  37 OH H2O2 = HO2
     &                +( 1.000)*r( 39) !  39 OH H2 = HO2
     &                +( 1.000)*r( 40) !  40 OH O = HO2
     &                +( 2.000)*r( 41) !  41 OH OH = O
     &                +( 2.000)*r( 42) !  42 OH OH = H2O2
     &                +( 1.000)*r( 43) !  43 OH HO2 =
     &                +( 1.000)*r( 47) !  47 NO3 OH = HO2 NO2
     &                +( 1.000)*r( 61) !  61 NTR OH = HNO3 HO2 0.33 FORM 0.33 ALD2
     &                +( 1.000)*r( 63) !  63 SO2 OH = SULF HO2
     &                +( 1.000)*r( 64) !  64 ROOH OH = XO2 0.5 ALD2 0.5 ALDX
     &                +( 1.000)*r( 66) !  66 OH CO = HO2
     &                +( 1.000)*r( 67) !  67 OH CH4 = MEO2
     &                +( 1.000)*r( 71) !  71 MEPX OH = 0.7 MEO2 0.3 XO2 0.3 HO2
     &                +( 1.000)*r( 73) !  73 MEOH OH = FORM HO2
     &                +( 1.000)*r( 74) !  74 FORM OH = HO2 CO
     &                +( 1.000)*r( 83) !  83 FACD OH = HO2
     &                +( 1.000)*r( 85) !  85 ALD2 OH = C2O3
     &                +( 1.000)*r( 96) !  96 PACD OH = C2O3
     &                +( 1.000)*r( 98) !  98 AACD OH = MEO2
     &                +( 1.000)*r(100) ! 100 ALDX OH = CXO3
     &                +( 1.000)*r(107) ! 107 PANX OH = ALD2 NO2
     &                +( 1.000)*r(113) ! 113 OH ETHA = 0.991 ALD2 0.991 XO2
     &                +( 1.000)*r(114) ! 114 OH ETOH = HO2 0.9 ALD2 0.05 ALDX
     &                +( 1.000)*r(115) ! 115 PAR OH = 0.87 XO2 0.13 XO2N 0.11 HO2
     &                +( 1.000)*r(120) ! 120 OH OLE = 0.8 FORM 0.33 ALD2 0.62 ALDX
     &                +( 1.000)*r(124) ! 124 OH ETH = XO2 1.56 FORM 0.22 ALDX
     &                +( 1.000)*r(128) ! 128 IOLE OH = 1.3 ALD2 0.7 ALDX HO2 XO2
     &                +( 1.000)*r(131) ! 131 TOL OH = 0.44 HO2 0.08 XO2 0.36 CRES
     &                +( 1.000)*r(134) ! 134 OH CRES = 0.4 CRO 0.6 XO2 0.6 HO2
     &                +( 1.000)*r(139) ! 139 OPEN OH = XO2 2 CO 2 HO2 C2O3 FORM
     &                +( 1.000)*r(141) ! 141 OH XYL = 0.7 HO2 0.5 XO2 0.2 CRES
     &                +( 1.000)*r(142) ! 142 OH MGLY = XO2 C2O3
     &                +( 1.000)*r(145) ! 145 OH ISOP = 0.912 ISPD 0.629 FORM
     &                +( 1.000)*r(149) ! 149 OH ISPD = 1.565 PAR 0.167 FORM 0.713 XO2
     &                +( 1.000)*r(154) ! 154 TERP OH = 0.75 HO2 1.25 XO2 0.25 XO2N
C...  HO2 reacted
      HO2_reacted =
     &                +( 1.000)*r( 13) !  13 O3 HO2 = OH
     &                +( 1.000)*r( 30) !  30 HO2 NO = OH NO2
     &                + cyc_HNO4_p     !  31 HO2 NO2 = PNA
     &                + cyc_H2O2_p     !  34 HO2 HO2 = H2O2; 35 HO2 HO2 H2O = H2O2
     &                +( 2.000)*r( 35) !  35 HO2 HO2 H2O = H2O2
     &                +( 1.000)*r( 43) !  43 OH HO2 =
     &                +( 1.000)*r( 44) !  44 HO2 O = OH
     &                +( 1.000)*r( 48) !  48 NO3 HO2 = HNO3
     &                + cyc_ROOH_p     !  56 XO2 HO2 = ROOH; 57 XO2N HO2 = ROOH
     &                +( 1.000)*r( 57) !  57 XO2N HO2 = ROOH
     &                + cyc_MEPX_p     !  69 MEO2 HO2 = MEPX; 82 HCO3 HO2 = MEPX
     &                + cyc_HCO3_p     !  79 FORM HO2 = HCO3; 80 HCO3 = FORM HO2
     &                +( 1.000)*r( 82) !  82 HCO3 HO2 = MEPX
     &                +( 1.000)*r( 92) !  92 C2O3 HO2 = 0.8 PACD 0.2 AACD 0.2 O3
     &                +( 1.000)*r(108) ! 108 CXO3 HO2 = 0.8 PACD 0.2 AACD 0.2 O3
     &                +( 1.000)*r(137) ! 137 CRO HO2 = CRES
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
c     &      +R( 12) !{OH+O3}
c     &      +R( 37) !{OH+H2OH}
c     &      +R( 39) !{OH+H2}
c     &      +R( 47) !{OH+NO3}
c     &      +R( 63) !{OH+SO2}
c      PA(nn) = other_OH_prop
C
C
C...  { RO2 reacted: reaction consuming C2O3, CXO3, MEO2, TO2, XO2 or 
C     XO2N as reactant }
C
      nn = nn + 1
      ptname(nn)  = 'RO2_rctd'
C...  C2O3 reacted
      PA(nn) = 1.000*r( 88) !  88 C2O3 NO = MEO2 NO2
     &       + cyc_PAN_p    !  89 C2O3 NO2 = PAN
     &       + 1.000*r( 92) !  92 C2O3 HO2 = 0.8 PACD 0.2 AACD 0.2 O3
     &       + 1.000*r( 93) !  93 C2O3 MEO2 = 0.9 MEO2 0.9 HO2 FORM 0.1 AACD
     &       + 1.000*r( 94) !  94 C2O3 XO2 = 0.9 MEO2 0.1 AACD
     &       + 2.000*r( 95) !  95 C2O3 C2O3 = 2 MEO2
     &       + 1.000*r(112) ! 112 CXO3 C2O3 = MEO2 XO2 HO2 ALD2
C...  CXO3 reacted
      PA(nn) = PA(nn)
     &       + 1.000*r(103) ! 103 CXO3 NO = ALD2 NO2 HO2 XO2
     &       + cyc_PANX_p   ! 104 CXO3 NO2 = PANX
     &       + 1.000*r(108) ! 108 CXO3 HO2 = 0.8 PACD 0.2 AACD 0.2 O3
     &       + 1.000*r(109) ! 109 CXO3 MEO2 = 0.9 ALD2 0.9 XO2 HO2 0.1 AACD
     &       + 1.000*r(110) ! 110 CXO3 XO2 = 0.9 ALD2 0.1 AACD
     &       + 2.000*r(111) ! 111 CXO3 CXO3 = 2 ALD2 2 XO2 2 HO2
     &       + 1.000*r(112) ! 112 CXO3 C2O3 = MEO2 XO2 HO2 ALD2
C...  MEO2 reacted
      PA(nn) = PA(nn)
     &       + 1.000*r( 68) !  68 MEO2 NO = FORM HO2 NO2
     &       + 1.000*r( 69) !  69 MEO2 HO2 = MEPX
     &       + 2.000*r( 70) !  70 MEO2 MEO2 = 1.37 FORM 0.74 HO2 0.63 MEOH
     &       + 1.000*r( 93) !  93 C2O3 MEO2 = 0.9 MEO2 0.9 HO2 FORM 0.1 AACD
     &       + 1.000*r(109) ! 109 CXO3 MEO2 = 0.9 ALD2 0.9 XO2 HO2 0.1 AACD 0.1 FORM
C...  TO2 reacted
      PA(nn) = PA(nn)
     &       + 1.000*r(132) ! 132 TO2 NO = 0.9 NO2 0.9 HO2 0.9 OPEN 0.1 NTR
     &       + 1.000*r(133) ! 133 TO2 = CRES HO2
C...  XO2 reacted
      PA(nn) = PA(nn)
     &       + 1.000*r( 54) !  54 XO2 NO = NO2
     &       + 1.000*r( 56) !  56 XO2 HO2 = ROOH
     &       + 2.000*r( 58) !  58 XO2 XO2 =
     &       + 1.000*r( 60) !  60 XO2 XO2N =
     &       + 1.000*r( 94) !  94 C2O3 XO2 = 0.9 MEO2 0.1 AACD
     &       + 1.000*r(110) ! 110 CXO3 XO2 = 0.9 ALD2 0.1 AACD
C...  XO2N reacted
      PA(nn) = PA(nn)
     &       + 1.000*r( 55) !  55 XO2N NO = NTR
     &       + 1.000*r( 57) !  57 XO2N HO2 = ROOH
     &       + 2.000*r( 59) !  59 XO2N XO2N =
     &       + 1.000*r( 60) !  60 XO2 XO2N =
C
C
C...  { OH from HO2 }
C     Here we add up all rxns that convert HO2 to OH
      nn = nn + 1
      ptname(nn)  = 'OHfromHO2'
      OH_from_HO2 =
     &       + 1.000*r( 13) !  13 O3 HO2 = OH
     &       + 1.000*r( 30) !  30 HO2 NO = OH NO2
     &       + 1.000*r( 44) !  44 HO2 O = OH
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
C     C2O3,CXO3,MEO2
C
C...  OH terminated
      OH_term =
     &                + cyc_HONO_p   !  24 NO OH = HONO
     &                + 1.000*r( 26) !  26 OH HONO = NO2
     &                + 1.000*r( 28) !  28 NO2 OH = HNO3
     &                + 1.000*r( 29) !  29 OH HNO3 = NO3
     &                + 1.000*r( 33) !  33 OH PNA = NO2
     &                + 2.000*r( 41) !  41 OH OH = O
     &                + 2.000*r( 42) !  42 OH OH = H2O2
     &                + 1.000*r( 43) !  43 OH HO2 =
     &                + 1.000*r( 64) !  64 ROOH OH = XO2 0.5 ALD2 0.5 ALDX
     &                + 1.000*r(107) ! 107 PANX OH = ALD2 NO2
C
C...  HO2 terminated
      HO2_term =
     &                + cyc_HNO4_p   !  31 HO2 NO2 = PNA
     &                + cyc_H2O2_p   !  34 HO2 HO2 = H2O2; 35 HO2 HO2 H2O = H2O2
     &                + 1.000*r( 43) !  43 OH HO2 =
     &                + 1.000*r( 48) !  48 NO3 HO2 = HNO3
     &                + cyc_ROOH_p   !  56 XO2 HO2 = ROOH; 57 XO2N HO2 = ROOH
     &                + cyc_MEPX_p   !  69 MEO2 HO2 = MEPX; 82 HCO3 HO2 = MEPX
     &                + cyc_HCO3_p   !  79 FORM HO2 = HCO3; 80 HCO3 = FORM HO2
     &                + 1.000*r( 92) !  92 C2O3 HO2 = 0.8 PACD 0.2 AACD 0.2 O3
     &                + 1.000*r(108) ! 108 CXO3 HO2 = 0.8 PACD 0.2 AACD 0.2 O3
     &                + 1.000*r(137) ! 137 CRO HO2 = CRES
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
C     RO2 radicals reacted are C2O3, CXO3, MEO2, TO2, XO2, XO2N
C
      nn = nn + 1
      ptname(nn)  = 'RO2_term'
C...  C2O3
      PA(nn) =
     &      + cyc_PAN_p    !  89 C2O3 NO2 = PAN
     &      + 1.000*r( 92) !  92 C2O3 HO2 = 0.8 PACD 0.2 AACD 0.2 O3
     &      + 1.000*r( 93) !  93 C2O3 MEO2 = 0.9 MEO2 0.9 HO2 FORM 0.1 AACD
     &      + 1.000*r( 94) !  94 C2O3 XO2 = 0.9 MEO2 0.1 AACD
     &      + 1.000*r(112) ! 112 CXO3 C2O3 = MEO2 XO2 HO2 ALD2
C...  CXO3
       PA(nn) = PA(nn)
     &      + cyc_PANX_p   ! 104 CXO3 NO2 = PANX
     &      + 1.000*r(108) ! 108 CXO3 HO2 = 0.8 PACD 0.2 AACD 0.2 O3
     &      + 1.000*r(109) ! 109 CXO3 MEO2 = 0.9 ALD2 0.9 XO2 HO2 0.1 AACD 0.1 FORM
     &      + 1.000*r(110) ! 110 CXO3 XO2 = 0.9 ALD2 0.1 AACD
     &      + 2.000*r(111) ! 111 CXO3 CXO3 = 2 ALD2 2 XO2 2 HO2
     &      + 1.000*r(112) ! 112 CXO3 C2O3 = MEO2 XO2 HO2 ALD2
C...  MEO2
      PA(nn) = PA(nn)
     &      + 1.000*r( 69) !  69 MEO2 HO2 = MEPX
     &      + 2.000*r( 70) !  70 MEO2 MEO2 = 1.37 FORM 0.74 HO2 0.63 MEOH
     &      + 1.000*r(109) ! 109 CXO3 MEO2 = 0.9 ALD2 0.9 XO2 HO2 0.1 AACD 0.1 FORM
C...  XO2
       PA(nn) = PA(nn)
     &       + 1.000*r( 56) !  56 XO2 HO2 = ROOH
     &       + 2.000*r( 58) !  58 XO2 XO2 =
     &       + 1.000*r( 60) !  60 XO2 XO2N =
     &       + 1.000*r( 94) !  94 C2O3 XO2 = 0.9 MEO2 0.1 AACD 
     &       + 1.000*r(110) ! 110 CXO3 XO2 = 0.9 ALD2 0.1 AACD
C...  XO2N
       PA(nn) = PA(nn)
     &       + 1.000*r( 55) !  55 XO2N NO = NTR
     &       + 1.000*r( 57) !  57 XO2N HO2 = ROOH
     &       + 2.000*r( 59) !  59 XO2N XO2N =
     &       + 1.000*r( 60) !  60 XO2 XO2N =
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
     &       + 1.000*r(123) ! 123 O ETH = FORM 1.7 HO2 CO 0.7 XO2 0.3 OH
     &       + 1.560*r(124) ! 124 OH ETH = XO2 1.56 FORM 0.22 ALDX HO2
     &       + 1.000*r(125) ! 125 O3 ETH = FORM 0.63 CO 0.13 HO2 0.13 OH 0.37 FACD
     &       + 2.000*r(126) ! 126 NO3 ETH = NO2 XO2 2 FORM
      PA(nn) = hcho_from_eth
C
C
C... { HCHO Production from OLE }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_ole'
      hcho_from_ole =
     &       + 0.200*r(119) ! 119 O OLE = 0.2 ALD2 0.3 ALDX 0.3 HO2 0.2 FORM
     &       + 0.800*r(120) ! 120 OH OLE = 0.8 FORM 0.33 ALD2 0.62 ALDX 0.8 XO2
     &       + 0.740*r(121) ! 121 O3 OLE = 0.18 ALD2 0.74 FORM 0.32 ALDX 0.22 XO2
     &       + 1.000*r(122) ! 122 NO3 OLE = NO2 FORM 0.91 XO2 0.09 XO2N
      PA(nn) = hcho_from_ole
C
C
C... { HCHO Production from IOLE }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_iole'
      hcho_from_iole =
     &       + 0.250*r(129) ! 129 IOLE O3 = 0.65 ALD2 0.35 ALDX 0.25 FORM 0.25 CO
      PA(nn) = hcho_from_iole
C
C
C... { HCHO Production from OPEN }
cgy     nn = nn + 1
cgy     ptname(nn)  = 'HCHOp_open'
      hcho_from_open =
     &       + 1.000*r(139) ! 139 OPEN OH = XO2 2 CO 2 HO2 C2O3 FORM
     &       + 0.700*r(140) ! 140 OPEN O3 = 0.03 ALDX 0.62 C2O3 0.7 FORM 0.03 XO2
cgy     PA(nn) = hcho_from_open
C
C
C...  { HCHO Production from terpene }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_terp'
      hcho_from_terp =
     &       + 0.280*r(154) ! 154 TERP OH = 0.75 HO2 1.25 XO2 0.25 XO2N 0.28 FORM
     &       + 0.240*r(155) ! 155 TERP O3 = 0.57 OH 0.07 HO2 0.76 XO2 0.24 FORM
      PA(nn) = hcho_from_terp
C
C
C...  { HCHO Production from isoprene }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_isop'
      hcho_from_isop =
     &       + 0.500*r(144) ! 144 O ISOP = 0.75 ISPD 0.5 FORM 0.25 XO2 0.25 HO2
     &       + 0.629*r(145) ! 145 OH ISOP = 0.912 ISPD 0.629 FORM 0.991 XO2
     &       + 0.600*r(146) ! 146 O3 ISOP = 0.65 ISPD 0.6 FORM 0.2 XO2 0.066 HO2
      PA(nn) = hcho_from_isop
C
C
C...  { HCHO Production from isoprene product}
      nn = nn + 1
      ptname(nn)  = 'HCHOp_ispd'
      hcho_from_ispd =
     &       + 0.167*r(149) ! 149 OH ISPD = 1.565 PAR 0.167 FORM 0.713 XO2
     &       + 0.150*r(150) ! 150 O3 ISPD = 0.114 C2O3 0.15 FORM 0.85 MGLY
     &       + 0.282*r(151) ! 151 NO3 ISPD = 0.357 ALDX 0.282 FORM 1.282 PAR
     &       + 0.900*r(152) ! 152 ISPD = 0.333 CO 0.067 ALD2 0.9 FORM 0.832 PAR
      PA(nn) = hcho_from_ispd
C
C
C...  Total HCHO Production
      nn = nn + 1
      ptname(nn)  = 'HCHOp_Tot'
      PA(nn) =
     &      + hcho_from_eth
     &      + hcho_from_ole
     &      + hcho_from_iole
     &      + hcho_from_open
     &      + hcho_from_terp
     &      + hcho_from_isop
     &      + hcho_from_ispd
     &       + 0.330*r( 61) !  61 NTR OH = HNO3 HO2 0.33 FORM 0.33 ALD2 0.33 ALDX
     &       + 0.330*r( 62) !  62 NTR = NO2 HO2 0.33 FORM 0.33 ALD2 0.33 ALDX
     &       + 1.000*r( 68) !  68 MEO2 NO = FORM HO2 NO2
     &       + 1.370*r( 70) !  70 MEO2 MEO2 = 1.37 FORM 0.74 HO2 0.63 MEOH
     &       + 1.000*r( 72) !  72 MEPX = FORM HO2 OH
     &       + 1.000*r( 73) !  73 MEOH OH = FORM HO2
     &       + 1.000*r( 80) !  80 HCO3 = FORM HO2
     &       + cyc_HCO3_l   !  80 HCO3 = FORM HO2; 79 FORM HO2 = HCO3 
     &       + 1.000*r( 93) !  93 C2O3 MEO2 = 0.9 MEO2 0.9 HO2 FORM + 0.1 AACD
     &       + 0.100*r(109) ! 109 CXO3 MEO2 = 0.9 ALD2 0.9 XO2 HO2 0.1 AACD 0.1 FORM
     &       + 0.100*r(114) ! 114 OH ETOH = HO2 0.9 ALD2 0.05 ALDX 0.1 FORM 0.1 XO2
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
      PA(nn) = 1.000*r( 28) !  28 NO2 OH = HNO3
C
C
C...  { HNO3 production from NO3 }
      nn = nn + 1
      ptname(nn)  = 'HNO3_NO3HC'
      PA(nn) = 1.000*r( 48) !  48 NO3 HO2 = HNO3
     &       + 1.000*r( 78) !  78 FORM NO3 = HNO3 HO2 CO
     &       + 1.000*r( 86) !  86 ALD2 NO3 = C2O3 HNO3
     &       + 1.000*r(101) ! 101 ALDX NO3 = CXO3 HNO3
     &       + 1.000*r(135) ! 135 CRES NO3 = CRO HNO3
     &       + 0.150*r(151) ! 151 NO3 ISPD = 0.357 ALDX 0.282 FORM 0.15 HNO3
C
C
C...  { HNO3 production from N2O5 }
      nn = nn + 1
      ptname(nn)  = 'HNO3_N2O5'
      PA(nn) = 2.000*r( 19) !  19 N2O5 H2O = 2 HNO3
     &       + 2.000*r( 20) !  20 N2O5 H2O H2O = 2 HNO3
C
C
C...  { Other HNO3 production }
C...   61 NTR OH = HNO3 HO2 0.33 FORM 0.33 ALD2 0.33 ALDX -0.66 PAR
C
C
C...  { Net PAN prod }
      nn = nn + 1
      ptname(nn)  = 'PANprodNet'
      PA(nn) = cyc_PAN_p
C
C
C...  { Net PAN loss }
      nn = nn + 1
      ptname(nn)  = 'PANlossNet'
      PA(nn) = cyc_PAN_l
C
C
C...  { Organic Nitrate Production }
      nn = nn + 1
      ptname(nn)  = 'RNO3_prod'
      PA(nn) =  1.000*r( 55) !  55 XO2N NO = NTR
     &       +  1.000*r(118) ! 118 ROR NO2 = NTR
     &       +  0.100*r(132) ! 132 TO2 NO = 0.9 NO2 0.9 HO2 0.9 OPEN 0.1 NTR
     &       +  1.000*r(136) ! 136 CRO NO2 = NTR
     &       +  0.800*r(147) ! 147 NO3 ISOP = 0.2 ISPD 0.8 NTR XO2 0.8 HO2 0.2 NO2
     &       +  0.800*r(148) ! 148 NO2 ISOP = 0.2 ISPD 0.8 NTR XO2 0.8 HO2 0.2 NO
     &       +  0.850*r(151) ! 151 NO3 ISPD = 0.357 ALDX 0.282 FORM 0.85 NTR
     &       +  0.530*r(156) ! 156 TERP NO3 = 0.47 NO2 0.28 HO2 1.03 XO2 0.53 NTR
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
      PA(nn) =  1.000*r( 29) !  29 OH HNO3 = NO3
     &       +  1.000*r( 52) !  52 HNO3 = OH NO2
     &       +  1.000*r( 62) !  62 NTR = NO2 HO2 0.33 FORM 0.33 ALD2 0.33 ALDX
C
C
C...  { NO to NO2 by HO2 }
      nn = nn + 1
      ptname(nn)  = 'NOw_HO2'
      PA(nn) =  1.000*r( 30) !  30 HO2 NO = OH NO2
C
C
C...  { NO to NO2 by RO2s }
      nn = nn + 1
      ptname(nn)  = 'NOw_RO2s'
      PA(nn) =  1.000*r( 54) !  54 XO2 NO = NO2
     &       +  1.000*r( 68) !  68 MEO2 NO = FORM HO2 NO2
     &       +  0.900*r(132) ! 132 TO2 NO = 0.9 NO2 0.9 HO2 0.9 OPEN 0.1 NTR
C
C
C...  { NO to NO2 by RCO3s }
      nn = nn + 1
      ptname(nn)  = 'NOw_RCO3s'
      PA(nn) =  1.000*r( 54) !  54 XO2 NO = NO2
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
