      subroutine cpamech3( r, rk, dtfact, nr, pa, npa, npa_init, ldark )
      use filunit
      use tracer
      use procan
c
c----CAMx v5.30 101223
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
C***********************************************************************
C  Called by CHEMDRIV, uses integrated reaction rates (R) to calculate
C  important chemical process rates (PA)
C
C    *** NOTE:  This code is hardwired for the CAMx CB4 mechanism 3 ***
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
C**   01     10/00  GST  Created for use with the CB4 photochemical mechanism.
C**   02     08/03  GY   Added output of J(NO2) and J(O3 to O1D) in hr-1
C**   03     08/04  GST  Expanded output parameters, including chain lengths
C**   04     08/05  GY   Revised radical prod/loss etc. and chain length
C**   05     08/07  PP|GY  Corrected errors and added HCHOp_eth and HCHOp_ole
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
      REAL   cyc_HNO4_p, cyc_HNO4_l
      REAL   cyc_PAN_p,  cyc_PAN_l
      REAL   OH_new, HO2_new, HOx_new, RO2_new
      REAL   OH_reacted, HO2_reacted, HOx_reacted
      REAL   OH_term, HO2_term, HOx_term, RO2_term
      REAL   OHwHC
      REAL   other_OH_prop
      REAL   POx_net, ratio_ind, total_ho2_prod 
      REAL   prod_h2o2, prod_hno3, o3_prod, o3_loss 
      REAL   Y_OH_per_HO2, OH_from_HO2, ho2tmp
      REAL   ho2_frac_c2o3
      REAL   hcho_from_eth, hcho_from_ole
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
C     OHw_PAR
C     OHw_TOL
C     OHw_XYL
C     OHw_ETH
C     OHw_OLE
C     OHw_ISOP
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
C     Ox =O3, NO2, NO3*2, O3P, O1D, HNO3, PAN, HNO4, N2O5*3, ORNIT
      nn =  1
      ptname(nn)  = 'OxProd'
      PA(nn) = 2.*R(20)  ! NO+NO=2*NO2
     &       +    R(24)  ! OH+HONO=NO2
     &       +    R(25)  ! HONO+HONO=NO+NO2 
     &       +    R(27)  ! OH+HNO3=NO3
     &       +    R(28)  ! HO2+NO=NO2+OH
     &       +    R(46)  ! C2O3+NO
     &       +    R(64)  ! TO2+NO=.9*NO2+.1*ORNIT
     &       +    R(79)  ! XO2+NO=NO2
     &       +    R(81)  ! XO2N+NO
C
C
      nn = nn + 1
      ptname(nn)  = 'OxLoss'
      PA(nn) =2.0*R(4)   ! O3P+NO2=NO
     &       +    R(11)  ! O1D+H2O=2*OH
     &       +    R(12)  ! O3+OH=HO2
     &       +    R(13)  ! O3+HO2=OH
     &       +.22*R(14)  ! NO3=NO
     &       +2.0*R(16)  ! NO3+NO2=NO+NO2
     &       +    R(18)  ! N2O5+H2O=2*HNO3
     &       +    R(21)  ! no+no2+h2o=2.000*hono
     &       +    R(40)  ! form+o=oh+ho2+co 
     &       +    R(41)  ! HCHO+NO3=HO2+HNO3+CO
     &       +    R(42)  ! ALD2+O
     &       +    R(44)  ! ALD2+NO3=C2O3+HNO3+.4*CP
     &       +    R(56)  ! OLE+O
     &       +    R(58)  ! OLT+O3
     &       +    R(59)  ! OLE+NO3=products
     &       +    R(60)  ! ETH+O
     &       +    R(62)  ! OL2+O3=HCHO+.40*ORA1+.42*CO+.12*HO2
     &       +    R(67)  ! CSL+NO3=HNO3+XNO2+.50*CSL
     &       +    R(71)  ! OPEN+O3
     &       +    R(75)  ! O+ISOP
     &       +    R(77)  ! ISO+O3
     &       +    R(78)  ! ISO+NO3=0.8*ONIT+0.2*NO2
     &       +    R(93)  ! ISPD+O3
     &       +    R(94)  ! ISPD+NO3=.85*NTR+.15*HNO3
     &       + .2*R(96)  ! ISOP+NO2=0.8*NTR
C
C
C...  calculate ratio of P(H2O2)/P(HNO3) to use as indicator
C     of NOX or VOC sensitivity
      prod_h2o2 =  R(32) + R(33) 
      prod_hno3 =  R(26) 
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
      O3_prod = R( 2)  ! O3P + O2
      O3_loss = R( 3)  ! NO
     &        + R( 7)  ! NO2
     &        + R( 8)  ! hv
     &        + R( 9)  ! hv
     &        + R(12)  ! OH
     &        + R(13)  ! HO2
     &        + R(58)  ! OLE
     &        + R(62)  ! ETH
     &        + R(71)  ! OPEN
     &        + R(77)  ! ISOP
     &        + R(93)  ! ISPD
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
C     ho2tmp is reaction of HO2 except with NO
      ho2tmp = R(50)+R(86)+R(87)+R(90) + 2.0*(R(32)+R(33))
C
      PA(nn) =  PA(nn)
     &       - R(12) * ho2tmp
     &               / ( ho2tmp + R(28) )
C...  O(3P) + VOC
      PA(nn) =  PA(nn)
     &       - ( R(56)+R(60)+R(75) )
C...  O3 + VOC
      PA(nn) =  PA(nn)
     &       - ( R(58)+R(62)+R(71)+R(77)+R(93) )
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
C  { 23} HONO=OH+NO
C  { 22} NO+OH=HONO
C
      sum =  R(22) - R(23)          ! HONO cycle
      if (sum.GT.0.) then
         cyc_HONO_p = sum
         cyc_HONO_l = 0.0
      else
         cyc_HONO_p =  0.0
         cyc_HONO_l = -sum
      end if
C 
C  { 29} HO2+NO2=HNO4
C  { 30} HNO4=HO2+NO2
C
      sum =  R(29) - R(30)           ! HNO4 cycle
      if (sum.GT.0.) then
         cyc_HNO4_p = sum
         cyc_HNO4_l = 0.0
      else
         cyc_HNO4_p =  0.0
         cyc_HNO4_l = -sum
      end if
C
C { 32} HO2+HO2=H2O2
C { 33} HO2+HO2+H2O=H2O2
C { 34} H2O2=2.00*OH
      sum =  R(32) + R(33) - R(34)   ! H2O2 cycle
      if (sum.GT.0.) then
         cyc_H2O2_p = 2.*sum
         cyc_H2O2_l = 0.0
      else
         cyc_H2O2_p =  0.0
         cyc_H2O2_l = -2.0*sum
      end if
C
C  { 47} C2O3+NO2=PAN
C  { 48} PAN=C2O3+NO2
C
      sum =  R(47) - R(48)            ! PAN Cycle
      if (sum.GT.0.) then
         cyc_PAN_p = sum
         cyc_PAN_l = 0.0
      else
         cyc_PAN_p =  0.0
         cyc_PAN_l = -sum
      end if
C
C
C     {      R a d i c a l    I n i t i a t i o n       }
C
C
C     CB4 has recursive radical production that must be
C     considered when tracking radical initiation, i.e.:
C     C2O3 makes some HO2
C
C...  Fractional yield of HO2 from C2O3 
C     50 C2O3 HO2 = 0.79 FORM 0.79 XO2 0.79 HO2 
C     46 C2O3 NO = 1 FORM 1 NO2 1 HO2 
C     47 C2O3 NO2 = 1 PAN
C     48 PAN = 1 C2O3 1 NO2
C     49 C2O3 C2O3 = 2 FORM 2 XO2 2 HO2  
      ho2_frac_c2o3 = ( 0.79*r(50)+r(46)+2.0*r(49) ) /
     &                  ( r(50)+r(46)+2.0*r(49)+cyc_PAN_p )
C
C
cgy      nn = nn + 1
cgy      ptname(nn)  = 'HO2frC2O3'
cgy      PA(nn) = ho2_frac_c2o3 * (dtfact/ppbfact)
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
     &                +( 2.000)*r( 11) ! 11 O1D H2O = 2 OH
     &                +( 1.000)*r( 23) ! 23 HONO = 1 NO 1 OH 
     &                +( 2.000)*r( 34) ! 34 H2O2 = 2 OH 
     &                +( 1.000)*r( 40) ! 40 FORM O = 1 OH 1 HO2 1 CO
     &                +( 1.000)*r( 42) ! 42 ALD2 O = 1 C2O3 1 OH
     &                +( 0.200)*r( 56) ! 56 O OLE = 0.63 ALD2 0.38 HO2 0.28 XO2
     &                +( 0.100)*r( 58) ! 58 O3 OLE = 0.5 ALD2 0.74 FORM 0.22 XO2
     &                +( 0.300)*r( 60) ! 60 O ETH = 1 FORM 1.7 HO2 1 CO
     &                +( 0.080)*r( 71) ! 71 OPEN O3 = 0.03 ALD2 0.62 C2O3 0.7 FORM
     &                +( 0.266)*r( 77) ! 77 O3 ISOP = 0.65 ISPD 0.6 FORM 0.2 XO2
     &                +( 0.268)*r( 93) ! 93 O3 ISPD = 0.114 C2O3 0.15 FORM 0.85 MGLY
C...  new HO2
      HO2_new =
     &                +( 2.000)*r( 38) ! 38 FORM = 2 HO2 1 CO 
     &                +( 1.000)*r( 40) ! 40 FORM O = 1 OH 1 HO2 1 CO 
     &                +( 1.000)*r( 41) ! 41 FORM NO3 = 1 HNO3 1 HO2 1 CO 
     &                +( 2.000)*r( 45) ! 45 ALD2 = 1 FORM 2 HO2 1 CO 
     &                +( 0.380)*r( 56) ! 56 O OLE = 0.63 ALD2 0.38 HO2 0.28 XO2 
     &                +( 0.440)*r( 58) ! 58 O3 OLE = 0.5 ALD2 0.74 FORM 0.22 XO2 
     &                +( 1.700)*r( 60) ! 60 O ETH = 1 FORM 1.7 HO2 1 CO 
     &                +( 0.120)*r( 62) ! 62 O3 ETH = 1 FORM 0.42 CO 0.12 HO2 
     &                +( 1.000)*r( 69) ! 69 OPEN = 1 C2O3 1 HO2 1 CO
     &                +( 0.760)*r( 71) ! 71 OPEN O3 = 0.03 ALD2 0.76 HO2
     &                +( 1.000)*r( 74) ! 74 MGLY = C2O3 HO2 CO
     &                +( 0.250)*r( 75) ! 75 O ISOP = 0.75 ISPD 0.25 HO2
     &                +( 0.066)*r( 77) ! 77 O3 ISOP = 0.65 ISPD 0.066 HO2
     &                +( 0.800)*r( 78) ! 78 NO3 ISOP = 0.2 ISPD 0.8 NTR 0.8 HO2
     &                +( 0.154)*r( 93) ! 93 O3 ISPD = 0.114 C2O3 0.154 HO2
     &                +( 0.925)*r( 94) ! 94 NO3 ISPD = 0.357 ALD2 0.925 HO2
     &                +( 1.033)*r( 95) ! 95 ISPD = 0.333 CO 0.067 ALD2 1.033 HO2
     &                +( 0.800)*r( 96) ! 96 NO2 ISOP = 0.2 ISPD 0.8 NTR XO2 0.8 HO2
C
C...  plus HO2 from decomposition of PNA, PAN compounds 
C     30 PNA = 1 HO2 1 NO2 
C     48 PAN = 1 C2O3 1 NO2
      HO2_new = HO2_new
     &                +cyc_HNO4_l+cyc_PAN_l*ho2_frac_c2o3
C
C...  plus second generation HO2 from photolysis of carbonyls
C     69 OPEN = 1 C2O3 1 HO2 1 CO
C     74 MGLY = 1 C2O3 1 HO2 1 CO
      HO2_new = HO2_new
     &                +(r(69)+r(74))*ho2_frac_c2o3
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
      PA(nn) = 2.*R(11)   !O1D+H2O->2*OH
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
     &       + ( 0.100)*r( 58) ! 58 O3 OLE = 0.5 ALD2 0.74 FORM 0.22 XO2
     &       + ( 0.080)*r( 71) ! 71 OPEN O3 = 0.03 ALD2 0.62 C2O3 0.7 FORM
     &       + ( 0.266)*r( 77) ! 77 O3 ISOP = 0.65 ISPD 0.6 FORM 0.2 XO2
     &       + ( 0.268)*r( 93) ! 93 O3 ISPD = 0.114 C2O3 0.15 FORM 0.85 MGLY
C
C
C...  { New HO2 from HCHO }
      nn = nn + 1
      ptname(nn)  = 'nwHO2_HCHO'
      PA(nn) = 2.*R(38) !HCHO=2*HO2+CO
     &       +    R(40) !HCHO+O
     &       +    R(41) !HCHO+NO3
C
C
C...  { New RO2 Production }
      nn = nn + 1
      ptname(nn)  = 'RO2_new'
C...  new C2O3
      RO2_new = cyc_PAN_l
     &       + 1.000*R(69) ! 69 OPEN = 1 C2O3 1 HO2 1 CO
     &       + 1.000*R(74) ! 74 MGLY = 1 C2O3 1 HO2 1 CO
     &       + 0.967*R(95) ! 95 ISPD = 0.333 CO 0.067 ALD2 0.9 FORM
     &       + 1.000*r(42) ! 42 ALD2 O = 1 C2O3 1 OH
     &       + 0.250*r(75) ! 75 O ISOP = 0.75 ISPD 0.5 FORM 0.25 XO2
     &       + 0.620*r(71) ! 71 OPEN O3 = 0.03 ALD2 0.62 C2O3 0.7 FORM
     &       + 0.200*r(77) ! 77 O3 ISOP = 0.65 ISPD 0.6 FORM 0.2 XO2
     &       + 0.114*r(93) ! 93 O3 ISPD = 0.114 C2O3 0.15 FORM 0.85 MGLY
     &       + 1.000*r(44) ! 44 ALD2 NO3 = 1 C2O3 1 HNO3
     &       + 0.075*r(94) ! 94 NO3 ISPD = 0.357 ALD2 0.282 FORM 1.282 PAR
C...  new XO2
      RO2_new = RO2_new
     &       + 1.000*R(45) ! 45 ALD2 = FORM 2 HO2 CO XO2
     &       + 0.700*R(95) ! 95 ISPD = 0.333 CO 0.067 ALD2 0.9 FORM
     &       + 0.280*R(56) ! 56 O OLE = 0.63 ALD2 0.38 HO2 0.28 XO2
     &       + 0.700*R(60) ! 60 O ETH = 1 FORM 1.7 HO2 1 CO
     &       + 0.250*R(75) ! 75 O ISOP = 0.75 ISPD 0.5 FORM 0.25 XO2
     &       + 0.220*R(58) ! 58 O3 OLE = 0.5 ALD2 0.74 FORM 0.22 XO2
     &       + 0.030*R(71) ! 71 OPEN O3 = 0.03 ALD2 0.62 C2O3 0.7 FORM
     &       + 0.200*R(77) ! 77 O3 ISOP = 0.65 ISPD 0.6 FORM 0.2 XO2
     &       + 0.064*R(93) ! 93 O3 ISPD = 0.114 C2O3 0.15 FORM 0.85 MGLY
     &       + 0.910*R(59) ! 59 NO3 OLE = 0.91 XO2 1 FORM 0.09 XO2N
     &       + 1.000*R(78) ! 78 NO3 ISOP = 0.2 ISPD 0.8 NTR 1 XO2
     &       + 0.075*R(94) ! 94 NO3 ISPD = 0.357 ALD2 0.282 FORM 1.282 PAR
     &       + 1.000*R(96) ! 96 NO2 ISOP = 0.2 ISPD 0.8 NTR XO2 0.8 HO2 0.2 NO
C...  new XO2N
      RO2_new = RO2_new
     &       + 0.020*R(56) ! 56 O OLE = 0.63 ALD2 0.38 HO2 0.28 XO2
     &       + 0.090*R(59) ! 59 NO3 OLE = 0.91 XO2 1 FORM 0.09 XO2N
C
C...  Total RO2 Rad initiation
C
      PA(nn) = RO2_new 
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
      PA(nn) = R(36)
C
C...  OH+CH4
      nn = nn + 1
      ptname(nn)  = 'OHw_CH4'
      PA(nn) = R(51)
C
C...  OH+PAR
      nn = nn + 1
      ptname(nn)  = 'OHw_PAR'
      PA(nn) = R(52)
C
C...  OH+TOL
      nn = nn + 1
      ptname(nn)  = 'OHw_TOL'
      PA(nn) = R(63)
C
C...  OH+XYL
      nn = nn + 1
      ptname(nn)  = 'OHw_XYL'
      PA(nn) = R(72)
C
C...  OH+ETH
      nn = nn + 1
      ptname(nn)  = 'OHw_ETH'
      PA(nn) = R(61)
C
C...  OH+OLE
      nn = nn + 1
      ptname(nn)  = 'OHw_OLE'
      PA(nn) = R(57)
C
C...  OH+ISOP
      nn = nn + 1
      ptname(nn)  = 'OHw_ISOP'
      PA(nn) = R(76)  !ISOP+OH
C
C
C...  { All OH rxns with organics }
      nn = nn + 1
      ptname(nn)  = 'OHw_all_HC'
      OHwHC  = R(36)   !OH+CO
     &       + R(37)   !HCHO
     &       + R(43)   !ALD2
     &       + R(51)   !OH+CH4
     &       + R(52)   !PAR
     &       + R(57)   !OLE  
     &       + R(61)   !ETH
     &       + R(63)   !TOL
     &       + R(66)   !CRES
     &       + R(70)   !OPEN
     &       + R(72)   !XYL
     &       + R(73)   !MGLY
     &       + R(76)   !ISOP+OH
     &       + R(84)   !{MEOH+OH}
     &       + R(85)   !{ETOH+OH}
     &       + R(92)   !OH+ISPD
      PA(nn) = OHwHC
C
C
C
C...  { Isoprene with other oxidants }
      nn = nn + 1
      ptname(nn)  = 'ISOPwOx'
      PA(nn) = R(75) !ISOP+O3P
     &       + R(77) !ISOP+O3
     &       + R(78) !ISOP+NO3
     &       + R(96) !ISOP+NO2
C
C
C...  { HOx reacted: reaction consuming OH or HO2 as reactant }
C
C...  OH reacted
      OH_reacted =
     &                +( 1.000)*r( 12) ! 12 O3 OH = 1 HO2
     &                +( 1.000)*r( 22) ! 22 NO OH = 1 HONO
     &                +( 1.000)*r( 24) ! 24 OH HONO = 1 NO2
     &                +( 1.000)*r( 26) ! 26 NO2 OH = 1 HNO3 
     &                +( 1.000)*r( 27) ! 27 OH HNO3 = 1 NO3
     &                +( 1.000)*r( 31) ! 31 OH PNA = 1 NO2
     &                +( 1.000)*r( 35) ! 35 OH H2O2 = 1 HO2 
     &                +( 1.000)*r( 36) ! 36 OH CO = 1 HO2 
     &                +( 1.000)*r( 37) ! 37 FORM OH = 1 HO2 1 CO 
     &                +( 1.000)*r( 43) ! 43 ALD2 OH = 1 C2O3
     &                +( 1.000)*r( 51) ! 51 OH = 1 FORM 1 XO2 1 HO2
     &                +( 1.000)*r( 52) ! 52 PAR OH = 0.87 XO2 0.13 XO2N 0.11 HO2
     &                +( 1.000)*r( 57) ! 57 OH OLE = 1 FORM 1 ALD2 -1 PAR
     &                +( 1.000)*r( 61) ! 61 OH ETH = 1 XO2 1.56 FORM 0.22 ALD2
     &                +( 1.000)*r( 63) ! 63 TOL OH = 0.44 HO2 0.08 XO2 0.36 CRES
     &                +( 1.000)*r( 66) ! 66 OH CRES = 0.4 CRO 0.6 XO2 0.6 HO2
     &                +( 1.000)*r( 70) ! 70 OPEN OH = 1 XO2 2 CO 2 HO2
     &                +( 1.000)*r( 72) ! 72 OH XYL = 0.7 HO2 0.5 XO2 0.2 CRES
     &                +( 1.000)*r( 73) ! 73 OH MGLY = 1 XO2 1 C2O3 
     &                +( 1.000)*r( 76) ! 76 OH ISOP = 0.912 ISPD 0.629 FORM 0.991 XO2
     &                +( 1.000)*r( 82) ! 82 SO2 OH = 1 SULF 1 HO2
     &                +( 1.000)*r( 84) ! 84 MEOH OH = 1 FORM 1 HO2 
     &                +( 1.000)*r( 85) ! 85 ETOH OH = 1 HO2 1 ALD2 
     &                +( 1.000)*r( 90) ! 90 OH HO2 =  
     &                +( 1.000)*r( 92) ! 92 OH ISPD = 1.565 PAR 0.167 FORM 0.713 XO2
C...  HO2 reacted
      HO2_reacted = 
     &                +( 1.000)*r( 13) ! 13 O3 HO2 = 1 OH
     &                +( 1.000)*r( 28) ! 28 HO2 NO = 1 OH 1 NO2 
     &                +( 1.000)*r( 29) ! 29 HO2 NO2 = 1 PNA 
     &                +( 2.000)*r( 32) ! 32 HO2 HO2 = 1 H2O2 
     &                +( 2.000)*r( 33) ! 33 HO2 HO2 H2O = 1 H2O2
     &                +( 1.000)*r( 50) ! 50 C2O3 HO2 = 0.79 FORM 0.79 XO2 0.79 HO2
     &                +( 1.000)*r( 86) ! 86 XO2 HO2 = 
     &                +( 1.000)*r( 87) ! 87 XO2N HO2 = 
     &                +( 1.000)*r( 90) ! 90 OH HO2 = 
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
C...  { RO2 reacted: reaction consuming C2O3, TO2, XO2 or 
C     XO2N as reactant }
C
      nn = nn + 1
      ptname(nn)  = 'RO2_rctd'
C...  C2O3 reacted
      PA(nn) = 1.000*r(46) ! 46 C2O3 NO = 1 FORM 1 NO2 1 HO2
     &       + cyc_PAN_p   ! 47 C2O3 NO2 = 1 PAN
     &       + 2.000*r(49) ! 49 C2O3 C2O3 = 2 FORM 2 XO2 2 HO2
     &       + 1.000*r(50) ! 50 C2O3 HO2 = 0.79 FORM 0.79 XO2 0.79 HO2
C...  TO2 reacted
      PA(nn) = PA(nn)
     &       + 1.000*r(64) ! 64 TO2 NO = 0.9 NO2 0.9 HO2 0.9 OPEN
     &       + 1.000*r(65) ! 65 TO2 = 1 CRES 1 HO2
C...  XO2 reacted
      PA(nn) = PA(nn)
     &       + 1.000*r(79) ! 79 XO2 NO = 1 NO2
     &       + 2.000*r(80) ! 80 XO2 XO2 =
     &       + 1.000*r(86) ! 86 XO2 HO2 =
     &       + 1.000*r(89) ! 89 XO2 XO2N =
C...  XO2N reacted
      PA(nn) = PA(nn)
     &       + 1.000*r(81) ! 81 XO2N NO = 1 NTR
     &       + 1.000*r(87) ! 87 XO2N HO2 =
     &       + 2.000*r(88) ! 88 XO2N XO2N =
     &       + 1.000*r(89) ! 89 XO2 XO2N =
C
C
C...  { OH from HO2 }
C     Here we add up all rxns that convert HO2 to OH
      nn = nn + 1
      ptname(nn)  = 'OHfromHO2'
      OH_from_HO2 = 
     &       +      R(13)      !HO2+O3  
     &       +      R(28)      !HO2+NO
     &       + 0.79*R(50)      !HO2+C2O3
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
C     C2O3
C
C...  OH terminated
      OH_term =
     &                + cyc_HONO_p  ! 22 NO OH = 1 HONO
     &                + 1.000*R(24) ! 24 OH HONO = 1 NO2
     &                + 1.000*R(26) ! 26 NO2 OH = 1 HNO3
     &                + 1.000*R(27) ! 27 OH HNO3 = 1 NO3
     &                + 1.000*R(31) ! 31 OH PNA = 1 NO2
     &                + 1.000*R(90) ! 90 OH HO2 =
C
C...  HO2 terminated
      HO2_term = 
     &                + cyc_HNO4_p  ! 29 HO2 NO2 = 1 PNA
     &                + cyc_H2O2_p  ! 32 HO2 HO2 and 33 HO2 HO2 H2O
     &                + 1.000*R(86) ! 86 XO2 HO2 =
     &                + 1.000*R(87) ! 87 XO2N HO2 = 
     &                + 1.000*R(90) ! 90 OH HO2 =
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
C     RO2 radicals are C2O3, TO2, XO2, XO2N 
      nn = nn + 1
      ptname(nn)  = 'RO2_term'
C...  C2O3 
      PA(nn) = cyc_PAN_p   ! 47 C2O3 NO2 = 1 PAN
     &       + 1.000*r(50) ! 50 C2O3 HO2 = 0.79 FORM 0.79 XO2 0.79 HO2
C...  TO2
      PA(nn) = PA(nn)
     &       + 0.100*r(64) ! 64 TO2 NO = 0.9 NO2 0.9 HO2 0.9 OPEN 0.1 NTR
     &       + 1.000*r(65) ! 65 TO2 = 1 CRES 1 HO2
C...  XO2
      PA(nn) = PA(nn)
     &       + 2.000*r(80) ! 80 XO2 XO2 =
     &       + 1.000*r(86) ! 86 XO2 HO2 =
     &       + 1.000*r(89) ! 89 XO2 XO2N =
C...  XO2N
      PA(nn) = PA(nn)
     &       + 1.000*r(81) ! 81 XO2N NO = 1 NTR
     &       + 1.000*r(87) ! 87 XO2N HO2 =
     &       + 2.000*r(88) ! 88 XO2N XO2N =
     &       + 1.000*r(89) ! 89 XO2 XO2N =
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
      ptname(nn)  = 'HCHOp_eth'
      hcho_from_eth =
     &       + 1.000*r( 60) !  60 O ETH = FORM 1.7 HO2 CO 0.7 XO2 0.3 OH
     &       + 1.560*r( 61) !  61 OH ETH = XO2 1.56 FORM 0.22 ALDX HO2
     &       + 1.000*r( 62) !  62 O3 ETH = FORM 0.42 CO 0.12 HO2
      PA(nn) = hcho_from_eth
C
C
C... { HCHO Production from OLE }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_ole'
      hcho_from_ole =
     &       + 0.200*r( 56) !  56 O OLE = 0.63 ALD2 0.38 HO2 0.28 XO2 0.2 FORM
     &       + 1.000*r( 57) !  57 OH OLE = FORM ALD2 -1 PAR XO2 HO2
     &       + 0.740*r( 58) !  58 O3 OLE = 0.5 ALD2 0.74 FORM 0.22 XO2 0.1 OH
     &       + 1.000*r( 59) !  59 NO3 OLE = 0.91 XO2 + FORM + 0.09 XO2N
      PA(nn) = hcho_from_ole
C
C
C... { HCHO Production from isoprene }
      nn = nn + 1
      ptname(nn)  = 'HCHOp_isop'
      hcho_from_isop =
     &       + 0.50*R(75)  !ISOP+O
     &       +0.629*R(76)  !ISOP+OH
     &       + 0.60*R(77)  !ISOP+O3
      PA(nn) = hcho_from_isop     
C
C
C... { HCHO Production from isoprene products}
      nn = nn + 1
      ptname(nn)  = 'HCHOp_ispd'
      hcho_from_ispd =
     &       +0.167*R(92)  !ISPD+OH
     &       + 0.15*R(93)  !ISPD+O3
     &       +0.282*R(94)  !ISPD+NO3
     &       + 0.90*R(95)  !ISPD
      PA(nn) = hcho_from_ispd     
C
C
C...  Total HCHO Production
      nn = nn + 1
      ptname(nn)  = 'HCHOp_Tot'
      PA(nn) =      R(45)  !ALD2=
     &       +      R(46)  !C2O3+NO
     &       + 2.00*R(49)  !C2O3+C2O3
     &       + 0.79*R(50)  !C2O3+HO2
     &       +      R(51)  !CH4+OH
     &       +      R(70)  !OPEN+OH
     &       + 0.70*R(71)  !OPEN+O3
     &       +      R(84)  !MEOH+OH
     &       + hcho_from_eth
     &       + hcho_from_ole
     &       + hcho_from_isop
     &       + hcho_from_ispd
C
C
C-----------------End of Formaldehyde Chemistry-------------------------
C
C
C            {  N O y     C h e m i s t r y      }
C
C
C... { HNO3 production from OH+NO2}
      nn = nn + 1
      ptname(nn)  = 'HNO3_OHNO2'
      PA(nn) = R(26)  !OH+NO2
C
C
C...  { HNO3 production from NO3 }
      nn = nn + 1
      ptname(nn)  = 'HNO3_NO3HC'
      PA(nn) =      R(41)  !NO3+HCHO
     &       +      R(44)  !NO3+ALD
     &       +      R(67)  !NO3+CRES
     &       + 0.15*R(94)  !NO3+ISPD
C
C
C...  { Other HNO3 production }
      nn = nn + 1
      ptname(nn)  = 'HNO3_N2O5'
      PA(nn) = 2.0*R(18)  !N2O5+H2O
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
      PA(nn) =      R(55)  !ROR+NO2
     &       + 0.10*R(64)  !TO2+NO
     &       +      R(68)  !CRO+NO2
     &       + 0.80*R(78)  !NO3+ISO
     &       +      R(81)  !XO2N+NO
     &       + 0.85*R(94)  !NO3+ISPD
     &       + 0.80*R(96)  !NO2+ISPD
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
      PA(nn)  =  1.000*R(27)  ! 27 OH HNO3 = 1 NO3
C
C
C...  { NO to NO2 by HO2 }
      nn = nn + 1
      ptname(nn)  = 'NOw_HO2'
      PA(nn)  =  1.000*R(28)  ! 28 HO2 NO = 1 OH 1 NO2
C
C
C...  { NO to NO2 by RO2s }
      nn = nn + 1
      ptname(nn)  = 'NOw_RO2s'
      PA(nn)  =  0.900*R(64)  ! 64 TO2 NO = 0.9 NO2 0.9 HO2 0.9 OPEN
     &        +  1.000*R(79)  ! 79 XO2 NO = 1 NO2
C
C
C...  { NO to NO2 by RCO3s }(peroxyacyl radicals)
      nn = nn + 1
      ptname(nn)  = 'NOw_RCO3s'
      PA(nn)  =  1.000*R(46)  ! 46 C2O3 NO = FORM NO2 HO2 XO2
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
         write(iout,'(//,a)') 'ERROR in CPAMECH3'
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
