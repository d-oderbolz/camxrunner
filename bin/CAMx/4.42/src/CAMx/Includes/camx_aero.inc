c
c-----CAMx v4.42 070603
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Include file for passing aerosol module parameters
c     to AERO routines
c
      real*8 dsec_c(nsecp1),dsecf_c(nsecp1)
      real*8 dmin,dmax
c
      common /sections/ dmin,dmax,dsec_c,dsecf_c

c
c-----Optional flags for the PM routines
c
      character*4 chaero,chaq
      common /aeroopt/chaero,chaq

      real aqcwmin,aqtamin
      common /aqcond/aqcwmin,aqtamin
   
      logical lfrst
      common /init/ lfrst
c
c-----Pointers to CAMx species requird by PM routines
c
      common /ptrs/ kso2_c,kh2o2_c,kform_c,khono_c,ko3_c,koh_c,
     &              kho2_c,kno3_c,kno_c,kno2_c,kpan_c,kcg1_c,
     &              kcg2_c,kcg3_c,kcg4_c,kcg5_c,khno3_c,knh3_c,
     &              kh2so4_c,khcl_c,ksoa1_c,ksoa2_c,ksoa3_c,ksoa4_c,
     &              ksoa5_c,kcrst_c,kpoa_c,kpec_c,kph2o_c,kpcl_c,kna_c,
     &              kpnh4_c,kpno3_c,kpso4_c,knxoy_c
c
c-----Adjustment factors for aerosol molecular weights
c     for interface between aerosol modules (= CAMx M/Aerosol M)
c
c      real*4 wtfac_ae(MXSPEC), wtfac_aq(MXSPEC)
c
c      common /wtfac/ wtfac_ae,wtfac_aq
