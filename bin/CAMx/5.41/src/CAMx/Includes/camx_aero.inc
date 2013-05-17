c
c----CAMx v5.41 121109
c     
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
      real nacl
      real co2
      real foa
      real mhp
      real paa
      real caco3
      real mgco3
      real a3fe
      real b2mn
      real potcl
c
      common /radmx/ nacl, co2, foa, mhp, paa, caco3, mgco3, 
     &               a3fe, b2mn, potcl

      real eps
      real e0
      real lv
      real rv
c
      common /rhcalx/ eps, e0, lv, rv
c
c
c-----Pointers to CAMx species requird by PM routines
c
      integer kso2_c
      integer kh2o2_c
      integer kform_c
      integer khono_c
      integer ko3_c
      integer koh_c
      integer kho2_c
      integer kno3_c
      integer kno_c
      integer kno2_c
      integer kpan_c
      integer kcg1_c
      integer kcg2_c
      integer kcg3_c
      integer kcg4_c
      integer kcg5_c
      integer kcg6_c
      integer kcg7_c
      integer khno3_c
      integer knh3_c
      integer kh2so4_c
      integer khcl_c
      integer ksoa1_c
      integer ksoa2_c
      integer ksoa3_c
      integer ksoa4_c
      integer ksoa5_c
      integer ksoa6_c
      integer ksoa7_c
      integer ksopa_c
      integer ksopb_c
      integer kcrst_c
      integer kpoa_c
      integer kpec_c
      integer kph2o_c
      integer kpcl_c
      integer kna_c
      integer kpnh4_c
      integer kpno3_c
      integer kpso4_c
      integer kn2o5_c
      integer khpo_c
      integer kfoa_c
      integer kmhp_c
      integer kpaa_c
      integer kohp_c
      integer kopa_c
      integer nspec_c
c
      common /pm_ptrs/ kso2_c,kh2o2_c,kform_c,khono_c,ko3_c,koh_c,
     &                 kho2_c,kno3_c,kno_c,kno2_c,kpan_c,kcg1_c,
     &                 kcg2_c,kcg3_c,kcg4_c,kcg5_c,kcg6_c,kcg7_c,khno3_c,
     &                 knh3_c,kh2so4_c,khcl_c,ksoa1_c,ksoa2_c,ksoa3_c,
     &                 ksoa4_c,ksoa5_c,ksoa6_c,ksoa7_c,ksopa_c,ksopb_c,
     &                 kcrst_c,kpoa_c,kpec_c,kph2o_c,kpcl_c,kna_c,kpnh4_c,
     &                 kpno3_c,kpso4_c,kn2o5_c,
     &                 khpo_c,kfoa_c,kmhp_c,kpaa_c,kohp_c,kopa_c,nspec_c
