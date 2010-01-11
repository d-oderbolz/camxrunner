c
c-----CAMx v4.51 080522
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
      real nacl,co2,foa,mhp,paa,caco3,mgco3,a3fe,b2mn,potcl
      common /radmx/ nacl,co2,foa,mhp,paa,caco3,mgco3,a3fe,b2mn,potcl

      real eps,e0,lv,rv
      common /rhcalx/ eps,e0,lv,rv
c
c-----Pointers to CAMx species requird by PM routines
c
      integer       kso2_c,kh2o2_c,kform_c,khono_c,ko3_c,koh_c,
     &              kho2_c,kno3_c,kno_c,kno2_c,kpan_c,kcg1_c,
     &              kcg2_c,kcg3_c,kcg4_c,kcg5_c,kcg6_c,kcg7_c,khno3_c,
     &              knh3_c,kh2so4_c,khcl_c,ksoa1_c,ksoa2_c,ksoa3_c,
     &              ksoa4_c,ksoa5_c,ksoa6_c,ksoa7_c,ksopa_c,ksopb_c,
     &              kcrst_c,kpoa_c,kpec_c,kph2o_c,kpcl_c,kna_c,kpnh4_c,
     &              kpno3_c,kpso4_c,knxoy_c,
     &              khpo_c,kfoa_c,kmhp_c,kpaa_c,kohp_c,kopa_c,nspec_c
      common /ptrs/ kso2_c,kh2o2_c,kform_c,khono_c,ko3_c,koh_c,
     &              kho2_c,kno3_c,kno_c,kno2_c,kpan_c,kcg1_c,
     &              kcg2_c,kcg3_c,kcg4_c,kcg5_c,kcg6_c,kcg7_c,khno3_c,
     &              knh3_c,kh2so4_c,khcl_c,ksoa1_c,ksoa2_c,ksoa3_c,
     &              ksoa4_c,ksoa5_c,ksoa6_c,ksoa7_c,ksopa_c,ksopb_c,
     &              kcrst_c,kpoa_c,kpec_c,kph2o_c,kpcl_c,kna_c,kpnh4_c,
     &              kpno3_c,kpso4_c,knxoy_c,
     &              khpo_c,kfoa_c,kmhp_c,kpaa_c,kohp_c,kopa_c,nspec_c
