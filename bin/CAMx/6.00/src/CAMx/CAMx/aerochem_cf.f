      subroutine aerochem_cf(h2o,tempk,press,cwc,cph,con,convfac,dtaq,
     &                       dtaer,delcon,ldoipr,ipa_cel,aero_flag,
     &                       pig_flag)
      use camxcom
      use chmstry
      use filunit
      use tracer
      use procan
c
c----CAMx v6.00 130506
c
c     AEROCHEM_CF drives the CF 2-section aerosol model in CAMx.
c     It calculates the following chemical transformations:
c        1. Condensible organic gasses to organic aerosol (SOAP)
c        2. Gaseous sulfate to aerosol sulfate (from gas-phase chem)
c        3. Inorganic aqueous reactions (RADM-AQ approach)
c        4. Inorganic gas-aerosol equilibrium partitioning for the
c           ammonium/nitrate/sulfate/sodium/chloride system (ISORROPIA)
c     The gas species are in ppm
c     The aerosol species are in (ug/m3)
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications: 
c        1/9/02         Minor bug fixes (units conversions)
c        11/19/02       Incorporated RADM aqueous chemistry and ISORROPIA
c        12/9/02        Incorporated SOAP routine
c        10/24/05       Cloud pH is now passed in/out; Henry's Law values
c                       for SO2 and O3 no longer passed out
c        8/23/06        Removed metastable option in ISOROPIA
c        12/26/06       AQ-CHEM now called each chemistry timestep
c        12/29/06       Revised for the updated SOA scheme
c        01/10/07       Added species mapping for CB05/SAPRC99
c        05/04/07       Added array (wt_save) to keep original total
c                       mass because ISOROPIA overwrites with lower
c                       bound value.
c        05/30/07       Skip call to RAQCHEM under high acid conditions.
c        05/13/08       Modified to skip ISOROPIA for certain PiG calls.
c        01/21/10       Added formation of mineral dust nitrate
c
c     Input arguments:
c        h2o                 cell water vapor (ppm)
c        tempk               cell temperature (K)
c        press               cell pressure (mb)
c        cwc                 cloud water content (g/m3)
c        cph                 cloud water pH 
c        con                 species concentrations (ppm, ug/m3)
c        convfac             conversion factor: umol/m3 = ppm * convfac
c        dtaq                aqueous chemistry time step (hours)
c        dtaer               general PM chemistry time step (hours)
c        aero_flag           1 - time for aqueous chemistry (includes isoropia);
c                            2 - time for all chemistry
c        pig_flag            T - skip isorropia chemistry for PiG
c
c     Output arguments:
c        con                 species concentrations (ppm, ug/m3)
c        cph                 cloud water pH 
c
c     Routines called: 
c        SOAP
c        RAQCHEM
c        ISOROPIA
c 
c     Called by: 
c        CHEMDRIV 
c
      include 'camx.prm' 
      include 'camx_aero.inc'
      include 'chmdbg.inc'
c
c========================== Process Analysis Begin =======================
c
      logical ldoipr
      integer ipa_cel
      real csulf,chno3,cnh3,cpso4,cpno3,cpnh4
c
c========================== Process Analysis End =======================
c
      real    h2o
      real    tempk
      real    press
      real    cwc
      real    cph
      real    con(*)
      real    convfac
      real    dtaq
      real    dtaer
      real    delcon(5,*)
      integer aero_flag
      logical pig_flag

      real cold(MXSPEC), deln2o5
      real cw_kgm3
      integer isrpia_flag
c
c-----Arrays for SOAP
c
      real soacon(7),cg(7)
      real csatT(7),cpre,mwpre,mwpre0
      real apoly(7),sopa,sopb
c
c-----Arrays for RADM aqueous chemistry
c
      real r_gas(11),r_aer(9)
      real r_1,r_2
c
c-----Variables for ISOROPIA need double precision
c
      real*8 wi(5),wt(5),wt_save(5)
      real*8 rhi,tempi,cntrl(2)
      real*8 gasis(3),aerliq(12),aersld(9),other(6)
      character*15 scasi
c
c-----Variables for mineral dust nitrate
c
      real*8 dno3 ! mineral dust nitrate (mol/m3)
      real fCaCO3
      parameter (fCaCO3 = 0.065) ! mass fraction of CaCO3 on dust
c
c-----MW of Primary Organic Aerosol - should be consistent with that in
c     /CMU_AERO/block.f
c
      data mwpre0 /220.0/
c 
c-----Entry point 
c
      do ispc = 1,nspec
         cold(ispc) = con(ispc)
      enddo
      con(nspec+1) = 0.
      cph = 5.0
      isrpia_flag = 0
c
c-----Calculate relative humidity
c
      qwatr = 1.e-6*h2o*18./28.8 
      ev = qwatr*press/(qwatr + eps) 
      es = e0*exp((lv/rv)*(1./273. - 1./tempk)) 
      rh = 100.0*amin1(0.99,ev/es)
c
c-----Partitioning of condensable secondary organics between the gas (CG)
c     and aerosol phases (SOA) using the SOAP semi-volatile scheme.
c     CGs are in ppm, SOAs are in ug/m3, SOAP does the units conversion.
c
      if (aero_flag.eq.2) then
        cg(1)  = con(kcg1)
        cg(2)  = con(kcg2)
        cg(3)  = con(kcg3)
        cg(4)  = con(kcg4)
        cg(5)  = con(kcg5)
        cg(6)  = con(kcg6)
        cg(7)  = con(kcg7)
        soacon(1) = con(ksoa1)
        soacon(2) = con(ksoa2)
        soacon(3) = con(ksoa3)
        soacon(4) = con(ksoa4)
        soacon(5) = con(ksoa5)
        soacon(6) = con(ksoa6)
        soacon(7) = con(ksoa7)
        sopa   = con(ksopa)
        sopb   = con(ksopb)
        cpre   = con(kpoa)
        mwpre  = mwpre0
        call soap(7,soacon,cg,apoly,sopa,sopb,tempk,convfac,
     &            dtaer,iout,igrdchm,ichm,jchm,kchm,.TRUE.,
     &            cpre,mwpre,csatT)
        con(kcg1)  = amax1(cg(1),bdnl(kcg1))
        con(kcg2)  = amax1(cg(2),bdnl(kcg2))
        con(kcg3)  = amax1(cg(3),bdnl(kcg3))
        con(kcg4)  = amax1(cg(4),bdnl(kcg4))
        con(kcg5)  = amax1(cg(5),bdnl(kcg5))
        con(kcg6)  = amax1(cg(6),bdnl(kcg6))
        con(kcg7)  = amax1(cg(7),bdnl(kcg7))
        con(ksoa1) = amax1(soacon(1),bdnl(ksoa1))
        con(ksoa2) = amax1(soacon(2),bdnl(ksoa2))
        con(ksoa3) = amax1(soacon(3),bdnl(ksoa3))
        con(ksoa4) = amax1(soacon(4),bdnl(ksoa4))
        con(ksoa5) = amax1(soacon(5),bdnl(ksoa5))
        con(ksoa6) = amax1(soacon(6),bdnl(ksoa6))
        con(ksoa7) = amax1(soacon(7),bdnl(ksoa7))
        con(ksopa) = amax1(sopa,bdnl(ksopa))
        con(ksopb) = amax1(sopb,bdnl(ksopb))
c
c========================= Process Analysis Begin ======================
c
        if ( ldoipr ) then
          cipr(IPR_OAERO,ipa_cel,kcg1) = cipr(IPR_OAERO,ipa_cel,kcg1)
     &                                 + (con(kcg1)-cold(kcg1))*convfac
          cipr(IPR_OAERO,ipa_cel,kcg2) = cipr(IPR_OAERO,ipa_cel,kcg2)
     &                                 + (con(kcg2)-cold(kcg2))*convfac
          cipr(IPR_OAERO,ipa_cel,kcg3) = cipr(IPR_OAERO,ipa_cel,kcg3)
     &                                 + (con(kcg3)-cold(kcg3))*convfac
          cipr(IPR_OAERO,ipa_cel,kcg4) = cipr(IPR_OAERO,ipa_cel,kcg4)
     &                                 + (con(kcg4)-cold(kcg4))*convfac
          cipr(IPR_OAERO,ipa_cel,kcg5) = cipr(IPR_OAERO,ipa_cel,kcg5)
     &                                 + (con(kcg5)-cold(kcg5))*convfac
          cipr(IPR_OAERO,ipa_cel,kcg6) = cipr(IPR_OAERO,ipa_cel,kcg6)
     &                                 + (con(kcg6)-cold(kcg6))*convfac
          cipr(IPR_OAERO,ipa_cel,kcg7) = cipr(IPR_OAERO,ipa_cel,kcg7)
     &                                 + (con(kcg7)-cold(kcg7))*convfac
          cipr(IPR_OAERO,ipa_cel,ksoa1) = cipr(IPR_OAERO,ipa_cel,ksoa1)
     &                                  + con(ksoa1)-cold(ksoa1)
          cipr(IPR_OAERO,ipa_cel,ksoa2) = cipr(IPR_OAERO,ipa_cel,ksoa2)
     &                                  + con(ksoa2)-cold(ksoa2)
          cipr(IPR_OAERO,ipa_cel,ksoa3) = cipr(IPR_OAERO,ipa_cel,ksoa3)
     &                                  + con(ksoa3)-cold(ksoa3)
          cipr(IPR_OAERO,ipa_cel,ksoa4) = cipr(IPR_OAERO,ipa_cel,ksoa4)
     &                                  + con(ksoa4)-cold(ksoa4)
          cipr(IPR_OAERO,ipa_cel,ksoa5) = cipr(IPR_OAERO,ipa_cel,ksoa5)
     &                                  + con(ksoa5)-cold(ksoa5)
          cipr(IPR_OAERO,ipa_cel,ksoa6) = cipr(IPR_OAERO,ipa_cel,ksoa6)
     &                                  + con(ksoa6)-cold(ksoa6)
          cipr(IPR_OAERO,ipa_cel,ksoa7) = cipr(IPR_OAERO,ipa_cel,ksoa7)
     &                                  + con(ksoa7)-cold(ksoa7)
          cipr(IPR_OAERO,ipa_cel,ksopa) = cipr(IPR_OAERO,ipa_cel,ksopa)
     &                                  + con(ksopa)-cold(ksopa)
          cipr(IPR_OAERO,ipa_cel,ksopb) = cipr(IPR_OAERO,ipa_cel,ksopb)
     &                                  + con(ksopb)-cold(ksopb)
          csulf = con(ksulf)
          chno3 = con(khno3)
          cnh3  = con(knh3 )
          cpso4 = con(kpso4)
          cpno3 = con(kpno3)
          cpnh4 = con(kpnh4)
        endif
c
c========================== Process Analysis End =======================
c
        isrpia_flag = 1
        if (pig_flag) isrpia_flag = 0
      endif ! call SOAP?
c
c-----Do RADM aqueous chemistry if CWC is above threshold
c     All conc units must be mol/mol (mixing ratio)
c
      if (cwc.ge.cwmin .and. tempk.ge.tamin) then
        pres_pa = 100.*press
        dt_sec = dtaq*3600.
        cw_kgm3 = cwc/1000.
c
        r_gas(1)  = con(kso2)*1.e-6
        r_gas(2)  = con(khno3)*1.e-6
        r_gas(3)  = con(kn2o5)*1.e-6
        r_gas(4)  = co2*1.e-6
        r_gas(5)  = con(knh3)*1.e-6
        r_gas(6)  = con(khpo_c)*1.e-6
        r_gas(7)  = con(ko3)*1.e-6
        if (kfoa_c.lt.nspec+1) then
          r_gas(8) = con(kfoa_c)*1.e-6
        else
          r_gas(8) = foa*1.e-6
        endif
        if (kmhp_c.lt.nspec+1 .or. kohp_c.lt.nspec+1) then
          r_1 = con(kmhp_c) + con(kohp_c)
          r_gas(9) = r_1*1.e-6
          r_1 = con(kmhp_c) / r_1
        else
          r_gas(9) = mhp*1.e-6
        endif
        if (kpaa_c.lt.nspec+1 .or. kopa_c.lt.nspec+1) then
          r_2 = con(kpaa_c) + con(kopa_c)
          r_gas(10) = r_2*1.e-6
          r_2 = con(kpaa_c) / r_2
        else
          r_gas(10) = paa*1.e-6
        endif
        r_gas(11) = con(ksulf)*1.e-6
c
        r_aer(1)  = (con(kpso4)/96./convfac)*1.e-6
        r_aer(2)  = (con(kpnh4)/18./convfac)*1.e-6
        r_aer(3)  = (con(kpno3)/62./convfac)*1.e-6
        r_aer(4)  = (fCaCO3*con(kFCRS)/100./convfac)*1.e-6
        r_aer(5)  = (mgco3/84./convfac)*1.e-6
        if (kna.eq.nspec+1) then
          r_aer(6) = (nacl/58./convfac)*1.e-6
        else
          if (con(kna).gt.con(kpcl)) then
            r_aer(6) = (con(kpcl)/35./convfac)*1.e-6
          else
            r_aer(6) = (con(kna)/23./convfac)*1.e-6
          endif
        endif
        r_aer(7)  = (a3fe/56./convfac)*1.e-6
        r_aer(8)  = (b2mn/55./convfac)*1.e-6
        r_aer(9)  = (potcl/74./convfac)*1.e-6
c
        if (r_aer(1).lt.5.0e-7 .and. r_aer(3).lt.1.5e-6) then ! check high acid conditions

        call raqchem(tempk,pres_pa,dt_sec,cw_kgm3,r_gas,r_aer,cph,
     &               idiag,iout,igrdchm,ichm,jchm,kchm)
c
        con(kso2)  = amax1(r_gas(1) *1.e6,bdnl(kso2))     ! SO2 (ppm)
        con(khno3) = amax1(r_gas(2) *1.e6,bdnl(khno3))    ! HNO3 (ppm)
        con(kn2o5) = amax1(r_gas(3) *1.e6,bdnl(kn2o5))    ! N2O5 gas (ppm)
        con(knh3)  = amax1(r_gas(5) *1.e6,bdnl(knh3))     ! NH3 (ppm)
        con(khpo_c)= amax1(r_gas(6) *1.e6,bdnl(khpo_c))   ! H2O2 (ppm)
        con(ko3)   = amax1(r_gas(7) *1.e6,bdnl(ko3))      ! O3 (ppm)
        con(kfoa_c)= amax1(r_gas(8) *1.e6,bdnl(kfoa_c))
        con(kmhp_c)= amax1(r_gas(9) *1.e6*r_1,bdnl(kmhp_c))
        con(kohp_c)= amax1(r_gas(9) *1.e6*(1.-r_1),bdnl(kohp_c))
        con(kpaa_c)= amax1(r_gas(10)*1.e6*r_2,bdnl(kpaa_c))
        con(kopa_c)= amax1(r_gas(10)*1.e6*(1.-r_2),bdnl(kopa_c))
        con(ksulf) = amax1(r_gas(11)*1.e6,bdnl(ksulf))    ! H2SO4 (ppm)
c
        con(kpso4) = amax1(r_aer(1)*convfac*96.*1.e6,bdnl(kpso4)) ! PSO4 (ug/m3)
        con(kpnh4) = amax1(r_aer(2)*convfac*18.*1.e6,bdnl(kpnh4)) ! PNH4 (ug/m3)
        con(kpno3) = amax1(r_aer(3)*convfac*62.*1.e6,bdnl(kpno3)) ! PNO3 (ug/m3)

        endif ! check high acid conditions
c
        con(nspec+1) = 0.
c
c========================= Process Analysis Begin ======================
c
        if ( ldoipr ) then
          cipr(IPR_AQCHEM,ipa_cel,kso2 )=cipr(IPR_AQCHEM,ipa_cel,kso2 )
     &                                +(con(kso2 )-cold(kso2 ))*convfac
          cipr(IPR_AQCHEM,ipa_cel,khno3)=cipr(IPR_AQCHEM,ipa_cel,khno3)
     &                                +(con(khno3)-cold(khno3))*convfac
          cipr(IPR_AQCHEM,ipa_cel,kn2o5)=cipr(IPR_AQCHEM,ipa_cel,kn2o5)
     &                                +(con(kn2o5)-cold(kn2o5))*convfac
          cipr(IPR_AQCHEM,ipa_cel,knh3 )=cipr(IPR_AQCHEM,ipa_cel,knh3 )
     &                                +(con(knh3 )-cold(knh3 ))*convfac
          cipr(IPR_AQCHEM,ipa_cel,khpo_c)
     &                                 =cipr(IPR_AQCHEM,ipa_cel,khpo_c)
     &                              +(con(khpo_c)-cold(khpo_c))*convfac
          cipr(IPR_AQCHEM,ipa_cel,ko3  )=cipr(IPR_AQCHEM,ipa_cel,ko3  )
     &                                +(con(ko3  )-cold(ko3  ))*convfac
          if (kfoa_c.lt.nspec+1) cipr(IPR_AQCHEM,ipa_cel,kfoa_c)
     &                                 =cipr(IPR_AQCHEM,ipa_cel,kfoa_c)
     &                              +(con(kfoa_c)-cold(kfoa_c))*convfac
          if (kmhp_c.lt.nspec+1) cipr(IPR_AQCHEM,ipa_cel,kmhp_c)
     &                                 =cipr(IPR_AQCHEM,ipa_cel,kmhp_c)
     &                              +(con(kmhp_c)-cold(kmhp_c))*convfac
          if (kohp_c.lt.nspec+1) cipr(IPR_AQCHEM,ipa_cel,kohp_c)
     &                                 =cipr(IPR_AQCHEM,ipa_cel,kohp_c)
     &                              +(con(kohp_c)-cold(kohp_c))*convfac
          if (kpaa_c.lt.nspec+1) cipr(IPR_AQCHEM,ipa_cel,kpaa_c)
     &                                 =cipr(IPR_AQCHEM,ipa_cel,kpaa_c)
     &                              +(con(kpaa_c)-cold(kpaa_c))*convfac
          if (kopa_c.lt.nspec+1) cipr(IPR_AQCHEM,ipa_cel,kopa_c)
     &                                 =cipr(IPR_AQCHEM,ipa_cel,kopa_c)
     &                              +(con(kopa_c)-cold(kopa_c))*convfac
          cipr(IPR_AQCHEM,ipa_cel,ksulf)=cipr(IPR_AQCHEM,ipa_cel,ksulf)
     &                                +(con(ksulf)-cold(ksulf))*convfac
          cipr(IPR_AQCHEM,ipa_cel,kpso4)=cipr(IPR_AQCHEM,ipa_cel,kpso4)
     &                                  +con(kpso4)-cold(kpso4)
          cipr(IPR_AQCHEM,ipa_cel,kpnh4)=cipr(IPR_AQCHEM,ipa_cel,kpnh4)
     &                                  +con(kpnh4)-cold(kpnh4)
          cipr(IPR_AQCHEM,ipa_cel,kpno3)=cipr(IPR_AQCHEM,ipa_cel,kpno3)
     &                                  +con(kpno3)-cold(kpno3)
          csulf = con(ksulf)
          chno3 = con(khno3)
          cnh3  = con(knh3 )
          cpso4 = con(kpso4)
          cpno3 = con(kpno3)
          cpnh4 = con(kpnh4)
        endif
c
c========================== Process Analysis End =======================
c
        isrpia_flag = 1
        if (pig_flag) isrpia_flag = 0
      endif ! call RADM-AQ?
c
c======================== Source Apportion Begin =======================
c
      if( ltrace ) then
         do ispc=1,ngas
           delcon(2,ispc) = delcon(2,ispc) +
     &                     AMAX1(bdnl(ispc),con(ispc))*convfac
         enddo
         do ispc=ngas+1,nspec
           delcon(2,ispc) = delcon(2,ispc) +
     &                               AMAX1(bdnl(ispc),con(ispc))
         enddo
         deln2o5 = delcon(2,kn2o5) - delcon(1,kn2o5)
         delcon(4,khno3) = delcon(4,khno3) - deln2o5 * 2.0
         delcon(4,kn2o5) = delcon(4,kn2o5) + deln2o5
         if (aero_flag.eq.2) then
           delcon(5,ksoa1) = -apoly(1)
           delcon(5,ksoa2) = -apoly(2)
           delcon(5,ksoa3) = -apoly(3)
           delcon(5,ksoa4) = -apoly(4)
           delcon(5,ksoa5) = -apoly(5)
           delcon(5,ksoa6) = -apoly(6)
           delcon(5,ksoa7) = -apoly(7)
         endif
      endif
c
c======================== Source Apportion End =======================
c
c
c-----Inorganic aerosol equilibrium chemistry with ISOROPIA
c     Convert conc units to mol/m3 (double precision)
c
      if (isrpia_flag.eq.1) then
        rhi = rh/100.
        tempi = tempk
        cntrl(1) = 0.d0                   ! 0 = forward problem
        cntrl(2) = 0.d0                   ! 0 = solids and liquid allowed
        if (kna.eq.nspec+1) then
          wi(1) = nacl/58.*1.e-6          ! total sodium
          wi(5) = wi(1)                   ! total chloride
        else
          wi(1) = con(kna)/23.*1.e-6      ! total sodium
          wi(5) = (con(khcl)*convfac + con(kpcl)/35.)*1.e-6 ! total chloride
        endif
        wi(2) = (con(ksulf)*convfac + con(kpso4)/96.)*1.e-6 ! total sulfate
        wi(3) = (con(knh3) *convfac + con(kpnh4)/18.)*1.e-6 ! total ammonium
        wi(4) = (con(khno3)*convfac + con(kpno3)/62.)*1.e-6 ! total nitrate

        dno3 = dmin1(DBLE(fCaCO3*con(kFCRS)/100.*1.e-6), wi(4))
        wi(4) = wi(4) - dno3 ! exclude mineral dust nitrate
c
        wt_save(1) = wi(1)
        wt_save(2) = wi(2)
        wt_save(3) = wi(3)
        wt_save(4) = wi(4)
        wt_save(5) = wi(5)

        call isoropia(wi,rhi,tempi,cntrl,wt,gasis,aerliq,aersld,
     &                scasi,other)
c 
c-----Load results back to local CON array (ppm for gas, ug/m3 for aerosol)
c 
        con(ksulf) = bdnl(ksulf)                      ! sulfuric acid gas
        con(kpso4) = (wt_save(2) - con(ksulf)*1.e-6*convfac)*96.*1.e6 ! sulfate
        con(kpso4) = amax1(con(kpso4),bdnl(kpso4))
c
        con(khno3) = gasis(2)*1.e6/convfac            ! nitric acid gas
        con(khno3) = amax1(con(khno3),bdnl(khno3))    
        con(kpno3) = (wt_save(4) - con(khno3)*1.e-6*convfac)*62.*1.e6 ! nitrate
        con(kpno3) = con(kpno3) + SNGL(dno3)*62.*1.e6 ! add mineral dust nitrate
        con(kpno3) = amax1(con(kpno3),bdnl(kpno3))
c
        con(knh3 ) = gasis(1)*1.e6/convfac            ! ammonia gas
        con(knh3 ) = amax1(con(knh3),bdnl(knh3))    
        con(kpnh4) = (wt_save(3) - con(knh3)*1.e-6*convfac)*18.*1.e6 ! ammonium
        con(kpnh4) = amax1(con(kpnh4),bdnl(kpnh4))
c
        if (kna.ne.nspec+1) then
          con(kna)  = amax1(con(kna),bdnl(kna))       ! sodium aerosol
          con(khcl) = gasis(3)*1.e6/convfac           ! HCl gas
          con(khcl) = amax1(con(khcl),bdnl(khcl))
          con(kpcl) = (wt_save(5) - con(khcl)*1.e-6*convfac)*35.*1.e6 ! chloride
          con(kpcl) = amax1(con(kpcl),bdnl(kpcl))
        endif
c
        con(kph2o) = aerliq(8)*18.*1.e6
        con(kph2o) = amax1( con(kph2o),bdnl(kph2o) ) ! aerosol water
c
c========================= Process Analysis Begin ======================
c
        if ( ldoipr ) then
          cipr(IPR_IAERO,ipa_cel,ksulf) = cipr(IPR_IAERO,ipa_cel,ksulf)
     &                                  + (con(ksulf)-csulf)*convfac
          cipr(IPR_IAERO,ipa_cel,khno3) = cipr(IPR_IAERO,ipa_cel,khno3)
     &                                  + (con(khno3)-chno3)*convfac
          cipr(IPR_IAERO,ipa_cel,knh3 ) = cipr(IPR_IAERO,ipa_cel,knh3 )
     &                                  + (con(knh3 )-cnh3 )*convfac
          cipr(IPR_IAERO,ipa_cel,kpso4) = cipr(IPR_IAERO,ipa_cel,kpso4)
     &                                  + con(kpso4)-cpso4
          cipr(IPR_IAERO,ipa_cel,kpno3) = cipr(IPR_IAERO,ipa_cel,kpno3)
     &                                  + con(kpno3)-cpno3
          cipr(IPR_IAERO,ipa_cel,kpnh4) = cipr(IPR_IAERO,ipa_cel,kpnh4)
     &                                  + con(kpnh4)-cpnh4
          if (kna.ne.nspec+1) then
            cipr(IPR_IAERO,ipa_cel,khcl) = cipr(IPR_IAERO,ipa_cel,khcl)
     &                                 + (con(khcl)-cold(khcl))*convfac
            cipr(IPR_IAERO,ipa_cel,kpcl) = cipr(IPR_IAERO,ipa_cel,kpcl)
     &                                   + con(kpcl)-cold(kpcl)
          endif
          cipr(IPR_IAERO,ipa_cel,kph2o) = cipr(IPR_IAERO,ipa_cel,kph2o)
     &                                  + con(kph2o)-cold(kph2o)
        endif
c
c========================== Process Analysis End =======================
c
      endif ! call ISORROPIA?
c
      return
c
      end
