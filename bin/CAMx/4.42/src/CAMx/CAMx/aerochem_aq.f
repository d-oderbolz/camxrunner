      subroutine aerochem_aq(mxspec_c,nspec_c,ngas_c,aermod,dtaq,h2o,
     &                       tempk,press,cwc,cph,con,bndlow,convfac,
     &                       ltrace,delcon,ldoipr,ipa_cel)
c
c----CAMx v4.42 070603
c
c     AEROCHEM_AQ drives the aqueous chemistry for both the CF and CMU
c     aerosol options. It performs the following steps:
c        1. Calls RADM_AQ to calculate the aqueous change in bulk
c           (total over all sizes) inorganic species (PSO4,PNO3,PNH4,PCl).
c        2. Calls ISORROPIA to re-equilibrate the resulting bulk inorganics
c           and to determine bulk particulate water (PH2O).
c        3. CMU ONLY: The change in bulk species is assigned to the 
c           default RADM size distribution and added to the original 
c           concentrations in each size bin.
c        4. CMU ONLY: Bulk PH2O is distributed according to the new
c           PSO4 distribution
c           
c     The gas species are in ppm
c     The aerosol species are in (ug/m3)
c
c     Copyright 2006-2007
c     ENVIRON International Corporation
c
c     Modifications: 
c        08/23/06       Removed metastable option in ISOROPIA
c        05/04/07       Added array (wt_save) to keep original total
c                       mass because ISOROPIA overwrites with lower
c                       bound value.
c        05/30/07       Skip call to RAQCHEM under high acid conditions.
c
c     Input arguments:
c        mxspec_c            max number of total species
c        nspec_c             number of total species
c        ngas_c              number of gas species
c        areomod             aerosol model options (CF or CMU)
c        dtaq                time step (hours)
c        h2o                 cell water vapor (ppm)
c        tempk               cell temperature (K)
c        press               cell pressure (mb)
c        cwc                 cloud water content (g/m3)
c        cph                 cloud water pH 
c        con                 species concentrations (ppm, ug/m3)
c        bndlow              lower bound concentrations (ppm, ug/m3)
c        convfac             conversion factor: umol/m3 = ppm * convfac
c
c     Output arguments:
c        con                 species concentrations (ppm, ug/m3)
c        cph                 cloud water pH 
c
c     Routines called: 
c        GET_PARAM
c        RAQCHEM
c        ISOROPIA
c        AQDIST
c 
c     Called by: 
c        CHEMDRIV 
c
      include 'dynamic.inc'
      include 'aerpar.inc'
      include 'droppar.inc'
      include 'camx_aero.inc'
      include 'camx_aero.com'
      include 'camx.prm'
      include 'procan.com'
c
      integer mxspec_c
c
c======================== Source Apportion Begin =======================
c
      logical ltrace
      real delcon(4,mxspec_c),delnxoy
c
c======================== Source Apportion End =======================
c
c========================= Process Analysis Begin ======================
c
      logical ldoipr
      integer ipa_cel
      real cold(mxspec_c)
      real csulf,chno3,cnh3,cpso4,cpno3,cpnh4
c
c========================== Process Analysis End =======================
c
      character*10 aermod
      integer nspec_c,ngas_c
      real h2o,press,tempk,dtaq,cwc,convfac,cph
c
      integer ispc,n,ksec,r_idx(4)
      real qwatr,ev,es,rha,pres_pa,dt_sec,cw_kgm3,s_neg,s_pos,sumso4
      real bso4,bno3,bnh4,bcl,bh2o,dso4,dnh4,dno3,dcl
      real cut(nsect+1)
      real con(mxspec_c+1),bndlow(mxspec_c+1)
c
c-----Arrays for RADM aqueous chemistry
c
      real r_gas(11),r_aer(9),r_sum(5)
c
c-----Variables for ISOROPIA need double precision
c
      real*8 wi(5),wt(5),wt_save(5)
      real*8 rhi,tempi,cntrl(2)
      real*8 gasis(3),aerliq(12),aersld(9),other(6)
      character*15 scasi
c 
c-----Entry point 
c
      do ispc = 1,nspec_c
        cold(ispc) = con(ispc)
      enddo
      con(nspec_c+1) = 0.
      cph = 5.0
c
c-----Calculate relative humidity
c
      qwatr = 1.e-6*h2o*18./28.8 
      ev = qwatr*press/(qwatr + eps) 
      es = e0*exp((lv/rv)*(1./273. - 1./tempk)) 
      rha = 100.0*amin1(0.99,ev/es)
c
c-----Do RADM aqueous chemistry
c     all conc units must be mol/mol (mixing ratio)
c
      pres_pa = 100.*press
      dt_sec = dtaq*3600.
      cw_kgm3 = cwc/1000.
c
c-----Gas species mapping
c
      r_gas(1)  = con(kso2_c)*1.e-6
      r_gas(2)  = con(khno3_c)*1.e-6
      r_gas(3)  = con(knxoy_c)*0.5*1.e-6
      r_gas(4)  = co2*1.e-6
      r_gas(5)  = con(knh3_c)*1.e-6
      r_gas(6)  = con(kh2o2_c)*1.e-6
      r_gas(7)  = con(ko3_c)*1.e-6
      r_gas(8)  = foa*1.e-6
      r_gas(9)  = mhp*1.e-6
      r_gas(10) = paa*1.e-6
      r_gas(11) = con(kh2so4_c)*1.e-6
c
c-----Aerosol species mapping
c
      if (aermod.EQ.'CMU') then
        do n = 1,5
          r_sum(n) = 0.0
        enddo
        do ksec = 1,nsect
          r_sum(1) = r_sum(1) + con(kpso4_c+(ksec-1))
          r_sum(2) = r_sum(2) + con(kpnh4_c+(ksec-1))
          r_sum(3) = r_sum(3) + con(kpno3_c+(ksec-1))
          r_sum(4) = r_sum(4) + con(kna_c  +(ksec-1))
          r_sum(5) = r_sum(5) + con(kpcl_c +(ksec-1))
        enddo
        r_aer(1)  = (r_sum(1)/96./convfac)*1.e-6
        r_aer(2)  = (r_sum(2)/18./convfac)*1.e-6
        r_aer(3)  = (r_sum(3)/62./convfac)*1.e-6
        if (r_sum(4).gt.r_sum(5)) then
          r_aer(6) = (r_sum(5)/35./convfac)*1.e-6
        else
          r_aer(6) = (r_sum(4)/23./convfac)*1.e-6
        endif
      else
        r_aer(1)  = (con(kpso4_c)/96./convfac)*1.e-6
        r_aer(2)  = (con(kpnh4_c)/18./convfac)*1.e-6
        r_aer(3)  = (con(kpno3_c)/62./convfac)*1.e-6
        if (kna_c.eq.nspec_c+1) then
          r_aer(6) = (nacl/58./convfac)*1.e-6
        else
          if (con(kna_c).gt.con(kpcl_c)) then
            r_aer(6) = (con(kpcl_c)/35./convfac)*1.e-6
          else
            r_aer(6) = (con(kna_c)/23./convfac)*1.e-6
          endif
        endif
      endif
      r_aer(4)  = (caco3/100./convfac)*1.e-6
      r_aer(5)  = (mgco3/84./convfac)*1.e-6
      r_aer(7)  = (a3fe/56./convfac)*1.e-6
      r_aer(8)  = (b2mn/55./convfac)*1.e-6
      r_aer(9)  = (potcl/74./convfac)*1.e-6
c
      call get_param(igrdchm_c,ichm_c,jchm_c,kchm_c,iout_c,idiag_c)
      if (r_aer(1).lt.5.0e-7 .and. r_aer(3).lt.1.5e-6) then
        call raqchem(tempk,pres_pa,dt_sec,cw_kgm3,r_gas,r_aer,cph,
     &               idiag_c,iout_c,igrdchm_c,ichm_c,jchm_c,kchm_c)
      endif
c
      con(kso2_c)   = amax1(r_gas(1) *1.e6,bndlow(kso2_c))     ! SO2 (ppm)
      con(khno3_c)  = amax1(r_gas(2) *1.e6,bndlow(khno3_c))    ! HNO3 (ppm)
      con(knxoy_c)  = amax1(r_gas(3) *2.*1.e6,bndlow(knxoy_c)) ! N2O5 gas (ppm)
      con(knh3_c)   = amax1(r_gas(5) *1.e6,bndlow(knh3_c))     ! NH3 (ppm)
      con(kh2o2_c)  = amax1(r_gas(6) *1.e6,bndlow(kh2o2_c))    ! H2O2 (ppm)
      con(ko3_c)    = amax1(r_gas(7) *1.e6,bndlow(ko3_c))      ! O3 (ppm)
      con(kh2so4_c) = amax1(r_gas(11)*1.e6,bndlow(kh2so4_c))   ! H2SO4 (ppm)

      if (aermod.EQ.'CMU') then
        r_sum(1) = amax1(r_aer(1)*convfac*96.*1.e6,0.0) ! PSO4 (ug/m3)
        r_sum(2) = amax1(r_aer(2)*convfac*18.*1.e6,0.0) ! PNH4 (ug/m3)
        r_sum(3) = amax1(r_aer(3)*convfac*62.*1.e6,0.0) ! PNO3 (ug/m3)
      else
        con(kpso4_c) = amax1(r_aer(1)*convfac*96.*1.e6,bndlow(kpso4_c)) ! PSO4 (ug/m3)
        con(kpnh4_c) = amax1(r_aer(2)*convfac*18.*1.e6,bndlow(kpnh4_c)) ! PNH4 (ug/m3)
        con(kpno3_c) = amax1(r_aer(3)*convfac*62.*1.e6,bndlow(kpno3_c)) ! PNO3 (ug/m3)
c
c========================= Process Analysis Begin ======================
c
        if ( ldoipr ) then
          cipr(IPR_AQCHEM,ipa_cel,kso2_c  ) =
     &    cipr(IPR_AQCHEM,ipa_cel,kso2_c  ) +
     &                           (con(kso2_c  )-cold(kso2_c  ))*convfac
          cipr(IPR_AQCHEM,ipa_cel,khno3_c ) = 
     &    cipr(IPR_AQCHEM,ipa_cel,khno3_c ) +
     &                           (con(khno3_c )-cold(khno3_c ))*convfac
          cipr(IPR_AQCHEM,ipa_cel,knxoy_c ) = 
     &    cipr(IPR_AQCHEM,ipa_cel,knxoy_c ) +
     &                           (con(knxoy_c )-cold(knxoy_c ))*convfac
          cipr(IPR_AQCHEM,ipa_cel,knh3_c  ) = 
     &    cipr(IPR_AQCHEM,ipa_cel,knh3_c  ) +
     &                           (con(knh3_c  )-cold(knh3_c  ))*convfac
          cipr(IPR_AQCHEM,ipa_cel,kh2o2_c ) = 
     &    cipr(IPR_AQCHEM,ipa_cel,kh2o2_c ) +
     &                           (con(kh2o2_c )-cold(kh2o2_c ))*convfac
          cipr(IPR_AQCHEM,ipa_cel,ko3_c   ) = 
     &    cipr(IPR_AQCHEM,ipa_cel,ko3_c   ) +
     &                           (con(ko3_c   )-cold(ko3_c   ))*convfac
          cipr(IPR_AQCHEM,ipa_cel,kh2so4_c) = 
     &    cipr(IPR_AQCHEM,ipa_cel,kh2so4_c) +
     &                           (con(kh2so4_c)-cold(kh2so4_c))*convfac
          cipr(IPR_AQCHEM,ipa_cel,kpso4_c ) = 
     &    cipr(IPR_AQCHEM,ipa_cel,kpso4_c ) +
     &                            con(kpso4_c )-cold(kpso4_c )
          cipr(IPR_AQCHEM,ipa_cel,kpnh4_c ) = 
     &    cipr(IPR_AQCHEM,ipa_cel,kpnh4_c ) +
     &                            con(kpnh4_c )-cold(kpnh4_c )
          cipr(IPR_AQCHEM,ipa_cel,kpno3_c ) = 
     &    cipr(IPR_AQCHEM,ipa_cel,kpno3_c ) +
     &                            con(kpno3_c )-cold(kpno3_c )
          csulf = con(kh2so4_c)
          chno3 = con(khno3_c )
          cnh3  = con(knh3_c  )
          cpso4 = con(kpso4_c )
          cpno3 = con(kpno3_c )
          cpnh4 = con(kpnh4_c )
        endif
c
c========================== Process Analysis End =======================
c
      endif
c
c======================== Source Apportion Begin =======================
c
      if( ltrace .AND. aermod.EQ.'CF' ) then
         do ispc=1,ngas_c
           delcon(2,ispc) = delcon(2,ispc) +
     &                     AMAX1(bndlow(ispc),con(ispc))*convfac
         enddo
         do ispc=ngas_c+1,nspec_c
           delcon(2,ispc) = delcon(2,ispc) +
     &                               AMAX1(bndlow(ispc),con(ispc))
         enddo
         delnxoy = delcon(2,knxoy_c) - delcon(1,knxoy_c)
         delcon(4,khno3_c) = delcon(4,khno3_c) - delnxoy
         delcon(4,knxoy_c) = delcon(4,knxoy_c) + delnxoy
      endif
c
c======================== Source Apportion End =======================
c
c
c-----Inorganic aerosol equilibrium chemistry with ISOROPIA
c     convert conc units to mol/m3 (double precision)
c
      rhi = amin1( rha/100.,0.994 )
      tempi = tempk
      cntrl(1) = 0.d0                  ! 0 = forward problem
      cntrl(2) = 0.d0                  ! 0 = solids and liquid allowed
      if (aermod.eq.'CMU') then
        wi(1) =                          r_sum(4)/23. *1.e-6 ! total sodium
        wi(2) = (con(kh2so4_c)*convfac + r_sum(1)/96.)*1.e-6 ! total sulfate
        wi(3) = (con(knh3_c)  *convfac + r_sum(2)/18.)*1.e-6 ! total ammonium
        wi(4) = (con(khno3_c) *convfac + r_sum(3)/62.)*1.e-6 ! total nitrate
        wi(5) = (con(khcl_c)  *convfac + r_sum(5)/35.)*1.e-6 ! total chloride
      else
        if (kna_c.eq.nspec_c+1) then
          wi(1) = nacl/58.*1.e-6       ! total sodium
          wi(5) = wi(1)                ! total chloride
        else
          wi(1) = con(kna_c)/23.*1.e-6 ! total sodium
          wi(5) = (con(khcl_c)*convfac + con(kpcl_c)/35.)*1.e-6  ! total chloride
        endif
        wi(2) = (con(kh2so4_c)*convfac + con(kpso4_c)/96.)*1.e-6 ! total sulfate
        wi(3) = (con(knh3_c)  *convfac + con(kpnh4_c)/18.)*1.e-6 ! total ammonium
        wi(4) = (con(khno3_c) *convfac + con(kpno3_c)/62.)*1.e-6 ! total nitrate
      endif
c
      wt_save(1) = wi(1)
      wt_save(2) = wi(2)
      wt_save(3) = wi(3)
      wt_save(4) = wi(4)
      wt_save(5) = wi(5)
      if (r_aer(1).lt.5.0e-7 .and. r_aer(3).lt.1.5e-6) then
        call isoropia(wi,rhi,tempi,cntrl,wt,gasis,aerliq,aersld,
     &                scasi,other)
      endif
c 
c-----Load gasses back to local CON array (ppm) and calculate bulk 
c     inorganic aerosols (ug/m3)
c 
      con(kh2so4_c) = bndlow(kh2so4_c)
      bso4       = (wt_save(2) - con(kh2so4_c)*1.e-6*convfac)*96.*1.e6
c
      con(khno3_c) = gasis(2)*1.e6/convfac
      con(khno3_c) = amax1(con(khno3_c),bndlow(khno3_c))    
      bno3       = (wt_save(4) - con(khno3_c)*1.e-6*convfac)*62.*1.e6
c
      con(knh3_c)  = gasis(1)*1.e6/convfac
      con(knh3_c)  = amax1(con(knh3_c),bndlow(knh3_c))    
      bnh4       = (wt_save(3) - con(knh3_c)*1.e-6*convfac)*18.*1.e6
c
      if (kna_c.ne.nspec_c+1) then
        con(kna_c)  = amax1(con(kna_c),bndlow(kna_c))
        con(khcl_c) = gasis(3)*1.e6/convfac
        con(khcl_c) = amax1(con(khcl_c),bndlow(khcl_c))
        bcl      = (wt_save(5) - con(khcl_c)*1.e-6*convfac)*35.*1.e6
      endif
c
      bh2o       = aerliq(8)*18.*1.e6
c
c-----CF option: load aerosols back into CON array
c
      if (aermod.EQ.'CF') then
        con(kpso4_c) = amax1(bso4,bndlow(kpso4_c))
        con(kpno3_c) = amax1(bno3,bndlow(kpno3_c))
        con(kpnh4_c) = amax1(bnh4,bndlow(kpnh4_c))
        if (kna_c.ne.nspec_c+1) then
          con(kpcl_c) = amax1(bcl,bndlow(kpcl_c))
        endif
        con(kph2o_c) = amax1(bh2o,bndlow(kph2o_c))
c
c========================= Process Analysis Begin ======================
c
        if ( ldoipr ) then
          cipr(IPR_IAERO,ipa_cel,kh2so4_c) = 
     &    cipr(IPR_IAERO,ipa_cel,kh2so4_c) +
     &                                    (con(kh2so4_c)-csulf)*convfac
          cipr(IPR_IAERO,ipa_cel,khno3_c ) = 
     &    cipr(IPR_IAERO,ipa_cel,khno3_c ) +
     &                                    (con(khno3_c )-chno3)*convfac
          cipr(IPR_IAERO,ipa_cel,knh3_c  ) = 
     &    cipr(IPR_IAERO,ipa_cel,knh3_c  ) +
     &                                    (con(knh3_c  )-cnh3 )*convfac
          cipr(IPR_IAERO,ipa_cel,kpso4_c ) = 
     &    cipr(IPR_IAERO,ipa_cel,kpso4_c ) +
     &                                     con(kpso4_c )-cpso4
          cipr(IPR_IAERO,ipa_cel,kpno3_c ) = 
     &    cipr(IPR_IAERO,ipa_cel,kpno3_c ) +
     &                                     con(kpno3_c )-cpno3
          cipr(IPR_IAERO,ipa_cel,kpnh4_c ) = 
     &    cipr(IPR_IAERO,ipa_cel,kpnh4_c ) +
     &                                     con(kpnh4_c )-cpnh4
          if (kna_c.ne.nspec_c+1) then
            cipr(IPR_IAERO,ipa_cel,khcl_c) = 
     &      cipr(IPR_IAERO,ipa_cel,khcl_c) +
     &                               (con(khcl_c)-cold(khcl_c))*convfac
            cipr(IPR_IAERO,ipa_cel,kpcl_c) = 
     &      cipr(IPR_IAERO,ipa_cel,kpcl_c) +
     &                                con(kpcl_c)-cold(kpcl_c)
          endif
          cipr(IPR_IAERO,ipa_cel,kph2o_c ) = 
     &    cipr(IPR_IAERO,ipa_cel,kph2o_c ) +
     &                                con(kph2o_c)-cold(kph2o_c)
        endif
c
c========================== Process Analysis End =======================
c
        return
      endif
c
c-----CMU Option: distribute processed bulk inorganics to size segments
c
c-----Determine change in bulk aerosols due to aqueous chem + ISORROPIA
c
      dso4 = amax1(bso4,0.0) - r_sum(1)
      dnh4 = amax1(bnh4,0.0) - r_sum(2)
      dno3 = amax1(bno3,0.0) - r_sum(3)
      dcl  = amax1(bcl, 0.0) - r_sum(5)
c
c-----Calculate fdist
c
      do ksec = 1,nsect+1
        cut(ksec) = SNGL(dsecf(ksec))
      enddo
      call aqdist(nsect,cut,fdist,fdist2)
c
c-----Add the change to the corresponding section
c
      do ksec = 1,nsect
        con(kpso4_c+(ksec-1)) = con(kpso4_c+(ksec-1)) + dso4*fdist(ksec)
        con(kpnh4_c+(ksec-1)) = con(kpnh4_c+(ksec-1)) + dnh4*fdist(ksec)
        con(kpno3_c+(ksec-1)) = con(kpno3_c+(ksec-1)) + dno3*fdist(ksec)
        con(kpcl_c +(ksec-1)) = con(kpcl_c +(ksec-1)) + dcl *fdist(ksec)
      enddo
c
c-----Adjust mass to avoid negative concentration sections
c
      r_idx(1) = kpso4_c
      r_idx(2) = kpnh4_c
      r_idx(3) = kpno3_c
      r_idx(4) = kpcl_c
      do n = 1,4
        s_neg = 0.0
        s_pos = 0.0
        do ksec = 1,nsect
          if (con(r_idx(n)+(ksec-1)) .lt. 0.0) then
            s_neg = s_neg - con(r_idx(n)+(ksec-1))
            con(r_idx(n)+(ksec-1)) = 0.0
          elseif (con(r_idx(n)+(ksec-1)) .gt. 1.e-12) then
            s_pos = s_pos + con(r_idx(n)+(ksec-1))
          endif
        enddo
        do ksec = 1,nsect
          if (con(r_idx(n)+(ksec-1)) .gt. 1.e-12)
     &      con(r_idx(n)+(ksec-1)) =
     &            amax1((1.-s_neg/s_pos)*con(r_idx(n)+(ksec-1)),0.)
        enddo
      enddo
c
c-----Assign new aerosol water to the new total sulfate distribution
c
      sumso4 = 0.
      do ksec = 1,nsect
        sumso4 = sumso4 + con(kpso4_c+(ksec-1))
      enddo
      do ksec = 1,nsect
        con(kph2o_c+(ksec-1)) = bh2o*con(kpso4_c+(ksec-1))/sumso4
      enddo
c
      return
c
      end
