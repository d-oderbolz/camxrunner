      subroutine aerochem_cmu(water,tempk,press,lwc_c,cph,mxspec_c,
     &                        con,bdnl,convfac,t00,dtaq,
     &                        dtaer,aero_flag)
c
c----CAMx v5.41 121109
c
c     AEROCHEM_CMU drives the CMU multi-section aerosol modules for CAMx
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Modifications:
c        01/30/2002 (tmg); do not set prevsp to true because each grid cell
c                          will be different
c        03/22/2002 (tmg); do not update inorganic gases after soap module
c                          because they are not changed inside it
c        04/15/2002 (tmg); changed molecular weights in conversion of 
c                          organic gases
c        11/25/2002 (tmg); added include file other.inc to track whether
c                          aerosol or aqueous module is called
c        12/05/2002 (tmg); added countcell to track which grid cell is
c                          current (for debugging purposes)
c        03/09/03 (bkoo)   removed code related with lsoap
c                          (SOAP has been merged with inorganic aerosol module)
c        12/26/2006 (bkoo) AQ-CHEM now called each chemistry timestep
c        12/29/2006 (bkoo) added species mapping for the updated SOA scheme
c        01/10/2007 (bkoo) Added species mapping for CB05/SAPRC99
c        05/30/2007 (cae)  Skip call to RAQCHEM under high acid conditions.
c
c     Input arguments:
c        water               cell water vapor (ppm)
c        tempk               cell temperature (K)
c        press               cell pressure (mb)
c        lwc_c               cloud water content (g/m3)
c        cph                 cloud water pH
c        mxspec_c            Max number of species
c        con                 species concentrations (ppm, ug/m3)
c        bdnl                lower bound concentrations (ppm, ug/m3)
c        convfac             conversion factor: umol/m3 = ppm * convfac
c        t00                 model time (HHMM)
c        dtaq                aqueous chemistry time step (hours)
c        dtaer               general PM chemistry time step (hours)
c        aero_flag           1 - time for AQ; 2 - time for general PM
c
c     Output arguments:
c        con                 species concentrations (ppm, ug/m3)
c
c     Routines called: 
c        GET_PARAM
c        RAQCHEM
c        AQDIST
c        AQCHEM
c        WDIAMETER
c        EQPARTO
c        DDIAMETER
c        NEWDIST
c        STEP
c        AERCHEM
c 
c     Called by: 
c        CHEMDRIV 
c 
      include 'dynamic.inc'
      include 'camx.prm'
      include 'aerpar.inc'
      include 'droppar.inc'
      include 'camx_aero.inc'
      include 'camx_aero_cmu.inc'
c
      real*4 con(mxspec_c+1)
      real*4 bdnl(mxspec_c+1)
      real*4 lwc_c,tempk,ev,es,water,press,convfac,t00
      real*4 dtaq,dtaer,hour,tmin,t1_min,dt_min
      real*4 prs,qwatr,rhumid
      real*4 gas(ngas_aq), aerosol(nsect,naers)
      real*8 t0,t1
      real*8 q(ntotal)
      integer modeaero,aero_flag
c
c-----Variables for RADM
c
      real  r_gas(11),r_aer(9)
      real  pres_pa,dt_sec,cw_kgm3
      real  r_sum(5),cut(nsect+1)
      real  dnit,dchlo,dammo,dsulf
      real  totpcl,totph2o,totpso4
      real  s_neg,s_pos
      real  cph
      real  r_1,r_2
      integer r_idx(4)
c
c-----Variables for ISORROPIA (for adjustment after RADM)
c
      real*8 wi(5),wt(5)
      real*8 cntrl(2)
      real*8 gasis(3),aerliq(12),aersld(9),other(6)
      character*15 scasi
c
c-----Entry point 
c
      modeaero = 0
c
c-----Pass some variables to aerosol module common blocks
c
      temp = tempk
      pres = dble(press/1013.25)
      prs  = press/1013.25
c
      hour = aint(t00/100.)
      tmin = t00 - 100.*hour
      if (tmin.ge.59.99 .and. tmin.le.60.01) tmin = 60.
      if (tmin.ge.60.0) then
        tmin = tmin - 60.
        hour = hour + 1.
      endif
c
      t1_min = 60.*hour + tmin ! end time (min) for AQCHEM
      dt_min = dtaq*60.        ! delta t (min) for AQCHEM
      t1 = DBLE(t1_min*60.)    ! end time (sec) for AEROCHEM
      dt = DBLE(dtaer*3600.)   ! delta t (sec) for AEROCHEM
      t0 = t1 - dt             ! start time (sec) for AEROCHEM
      tcom = t0                ! common t (sec) for AEROCHEM
c
c-----Calculate RH
c
      qwatr = 1.e-6*water*18./28.8
      ev = qwatr*press/(qwatr+eps)
      es = e0*exp((lv/rv)*(1./273.-1./tempk))
      rhumid = amin1(0.99,ev/es)
      rh = rhumid
c
      if (lfrst) then
        do i = 1,nsecp1
  	  dsec(i) = dsec_c(i)
	  dsecf(i) = dsecf_c(i)
        enddo
      endif
c
      if (lwc_c.ge.aqcwmin .and. tempk.ge.aqtamin) then
        modeaero = 1 ! flag to indicate aqueous chem module is called
c
c-----RADM aqueous chemistry
c
        if ( chaq.eq.'RADM' ) then
          pres_pa = 100.*press
          dt_sec  = dtaq*3600.
          cw_kgm3 = lwc_c/1000.
c
c-----Gas species mapping
c
          r_gas(1)  = con(kso2_c)*1.e-6
          r_gas(2)  = con(khno3_c)*1.e-6
          r_gas(3)  = con(kn2o5_c)*1.e-6
          r_gas(4)  = co2*1.e-6
          r_gas(5)  = con(knh3_c)*1.e-6
          r_gas(6)  = con(khpo_c)*1.e-6
          r_gas(7)  = con(ko3_c)*1.e-6
          if (kfoa_c.lt.nspec_c+1) then
            r_gas(8) = con(kfoa_c)*1.e-6
          else
            r_gas(8) = foa*1.e-6
          endif
          if (kmhp_c.lt.nspec_c+1 .or. kohp_c.lt.nspec_c+1) then
            r_1 = con(kmhp_c) + con(kohp_c)
            r_gas(9) = r_1*1.e-6
            r_1 = con(kmhp_c) / r_1
          else
            r_gas(9) = mhp*1.e-6
          endif
          if (kpaa_c.lt.nspec_c+1 .or. kopa_c.lt.nspec_c+1) then
            r_2 = con(kpaa_c) + con(kopa_c)
            r_gas(10) = r_2*1.e-6
            r_2 = con(kpaa_c) / r_2
          else
            r_gas(10) = paa*1.e-6
          endif
          r_gas(11) = con(kh2so4_c)*1.e-6
c
c-----Aerosol species mapping
c
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
          totpcl = r_sum(5)
          r_aer(1)  = (r_sum(1)/96./convfac)*1.e-6
          r_aer(2)  = (r_sum(2)/18./convfac)*1.e-6
          r_aer(3)  = (r_sum(3)/62./convfac)*1.e-6
          r_aer(4)  = (caco3/100./convfac)*1.e-6
          r_aer(5)  = (mgco3/84./convfac)*1.e-6
          if (r_sum(4).gt.r_sum(5)) then
            r_aer(6) = (r_sum(5)/35./convfac)*1.e-6
          else
            r_aer(6) = (r_sum(4)/23./convfac)*1.e-6
          endif
          r_aer(7)  = (a3fe/56./convfac)*1.e-6
          r_aer(8)  = (b2mn/55./convfac)*1.e-6
          r_aer(9)  = (potcl/74./convfac)*1.e-6
c
c-----Call RADM
c
          if (r_aer(1).lt.5.0e-7 .and. r_aer(3).lt.1.5e-6) then ! check high acid conditions

          call get_param(igrdchm,ichm,jchm,kchm,iout,idiag)
          call raqchem(tempk,pres_pa,dt_sec,cw_kgm3,r_gas,r_aer,
     &                 cph,idiag,iout,igrdchm,ichm,jchm,kchm)
c
c-----Call ISORROPIA for ammonium/nitrate/chloride/water if AQ only
c     Since RADM-AQ puts all H2SO4 into aerosol phase ISORROPIA won't change
c     H2SO4/sulfate partitioning
c
          if ( aero_flag.eq.1 ) then
            cntrl(1) = 0.d0 ! forward problem
            cntrl(2) = 0.d0 ! solids and liquid allowed

            wi(1) =                        r_sum(4)/23.*1.e-6  ! total sodium
            wi(2) = (r_gas(11) + r_aer(1))*convfac             ! total sulfate
            wi(3) = (r_gas(5)  + r_aer(2))*convfac             ! total ammonium
            wi(4) = (r_gas(2)  + r_aer(3))*convfac             ! total nitrate
            wi(5) = (con(khcl_c)*convfac + r_sum(5)/35.)*1.e-6 ! total chloride

            call isoropia(wi,rh,temp,cntrl,wt,gasis,aerliq,aersld,
     &                    scasi,other)

            r_gas(5) =        gasis(1) /convfac ! NH3 (mol/mol)
            r_aer(2) = (wt(3)-gasis(1))/convfac ! ammonium (mol/mol)
            r_gas(2) =        gasis(2) /convfac ! HNO3 (mol/mol)
            r_aer(3) = (wt(4)-gasis(2))/convfac ! nitrate
            con(khcl_c) = gasis(3)/convfac*1.e6 ! HCL (ppm)
            con(khcl_c) = amax1(con(khcl_c),bdnl(khcl_c))
            totpcl  = (wt(5)-gasis(3))*35.*1.e6 ! chloride (ug/m3)
            totpcl  = amax1(totpcl ,0.0)
            totph2o = aerliq(8)*18.*1.e6        ! aerosol water (ug/m3)
            totph2o = amax1(totph2o,0.0)
            totpso4 = wt(2)*96.*1.e6            ! sulfate (ug/m3)
            totpso4 = amax1(totpso4,1.e-12)
          endif
c
c-----Map gas back to con
c
          con(kso2_c)  = amax1(r_gas(1)*1.e6,bdnl(kso2_c))     ! SO2 (ppm)
          con(khno3_c) = amax1(r_gas(2)*1.e6,bdnl(khno3_c))
          con(kn2o5_c) = amax1(r_gas(3)*1.e6,bdnl(kn2o5_c))    ! N2O5 gas (ppm)
          con(knh3_c)  = amax1(r_gas(5)*1.e6,bdnl(knh3_c))
          con(khpo_c)  = amax1(r_gas(6)*1.e6,bdnl(khpo_c))     ! H2O2 (ppm)
          con(ko3_c)   = amax1(r_gas(7)*1.e6,bdnl(ko3_c))      ! O3 (ppm)
          con(kfoa_c)  = amax1(r_gas(8)*1.e6,bdnl(kfoa_c))
          con(kmhp_c)  = amax1(r_gas(9)*1.e6*r_1,bdnl(kmhp_c))
          con(kohp_c)  = amax1(r_gas(9)*1.e6*(1.-r_1),bdnl(kohp_c))
          con(kpaa_c)  = amax1(r_gas(10)*1.e6*r_2,bdnl(kpaa_c))
          con(kopa_c)  = amax1(r_gas(10)*1.e6*(1.-r_2),bdnl(kopa_c))
          con(kh2so4_c)= amax1(r_gas(11)*1.e6,bdnl(kh2so4_c))
c
c-----Calculate the differences
c
          dsulf = amax1(r_aer(1)*1.e6*convfac*96.,0.0) - r_sum(1)
          dammo = amax1(r_aer(2)*1.e6*convfac*18.,0.0) - r_sum(2)
          dnit  = amax1(r_aer(3)*1.e6*convfac*62.,0.0) - r_sum(3)
          dchlo = totpcl - r_sum(5)
c          write(*,*)'RADM-change: ',dsulf,dnit,dammo
c
c-----Calculate fdist
c
          do ksec = 1,nsect+1
            cut(ksec) = SNGL(dsecf(ksec))
          enddo
          call aqdist(nsect,cut,fdist,fdist2)
c
c-----Add the differences to the corresponding section
c
          do ksec = 1,nsect
           con(kpso4_c+(ksec-1))=con(kpso4_c+(ksec-1))+dsulf*fdist(ksec)
           con(kpnh4_c+(ksec-1))=con(kpnh4_c+(ksec-1))+dammo*fdist(ksec)
           con(kpno3_c+(ksec-1))=con(kpno3_c+(ksec-1))+dnit *fdist(ksec)
           con(kpcl_c +(ksec-1))=con(kpcl_c +(ksec-1))+dchlo*fdist(ksec)
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
     &            con(r_idx(n)+(ksec-1)) =
     &            amax1((1.-s_neg/s_pos)*con(r_idx(n)+(ksec-1)),0.)
            enddo
          enddo
c
c-----Assign new aerosol water to the new total sulfate distribution if AQ only
c
          if ( aero_flag.eq.1 ) then
            do ksec = 1,nsect
              con(kph2o_c+(ksec-1)) = totph2o * con(kpso4_c+(ksec-1))
     &                                               / totpso4
            enddo
            modeaero = 0
          endif

          endif ! check high acid conditions
c
c-----Reset non-existing species concentrations
c
          con(nspec_c+1) = 0.
c
c-----VSRM aqueous chemistry
c
        else
          do k = 1,ngas_aq
            gas(k) = 0.0
          enddo
          do k = 1,naers
            do ks = 1,nsect
              aerosol(ks,k) = 0.0
            enddo
          enddo
          if (con(kh2so4_c) .gt. 0.0) then
            tot_sulf = 0.0
            do k = 1,nsect
              tot_sulf = tot_sulf+con(kpso4_c+(k-1))
            enddo
            if (tot_sulf .gt. 0.0) then
              do k = 1,nsect
                sfac = con(kpso4_c+(k-1))/tot_sulf
                con(kpso4_c+(k-1)) = con(kpso4_c+(k-1)) +
     &                               sfac*con(kh2so4_c)*convfac*96.
              enddo
              con(kh2so4_c) = 0.0
            endif
          endif
c
c-----Map con to gas and aerosol gas in ppm; aerosol in ugr/m3
c   
          gas(nga)       = con(knh3_c)         ! NH3(g) in ppm
          gas(ngn)       = con(khno3_c)        ! HNO3(g) in ppm 
          gas(ngc)       = con(khcl_c)         ! HCl (g) in ppm
          gas(ngso2)     = con(kso2_c)         ! SO2(g) in ppm
          gas(ngh2o2)    = con(kh2o2_c)        ! H2O2(g) in ppm
          gas(nghcho)    = con(kform_c)        ! HCHO(g) in ppm    
          gas(nghcooh)   = 0.1*con(kh2o2_c)    ! HCOOH(g) in ppm  = 0.1*H2O2
          gas(nghno2)    = con(khono_c)        ! HNO2(g) in ppm
          gas(ngo3)      = con(ko3_c)          ! O3 in ppm
          gas(ngoh)      = con(koh_c)          ! OH in ppm
          gas(ngho2)     = con(kho2_c)         ! HO2 in ppm
          gas(ngno3)     = con(kno3_c)         ! NO3 in ppm
          gas(ngno)      = con(kno_c)          ! NO(g) in ppm
          gas(ngno2)     = con(kno2_c)         ! NO2(g) in ppm
          gas(ngpan)     = con(kpan_c)         ! PAN(g) in ppm
          gas(ngch3o2h)  = 0.2*con(kh2o2_c)    ! CH3OOH(g) in ppm  = 0.2*H2O2
          gas(ngch3o2)   = 1.0e-6              ! CH3O2(g) in ppm
          gas(ngch3oh)   = 1.0e-3              ! CH3OH(g) in ppm = 1 ppb
          gas(ngch3co3h) = 0.05*con(kh2o2_c)   ! CH3C(O)OOH(g) in ppm  = 0.05*H2O2
c
          do knsec=1,nsect
            aerosol(knsec,naw)    = con(kph2o_c+(knsec-1))   ! water
            aerosol(knsec,naa)    = con(kpnh4_c+(knsec-1))   ! ammonium
            aerosol(knsec,na4)    = con(kpso4_c+(knsec-1))   ! sulfate
            aerosol(knsec,nan)    = con(kpno3_c+(knsec-1))   ! nitrate
            aerosol(knsec,nas)    = con(kna_c+(knsec-1))     ! sodium
            aerosol(knsec,nac)    = con(kpcl_c+(knsec-1))    ! chloride
            aerosol(knsec,nae)    = con(kpec_c+(knsec-1))    ! elemental carbon
            aerosol(knsec,nao)    = con(kpoa_c+(knsec-1))    ! primary organics
            aerosol(knsec,nar)    = con(kcrst_c+(knsec-1))   ! crustal
            aerosol(knsec,nahso5) = 0.0
            aerosol(knsec,nahmsa) = 0.0
          enddo
c
          call aqchem(gas,aerosol,rhumid,prs,tempk,lwc_c,t1_min,
     &                dt_min,dsecf,ierr)
c
c-----CAMx doesn't carry HMSA or HSO5 so we put their mass into sulfate (NA4)
c
          do knsec=1,nsect
            aerosol(knsec,na4) = aerosol(knsec,na4) +
     &                           96./111.*aerosol(knsec,nahmsa) +
     &                           96./113.*aerosol(knsec,nahso5)
          enddo
c
c-----Map gas and aerosol back con
c
          con(knh3_c)    = amax1(gas(nga)   ,bdnl(knh3_c))
          con(khno3_c)   = amax1(gas(ngn)   ,bdnl(khno3_c))
          con(khcl_c)    = amax1(gas(ngc)   ,bdnl(khcl_c))
          con(kso2_c)    = amax1(gas(ngso2) ,bdnl(kso2_c))
          con(kh2o2_c)   = amax1(gas(ngh2o2),bdnl(kh2o2_c))
          con(kform_c)   = amax1(gas(nghcho),bdnl(kform_c))
          con(khono_c)   = amax1(gas(nghno2),bdnl(khono_c))
          con(ko3_c)     = amax1(gas(ngo3)  ,bdnl(ko3_c))
          con(kno3_c)    = amax1(gas(ngno3) ,bdnl(kno3_c))
          con(kno_c)     = amax1(gas(ngno)  ,bdnl(kno_c))
          con(kno2_c)    = amax1(gas(ngno2) ,bdnl(kno2_c))
          con(kpan_c)    = amax1(gas(ngpan) ,bdnl(kpan_c))
c
          do knsec=1,nsect
            con(kph2o_c+(knsec-1)) = aerosol(knsec,naw)
            con(kpnh4_c+(knsec-1)) = aerosol(knsec,naa)
            con(kpso4_c+(knsec-1)) = aerosol(knsec,na4)
            con(kpno3_c+(knsec-1)) = aerosol(knsec,nan)
            con(kna_c+(knsec-1))   = aerosol(knsec,nas)
            con(kpcl_c+(knsec-1))  = aerosol(knsec,nac)
            con(kpoa_c+(knsec-1))  = aerosol(knsec,nao)
            con(kpec_c+(knsec-1))  = aerosol(knsec,nae)
            con(kcrst_c+(knsec-1)) = aerosol(knsec,nar)
          enddo
          if ( aero_flag.eq.1 ) modeaero = 0 ! if AQ only
        endif
      endif
c
c-----RADM or VSRM -> call AER even if the aqueous module is called
c     OVSR         -> call SOAP alone if the aqueous module is called
c
      if ( aero_flag.eq.2 ) then
        if ( chaq.eq.'OVSR' .and. modeaero.eq.1 ) then
          modeaero = 1
        else
          modeaero = 2
        endif
      endif
c
c-----If neither AQCHEM nor AERCHEM is called we don't call soap
c
      if (modeaero.ne.0) then
        do kk = 1,ntotal
          q(kk) = 0.d0
        enddo
c
c-----Map con to q [ug] gas in ppm
c
        do knsec=1,nsec
          q((knsec-1)*nsp+kh2o)=con(kph2o_c+(knsec-1))
          q((knsec-1)*nsp+kna)=con(kna_c+(knsec-1))
          q((knsec-1)*nsp+kso4)=con(kpso4_c+(knsec-1)) / 96.  * 98.
          q((knsec-1)*nsp+kno3)=con(kpno3_c+(knsec-1)) / 62.  * 63.
          q((knsec-1)*nsp+knh4)=con(kpnh4_c+(knsec-1)) / 18.  * 17.
          q((knsec-1)*nsp+kcl)=con(kpcl_c+(knsec-1))   / 35.5 * 36.5
          q((knsec-1)*nsp+ksoa1)=con(ksoa1_c+(knsec-1))
          q((knsec-1)*nsp+ksoa2)=con(ksoa2_c+(knsec-1))
          q((knsec-1)*nsp+ksoa3)=con(ksoa3_c+(knsec-1))
          q((knsec-1)*nsp+ksoa4)=con(ksoa4_c+(knsec-1))
          q((knsec-1)*nsp+ksoa5)=con(ksoa5_c+(knsec-1))
          q((knsec-1)*nsp+ksoa6)=con(ksoa6_c+(knsec-1))
          q((knsec-1)*nsp+ksoa7)=con(ksoa7_c+(knsec-1))
          q((knsec-1)*nsp+ksopa)=con(ksopa_c+(knsec-1))
          q((knsec-1)*nsp+ksopb)=con(ksopb_c+(knsec-1))
          q((knsec-1)*nsp+kpom)=con(kpoa_c+(knsec-1))
          q((knsec-1)*nsp+kec)=con(kpec_c+(knsec-1))
          q((knsec-1)*nsp+kcrus)=con(kcrst_c+(knsec-1))
        enddo
c
c-----Use MW instead of 100 and actual pressure and temperature for
c     conversion to ppm for organic gases (tmg, 04/15/02)
c     now organic gases are given in ppm - bkoo (08/25/03)
c
        q(naer+inh3)   = con(knh3_c)
        q(naer+ihno3)  = con(khno3_c)
        q(naer+ih2so4) = con(kh2so4_c)
        q(naer+ihcl)   = con(khcl_c)
        q(naer+icg1)   = con(kcg1_c)
        q(naer+icg2)   = con(kcg2_c)
        q(naer+icg3)   = con(kcg3_c)
        q(naer+icg4)   = con(kcg4_c)
        q(naer+icg5)   = con(kcg5_c)
        q(naer+icg6)   = con(kcg6_c)
        q(naer+icg7)   = con(kcg7_c)
c
c-----Call SOAP only
c
        if (modeaero.eq.1) then
          do i = 1,nsec
            qt(i) = 0.d0
            do ki = 2,nsp ! use dry basis
              qt(i) = qt(i)+q((i-1)*nsp+ki) ! total mass per section (ug/m3)
            enddo  
            qn(i) = qt(i)/dsec(i)**3 ! calculate 0th moment (number of particles)
          enddo ! if density = 1 g/cm3 units are particles/cm3
          call wdiameter(q) ! wet diameter
          ntotalx  = 0 ! not used
          nsecx    = 0 ! not used
          ntotalx2 = ntotal
          nsecx2   = nsec
          call eqparto(t1,q) ! equilibrium organic aerosol partitioning
          call ddiameter(q)  ! dry diameter
          call newdist(t1,q) ! size distribution mapping
          call step(nsec,q)  ! calculate water in each section
c
c-----Call SOAP + AER
c
        else
          call aerchem(chaero,q,t0,t1,lfrst,ierr)
        endif
c
c-----Map q back to con 
c
        do knsec=1,nsec
          con(kph2o_c+(knsec-1))=q((knsec-1)*nsp+kh2o)
          con(kna_c  +(knsec-1))=q((knsec-1)*nsp+kna)
          con(kpso4_c+(knsec-1))=q((knsec-1)*nsp+kso4) * 96.  / 98.
          con(kpno3_c+(knsec-1))=q((knsec-1)*nsp+kno3) * 62.  / 63.
          con(kpnh4_c+(knsec-1))=q((knsec-1)*nsp+knh4) * 18.  / 17.
          con(kpcl_c +(knsec-1))=q((knsec-1)*nsp+kcl)  * 35.5 / 36.5
          con(ksoa1_c+(knsec-1))=q((knsec-1)*nsp+ksoa1)
          con(ksoa2_c+(knsec-1))=q((knsec-1)*nsp+ksoa2)
          con(ksoa3_c+(knsec-1))=q((knsec-1)*nsp+ksoa3)
          con(ksoa4_c+(knsec-1))=q((knsec-1)*nsp+ksoa4)
          con(ksoa5_c+(knsec-1))=q((knsec-1)*nsp+ksoa5)
          con(ksoa6_c+(knsec-1))=q((knsec-1)*nsp+ksoa6)
          con(ksoa7_c+(knsec-1))=q((knsec-1)*nsp+ksoa7)
          con(ksopa_c+(knsec-1))=q((knsec-1)*nsp+ksopa)
          con(ksopb_c+(knsec-1))=q((knsec-1)*nsp+ksopb)
          con(kpoa_c +(knsec-1))=q((knsec-1)*nsp+kpom)
          con(kpec_c +(knsec-1))=q((knsec-1)*nsp+kec)
          con(kcrst_c+(knsec-1))=q((knsec-1)*nsp+kcrus)
        enddo
c
        con(knh3_c)  =amax1(SNGL(q(naer+inh3)),  bdnl(knh3_c))
        con(khno3_c) =amax1(SNGL(q(naer+ihno3)), bdnl(khno3_c))
        con(kh2so4_c)=amax1(SNGL(q(naer+ih2so4)),bdnl(kh2so4_c))
        con(khcl_c)  =amax1(SNGL(q(naer+ihcl)),  bdnl(khcl_c))
        con(kcg1_c) = amax1(SNGL(q(naer+icg1)),  bdnl(kcg1_c))
        con(kcg2_c) = amax1(SNGL(q(naer+icg2)),  bdnl(kcg2_c))
        con(kcg3_c) = amax1(SNGL(q(naer+icg3)),  bdnl(kcg3_c))
        con(kcg4_c) = amax1(SNGL(q(naer+icg4)),  bdnl(kcg4_c))
        con(kcg5_c) = amax1(SNGL(q(naer+icg5)),  bdnl(kcg5_c))
        con(kcg6_c) = amax1(SNGL(q(naer+icg6)),  bdnl(kcg6_c))
        con(kcg7_c) = amax1(SNGL(q(naer+icg7)),  bdnl(kcg7_c))
c
      endif
c
      lfrst = .false.
      return
      end
