      subroutine wetdep(m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon,
     &                  igrd,ncol,nrow,nlay,nspcs,nspdep,deltat,deltax,
     &                  deltay,mapscl,depth,tempk,press,cwc,pwr,pws,
     &                  pwg,cph,densfac,idfin,conc,fluxes,fluxtmp,
     &                  depfld,wetfld,dtout,ipa_cel,iptrsa)
      use chmstry
      use bndary
      use procan
      use tracer
c
c----CAMx v5.41 121109
c 
c     WETDEP modifies vertical concentration profiles for a given grid via 
c     precipitation processes.  This subroutine has been completely rewritten
c     for CAMx v4.
c 
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c           
c     Modifications:
c        06/15/03   Fixed bug in scaling of tmass
c        02/11/04   Updated empirical rainfall relationships
c        04/21/04   Incorporated sectional PM
c        05/03/05   Revised gas scavenging calculation
c        05/04/05   Revised DDM adjustments
c        06/20/05   Revised to handle liquid/frozen cloud and precip water
c        10/21/05   Added dissociation to Henry's Law for NH3, HNO3, SO2
c        08/31/06   Added map scale factor
c        07/16/07   Fixed PA bug - adjust cipr if evap
c        07/16/07 -bkoo-     Added check for HDDM
c        05/12/08 -cemery-   Revised deposited liquid concentration to use correct rainfall volume
c        07/16/08 -bkoo-     Added DDM turn-off flag
c        07/16/08 -gwilson-  Now accumulates mass flux summary outside OMP loop
c        12/22/09 -cemery-   Improved treatment of "c0" array
c
c     Input arguments:
c        igrd               grid index 
c        ncol                number of columns 
c        nrow                number of rows 
c        nlay                number of layers 
c        nspcs               number of total species
c        nspdep              number of deposition species
c        deltat              time step (s)
c        deltax              cell size in x-direction (m)
c        deltay              cell size in y-direction (m)
c        mapscl              map scale factor
c        depth               cell depth (m)
c        tempk               temperature field (K)
c        press               pressure field (mb)
c        cwc                 cloud water content (g/m3)
c        pwr                 rain water content (g/m3)
c        pws                 snow water content (g/m3)
c        pwg                 graupel water content (g/m3)
c        cph                 cloud water pH
c        densfac             factor to convert to umol/m3
c        idfin               map of nested grids in this grid
c        conc                concentration field (umol/m3, ug/m3)
c        dtout               output frequency (minutes)
c        ipa_cel             gridded array to identify if cell is
c                            in a IPRM sub-domain
c        iptrsa              pointer into probing tools conc array for
c                            this grid
c             
c     Output arguments: 
c        conc                concentration field (umol/m3, ug/m3)
c        fluxes              boundary mass fluxes (umol, ug)
c        fluxtmp             temporary array for fluxes
c        depfld              2-D array of wet deposited mass (mol/ha, g/ha)
c                            and surface liquid concentrations (mol/l, g/l)
c        wetfld              wet depostion array for tracer species
c             
c     Routines called: 
c        SCAVRAT
c        HENRYFNC
c             
c     Called by: 
c        EMISTRNS
c 
      implicit none
      include 'camx.prm'
      include 'flags.inc'
c
      integer ncol,nrow,nlay
c
c======================== Probing Tool Begin ===========================
c
      real fc2r, fr2c, fc2rc0, fr2cc0, cyctr
      logical lzerc, levap
      integer iptrsa, icls
c
      real c0trac(MXTRSP)
      real tmtrac(MXTRSP)
      real delcls(MXALCLS)
      real c0cls(MXALCLS)
      real concls(MXALCLS)
      real cytcls(MXALCLS)
c
      integer ipa_cel(ncol,nrow,nlay), ipa_idx
c
c========================= Probing Tool End ============================
c
      integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon
      integer :: m_zi1,m_zi2,m_zj1,m_zj2

      integer nspcs,nspdep,igrd,j,jycl,i,ixcl,kbot,ktop,
     &        k,ncnt,kzcl,l,isemptyf,isemptyc,kwtr,isec,isempty,
     &        iaero,ll
      integer idfin(m1,m2)
      real deltax(nrow)
      real, dimension(m1,m2,m3) :: tempk,press,
     &                             cwc,pwr,pws,pwg,cph
      real, dimension(m1,m2,m3) :: depth
      real conc(m1,m2,m3,nspcs),depfld(m1,m2,3*nspdep)
      real fluxtmp(m1,m2,m3,nspcs)
      real wetfld(m1,m2,notimespc)
      real mapscl(m1,m2)
      real*8 fluxes(nspcs,11)
      real rd,rhoh2o,deltat,deltay,densfac,dtout,cellvol,rainvol,rhoair,
     &     delc,delm,convfac,cmin,hlaw,gscav,ascav,totc,totw,ceq,
     &     qtf,qtc,vtf,vtc,psizec,ascavf,roprta,psize,
     &     ascavc,qt,vt,cwat,pwat,rconst,area,volume,dtfall,drpvel,delc0
      logical lcloud,ltop,lgraupl
c
      real c0(MXSPEC)
      real pp(MXLAYER)
      real rr(MXLAYER)
      real volrat(MXLAYER)
      real tmass(MXSPEC)
      real delr(MXSPEC)
c
      data rd /287./         ! Dry air gas constant (J/K/kg)
      data rhoh2o /1.e6/     ! water density (g/m3)
      data rconst /8.206e-2/ ! gas constant (l.atm/mol.K)
c
c-----Entry point
c
      call zeros( fluxtmp,m1*m2*m3*nspcs )
c
! Set grid point limits
      m_zi1= ia-1
      m_zi2= iz+1
      m_zj1= ja-1
      m_zj2= jz+1

      if(btest(ibcon,0)) m_zi1 = ia
      if(btest(ibcon,1)) m_zi2 = iz
      if(btest(ibcon,2)) m_zj1 = ja
      if(btest(ibcon,3)) m_zj2 = jz
c
c-----Loop over rows and columns
c
      do 10 j =  m_zj1, m_zj2   !2,nrow-1
        jycl = j
c
c$omp parallel default(shared)
c$omp&  private(i,ixcl,kbot,ktop,k,ncnt,volrat,
c$omp&          rr,pp,ltop,kzcl,lcloud,lgraupl,cellvol,rainvol,
c$omp&          rhoair,l,c0trac,tmtrac,delcls,concls,cytcls,
c$omp&          c0cls,hlaw,cwat,pwat,delc,delm,tmass,c0,convfac,
c$omp&          cmin,gscav,ascav,totc,totw,ceq,cyctr,
c$omp&          fc2r,fr2c,fc2rc0,fr2cc0,levap,lzerc,ipa_idx,isemptyf,
c$omp&          isemptyc,qtf,qtc,vtf,vtc,psizec,ascavf,
c$omp&          roprta,psize,ascavc,delr,kwtr,isec,isempty,
c$omp&          qt,vt,iaero,area,volume,dtfall,drpvel,delc0)
c
c$omp do schedule(dynamic)
c
        do 20 i = m_zi1, m_zi2    !2,ncol-1
          ixcl = i
          if (idfin(i,j).gt.igrd) goto 20
          do l=1,nspec
            tmass(l) = 0.
            c0(l) = 0.
          enddo
c
c-----Scan column for layers containing precipitation bottom/top
c
          kbot = 0
          ktop = 0
          do k = 1,nlay
            if (pwr(i,j,k).ge.cwmin .or. pws(i,j,k).ge.cwmin .or.
     &          pwg(i,j,k).ge.cwmin) then
              kbot = k
              goto 25
            endif
          enddo
          goto 20
c
  25      continue
          if (kbot.eq.nlay) goto 20
          ncnt = 1
          do k = kbot+1,nlay
            if (pwr(i,j,k).lt.cwmin .and. pws(i,j,k).lt.cwmin .and.
     &          pwg(i,j,k).lt.cwmin) then
              ktop = k-1
              goto 26
            endif
            ncnt = ncnt + 1
          enddo
          ktop = nlay
  26      continue
          if (kbot.gt.1 .and. ncnt.eq.1) goto 20
c
c-----Load precip and cloud profiles, and determine precip rate
c
          do k = 1,nlay
            volrat(k) = 0.
            rr(k) = 0.
            pp(k) = pwr(i,j,k) + pws(i,j,k) + pwg(i,j,k)
          enddo
          do k = kbot,ktop
            volrat(k) = pp(k)/rhoh2o                 ! drop volume/air volume
            rr(k) = (volrat(k)/1.0e-7)**1.27         ! rainfall rate (mm/hr)
          enddo
c
c-----Loop over layers and species for precipitating columns
c
          ltop = .true.
          do 30 k = ktop,kbot,-1
            kzcl = k
            lcloud = .false.
            lgraupl = .false.
            if (cwc(i,j,k).ge.cwmin) lcloud = .true.
            if (pwg(i,j,k).ge.cwmin) lgraupl = .true.
            cellvol = deltax(j+j0)*deltay*
     &                           depth(i,j,k)/mapscl(i,j)**2
            rainvol = volrat(k)*cellvol
            rhoair = 100.*press(i,j,k)/(rd*tempk(i,j,k))
c
c======================== Probing Tool Begin ===========================
c
            if( ((lddm .OR. lhddm) .AND. lddmcalc(igrd)) .OR.
     &                         (ltrace .AND. tectyp .NE. RTRAC 
     &                                  .AND. tectyp .NE. RTCMC) ) then
               if (ltop) then
                  do l=1,ntotsp
                     c0trac(l) = 0.
                     tmtrac(l) = 0.
                  enddo
               endif
               do icls=1,ntrcls
                 delcls(icls) = 0.
                 concls(icls) = 0.
                 cytcls(icls) = 0.
                 c0cls(icls) = 0.
               enddo
            endif
c
c======================== Probing Tool End =============================
c
c
c-----Calculate scavenging for soluble gas species
c
            do 40 l = nrad+1,ngas
              if (ltop) then
                tmass(l) = 0.
                c0(l) = 0.
              endif
              if( henry0(l).LT.1.e-6 ) goto 40
              call henryfnc(l,henry0(l),tfact(l),tempk(i,j,k),
     &                      cph(i,j,k),knh3,khno3,kso2,hlaw)
              hlaw = hlaw*rconst*tempk(i,j,k)
              cwat = cwc(i,j,k)
              pwat = pp(k)
              if (tempk(i,j,k).lt.273. .and. rscale(l).gt.0.) then
                cwat = amax1(0.,cwc(i,j,k)*
     &                          (tempk(i,j,k) - tamin)/(273. - tamin))
                pwat = pwr(i,j,k)
              endif
              delc = 0.
              delm = 0.
              convfac = densfac*(273./tempk(i,j,k))*(press(i,j,k)/1013.)
              cmin = bdnl(l)*convfac
              conc(i,j,k,l) = amax1(cmin,conc(i,j,k,l))
c
              call scavrat(.false.,lcloud,lgraupl,tamin,rr(k),
     &                     tempk(i,j,k),cwat,rhoair,conc(i,j,k,l),
     &                     hlaw,diffrat(l),rscale(l),0.,0.,gscav,ascav,
     &                     drpvel)
c
              totc = conc(i,j,k,l) + c0(l)
              totw = cwat + pwat
              ceq = totc/(1. + hlaw*totw/rhoh2o)
              ceq = totc - ceq
              if( totw .NE. 0.0 ) then
                 ceq = ceq*pwat/totw
              else
                 ceq = 0.0
              endif
              delc = (ceq - c0(l))*(1. - exp(-gscav*deltat))
              if (delc.gt.0.) delc = amin1(delc,conc(i,j,k,l)-cmin)
              dtfall = depth(i,j,k)/drpvel
              delc0 = (ceq - c0(l))*(1. - exp(-gscav*dtfall))
              if (delc0.gt.0.) delc0 = amin1(delc0,conc(i,j,k,l)-cmin)
c
c======================== Probing Tool Begin ===========================
c
c  --- Accumulate OSAT/PSAT tracer adjustments by tracer class ---
c
              if( ltrace .AND. tectyp .NE. RTRAC .AND.
     &                                       tectyp .NE. RTCMC ) then
                 cyctr = 0.5*(1. - EXP(-gscav*deltat))*
     &                                          MIN(conc(i,j,k,l),c0(l))
                 do icls=1,ntrcls
                    concls(icls) = concls(icls) + conc(i,j,k,l)*
     &                                                   fluxmap(l,icls)
                    c0cls(icls) = c0cls(icls) + c0(l)*fluxmap(l,icls)
                    delcls(icls) = delcls(icls) + delc*fluxmap(l,icls)
                    cytcls(icls) = cytcls(icls) + cyctr*fluxmap(l,icls)
                 enddo
              endif
c
c  --- For DDM convert delc to flux ---
c
              if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
c
c  --- For DDM convert delc to flux ---
c
                 if( delc.GT.0. ) then
                    fc2r = delc/conc(i,j,k,l)
                    fr2c = 0.
                    fc2rc0 = delc0/conc(i,j,k,l)
                    fr2cc0 = 0.
                 elseif( c0(l) .GT. 1.0e-20 ) then
                    fc2r = 0.
                    fr2c = -delc/c0(l)
                    fc2rc0 = 0.
                    fr2cc0 = -delc0/c0(l)
                 else
                    fc2r = 0.
                    fr2c = 0.
                    fc2rc0 = 0.
                    fr2cc0 = 0.
                 endif
c
c  --- Check for special situations ---
c
                 levap = .FALSE.
                 if( kbot.GT.1 .and. k.EQ.kbot ) levap = .TRUE.
                 lzerc = .FALSE.
                 if( conc(i,j,k,l)-delc .LE. 1.1*cmin ) lzerc = .TRUE.
c
c  --- Adjust DDM sensitivities for this species ---
c 
                 call adjddmc0(l,ixcl,jycl,kzcl,fc2r,fr2c,fc2rc0,fr2cc0,
     &                         c0trac,tmtrac,cellvol,rainvol,lzerc,
     &                         levap,m1,m2,m3,ntotsp,ptconc(iptrsa))
              endif
c
c-----PA change from wet deposition
c
              if( lipr ) then
                if( i .GE. ia .AND. i .LE. iz .AND. j .GE. ja
     &                                         .AND. j .LE. jz ) then
                   if( ipa_cel(i+i0,j+j0,k) .GT. 0 ) then
                      ipa_idx = ipa_cel(i+i0,j+j0,k)
                      cipr(IPR_WDEP, ipa_idx, l) =
     &                                 cipr(IPR_WDEP, ipa_idx, l) - delc
                   endif
                 endif
              endif
c
c========================= Probing Tool End ============================
c
c-----Update the cell concentration and rain mass
c
              conc(i,j,k,l) = conc(i,j,k,l) - delc
              delm = delc*cellvol
              tmass(l) = tmass(l) + delm
              c0(l) = c0(l) + delc0
 40         continue
c
c-----Calculate scavenging for particulate species
c
            if (naero .gt. 0) then
c
c-----Recalculate particle size for wet diameter
c
c-----CF 2-bin scheme
c
              if (lchem .AND. aeropt.eq.'CF') then
                isemptyf = 1
                isemptyc = 1
                qtf = 0. ! fine dry total mass
                qtc = 0. ! coarse dry total mass
                vtf = 0. ! fine dry total volume
                vtc = 0. ! coarse dry total volume
                do l = ngas+1,nspec
                  if (dcut(l,2).lt.(dcut(kph2o,2)+1.e-5)) then     ! fine
                    if (l.ne.kph2o) then
                      if (conc(i,j,k,l).gt.bdnl(l)) isemptyf = 0
                      qtf = qtf + conc(i,j,k,l)
                      vtf = vtf + conc(i,j,k,l)/roprt(l)
                    endif
                  else                                             ! coarse
                    if (conc(i,j,k,l).gt.bdnl(l)) isemptyc = 0
                    qtc = qtc + conc(i,j,k,l)
                    vtc = vtc + conc(i,j,k,l)/roprt(l)
                    psizec = sqrt(dcut(l,1)*dcut(l,2))
                  endif
                enddo
                ascavf = 0.
                if (isemptyf.eq.0) then
                  roprta = (qtf + conc(i,j,k,kph2o)) /
     &                     (vtf + conc(i,j,k,kph2o)/roprt(kph2o))
                  psize = sqrt(dcut(kph2o,1)*dcut(kph2o,2))
                  psize = 1.e-6*psize*(1. + 
     &                    conc(i,j,k,kph2o)/roprt(kph2o)/vtf)**0.33333
                  call scavrat(.true.,lcloud,lgraupl,tamin,rr(k),
     &                         tempk(i,j,k),0.,rhoair,0.,0.,0.,0.,
     &                         psize,roprta,gscav,ascavf,drpvel)
                endif
                ascavc = 0.
                if (isemptyc.eq.0) then
                  roprta = qtc/vtc
                  psize = psizec*1.e-6
                  call scavrat(.true.,lcloud,lgraupl,tamin,rr(k),
     &                         tempk(i,j,k),0.,rhoair,0.,0.,0.,0.,
     &                         psize,roprta,gscav,ascavc,drpvel)
                endif
                do l = ngas+1,nspec
                  if (dcut(l,2).lt.(dcut(kph2o,2)+1.e-5)) then
                    delr(l) = 1. - exp(-ascavf*deltat)
                  else
                    delr(l) = 1. - exp(-ascavc*deltat)
                  endif
                enddo
c
c-----CMU multi-section scheme
c
              elseif (lchem .AND. aeropt.eq.'CMU') then
                kwtr = (kph2o_1 - ngas)/nbin + 1
                if (nbin.eq.1) kwtr = kph2o_1 - ngas
                do isec = 1, nbin
                  isempty = 1
                  qt = 0. ! dry total mass
                  vt = 0. ! dry total volume
                  do iaero = 1, naero
                    if (iaero.ne.kwtr) then
                      l = ngas + (iaero-1)*nbin + isec
                      if (conc(i,j,k,l).gt.bdnl(l)) isempty = 0
                      qt = qt + conc(i,j,k,l)
                      vt = vt + conc(i,j,k,l)/roprt(l)
                    endif
                  enddo
                  ascav = 0.
                  if (isempty.eq.0) then
                    roprta = (qt + conc(i,j,k,kph2o_1-1+isec)) /
     &                       (vt + conc(i,j,k,kph2o_1-1+isec)/
     &                        roprt(kph2o_1))
                    psize = sqrt(dcut(ngas+isec,1)*dcut(ngas+isec,2))
                    psize = 1.e-6*psize*(1. + 
     &                      conc(i,j,k,kph2o_1-1+isec)/
     &                      roprt(kph2o_1)/vt)**0.33333
                    call scavrat(.true.,lcloud,lgraupl,tamin,rr(k),
     &                           tempk(i,j,k),0.,rhoair,0.,0.,0.,0.,
     &                           psize,roprta,gscav,ascav,drpvel)
                  endif
                  do iaero = 1, naero
                    l = ngas + (iaero-1)*nbin + isec
                    delr(l) = 1. - exp(-ascav*deltat)
                  enddo
                enddo
c
c-----Other scheme
c
              else
                do l = ngas+1,nspec
                  psize = 1.e-6*sqrt(dcut(l,1)*dcut(l,2))
                  call scavrat(.true.,lcloud,lgraupl,tamin,rr(k),
     &                         tempk(i,j,k),0.,rhoair,0.,0.,0.,0.,
     &                         psize,roprt(l),gscav,ascav,drpvel)
                  delr(l) = 1. - exp(-ascav*deltat)
                enddo
              endif
c
              do 50 l = ngas+1,nspec
                delc = 0.
                delm = 0.
                if (ltop) tmass(l) = 0.
                cmin = bdnl(l)
                conc(i,j,k,l) = amax1(cmin,conc(i,j,k,l))
                delc = conc(i,j,k,l)*delr(l)
                delc = amin1(delc,conc(i,j,k,l)-cmin)
c
                conc(i,j,k,l) = conc(i,j,k,l) - delc
                delm = delc*cellvol
                tmass(l) = tmass(l) + delm
c
c========================= Probing Tool Begin ============================
c
c
c-----PA change from wet deposition
c
                if( lipr ) then
                   if( i .GE. ia .AND. i .LE. iz .AND. j .GE. ja
     &                                         .AND. j .LE. jz ) then
                     if( ipa_cel(i+i0,j+j0,k) .GT. 0 ) then
                         ipa_idx = ipa_cel(i+i0,j+j0,k)
                         cipr(IPR_WDEP, ipa_idx, l) =
     &                              cipr(IPR_WDEP, ipa_idx, l) - delc
                     endif
                   endif
                endif
c
                if( ltrace .AND. tectyp .NE. RTRAC .AND.
     &                                     tectyp .NE. RTCMC ) then
                   do icls=1,ntrcls
                      delcls(icls) = delcls(icls) + delc*
     &                                               fluxmap(l,icls)
                   enddo
                endif
c
c========================= Probing Tool End ============================
c
 50           continue
            endif
c
c======================== Source Apportion Begin =======================
c
c  --- Adjust OSAT/PSAT tracer by tracer class ---
c
            if( ltrace .AND. tectyp .NE. RTRAC .AND.
     &                                       tectyp .NE. RTCMC ) then
               levap = .FALSE.
               if( kbot .GT. 1 .AND. k .EQ. kbot ) levap = .TRUE.
               call adjstc0(m1,m2,m3,igrd,ixcl,jycl,kzcl,delcls,
     &                concls,cytcls,c0cls,tmtrac,cellvol,rainvol,levap)
            endif
c
c======================== Source Apportion End =========================
c
            ltop = .false.
 30       continue
c
c-----If rain evaporates before reaching the ground, return all mass
c     back to layer KBOT
c
          if (kbot.gt.1) then
            cellvol = deltax(j+j0)*deltay*
     &                      depth(i,j,kbot)/mapscl(i,j)**2
            do l = nrad+1,nspec
              conc(i,j,kbot,l) = conc(i,j,kbot,l) + tmass(l)/cellvol
c
c========================= Probing Tool Begin ============================
c
c-----PA change from rain evaporation
c
              if( lipr ) then
                if( i .GE. ia .AND. i .LE. iz .AND. j .GE. ja
     &                                         .AND. j .LE. jz ) then
                   if( ipa_cel(i+i0,j+j0,kbot) .GT. 0 ) then
                     ipa_idx = ipa_cel(i+i0,j+j0,kbot)
                     cipr(IPR_WDEP, ipa_idx, l) =
     &                   cipr(IPR_WDEP, ipa_idx, l) + tmass(l)/cellvol
                   endif
                endif
              endif
c
c========================= Probing Tool End ============================
c
              tmass(l) = 0.
            enddo
c
c-----Otherwise rain reaches the ground, increment deposition flux
c     arrays
c
          else
            area = deltax(j+j0)*deltay/mapscl(i,j)**2
            volume = 3.6e-6*rr(kbot)*deltat*area
            do l = nrad+1,nspec
              if( i .GE. ia .AND. i .LE. iz .AND. j .GE. ja 
     &                       .AND. j .LE. jz ) fluxtmp(i,j,1,l) = 
     &                                      fluxtmp(i,j,1,l) - tmass(l)
              do ll = 1,ndepspc
                if (l .eq. ldepmap(ll)) then
                  depfld(i,j,ndepspc+ll) = depfld(i,j,ndepspc+ll) +
     &                  1.e-2*tmass(l)/area
                  depfld(i,j,2*ndepspc+ll) = depfld(i,j,2*ndepspc+ll) +
     &                  1.e-9*(tmass(l)/volume)*deltat/(60.*dtout)
                  goto 100
                endif
              enddo
 100          continue
            enddo
c
c======================== Source Apportion Begin =======================
c
            if( lptdepout) then
              do l = 1,notimespc
                 wetfld(i,j,l) = wetfld(i,j,l) + 1.e-2*tmtrac(l)/area
              enddo
            endif
c
c======================== Source Apportion End =======================
c
          endif
c
 20     continue
c
c  --- end of parallelized loop ---
c
c$omp end parallel
c
 10   continue
c
c  --- load fluxes into global array ---
c
      do j=m_zj1,m_zj2
        do i = m_zi1, m_zi2    !2,ncol-1
           do l=1,nspec
              fluxes(l,11) = fluxes(l,11) + fluxtmp(i,j,1,l)
           enddo
        enddo
      enddo
      call flush(6)
c
      return
      end