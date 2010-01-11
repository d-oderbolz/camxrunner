      subroutine wetdep(igrid,ncol,nrow,nlay,nspcs,deltat,deltax,deltay,
     &                  mapscl,depth,tempk,press,cwc,pwr,pws,pwg,cph,
     &                  densfac,idfin,conc,fluxes,depfld,dtout,ipa_cel,
     &                  iptrsa)
c
c----CAMx v4.42 070603
c 
c     WETDEP modifies vertical concentration profiles for a given grid via 
c     precipitation processes.  This subroutine has been completely rewritten
c     for CAMx v4.
c 
c     Copyright 1996-2007
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
c
c     Input arguments:
c        igrid               grid index 
c        ncol                number of columns 
c        nrow                number of rows 
c        nlay                number of layers 
c        nspcs               number of total species
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
c        depfld              2-D array of wet deposited mass (mol/ha, g/ha)
c                            and surface liquid concentrations (mol/l, g/l)
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
      include 'bndary.com'
      include 'chmstry.com'
c
      integer ncol,nrow,nlay
c
c======================== Probing Tool Begin ===========================
c
      include 'tracer.com'
      real fc2r, fr2c, c0trac(MXTRSP), tmtrac(MXTRSP)
      real delcls(MXALCLS), c0cls(MXALCLS), concls(MXALCLS)
      real cytcls(MXALCLS), cyctr
      logical lzerc, levap
      integer iptrsa, icls
c
      include 'procan.com'
      integer ipa_cel(ncol,nrow,nlay),ipa_idx
c
c========================= Probing Tool End ============================
c
      integer nspcs,igrid,j,jycl,i1,i2,i,ixcl,kbot,ktop,
     &        k,ncnt,kzcl,l,isemptyf,isemptyc,kwtr,isec,isempty,
     &        iaero,ll
      integer idfin(ncol,nrow)
      real deltax(nrow),tempk(ncol,nrow,nlay),press(ncol,nrow,nlay),
     &     cwc(ncol,nrow,nlay),pwr(ncol,nrow,nlay),pws(ncol,nrow,nlay),
     &     pwg(ncol,nrow,nlay),cph(ncol,nrow,nlay),depth(ncol,nrow,nlay)
      real mapscl(ncol,nrow)
      real conc(ncol,nrow,nlay,nspcs),depfld(ncol,nrow,3*nspcs)
      real*8 fluxes(nspcs,11)
      real c0(MXSPEC),pp(MXLAYA),rr(MXLAYA),volrat(MXLAYA),
     &     tmass(MXSPEC)
      real delr(MXSPEC)
      real rd,rhoh2o,deltat,deltay,densfac,dtout,cellvol,rainvol,rhoair,
     &     delc,delm,convfac,cmin,hlaw,gscav,ascav,c00,totc,totw,ceq,
     &     qtf,qtc,vtf,vtc,psizec,ascavf,roprta,psize,
     &     ascavc,qt,vt,cwat,pwat,rconst
      logical lcloud,ltop,lgraupl
c
      data rd /287./         ! Dry air gas constant (J/K/kg)
      data rhoh2o /1.e6/     ! water density (g/m3)
      data rconst /8.206e-2/ ! gas constant (l.atm/mol.K)
c
c-----Entry point
c
c-----Loop over rows and columns
c
      do 10 j = 2,nrow-1 
        jycl = j
        i1 = 2 
        i2 = ncol-1 
        if (igrid.eq.1) then 
          if (ibeg(j).eq.-999) goto 10 
          i1 = ibeg(j) 
          i2 = iend(j) 
        endif 
        do 20 i = i1,i2
          ixcl = i
          if (idfin(i,j).gt.igrid) goto 20
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
            cellvol = deltax(j)*deltay*depth(i,j,k)/mapscl(i,j)**2
            rainvol = volrat(k)*cellvol
            rhoair = 100.*press(i,j,k)/(rd*tempk(i,j,k))
c
c======================== Probing Tool Begin ===========================
c
            if( lddm .OR. ltrace .AND. tectyp .NE. RTRAC ) then
               if (ltop) then
                  do l=1,ntotsp
                     c0trac(l) = 0.
                     tmtrac(l) = 0.
                  enddo
               else
                  do l=1,ntotsp
                     c0trac(l) = tmtrac(l) / rainvol
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
            do 40 l = 1,ngas
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
              if (ltop) tmass(l) = 0.
              c0(l) = tmass(l)/rainvol
              convfac = densfac*(273./tempk(i,j,k))*(press(i,j,k)/1013.)
              cmin = bdnl(l)*convfac
              conc(i,j,k,l) = amax1(cmin,conc(i,j,k,l))
c
              call scavrat(.false.,lcloud,lgraupl,tamin,rr(k),
     &                     tempk(i,j,k),cwat,depth(i,j,k),rhoair,
     &                     conc(i,j,k,l),hlaw,diffrat(l),
     &                     rscale(l),0.,0.,gscav,ascav)
c
              c00 = c0(l)*volrat(k)
              totc = conc(i,j,k,l) + c00
              totw = cwat + pwat
              ceq = totc/(1. + hlaw*totw/rhoh2o)
              ceq = totc - ceq
              delc = (ceq - c00)*(1. - exp(-gscav*deltat))
              if (delc.gt.0.) delc = amin1(delc,conc(i,j,k,l)-cmin)
c
c======================== Probing Tool Begin ===========================
c
c  --- Accumulate OSAT/PSAT tracer adjustments by tracer class ---
c
              if( ltrace .AND. tectyp .NE. RTRAC ) then
                 cyctr = 0.5*(1. - EXP(-gscav*deltat))*
     &                                           MIN(conc(i,j,k,l), c00)
                 do icls=1,ntrcls
                    concls(icls) = concls(icls) + conc(i,j,k,l)*
     &                                                   fluxmap(l,icls)
                    c0cls(icls) = c0cls(icls) + c0(l)*fluxmap(l,icls)
                    delcls(icls) = delcls(icls) + delc*fluxmap(l,icls)
                    cytcls(icls) = cytcls(icls) + cyctr*fluxmap(l,icls)
                 enddo
              endif
c
              if( lddm ) then
c
c  --- For DDM convert delc to flux ---
c
                 if( delc.GT.0. ) then
                    fc2r = delc/conc(i,j,k,l)
                    fr2c = 0.
                 elseif( c0(l) .GT. 1.0e-20 ) then
                    fc2r = 0.
                    fr2c = -delc/c0(l)
                 else
                    fc2r = 0.
                    fr2c = 0.
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
                 call adjddmc0(l,ixcl,jycl,kzcl,fc2r,fr2c,
     &                         c0trac,tmtrac,cellvol,rainvol,lzerc,
     &                         levap,ncol,nrow,nlay,ntotsp,
     &                         ptconc(iptrsa))
              endif
c
c-----PA change from wet deposition
c
              if( lipr .AND. ipa_cel(i,j,k) .GT. 0 ) then
                ipa_idx = ipa_cel(i,j,k)
                cipr(IPR_WDEP, ipa_idx, l) =
     &                              cipr(IPR_WDEP, ipa_idx, l) - delc
              endif
c
c========================= Probing Tool End ============================
c
c-----Update the cell concentration and rain mass
c
              conc(i,j,k,l) = conc(i,j,k,l) - delc
              delm = delc*cellvol
              tmass(l) = tmass(l) + delm
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
              if (aeropt.eq.'CF' .and. kph2o.ne.nspec+1) then
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
     &                         tempk(i,j,k),0.,depth(i,j,k),
     &                         rhoair,0.,0.,0.,0.,psize,roprta,
     &                         gscav,ascavf)
                endif
                ascavc = 0.
                if (isemptyc.eq.0) then
                  roprta = qtc/vtc
                  psize = psizec*1.e-6
                  call scavrat(.true.,lcloud,lgraupl,tamin,rr(k),
     &                         tempk(i,j,k),0.,depth(i,j,k),
     &                         rhoair,0.,0.,0.,0.,psize,roprta,
     &                         gscav,ascavc)
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
              elseif (aeropt.eq.'CMU' .and. kph2o.ne.nspec+1) then
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
     &                           tempk(i,j,k),0.,depth(i,j,k),
     &                           rhoair,0.,0.,0.,0.,psize,roprta,
     &                           gscav,ascav)
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
     &                         tempk(i,j,k),0.,depth(i,j,k),
     &                         rhoair,0.,0.,0.,0.,psize,roprt(l),
     &                         gscav,ascav)
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
c-----PA change from wet deposition
c
                if( lipr .AND. ipa_cel(i,j,k) .GT. 0 ) then
                  ipa_idx = ipa_cel(i,j,k)
                  cipr(IPR_WDEP, ipa_idx, l) =
     &                              cipr(IPR_WDEP, ipa_idx, l) - delc
                endif
c
c
                if( ltrace .AND. tectyp .NE. RTRAC ) then
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
c========================= Probing Tool Begin ==========================
c
c  --- Adjust OSAT/PSAT tracer by tracer class ---
c
            if( ltrace .AND. tectyp .NE. RTRAC ) then
               levap = .FALSE.
               if( kbot .GT. 1 .AND. k .EQ. kbot ) levap = .TRUE.
               call adjstc0(igrid,ixcl,jycl,kzcl,delcls,concls,cytcls,
     &                               c0cls,tmtrac,cellvol,rainvol,levap)
            endif
c
c========================= Probing Tool End ============================
c
            ltop = .false.
 30       continue
c
c-----If rain evaporates before reaching the ground, return all mass
c     back to layer KBOT
c
          if (kbot.gt.1) then
            cellvol = deltax(j)*deltay*depth(i,j,kbot)/mapscl(i,j)**2
            do l = 1,nspec
              conc(i,j,kbot,l) = conc(i,j,kbot,l) + tmass(l)/cellvol
              tmass(l) = 0.
            enddo
c
c-----Otherwise rain reaches the ground, increment deposition flux
c     arrays
c
          else
            do l = 1,nspec
              fluxes(l,11) = fluxes(l,11) - tmass(l)
              do ll = 1,navspc
                if (l .eq. lavmap(ll)) then
                  depfld(i,j,navspc+ll) = depfld(i,j,navspc+ll) +
     &                  1.e-2*tmass(l)/(deltax(j)*deltay/mapscl(i,j)**2)
                  depfld(i,j,2*navspc+ll) = depfld(i,j,2*navspc+ll) +
     &                  1.e-9*(tmass(l)/rainvol)*deltat/(60.*dtout)
                  goto 100
                endif
              enddo
 100          continue
            enddo
          endif
c
 20     continue
 10   continue
c
      return
      end
