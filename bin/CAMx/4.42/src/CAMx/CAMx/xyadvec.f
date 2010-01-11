      subroutine xyadvec(igrd,xyordr,ncol,nrow,nlay,nspc,nsen,nadv,
     &                   deltat,dx,dy,windu,windv,depth,mapscl,conc,
     &                   fluxes,sens,tarray2,isaptr,ipa_cel)
c
c----CAMx v4.42 070603
c
c     XYADVEC drives 2-D advection of concentrations.  This version also
c     performs the 2-D advection on DDM sensitivies, if DDM is implemented
c                          
c     Copyright 1996-2007
c     ENVIRON International Corporation
c          
c     Modifications:
c        4/23/99   Coarse grid outflow BC's set equal to concentration in
c                  the first inner computational cells
c        4/26/99   Added Piecewise Parabolic Method for horizontal advection
c       10/30/01   Revised map scale factor application to OSAT fluxes to be
c                  more consistent with how the fluxes are used.
c       12/07/01   added instructions for OMP and rearranged some loops
c                  to facilitate parallel processing
c       01/22/02   now only calls OSAT routines if not doing RTRAC
c       01/30/02   Added code for RTRAC probing tool
c        4/10/03   X/Y advection now uses layer-dependent timestep
c       10/13/03   area weighting applied to winds rather than conc
c
c     Input arguments:
c        igrd              grid index
c        xyordr            order of x & y advection
c        ncol              number of columns
c        nrow              number of rows
c        nlay              number of layers
c        nspc              number of species
c        nsen              number of species times number of DDM parameters
c        nadv              number of sub-steps per timestep
c        deltat            time step (s)
c        dx                cell size in x-direction (m)
c        dy                cell size in y-direction (m)
c        windu             wind speed in x-direction (m/s)
c        windv             wind speed in y-direction (m/s)
c        depth             layer depth (m)
c        mapscl            map-scale factor at cell centroids
c        conc              species concentrations (umol/m3)
c        sens              sensitivity coefficient (umol/m3/parameter unit)
c        tarray2           CPU timing arguments (s)
c        isaptr            pointer into tracer conc array for this grid
c        ipa_cel           gridded array to identify if cell is
c                          in a IPRM sub-domain
c
c     Output arguments:
c        conc              species concentrations (umol/m3)
c        fluxes            fluxes across the boundaries (umol)
c        sens              sensitivity coefficient (umol/m3/parameter unit)
c
c     Routines Called:
c        HADVBOT
c        HADVPPM
c        BOTTDDM
c
c     Called by:
c        EMISTRNS
c
      include "camx.prm"
      include "bndary.com"
      include "chmstry.com"
      include "filunit.com"
      include "flags.com"
c
c======================== Source Apportion Begin =======================
c
      include "tracer.com"
c
c======================== Source Apportion End =======================
c
c
c======================== Process Analysis Begin ====================================
c
      include "procan.com"
      integer ipa_cel(ncol,nrow,nlay)
c
c========================= Process Analysis End =====================================
c
      integer xyordr
      integer nadv(nlay)
      dimension conc(ncol,nrow,nlay,nspc)
      dimension sens(ncol,nrow,nlay,nsen)
      real windu(ncol,nrow,nlay),windv(ncol,nrow,nlay),
     &     depth(ncol,nrow,nlay),mapscl(ncol,nrow),dx(nrow)
      real c1d(MX1D),v1d(MX1D),a1d(MX1D),area(MX1D),flxarr(MX1D)
      real fluxcls(MXTRCLS,MX1D), cnccls(MXTRCLS,MX1D)
      real c1d0(MX1D),fpc(MX1D),fmc(MX1D)
      real sen1d(MX1D,MXTRSP)
      real*8 fluxes(nspc,11),flux1,flux2
      real*8 fluxtmp(MXSPALL,8,MXLAYA)
      dimension tarray2(2)
c
      common /comxyadvec/ fluxtmp
c
c-----Entry point
c
c-----Intialize the local flux array to zero
c
      do i=1,nspc
        do j=1,8
          do k=1,nlay
            fluxtmp(i,j,k) = 0.
          enddo
        enddo
      enddo
c
      if( xyordr .eq. 0 ) goto 200
c
c-----Advection in x-direction
c
 100  continue
c
c$omp master
      write(*,'(a20,$)') 'x advection ......'
      write(iout,'(a20,$)') 'x advection ......'
      tcpu = dtime(tarray2)
c$omp end master
c
c$omp parallel default(shared)
c$omp&  private(i,j,k,i1,i2,ispc,l,c1d,v1d,a1d,area,nn,iddm,c1d0,ioff,
c$omp&          sen1d,flxarr,flux1,flux2,fpc,fmc,
c$omp&          ipa_idx,fluxcls,cnccls,icls,
c$omp&          istep,dtuse,avarea,areamx)
c
c$omp do schedule(dynamic)
c
      do 20 k = 1,nlay
        dtuse = deltat/nadv(k)
        do 21 istep = 1,nadv(k)
c
        do 10 j = 2,nrow-1
          i1 = 1
          i2 = ncol
          if (igrd.eq.1) then
            if (ibeg(j).eq.-999) goto 10
            i1 = ibeg(j) - 1
            i2 = iend(j) + 1
          endif
          areamx = 0.
          do i = i1,i2
            area(i) = dy*depth(i,j,k)
            areamx = amax1(areamx,area(i))
          enddo
          l = 0
          do i = i1,i2-1
            l = l + 1
            avarea = dy*(depth(i,j,k) + depth(i+1,j,k))/
     &               (mapscl(i,j) + mapscl(i+1,j))
            v1d(l) = windu(i,j,k)*avarea/areamx
            a1d(l) = mapscl(i,j)*mapscl(i,j)*areamx/area(i)
          enddo
          l = l + 1
          avarea = dy*depth(i2,j,k)/mapscl(i2,j)
          v1d(l) = windu(i2,j,k)*avarea/areamx
          a1d(l) = mapscl(i2,j)*mapscl(i2,j)*areamx/area(i2)
          nn = i2 - i1 + 1
c
c======================== Source Apportion Begin =======================
c
c
c  --- initialize the tracer fluxes/concs to zero ----
c
           do i=1,MX1D
             do icls=1,ntrcls
                fluxcls(icls,i) = 0.
                cnccls(icls,i) = 0.
             enddo
           enddo
c
c========================= Source Apportion End ========================
c
c
          do 22 ispc = 1,nspc
c
            l = 0
            do i = i1,i2
              l = l + 1
              c1d(l) = conc(i,j,k,ispc)
            enddo
            if (igrd.eq.1 .and. v1d(1).lt.0.) c1d(1) = c1d(2)
            if (igrd.eq.1 .and. v1d(nn-1).gt.0.) c1d(nn) = c1d(nn-1)
c
c======================== DDM Begin =======================
c
            if( lddm ) then
               do iddm=1,nddmsp
                 l = 0
                 do i = i1,i2
                   l = l + 1
                   c1d0(l) = c1d(l)
                   ioff = iptddm(ispc)+iddm-1
                   sen1d(l,iddm) = sens(i,j,k,ioff)
                 enddo
                 if (igrd.eq.1 .and. v1d(1).lt.0.) 
     &                                  sen1d(1,iddm) = sen1d(2,iddm)
                 if (igrd.eq.1 .and. v1d(nn-1).gt.0.) 
     &                              sen1d(nn,iddm) = sen1d(nn-1,iddm)
               enddo
            endif
c
c======================== DDM End =======================
c
            if (iadvct.eq.2) then
              call hadvbot(nn,dtuse,dx(j),c1d,v1d,a1d,flxarr,
     &                     fpc,fmc)
            elseif( iadvct .eq. 3) then
              call hadvppm(nn,dtuse,dx(j),c1d,v1d,a1d,flxarr)
            endif
c
c======================== DDM Begin =======================
c
c
            if( lddm .AND. iadvct .eq. 2 ) then
              call bottddm(nn,dtuse,dx(j),sen1d,nddmsp,c1d0,c1d,
     &                     fpc,fmc,v1d,a1d)
            endif
c
c======================== DDM End =======================
c
c
c======================== Process Analysis Begin ====================================
c
            if( lipr ) then
c
c-----Change from X-direction horizontal advection
c 
              l = 1
              do i = i1+1,i2-1
                l = l+1
c
                if( ipa_cel(i,j,k) .GT. 0 ) then
                  ipa_idx = ipa_cel(i,j,k)
c
c-----Flux at west boundary
c
                   cipr(IPR_WADV, ipa_idx, ispc) =
     &             cipr(IPR_WADV, ipa_idx, ispc) + a1d(l)*flxarr(l-1)
c
c-----Flux at east boundary
c
                   cipr(IPR_EADV, ipa_idx, ispc) =
     &             cipr(IPR_EADV, ipa_idx, ispc) - a1d(l)*flxarr(l)
c
c-----Average volume
                   cipr(IPR_VOL, ipa_idx, ispc) =
     &             cipr(IPR_VOL, ipa_idx, ispc) + dx(j)*dy*depth(i,j,k)
                   npastep(ipa_idx,ispc) = npastep(ipa_idx,ispc) + 1
                 endif
              enddo
            endif
c
c========================= Process Analysis End =====================================
c
c
c======================== Source Apportion Begin =======================
c
            if( ltrace .AND. tectyp .NE. RTRAC ) then
               l = 0
               do i = i1,i2
                 l = l + 1
                 do icls=1,ntrcls
                     fluxcls(icls,l) = fluxcls(icls,l) + flxarr(l) *
     &                                              fluxmap(ispc,icls)
                     cnccls(icls,l) = cnccls(icls,l) +
     &                           conc(i,j,k,ispc) * fluxmap(ispc,icls)
                 enddo
               enddo
            endif
c
c======================== Source Apportion End =======================
c
            l = 1
            do i = i1+1,i2-1
              l = l + 1
              conc(i,j,k,ispc) = c1d(l)
            enddo
c
c======================== DDM Begin =======================
c
            if( lddm ) then
               do iddm=1,nddmsp
                 l = 1
                 do i = i1+1,i2-1
                   l = l + 1
                   ioff = iptddm(ispc)+iddm-1
                   sens(i,j,k,ioff) = sen1d(l,iddm)
                 enddo
               enddo
             endif
c
c======================== DDM End =======================
c
c-----Sum up fluxes in east and west sides
c
            flux1 = flxarr(1)*areamx*dx(j)*mapscl(i1+1,j)**2
            flux2 = flxarr(nn-1)*areamx*dx(j)*mapscl(i2-1,j)**2
            if (flux1.gt.0.0) then
              fluxtmp(ispc,7,k)  = fluxtmp(ispc,7,k)  + flux1
            else
              fluxtmp(ispc,8,k)  = fluxtmp(ispc,8,k)  + flux1
            endif
            if (flux2.lt.0.0) then
              fluxtmp(ispc,5,k)  = fluxtmp(ispc,5,k)  - flux2
            else
              fluxtmp(ispc,6,k)  = fluxtmp(ispc,6,k)  - flux2
            endif
  22      continue
c
c======================== Source Apportion Begin =======================
c
c  --- call routine to update the tracer concentrations
c      based on the calculated fluxes ---
c
          if( ltrace .AND. tectyp .NE. RTRAC ) then
             call xfluxsa(ncol,nrow,nlay,ntotsp,ptconc(isaptr),
     &                            i1+1,i2-1,j,k,a1d,fluxcls,cnccls)
          endif
c
c======================== Source Apportion End =======================
c
c  --- next row, non-parallelized loop ---
c
  10    continue
c
c  --- next layer ---
c
  21  continue
  20  continue
c
c  --- end of parallelized loop ---
c
c$omp end parallel
c
c$omp master
      tcpu = dtime(tarray2)
      write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
      write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
c$omp end master
c
      if (xyordr.eq.0) goto 300
c
c-----Advection in y-direction
c
 200  continue
c
c$omp master
      write(*,'(a20,$)') 'y advection ......'
      write(iout,'(a20,$)') 'y advection ......'
c$omp end master
c
c$omp parallel default(shared)
c$omp&  private(i,j,k,j1,j2,ispc,l,c1d,v1d,a1d,area,nn,iddm,c1d0,ioff,
c$omp&          sen1d,flxarr,flux1,flux2,fpc,fmc,
c$omp&          ipa_idx,fluxcls,cnccls,icls,
c$omp&          istep,dtuse,avarea,areamx)
c
c$omp do schedule(dynamic)
c
      do 40 k = 1,nlay
        dtuse = deltat/nadv(k)
        do 41 istep = 1,nadv(k)
c
        do 30 i = 2,ncol-1
          j1 = 1
          j2 = nrow
          if (igrd.eq.1) then
            if (jbeg(i).eq.-999) goto 30
            j1 = jbeg(i) - 1
            j2 = jend(i) + 1
          endif
          areamx = 0.
          do j = j1,j2
            area(j) = dx(j)*depth(i,j,k)
            areamx = amax1(areamx,area(j))
          enddo
          l = 0
          do j = j1,j2-1
            l = l + 1
            avarea = (dx(j)*depth(i,j,k) + dx(j+1)*depth(i,j+1,k))/
     &               (mapscl(i,j) + mapscl(i,j+1))
            v1d(l) = windv(i,j,k)*avarea/areamx
            a1d(l) = mapscl(i,j)*mapscl(i,j)*areamx/area(j)
          enddo
          l = l + 1
          avarea = dx(j2)*depth(i,j2,k)/mapscl(i,j2)
          v1d(l) = windv(i,j2,k)*avarea/areamx
          a1d(l) = mapscl(i,j2)*mapscl(i,j2)*areamx/area(j2)
          nn = j2 - j1 + 1
c
c======================== Source Apportion Begin =======================
c
c  --- initialize the tracer fluxes/concs to zero ----
c
          do j=1,MX1D
             do icls=1,ntrcls
                fluxcls(icls,j) = 0.
                cnccls(icls,j) = 0.
             enddo
          enddo
c
c========================= Source Apportion End ========================
c
         do 42 ispc = 1,nspc
c
            l = 0
            do j = j1,j2
              l = l + 1
              c1d(l) = conc(i,j,k,ispc)
            enddo
            if (igrd.eq.1 .and. v1d(1).lt.0.) c1d(1) = c1d(2)
            if (igrd.eq.1 .and. v1d(nn-1).gt.0.) c1d(nn) = c1d(nn-1)
c
c======================== DDM Begin =======================
c
            if( lddm ) then
               do iddm=1,nddmsp
                 l = 0
                 do j = j1,j2
                   l = l + 1
                   c1d0(l) = c1d(l)
                   ioff = iptddm(ispc)+iddm-1
                   sen1d(l,iddm) = sens(i,j,k,ioff)
                 enddo
                 if (igrd.eq.1 .and. v1d(1).lt.0.) 
     &                                     sen1d(1,iddm) = sen1d(2,iddm)
                 if (igrd.eq.1 .and. v1d(nn-1).gt.0.) 
     &                                 sen1d(nn,iddm) = sen1d(nn-1,iddm)
               enddo
            endif
c
c======================== DDM End =======================
c
            if (iadvct.eq.2) then
              call hadvbot(nn,dtuse,dy,c1d,v1d,a1d,flxarr,
     &                     fpc,fmc)
            elseif( iadvct .eq. 3) then
              call hadvppm(nn,dtuse,dy,c1d,v1d,a1d,flxarr)
            endif
c
c======================== DDM Begin =======================
c
c
            if( lddm .AND. iadvct .eq. 2 ) then
              call bottddm(nn,dtuse,dy,sen1d,nddmsp,c1d0,c1d,
     &                     fpc,fmc,v1d,a1d)
            endif
c
c======================== DDM End =======================
c
c
c======================== Process Analysis Begin ====================================
c
            if( lipr ) then
c
c-----Change from Y-direction horizontal advection
c 
              l = 1
              do j = j1+1,j2-1
                l = l+1
c
                if( ipa_cel(i,j,k) .GT. 0 ) then
                  ipa_idx = ipa_cel(i,j,k)
c
c-----Flux at south boundary
c
                  cipr(IPR_SADV, ipa_idx, ispc) =
     &            cipr(IPR_SADV, ipa_idx, ispc) + a1d(l)*flxarr(l-1)
c
c-----Flux at north boundary
c
                  cipr(IPR_NADV, ipa_idx, ispc) =
     &            cipr(IPR_NADV, ipa_idx, ispc) - a1d(l)*flxarr(l)
c
                endif
              enddo
            endif
c
c========================= Process Analysis End =====================================
c
c
c======================== Source Apportion Begin =======================
c
            if( ltrace .AND. tectyp .NE. RTRAC ) then
               l = 0
               do j = j1,j2
                 l = l + 1
                 do icls=1,ntrcls
                     fluxcls(icls,l) = fluxcls(icls,l) + flxarr(l) *
     &                                              fluxmap(ispc,icls)
                     cnccls(icls,l) = cnccls(icls,l) +
     &                           conc(i,j,k,ispc) * fluxmap(ispc,icls)
                 enddo
              enddo
            endif
c
c======================== Source Apportion End =======================
c
            l = 1
            do j = j1+1,j2-1
              l = l+1
              conc(i,j,k,ispc) = c1d(l)
            enddo
c
c======================== DDM Begin =======================
c
            if( lddm ) then
               do iddm=1,nddmsp
                 l = 1
                 do j = j1+1,j2-1
                   l = l + 1
                   ioff = iptddm(ispc)+iddm-1
                   sens(i,j,k,ioff) = sen1d(l,iddm)
                 enddo
               enddo
            endif
c
c======================== DDM End =======================
c
c
c-----Sum up fluxes in north and south sides
c
            flux1 = flxarr(1)*areamx*dy*mapscl(i,j1+1)**2
            flux2 = flxarr(nn-1)*areamx*dy*mapscl(i,j2-1)**2
            if (flux1.gt.0.0) then
              fluxtmp(ispc,3,k)  = fluxtmp(ispc,3,k)  + flux1
            else
              fluxtmp(ispc,4,k)  = fluxtmp(ispc,4,k)  + flux1
            endif
            if (flux2.lt.0.0) then
              fluxtmp(ispc,1,k)  = fluxtmp(ispc,1,k)  - flux2
            else
              fluxtmp(ispc,2,k)  = fluxtmp(ispc,2,k)  - flux2
            endif
  42      continue
c
c======================== Source Apportion Begin =======================
c
c  --- call routine to update the tracer concentrations
c      based on the calculated fluxes ---
c
          if( ltrace .AND. tectyp .NE. RTRAC ) then
             call yfluxsa(ncol,nrow,nlay,ntotsp,
     &                    ptconc(isaptr),i,j1+1,j2-1,k,a1d,
     &                    fluxcls,cnccls)
          endif
c
c======================== Source Apportion End =======================
c
  30    continue
c
c  --- next layer, end of parallelized loop ---
c
  41  continue
  40  continue
c
c$omp end parallel
c
c$omp master
      tcpu = dtime(tarray2)
      write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
      write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
c$omp end master
c
      if (xyordr.eq.0) goto 100
c
 300  continue
c
c  ---- put fluxes into global array ----
c
      do i=1,nspc
        do j=1,8
          do k=1,nlay
            fluxes(i,j) = fluxes(i,j) + fluxtmp(i,j,k)
          enddo
        enddo
      enddo
c
      call flush(6)
      call flush(iout)
      return
      end
