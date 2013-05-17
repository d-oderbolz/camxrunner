      subroutine xyadvec(m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon,igrd,xyordr,
     &                   ncol,nrow,nlay,nrads,nspc,nspcsa,nadv,
     &                   deltat,dx,dy,windu,windv,depth,mapscl,conc,
     &                   ctop,ptrtop,fluxes,tarray2,isaptr,ipa_cel,iproc_id)
      use filunit
      use chmstry
      use bndary
      use procan
      use tracer
c
c----CAMx v5.41 121109
c
c     XYADVEC drives 2-D advection of concentrations.  This version also
c     performs the 2-D advection on DDM sensitivies, if DDM is implemented
c                          
c     Copyright 1996 - 2012
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
c       07/16/07 -bkoo-     Revised for HDDM
c       07/16/08 -bkoo-     Added DDM turn-off flag
c       11/04/09   Saving top-layer concentrations for zero-gradient vertical
c                  top boundary condition
c        3/12/10   Removed X/Y advection order flip
c        8/13/10   Calculating top BC only at met update times
c
c     Input arguments:
c        igrd              grid index
c        xyordr            order of x & y advection
c        ncol              number of columns
c        nrow              number of rows
c        nlay              number of layers
c        nrads             number of radicals
c        nspc              number of species
c        nspcsa            number of probing tool species
c        nadv              number of sub-steps per timestep
c        deltat            time step (s)
c        dx                cell size in x-direction (m)
c        dy                cell size in y-direction (m)
c        windu             wind speed in x-direction (m/s)
c        windv             wind speed in y-direction (m/s)
c        depth             layer depth (m)
c        mapscl            map-scale factor at cell centroids
c        conc              species concentrations (umol/m3)
c        tarray2           CPU timing arguments (s)
c        isaptr            pointer into tracer conc array for this grid
c        ipa_cel           gridded array to identify if cell is
c                          in a IPRM sub-domain
c        iproc_id          ID for this processor in MPI rank
c
c     Output arguments:
c        conc              species concentrations (umol/m3)
c        ctop              initial concentrations in top layer (umol/m3)
c        ptrtop            initial tracer/DDM concentrations in top layer (umol/m3)
c        fluxes            fluxes across the boundaries (umol)
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
      include "flags.inc"
c
c======================== Process Analysis Begin ====================================
c
      integer ipa_cel(ncol,nrow,nlay)
c
c========================= Process Analysis End =====================================
c
      integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon
      integer :: m_xi1,m_xi2,m_xj1,m_xj2
      integer :: m_yi1,m_yi2,m_yj1,m_yj2

      integer xyordr
      integer nadv(nlay)
      real, dimension(m1,m2,m3,nspc) :: conc
      real, dimension(m1,m2,m3) :: windu,windv
      real, dimension(m1,m2,m3) :: depth
      real :: mapscl(m1,m2), dx(nrow)
      real, dimension(m1,m2,nspc) :: ctop
      real, dimension(m1,m2,nspcsa) :: ptrtop
      real*8  fluxes(nspc,11)
      real    tarray2(2)
      integer iproc_id
      integer idx
c
      real*8  flux1,flux2
c
      real   c1d(MXCELLS)
      real   v1d(MXCELLS)
      real   a1d(MXCELLS)
      real   av1d(MXCELLS)
      real   flxarr(MXCELLS)
      real   c1d0(MXCELLS)
      real   fpc(MXCELLS)
      real   fmc(MXCELLS)
      real   fluxcls(MXTRCLS,MXCELLS)
      real   cnccls(MXTRCLS,MXCELLS)
      real   sen1d(MXCELLS,MXTRSP)
      real   sflxarr(MXCELLS)
      real*8 fluxtmp(MXSPEC+MXTRSP,8,MXLAYER)
c
      logical, external :: isbound
c
      common /comxyadvec/ fluxtmp
c
c-----Entry point
c
      num1d = MAX(m1,m2,m3)
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
c-----Store current top layer concentration if it's time
c
      do l = 1,nspc
        do j = 1,m2
          do i = 1,m1
            ctop(i,j,l) = conc(i,j,nlay,l)
          enddo
        enddo
      enddo
c
c======================== DDM Begin =======================
c
      if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
         do l = 1,nspc
           do iddm = 1,nddmsp
             do j = 1,m2
               do i = 1,m1
                 ioff = iptddm(l) + iddm - 1
                 idx =  isaptr-1 + i + m1*(j-1) + m1*m2*(nlay-1) +
     &                                            m1*m2*m3*(ioff-1)
                 ptrtop(i,j,l) = ptconc(idx) * ptop_fac(ioff)
                 enddo
             enddo
           enddo
         enddo
      endif
c
c======================== DDM End =======================
c
      if( .NOT. lmpi ) then
         m_xj1 = 2
         m_xj2 = m2-1
         m_xi1 = 2
         m_xi2 = m1-1
         m_yi1 = 2
         m_yi2 = m1-1
         m_yj1 = 2
         m_yj2 = m2-1
      else
         m_xj1 = 1
         m_xj2 = m2
         m_xi1 = 1
         m_xi2 = m1
         m_yi1 = 1
         m_yi2 = m1
         m_yj1 = 1
         m_yj2 = m2
         if( btest(ibcon,0) ) then
           m_xi1 = 2
           m_yi1 = 2
         endif
         if( btest(ibcon,1) ) then
           m_xi2 = m1-1
           m_yi2 = m1-1
         endif
         if( btest(ibcon,2) ) then
           m_xj1 = 2
           m_yj1 = 2
         endif
         if( btest(ibcon,3) ) then
           m_xj2 = m2-1
           m_yj2 = m2-1
         endif
      endif
c
c-----Advection in x-direction
c
 100  continue
c
c$omp master
c
      if( iproc_id .LE. 1 ) then
         write(*,'(a20,$)') 'x advection ......'
      endif
      write(iout,'(a20,$)') 'x advection ......'
      tcpu = dtime(tarray2)
c
c$omp end master
c
c$omp parallel default(shared)
c$omp&  private(i,j,k,ispc,l,c1d,v1d,a1d,nn,iddm,c1d0,ioff,
c$omp&          sen1d,sflxarr,flxarr,flux1,flux2,fpc,fmc,
c$omp&          ipa_idx,fluxcls,cnccls,icls,
c$omp&          istep,dtuse,av1d)
c
c$omp do schedule(dynamic)
c
      do 20 k = 1,nlay
        dtuse = deltat/nadv(k)
        do 21 istep = 1,nadv(k)
c
        do 10 j = m_xj1, m_xj2
          do i = 1,m1-1
            av1d(i) = dy*(depth(i,j,k) + depth(i+1,j,k))/
     &                (mapscl(i,j) + mapscl(i+1,j))
            v1d(i) = windu(i,j,k)
            a1d(i) = mapscl(i,j)*mapscl(i,j)/(dy*depth(i,j,k))
          enddo
          av1d(m1) = dy*depth(m1,j,k)/mapscl(m1,j)
          v1d(m1) = windu(m1,j,k)
          a1d(m1) = mapscl(m1,j)*mapscl(m1,j)/(dy*depth(m1,j,k))
c
c======================== Source Apportion Begin =======================
c
c
c  --- initialize the tracer fluxes/concs to zero ----
c
           if( ltrace ) then
           do i=1,num1d
             do icls=1,ntrcls
                fluxcls(icls,i) = 0.
                cnccls(icls,i) = 0.
             enddo
           enddo
           end if
c
c========================= Source Apportion End ========================
c
c
          do 22 ispc = nrads+1,nspc
c
            do i = 1,m1
              c1d(i) = conc(i,j,k,ispc)
            enddo
            if (btest(ibcon,0) .and. igrd.eq.1 .and. 
     &                          v1d(1).lt.0.) c1d(1) = c1d(2)
            if (btest(ibcon,1) .and. igrd.eq.1 .and. 
     &                          v1d(m1-1).gt.0.) c1d(m1) = c1d(m1-1)
c
c======================== DDM Begin =======================
c
            if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
               do iddm=1,nddmsp
                 do i = 1,m1
                   c1d0(i) = c1d(i)
                   ioff = iptddm(ispc)+iddm-1
                   idx =  isaptr-1 + i + m1*(j-1) + m1*m2*(k-1)+
     &                                               m1*m2*m3*(ioff-1)
                   sen1d(i,iddm) = ptconc(idx)
                 enddo
                 if (igrd.eq.1 .and. v1d(1).lt.0.) 
     &                                  sen1d(1,iddm) = sen1d(2,iddm)
                 if (igrd.eq.1 .and. v1d(m1-1).gt.0.) 
     &                              sen1d(m1,iddm) = sen1d(m1-1,iddm)
               enddo
            endif
c
c======================== DDM End =======================
c
            if (iadvct.eq.2) then
              call hadvbot(m1,dtuse,dx(j+j0),c1d,v1d,a1d,av1d,flxarr,
     &                     fpc,fmc,num1d)
            elseif( iadvct .eq. 3) then
              call hadvppm(m1,dtuse,dx(j+j0),c1d,v1d,a1d,av1d,flxarr,
     &                     num1d)
            endif
c
c======================== DDM Begin =======================
c
            if ( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
              if ( iadvct .eq. 2 ) then
                call bottddm(m1,dtuse,dx(j+j0),ntotsp,sen1d,
     &                       nddmsp,c1d0,c1d,fpc,fmc,v1d,a1d,av1d)
              elseif ( iadvct .eq. 3 ) then
                do iddm=1,nddmsp
                  call hadvppm(m1,dtuse,dx(j+j0),sen1d(1,iddm),
     &                                       v1d,a1d,av1d,sflxarr,num1d)
                enddo
              else ! this should never happen
                write(iout,'(//,a)') 'ERROR in XYADVEC:'
                write(iout,*) '(H)DDM must be used with BOTT or PPM.'
                call camxerr()
              endif
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
              do i = 2,m1-1
                l = l+1
                if( i .GE. ia .AND. i .LE. iz .AND. j .GE. ja 
     &                                        .AND. j .LE. jz ) then
                     if( ipa_cel(i+i0,j+j0,k) .GT. 0 ) then
                       ipa_idx = ipa_cel(i+i0,j+j0,k)
c
c-----Flux at west boundary
c
                        cipr(IPR_WADV, ipa_idx, ispc) =
     &                            cipr(IPR_WADV, ipa_idx, ispc) + 
     &                                      a1d(l)*av1d(l-1)*flxarr(l-1)
c
c-----Flux at east boundary
c
                        cipr(IPR_EADV, ipa_idx, ispc) =
     &                       cipr(IPR_EADV, ipa_idx, ispc) - 
     &                                      a1d(l)*av1d(l)*flxarr(l)
c
c-----Average volume
                        cipr(IPR_VOL, ipa_idx, ispc) =
     &                  cipr(IPR_VOL, ipa_idx, ispc) + dx(j+j0)*dy*
     &                                                   depth(i,j,k)
                        npastep(ipa_idx,ispc) = npastep(ipa_idx,ispc)+1
                     endif
                 endif
              enddo
            endif
c
c========================= Process Analysis End =====================================
c
c
c======================== Source Apportion Begin =======================
c
            if( ltrace .AND. tectyp .NE. RTRAC .AND.
     &                                         tectyp .NE. RTCMC ) then
               l = 0
               do i = 1,m1
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
            do i = m_xi1, m_xi2
              conc(i,j,k,ispc) = c1d(i)
            enddo
c
c======================== DDM Begin =======================
c
            if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
               do iddm=1,nddmsp
                 do i = 2,m1-1
                   ioff = iptddm(ispc)+iddm-1
                   idx =  isaptr-1 + i + m1*(j-1) + m1*m2*(k-1)+
     &                                               m1*m2*m3*(ioff-1)
                   ptconc(idx) = sen1d(i,iddm)
                 enddo
               enddo
             endif
c
c======================== DDM End =======================
c
c-----Sum up fluxes in east and west sides
c
            flux1 = flxarr(1)   *av1d(1)*dx(j+j0)*mapscl(2,j)**2
            flux2 = flxarr(m1-1)*av1d(m1-1)*dx(j+j0)*mapscl(m1-1,j)**2

            if( 2 .GE. ia .AND. 2 .LE. iz .AND. j .GE. ja 
     &                                    .AND. j .LE. jz ) then
               if (flux1.gt.0.0) then
                 fluxtmp(ispc,7,k)  = fluxtmp(ispc,7,k)  + flux1
               else
                 fluxtmp(ispc,8,k)  = fluxtmp(ispc,8,k)  + flux1
               endif
            endif
            if( m1-1 .GE. ia .AND. m1-1 .LE. iz .AND. j .GE. ja 
     &                                          .AND. j .LE. jz ) then
               if (flux2.lt.0.0) then
                 fluxtmp(ispc,5,k)  = fluxtmp(ispc,5,k)  - flux2
               else
                 fluxtmp(ispc,6,k)  = fluxtmp(ispc,6,k)  - flux2
               endif
            endif
  22      continue
c
c======================== Source Apportion Begin =======================
c
c  --- call routine to update the tracer concentrations
c      based on the calculated fluxes ---
c
          if( ltrace .AND. tectyp .NE. RTRAC .AND.
     &                                      tectyp .NE. RTCMC ) then
             call xfluxsa(m1,m2,m3,ntotsp,ptconc(isaptr),
     &                               2,m1-1,j,k,a1d,av1d,fluxcls,cnccls)
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
c
      tcpu = dtime(tarray2)
      if( iproc_id .LE. 1) then
         write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
         call flush(6)
      endif
      write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
      call  flush(iout)
c
c$omp end master
c
c-----Advection in y-direction
c
 200  continue
c
c$omp master
c
      if( iproc_id .LE. 1) then
         write(*,'(a20,$)') 'y advection ......'
      endif
      write(iout,'(a20,$)') 'y advection ......'
c
c$omp end master
c
c$omp parallel default(shared)
c$omp&  private(i,j,k,ispc,l,c1d,v1d,a1d,nn,iddm,c1d0,ioff,
c$omp&          sen1d,sflxarr,flxarr,flux1,flux2,fpc,fmc,
c$omp&          ipa_idx,fluxcls,cnccls,icls,
c$omp&          istep,dtuse,av1d)
c
c$omp do schedule(dynamic)
c

      do 40 k = 1,nlay
        dtuse = deltat/nadv(k)
        do 41 istep = 1,nadv(k)
c
        do 30 i = m_yi1, m_yi2
          do j = 1, m2-1   !nrow-1
            av1d(j) = (dx(j+j0)*depth(i,j,k) + 
     &                 dx(j+j0+1)*depth(i,j+1,k))/
     &                (mapscl(i,j) + mapscl(i,j+1))
            v1d(j) = windv(i,j,k)
            a1d(j) = mapscl(i,j)*mapscl(i,j)/
     &               (dx(j+j0)*depth(i,j,k))
          enddo
          av1d(m2) = dx(m2+j0)*depth(i,m2,k)/mapscl(i,m2)
          v1d(m2) = windv(i,m2,k)
          a1d(m2) = mapscl(i,m2)*mapscl(i,m2)/
     &              (dx(m2+j0)*depth(i,m2,k))
c
c======================== Source Apportion Begin =======================
c
c  --- initialize the tracer fluxes/concs to zero ----
c
          if (ltrace) then
            do j=1,num1d
               do icls=1,ntrcls
                  fluxcls(icls,j) = 0.
                  cnccls(icls,j) = 0.
               enddo
            enddo
          endif
c
c========================= Source Apportion End ========================
c
         do 42 ispc = nrads+1,nspc
c
            do j = 1,m2  !nrow
              c1d(j) = conc(i,j,k,ispc)
            enddo
            if (btest(ibcon,2) .and. igrd.eq.1 .and. 
     &                            v1d(1).lt.0.) c1d(1) = c1d(2)
            if (btest(ibcon,3) .and. igrd.eq.1 .and. 
     &                            v1d(m2-1).gt.0.) c1d(m2) = c1d(m2-1)
c
c======================== DDM Begin =======================
c
            if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
               do iddm=1,nddmsp
                 do j = 1, m2
                   c1d0(j) = c1d(j)
                   ioff = iptddm(ispc)+iddm-1
                   idx =  isaptr-1 + i + m1*(j-1) + m1*m2*(k-1)+
     &                                               m1*m2*m3*(ioff-1)
                   sen1d(j,iddm) = ptconc(idx)
                 enddo
                 if (igrd.eq.1 .and. v1d(1).lt.0.) 
     &                                     sen1d(1,iddm) = sen1d(2,iddm)
                 if (igrd.eq.1 .and. v1d(m2-1).gt.0.) 
     &                                 sen1d(m2,iddm) = sen1d(m2-1,iddm)
               enddo
            endif
c
c======================== DDM End =======================
c
            if (iadvct.eq.2) then
              call hadvbot(m2,dtuse,dy,c1d,v1d,a1d,av1d,flxarr,
     &                     fpc,fmc,num1d)
            elseif( iadvct .eq. 3) then
              call hadvppm(m2,dtuse,dy,c1d,v1d,a1d,av1d,flxarr,num1d)
            endif
c
c======================== DDM Begin =======================
c
            if ( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
              if ( iadvct .eq. 2 ) then
                call bottddm(m2,dtuse,dy,ntotsp,sen1d,nddmsp,
     &                               c1d0,c1d,fpc,fmc,v1d,a1d,av1d)
              elseif ( iadvct .eq. 3 ) then
                do iddm=1,nddmsp
                  call hadvppm(m2,dtuse,dy,sen1d(1,iddm),
     &                                       v1d,a1d,av1d,sflxarr,num1d)
                enddo
              else ! this should never happen
                write(iout,'(//,a)') 'ERROR in XYADVEC:'
                write(iout,*) '(H)DDM must be used with BOTT or PPM.'
                call camxerr()
              endif
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
              do j = 2,m2-1
                l = l+1
                if( i .GE. ia .AND. i .LE. iz .AND. j .GE. ja 
     &                                         .AND. j .LE. jz ) then
                    if( ipa_cel(i+i0,j+j0,k) .GT. 0 ) then
                      ipa_idx = ipa_cel(i+i0,j+j0,k)
c
c-----Flux at south boundary
c
                      cipr(IPR_SADV, ipa_idx, ispc) =
     &                           cipr(IPR_SADV, ipa_idx, ispc) + 
     &                                      a1d(l)*av1d(l-1)*flxarr(l-1)
c
c-----Flux at north boundary
c
                      cipr(IPR_NADV, ipa_idx, ispc) =
     &                           cipr(IPR_NADV, ipa_idx, ispc) - 
     &                                          a1d(l)*av1d(l)*flxarr(l)
c
                    endif
                endif
              enddo
            endif
c
c========================= Process Analysis End =====================================
c
c
c======================== Source Apportion Begin =======================
c
            if( ltrace .AND. tectyp .NE. RTRAC .AND.
     &                                          tectyp .NE. RTCMC ) then
               l = 0
               do j = 1, m2
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
            do j = m_yj1, m_yj2
              conc(i,j,k,ispc) = c1d(j)
            enddo
c
c======================== DDM Begin =======================
c
            if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
               do iddm=1,nddmsp
                 do j = 2,m2-1
                   ioff = iptddm(ispc)+iddm-1
                   idx =  isaptr-1 + i + m1*(j-1) + m1*m2*(k-1)+
     &                                               m1*m2*m3*(ioff-1)
                   ptconc(idx) = sen1d(j,iddm)
                 enddo
               enddo
            endif
c
c======================== DDM End =======================
c
c
c-----Sum up fluxes in north and south sides
c
            flux1 = flxarr(1)   *av1d(1)*dy*mapscl(i,2)**2
            flux2 = flxarr(m2-1)*av1d(m2-1)*dy*mapscl(i,m2-1)**2
            if(i .GE. ia .AND. i .LE. iz .AND. 2 .GE. ja 
     &                                   .AND. 2 .LE. jz ) then
               if (flux1.gt.0.0) then
                 fluxtmp(ispc,3,k)  = fluxtmp(ispc,3,k)  + flux1
               else
                 fluxtmp(ispc,4,k)  = fluxtmp(ispc,4,k)  + flux1
               endif
            endif
            if(i .GE. ia .AND. i .LE. iz .AND. m2-1 .GE. ja 
     &                                   .AND. m2-1 .LE. jz ) then
               if (flux2.lt.0.0) then
                 fluxtmp(ispc,1,k)  = fluxtmp(ispc,1,k)  - flux2
               else
                 fluxtmp(ispc,2,k)  = fluxtmp(ispc,2,k)  - flux2
               endif
            endif
  42      continue
c
c======================== Source Apportion Begin =======================
c
c  --- call routine to update the tracer concentrations
c      based on the calculated fluxes ---
c
         if( ltrace .AND. tectyp .NE. RTRAC .AND.
     &                                         tectyp .NE. RTCMC ) then
            call yfluxsa(m1,m2,m3,ntotsp,ptconc(isaptr),
     &                               i,2,m2-1,k,a1d,av1d,fluxcls,cnccls)
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
c
      tcpu = dtime(tarray2)
      if( iproc_id .LE. 1) then
         write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
         call flush(6)
      endif
      write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
      call flush(iout)
c
c$omp end master
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
c
      return
      end
