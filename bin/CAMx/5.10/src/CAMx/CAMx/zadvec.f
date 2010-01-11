      subroutine zadvec(m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon,
     &                  losat,igrd,ncol,nrow,nlay,nspc,nsen,deltat,dx,
     &                  dy,densfac,idfin,tpconc,depth,entrn,
     &                  dilut,tempk,press,species,conc,fluxes,
     &                  tpsens,isaptr,tarray2,ipa_xy,ipa_lay,iproc_id)
      use filunit
      use chmstry
      use bndary
      use procan
      use rtracchm
      use tracer
c
c----CAMx v5.10 090918
c
c     ZADVEC drives vertical transport of concentrations.  The operation
c     is performed on the regular model species (losat = False) or source
c     apportionment concentrations (losat = True).
c     This version also performs vertical transport of sensitivities 
c     if DDM is enabled.
c                          
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c          
c     Modifications:
c        12/3/99   Top boundary condition is converted from ppm to umol/m3
c                  using extrapolated density in an overlying layer
c                  that is assumed to be the same thickness as the top
c                  layer of the model
c       12/07/01   added instructions for OMP
c       01/30/02   Added code for RTRAC probing tool
c        3/06/06   Revised top BC approach; removed top dummy layer
c       05/01/07   Fixed bug that was causing PM PSAT species to
c                  be converted to micro-moles
c       07/16/07 -bkoo-     Added check for HDDM
c       07/16/08 -bkoo-     Added DDM turn-off flag
c       04/09/09   Improved definition of atmospheric density at top of model
c
c     Input arguments:
c                          OSAT concentrations.
c        igrd              grid index
c        ncol              number of columns
c        nrow              number of rows
c        nlay              number of layers
c        nspc              number of species
c        nsen              number of species times number of DDM parameters
c                          or 1, whichever is larger
c        deltat            time step (s)
c        dx                cell size in x-direction (m)
c        dy                cell size in y-direction (m)
c        densfac           density conversion factor (mol/m3)
c        idfin             map of nested grids in this grid
c        tpconc            top concentrations (ppm)
c        depth             layer depth (m)
c        entrn             entrainment rate (m/s)
c        dilut             dilution rate (m/s)
c        tempk             temperature (K)
c        press             pressure (mb)
c        species           species names
c        conc              species concentrations (umol/m3)
c        tarray2           CPU timing arguments (s)
c        ipa_xy            2-D gridded array to identify if cell is
c                          in a IPRM sub-domain
c        ipa_lay           3-D gridded array to identify which IPRM sub-domain
c                          each layer is in
c        iproc_id          ID for this processor in MPI rank
c
c     Output arguments:
c        conc              species concentrations (umol/m3)
c        fluxes            fluxes across the boundaries (umol)
c
c     Routines Called:
c        VRTSLV
c
c     Called by:
c        EMISTRNS
c
      implicit none
      include "camx.prm"
c
      integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon
      integer :: m_xi1,m_xi2,m_xj1,m_xj2
      integer :: m_yi1,m_yi2,m_yj1,m_yj2
      integer :: m_zi1,m_zi2,m_zj1,m_zj2
c
      integer      ncol
      integer      nrow
      integer      nlay
      integer      nspc
      integer      igrd
      integer      nsen
      character*10 species(nspc)
      real         conc(m1,m2,m3,nspc)
      real         tpconc(nspc)
      real         tarray2(2)
      real         tpsens(nsen)
      integer      isaptr
      real, dimension(m1,m2,m3) :: entrn,dilut,tempk,press
      real, dimension(ncol,nrow,nlay) :: depth
      real         dx(nrow)
      integer      idfin(m1,m2)
      logical      losat
      real*8       fluxes(nspc,11)
      real         deltat
      real         dy
      real         densfac
      integer      iproc_id
c
c===========================DDM Begin=================================
c
      integer ls,isen,ioff
c
c===========================DDM End===================================
c
c======================== Process Analysis Begin ====================================
c
      integer ipa_xy(ncol,nrow)
      integer ipa_lay(ncol,nrow,nlay)
      integer ipa_idx
c
      logical ldoipts
c
      real*8 fluxtop
c
      integer ispc,j,i1,i2,i,k,idx
      real rho,rhotop,convfac,tcpu,dtime
      real ttop,ptop,dtdz,tavg
c
      real c1d(MXLAYER+1)
      real d1d(MXLAYER)
      real ent1d(MXLAYER)
      real dil1d(MXLAYER)
      real sen1d(MXLAYER*(MXTRSP+1))
      real fc1(MXLAYER)
      real fc2(MXLAYER)
      real fc3(MXLAYER)
c
c-----Entry point
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
c-----Vertical advection, entrainment, and dilution
c
      if( iproc_id .LE. 1 ) then
        if( .NOT. losat ) then
          write(*,'(a20,$)') 'z advection ......'
        else
          write(*,'(a20,$)') 'SA z advection ...'
        endif
      endif
      if( .NOT. losat ) then
         write(iout,'(a20,$)') 'z advection ......'
      else
         write(iout,'(a20,$)') 'SA z advection ...'
      endif
c
c$omp parallel default(shared)
c$omp&  private(ispc,i,j,k,ent1d,dil1d,d1d,c1d,
c$omp&          rho,rhotop,convfac,ls,isen,ioff,sen1d,fluxtop,
c$omp&          ttop,ptop,dtdz,tavg,ldoipts,ipa_idx,fc1,
c$omp&          fc2,fc3)
c
c$omp do schedule(dynamic)
c
      do 61 ispc = 1,nspc
        do 60 j = m_zj1, m_zj2   !2,nrow-1
          do 50 i = m_zi1, m_zi2  !2,ncol-1
c
c-----Skip cells occupied by child grids
c
            if (idfin(i,j).gt.igrd) goto 50
            do k = 1,nlay
              ent1d(k) = entrn(i,j,k)
              dil1d(k) = dilut(i,j,k)
              d1d(k) = depth(i+i0,j+j0,k)
              c1d(k) = conc(i,j,k,ispc)
            enddo
            rho = press(i,j,nlay)/tempk(i,j,nlay)
            dtdz = 2.*(tempk(i,j,nlay) - tempk(i,j,nlay-1))/
     &                (d1d(nlay) + d1d(nlay-1))
            ttop = tempk(i,j,nlay) + dtdz*d1d(nlay)/2.
            tavg = (ttop + tempk(i,j,nlay))/2.
            ptop = press(i,j,nlay)*exp(-9.8*d1d(nlay)/(2.*287.*tavg))
            rhotop = ptop/ttop
            convfac = densfac*273.*rhotop/1013.
            if (ispc.gt.ngas .and. .not.losat) then
              convfac = 1.
            endif
            if( losat .AND. (tectyp .NE. RTRAC .AND. tectyp .NE. RTCMC)
     &          .AND. .NOT. lsagas(ispc) ) then
                   convfac = 1.
            endif
            if( losat .AND. (tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC)
     &          .AND. ispc .GT. nrtgas ) then
              convfac = 1.
            endif
            c1d(nlay+1) = convfac*tpconc(ispc)
c
c================================DDM Begin==============================
c
            if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
              ls = 0
              do isen = 1,nddmsp
                ioff = iptddm(ispc)+isen-1
                do k = 1,nlay
                  ls = ls + 1
                  idx =  isaptr-1 + i + m1*(j-1) + m1*m2*(k-1)+ 
     &                                               m1*m2*m3*(ioff-1)
                  sen1d(ls) = ptconc(idx)
                enddo
                ls = ls + 1
                idx =  isaptr-1 + i + m1*(j-1) + m1*m2*(nlay-1)+ 
     &                                               m1*m2*m3*(ioff-1)
                sen1d(ls) = convfac*tpsens(ioff)
              enddo
            endif
c
c================================DDM End================================
c
c
c======================== Process Analysis Begin ====================================
c
            ldoipts = .FALSE.
            if ( .NOT. ltrace .AND. lipr ) then
                if( i .GE. ia .AND. i .LE. iz .AND. j .GE. ja
     &                                         .AND. j .LE. jz ) then
                   if( ipa_xy(i+i0,j+j0) .GT. 0 ) ldoipts = .TRUE.
                endif
            endif
c
c
c========================= Process Analysis End =====================================
c
c-----Solve vertical mass adjustments using Crank-Nicholson solver
c
            call vrtslv(nlay,i,j,igrd,deltat,ent1d,dil1d,d1d,c1d,
     &                  fluxtop,sen1d,species(ispc),fc1,fc2,fc3,ldoipts)
c
c======================== Process Analysis Begin ====================================
c
            if( ldoipts ) then
               do k=1,nlay
                  if( ipa_lay(i+i0,j+j0,k) .GT. 0 ) then
                    ipa_idx = ipa_lay(i+i0,j+j0,k)
c
c-----Concentration change from vertical advection
c
                    cipr(IPR_BADV, ipa_idx, ispc) =
     &                            cipr(IPR_BADV, ipa_idx, ispc) + fc1(k)
c
                    cipr(IPR_TADV, ipa_idx, ispc) =
     &                           cipr(IPR_TADV, ipa_idx, ispc) + fc2(k)
c
                    cipr(IPR_DADV, ipa_idx, ispc) =
     &                           cipr(IPR_DADV, ipa_idx, ispc) + fc3(k)
c
                  endif
               enddo
            endif
c
c========================= Process Analysis End =====================================
c
            do k = 1,nlay
              conc(i,j,k,ispc) = c1d(k)
            enddo

            if(i .GE. ia .AND. i .LE. iz .AND. j .GE. ja 
     &                                     .AND. j .LE. jz ) then
                if (fluxtop.lt.0) then
                  fluxes(ispc,9)  = fluxes(ispc,9) - 
     &                                 fluxtop*dx(j+j0)*dy*deltat
                else
                  fluxes(ispc,10) = fluxes(ispc,10) - 
     &                                fluxtop*dx(j+j0)*dy*deltat
                endif
            endif
c
c================================DDM Begin=============================
c
            if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
              ls = -1
              do isen = 1, nddmsp
                ls = ls + 1
                ioff = iptddm(ispc)+isen-1
                do k = 1, nlay
                  ls = ls + 1
                  idx =  isaptr-1 + i + m1*(j-1) + m1*m2*(k-1)+               
     &                                               m1*m2*m3*(ioff-1)
                  ptconc(idx) = sen1d(ls)
                enddo
              enddo
            endif
c
c================================DDM End===============================
c
  50      continue
  60    continue
  61  continue
c
c$omp enddo
c
c$omp end parallel
c
      tcpu = dtime(tarray2) 
      if( iproc_id .LE. 1 ) then
        write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
        call flush(6)
      endif
      write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
      call flush(iout)
c
      return
      end
