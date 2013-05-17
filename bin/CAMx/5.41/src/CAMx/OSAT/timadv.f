      subroutine timadv(iproc_id,m1,m2,m3,joff,
     &                  igrd,xyordr,nrow,nspc,deltat,dx,dy,
     &                  windu,windv,depth,mapscl,saconc)
      use filunit
      use chmstry
      use bndary
      use tracer
      use node_mod
c
      implicit none
c
c----CAMx v5.41 121109
c
c     TIMADV drives 2-D advection of timing tracers
c                          
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c          
c     Modifications:
c        02/05/03    --gwilson--  Removed SMOLAR advections solver.
c        none
c
c     Input arguments:
c        iproc_id          process ID for MPI
c        igrd              grid index
c        xyordr            order of x & y advection
c        nrow              number of rows
c        nspc              number of species
c        deltat            time step (s)
c        dx                cell size in x-direction (m)
c        dy                cell size in y-direction (m)
c        windu             wind speed in x-direction (m/s)
c        windv             wind speed in y-direction (m/s)
c        depth             layer depth (m)
c        mapscl            map-scale factor at cell centroids
c        saconc            species concentrations (umol/m3)
c
c     Output arguments:
c        saconc             species concentrations (umol/m3)
c
c     Routines Called:
c        HADVSMO
c        HADVBOT
c
c     Called by:
c        EMISTRNS
c
      include "camx.prm"
      include "flags.inc"
c
      integer :: iproc_id
      integer :: m1,m2,m3,joff
c
      integer xyordr
      integer nrow
      integer nspc
      integer igrd
      real    deltat
      real    dy
      real    tcpu
      real    saconc(m1,m2,m3,nspc)
      real    windu(m1,m2,m3)
      real    windv(m1,m2,m3)
      real    depth(m1,m2,m3)
      real    mapscl(m1,m2)
      real    dx(nrow)
c
      real tarray2(2)
      integer num1d
c
      real c1d(MXCELLS)
      real v1d(MXCELLS)
      real a1d(MXCELLS)
      real av1d(MXCELLS)
      real flxarr(MXCELLS)
      real fpc(MXCELLS)
      real fmc(MXCELLS)
c
      integer :: m_xi1,m_xi2,m_xj1,m_xj2
      integer :: m_yi1,m_yi2,m_yj1,m_yj2
      integer :: i, j, ispc, k
c
      real dtime
c
c-----Entry point
c
      num1d = MAX( m1, m2 )
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
c
c$omp master
c
      if( iproc_id .LE. 1 ) then
        write(*,'(a20,$)') '  SA x advect ......'  
        write(iout,'(a20,$)') '  SA x advect ......'  
      endif
c
c$omp end master
c
c$omp parallel default(shared)
c$omp&  private(i,j,k,ispc,c1d,v1d,a1d,av1d,flxarr,fpc,fmc)
c
c$omp do schedule(dynamic)
c
      do 20 k = 1,m3
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
          do 21 ispc = ipttim,nsaspc
c
            do i = 1,m1
              c1d(i) = MAX( BNDLPT, saconc(i,j,k,ispc) )
            enddo
            if (btest(ibcon,0) .and. igrd.eq.1 .and.
     &                          v1d(1).lt.0.) c1d(1) = c1d(2)
            if (btest(ibcon,1) .and. igrd.eq.1 .and.
     &                          v1d(m1-1).gt.0.) c1d(m1) = c1d(m1-1)
            if (iadvct.eq.2) then
              call hadvbot(m1,deltat,dx(j+joff),c1d,v1d,a1d,av1d,flxarr,fpc,
     &                                                      fmc,num1d)
            elseif (iadvct.eq.3) then
              call hadvppm(m1,deltat,dx(j+joff),c1d,v1d,a1d,av1d,flxarr,num1d)
            endif
c
            do i = m_xi1, m_xi2
              saconc(i,j,k,ispc) = MAX( BNDLPT, c1d(i) )
            enddo
  21      continue
c
  10    continue
  20  continue
c
c$omp end parallel
c
c$omp master
c
      if( iproc_id .LE. 1 ) then
         write(*,'(a,f10.3)') '   CPU = ', tarray2(1) 
         write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
         call flush(6)
         call flush(iout)
c
c-----Advection in y-direction
c
          write(*,'(a20,$)') '  SA y advect ......'  
          write(iout,'(a20,$)') '  SA y advect ......'  
      endif
c
c$omp end master
c
c
c$omp parallel default(shared)
c$omp&  private(i,j,k,ispc,c1d,v1d,a1d,av1d,flxarr,fpc,fmc)
c
c$omp do schedule(dynamic)
c
      do 40 k = 1,m3
        do 30 i = m_yi1, m_yi2
          do j = 1,m2-1
            av1d(j) = (dx(j+joff)*depth(i,j,k) +
     &                 dx(j+joff+1)*depth(i,j+1,k))/
     &                (mapscl(i,j) + mapscl(i,j+1))
            v1d(j) = windv(i,j,k)
            a1d(j) = mapscl(i,j)*mapscl(i,j)/
     &               (dx(j+joff)*depth(i,j,k))
          enddo
          av1d(m2) = dx(m2+joff)*depth(i,m2,k)/mapscl(i,m2)
          v1d(m2) = windv(i,m2,k)
          a1d(m2) = mapscl(i,m2)*mapscl(i,m2)/
     &              (dx(m2+joff)*depth(i,m2,k))
c
          do 41 ispc = ipttim,nsaspc
c
            do j = 1,m2  !nrow
              c1d(j) = MAX( BNDLPT, saconc(i,j,k,ispc) )
            enddo

            if (btest(ibcon,2) .and. igrd.eq.1 .and.
     &                            v1d(1).lt.0.) c1d(1) = c1d(2)
            if (btest(ibcon,3) .and. igrd.eq.1 .and.
     &                            v1d(m2-1).gt.0.) c1d(m2) = c1d(m2-1)
c
            if (iadvct.eq.2) then
              call hadvbot(m2,deltat,dy,c1d,v1d,a1d,av1d,flxarr,fpc,
     &                                                    fmc,num1d)
            elseif (iadvct.eq.3) then
              call hadvppm(m2,deltat,dy,c1d,v1d,a1d,av1d,flxarr,num1d)
            endif
c
  41      continue
c
  30    continue
c
  40  continue
c
c$omp end parallel
c
c$omp master
c
      tcpu = dtime(tarray2) 
      if( iproc_id .LE. 1 ) then
        write(*,'(a,f10.3)') '   CPU = ', tarray2(1) 
        write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
        call flush(6)
        call flush(iout)
      endif
c
c$omp end master
c
      return
      end
