      subroutine timadv(m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon,
     &                  igrd,xyordr,ncol,nrow,nlay,nspc,deltat,dx,dy,
     &                  windu,windv,depth,mapscl,saconc)
      use filunit
      use chmstry
      use bndary
      use tracer
c
c----CAMx v5.10 090918
c
c     TIMADV drives 2-D advection of timing tracers
c                          
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c          
c     Modifications:
c        02/05/03    --gwilson--  Removed SMOLAR advections solver.
c        none
c
c     Input arguments:
c        igrd              grid index
c        xyordr            order of x & y advection
c        ncol              number of columns
c        nrow              number of rows
c        nlay              number of layers
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
      include "flags.com"
c
      integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon
c
      integer xyordr
      real    saconc(m1,m2,m3,nspc)
      real    windu(m1,m2,m3)
      real    windv(m1,m2,m3)
      real    depth(ncol,nrow,nlay)
      real    mapscl(m1,m2)
      real    dx(nrow)
c
      real tarray2(2)
      integer num1d
c
      real c1d(MXCELLS)
      real v1d(MXCELLS)
      real flxarr(MXCELLS)
      real m1d(MXCELLS)
      real fpc(MXCELLS)
      real fmc(MXCELLS)
c
c-----Entry point
c
      num1d = MAX( m1, m2 )
c
      if (xyordr.eq.0) goto 200
c
c-----Advection in x-direction
c
 100  write(*,'(a20,$)') '  SA x advect ......'  
      write(iout,'(a20,$)') '  SA x advect ......'  
      do 20 k = 1,m3
        do 10 j = 2,m2-1
          do 21 ispc = ipttim,nspc
c
            l = 0
            do i = 1,m1
              l = l + 1
              v1d(l) = windu(i,j,k)
              if (i.lt.m1)
     &                 v1d(l) = 2.*v1d(l)/(mapscl(i+1,j) + mapscl(i,j))
              c1d(l) = saconc(i,j,k,ispc)*dy*depth(i+i0,j+j0,k)
              m1d(l) = mapscl(i,j)*mapscl(i,j)
            enddo
            nn = m1
            if (iadvct.eq.2) then
              call hadvbot(nn,deltat,dx(j+j0),c1d,v1d,m1d,flxarr,fpc,
     &                                                      fmc,num1d)
            elseif (iadvct.eq.3) then
              call hadvppm(nn,deltat,dx(j+j0),c1d,v1d,m1d,flxarr,num1d)
            endif
c
            l = 1
            do i = 2,m1-1
              l = l + 1
              saconc(i,j,k,ispc) = c1d(l)/dy/depth(i+i0,j+j0,k)
            enddo
  21      continue
c
  10    continue
  20  continue
      write(*,'(a,f10.3)') '   CPU = ', tarray2(1) 
      write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
      call flush(6)
      call flush(iout)
      if (xyordr.eq.0) goto 300
c
c-----Advection in y-direction
c
 200  write(*,'(a20,$)') '  SA y advect ......'  
      write(iout,'(a20,$)') '  SA y advect ......'  
      do 40 k = 1,m3
        do 30 i = 2,m1-1
         do 41 ispc = 1,nspc
            l = 0
            do j = 1,m2
              l = l + 1
              v1d(l) = windv(i,j,k)
              if (j.lt.m2)
     &          v1d(l) = 2.*v1d(l)/(mapscl(i,j+1) + mapscl(i,j))
              c1d(l) = saconc(i,j,k,ispc)*dx(j+j0)*depth(i+i0,j+j0,k)
              m1d(l) = mapscl(i,j)*mapscl(i,j)
            enddo
            nn = m2
c
            if (iadvct.eq.2) then
              call hadvbot(nn,deltat,dy,c1d,v1d,m1d,flxarr,fpc,
     &                                                    fmc,num1d)
            elseif (iadvct.eq.3) then
              call hadvppm(nn,deltat,dy,c1d,v1d,m1d,flxarr,num1d)
            endif
c
            l = 1
            do j = 1,m2
              l = l+1
              saconc(i,j,k,ispc) = c1d(l)/dx(j+j0)/depth(i+i0,j+j0,k)
            enddo
  41      continue
c
  30    continue
  40  continue
      tcpu = dtime(tarray2) 
      write(*,'(a,f10.3)') '   CPU = ', tarray2(1) 
      write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
      call flush(6)
      call flush(iout)
      if (xyordr.eq.0) goto 100
c
 300  continue
c
      return
      end
