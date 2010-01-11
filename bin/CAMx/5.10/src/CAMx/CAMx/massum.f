
      subroutine massum(m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon,
     &                  igrid,nospec,ncol,nrow,nlay,dx,dy,
     &                  depth,mapscl,conc,xmass)
      use bndary
c
c----CAMx v5.10 090918
c
c     MASSUM sums up mass on a given grid, not including boundary cells
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications:
c        8/31/06         Added map scale factor
c
c     Input arguments:
c        igrid               grid index
c        nospec              number of species
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
c        dx                  cell size in x-direction (m)
c        dy                  cell size in y-direction (m)
c        depth               cell depth (m)
c        mapscl              map scale factor
c        conc                concentration field (umol/m3)
c
c     Output arguments:
c        xmass               grid mass (umol)
c
c     Routines called:
c        none
c
c     Called by:
c        CAMx
c        AGGR00
c        CHEMRXN
c
      include "camx.prm"
c
      integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon

      real*8 xmass(nospec)
      real   mapscl(m1,m2)
      real   conc(m1,m2,m3,nospec)
      real   dx(nrow)
      real   depth(ncol,nrow,nlay)
c
      real*8 dtmp

c
c-----Entry point
c
      do 50 is = 1,nospec
        xmass(is) = 0.
        do 30 k = 1,nlay
          do 20 j = ja,jz  !2,nrow-1
            do i = ia,iz !2,ncol-1
              dtmp = conc(i,j,k,is)*dx(j+j0)
     &                         *dy*depth(i+i0,j+j0,k)/(mapscl(i,j)**2)
              xmass(is) = xmass(is) + dtmp
            enddo
  20      continue
  30    continue
c
  50  continue
c
      return
      end
