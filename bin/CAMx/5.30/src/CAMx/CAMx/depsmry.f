      subroutine depsmry(igrid,ncol,nrow,nspc,vdep)
      use filunit
      use chmstry
      use bndary
c 
c----CAMx v5.30 101223
c
c     DEPSMRY finds the maximum, minimum, and mean of deposition
c     velocity, and writes out a table
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments: 
c        igrid               grid index
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
c        nspc                number of species
c        vdep                deposition velocity (m/s)
c
c     Output arguments:
c        none
c     
c     Routines Called: 
c        none
c             
c     Called by: 
c        CAMx
c
      include "camx.prm"
c
      real vdep(ncol,nrow,nspc)
c
c-----Entry point
c
      write(idiag,*)
      write(idiag,'(a,i3)') 'DRY DEPOSITION VELOCITY (cm/s) FOR GRID ',
     &                      igrid
      write(idiag,'(a,3a12)') 'SPECIES   ','MAX','MIN','MEAN'
c
c-----Determin min, max, avg for each species
c
      do 100 l=1,nspc
        vmax = 0.
        csum = 0.
        kount = 0
        vmin = 1.e6
        do 30 j = 2,nrow-1
          do i = 2,ncol-1
            kount = kount + 1
            vmax = amax1(vmax,vdep(i,j,l))
            vmin = amin1(vmin,vdep(i,j,l))
            csum = csum + vdep(i,j,l)
          enddo
  30    continue
        vmean = csum/kount
        write(idiag,'(a10,3e12.3)') spname(l),vmax*100.,vmin*100.,
     &                              vmean*100.
  100 continue
      write(idiag,*)
      call flush(idiag)
c
      return
      end
