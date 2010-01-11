      subroutine wrfgcon(iendat,endtim)
      use camxcom
      use filunit
      use camxfld
      use chmstry
      use grid
c
c----CAMx v5.10 090918
c
c     WRFGCON writes the instantaneous and average concentration fields
c     for the fine grids.
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications:
c        8/23/06             Now only writes instant restart file; only a
c                            single instant restart file is written
c                            at end of simulation
c
c     Input arguments:
c        iendat              current ending date (YYJJJ)
c        endtim              current ending time (HHMM)
c
c     Output arguments:
c        none
c
c     Rountines Called:
c        none
c
c     Called by:
c        CAMx
c
      include 'camx.prm'
c
      integer   iendat
      real      endtim
c
      integer   ifgptr(MXGRID)
      integer   ifglvl(MXGRID)
      real      cnctmp(MXCELLS,MXCELLS)
c
      integer   igrd, idx, i, j, k, l
c
c-----Entry point
c
c-----Set the fine grid pointers
c
      do i = 1,ngrid
        ifglvl(i) = 0
      enddo
      do i = 1,ngrid
        do j = 1,nchdrn(i)
          idch = idchdrn(j,i)
          ifgptr(idch) = i - 1 
          ifglvl(idch) = ifglvl(idch) + 1
          do k = 1,nchdrn(idch)
            ifglvl(idchdrn(k,idch)) = ifglvl(idchdrn(k,idch)) + 1
          enddo
        enddo
      enddo
c
c-----Write the file description header
c
      write(ifconc) runmsg
      write(ifconc) ngrid-1, nspec
      write(ifconc) (spname(i),i=1,nspec)
c
      do 20 igrd = 2,ngrid
        write(ifconc) inst1(igrd),jnst1(igrd),inst2(igrd),
     &                jnst2(igrd),meshold(igrd),meshold(igrd),
     &               ncol(igrd),nrow(igrd),nlay(igrd),
     &               ifgptr(igrd),ifglvl(igrd)
   20 continue
c
c-----Write time span
c
      write(ifconc) endtim, iendat
c
c-----Write the concentration data for this hour
c
      do 30 igrd = 2,ngrid
        do 40 l = 1,nspec
          do 50 k = 1,nlay(igrd)
            do 60 j = 1,nrow(igrd)
              do 70 i = 1,ncol(igrd)
                idx = i + ncol(igrd)*(j-1) + 
     &                ncol(igrd)*nrow(igrd)*(k-1) +
     &                ncol(igrd)*nrow(igrd)*nlay(igrd)*(l-1)
                cnctmp(i,j) = conc(iptr4d(igrd)-1+idx)
   70         continue
   60       continue
            write(ifconc) ((cnctmp(i,j),i=1,ncol(igrd)),j=1,nrow(igrd))
   50     continue
   40   continue
   30 continue
c
      return
      end
