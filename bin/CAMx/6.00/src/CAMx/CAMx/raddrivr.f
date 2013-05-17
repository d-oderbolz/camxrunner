      subroutine raddrivr
      use grid
      use chmstry
      use bndary
      use camxfld
      use camxcom
c
c----CAMx v6.00 130506
c
c     RADDRIVR initializes radical concentrations for the grids
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications:  
c        12/11/00 - initialize radicals to 1.0e-10 - gy  
c
c     Input arguments:
c        none
c
c     Output arguments:
c        none
c
c     Routines called:
c
c     Called by:
c        CAMx
c
      include "camx.prm"
c
c-----Entry point
c
c
c-----Initialize radicals
c
      do igrd = 1,ngrid
        do k = 1,nlay(igrd)
          do j = 2,nrow(igrd)-1
            do i = 2,ncol(igrd)-1
              n2d = i + (j-1)*ncol(igrd)
              n3d = n2d + ncol(igrd)*nrow(igrd)*(k-1)
              do l = 1,nrad
                n4d = n3d + ncol(igrd)*nrow(igrd)*nlay(igrd)*(l-1)
                conc(iptr4d(igrd)-1+n4d) = 1.0e-10
              enddo
            enddo
          enddo
  10      continue
        enddo
      enddo
c
      return
      end 
