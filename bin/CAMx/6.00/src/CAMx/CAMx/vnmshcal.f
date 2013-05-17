      subroutine vnmshcal(igrd,ncol,nrow,nlay,i1,j1,ncolf,nrowf,nlayf,
     &                    height,heightf)
      use filunit
c
c----CAMx v6.00 130506
c
c     VNMSHCAL compares the vertical mesh between the coarse grid and
c     the target fine grid to ensure each layer interface matches to
c     within 5%.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c 
c     Modifications:  
c        1/28/99   Changed check on fine/coarse layer interface matching
c                  from absolute (0.1 m) to relative (5%)
c       11/05/12   Removed vertical nesting
c  
c     Input arguments:  
c        igrd                grid number of fine grid
c        ncol                number of columns in coarse grid
c        nrow                number of rows in coarse grid
c        nlay                number of layers on coarse grid
c        i1                  starting i index for the fine grid 
c        j1                  starting j index for the fine grid 
c        ncolf               number of columns in fine grid
c        nrowf               number of rows in fine grid
c        nlayf               number of layers on fine grid
c        height              layer interface height on coarse grid(m)
c        heightf             layer interface height on fine grid(m)
c             
c     Output arguments:  
c        none
c
c     Routines called:
c        none
c
c     Called by:
c        STARTUP 
c
      implicit none
      include "camx.prm"
c
      integer igrd,ncol,nrow,nlay,i1,j1,ncolf,nrowf,nlayf
      real    height(ncol,nrow,nlay)
      real    heightf(ncolf,nrowf,nlayf)
c
      integer k,kp
      real dht(MXLAYER)
c
c------Entry point
c
      do kp = 1,nlay
        dht(kp) = abs(heightf(2,2,kp) - height(i1,j1,kp))/
     &            height(i1,j1,kp)
      enddo
c
      do kp = 1,nlay
        if (dht(kp).gt.0.05) then
          write(iout,'(//,a)') 'ERROR in VNMSHCAL:'
          write(iout,*) 'Inconsistent vertical grid structure!'
          write(iout,'(a,i2)') '    Grid 1    Grid ',igrd 
          do k = 1,nlay
            write(iout,'(e10.1)') height(i1,j1,k), height(2,2,k)
          enddo
          call camxerr()
        endif
      enddo
c
      return
      end
