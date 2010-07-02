c**** FILFIN.F
c
      subroutine filfin(cncnew,cncold,igrid)
c
c-----------------------------------------------------------------------
c
c   This routine takes the concentrations in the grid array and
c   places them in the new fine grid array.  Just the cells in the
c   interior are placed into the new array.  The boundary cells are
c   resolved from the coarse grid.
c     Argument description:
c      Outputs:
c        cncnew  R  concentrations in the new grid resolution
c      Inputs:
c        cncold  R  concentrations in the old grid resolution
c        igrid   I  grid number (0 means coarse grid)
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camxtrct.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      real*4    cncnew(MXCELL,MXCELL,MXLAYR)
      real*4    cncold(MXCELL,MXCELL,MXLAYR)
      integer*4 igrid
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 icl, jcl, i, j, k
      real*4 valmax, valmin, valavg
      integer*4 imax, jmax, imin, jmin
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  --- loop over layers in grid ---
c
      do 10 k=1,nlgrid(igrid)
c
c  --- loop over cells in grid and fill cells where data is available ---
c
         do 20 j=2,nygrid(igrid)-1
c
c  --- loop over cells in this row ----
c
            if( iwest(j,igrid) .LE. 0 .OR. 
     &                               ieast(j,igrid) .LE. 0 ) goto 20
            do 30 i=iwest(j,igrid),ieast(j,igrid)
c
c  --- find index of cell in new domain resolution ---
c
                if( igrid .EQ. 0 .OR. avgtyp .EQ. FINE) then
                  icl = i
                  jcl = j
                else
                  icl = (iclbeg(igrid)-1)*nmesh(0) + i - 1
                  jcl = (jclbeg(igrid)-1)*nmesh(0) + j - 1
                endif
c
c  --- put concentration in array ---
c
                cncnew(icl,jcl,k) = cncold(i,j,k)
c
c   --- next cell ---
c
   30       continue
   20    continue
c
c  --- next layer ----
c
   10 continue
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
