c**** CRS2FIN.F
c
      subroutine crs2fin(cncfin,cnccrs,igrid)
c
c-----------------------------------------------------------------------
c
c   This routine takes the concentrations in the coarse grid array and
c   resolves it to the fine grid resolution.  Just the internal cells
c   are handled by this routine, since we don't have enough data for
c   the bi-linear interpolation at the edges.
c     Argument description:
c      Outputs:
c        cncfin  R  concentrations in the new grid resolution
c      Inputs:
c        cnccrs  R  concentrations in the old grid resolution
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
      real*4    cncfin(MXCELL,MXCELL,MXLAYR)
      real*4    cnccrs(MXCELL,MXCELL,MXLAYR)
      integer*4 igrid
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
c    grdtrp   R   returns the interpolated concentration
c
      real*4 grdtrp
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 icl, jcl, icrs, jcrs, i, j, k
      integer*4 idxwst(MXCELL), idxest(MXCELL)
      integer*4 idxsth(MXCELL), idxnth(MXCELL)
      real*4    xloc, yloc, grdval, xleft, xright, ybelow, yabove
      real*4    delx, dely
      logical*4 luse
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  --- set the cell widths for the interpolation ---
c
       if( igrid .EQ. 0 ) then
           delx = deltax 
           dely = deltay 
       else
           delx = grdelx(igrid)
           dely = grdely(igrid)
       endif
c
c  --- load the cell boundary definition into local arrays ----
c
      do 10 j=1,nygrid(igrid)
         idxwst(j) = iwest(j,igrid)
         idxest(j) = ieast(j,igrid)
   10 continue
      do 20 i=1,nxgrid(igrid)
         idxsth(i) = isouth(i,igrid)
         idxnth(i) = inorth(i,igrid)
   20 continue
c
c  --- loop over layers in grid ---
c
      do 30 k=1,nlgrid(igrid)
c
c  --- loop over cells in grid and perform interpolation on
c      cells where data is available ----
c
         do 40 jcrs=2,nygrid(igrid)-1
c
c  --- set the coordinates for the extent of this row ----
c
            if( igrid .EQ. 0 ) then
               xleft = (idxwst(jcrs)-1) * deltax  + grxorg(igrid)
               xright = idxest(jcrs) * deltax  + grxorg(igrid)
            else
               xleft = (idxwst(jcrs)-1) * grdelx(igrid) + grxorg(igrid)
               xright = idxest(jcrs) * grdelx(igrid) + grxorg(igrid)
            endif
c
c  --- loop over cells in this row ----
c
            if( idxwst(jcrs) .LE. 0 .OR. idxest(jcrs) .LE. 0 ) goto 40
            do 50 icrs=idxwst(jcrs),idxest(jcrs)
c
c  --- set the coordinates for the extent of this column ----
c
               if( igrid .EQ. 0 ) then
                  ybelow = (idxsth(icrs)-1) * deltay + gryorg(igrid)
                  yabove = idxnth(icrs) * deltay + gryorg(igrid)
               else
                  ybelow = (idxsth(icrs)-1) * grdely(igrid) + 
     &                                                  gryorg(igrid)
                  yabove = idxnth(icrs) * grdely(igrid) + 
     &                                                 gryorg(igrid)
               endif
c
c  --- loop over the meshing factor in each dimension ---
c
               do 60 j=1,nmesh(igrid)
                   do 70 i=1,nmesh(igrid)
c
c  --- find index of cell in new domain resolution ---
c      labels coarse grid using fine grid coordinates
c
                      if( igrid .EQ. 0 ) then
                         icl = (icrs-1)*nmesh(igrid) + i
                         jcl = (jcrs-1)*nmesh(igrid) + j
                      else
                         icl = (iclbeg(igrid)-1)*nmesh(0) + 
     &                                        (icrs-2)*nmesh(igrid) + i
                         jcl = (jclbeg(igrid)-1)*nmesh(0) + 
     &                                        (jcrs-2)*nmesh(igrid) + j
                      endif
c
c  --- find the corrdinates of the centroid of this cell ---
c      grdelx = cell width of new domain
c
                      xloc = (FLOAT( icl )-0.5) * grdelx(0) + grxorg(0)
                      yloc = (FLOAT( jcl )-0.5) * grdely(0) + gryorg(0)
c
c   --- if new cell centroid is at least a half a cell away from any
c       edge the we have data, so call function to perform the interpolation ---
c
                      luse = .TRUE.
                      if((xloc - xleft ) .LE. 0.5001*delx) luse=.FALSE.
                      if((xright - xloc ) .LE. 0.5001*delx) luse=.FALSE.
                      if((yloc - ybelow ) .LE. 0.5001*dely) luse=.FALSE.
                      if((yabove - yloc ) .LE. 0.5001*dely) luse=.FALSE.
                      if( luse ) then
                         grdval = grdtrp( xloc, yloc, cnccrs, k, 
     &                                 nxgrid(igrid), nygrid(igrid), 
     &                                 grxorg(igrid), gryorg(igrid),
     &                                 delx, dely, idxwst, idxest,
     &                                                 idxsth, idxnth ) 
                          cncfin(icl,jcl,k) = grdval
                      else
                          cncfin(icl,jcl,k) = cnccrs(icrs,jcrs,k)
                      endif
c
c   --- next cell ---
c
   70              continue
   60          continue
c
c  --- next coarse grid cell ---
c
   50       continue
   40    continue
c
c  --- next layer ----
c
   30 continue
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
