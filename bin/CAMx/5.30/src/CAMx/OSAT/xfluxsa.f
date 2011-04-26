c*** XFLUXSA
c
      subroutine xfluxsa(numcol,numrow,numlay,nspc,saconc,
     &               iclbeg,iclend,jcell,kcell,area,av1d,fluxcls,cnccls)
      use tracer
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c   Description:
c     This routine updates the tracer concentrations arrays in the
c     common block variables by applying the appropriate flux value
c     calculated from the regular model transport routine.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       numcol   I  number of columns in the grid
c       numrow   I  number of rows in the grid
c       numlay   I  number of layers in the grid
c       nspc     I  number of species
c       saconc   R  3-D concentration arrays
c       iclbeg   I  beginning cell in the X direction
c       iclend   I  ending cell in the X direction
c       jcell    I  cell index in the Y direction
c       kcell    I  the vertical grid location of current layer
c       area     R  area adjustment vector
c       av1d     R  interfacial area adjustment vector
c       fluxcls  R  flux for the each tracer class
c       cnccls   R  regular model concentration for each tracer class
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     10/02/98   --gwilson--  Orignal development
c     10/30/01   --cemery--   Added map scale factor to flux div
c                             calculation 
c     11/04/03   --cemery--   Revised to parallel changes to reg model
c                             advection
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer     numcol
      integer     numrow
      integer     numlay
      integer     nspc
      real        saconc(numcol,numrow,numlay,nspc)
      integer     iclbeg
      integer     iclend
      integer     jcell
      integer     kcell
      real        area(*),av1d(*)
      real        fluxcls(MXTRCLS,*)
      real        cnccls(MXTRCLS,*)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   icl, i1d, ispc
c
      real flux(MXCELLS)
      real cnctmp(MXCELLS)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- Loop over each tracer class ---
c
      do icls=1,ntrcls
c
c   --- loop over each tracer species in this class ---
c
          do ispc=iptcls(icls),nptcls(icls)
c
c   --- make sure all values are above lower bound, to start ----
c
            do icl=iclbeg,iclend
              saconc(icl,jcell,kcell,ispc) =
     &                      AMAX1(saconc(icl,jcell,kcell,ispc),BNDLPT)
            enddo
c
c   --- normalize the flux on the boundary ---
c
            i1d = 1
            if( fluxcls(icls,i1d) .GE. 0 ) then
               flux(i1d) = saconc(iclbeg-1,jcell,kcell,ispc) * 
     &                           fluxcls(icls,i1d) / cnccls(icls,i1d)
            else
               flux(i1d) = saconc(iclbeg,jcell,kcell,ispc) * 
     &                           fluxcls(icls,i1d) / cnccls(icls,i1d+1)
            endif
c
c   --- loop over all interior cells in this row ---
c
            do icl=iclbeg,iclend
              i1d = i1d + 1 
c
c   ---  apply flux to the NOx tracer species ---
c
              if( fluxcls(icls,i1d) .GE. 0. ) then
                flux(i1d) = saconc(icl,jcell,kcell,ispc) *
     &                            fluxcls(icls,i1d) / cnccls(icls,i1d)
              else
                flux(i1d) = saconc(icl+1,jcell,kcell,ispc) *
     &                          fluxcls(icls,i1d) / cnccls(icls,i1d+1)
              endif
              cnctmp(i1d) = saconc(icl,jcell,kcell,ispc) - 
     &         area(i1d)*(av1d(i1d)*flux(i1d) - av1d(i1d-1)*flux(i1d-1))
            enddo
c
c  --- put new concentrations back into 3-D array ---
c
            i1d = 1
            do icl=iclbeg,iclend
              i1d = i1d + 1 
              saconc(icl,jcell,kcell,ispc) = AMAX1(cnctmp(i1d),BNDLPT)
            enddo
          enddo
c
c   --- next tracer class
c
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end
