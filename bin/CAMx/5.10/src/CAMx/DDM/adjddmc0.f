c*** ADJDDMC0
c
      subroutine adjddmc0(ispc,ixcl,jycl,kzcl,fc2r,fr2c,c0trac,
     &                       tmtrac,cellvol,rainvol,lzerc,levap,
     &                               ncolx,nrowy,nlayz,nsens,snsconc)
      use grid
      use tracer
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c   Description:
c     This routine makes Wet Dep adjustments to the DMM sensitivities.
c     The adjustments are based on the relative fluxes between cell
c     and rain for the affected species.  If the rain evaporates
c     before reaching the ground, all sensitivity must be returned
c     to the cell.
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       ispc    I  index into regular model species order
c       ixcl     I  the X grid location of current cell
c       jycl     I  the Y grid location of current cell
c       kzcl     I  the vertical grid location of current layer
c       fc2r    R  relative flux from cell to rain
c       fr2c    R  relative flux from rain to cell
c       cellvol R  cell volume (m3)
c       rainvol R  volume of rain water (m3)
c       lzerc   L  true if cell concentrations are at lower bound
c       levap   L  true if the rain totally evaporates in this cell
c       ncolx   I  number of columns in this grid
c       nrowy   I  number of rows in this grid
c       nlayz   I  number of layers in this grid
c       nsens   I  number of DDM sensitivies
c       snsconc R  gridded array of sensitivities
c     Output: 
c       tmtrac  R  total rain mass for sensitivities (umol units)
c       c0trac  R  rain concentration for sensitivities (umol/m3 rain)
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c       04/08/03  --gwilson--   Original development
c       08/15/03  --gyarwood--  Revised adjustments
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      implicit none
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer   ispc
      integer   ixcl
      integer   jycl
      integer   kzcl
      real      fc2r
      real      fr2c
      real      c0trac(*)
      real      tmtrac(*)
      real      cellvol
      real      rainvol
      logical   lzerc
      logical   levap
      integer   ncolx
      integer   nrowy
      integer   nlayz
      integer   nsens
      real      snsconc(ncolx,nrowy,nlayz,nsens)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer idxspec, i
      real    sum
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- adjust all the sensitivities associated with this species 
c       if levap is true, all the sensitivity is returned to the cell
c       if lzerc is true, all the sensitivity is scavenged by rain ---
c
      if (levap) then
        do i=1,nddmsp
          idxspec = iptddm(ispc)+i-1
          snsconc(ixcl,jycl,kzcl,idxspec) = 
     &      snsconc(ixcl,jycl,kzcl,idxspec) + tmtrac(idxspec) / cellvol
          tmtrac(idxspec) = 0.
          c0trac(idxspec) = 0.
        enddo
      elseif (lzerc) then
        do i=1,nddmsp
          idxspec = iptddm(ispc)+i-1
          sum = snsconc(ixcl,jycl,kzcl,idxspec) - BNDLPT
          snsconc(ixcl,jycl,kzcl,idxspec) = BNDLPT
          tmtrac(idxspec) = tmtrac(idxspec) + sum * cellvol
          c0trac(idxspec) = tmtrac(idxspec) / rainvol
        enddo
      else
        do i=1,nddmsp
          idxspec = iptddm(ispc)+i-1
          sum = (fc2r * snsconc(ixcl,jycl,kzcl,idxspec)) - 
     &                                       (fr2c * c0trac(idxspec))
          snsconc(ixcl,jycl,kzcl,idxspec) = 
     &                           snsconc(ixcl,jycl,kzcl,idxspec) - sum
          tmtrac(idxspec) = tmtrac(idxspec) + sum * cellvol
          c0trac(idxspec) = tmtrac(idxspec) / rainvol
        enddo
      endif
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end
