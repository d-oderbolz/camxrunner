      subroutine master_bndconc(ncol,nrow,nlay,nspec,nedge,conc,bndconc)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      implicit none
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    This routine will load the updated edge concentrations (boundary
c    conditions) from the conc array and put it into the bndconc array
c    to be passed to the computational slices.
c
c    Argument descriptions:
c     Input:  
c       ncol    -- number of columns in master grid
c       nrow    -- number of rows in master grid
c       nlay    -- number of layers in master grid
c       nspec   -- number of species
c       nedge   -- maximum number of cells on an edge
c       conc    -- gridded conentrations
c     Output:  
c       bndconc -- concentrations on edge cells
c
c    Called by:
c       BNDRY_UPDT
c    Subroutines called:
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer ncol
      integer nrow
      integer nlay
      integer nspec
      integer nedge
      real conc(ncol,nrow,nlay,nspec)
      real bndconc(4,nedge,nlay,nspec)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer ilay, icl, jcl, ispc
c     
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- Loop over species ---
c
      do ispc=1,nspec
c
c  --- Loop over layers ---
c
         do ilay=1,nlay
c
c  --- North edge ---
c
           do icl=1,ncol
             bndconc(IDNORTH,icl,ilay,ispc) = conc(icl,nrow,ilay,ispc)
           enddo
c
c  --- South edge ---
c
           do icl=1,ncol
             bndconc(IDSOUTH,icl,ilay,ispc) = conc(icl,1,ilay,ispc)
           enddo
c
c  --- West edge ---
c
           do jcl=1,nrow
             bndconc(IDWEST,jcl,ilay,ispc) = conc(1,jcl,ilay,ispc)
           enddo
c
c  --- East edge ---
c
           do jcl=1,nrow
             bndconc(IDEAST,jcl,ilay,ispc) = conc(ncol,jcl,ilay,ispc)
           enddo
c
         enddo
c
      enddo
c
      end
