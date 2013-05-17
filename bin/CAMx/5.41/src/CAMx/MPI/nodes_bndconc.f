      subroutine nodes_bndconc(ioff,joff,m1,m2,ncol,nrow,nlay,
     &                                       nspec,nedge,conc,bndconc)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
c
      implicit none
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    This routine will load the updated edge concentrations (boundary
c    conditions) passed from the master in the bndconc array and put 
c    it into the gridded conc array of the slices.
c
c    Argument descriptions:
c     Input:  
c       ioff    -- offset of origin for this slice in X-direction
c       joff    -- offset of origin for this slice in Y-direction
c       m1      -- number of columns in this slice
c       m2      -- number of rows in this slice
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
c     Copyright 1996 - 2012
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
      integer ioff
      integer joff
      integer m1
      integer m2
      integer ncol
      integer nrow
      integer nlay
      integer nspec
      integer nedge
      real conc(m1,m2,nlay,nspec)
      real bndconc(4,nedge,nlay,nspec)
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      logical, external :: isbound
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
           if( isbound( mibcon(1),mmxp(1),mmyp(1),0,mmyp(1) ) ) then
              do icl=1,mmxp(1)
                conc(icl,mmyp(1),ilay,ispc) =  
     &                            bndconc(IDNORTH,icl+mi0(1),ilay,ispc)
              enddo
           endif
c
c  --- South edge ---
c
           if( isbound( mibcon(1),mmxp(1),mmyp(1),0,1 ) ) then
              do icl=1,mmxp(1)
                conc(icl,1,ilay,ispc) =  
     &                             bndconc(IDSOUTH,icl+mi0(1),ilay,ispc)
              enddo
           endif
c
c  --- West edge ---
c
           if( isbound( mibcon(1),mmxp(1),mmyp(1),1,0 ) ) then
              do jcl=1,mmyp(1)
                conc(1,jcl,ilay,ispc) =  
     &                               bndconc(IDWEST,jcl+mj0(1),ilay,ispc)
              enddo
           endif
c
c  --- East edge ---
c
           if( isbound( mibcon(1),mmxp(1),mmyp(1),mmxp(1),0 ) ) then
              do jcl=1,mmyp(1)
                conc(mmxp(1),jcl,ilay,ispc) =  
     &                              bndconc(IDEAST,jcl+mj0(1),ilay,ispc)
              enddo
           endif
c
         enddo
c
      enddo
c
      end
