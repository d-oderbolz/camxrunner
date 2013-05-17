      subroutine node_send_1species_data(concs,ng,my_nlay,this_layer,
     &                                   my_nspc,this_species,this_tag)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use camx_includes
      use node_mod
      use camxfld
      use filunit
c
      implicit none
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        This routine should only be used by slave node
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       MASTER_UPDATE
c    Subroutines called:
c       MPI_SEND
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
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: ng
      integer :: my_nlay
      integer :: my_nspc
      integer :: this_tag
      integer :: this_layer
      integer :: this_species
c
      real    :: concs(mmxp(ng),mmyp(ng),my_nlay,my_nspc) 
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c  --- data send buffer ---
c
      real    :: datsend(mmxp(ng),mmyp(ng))
c
      integer :: ierr
      integer :: npts
      integer :: c
      integer :: r
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      npts = mmxp(ng)*mmyp(ng)
      do c=1, mmxp(ng)
         do r=1, mmyp(ng)
            datsend(c,r) = concs(c,r,this_layer,this_species)
         enddo
      enddo
      call MPI_SEND(datsend,npts,MPI_REAL,0,this_tag,MPI_COMM_WORLD,ierr)
c
      end
