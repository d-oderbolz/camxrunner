      subroutine node_recv_1species_data(concs,ng,my_nlay,this_layer, 
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
c       BNDRY_UPDT
c       TSTEP_INIT
c       NODES_EMISS
c       NODES_INIT
c       NODES_MET
c       NODES_TSTEP
c    Subroutines called:
c       MPI_RECV
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
      integer :: this_layer
      integer :: my_nspc
      integer :: this_species
      integer :: this_tag
c
      real    :: concs(mmxp(ng), mmyp(ng), my_nlay, my_nspc)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c  --- data received buffer ---
c
      real    :: datrecv(mmxp(ng), mmyp(ng))
c
      integer :: ierr
      integer :: npts
      integer :: c
      integer :: r
      integer :: status(MPI_STATUS_SIZE)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      npts = mmxp(ng)*mmyp(ng)
      call MPI_RECV(datrecv,npts,MPI_REAL,0,this_tag,
     &              MPI_COMM_WORLD,status,ierr       )
      do c=1, mmxp(ng)
         do r=1, mmyp(ng)
            concs(c,r,this_layer,this_species) = datrecv(c,r)
         enddo
      enddo
c
      end
