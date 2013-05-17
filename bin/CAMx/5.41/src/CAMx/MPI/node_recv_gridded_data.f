      subroutine node_recv_gridded_data(concs,ng,my_nlay,nspc,this_tag)
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
c       SIM_INIT
c       NODES_AHOZ
c       NODES_INIT
c       NODES_MET
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
      integer :: nspc
      integer :: this_tag
c
      real    :: concs(mmxp(ng), mmyp(ng), my_nlay, nspc)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c  --- data received buffer ---
c
      real    :: datrecv(mmxp(ng), mmyp(ng), my_nlay, nspc)
c
      integer :: ierr
      integer :: npts
      integer :: c
      integer :: r
      integer :: L
      integer :: n
      integer :: status(MPI_STATUS_SIZE)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      npts = mmxp(ng)*mmyp(ng)*my_nlay*nspc
      call MPI_RECV(datrecv,npts,MPI_REAL,0,this_tag,
     &              MPI_COMM_WORLD,status,ierr       )
      do c=1, mmxp(ng)
         do r=1, mmyp(ng)
            do L=1, my_nlay
               do n=1, nspc
                  concs(c,r,L,n) = datrecv(c,r,L,n)
               enddo
            enddo
         enddo
      enddo
c
      end
