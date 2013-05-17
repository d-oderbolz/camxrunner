      subroutine nodes_send_pig_misc_real8(iarray,nbytes,mpi_type,
     &                                     thistag,nodeIdx,iproc_id)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use grid
      use chmstry
      use filunit
      use ahomap
      use bndary
      use camxfld
      use camxcom
      use pigsty
      use ptemiss
      use procan
      use rtracchm
      use tracer
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
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NODES_PIG_PASS
c    Subroutines called:
c       MPI_RECV
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
      include 'camx.prm'
      include 'flags.inc'
      include 'camx_aero.inc'
      include 'soap.inc'
      include 'mpif.h'
c     
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real*8  :: iarray(nbytes)
c
      integer :: nbytes
      integer :: mpi_type
      integer :: thistag
      integer :: nodeIdx
      integer :: iproc_id
c     
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: ierr
      integer :: status(MPI_STATUS_SIZE)
c     
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (.not. LMPI) return
c
c  --- master receives data from nodes ---
c
      if (iproc_id .eq. 0) then
         call MPI_RECV(iarray,nbytes,mpi_type,nodeIdx,thistag,
     &                 MPI_COMM_WORLD,status,ierr             )
      else
c
c  --- node sends data back to master ---
c
         call MPI_SEND(iarray,nbytes,mpi_type,0,thistag,MPI_COMM_WORLD,ierr)
      endif
      thistag = thistag+1
c
      return
      end
