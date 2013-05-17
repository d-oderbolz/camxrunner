
      subroutine nodes_send_pig_sum(iarray,nbytes,mpi_type,thistag,
     &                               numprocs,iproc_id               )
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use grid
      use chmstry
      use filunit
      use o3colmap
      use bndary
      use grid
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
c----CAMx v6.00 130506
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
      include 'flags.inc'
      include 'camx_aero.inc'
      include 'soap.inc'
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: iarray(nbytes)
c
      integer :: nbytes
      integer :: mpi_type
      integer :: thistag
      integer :: numprocs
      integer :: iproc_id
c     
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: ierr, iproc, i
      integer :: status(MPI_STATUS_SIZE)
      integer :: itemp_var(nbytes)
c     
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (.not. LMPI ) return
c
c  --- master receives data from nodes ---
c
      if (iproc_id .eq. 0) then
         do i=1,nbytes
           iarray(i) = 0.
         enddo
         do iproc=1, numprocs
           call MPI_RECV(itemp_var,nbytes,mpi_type,iproc,thistag,
     &                 MPI_COMM_WORLD,status,ierr             )
           do i=1,nbytes
              iarray(i) = iarray(i) + itemp_var(i)
            enddo
         enddo
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
      subroutine nodes_send_pig_sum_real(rarray,nbytes,mpi_type,thistag,
     &                               numprocs,iproc_id               )
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use grid
      use chmstry
      use filunit
      use o3colmap
      use bndary
      use grid
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
c----CAMx v6.00 130506
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
      include 'flags.inc'
      include 'camx_aero.inc'
      include 'soap.inc'
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real    :: rarray(nbytes)
c
      integer :: nbytes
      integer :: mpi_type
      integer :: thistag
      integer :: numprocs
      integer :: iproc_id
c     
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: ierr, iproc, i
      integer :: status(MPI_STATUS_SIZE)
      real    :: rtemp_var(nbytes)
c     
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (.not. LMPI ) return
c
c  --- master receives data from nodes ---
c
      if (iproc_id .eq. 0) then
         do i=1,nbytes
           rarray(i) = 0.
         enddo
         do iproc=1, numprocs
           call MPI_RECV(rtemp_var,nbytes,mpi_type,iproc,thistag,
     &                 MPI_COMM_WORLD,status,ierr             )
           do i=1,nbytes
              rarray(i) = rarray(i) + rtemp_var(i)
            enddo
         enddo
      else
c
c  --- node sends data back to master ---
c
         call MPI_SEND(rarray,nbytes,mpi_type,0,thistag,MPI_COMM_WORLD,ierr)
      endif
      thistag = thistag+1
c 
      return
      end
