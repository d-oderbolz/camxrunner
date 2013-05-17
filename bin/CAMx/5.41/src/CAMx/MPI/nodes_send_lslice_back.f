      subroutine nodes_send_lslice_back(iarray,numgrid,numpig,mpi_type,
     &                                  thistag,igrd,numprocs,iproc_id )
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
      integer :: iarray(numgrid,numprocs,MXPIG)
      integer :: numgrid
      integer :: numpig
      integer :: mpi_type
      integer :: thistag
      integer :: igrd
      integer :: numprocs
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c  --- master node's buffer array ---
c
      integer :: marray(numgrid,numprocs,MXPIG)
c
      integer :: i
      integer :: n
      integer :: nbytes
      integer :: ierr
      integer :: status(MPI_STATUS_SIZE)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (.NOT. lmpi) return
c
c  --- Re-initialize the in-slice flag ---
c
      if (iproc_id .EQ. 0) then
         do i=1, numprocs
            do n=1,numpig
               iarray(igrd,i,n) = 0
            enddo
         enddo
      endif
      nbytes=numgrid*numprocs*numpig     
c
c  --- master receives data from nodes ---
c
      if (iproc_id .eq. 0) then
         do i=1, numprocs
            call MPI_RECV(marray,nbytes,mpi_type,i,thistag,
     &                    MPI_COMM_WORLD,status,ierr       )
            do n=1,numpig
               iarray(igrd,i,n) = marray(igrd,i,n)
            enddo
         enddo
      else
c
c  --- node sends data back to master ---
c
         call MPI_SEND(iarray,nbytes,mpi_type,0,thistag,MPI_COMM_WORLD,ierr)
      endif
      thistag = thistag + 1
c
      return
      end
