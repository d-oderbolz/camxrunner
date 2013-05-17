      subroutine nodes_pass(iarray,nbytes,mpi_type,itag,numprocs,iproc_id)
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        This routine will do the actual passing of data from node zero
c        to the process nodes.  The array in the argument list will be
c        passed, using the number of bytes and the type specified.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        iarray              array containing the data - scalar 
c                            variables are of length 1
c        nbytes              the number of bytes to pass
c        mpi_type            the MPI code for the variable type
c        itag                the tag ID for this transfer
c        numprocs            the number of processes
c        iproc_id            the process ID for this processor
c     Output:  
c
c    Called by:
c       CAMX
c       SIM_INIT
c       BROADCAST_GRID_DIMENS
c       BROADCAST_PROC_ID
c       NODES_ALLOC
c       NODES_EMISS
c       NODES_MET
c       NODES_MET_PIG
c       NODES_PIG_PASS
c    Subroutines called:
c       MPI_SEND
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
      include 'camx.prm'
      include 'flags.inc'
      include 'mpif.h'
c     
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real    :: iarray(nbytes)
c
      integer :: nbytes
      integer :: mpi_type
      integer :: itag
      integer :: numprocs
      integer :: iproc_id
c     
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: ierr
      integer :: i
      integer :: status(MPI_STATUS_SIZE)
c     
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (.NOT. lmpi .AND. iproc_id .GT. 0) return
c
c  --- process ID zero sends all of the messages,
c      to all of the other processes ---
c
      if (iproc_id .EQ. 0) then
         do i=1,numprocs 
            call MPI_SEND(iarray,nbytes,mpi_type,i,itag,MPI_COMM_WORLD,ierr)
         enddo
c
c  --- if this is a compute process, allocate the data structures
c      and wait for the data to arrive ---
c
      else
         call MPI_RECV(iarray,nbytes,mpi_type,0,itag,MPI_COMM_WORLD,status,ierr)
      endif
      itag = itag + 1
c
      end
