      subroutine nodes_pass1(iarray,nbytes,mpi_type,itag,
     &                       iproc_id,slave_id           )
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c        This routine will do the actual passing of data from node zero
c        to one of the process node.  The array in the argument list
c        will be passed, using the number of bytes and the type specified.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        iarray              array containing the data - scalar 
c                            variables are of length 1
c        nbytes              the number of bytes to pass
c        mpi_type            the MPI code for the variable type
c        itag                the tag ID for this transfer
c        iproc_id            the process ID for this processor
c     Output:  
c
c    Called by:
c       BROADCAST_GRID_DIMENS
c    Subroutines called:
c       MPI_SEND
c       MPI_RECV
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
      include 'mpif.h'
c     
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: nbytes
c
      real    :: iarray(nbytes)
c
      integer :: mpi_type
      integer :: itag
      integer :: iproc_id
      integer :: slave_id
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
c  --- process ID zero sends all of the messages,
c      to all of the other processes ---
c
      if (iproc_id .EQ. 0) then
         call MPI_SEND(iarray,nbytes,mpi_type,slave_id,itag,
     &                 MPI_COMM_WORLD,ierr                  )
c
c  --- if this is a compute process, allocate the data
c      structures and wait for the data to arrive ---
c
      elseif (slave_id .eq. iproc_id) then
         call MPI_RECV(iarray,nbytes,mpi_type,0,itag,
     &                 MPI_COMM_WORLD,status,ierr    )
      endif
      itag = itag+1
c
      end
