      subroutine nodes_pass_sapnts(numprocs,iproc_id)
      use tracer
      use ptemiss
      implicit none
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c        This routine passes the point source array to the nodes.
c        A special routine is needed for this because the size of
c        array can be too large for one pass. Instead a pass for 
c        each species is needed.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c       numprocs       the number of processes
c       iproc_id       process ID
c     Output:  
c
c    Called by:
c    Subroutines called:
c       MPI_SEND
c       MPI_RECV
c
c     Copyright 1996 - 2009
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
      include 'flags.com'
      include 'filunit.com'
      include 'tracerpts.com'
      include 'mpif.h'
c     
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer numprocs
      integer iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      real    :: array(MXPTSRC)
      integer :: i, ispc, ipts, ierr
      integer :: status(MPI_STATUS_SIZE)
c     
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (.NOT. lmpi .AND. iproc_id .GT. 0) return
c
c   --- loop over tracer species ---
c
      do ispc=1,ntotsp
c
c  --- process ID zero sends all of the messages,
c      to all of the other processes ---
c
         if (iproc_id .EQ. 0) then
c
c   --- load all points into local array ----
c
            do ipts=1,nptsrc
               array(ipts) = sapnts(ipts,ispc)
            enddo
c
c   --- send local array to the compute nodes ---
c
            do i=1,numprocs 
               call MPI_SEND(array,nptsrc,MPI_REAL,i,itag,MPI_COMM_WORLD,ierr)
            enddo
c
c  --- if this is a compute process, allocate the data structures
c      and wait for the data to arrive ---
c
         else
            call MPI_RECV(array,nptsrc,MPI_REAL,0,itag,MPI_COMM_WORLD,status,ierr)
c
c  --- load data back into global array ---
c
            do ipts=1,nptsrc
               sapnts(ipts,ispc) = array(ipts)
            enddo
         endif
         itag = itag + 1
      enddo
c
      end
