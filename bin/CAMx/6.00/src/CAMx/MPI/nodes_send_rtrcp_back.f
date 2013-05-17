      subroutine nodes_send_rtrcp_back(numprocs,iproc_id,thistag)

c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use rtracchm
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
c        numprocs  -- number of MPI slices
c        iproc_id  -- process ID for this slice
c        thistag   -- current msgtag ID
c     Output:  
c
c    Called by:
c       MASTER_UPDATE
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
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: numprocs
      integer :: iproc_id
      integer :: thistag
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      real    :: tmprcp(MXRTCEL,MXSPEC)
      integer :: i, n, ircp
      integer :: ierr
      integer :: status(MPI_STATUS_SIZE)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if( .NOT. lmpi ) return
c
      if( iproc_id .EQ. 0 ) then
         do i=1, numprocs
            call MPI_RECV(tmprcp,nrtgas*MXRTCEL,MPI_REAL,i,thistag,
     &                    MPI_COMM_WORLD,status,ierr    )
            do n=1,nrtgas
              do ircp = 1,MXRTCEL
                 rcpdcy(ircp,n) = rcpdcy(ircp,n) + tmprcp(ircp,n)
               enddo
            enddo
         enddo
      else
c
c  --- node sends data back to master ---
c
         call MPI_SEND(rcpdcy,nrtgas*MXRTCEL,MPI_REAL,0,thistag,MPI_COMM_WORLD,ierr)
         rcpdcy = 0
      endif
      thistag = thistag+1
c
      return
      end
