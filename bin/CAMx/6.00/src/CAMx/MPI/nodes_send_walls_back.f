      subroutine nodes_send_walls_back(numprocs,iproc_id,ircp,thistag)

c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use tracer
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
c        ircp      -- index into arrays for this receptor
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
      integer :: ircp
      integer :: thistag
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      real    :: tmprcp(MXTRSP)
      real    :: tmpvol
      integer :: i, n
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
            call MPI_RECV(tmpvol,1,MPI_REAL,i,thistag,
     &                    MPI_COMM_WORLD,status,ierr    )
            volrcp(ircp) = volrcp(ircp) + tmpvol
         enddo
      else
c
c  --- node sends data back to master ---
c
         tmpvol = volrcp(ircp)
         call MPI_SEND(tmpvol,1,MPI_REAL,0,thistag,MPI_COMM_WORLD,ierr)
      endif
      thistag = thistag+1
c
      if( iproc_id .EQ. 0 ) then
         do i=1, numprocs
            call MPI_RECV(tmprcp,ntotsp,MPI_REAL,i,thistag,
     &                    MPI_COMM_WORLD,status,ierr    )
            do n=1,ntotsp
               conrcp(n,ircp) = conrcp(n,ircp) + tmprcp(n)
            enddo
         enddo
      else
c
c  --- node sends data back to master ---
c
         do n=1,ntotsp
            tmprcp(n) = conrcp(n,ircp)
         enddo
         call MPI_SEND(tmprcp,ntotsp,MPI_REAL,0,thistag,MPI_COMM_WORLD,ierr)
      endif
      thistag = thistag+1
c
c  --- everything has been accumulated, now calculate ratio ---
c
      if( iproc_id .EQ. 0 ) then
         if( volrcp(ircp) .NE. 0. ) then
            do n=1,ntotsp
               conrcp(n,ircp) = conrcp(n,ircp) / volrcp(ircp)
            enddo
            volrcp(ircp) = 0.
         endif
c
c  --- re-initialize slice data to zero ---
c
      else
         volrcp(ircp) = 0.
         do n=1,ntotsp
            conrcp(n,ircp) = 0.
         enddo
      endif
c
      return
      end
