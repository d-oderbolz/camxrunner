      subroutine nodes_send_puffmass_back(iarray,nspcs,nrctr,numpigs,
     &                                    mpi_type,thistag,numprocs,iproc_id,
     &                                    inslice,numpig,numgrid,igrd        )
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
c----CAMx v5.30 101223
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
c     Copyright 1996 - 2010
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
      real    :: iarray(nspcs,nrctr,MXPIG)
c
      integer :: nspcs
      integer :: nrctr
      integer :: numpigs
      integer :: mpi_type
      integer :: thistag
      integer :: numprocs
      integer :: iproc_id
      integer :: inslice(numgrid,numprocs,MXPIG)
      integer :: numpig
      integer :: numgrid
      integer :: igrd
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      real    :: marray(nrctr,MXPIG)
c
      integer :: nbytes
      integer :: i
      integer :: ispc
      integer :: n
      integer :: k
      integer :: ierr
      integer :: status(MPI_STATUS_SIZE)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (.NOT. lmpi) return
c
      nbytes = nrctr * numpigs
c
c  --- master receives data from nodes ---
c
      do ispc=1,nspcs
         if (iproc_id .eq. 0) then
            do i=1, numprocs
               call MPI_RECV(marray,nbytes,mpi_type,i,thistag,
     &                          MPI_COMM_WORLD,status,ierr       )
               do n=1,numpig
                  if (inslice(igrd,i,n) .eq. 1) then
                     do k=1,nrctr
                        iarray(ispc,k,n) = marray(k,n)
                     enddo
                  endif
               enddo
            enddo
         else
c
c  --- node sends data back to master ---
c
            do n=1,numpig
               if (inslice(igrd,iproc_id,n) .eq. 1) then
                  do k=1,nrctr
                     marray(k,n) = iarray(ispc,k,n)
                  enddo
               endif
            enddo
            call MPI_SEND(marray,nbytes,mpi_type,0,thistag,
     &                                          MPI_COMM_WORLD,ierr)
         endif
         thistag = thistag+1
      enddo
c
      return
      end
