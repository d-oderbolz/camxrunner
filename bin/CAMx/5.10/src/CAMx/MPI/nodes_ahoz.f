      subroutine nodes_ahoz(numprocs,iproc_id)
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
      use lsbox
      use procan
      use rtracchm
      use tracer
c
      implicit none
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c        This routine passes all of the data that is time-step
c        dependent to the compute nodes when in MPI mode.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        numprocs            the number of processes
c     Output:  
c
c    Called by:
c       AHOZ_UPD
c    Subroutines called:
c       MASTER_SEND_GRIDDED_DATA
c       NODE_RECV_GRIDDED_DATA
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
      include 'camx_aero.com'
      include 'soap.com'
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: numprocs
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: nrowa
      integer :: nlaya
      integer :: mvec2d
      integer :: mvec3d
      integer :: i
      integer :: mvec4d
      integer :: mvecem
      integer :: mvecrd
      integer :: mvec3a
      integer :: mveclu
      integer :: mvecdp
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (.NOT. lmpi) return
c
c  --- send the other variables in the ahomap include file ----
c
      do i=1,ngrid
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(icdhaz(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data(icdhaz(iptr2d(i)),i,1,1,itag)          
         endif
         itag = itag+1 
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(icdozn(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data(icdozn(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
         if (iproc_id .eq. 0) then
            call master_send_gridded_data(icdsno(iptr2d(i)),i,1,1,itag)
         else
            call node_recv_gridded_data(icdsno(iptr2d(i)),i,1,1,itag)
         endif
         itag = itag+1
      enddo
c
      end
