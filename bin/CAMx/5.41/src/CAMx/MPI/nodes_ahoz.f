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
      use procan
      use rtracchm
      use tracer
c
      implicit none
c
c----CAMx v5.41 121109
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
      integer :: numprocs
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i
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
