      subroutine broadcast_procid(iproc_id,nproc)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use master_mod
      use node_mod
c
c  --- for itag only ---
c
      use filunit
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
c       NODES_ALLOC
c    Subroutines called:
c       NODES_PASS
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     01/15/08   --gwilson--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: iproc_id
      integer :: nproc
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: nm
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (iproc_id .eq. 0) then
c
c  --- var used by slaves ---
c
         master_num = 0
         nmachines=nproc
         do nm=1,nmachines
c
c  --- var used by master ---
c
            machnum(nm)=nm
            machs  (nm)=nm
         enddo
c
c  --- nmachines is used by master, nmachs is used by slaves ---
c
         nmachs = nmachines
      endif
      call nodes_pass(master_num,1,MPI_INTEGER,itag,nproc,iproc_id)
      call nodes_pass(nmachs,    1,MPI_INTEGER,itag,nproc,iproc_id)
      call nodes_pass(machs,nmachs,MPI_INTEGER,itag,nproc,iproc_id)
      if (iproc_id .ne. 0) then
         mynum = iproc_id
      endif
c
      return
      end
