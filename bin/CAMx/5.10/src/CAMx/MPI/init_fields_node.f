      subroutine init_fields_node(init)                  
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use camx_includes
      use grid  
      use tracer
      use procan
c
      implicit none
c
c----CAMx v5.10 090918
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
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications:
c       03/15/09     Added code for deposition output for tracers
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: init
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: ng
      integer :: nm
      integer :: itype
      integer :: ii1
      integer :: jj1
      integer :: ii2
      integer :: jj2
      integer :: memf
      integer :: npvar
      integer :: nv
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- Get all necessary fields from master. ---
c
c  --- bwang: this call has been moved to somewhere else. ---
c  --- call node_getinit() ---
c
c  --- Can we use existing memory for the nesting communication buffers?
c      If not, allocate new buffers or compute buffer sizes. ---
c
c  --- Check feedback buffer. ---
c
      itype=6
      nbuff_feed=0
      do ng=1,ngrid
         do nm=1,nmachs
            ii1=ipaths(1,itype,ng,nm)
            ii2=ipaths(2,itype,ng,nm)
            jj1=ipaths(3,itype,ng,nm)
            jj2=ipaths(4,itype,ng,nm)
            memf=(ii2-ii1+1)*(jj2-jj1+1)*(nlay(ng))*(4+nspec)
            nbuff_feed=max(nbuff_feed,memf)
         enddo
      enddo
c
c  --- Allocate long time step send and receive buffers ---
c
      if (init .NE. 1) then
         do nm=1,nmachs
            if( associated(node_buffs(nm)%lbc_send_buff))
     &                                 deallocate(node_buffs(nm)%lbc_send_buff)
            if( associated(node_buffs(nm)%lbc_recv_buff))
     &                                 deallocate(node_buffs(nm)%lbc_recv_buff)
         enddo
      endif
c
      do nm=1,nmachs
         if( node_buffs(nm)%nsend > 0)
     &             allocate(node_buffs(nm)%lbc_send_buff(node_buffs(nm)%nsend))
         if( node_buffs(nm)%nrecv > 0)
     &             allocate(node_buffs(nm)%lbc_recv_buff(node_buffs(nm)%nrecv))
      enddo
c
      if (init .NE. 1) then
         do nm=1,nmachs
           if( associated(node_buffs(nm)%lbc_dp_send_buff))
     &                              deallocate(node_buffs(nm)%lbc_dp_send_buff)
           if( associated(node_buffs(nm)%lbc_dp_recv_buff))
     &                              deallocate(node_buffs(nm)%lbc_dp_recv_buff)
         enddo
      endif
c
      do nm=1,nmachs
         if( node_buffs(nm)%nsend > 0)
     &          allocate(node_buffs(nm)%lbc_dp_send_buff(node_buffs(nm)%nsend))
         if( node_buffs(nm)%nrecv > 0)
     &          allocate(node_buffs(nm)%lbc_dp_recv_buff(node_buffs(nm)%nrecv))
      enddo
c
c  --- allocate lbc_pt_send/recv_buff ---
c
      if( ltrace .OR. lddm .OR. lhddm .OR. lirr ) then 
         if (init .NE. 1) then
            do nm=1,nmachs
               if( associated(node_buffs(nm)%lbc_pt_send_buff))
     &                              deallocate(node_buffs(nm)%lbc_pt_send_buff)
               if( associated(node_buffs(nm)%lbc_pt_recv_buff))
     &                              deallocate(node_buffs(nm)%lbc_pt_recv_buff)
            enddo
         endif
c
         do nm=1,nmachs
            if( node_buffs(nm)%nsend > 0)
     &          allocate(node_buffs(nm)%lbc_pt_send_buff(node_buffs(nm)%nsend))
            if( node_buffs(nm)%nrecv > 0)
     &          allocate(node_buffs(nm)%lbc_pt_recv_buff(node_buffs(nm)%nrecv))
         enddo
      endif
c
c  --- allocate lbc_rad_send/recv_buff ---
c
      if (init .NE. 1) then
         do nm=1,nmachs
            if( associated(node_buffs(nm)%lbc_rad_send_buff))
     &                              deallocate(node_buffs(nm)%lbc_rad_send_buff)
            if( associated(node_buffs(nm)%lbc_rad_recv_buff))
     &                              deallocate(node_buffs(nm)%lbc_rad_recv_buff)
         enddo
      endif
c
      do nm=1,nmachs
         if( node_buffs(nm)%nsend > 0)
     &          allocate(node_buffs(nm)%lbc_rad_send_buff(node_buffs(nm)%nsend))
         if( node_buffs(nm)%nrecv > 0)
     &          allocate(node_buffs(nm)%lbc_rad_recv_buff(node_buffs(nm)%nrecv))
      enddo
c
c  --- allocate lbc_dry_send/recv_buff ---
c
      if( lptdepout ) then
         if (init .NE. 1) then
            do nm=1,nmachs
               if( associated(node_buffs(nm)%lbc_dry_send_buff))
     &                              deallocate(node_buffs(nm)%lbc_dry_send_buff)
               if( associated(node_buffs(nm)%lbc_dry_recv_buff))
     &                              deallocate(node_buffs(nm)%lbc_dry_recv_buff)
            enddo
         endif
c
         do nm=1,nmachs
            if( node_buffs(nm)%nsend > 0)
     &          allocate(node_buffs(nm)%lbc_dry_send_buff(node_buffs(nm)%nsend))
            if( node_buffs(nm)%nrecv > 0)
     &          allocate(node_buffs(nm)%lbc_dry_recv_buff(node_buffs(nm)%nrecv))
         enddo
      endif
c
c  --- allocate lbc_wet_send/recv_buff ---
c
      if( lptdepout ) then
         if (init .NE. 1) then
            do nm=1,nmachs
               if( associated(node_buffs(nm)%lbc_wet_send_buff))
     &                              deallocate(node_buffs(nm)%lbc_wet_send_buff)
               if( associated(node_buffs(nm)%lbc_wet_recv_buff))
     &                              deallocate(node_buffs(nm)%lbc_wet_recv_buff)
            enddo
         endif
c
         do nm=1,nmachs
            if( node_buffs(nm)%nsend > 0)
     &          allocate(node_buffs(nm)%lbc_wet_send_buff(node_buffs(nm)%nsend))
            if( node_buffs(nm)%nrecv > 0)
     &          allocate(node_buffs(nm)%lbc_wet_recv_buff(node_buffs(nm)%nrecv))
         enddo
      endif
c
      return
      end
