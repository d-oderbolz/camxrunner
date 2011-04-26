      subroutine node_get_feed(ifm,icm,dx,dy,depths)                                 
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use camxfld
      use grid
c
      implicit none
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c    Description:
c        Get the portion of the coarse grid subdomain that will be filled
c        with fine grid info from each node.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_WAIT
c       PAR_ASSOC_BUFF
c       PAR_GET_INT
c       PAR_GET_FLOAT
c       UNFDBACKP
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c       03/15/09     Added code for deposition output for tracers
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'chmdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: icm
      integer :: ifm
      real    :: dx(*)
      real    :: dy
      real    :: depths(ncol(icm),nrow(icm),nlay(icm))
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i1s
      integer :: i2s
      integer :: j1s
      integer :: j2s
      integer :: k1crs
      integer :: k2crs
      integer :: mtp
      integer :: nfx
      integer :: nfy
      integer :: nfz
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: ibytes
      integer :: msgid
      integer :: ihostnum
      integer :: machf
      integer :: nwds
      integer :: n
      integer :: c
      integer :: r 
      integer :: commcl(ncol(icm),nrow(icm))
c
      logical :: firstime(nspec)
c
      real, save, allocatable :: pbuff(:)
      integer, save           :: nbuff_save=0
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      itype=6
c
c  --- make sure all sends are finished and send arrays are de-allocated ---
c
      do nm=1,nmachs
         if (ipaths(1,itype,ifm,nm) .ne. 0) then
            call par_wait(isend_req(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
c  --- wait on receives ---
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_wait(irecv_req(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
      do n=1, nspec
         firstime = .true.
      enddo
      do c=1, ncol(icm)
         do r=1, nrow(icm)
            commcl(c,r) = 0 
         enddo
      enddo
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            
            call par_assoc_buff(node_buffs(nm)%lbc_recv_buff(1),
     &                          node_buffs(nm)%nrecv            )
            call par_get_int(i1s,1)
            call par_get_int(i2s,1)
            call par_get_int(j1s,1)
            call par_get_int(j2s,1)
            call par_get_int(k1crs,1)
            call par_get_int(k2crs,1)
            call par_get_int(machf,1)
            call par_get_int(nvar,1)
            call par_get_int(nwds,1)
            if (nwds > nbuff_save) then
               if (allocated(pbuff)) deallocate (pbuff)
               allocate (pbuff(nwds))
               nbuff_save=nwds
            endif    
            call par_get_float(pbuff,nwds)
            do c=i1s, i2s
               do r=j1s, j2s
                  commcl(c,r) = commcl(c,r) + 1
               enddo
            enddo
c
            nfx=i2s-i1s+1
            nfy=j2s-j1s+1
            nfz=k2crs-k1crs+1
            mtp=nfx*nfy*nfz
            iptr=0
            do nv=1,nspec
               ispbeg=iptr4d(icm)+(nv-1)*mmzp(icm)*mmxp(icm)*mmyp(icm)
               call unfdbackp(conc(ispbeg),pbuff(1+iptr),mtp,
     &                        mmxp(icm),mmyp(icm),mmzp(icm),
     &                        mi0(icm),mj0(icm),i1s,i2s,j1s,j2s,
     &                        k1crs,k2crs,mynum,nspec,nv,firstime,
     &                        commcl,ncol(icm),nrow(icm),dx,dy,depths         )
               iptr=iptr+mtp
            enddo
         endif
      enddo
c
      return
      end
c
c---------------------------------------------------------------------
c    END subroutine node_get_feed:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_get_feed_rad
c-----------------------------------------------------------------------
c
      subroutine node_get_feed_rad(ifm,icm,dx,dy,depths)                                 
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use camxfld
      use grid
c
      implicit none
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c    Description:
c        Get the portion of the coarse grid subdomain that will be filled
c        with fine grid info from each node.
c        This version is for the radical concentrations.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_WAIT
c       PAR_ASSOC_BUFF
c       PAR_GET_INT
c       PAR_GET_FLOAT
c       UNFDBACKP
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
      include 'chmdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: icm
      integer :: ifm
      real    :: dx(*)
      real    :: dy
      real    :: depths(ncol(icm),nrow(icm),nlay(icm))
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i1s
      integer :: i2s
      integer :: j1s
      integer :: j2s
      integer :: k1crs
      integer :: k2crs
      integer :: mtp
      integer :: nfx
      integer :: nfy
      integer :: nfz
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: ibytes
      integer :: msgid
      integer :: ihostnum
      integer :: machf
      integer :: nwds
      integer :: n
      integer :: c
      integer :: r 
      integer :: commcl(ncol(icm),nrow(icm))
c
      logical :: firstime(nspec)
c
      real, save, allocatable :: pbuff_rad(:)
      integer, save           :: nbuff_save=0
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      itype=6
c
c  --- make sure all sends are finished and send arrays are de-allocated ---
c
      do nm=1,nmachs
         if (ipaths(1,itype,ifm,nm) .ne. 0) then
            call par_wait(isend_req_rad(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
c  --- wait on receives ---
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_wait(irecv_req_rad(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
      do n=1, nrad
         firstime = .true.
      enddo
      do c=1, ncol(icm)
         do r=1, nrow(icm)
            commcl(c,r) = 0 
         enddo
      enddo
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            
            call par_assoc_buff(node_buffs(nm)%lbc_rad_recv_buff(1),
     &                          node_buffs(nm)%nrecv            )
            call par_get_int(i1s,1)
            call par_get_int(i2s,1)
            call par_get_int(j1s,1)
            call par_get_int(j2s,1)
            call par_get_int(k1crs,1)
            call par_get_int(k2crs,1)
            call par_get_int(machf,1)
            call par_get_int(nvar,1)
            call par_get_int(nwds,1)
            if (nwds > nbuff_save) then
               if (allocated(pbuff_rad)) deallocate (pbuff_rad)
               allocate (pbuff_rad(nwds))
               nbuff_save=nwds
            endif    
            call par_get_float(pbuff_rad,nwds)
            do c=i1s, i2s
               do r=j1s, j2s
                  commcl(c,r) = commcl(c,r) + 1
               enddo
            enddo
c
            nfx=i2s-i1s+1
            nfy=j2s-j1s+1
            nfz=k2crs-k1crs+1
            mtp=nfx*nfy*nfz
            iptr=0
            do nv=1,nrad
               ispbeg=iptrad(icm)+(nv-1)*mmzp(icm)*mmxp(icm)*mmyp(icm)
               call unfdbackp(cncrad(ispbeg),pbuff_rad(1+iptr),mtp,
     &                        mmxp(icm),mmyp(icm),mmzp(icm),
     &                        mi0(icm),mj0(icm),i1s,i2s,j1s,j2s,
     &                        k1crs,k2crs,mynum,nrad,nv,firstime,
     &                        commcl,ncol(icm),nrow(icm),dx,dy,depths         )
               iptr=iptr+mtp
            enddo
         endif
      enddo
c
      return
      end
c
c---------------------------------------------------------------------
c    END subroutine node_get_feed_rad:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_get_feed_dp
c-----------------------------------------------------------------------
c
      subroutine node_get_feed_dp(ifm,icm,dx,dy)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use camxfld
      use grid
c
      implicit none
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c    Description:
c
c        This version is for the deposition velocities.
c        Get the portion of the coarse grid subdomain that will be filled
c        with fine grid info from each node. 
c
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_WAIT
c       PAR_ASSOC_BUFF
c       PAR_GET_INT
c       PAR_GET_FLOAT
c       UNFDBACK_DP
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
      include 'chmdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: icm
      integer :: ifm
c
      real    :: dx(*)
      real    :: dy
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i1s
      integer :: i2s
      integer :: j1s
      integer :: j2s
      integer :: mtp
      integer :: nfx
      integer :: nfy
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: ibytes
      integer :: msgid
      integer :: ihostnum
      integer :: machf
      integer :: nwds
      integer :: n
      integer :: c
      integer :: r 
      integer :: commcl(ncol(icm),nrow(icm))
c
      logical :: firstime(navspc*3)
c
      real, save, allocatable :: pbuff_dp(:)
      integer, save           :: nbuff_save=0
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      itype=6
c
c  --- make sure all sends are finished and send arrays are de-allocated ---
c
      do nm=1,nmachs
         if (ipaths(1,itype,ifm,nm) .ne. 0) then
            call par_wait(isend_req_dp(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
c  --- wait on receives ---
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_wait(irecv_req_dp(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
      do n=1, navspc*3
         firstime = .true.
      enddo
      do c=1, ncol(icm)
         do r=1, nrow(icm)
            commcl(c,r) = 0 
         enddo
      enddo
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then

            call par_assoc_buff(node_buffs(nm)%lbc_dp_recv_buff(1),
     &                          node_buffs(nm)%nrecv               )
            call par_get_int(i1s,1)
            call par_get_int(i2s,1)
            call par_get_int(j1s,1)
            call par_get_int(j2s,1)
            call par_get_int(machf,1)
            call par_get_int(nvar,1)
            call par_get_int(nwds,1)
            if (nwds > nbuff_save) then
               if (allocated(pbuff_dp)) deallocate (pbuff_dp)
               allocate (pbuff_dp(nwds))
               nbuff_save=nwds
            endif
            call par_get_float(pbuff_dp,nwds)
            do c=i1s, i2s
               do r=j1s, j2s
                  commcl(c,r) = commcl(c,r) + 1
               enddo
            enddo
c
            nfx=i2s-i1s+1
            nfy=j2s-j1s+1
            mtp=nfx*nfy
            iptr=0
            do nv=1,navspc*3
               ispbeg=iptrdp(icm)+(nv-1)*mmyp(icm)*mmxp(icm)
               call unfdback_dp(depfld(ispbeg),pbuff_dp(1+iptr),mtp,
     &                        mmxp(icm),mmyp(icm),mi0(icm),mj0(icm),
     &                        i1s,i2s,j1s,j2s,mynum,navspc*3,nv,
     &                        firstime,commcl,ncol(icm),nrow(icm),dx,dy)
               iptr=iptr+mtp
            enddo
         endif
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_get_feed_dp
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_get_feed_dry
c-----------------------------------------------------------------------
c
      subroutine node_get_feed_dry(ifm,icm,dx,dy)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use camxfld
      use grid
      use tracer
c
      implicit none
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c    Description:
c
c        This version is for the dry deposition for tracers.
c        Get the portion of the coarse grid subdomain that will be filled
c        with fine grid info from each node. 
c
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_WAIT
c       PAR_ASSOC_BUFF
c       PAR_GET_INT
c       PAR_GET_FLOAT
c       UNFDBACK_DP
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
      include 'chmdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: icm
      integer :: ifm
c
      real    :: dx(*)
      real    :: dy
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i1s
      integer :: i2s
      integer :: j1s
      integer :: j2s
      integer :: mtp
      integer :: nfx
      integer :: nfy
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: ibytes
      integer :: msgid
      integer :: ihostnum
      integer :: machf
      integer :: nwds
      integer :: n
      integer :: c
      integer :: r 
      integer :: commcl(ncol(icm),nrow(icm))
c
      logical :: firstime(ntotsp)
c
      real, save, allocatable :: pbuff_dry(:)
      integer, save           :: nbuff_save=0
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      itype=6
c
c  --- make sure all sends are finished and send arrays are de-allocated ---
c
      do nm=1,nmachs
         if (ipaths(1,itype,ifm,nm) .ne. 0) then
            call par_wait(isend_req_dry(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
c  --- wait on receives ---
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_wait(irecv_req_dry(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
      do n=1, ntotsp
         firstime = .true.
      enddo
      do c=1, ncol(icm)
         do r=1, nrow(icm)
            commcl(c,r) = 0 
         enddo
      enddo
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then

            call par_assoc_buff(node_buffs(nm)%lbc_dry_recv_buff(1),
     &                          node_buffs(nm)%nrecv               )
            call par_get_int(i1s,1)
            call par_get_int(i2s,1)
            call par_get_int(j1s,1)
            call par_get_int(j2s,1)
            call par_get_int(machf,1)
            call par_get_int(nvar,1)
            call par_get_int(nwds,1)
            if (nwds > nbuff_save) then
               if (allocated(pbuff_dry)) deallocate (pbuff_dry)
               allocate (pbuff_dry(nwds))
               nbuff_save=nwds
            endif
            call par_get_float(pbuff_dry,nwds)
            do c=i1s, i2s
               do r=j1s, j2s
                  commcl(c,r) = commcl(c,r) + 1
               enddo
            enddo
c
            nfx=i2s-i1s+1
            nfy=j2s-j1s+1
            mtp=nfx*nfy
            iptr=0
            do nv=1,notimespc
               ispbeg=ipsadep(icm)+(nv-1)*mmyp(icm)*mmxp(icm)
               call unfdback_dp(ptdryfld(ispbeg),pbuff_dry(1+iptr),mtp,
     &                        mmxp(icm),mmyp(icm),mi0(icm),mj0(icm),
     &                        i1s,i2s,j1s,j2s,mynum,notimespc,nv,
     &                        firstime,commcl,ncol(icm),nrow(icm),dx,dy)
               iptr=iptr+mtp
            enddo
         endif
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_get_feed_dry
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_get_feed_wet
c-----------------------------------------------------------------------
c
      subroutine node_get_feed_wet(ifm,icm,dx,dy)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use camxfld
      use grid
      use tracer
c
      implicit none
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c    Description:
c
c        This version is for the wet deposition for tracers.
c        Get the portion of the coarse grid subdomain that will be filled
c        with fine grid info from each node. 
c
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_WAIT
c       PAR_ASSOC_BUFF
c       PAR_GET_INT
c       PAR_GET_FLOAT
c       UNFDBACK_DP
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
      include 'chmdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: icm
      integer :: ifm
c
      real    :: dx(*)
      real    :: dy
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i1s
      integer :: i2s
      integer :: j1s
      integer :: j2s
      integer :: mtp
      integer :: nfx
      integer :: nfy
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: ibytes
      integer :: msgid
      integer :: ihostnum
      integer :: machf
      integer :: nwds
      integer :: n
      integer :: c
      integer :: r 
      integer :: commcl(ncol(icm),nrow(icm))
c
      logical :: firstime(ntotsp)
c
      real, save, allocatable :: pbuff_wet(:)
      integer, save           :: nbuff_save=0
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      itype=6
c
c  --- make sure all sends are finished and send arrays are de-allocated ---
c
      do nm=1,nmachs
         if (ipaths(1,itype,ifm,nm) .ne. 0) then
            call par_wait(isend_req_wet(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
c  --- wait on receives ---
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_wait(irecv_req_wet(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
      do n=1, ntotsp
         firstime = .true.
      enddo
      do c=1, ncol(icm)
         do r=1, nrow(icm)
            commcl(c,r) = 0 
         enddo
      enddo
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then

            call par_assoc_buff(node_buffs(nm)%lbc_wet_recv_buff(1),
     &                          node_buffs(nm)%nrecv               )
            call par_get_int(i1s,1)
            call par_get_int(i2s,1)
            call par_get_int(j1s,1)
            call par_get_int(j2s,1)
            call par_get_int(machf,1)
            call par_get_int(nvar,1)
            call par_get_int(nwds,1)
            if (nwds > nbuff_save) then
               if (allocated(pbuff_wet)) deallocate (pbuff_wet)
               allocate (pbuff_wet(nwds))
               nbuff_save=nwds
            endif
            call par_get_float(pbuff_wet,nwds)
            do c=i1s, i2s
               do r=j1s, j2s
                  commcl(c,r) = commcl(c,r) + 1
               enddo
            enddo
c
            nfx=i2s-i1s+1
            nfy=j2s-j1s+1
            mtp=nfx*nfy
            iptr=0
            do nv=1,notimespc
               ispbeg=ipsadep(icm)+(nv-1)*mmyp(icm)*mmxp(icm)
               call unfdback_dp(ptwetfld(ispbeg),pbuff_wet(1+iptr),mtp,
     &                        mmxp(icm),mmyp(icm),mi0(icm),mj0(icm),
     &                        i1s,i2s,j1s,j2s,mynum,notimespc,nv,
     &                        firstime,commcl,ncol(icm),nrow(icm),dx,dy)
               iptr=iptr+mtp
            enddo
         endif
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_get_feed_wet
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_get_feed_pt
c-----------------------------------------------------------------------
c
      subroutine node_get_feed_pt(ifm,icm,dx,dy,depths)                               
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use camxfld
      use grid
      use tracer
c
      implicit none
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c    Description:
c        probing tool
c
c        Get the portion of the coarse grid subdomain that will be filled
c        with fine grid info from each node. 
c
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_WAIT
c       PAR_ASSOC_BUFF
c       PAR_GET_INT
c       PAR_GET_FLOAT
c       UNFDBACKP
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
      include 'chmdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: icm
      integer :: ifm
c
      real    :: dx(*)
      real    :: dy
      real    :: depths(mmxp(icm),mmyp(icm),mmzp(icm))
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i1s
      integer :: i2s
      integer :: j1s
      integer :: j2s
      integer :: k1crs
      integer :: k2crs
      integer :: mtp
      integer :: nfx
      integer :: nfy
      integer :: nfz
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: ibytes
      integer :: msgid
      integer :: ihostnum
      integer :: machf
      integer :: nwds
      integer :: n
      integer :: c
      integer :: r 
      integer :: commcl(ncol(icm),nrow(icm))
c
      logical :: firstime(ntotsp)
c
      real, save, allocatable :: pbuff_pt(:)
      integer, save           :: nbuff_save=0
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      itype=6
c
c  --- make sure all sends are finished and send arrays are de-allocated ---
c
      do nm=1,nmachs
         if (ipaths(1,itype,ifm,nm) .ne. 0) then
            call par_wait(isend_req_pt(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
c  --- wait on receives ---
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_wait(irecv_req_pt(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
      do n=1, ntotsp
         firstime = .true.
      enddo
      do c=1, ncol(icm)
         do r=1, nrow(icm)
            commcl(c,r) = 0 
         enddo
      enddo
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_assoc_buff(node_buffs(nm)%lbc_pt_recv_buff(1),
     &                          node_buffs(nm)%nrecv               )
            call par_get_int(i1s,1)
            call par_get_int(i2s,1)
            call par_get_int(j1s,1)
            call par_get_int(j2s,1)
            call par_get_int(k1crs,1)
            call par_get_int(k2crs,1)
            call par_get_int(machf,1)
            call par_get_int(nvar,1)
            call par_get_int(nwds,1)
            if (nwds > nbuff_save) then
               if (allocated(pbuff_pt)) deallocate (pbuff_pt)
               allocate (pbuff_pt(nwds))
               nbuff_save=nwds
            endif
            call par_get_float(pbuff_pt,nwds)
            do c=i1s, i2s
               do r=j1s, j2s
                  commcl(c,r) = commcl(c,r) + 1
               enddo
            enddo
c
            nfx=i2s-i1s+1
            nfy=j2s-j1s+1
            nfz=k2crs-k1crs+1
            mtp=nfx*nfy*nfz
            iptr=0
            do nv=1,ntotsp
               ispbeg=ipsa3d(icm)+(nv-1)*mmzp(icm)*mmxp(icm)*mmyp(icm)
               call unfdbackp(ptconc(ispbeg),pbuff_pt(1+iptr),mtp,
     &                        mmxp(icm),mmyp(icm),mmzp(icm),
     &                        mi0(icm),mj0(icm),i1s,i2s,j1s,j2s,
     &                        k1crs,k2crs,mynum,ntotsp,nv,firstime,
     &                        commcl,ncol(icm),nrow(icm),dx,dy,depths         )
               iptr=iptr+mtp
            enddo
         endif
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_get_feed_pt
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine unfdbackp
c-----------------------------------------------------------------------
c
      subroutine unfdbackp(ac,acf,mtp,m1,m2,m3,
     &                     i0,j0,
     &                     i1s,i2s,j1s,j2s,k1crs,k2crs,mynum,
     &                     nspec,nv,firstime,commcl,ncol,nrow,
     &                     dx,dy,depths                       )
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
c       NODE_GET_FEED
c       NODE_GET_FEED_PT
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real    :: ac(m1,m2,m3)
      real    :: acf(mtp)
c
      integer :: mtp
      integer :: m1
      integer :: m2
      integer :: m3
      integer :: i0
      integer :: j0
      integer :: i1s
      integer :: i2s
      integer :: j1s
      integer :: j2s
      integer :: k1crs
      integer :: k2crs
      integer :: mynum
      integer :: nspec
      integer :: nv
      integer :: commcl(ncol,nrow)
      integer :: ncol
      integer :: nrow
c
      logical :: firstime(nspec)
c
      real, dimension(*)            :: dx
      real                          :: dy
      real, dimension(ncol,nrow,m3) :: depths
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i
      integer :: j
      integer :: k
      integer :: indcf
      integer :: ii
      integer :: jj
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      indcf = 1
c
      do k=k1crs,k2crs
         do j=j1s,j2s
            do i=i1s,i2s
               if (commcl(i,j) .le. 1) then
                  ac(i-i0,j-j0,k) = 0.
               endif 
            enddo
         enddo
      enddo
      do j=j1s,j2s
         do i=i1s,i2s
            do k=k1crs,k2crs
               ac(i-i0,j-j0,k) = ac(i-i0,j-j0,k) + acf(indcf) /
     &                                                 (dx(j)*dy*depths(i,j,k))
               indcf = indcf + 1
            enddo
         enddo
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine unfdbackp
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine unfdback_dp
c-----------------------------------------------------------------------
c
      subroutine unfdback_dp(ac,acf,mtp,m1,m2,i0,j0,i1s,i2s,j1s,j2s,
     &                      mynum,nspec,nv,firstime,commcl,ncol,nrow,
     &                      dx,dy )
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
c       NODE_GET_FEED_DT
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real    :: ac(m1,m2)
      real    :: acf(mtp)
c
      integer :: mtp
      integer :: m1
      integer :: m2
      integer :: i0
      integer :: j0
      integer :: i1s
      integer :: i2s
      integer :: j1s
      integer :: j2s
      integer :: mynum
      integer :: nspec
      integer :: nv
      integer :: commcl(ncol,nrow)
      integer :: ncol
      integer :: nrow
c
      logical :: firstime(nspec)
c
      real, dimension(*)            :: dx
      real                          :: dy
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i
      integer :: j
      integer :: k
      integer :: indcf
      integer :: ii
      integer :: jj
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      indcf = 1
c
      do j=j1s,j2s
        do i=i1s,i2s
           if (commcl(i,j) .le. 1) then
               ac(i-i0,j-j0) = 0.
            endif 
         enddo
      enddo
c
      do j=j1s,j2s
         do i=i1s,i2s
           ac(i-i0,j-j0) = ac(i-i0,j-j0) + acf(indcf) / (dx(j)*dy)
           indcf = indcf + 1
         enddo
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine unfdback_dp
c-----------------------------------------------------------------------
