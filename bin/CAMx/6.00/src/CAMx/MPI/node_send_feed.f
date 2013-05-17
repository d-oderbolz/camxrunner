      subroutine node_send_feed(ifm,icm,dx,dy,depths)                             
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
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c        Feed back the fine grid's portion of each coarse grid node
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_GET_NOBLOCK
c       MYOFFSET
c       FDBACKP
c       PAR_INIT_PUT
c       PAR_PUT_INT
c       PAR_PUT_FLOAT
c       PAR_SEND_NOBLOCK
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c       03/15/09     Added code for deposition output for tracers
c       10/29/09     Added code for RTRAC surface model
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
      integer :: ifm
      integer :: icm
c
      real :: dx(*)
      real :: dy
      real :: depths(mmxp(ifm),mmyp(ifm),mmzp(ifm))
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
      integer :: i1f
      integer :: i2f
      integer :: j1f
      integer :: j2f
      integer :: k1fine
      integer :: k2fine
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: itypef
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
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
c  --- before send anything, post the receives. ---
c
      do nm=1,nmachs
         irecv_req(nm)=0
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_get_noblock(node_buffs(nm)%lbc_recv_buff(1),
     &                           node_buffs(nm)%nrecv,5500+icm,
     &                           machs(nm),irecv_req(nm)         )
         endif
      enddo
c
c  --- Allocate new temporary buffer if bigger than the old one. ---
c
      if (nbuff_save .eq. 0) then
         if (allocated(pbuff)) deallocate (pbuff)
         allocate(pbuff(mmzp(ifm)*mmxp(ifm)*mmyp(ifm)*nspec))
         nbuff_save = mmzp(ifm)*mmxp(ifm)*mmyp(ifm)*nspec
      endif
      if (node_buffs(mynum)%nrecv > nbuff_save) then
         if (allocated(pbuff)) deallocate (pbuff)
         allocate (pbuff(node_buffs(mynum)%nrecv))
         nbuff_save=node_buffs(mynum)%nrecv
      endif
c
c  -- Feed back this fine grid's portion of the each coarse grid node --
c
      k1fine=1
      k2fine=nlay(ifm)
      k1crs=1
      k2crs=nlay(icm)
      do nm=1,nmachs
         isend_req(nm)=0
         if (ipaths(1,itype,ifm,nm) .ne. 0) then
            i1s=ipaths(1,itype,ifm,nm)
            i2s=ipaths(2,itype,ifm,nm)
            j1s=ipaths(3,itype,ifm,nm)
            j2s=ipaths(4,itype,ifm,nm)
            mtp=(i2s-i1s+1)*(j2s-j1s+1)*(k2crs-k1crs+1)
            itypef=7
            i1f=ipaths(1,itypef,ifm,nm)
            i2f=ipaths(2,itypef,ifm,nm)
            j1f=ipaths(3,itypef,ifm,nm)
            j2f=ipaths(4,itypef,ifm,nm)
            call myoffset(0,i1f,nmesh(ifm),offxb)
            call myoffset(1,i2f,nmesh(ifm),offxe)
            call myoffset(0,j1f,nmesh(ifm),offyb)
            call myoffset(1,j2f,nmesh(ifm),offye)
            iptr=0
c
            do nv=1,nspec
               ispbeg=iptr4d(ifm)+(nv-1)*mmzp(ifm)*mmxp(ifm)*mmyp(ifm)
               call fdbackp(mtp, conc(ispbeg),pbuff(1+iptr),
     &                      mmxp(ifm),mmyp(ifm),mmzp(ifm),
     &                      ncol(ifm),nrow(ifm),nlay(ifm),
     &                      i0,j0,
     &                      ifm,icm,i1f-i0,i2f-i0,j1f-j0,j2f-j0,
     &                      offxb,offxe,offyb,offye,
     &                      nmesh(ifm),nmesh(ifm),
     &                      dx,dy,depths,nv,k1crs,k2crs)
               iptr=iptr+mtp
            enddo
c
            call par_init_put(node_buffs(nm)%lbc_send_buff(1),
     &                        node_buffs(nm)%nsend            )
            call par_put_int(i1s,1)
            call par_put_int(i2s,1)
            call par_put_int(j1s,1)
            call par_put_int(j2s,1)
            call par_put_int(k1crs,1)
            call par_put_int(k2crs,1)
            call par_put_int(mynum,1)
            call par_put_int(nvar,1)
            call par_put_int(iptr,1)
            call par_put_float(pbuff(1),iptr)
            call par_send_noblock(ipaths(5,itype,ifm,nm),5500+icm,
     &                            isend_req(nm)                   )
         endif
      enddo
c
      deallocate( pbuff )
      nbuff_save = 0
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_send_feed:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_send_feed_dp:
c-----------------------------------------------------------------------
c
      subroutine node_send_feed_dp(ifm,icm,dx,dy)
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
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c
c        This version is for the deposition velocities.
c        Feed back the fine grid's portion of each coarse grid node
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_GET_NOBLOCK
c       MYOFFSET
c       FDBACK_DP
c       PAR_INIT_PUT
c       PAR_PUT_INT
c       PAR_PUT_FLOAT
c       PAR_SEND_NOBLOCK
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
      include 'chmdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: ifm
      integer :: icm
c
      real :: dx(*)
      real :: dy
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
      integer :: i1f
      integer :: i2f
      integer :: j1f
      integer :: j2f
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: itypef
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
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
c  --- before send anything, post the receives. ---
c
      do nm=1,nmachs
         irecv_req_dp(nm)=0
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_get_noblock(node_buffs(nm)%lbc_dp_recv_buff(1),
     &                           node_buffs(nm)%nrecv,5500+icm+dp_identifier,
     &                           machs(nm),irecv_req_dp(nm)                  )
         endif
      enddo
c
c  --- Allocate new temporary buffer if bigger than the old one. ---
c
      if (nbuff_save .eq. 0) then
         if (allocated(pbuff_dp)) deallocate (pbuff_dp)
         allocate(pbuff_dp(mmxp(ifm)*mmyp(ifm)*navspc*3))
         nbuff_save = mmxp(ifm)*mmyp(ifm)*navspc*3
      endif
      if (node_buffs(mynum)%nrecv > nbuff_save) then
         if (allocated(pbuff_dp)) deallocate (pbuff_dp)
         allocate (pbuff_dp(node_buffs(mynum)%nrecv))
         nbuff_save=node_buffs(mynum)%nrecv
      endif
c
      do nm=1,nmachs
         isend_req_dp(nm)=0
         if (ipaths(1,itype,ifm,nm).ne.0) then
            i1s=ipaths(1,itype,ifm,nm)
            i2s=ipaths(2,itype,ifm,nm)
            j1s=ipaths(3,itype,ifm,nm)
            j2s=ipaths(4,itype,ifm,nm)
            mtp=(i2s-i1s+1)*(j2s-j1s+1)
            itypef=7
            i1f=ipaths(1,itypef,ifm,nm)
            i2f=ipaths(2,itypef,ifm,nm)
            j1f=ipaths(3,itypef,ifm,nm)
            j2f=ipaths(4,itypef,ifm,nm)
            call myoffset(0,i1f,nmesh(ifm),offxb)
            call myoffset(1,i2f,nmesh(ifm),offxe)
            call myoffset(0,j1f,nmesh(ifm),offyb)
            call myoffset(1,j2f,nmesh(ifm),offye)
            iptr=0
c
            do nv=1,navspc*3
               ispbeg=iptrdp(ifm)+(nv-1)*mmxp(ifm)*mmyp(ifm)
               call fdback_dp(mtp, depfld(ispbeg),pbuff_dp(1+iptr),
     &                      mmxp(ifm),mmyp(ifm),i0,j0,ifm,icm,
     &                      i1f-i0,i2f-i0,j1f-j0,j2f-j0,
     &                      offxb,offxe,offyb,offye,
     &                      nmesh(ifm),nmesh(ifm),dx,dy,nv)
               iptr=iptr+mtp
            enddo
c
            call par_init_put(node_buffs(nm)%lbc_dp_send_buff(1),
     &                        node_buffs(nm)%nsend               )
            call par_put_int(i1s,  1)
            call par_put_int(i2s,  1)
            call par_put_int(j1s,  1)
            call par_put_int(j2s,  1)
            call par_put_int(mynum,1)
            call par_put_int(nvar, 1)
            call par_put_int(iptr, 1)
            call par_put_float(pbuff_dp(1),iptr)
            call par_send_noblock(ipaths(5,itype,ifm,nm),5500+icm+dp_identifier,
     &                            isend_req_dp(nm)                             )
         endif
      enddo
c
      deallocate( pbuff_dp )
      nbuff_save = 0
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_send_feed_dp:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_send_feed_pt:
c-----------------------------------------------------------------------
c
      subroutine node_send_feed_pt(ifm,icm,dx,dy,depths)                             
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
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c        probing tool
c        Feed back the fine grid's portion of each coarse grid node
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_GET_NOBLOCK
c       MYOFFSET
c       FDBACKP
c       PAR_INIT_PUT
c       PAR_PUT_INT
c       PAR_PUT_FLOAT
c       PAR_SEND_NOBLOCK
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
      include 'chmdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: ifm
      integer :: icm
c
      real :: dx(*)
      real :: dy
      real :: depths(mmxp(ifm),mmyp(ifm),mmzp(ifm))
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
      integer :: i1f
      integer :: i2f
      integer :: j1f
      integer :: j2f
      integer :: k1fine
      integer :: k2fine
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: itypef
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
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
c  --- before send anything, post the receives. ---
c
      do nm=1,nmachs
         irecv_req_pt(nm)=0
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_get_noblock(node_buffs(nm)%lbc_pt_recv_buff(1),
     &                           node_buffs(nm)%nrecv,5500+icm+pt_identifier,
     &                           machs(nm),irecv_req_pt(nm)                  )
         endif
      enddo
c
c  --- Allocate new temporary buffer if bigger than the old one. ---
c
      if (nbuff_save .eq. 0) then
         if (allocated(pbuff_pt)) deallocate (pbuff_pt)
         allocate(pbuff_pt(mmzp(ifm)*mmxp(ifm)*mmyp(ifm)*ntotsp))
         nbuff_save = mmzp(ifm)*mmxp(ifm)*mmyp(ifm)*ntotsp
      endif
      if (node_buffs(mynum)%nrecv > nbuff_save) then
         if (allocated(pbuff_pt)) deallocate (pbuff_pt)
         allocate (pbuff_pt(node_buffs(mynum)%nrecv))
         nbuff_save=node_buffs(mynum)%nrecv
      endif
c
c --- Feed back this fine grid's portion of the each coarse grid node ---
c
      k1crs=1
      k2crs=nlay(icm)
      k1fine=1
      k2fine=nlay(ifm)
c
      do nm=1,nmachs
         isend_req_pt(nm)=0
         if (ipaths(1,itype,ifm,nm).ne.0) then
            i1s=ipaths(1,itype,ifm,nm)
            i2s=ipaths(2,itype,ifm,nm)
            j1s=ipaths(3,itype,ifm,nm)
            j2s=ipaths(4,itype,ifm,nm)
            mtp=(i2s-i1s+1)*(j2s-j1s+1)*(k2crs-k1crs+1)
            itypef=7
            i1f=ipaths(1,itypef,ifm,nm)
            i2f=ipaths(2,itypef,ifm,nm)
            j1f=ipaths(3,itypef,ifm,nm)
            j2f=ipaths(4,itypef,ifm,nm)
            call myoffset(0,i1f,nmesh(ifm),offxb)
            call myoffset(1,i2f,nmesh(ifm),offxe)
            call myoffset(0,j1f,nmesh(ifm),offyb)
            call myoffset(1,j2f,nmesh(ifm),offye)
            iptr=0
c
            do nv=1,ntotsp
               ispbeg=ipsa3d(ifm)+(nv-1)*mmzp(ifm)*mmxp(ifm)*mmyp(ifm)
               call fdbackp(mtp, ptconc(ispbeg),pbuff_pt(1+iptr),
     &                      mmxp(ifm),mmyp(ifm),mmzp(ifm),
     &                      ncol(ifm),nrow(ifm),nlay(ifm),
     &                      i0,j0,
     &                      ifm,icm,i1f-i0,i2f-i0,j1f-j0,j2f-j0,
     &                      offxb,offxe,offyb,offye,
     &                      nmesh(ifm),nmesh(ifm),
     &                      dx,dy,depths,nv,k1crs,k2crs)
               iptr=iptr+mtp
            enddo
c
            call par_init_put(node_buffs(nm)%lbc_pt_send_buff(1),
     &                        node_buffs(nm)%nsend               )
            call par_put_int(i1s,  1)
            call par_put_int(i2s,  1)
            call par_put_int(j1s,  1)
            call par_put_int(j2s,  1)
            call par_put_int(k1crs,1)
            call par_put_int(k2crs,1)
            call par_put_int(mynum,1)
            call par_put_int(nvar, 1)
            call par_put_int(iptr, 1)
            call par_put_float(pbuff_pt(1),iptr)
            call par_send_noblock(ipaths(5,itype,ifm,nm),5500+icm+pt_identifier,
     &                            isend_req_pt(nm)                             )
         endif
      enddo
c
      deallocate( pbuff_pt )
      nbuff_save = 0
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_send_feed_pt:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_send_feed_dry:
c-----------------------------------------------------------------------
c
      subroutine node_send_feed_dry(ifm,icm,dx,dy)
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
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c
c        This version is for the deposition velocities. 
c        Feed back the fine grid's portion of each coarse grid node
c        This version is for the dry deposition tracer array.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_GET_NOBLOCK
c       MYOFFSET
c       FDBACK_DP
c       PAR_INIT_PUT
c       PAR_PUT_INT
c       PAR_PUT_FLOAT
c       PAR_SEND_NOBLOCK
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
      include 'chmdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: ifm
      integer :: icm
c
      real :: dx(*)
      real :: dy
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
      integer :: i1f
      integer :: i2f
      integer :: j1f
      integer :: j2f
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: itypef
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
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
c  --- before send anything, post the receives. ---
c
      do nm=1,nmachs
         irecv_req_dry(nm)=0
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_get_noblock(node_buffs(nm)%lbc_dry_recv_buff(1),
     &                           node_buffs(nm)%nrecv,5500+icm+dry_identifier,
     &                           machs(nm),irecv_req_dry(nm)                  )
         endif
      enddo
c
c  --- Allocate new temporary buffer if bigger than the old one. ---
c
      if (nbuff_save .eq. 0) then
         if (allocated(pbuff_dry)) deallocate (pbuff_dry)
         allocate(pbuff_dry(mmxp(ifm)*mmyp(ifm)*notimespc))
         nbuff_save = mmxp(ifm)*mmyp(ifm)*notimespc
      endif
      if (node_buffs(mynum)%nrecv > nbuff_save) then
         if (allocated(pbuff_dry)) deallocate (pbuff_dry)
         allocate (pbuff_dry(node_buffs(mynum)%nrecv))
         nbuff_save=node_buffs(mynum)%nrecv
      endif
c
      do nm=1,nmachs
         isend_req_dry(nm)=0
         if (ipaths(1,itype,ifm,nm).ne.0) then
            i1s=ipaths(1,itype,ifm,nm)
            i2s=ipaths(2,itype,ifm,nm)
            j1s=ipaths(3,itype,ifm,nm)
            j2s=ipaths(4,itype,ifm,nm)
            mtp=(i2s-i1s+1)*(j2s-j1s+1)
            itypef=7
            i1f=ipaths(1,itypef,ifm,nm)
            i2f=ipaths(2,itypef,ifm,nm)
            j1f=ipaths(3,itypef,ifm,nm)
            j2f=ipaths(4,itypef,ifm,nm)
            call myoffset(0,i1f,nmesh(ifm),offxb)
            call myoffset(1,i2f,nmesh(ifm),offxe)
            call myoffset(0,j1f,nmesh(ifm),offyb)
            call myoffset(1,j2f,nmesh(ifm),offye)
            iptr=0
c
            do nv=1,notimespc
               ispbeg=ipsadep(ifm)+(nv-1)*mmxp(ifm)*mmyp(ifm)
               call fdback_dp(mtp, ptdryfld(ispbeg),pbuff_dry(1+iptr),
     &                      mmxp(ifm),mmyp(ifm),i0,j0,ifm,icm,
     &                      i1f-i0,i2f-i0,j1f-j0,j2f-j0,
     &                      offxb,offxe,offyb,offye,
     &                      nmesh(ifm),nmesh(ifm),dx,dy,nv)
               iptr=iptr+mtp
            enddo
c
            call par_init_put(node_buffs(nm)%lbc_dry_send_buff(1),
     &                        node_buffs(nm)%nsend               )
            call par_put_int(i1s,  1)
            call par_put_int(i2s,  1)
            call par_put_int(j1s,  1)
            call par_put_int(j2s,  1)
            call par_put_int(mynum,1)
            call par_put_int(nvar, 1)
            call par_put_int(iptr, 1)
            call par_put_float(pbuff_dry(1),iptr)
            call par_send_noblock(ipaths(5,itype,ifm,nm),5500+icm+dry_identifier,
     &                            isend_req_dry(nm)                             )
         endif
      enddo
c
      deallocate( pbuff_dry )
      nbuff_save = 0
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_send_feed_dp:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_send_feed_wet:
c-----------------------------------------------------------------------
c
      subroutine node_send_feed_wet(ifm,icm,dx,dy)
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
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c
c        This version is for the deposition velocities. 
c        Feed back the fine grid's portion of each coarse grid node
c        This version is for the wet deposition tracer array.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_GET_NOBLOCK
c       MYOFFSET
c       FDBACK_DP
c       PAR_INIT_PUT
c       PAR_PUT_INT
c       PAR_PUT_FLOAT
c       PAR_SEND_NOBLOCK
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
      include 'chmdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: ifm
      integer :: icm
c
      real :: dx(*)
      real :: dy
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
      integer :: i1f
      integer :: i2f
      integer :: j1f
      integer :: j2f
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: itypef
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
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
c  --- before send anything, post the receives. ---
c
      do nm=1,nmachs
         irecv_req_wet(nm)=0
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_get_noblock(node_buffs(nm)%lbc_wet_recv_buff(1),
     &                           node_buffs(nm)%nrecv,5500+icm+wet_identifier,
     &                           machs(nm),irecv_req_wet(nm)                  )
         endif
      enddo
c
c  --- Allocate new temporary buffer if bigger than the old one. ---
c
      if (nbuff_save .eq. 0) then
         if (allocated(pbuff_wet)) deallocate (pbuff_wet)
         allocate(pbuff_wet(mmxp(ifm)*mmyp(ifm)*notimespc))
         nbuff_save = mmxp(ifm)*mmyp(ifm)*notimespc
      endif
      if (node_buffs(mynum)%nrecv > nbuff_save) then
         if (allocated(pbuff_wet)) deallocate (pbuff_wet)
         allocate (pbuff_wet(node_buffs(mynum)%nrecv))
         nbuff_save=node_buffs(mynum)%nrecv
      endif
c
      do nm=1,nmachs
         isend_req_wet(nm)=0
         if (ipaths(1,itype,ifm,nm).ne.0) then
            i1s=ipaths(1,itype,ifm,nm)
            i2s=ipaths(2,itype,ifm,nm)
            j1s=ipaths(3,itype,ifm,nm)
            j2s=ipaths(4,itype,ifm,nm)
            mtp=(i2s-i1s+1)*(j2s-j1s+1)
            itypef=7
            i1f=ipaths(1,itypef,ifm,nm)
            i2f=ipaths(2,itypef,ifm,nm)
            j1f=ipaths(3,itypef,ifm,nm)
            j2f=ipaths(4,itypef,ifm,nm)
            call myoffset(0,i1f,nmesh(ifm),offxb)
            call myoffset(1,i2f,nmesh(ifm),offxe)
            call myoffset(0,j1f,nmesh(ifm),offyb)
            call myoffset(1,j2f,nmesh(ifm),offye)
            iptr=0
c
            do nv=1,notimespc
               ispbeg=ipsadep(ifm)+(nv-1)*mmxp(ifm)*mmyp(ifm)
               call fdback_dp(mtp, ptwetfld(ispbeg),pbuff_wet(1+iptr),
     &                      mmxp(ifm),mmyp(ifm),i0,j0,ifm,icm,
     &                      i1f-i0,i2f-i0,j1f-j0,j2f-j0,
     &                      offxb,offxe,offyb,offye,
     &                      nmesh(ifm),nmesh(ifm),dx,dy,nv)
               iptr=iptr+mtp
            enddo
c
            call par_init_put(node_buffs(nm)%lbc_wet_send_buff(1),
     &                        node_buffs(nm)%nsend               )
            call par_put_int(i1s,  1)
            call par_put_int(i2s,  1)
            call par_put_int(j1s,  1)
            call par_put_int(j2s,  1)
            call par_put_int(mynum,1)
            call par_put_int(nvar, 1)
            call par_put_int(iptr, 1)
            call par_put_float(pbuff_wet(1),iptr)
            call par_send_noblock(ipaths(5,itype,ifm,nm),5500+icm+wet_identifier,
     &                            isend_req_wet(nm)                             )
         endif
      enddo
c
      deallocate( pbuff_wet )
      nbuff_save = 0
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_send_feed_wet:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_send_feed_rtsol:
c-----------------------------------------------------------------------
c
      subroutine node_send_feed_rtsol(ifm,icm,dx,dy)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use camxfld
      use grid
      use tracer
      use rtracchm
c
      implicit none
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c
c        This version is for the deposition velocities. 
c        Feed back the fine grid's portion of each coarse grid node
c        This version is for the RTRAC surface model.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_GET_NOBLOCK
c       MYOFFSET
c       FDBACK_DP
c       PAR_INIT_PUT
c       PAR_PUT_INT
c       PAR_PUT_FLOAT
c       PAR_SEND_NOBLOCK
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
      include 'chmdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: ifm
      integer :: icm
c
      real :: dx(*)
      real :: dy
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
      integer :: i1f
      integer :: i2f
      integer :: j1f
      integer :: j2f
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: itypef
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
c
      real, save, allocatable :: pbuff_rtsol(:)
      integer, save           :: nbuff_save=0
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      itype=6
c
c  --- before send anything, post the receives. ---
c
      do nm=1,nmachs
         irecv_req_rtsol(nm)=0
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_get_noblock(node_buffs(nm)%lbc_rtsol_recv_buff(1),
     & node_buffs(nm)%nrecv,5500+icm+rtsol_identifier,
     &                           machs(nm),irecv_req_rtsol(nm) )
         endif
      enddo
c
c  --- Allocate new temporary buffer if bigger than the old one. ---
c
      if (nbuff_save .eq. 0) then
         if (allocated(pbuff_rtsol)) deallocate (pbuff_rtsol)
         allocate(pbuff_rtsol(mmxp(ifm)*mmyp(ifm)*ntotsp))
         nbuff_save = mmxp(ifm)*mmyp(ifm)*ntotsp
      endif
      if (node_buffs(mynum)%nrecv > nbuff_save) then
         if (allocated(pbuff_rtsol)) deallocate (pbuff_rtsol)
         allocate (pbuff_rtsol(node_buffs(mynum)%nrecv))
         nbuff_save=node_buffs(mynum)%nrecv
      endif
c
      do nm=1,nmachs
         isend_req_rtsol(nm)=0
         if (ipaths(1,itype,ifm,nm).ne.0) then
            i1s=ipaths(1,itype,ifm,nm)
            i2s=ipaths(2,itype,ifm,nm)
            j1s=ipaths(3,itype,ifm,nm)
            j2s=ipaths(4,itype,ifm,nm)
            mtp=(i2s-i1s+1)*(j2s-j1s+1)
            itypef=7
            i1f=ipaths(1,itypef,ifm,nm)
            i2f=ipaths(2,itypef,ifm,nm)
            j1f=ipaths(3,itypef,ifm,nm)
            j2f=ipaths(4,itypef,ifm,nm)
            call myoffset(0,i1f,nmesh(ifm),offxb)
            call myoffset(1,i2f,nmesh(ifm),offxe)
            call myoffset(0,j1f,nmesh(ifm),offyb)
            call myoffset(1,j2f,nmesh(ifm),offye)
            iptr=0
c
            do nv=1,ntotsp
               ispbeg=ipsa2d(ifm)+(nv-1)*mmxp(ifm)*mmyp(ifm)
               call fdback_dp(mtp,rtsolmas(ispbeg),pbuff_rtsol(1+iptr),
     &                      mmxp(ifm),mmyp(ifm),i0,j0,ifm,icm,
     &                      i1f-i0,i2f-i0,j1f-j0,j2f-j0,
     &                      offxb,offxe,offyb,offye,
     &                      nmesh(ifm),nmesh(ifm),dx,dy,nv)
               iptr=iptr+mtp
            enddo
c
            call par_init_put(node_buffs(nm)%lbc_rtsol_send_buff(1),
     &                        node_buffs(nm)%nsend               )
            call par_put_int(i1s,  1)
            call par_put_int(i2s,  1)
            call par_put_int(j1s,  1)
            call par_put_int(j2s,  1)
            call par_put_int(mynum,1)
            call par_put_int(nvar, 1)
            call par_put_int(iptr, 1)
            call par_put_float(pbuff_rtsol(1),iptr)
            call par_send_noblock(ipaths(5,itype,ifm,nm),5500+icm+rtsol_identifier,
     &                            isend_req_rtsol(nm) )
         endif
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_send_feed_rtsol:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_send_feed_rtveg:
c-----------------------------------------------------------------------
c
      subroutine node_send_feed_rtveg(ifm,icm,dx,dy)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use camxfld
      use grid
      use tracer
      use rtracchm
c
      implicit none
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c
c        This version is for the deposition velocities.
c        Feed back the fine grid's portion of each coarse grid node
c        This version is for the RTRAC surface model.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:
c     Output:
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_GET_NOBLOCK
c       MYOFFSET
c       FDBACK_DP
c       PAR_INIT_PUT
c       PAR_PUT_INT
c       PAR_PUT_FLOAT
c       PAR_SEND_NOBLOCK
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
      include 'chmdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: ifm
      integer :: icm
c
      real :: dx(*)
      real :: dy
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
      integer :: i1f
      integer :: i2f
      integer :: j1f
      integer :: j2f
      integer :: ispbeg
      integer :: nm
      integer :: itype
      integer :: itypef
      integer :: nv
      integer :: iptr
      integer :: nvar
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
c
      real, save, allocatable :: pbuff_rtveg(:)
      integer, save           :: nbuff_save=0
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      itype=6
c
c  --- before send anything, post the receives. ---
c
      do nm=1,nmachs
         irecv_req_rtveg(nm)=0
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            call par_get_noblock(node_buffs(nm)%lbc_rtveg_recv_buff(1),
     &                           node_buffs(nm)%nrecv,5500+icm+rtveg_identifier,
     &                           machs(nm),irecv_req_rtveg(nm)                  )
         endif
      enddo
c
c  --- Allocate new temporary buffer if bigger than the old one. ---
c
      if (nbuff_save .eq. 0) then
         if (allocated(pbuff_rtveg)) deallocate (pbuff_rtveg)
         allocate(pbuff_rtveg(mmxp(ifm)*mmyp(ifm)*ntotsp))
         nbuff_save = mmxp(ifm)*mmyp(ifm)*ntotsp
      endif
      if (node_buffs(mynum)%nrecv > nbuff_save) then
         if (allocated(pbuff_rtveg)) deallocate (pbuff_rtveg)
         allocate (pbuff_rtveg(node_buffs(mynum)%nrecv))
         nbuff_save=node_buffs(mynum)%nrecv
      endif
c
      do nm=1,nmachs
         isend_req_rtveg(nm)=0
         if (ipaths(1,itype,ifm,nm).ne.0) then
            i1s=ipaths(1,itype,ifm,nm)
            i2s=ipaths(2,itype,ifm,nm)
            j1s=ipaths(3,itype,ifm,nm)
            j2s=ipaths(4,itype,ifm,nm)
            mtp=(i2s-i1s+1)*(j2s-j1s+1)
            itypef=7
            i1f=ipaths(1,itypef,ifm,nm)
            i2f=ipaths(2,itypef,ifm,nm)
            j1f=ipaths(3,itypef,ifm,nm)
            j2f=ipaths(4,itypef,ifm,nm)
            call myoffset(0,i1f,nmesh(ifm),offxb)
            call myoffset(1,i2f,nmesh(ifm),offxe)
            call myoffset(0,j1f,nmesh(ifm),offyb)
            call myoffset(1,j2f,nmesh(ifm),offye)
            iptr=0
c
            do nv=1,ntotsp
               ispbeg=ipsa2d(ifm)+(nv-1)*mmxp(ifm)*mmyp(ifm)
               call fdback_dp(mtp,rtvegmas(ispbeg),pbuff_rtveg(1+iptr),
     &                      mmxp(ifm),mmyp(ifm),i0,j0,ifm,icm,
     &                      i1f-i0,i2f-i0,j1f-j0,j2f-j0,
     &                      offxb,offxe,offyb,offye,
     &                      nmesh(ifm),nmesh(ifm),dx,dy,nv)
               iptr=iptr+mtp
            enddo
c
            call par_init_put(node_buffs(nm)%lbc_rtveg_send_buff(1),
     &                        node_buffs(nm)%nsend               )
            call par_put_int(i1s,  1)
            call par_put_int(i2s,  1)
            call par_put_int(j1s,  1)
            call par_put_int(j2s,  1)
            call par_put_int(mynum,1)
            call par_put_int(nvar, 1)
            call par_put_int(iptr, 1)
            call par_put_float(pbuff_rtveg(1),iptr)
            call par_send_noblock(ipaths(5,itype,ifm,nm),5500+icm+rtveg_identifier,
     &                            isend_req_rtveg(nm)                              )
         endif
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_send_feed_rtveg:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine fdbackp:
c-----------------------------------------------------------------------
c
      subroutine fdbackp(size_dat_coarse,dat_fine,dat_coarse,
     &                   m1,m2,m3, 
     &                   nx,ny,nz,
     &                   ioff,joff,
     &                   igrd,icm,
     &                   ibeg,iend,jbeg,jend,
     &                   offxb,offxe,offyb,offye,
     &                   nestratx,nestraty,
     &                   dx,dy,depths,nv,k1c,k2c)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use master_mod
      use node_mod
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
c     Output:  
c
c    Called by:
c       NODE_SEND_FEED
c       NODE_SEND_FEED_PT
c
c     Copyright 1996 - 2013
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
      real, dimension(m1,m2,m3) :: dat_fine
      real, dimension(*)        :: dat_coarse
c
      integer :: size_dat_coarse
      integer :: m1
      integer :: m2
      integer :: m3
      integer :: igrd
      integer :: icm
      integer :: nx
      integer :: ny
      integer :: nz
      integer :: ioff
      integer :: joff
      integer :: ibeg
      integer :: iend
      integer :: jbeg
      integer :: jend
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
      integer :: nv
      integer :: k1c
      integer :: k2c
c
c  --- nmesh in x/y direction ---
c
      integer :: nestratx
      integer :: nestraty
c
      real, dimension(*)        :: dx
      real                      :: dy
      real, dimension(m1,m2,m3) :: depths
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
      integer :: kcrs
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      dat_coarse(1:size_dat_coarse) = 0
      indcf = 0
c
      do j = jbeg-offyb,jend+offye,nestraty
         do i = ibeg-offxb,iend+offxe,nestratx
            do kcrs=k1c,k2c
               indcf = indcf + 1 
                  k = kcrs
                  do jj = j, j+nestraty-1
                     do ii = i, i+nestratx-1
                        if (ii .lt. ibeg .or. ii .gt. iend .or. 
     &                      jj .lt. jbeg .or. jj .gt. jend) goto 3000
                        if( ii .LT. mia(igrd) ) goto 3000
                        if( ii .GT. miz(igrd) ) goto 3000
                        if( jj .LT. mja(igrd) ) goto 3000
                        if( jj .GT. mjz(igrd) ) goto 3000
                        dat_coarse(indcf) = dat_coarse(indcf) +
     &                              dat_fine(ii,jj,k) * dx(jj+joff)*dy*
     &                                                  depths(ii,jj,K)
c
 3000 continue
                     enddo
                  enddo
            enddo
         enddo
      enddo
      if (indcf .ne. size_dat_coarse) then
         stop 'size_dat_coarse and indcf not consistant in fdbackup'
      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine fdbackp:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine fdback_dp:
c-----------------------------------------------------------------------
c
      subroutine fdback_dp(size_dat_coarse,dat_fine,dat_coarse,m1,m2,
     &                     ioff,joff,igrd,icm,ibeg,iend,jbeg,jend,
     &                     offxb,offxe,offyb,offye,nestratx,nestraty,
     &                     dx,dy,nv)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use master_mod
      use node_mod
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
c     Output:  
c
c    Called by:
c       NODE_SEND_FEED_DP
c
c     Copyright 1996 - 2013
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
      real, dimension(m1,m2) :: dat_fine
      real, dimension(*)     :: dat_coarse
c
      integer :: size_dat_coarse
      integer :: m1
      integer :: m2
      integer :: igrd
      integer :: icm
      integer :: ioff
      integer :: joff
      integer :: ibeg
      integer :: iend
      integer :: jbeg
      integer :: jend
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
      integer :: nv
c
c  --- nmesh in x/y direction ---
c
      integer :: nestratx
      integer :: nestraty
c
      real, dimension(*)        :: dx
      real                      :: dy
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i
      integer :: j
      integer :: indcf
      integer :: ii
      integer :: jj
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      dat_coarse(1:size_dat_coarse) = 0
      indcf = 0
c
      do j = jbeg-offyb,jend+offye,nestraty
         do i = ibeg-offxb,iend+offxe,nestratx
            indcf = indcf + 1 
            do jj = j, j+nestraty-1
              do ii = i, i+nestratx-1
                if (ii .lt. ibeg .or. ii .gt. iend .or. 
     &                     jj .lt. jbeg .or. jj .gt. jend) goto 3000
                if( ii .LT. mia(igrd) ) goto 3000
                if( ii .GT. miz(igrd) ) goto 3000
                if( jj .LT. mja(igrd) ) goto 3000
                if( jj .GT. mjz(igrd) ) goto 3000
                dat_coarse(indcf) = dat_coarse(indcf) + 
     &                                dat_fine(ii,jj) * dx(jj+joff)*dy
c
 3000 continue
              enddo
            enddo
         enddo
      enddo
c
      if (indcf .ne. size_dat_coarse) then
         stop 'size_dat_coarse and indcf not consistant in fdback_dp'
      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine fdback_dp:
c-----------------------------------------------------------------------
