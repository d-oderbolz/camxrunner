      subroutine node_send_nbc(ifm,icm)                                          
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use grid
      use camxfld
      use grid_dims
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
c       NESTING
c    Subroutines called:
c       PAR_GET_NOBLOCK
c       MYOFFSET
c       MKNEST_BUFF
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
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: nm
      integer :: itype
      integer :: mtp
      integer :: iptr
      integer :: nv
      integer :: ispbeg
      integer :: i1c
      integer :: i2c
      integer :: j1c
      integer :: j2c
      integer :: k1c
      integer :: k2c
      integer :: i1f
      integer :: i2f
      integer :: j1f
      integer :: j2f
      integer :: k1f
      integer :: k2f
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
      integer :: nbuff_save
c
      real, allocatable, save :: buffnest(:)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      nbuff_save = 0
      itype = 6
      k1c = 1
      k2c = nlay(icm)
      k1f = 1
      k2f = nlay(ifm)
c
c  --- First, before send anything, post the receives. ---
c
      do nm=1,nmachs
         irecv_req(nm)=0
         if (ipaths(5,6,ifm,nm) .ne. 0) then
            call par_get_noblock(node_buffs(nm)%lbc_recv_buff(1),
     &                          node_buffs(nm)%nrecv,6000+icm,nm,irecv_req(nm))
         endif
      enddo
      if (nbuff_save .eq. 0) then
         if (allocated(buffnest)) deallocate (buffnest)
         allocate( buffnest( 10*(mmxp(ifm)+mmyp(ifm))*mmzp(ifm)*nspec ) )
         nbuff_save = 10*(mmxp(ifm)+mmyp(ifm))*mmzp(ifm)*nspec
      endif
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            i1c=bpaths(1,6,ifm,nm)
            i2c=bpaths(2,6,ifm,nm)
            j1c=bpaths(3,6,ifm,nm)
            j2c=bpaths(4,6,ifm,nm)
            i1f=bpaths(1,7,ifm,nm)
            i2f=bpaths(2,7,ifm,nm)
            j1f=bpaths(3,7,ifm,nm)
            j2f=bpaths(4,7,ifm,nm)
            call myoffset(0,i1f,nmesh(ifm),offxb)
            call myoffset(1,i2f,nmesh(ifm),offxe)
            call myoffset(0,j1f,nmesh(ifm),offyb)
            call myoffset(1,j2f,nmesh(ifm),offye)
            mtp = ( (j2f-j1f+1) + (i2f-i1f+1) )*2*(k2f-k1f+1)
            iptr=0
            do nv=1,nspec
               ispbeg=iptr4d(icm)+(nv-1)*mmzp(icm)*mmxp(icm)*mmyp(icm)
               call mknest_buff(conc(ispbeg),mtp,buffnest(1+iptr),
     &                          mmxp(icm),mmyp(icm),mmzp(icm),
     &                          mi0(icm),mj0(icm),
     &                          i1c,i2c,j1c,j2c,
     &                          i1f,i2f,j1f,j2f,
     &                          offxb,offxe,offyb,offye,
     &                          k1c,k2c,nmesh(ifm),nmesh(ifm),icm)
               iptr=iptr+mtp
            enddo
            call par_init_put(node_buffs(nm)%lbc_send_buff(1),
     &                        node_buffs(nm)%nsend )
            call par_put_int(i1f,   1)
            call par_put_int(i2f,   1)
            call par_put_int(j1f,   1)
            call par_put_int(j2f,   1)
            call par_put_int(k1f,   1)
            call par_put_int(k2f,   1)
            call par_put_int(i1c,   1)
            call par_put_int(i2c,   1)
            call par_put_int(j1c,   1)
            call par_put_int(j2c,   1)
            call par_put_int(mia(icm)+mi0(icm),   1)
            call par_put_int(miz(icm)+mi0(icm),   1)
            call par_put_int(mja(icm)+mj0(icm),   1)
            call par_put_int(mjz(icm)+mj0(icm),   1)
            call par_put_int(mynum, 1)
            call par_put_int(nspec, 1)
            call par_put_int(iptr,  1)
            call par_put_float(buffnest(1),iptr)
            if (iget_paths(itype,ifm,nm) .ne. bpaths(5,5,ifm,nm)) then
               stop 'iget_paths and bpaths are inconsistent'
            endif
            isend_req(nm) = 0
            call par_send_noblock(iget_paths(itype,ifm,nm), 6000+icm, isend_req(nm))
         endif
      enddo
c
      deallocate( buffnest )
      nbuff_save = 0
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_send_nbc:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine mknest_buff:
c-----------------------------------------------------------------------
c
      subroutine mknest_buff(cnc,mtp,fcnc_snd,
     &                       m1,m2,m3,i0,j0,
     &                       i1c,i2c,j1c,j2c,
     &                       i1f,i2f,j1f,j2f,
     &                       offxb,offxe,offyb,offye,
     &                       k1c,k2c,nestratx,nestraty,icm)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use grid
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
c       NODE_SEND_NBC
c       NODE_SEND_NBC_PT
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
      real    :: cnc(m1,m2,m3)
c
      integer :: mtp
      real    :: fcnc_snd(mtp)
      integer :: m1
      integer :: m2
      integer :: m3
      integer :: i0
      integer :: j0
      integer :: i1c
      integer :: i2c
      integer :: j1c
      integer :: j2c
      integer :: i1f
      integer :: i2f
      integer :: j1f
      integer :: j2f
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
      integer :: k1c
      integer :: k2c
      integer :: nestratx
      integer :: nestraty
      integer :: icm
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i
      integer :: j
      integer :: k
      integer :: ii
      integer :: jj
      integer :: counter
      integer :: len_i
      integer :: len_j
      integer :: Li
      integer :: Lj
      integer :: kcrs
c
      real    :: tempbnd(mtp)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (i1c .eq. 1 .or. i2c .eq. ncol(icm) .or.
     &                                 j1c .eq. 1 .or. j2c .eq. nrow(icm)) then
         stop 'config is beyond the control of mknest_buff'
      endif 
c
      counter = 0
      len_i = i2f - i1f + 1
      len_j = j2f - j1f + 1 
c
      do kcrs=k1c,k2c
         k = kcrs
c
c  --- south ---
c
            Li=0
            do i=i1c, i2c
               do ii=1, nestratx
                  Li = Li + 1
                  tempbnd(Li)=cnc(i-i0,j1c-1-j0,kcrs)
               enddo
            enddo
            if (offxb+len_i .gt. Li) then 
               stop 'exception mknest_buff south'
            endif
            do i=offxb+1, offxb+len_i
               counter = counter + 1
               fcnc_snd(counter)=tempbnd(i)
            enddo
c
c  --- north ---
c
            Li=0
            do i=i1c, i2c
               do ii=1, nestratx
                  Li = Li + 1
                  tempbnd(Li)=cnc(i-i0,j2c+1-j0,kcrs)
               enddo
            enddo
            if (offxb+len_i .gt. Li) then
               stop 'exception mknest_buff north'
            endif
            do i=offxb+1, offxb+len_i
               counter = counter + 1
               fcnc_snd(counter)=tempbnd(i)
            enddo
c
c --- west ---
c
            Lj=0
            do j=j1c, j2c
               do jj=1, nestraty
                  Lj = Lj + 1
                  tempbnd(Lj)=cnc(i1c-1-i0,j-j0,kcrs)
               enddo
            enddo
            if (offyb+len_j .gt. Lj) then
               stop 'exception mknest_buff west'
            endif
            do j=offyb+1, offyb+len_j
               counter = counter + 1
               fcnc_snd(counter)=tempbnd(j)
            enddo
c
c  --- east ---
c
            Lj=0
            do j=j1c, j2c
               do jj=1, nestraty
                  Lj = Lj + 1
                  tempbnd(Lj)=cnc(i2c+1-i0,j-j0,kcrs)
               enddo
            enddo
            do j=offyb+1, offyb+len_j
               counter = counter + 1
               fcnc_snd(counter)=tempbnd(j)
            enddo
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine mknest_buff:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_send_nbc_pt:
c-----------------------------------------------------------------------
c
      subroutine node_send_nbc_pt(ifm,icm)                                          
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use grid
      use camxfld
      use grid_dims
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
c     Output:  
c
c    Called by:
c       NESTING
c    Subroutines called:
c       PAR_GET_NOBLOCK
c       MYOFFSET
c       MKNEST_BUFF
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
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: nm
      integer :: itype
      integer :: mtp
      integer :: iptr
      integer :: nv
      integer :: ispbeg
      integer :: i1c
      integer :: i2c
      integer :: j1c
      integer :: j2c
      integer :: k1c
      integer :: k2c
      integer :: i1f
      integer :: i2f
      integer :: j1f
      integer :: j2f
      integer :: k1f
      integer :: k2f
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
      integer :: nbuff_save_pt
c
      real, allocatable, save :: buffnest_pt(:)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      nbuff_save_pt = 0
      itype = 6
c
      k1c = 1
      k2c = nlay(icm)
      k1f = 1
      k2f = nlay(ifm)
c
c  --- First, before send anything, post the receives. ---
c
      do nm=1,nmachs
         irecv_req_pt(nm)=0
         if (ipaths(5,6,ifm,nm) .ne. 0) then
            call par_get_noblock(node_buffs(nm)%lbc_pt_recv_buff(1),  !cbwpt
     &                           node_buffs(nm)%nrecv,6000+icm+pt_identifier,
     &                           nm,irecv_req_pt(nm)                         )
         endif
      enddo
      if (nbuff_save_pt .eq. 0) then
         if (allocated(buffnest_pt)) deallocate (buffnest_pt)
         allocate( buffnest_pt( 10*(mmxp(ifm)+mmyp(ifm))*mmzp(ifm)*ntotsp ) )
         nbuff_save_pt = 10*(mmxp(ifm)+mmyp(ifm))*mmzp(ifm)*ntotsp
      endif   
c
      do nm=1,nmachs
         if (iget_paths(itype,ifm,nm) .ne. 0) then
            i1c=bpaths(1,6,ifm,nm)
            i2c=bpaths(2,6,ifm,nm)
            j1c=bpaths(3,6,ifm,nm)
            j2c=bpaths(4,6,ifm,nm)
            i1f=bpaths(1,7,ifm,nm)
            i2f=bpaths(2,7,ifm,nm)
            j1f=bpaths(3,7,ifm,nm)
            j2f=bpaths(4,7,ifm,nm)
            call myoffset(0,i1f,nmesh(ifm),offxb)
            call myoffset(1,i2f,nmesh(ifm),offxe)
            call myoffset(0,j1f,nmesh(ifm),offyb)
            call myoffset(1,j2f,nmesh(ifm),offye)
            mtp = ( (j2f-j1f+1) + (i2f-i1f+1) )*2*(k2f-k1f+1)
            iptr=0
            do nv=1,ntotsp
               ispbeg=ipsa3d(icm)+(nv-1)*mmzp(icm)*mmxp(icm)*mmyp(icm)
               call mknest_buff(ptconc(ispbeg),mtp,buffnest_pt(1+iptr),
     &                          mmxp(icm),mmyp(icm),mmzp(icm),
     &                          mi0(icm),mj0(icm),
     &                          i1c,i2c,j1c,j2c,
     &                          i1f,i2f,j1f,j2f,
     &                          offxb,offxe,offyb,offye,
     &                          k1c,k2c,nmesh(ifm),nmesh(ifm),icm)
               iptr=iptr+mtp
            enddo
            call par_init_put(node_buffs(nm)%lbc_pt_send_buff(1),  !cbwpt
     &                        node_buffs(nm)%nsend               )
            call par_put_int(i1f,   1)
            call par_put_int(i2f,   1)
            call par_put_int(j1f,   1)
            call par_put_int(j2f,   1)
            call par_put_int(k1f,   1)
            call par_put_int(k2f,   1)
            call par_put_int(i1c,   1)
            call par_put_int(i2c,   1)
            call par_put_int(j1c,   1)
            call par_put_int(j2c,   1)
            call par_put_int(mia(icm)+mi0(icm),   1)
            call par_put_int(miz(icm)+mi0(icm),   1)
            call par_put_int(mja(icm)+mj0(icm),   1)
            call par_put_int(mjz(icm)+mj0(icm),   1)
            call par_put_int(mynum, 1)
            call par_put_int(ntotsp,1)
            call par_put_int(iptr,  1)
            call par_put_float(buffnest_pt(1),iptr)
c
            if (iget_paths(itype,ifm,nm) .ne. bpaths(5,5,ifm,nm)) then
               stop 'iget_paths and bpaths are in-consistant'
            endif
            isend_req_pt(nm) = 0
               iptr=iptr+mtp
            call par_send_noblock(iget_paths(itype,ifm,nm), 
     &                            6000+icm+pt_identifier, isend_req_pt(nm))
         endif
      enddo
c
      deallocate( buffnest_pt )
      nbuff_save_pt = 0
c
      return
      end
