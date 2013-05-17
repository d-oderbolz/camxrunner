      subroutine node_get_nbc(ifm,icm)                                             
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use camxfld
      use grid_dims
      use grid
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
c       NESTING
c    Subroutines called:
c       PAR_WAIT
c       PAR_ASSOC_BUFF
c       PAR_GET_INT
c       PAR_GET_FLOAT
c       MYOFFSET
c       PAR_BINTP
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
      integer, dimension(maxmach) :: i1f
      integer, dimension(maxmach) :: i2f
      integer, dimension(maxmach) :: j1f
      integer, dimension(maxmach) :: j2f
      integer, dimension(maxmach) :: k1f
      integer, dimension(maxmach) :: k2f
      integer, dimension(maxmach) :: iptv
      integer, dimension(maxmach) :: iptc
      integer, dimension(maxmach) :: i1c
      integer, dimension(maxmach) :: i2c
      integer, dimension(maxmach) :: j1c
      integer, dimension(maxmach) :: j2c
      integer, dimension(maxmach) :: ibeg
      integer, dimension(maxmach) :: iend
      integer, dimension(maxmach) :: jbeg
      integer, dimension(maxmach) :: jend
c
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
      integer :: morder(nmachs)
      integer :: nnm
      integer :: nm
      integer :: ibytes
      integer :: msgid
      integer :: ihostnum
      integer :: iptr
      integer :: machf
      integer :: nvar
      integer :: nwords
      integer :: nv
      integer :: nxc
      integer :: nyc
      integer :: nzc
      integer :: mtp
      integer :: ispbeg
c
      real, allocatable, save :: buffnest(:)
      integer, save           :: nbuff_save=0
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- make sure sends are all finished and de-allocated ---
c
      do nm=1,nmachs
         if (iget_paths(6,ifm,nm) .ne. 0) then
            call par_wait(isend_req(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
c  --- wait on receives ---
c
      do nm=1,nmachs
         if (ipaths(5,6,ifm,nm) .ne. 0) then
            call par_wait(irecv_req(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
c  --- Allocate new temporary buffer if bigger than the old one. ---
c
      if (nbuff_save .eq. 0) then
         if (allocated(buffnest)) deallocate (buffnest)
         allocate( buffnest( 10*(mmxp(ifm)+mmyp(ifm))*mmzp(ifm)*nspec ) )
         nbuff_save = 10*(mmxp(ifm)+mmyp(ifm))*mmzp(ifm)*nspec
      endif
      if (node_buffs(mynum)%nrecv > nbuff_save) then
         if (allocated(buffnest)) deallocate (buffnest)
         allocate (buffnest(node_buffs(mynum)%nrecv))
         nbuff_save=node_buffs(mynum)%nrecv
      endif
c
      iptr=0
      nnm = 0
      do nm=1,nmachs
         if (nm .ne. mynum) then
            nnm = nnm + 1
            morder(nnm) = nm
         endif
      enddo
c
      morder(nmachs) = mynum
      do nnm=1,nmachs
         nm = morder(nnm)
         if (ipaths(5,6,ifm,nm) .ne. 0) then
            call par_assoc_buff(node_buffs(nm)%lbc_recv_buff(1),
     &                           node_buffs(nm)%nrecv           )
            call par_get_int(i1f(nm),1)
            call par_get_int(i2f(nm),1)
            call par_get_int(j1f(nm),1)
            call par_get_int(j2f(nm),1)
            call par_get_int(k1f(nm),1)
            call par_get_int(k2f(nm),1)
            call par_get_int(i1c(nm),1)
            call par_get_int(i2c(nm),1)
            call par_get_int(j1c(nm),1)
            call par_get_int(j2c(nm),1)
            call par_get_int(ibeg(nm),1)
            call par_get_int(iend(nm),1)
            call par_get_int(jbeg(nm),1)
            call par_get_int(jend(nm),1)
            call par_get_int(machf,  1)
            call par_get_int(nvar,   1)
            call par_get_int(nwords, 1)
            call par_get_float(buffnest(1+iptr),nwords)
            iptc(nm)=1+iptr
            iptv(nm)=0
            iptr=iptr+nwords
         endif
      enddo
c
      do nnm=1,nmachs
         nm = morder(nnm)
         if (ipaths(5,6,ifm,nm) .ne. 0) then
            nxc=i2f(nm)-i1f(nm)+1
            nyc=j2f(nm)-j1f(nm)+1
            nzc=k2f(nm)-k1f(nm)+1
            mtp=(nxc + nyc)*2*nzc
            call myoffset(0,i1f(nm),nmesh(ifm),offxb)
            call myoffset(1,i2f(nm),nmesh(ifm),offxe)
            call myoffset(0,j1f(nm),nmesh(ifm),offyb)
            call myoffset(1,j2f(nm),nmesh(ifm),offye)
            do nv=1,nspec
               ispbeg=iptr4d(ifm)+(nv-1)*mmzp(ifm)*mmxp(ifm)*mmyp(ifm)
               call par_bintp(conc(ispbeg),buffnest(iptc(nm)+iptv(nm)),mtp,
     &                        mmxp(ifm),mmyp(ifm),mmzp(ifm), 
     &                        i1f(nm),i2f(nm),j1f(nm),j2f(nm),
     &                        k1f(nm),k2f(nm), mi0(ifm),mj0(ifm), 
     &                        i1c(nm),i2c(nm),j1c(nm),j2c(nm),
     &                        ibeg(nm),iend(nm),jbeg(nm),jend(nm),
     &                        offxb,offxe,offyb,offye,
     &                        icm,ifm,nm,nv )
               iptv(nm)=iptv(nm)+mtp
            enddo
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
c    END subroutine node_get_nbc:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine par_bintp:
c-----------------------------------------------------------------------
c
      subroutine par_bintp(des,src,mtp,
     &                     m1,m2,m3,
     &                     i1f,i2f,j1f,j2f,
     &                     k1f,k2f,i0f,j0f,
     &                     i1c,i2c,j1c,j2c,
     &                     ibg,ind,jbg,jnd,
     &                     offxb,offxe,offyb,offye,
     &                     icm,ifm,nm,nv )
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod 
      use grid
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
c       NODE_GET_NBC
c       NODE_GET_NBC_PT
c    Subroutines called:
c       FI2CI
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
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real    :: des(m1,m2,m3)
      real    :: src(mtp)
c
      integer :: mtp
      integer :: m1
      integer :: m2
      integer :: m3
      integer :: i1f
      integer :: i2f
      integer :: j1f
      integer :: j2f
      integer :: k1f
      integer :: k2f
      integer :: i0f
      integer :: j0f
      integer :: i1c
      integer :: i2c
      integer :: j1c
      integer :: j2c
      integer :: ibg
      integer :: ind
      integer :: jbg
      integer :: jnd
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
      integer :: icm
      integer :: ifm
      integer :: nm
      integer :: nv
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i
      integer :: j
      integer :: k
      integer :: n
      integer :: counter
      integer :: curr
      integer :: currx
      integer :: curry
c
c  --- boundary thickness ---
c
      integer :: bthk
c
      logical :: mi0_flag
      logical :: mj0_flag
c
      integer :: ofxa
      integer :: ofxz
      integer :: ofya
      integer :: ofyz
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
      parameter (bthk = 3)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ofxa = NOVERLP
      ofxz = NOVERLP
      ofya = NOVERLP
      ofyz = NOVERLP
      if (nodei0(nm,icm) .eq. 0) ofxa = 1
      if ((nodemxp(nm,icm) + nodei0(nm,icm)) .eq. ncol(icm)) ofxz = 1
      if (nodej0(nm,icm) .eq. 0) ofya = 1
      if ((nodemyp(nm,icm) + nodej0(nm,icm)) .eq. nrow(icm)) ofyz = 1
      mi0_flag = .false.
      mj0_flag = .false.
      do n=1, nmachs
         if (nodei0(n, 1) .gt. 0) mi0_flag = .true.
         if (nodej0(n, 1) .gt. 0) mj0_flag = .true.
      enddo
      counter = 1
c
      do k=k1f,k2f
c
c --- south ---
c
         call fi2ci(1+j0f,nmesh(ifm),j1(ifm),curry)
         if (abs(j1f-j0f) .le. 2*nmesh(ifm)) then
            do i=i1f, i2f
               call fi2ci(i,nmesh(ifm),i1(ifm),curr)
               if (curr .gt. nodei0(nm,icm)+nodemxp(nm,icm)-ofxz .or.
     &                                      curr .le. nodei0(nm,icm)+ofxa) then
c
c  --- do nothing ---
c
               else
                 if( curry .GE. jbg .AND. curry .LE. jnd ) 
     &                                     des(i-i0f,1,k) = src(counter)
                endif
               counter = counter + 1
            enddo
         else
            counter = counter + (i2f-i1f+1)
         endif
c
c  --- north ---
c
         call fi2ci(m2+j0f,nmesh(ifm),j1(ifm),curry)
         if (abs( (j2f-j0f)-mmyp(ifm) ) .le. 2*nmesh(ifm)) then
            do i=i1f, i2f
               call fi2ci(i,nmesh(ifm),i1(ifm),curr)
               if (curr .gt. nodei0(nm,icm)+nodemxp(nm,icm)-ofxz .or.
     &                                      curr .le. nodei0(nm,icm)+ofxa) then
c
c  --- do nothing ---
c
               else
                 if( curry .GE. jbg .AND. curry .LE. jnd ) 
     &                                    des(i-i0f,m2,k) = src(counter)
               endif
               counter = counter + 1
            enddo
         else
            counter = counter + (i2f-i1f+1)
         endif
c
c  --- west ---
c
         call fi2ci(1+i0f,nmesh(ifm),i1(ifm),currx)
         if (abs(i1f-i0f) .le. 2*nmesh(ifm)) then
            do j=j1f, j2f
               call fi2ci(j,nmesh(ifm),j1(ifm),curr)
               if (curr .gt. nodej0(nm,icm)+nodemyp(nm,icm)-ofyz .or.
     &                                      curr .le. nodej0(nm,icm)+ofya) then
c
c  --- do nothing ---
c
               else
                 if( currx .GE. ibg .AND. currx .LE. ind ) 
     &                               des(1, j-j0f,k) = src(counter)
               endif
               counter = counter + 1
            enddo
         else
            counter = counter + (j2f-j1f+1)
         endif
c
c  --- east ---
c
         call fi2ci(m1+i0f,nmesh(ifm),i1(ifm),currx)
         if (abs( (i2f-i0f)-mmxp(ifm) ) .le. 2*nmesh(ifm)) then
            do j=j1f, j2f
               call fi2ci(j,nmesh(ifm),j1(ifm),curr)
               if (curr .gt. nodej0(nm,icm)+nodemyp(nm,icm)-ofyz .or.
     &                                      curr .le. nodej0(nm,icm)+ofya) then
c
c  --- do nothing ---
c
               else
                 if( currx .GE. ibg .AND. currx .LE. ind ) 
     &                              des(m1,j-j0f,k) = src(counter)
               endif
               counter = counter + 1
            enddo
         else
            counter = counter + (j2f-j1f+1)
         endif
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine par_bintp:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine fi2ci:
c-----------------------------------------------------------------------
c
c    Called by:
c       PAR_BINTP
c
      subroutine fi2ci(fi,nmesh,i0,ci)
c
      implicit none
c
c-----------------------------------------------------------------------
c    Description:
c        fine index to coarse index
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: fi
      integer :: nmesh
      integer :: i0
      integer :: ci
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: temp
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      temp = (fi-2)/nmesh
      ci = temp + i0
c
      if( fi .eq. 1 ) ci = i0 - 1
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine fi2ci:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_get_nbc_pt:
c-----------------------------------------------------------------------
c
      subroutine node_get_nbc_pt(ifm,icm)                                             
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use camxfld
      use grid_dims
      use grid
      use tracer
c
      implicit none
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        probing tool
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
c       MYOFFSET
c       PAR_BINTP
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
      integer, dimension(maxmach) :: i1f
      integer, dimension(maxmach) :: i2f
      integer, dimension(maxmach) :: j1f
      integer, dimension(maxmach) :: j2f
      integer, dimension(maxmach) :: k1f
      integer, dimension(maxmach) :: k2f
      integer, dimension(maxmach) :: iptv
      integer, dimension(maxmach) :: iptc
      integer, dimension(maxmach) :: i1c
      integer, dimension(maxmach) :: i2c
      integer, dimension(maxmach) :: j1c
      integer, dimension(maxmach) :: j2c
      integer, dimension(maxmach) :: ibeg
      integer, dimension(maxmach) :: iend
      integer, dimension(maxmach) :: jbeg
      integer, dimension(maxmach) :: jend
c
      integer :: offxb
      integer :: offxe
      integer :: offyb
      integer :: offye
      integer :: morder(nmachs)
      integer :: nnm
      integer :: nm
      integer :: ibytes
      integer :: msgid
      integer :: ihostnum
      integer :: iptr
      integer :: machf
      integer :: nvar
      integer :: nwords
      integer :: nv
      integer :: nxc
      integer :: nyc
      integer :: nzc
      integer :: mtp
      integer :: ispbeg
c
      real, allocatable, save :: buffnest_pt(:)
      integer, save           :: nbuff_save_pt=0
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- make sure sends are all finished and de-allocated ---
c
      do nm=1,nmachs
         if (iget_paths(6,ifm,nm) .ne. 0) then
            call par_wait(isend_req_pt(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
c  --- wait on receives ---
c
      do nm=1,nmachs
         if (ipaths(5,6,ifm,nm) .ne. 0) then
            call par_wait(irecv_req_pt(nm),ibytes,msgid,ihostnum)
         endif
      enddo
c
c  --- Allocate new temporary buffer if bigger than the old one. ---
c
      if (nbuff_save_pt .eq. 0) then
         if (allocated(buffnest_pt)) deallocate (buffnest_pt)
         allocate( buffnest_pt( 10*(mmxp(ifm)+mmyp(ifm))*mmzp(ifm)*ntotsp ) )
         nbuff_save_pt = 10*(mmxp(ifm)+mmyp(ifm))*mmzp(ifm)*ntotsp
      endif
      if (node_buffs(mynum)%nrecv > nbuff_save_pt) then
         if (allocated(buffnest_pt)) deallocate (buffnest_pt)
         allocate (buffnest_pt(node_buffs(mynum)%nrecv))
         nbuff_save_pt=node_buffs(mynum)%nrecv
      endif
c
      iptr=0
      nnm = 0
      do nm=1,nmachs
         if (nm .ne. mynum) then
            nnm = nnm + 1
            morder(nnm) = nm
         endif
      enddo
c
      morder(nmachs) = mynum
      do nnm=1,nmachs
         nm = morder(nnm)
         if (ipaths(5,6,ifm,nm) .ne. 0) then
            call par_assoc_buff(node_buffs(nm)%lbc_pt_recv_buff(1),  !cbwpt
     &                          node_buffs(nm)%nrecv               )
            call par_get_int(i1f(nm),1)
            call par_get_int(i2f(nm),1)
            call par_get_int(j1f(nm),1)
            call par_get_int(j2f(nm),1)
            call par_get_int(k1f(nm),1)
            call par_get_int(k2f(nm),1)
            call par_get_int(i1c(nm),1)
            call par_get_int(i2c(nm),1)
            call par_get_int(j1c(nm),1)
            call par_get_int(j2c(nm),1)
            call par_get_int(ibeg(nm),1)
            call par_get_int(iend(nm),1)
            call par_get_int(jbeg(nm),1)
            call par_get_int(jend(nm),1)
            call par_get_int(machf,  1)
            call par_get_int(nvar,   1)
            call par_get_int(nwords, 1)
            call par_get_float(buffnest_pt(1+iptr),nwords)
            iptc(nm)=1+iptr
            iptv(nm)=0
            iptr=iptr+nwords
         endif
      enddo
c
      do nnm=1,nmachs
         nm = morder(nnm)
         if (ipaths(5,6,ifm,nm) .ne. 0) then
            nxc=i2f(nm)-i1f(nm)+1
            nyc=j2f(nm)-j1f(nm)+1
            nzc=k2f(nm)-k1f(nm)+1
            mtp=(nxc + nyc)*2*nzc
            call myoffset(0,i1f(nm),nmesh(ifm),offxb)
            call myoffset(1,i2f(nm),nmesh(ifm),offxe)
            call myoffset(0,j1f(nm),nmesh(ifm),offyb)
            call myoffset(1,j2f(nm),nmesh(ifm),offye)
            do nv=1,ntotsp
               ispbeg=ipsa3d(ifm)+(nv-1)*mmzp(ifm)*mmxp(ifm)*mmyp(ifm)
               call par_bintp(ptconc(ispbeg),buffnest_pt(iptc(nm)+iptv(nm)),mtp,
     &                        mmxp(ifm),mmyp(ifm),mmzp(ifm), 
     &                        i1f(nm),i2f(nm),j1f(nm),j2f(nm),
     &                        k1f(nm),k2f(nm), mi0(ifm),mj0(ifm), 
     &                        i1c(nm),i2c(nm),j1c(nm),j2c(nm),
     &                        ibeg(nm),iend(nm),jbeg(nm),jend(nm),
     &                        offxb,offxe,offyb,offye,
     &                        icm, ifm, nm,nv )
               iptv(nm)=iptv(nm)+mtp
            enddo
         endif
      enddo
c
      deallocate( buffnest_pt )
      nbuff_save_pt = 0
c
      return
      end
