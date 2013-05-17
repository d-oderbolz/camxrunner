      subroutine node_send_lbc(iproc_id,nspc,ngr)                                   
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use grid
      use camxfld
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
c       EMISTRNS
c    Subroutines called:
c       PAR_GET_NOBLOCK
c       PAR_INIT_PUT
c       PAR_PUT_INT
c       MK_LBC4_BUFF
c       PAR_PUT_FLOAT
c       PAR_SEND_NOBLOCK
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
      integer :: iproc_id
      integer :: nspc 
      integer :: ngr
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: itype
      integer :: nm
      integer :: ii1
      integer :: ii2
      integer :: jj1
      integer :: jj2
      integer :: mtp
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      itype=1
c      
c  --- before send anything, post the receives. ---
c
      do nm=1,nmachs
         if (iget_paths(itype,ngr,nm) .ne. 0) then
            call par_get_noblock(node_buffs(nm)%lbc_recv_buff(1),
     &                           node_buffs(nm)%nrecv,20000+ngr,machs(nm),
     &                           irecv_req(nm)                            )
         endif
      enddo
c     
c  --- sending the stuff ---
c
      do nm=1,nmachs
         isend_req(nm) = nm
         if (ipaths(1,itype,ngr,nm) .ne. 0) then
            ii1=ipaths(1,itype,ngr,nm)
            ii2=ipaths(2,itype,ngr,nm)
            jj1=ipaths(3,itype,ngr,nm)
            jj2=ipaths(4,itype,ngr,nm)
            call par_init_put(node_buffs(nm)%lbc_send_buff(1),
     &                        node_buffs(nm)%nsend            )
            call par_put_int(ii1,1)
            call par_put_int(ii2,1)
            call par_put_int(jj1,1)
            call par_put_int(jj2,1)
            call par_put_int(mynum,1)
            call mk_lbc4_buff(mxp,myp,mzp,nspc,conc(iptr4d(ngr)),
     &                        scr1(1),ii1-i0,ii2-i0,jj1-j0,jj2-j0,mtp)
            call par_put_float(scr1(1),mtp)  
            call par_send_noblock(ipaths(5,itype,ngr,nm),
     &                            20000+ngr,isend_req(nm))
         endif
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine node_send_lbc:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine mk_lbc4_buff:
c-----------------------------------------------------------------------
c
c    Called by:
c       NODE_SEND_LBC
c       NODE_SEND_LBC_PT
c
      subroutine mk_lbc4_buff(n1,n2,n3,n4,a,b,il,ir,jb,jt,ind)
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
c     Copyright 1996 - 2012
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
      integer :: n1
      integer :: n2
      integer :: n3
      integer :: n4
c
      real    :: a(n1,n2,n3,n4)
      real    :: b(*)
c
      integer :: il
      integer :: ir
      integer :: jb
      integer :: jt
      integer :: ind
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i
      integer :: j
      integer :: k
      integer :: m
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ind=0
c
      do m=1,n4
         do k=1,n3
            do j=jb,jt
               do i=il,ir
                  ind=ind+1
                  b(ind)=a(i,j,k,m)
               enddo
            enddo
         enddo
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine mk_lbc4_buff:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine mk_lbc4_buff_pt:
c-----------------------------------------------------------------------
c
      subroutine mk_lbc4_buff_pt(n1,n2,n3,n4,a,b,il,ir,jb,jt,ind)
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
c     Copyright 1996 - 2012
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
      integer :: n1
      integer :: n2
      integer :: n3
      integer :: n4
c
      real    :: a(n1,n2,n3,n4)
      real    :: b(*)
c
      integer :: il
      integer :: ir
      integer :: jb
      integer :: jt
      integer :: ind
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i
      integer :: j
      integer :: k
      integer :: m
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ind=0
c
      do m=1,n4
         do k=1,n3
            do j=jb,jt
               do i=il,ir
                  ind=ind+1
                  b(ind)=a(i,j,k,m)
               enddo
            enddo
         enddo
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine mk_lbc4_buff_pt:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine node_send_lbc_pt:
c-----------------------------------------------------------------------
c
      subroutine node_send_lbc_pt(iproc_id,nspc,ngr)    
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use grid
      use camxfld
      use tracer
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
c       EMISTRNS
c    Subroutines called:
c       PAR_GET_NOBLOCK
c       PAR_INIT_PUT
c       PAR_PUT_INT
c       MK_LBC4_BUFF
c       PAR_PUT_FLOAT
c       PAR_SEND_NOBLOCK
c
c     Copyright 1996 - 2012
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
      integer :: iproc_id
      integer :: nspc 
      integer :: ngr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: itype
      integer :: nm
      integer :: ii1
      integer :: ii2
      integer :: jj1
      integer :: jj2
      integer :: mtp
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      itype=1
c      
c  --- before send anything, post the receives. ---
c
      do nm=1,nmachs
         if (iget_paths(itype,ngr,nm) .ne. 0) then
            call par_get_noblock(node_buffs(nm)%lbc_pt_recv_buff(1),  !cbwpt
     &                           node_buffs(nm)%nrecv,20000+ngr+pt_identifier,
     &                           machs(nm),irecv_req_pt(nm)                   )
         endif
      enddo
c     
c  --- sending the stuff ---
c
      do nm=1,nmachs
         isend_req_pt(nm) = 900+nm
         if (ipaths(1,itype,ngr,nm) .ne. 0) then
            ii1=ipaths(1,itype,ngr,nm)
            ii2=ipaths(2,itype,ngr,nm)
            jj1=ipaths(3,itype,ngr,nm)
            jj2=ipaths(4,itype,ngr,nm)
            call par_init_put(node_buffs(nm)%lbc_pt_send_buff(1), !cbwpt
     &                        node_buffs(nm)%nsend               )
            call par_put_int(ii1,  1)
            call par_put_int(ii2,  1)
            call par_put_int(jj1,  1)
            call par_put_int(jj2,  1)
            call par_put_int(mynum,1)
            call mk_lbc4_buff_pt(mxp,myp,mzp,nspc,ptconc(ipsa3d(ngr)),
     &                           scr1_pt(1),ii1-i0,ii2-i0,jj1-j0,jj2-j0,mtp)
            call par_put_float(scr1_pt(1),mtp) 
            call par_send_noblock(ipaths(5,itype,ngr,nm),
     &                            20000+ngr+pt_identifier,isend_req_pt(nm))
         endif
      enddo
c
      return
      end
