      subroutine master_send_gridded_data(concs,ng,my_nlay,nspc,this_tag) 
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use master_mod
      use camxfld
      use filunit
      use grid
c
      implicit none
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        This routine should only be used by master node
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       SIM_INIT
c       NODES_AHOZ
c       NODES_INIT
c       NODES_MET
c    Subroutines called:
c       MK_2_BUFF
c       MK_3_BUFF
c       MK_4_BUFF
c       MPI_SEND
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
c
      include 'chmdat.inc'
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
c  --- current grid number ---
c
      integer :: ng
c
c  --- data for the whole domain ---
c
      real    :: concs(ncol(ng),nrow(ng),my_nlay,nspc)
c
      integer :: my_nlay
      integer :: nspc
      integer :: this_tag
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c  --- data to be sent ---
c
      real    :: datsend(ncol(ng),nrow(ng),my_nlay,nspc)
c
      integer :: nm
      integer :: mxp
      integer :: myp
      integer :: mxyp
      integer :: npts
      integer :: ii1
      integer :: ii2
      integer :: jj1
      integer :: jj2
      integer :: ierr
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      do nm = 1,nmachines
         ii1   = nxbeg(nm,ng)
         ii2   = nxend(nm,ng)
         jj1   = nybeg(nm,ng)
         jj2   = nyend(nm,ng)
         mxp   = ii2 - ii1 + 1     
         myp   = jj2 - jj1 + 1
         mxyp  = mxp  * myp
c
         if (my_nlay .eq. 1 .and. nspc .eq. 1) then
c 
c  --- 2-d array ---
c
            call mk_2_buff(concs,datsend,
     &                     ncol(ng),nrow(ng),mxp,myp,
     &                     ii1,ii2,jj1,jj2           )
            npts=mxyp
         elseif (nspc .eq. 1) then
c
c  --- 3-d array: my_nlay .ne. 1 ---
c
            call mk_3_buff(concs,datsend,
     &                     ncol(ng),nrow(ng),my_nlay,
     &                     mxp,myp,my_nlay,ii1,ii2,jj1,jj2)       
            npts=mxyp*my_nlay
         else
c
c  --- 4-d array: this situation includes nspc .ne. 1 .and. my_nlay .eq. 1
c                 and includes nspc .ne. 1 .and. my_nlay .ne. 1 ---
c
            call mk_4_buff(concs,datsend,
     &                     ncol(ng),nrow(ng),my_nlay,nspc,
     &                     mxp,myp,my_nlay,nspc,ii1,ii2,jj1,jj2)
            npts=mxyp*my_nlay*nspc
         endif
         call MPI_SEND(datsend,npts,MPI_REAL,
     &                 nm,this_tag,MPI_COMM_WORLD,ierr)
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine master_send_gridded_data:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine mk_2_buff:
c-----------------------------------------------------------------------
c
c    Called by:
c       MASTER_SEND_GRIDDED_DATA
c
      subroutine mk_2_buff(a,b,n1,n2,m1,m2,i1,i2,j1,j2)
c
      implicit none
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real    :: a(n1,n2)
      real    :: b(m1,m2)
c
      integer :: n1
      integer :: n2
      integer :: m1
      integer :: m2
      integer :: i1
      integer :: i2
      integer :: j1
      integer :: j2
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         b(1:m1,1:m2)=a(i1:i2,j1:j2)
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine mk_2_buff:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine mk_3_buff:
c-----------------------------------------------------------------------
c
c    Called by:
c       MASTER_SEND_GRIDDED_DATA
c
      subroutine mk_3_buff(a,b,n1,n2,n3,m1,m2,m3,i1,i2,j1,j2)
c
      implicit none
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real    :: a(n1,n2,n3)
      real    :: b(m1,m2,m3)
c
      integer :: n1
      integer :: n2
      integer :: n3
      integer :: m1
      integer :: m2
      integer :: m3
      integer :: i1
      integer :: i2
      integer :: j1
      integer :: j2
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         b(1:m1,1:m2,1:m3)=a(i1:i2,j1:j2,1:n3)
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine mk_3_buff:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine mk_4_buff:
c-----------------------------------------------------------------------
c
c    Called by:
c       MASTER_SEND_GRIDDED_DATA
c
      subroutine mk_4_buff(a,b,n1,n2,n3,n4,m1,m2,m3,m4,i1,i2,j1,j2)
c
      implicit none
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      real    :: a(n1,n2,n3,m4)
      real    :: b(m1,m2,m3,m4)
c
      integer :: n1
      integer :: n2
      integer :: n3
      integer :: n4
      integer :: m1
      integer :: m2
      integer :: m3
      integer :: m4
      integer :: i1
      integer :: i2
      integer :: j1
      integer :: j2
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         b(1:m1,1:m2,1:m3,1:m4)=a(i1:i2,j1:j2,1:n3,1:n4)
c
      return
      end
