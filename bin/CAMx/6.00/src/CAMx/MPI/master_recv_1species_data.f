      subroutine master_recv_1species_data(concs,ng,my_nlay, 
     &                                     this_layer,my_nspc,
     &                                     this_species,this_tag)
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
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c        This routine should only be used by master node
c
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by: 
c       MASTER_UPDATE
c    Subroutines called:
c       MPI_RECV
c       EXSPECIES_4_BUFF
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
      real    :: concs(ncol(ng),nrow(ng),my_nlay,my_nspc)
c
      integer :: my_nlay
      integer :: this_layer
      integer :: my_nspc
      integer :: this_species
      integer :: this_tag
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c  --- data received ---
c
      real    :: datrecv(ncol(ng),nrow(ng))
c
      integer :: nm
      integer :: mxp
      integer :: myp
      integer :: mxyp
      integer :: npts
      integer :: il1
      integer :: ir2
      integer :: jb1
      integer :: jt2
      integer :: ierr
      integer :: status(MPI_STATUS_SIZE)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      do nm = 1,nmachines
         il1=nxbegc(nm,ng)
         ir2=nxendc(nm,ng)
         jb1=nybegc(nm,ng)
         jt2=nyendc(nm,ng)
         if (iand(ibcflg(nm,ng),1).ne.0) il1=il1-1
         if (iand(ibcflg(nm,ng),2).ne.0) ir2=ir2+1
         if (iand(ibcflg(nm,ng),4).ne.0) jb1=jb1-1
         if (iand(ibcflg(nm,ng),8).ne.0) jt2=jt2+1
         mxp = nxend(nm,ng) - nxbeg(nm,ng) + 1
         myp = nyend(nm,ng) - nybeg(nm,ng) + 1
         mxyp = mxp*myp
         npts = mxyp
         call MPI_RECV(datrecv,npts,MPI_REAL,
     &                 nm,this_tag,MPI_COMM_WORLD,status,ierr)
         call exspecies_4_buff(concs,datrecv,
     &                         ncol(ng),nrow(ng),my_nlay,my_nspc,mxp,myp,
     &                         ixoff(nm,ng),iyoff(nm,ng),il1,ir2,jb1,jt2,
     &                         this_layer,this_species                   )
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine master_recv_1species_data:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine exspecies_4_buff:
c-----------------------------------------------------------------------
c
c    Called by: 
c       MASTER_RECV_1SPECIES_DATA
c
      subroutine exspecies_4_buff(a,b,n1,n2,n3,n4,m1,m2,i0,j0,i1,i2,j1,j2,k,l)
c
      implicit none
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real    :: a(n1,n2,n3,n4)
      real    :: b(m1,m2)
c
      integer :: n1
      integer :: n2
      integer :: n3
      integer :: n4
      integer :: m1
      integer :: m2
      integer :: i0
      integer :: j0
      integer :: i1
      integer :: i2
      integer :: j1
      integer :: j2
      integer :: k
      integer :: l
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      a(i1+i0:i2+i0,j1+j0:j2+j0,k,l) = b(i1:i2,j1:j2)
c
      return
      end
