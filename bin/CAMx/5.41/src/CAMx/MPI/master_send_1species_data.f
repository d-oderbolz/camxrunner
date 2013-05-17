      subroutine master_send_1species_data(concs,ng,my_nlay,this_layer,
     &                                     my_nspc,this_species,this_tag)
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
c       BNDRY_UPDT
c       TSTEP_INIT
c       NODES_EMISS
c       NODES_INIT
c       NODES_MET
c       NODES_TSTEP
c    Subroutines called:
c       MKSPECIES_4_BUFF
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
c  --- data to be sent ---
c
      real    :: datsend(ncol(ng),nrow(ng))
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
         call mkspecies_4_buff(concs,datsend,
     &                         ncol(ng),nrow(ng),my_nlay,my_nspc,
     &                         mxp,myp,ii1,ii2,jj1,jj2,this_layer,this_species)
         npts=mxyp
         call MPI_SEND(datsend,npts,MPI_REAL,
     &                 nm,this_tag,MPI_COMM_WORLD,ierr)
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine master_send_1species_data:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine mkspecies_4_buff:
c-----------------------------------------------------------------------
c    Called by:
c       MASTER_SEND_1SPECIES_DATA
c
      subroutine mkspecies_4_buff(a,b,n1,n2,n3,n4,m1,m2,i1,i2,j1,j2,k,l)
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
      b(1:m1,1:m2)=a(i1:i2,j1:j2,k,l)
c
      return
      end
