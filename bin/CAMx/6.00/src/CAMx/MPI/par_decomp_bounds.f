      subroutine PAR_decomp_bounds(ngrids,nnxp,nnyp,nbndx,nbndy)    
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use master_mod
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
c       DOMAIN_DECOMP
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
      integer :: ngrids
      integer :: nnxp(*)
      integer :: nnyp(*)
      integer :: nbndx
      integer :: nbndy
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: ng
      integer :: nm
      integer :: nx
      integer :: ny
c
      logical :: debug_bound = .false.
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      do ng=1,ngrids
         nx=nnxp(ng)
         ny=nnyp(ng)
         do nm=1,nmachines
            ibcflg(nm,ng)=0
c
c  --- west boundary ---
c
            if (ixb(nm,ng) .eq. 2) then
               nxbeg(nm,ng)=1
               ixoff(nm,ng)=0
               nxbegc(nm,ng)=2
               ibcflg(nm,ng)=ibcflg(nm,ng)+1
            else
               nxbeg(nm,ng)=ixb(nm,ng)-nbndx
               ixoff(nm,ng)=nxbeg(nm,ng)-1
               nxbegc(nm,ng)=nbndx+1
            endif
c
c  --- east boundary ---
c
            if (ixe(nm,ng) .eq. nx-1) then
               nxend(nm,ng)=nx
               ibcflg(nm,ng)=ibcflg(nm,ng)+2
               nxendc(nm,ng)=(1+ixe(nm,ng)-ixb(nm,ng))+nxbegc(nm,ng)-1
            else
               nxend(nm,ng)=ixe(nm,ng)+nbndx
               nxendc(nm,ng)=(ixe(nm,ng)-ixb(nm,ng))+nxbegc(nm,ng)
            endif
c
c  --- south boundary ---
c
            if (iyb(nm,ng) .eq. 2) then
               nybeg(nm,ng)=1
               iyoff(nm,ng)=0
               nybegc(nm,ng)=2
               ibcflg(nm,ng)=ibcflg(nm,ng)+4
            else
               nybeg(nm,ng)=iyb(nm,ng)-nbndy
               iyoff(nm,ng)=nybeg(nm,ng)-1
               nybegc(nm,ng)=nbndy+1
            endif
c
c  --- north boundary ---
c
            if (iye(nm,ng) .eq. ny-1) then
               nyend(nm,ng)=ny
               ibcflg(nm,ng)=ibcflg(nm,ng)+8
               nyendc(nm,ng)=(1+iye(nm,ng)-iyb(nm,ng))+nybegc(nm,ng)-1
            else
               nyend(nm,ng)=iye(nm,ng)+nbndy
               nyendc(nm,ng)=(iye(nm,ng)-iyb(nm,ng))+nybegc(nm,ng)
            endif
c
c  --- end of machines ---
c
         enddo
c
c  --- end of grid ---
c
      enddo
c
      return
      end
