      subroutine iniptr_node()
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use grid
      use chmstry
      use tracer
      use procan
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
c        ngcol   -- maximum number of columns for each grid
c        ngrow   -- maximum number of columns for each grid
c        nglay   -- maximum number of columns for each grid
c
c    Called by: 
c       NODES_ALLOC
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
      include 'deposit.inc'
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  ---- set the pointers ---
c
      iptr2d(1) = 1
      iptr3d(1) = 1
      iptr4d(1) = 1
      iptrav(1) = 1
      iptrem(1) = 1
      iptrlu(1) = 1
      iptrdp(1) = 1
      ipsadep(1) = 1
      ipsa2d(1) = 1
      ipsa3d(1) = 1
c
      do i=2,ngrid
         iptr2d(i) = iptr2d(i-1) + mmxp(i-1)*mmyp(i-1)
         iptr3d(i) = iptr3d(i-1) + mmxp(i-1)*mmyp(i-1)*mmzp(i-1)
         iptr4d(i) = iptr4d(i-1) + mmxp(i-1)*mmyp(i-1)*mmzp(i-1)*nspec
         iptrem(i) = iptrem(i-1) + mmxp(i-1)*mmyp(i-1)*nspec
         if( nlu .GT. 0 ) then
            iptrlu(i) = iptrlu(i-1) + mmxp(i-1)*mmyp(i-1)*nlu
         else
            iptrlu(i) = 1
         endif
         iptrdp(i) = iptrdp(i-1) + mmxp(i-1)*mmyp(i-1)*ndepspc*3
         if( .NOT. l3davg(i-1) ) then
            iptrav(i) = iptrav(i-1) + mmxp(i-1)*mmyp(i-1)*navspc
         else
            iptrav(i) = iptrav(i-1) + mmxp(i-1)*mmyp(i-1)*mmzp(i-1)*navspc
         endif
         if( ltrace .OR. lddm .OR. lhddm .OR. lirr ) then
            if( lptdepout ) ipsadep(i) = ipsadep(i-1) + 
     &                                    mmxp(i-1)*mmyp(i-1)*notimespc
            ipsa2d(i) = ipsa2d(i-1) + mmxp(i-1)*mmyp(i-1)*ntotsp
            ipsa3d(i) = ipsa3d(i-1) + mmxp(i-1)*mmyp(i-1)*mmzp(i-1)*ntotsp
         else
            ipsadep(i) = 1
            ipsa2d(i) = 1
            ipsa3d(i) = 1
         endif
      enddo
c
      return
      end
