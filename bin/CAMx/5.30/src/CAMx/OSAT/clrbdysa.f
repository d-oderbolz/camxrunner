c**** CLRBDYSA.F
c
      subroutine clrbdysa(igrid,nox,noy,noz,nspsa,saconc)
      use bndary
      use tracer
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine resets the boundary conditions to zero.  This is 
c   necessary since the routine that fills the boundary conditions
c   accumulates all some of the regular model species.  Without resetting
c   the arrays to zero first, the boundary conditions become VERY large.
c   Since only the boundary conditions get filled in this way, only the
c   boundary should be reset to zero.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c      Argument description:
c       Outputs:
c           saconc   R  array of concentrations to clear
c       Inputs:
c           igrid    I  grid number of this grid
c           nox      I  number of cells in X direction
c           noy      I  number of cells in Y direction
c           noz      I  number of vertical cells in grid
c           nspsa    I  number of species 
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     06/06/96   --gwilson--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Arguement declarations:
c-----------------------------------------------------------------------
c
      integer   igrid
      integer   nox
      integer   noy
      integer   noz
      integer   nspsa
      real      saconc(nox,noy,noz,nspsa)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer      ioff, izcl, icl, jcl
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- do the WEST and EAST boundaries ----
c
      do 10 jcl=2,noy-1
c
c  --- West boundary ---
c
         do izcl=1,noz
c
c   --- if stratifying by boundary, put in seperate position ---
c
            if( lbndry ) then
               ioff = IDXBWS
            else
               ioff = 1
            endif
c
c   --- reset each boundary tracer to zero ----
c
            do icls=1,ntrcls
               saconc(1,jcl,izcl,iptcls(icls)+ioff) = 0.
            enddo
c
c  --- East boundary ---
c
c   --- if stratifying by boundary, put in seperate position ---
c
            if( lbndry ) then
               ioff = IDXBES
            else
               ioff = 1
            endif
c
c   --- reset each boundary tracer to zero ----
c
            do icls=1,ntrcls
                saconc(nox,jcl,izcl,iptcls(icls)+ioff) = 0.
            enddo
         enddo
   10 continue
c
c  --- do the SOUTH and NORTH boundaries ----
c
      do 20 icl=2,nox-1
         do izcl=1,noz
c
c   --- stratifying by boundary, put in seperate position ---
c
            if( lbndry ) then
                ioff = IDXBST
            else
                ioff = 1
            endif
c
c   --- reset each boundary tracer to zero ----
c
            do icls=1,ntrcls
               saconc(icl,1,izcl,iptcls(icls)+ioff) = 0.
            enddo
c
c  --- North boundary ---
c
c   --- if stratifying by boundary, put in seperate position ---
c
            if( lbndry ) then
                ioff = IDXBNT
            else
                ioff = 1
            endif
c
c   --- reset each boundary tracer to zero ----
c
            do icls=1,ntrcls
               saconc(icl,noy,izcl,iptcls(icls)+ioff) = 0.
            enddo
         enddo
   20 continue
c
c  --- return to the calling routine ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
