      subroutine newgrid(ngr)         
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
c       AGGR00
c       AVGALL
c       CAMX
c       NESTING
c       TSTEP_INIT
c    Subroutines called:
c       
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
      include 'flags.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: ngr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- Fill the single and 1D variables that the rest of the model
c      uses from the nest arrays and change grid level in the I/O. ---
c
      if (lmpi) then
         mxp=mmxp(ngr)
         myp=mmyp(ngr)
         mzp=mmzp(ngr)
         ia=mia(ngr)
         iz=miz(ngr)
         ja=mja(ngr)
         jz=mjz(ngr)
         i0=mi0(ngr)
         j0=mj0(ngr)
         ibcon=mibcon(ngr)
      else
         mxp=ncol(ngr)
         myp=nrow(ngr)
         mzp=nlay(ngr)
         ia=2
         iz=ncol(ngr)-1
         ja=2
         jz=nrow(ngr)-1
         i0=0
         j0=0
         ibcon=15
      endif
c
      return
      end
