c*** SRFMODRT
c
      subroutine srfmodrt(numcol,numrow,igrid,icell,jcell,dtime,zenith,
     &                    ctrns,ldrk,fclod)
      use grid
      use rtracchm
      use camxfld
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c   Description:
c     This routine performs chemistry adjustments on the reactive
c     tracer species in soil/vegetation using information from the
c     RTRAC chemistry input file.  
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       numcol  I number of columns in this slice
c       numrow  I number of rows in this slice
c       igrid   I  number of the grid containing the cell
c       icell   I  X index of the cell
c       jcell   I  Y index of the cell
c       dtime   R  time step (hours)
c       zenith  R  solar zenith angle (rad)
c       ctrns   R  UV cloud transmissivity
c       ldrk    L  darkness flag
c       fclod   R  fractional cloud cover  
c
c   Routines called:
c
c   Called by:
c       CHEMDRIV 
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     12/08/09   ---jjung---    Original development
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      implicit none
      include 'camx.prm'
      include 'rtracsrf.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer numcol
      integer numrow
      integer igrid
      integer icell
      integer jcell
      real    dtime
      real    zenith
      real    ctrns
      real    fclod
      logical ldrk
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i
      integer   idx
      integer   idxcel
      integer   idxsrf
      integer   m
      integer   iwater
      real      fshads
      real      fzenith
      real      fclouds
      real      deg2rad
      real      zenang
      real      cldrat
      real      totland
      real      effkphot
      real      dsolrate
      real      dvegrate
      real      eps
c
      data deg2rad /0.01745329/
      data eps /1.0e-20/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- Calculate array pointers for the current grid cell ---
c
      idxcel =  ipsa2d(igrid)-1 + icell + numcol*(jcell-1)
      idxsrf =  iptrlu(igrid)-1 + icell + numcol*(jcell-1)
c
c --- Calculate photolysis reduction factors (shade, solar angle, clouds)
c
      zenang = amin1(zenith,60.0)
      zenang = deg2rad*zenang
      fshads = 0.0
      totland = 0.0
      if (ldrk) then
         fzenith = 0.0
         fclouds = 0.0
      else
         do m = 1,NLUW89
            iwater = 7
            if (m .ne. iwater) then
               idx = idxsrf + numcol*numrow*(m-1)
               fshads = fshads + fshad(m)*fsurf(idx)
               totland = totland + fsurf(idx)
            endif
         enddo
         totland = amax1(totland,eps)
         fshads = fshads/totland
         fzenith = cos(zenang)
         cldrat = 1.6*ctrns*fzenith
         fclouds = 1. + fclod*(cldrat - 1.)
      endif
c
c --- Apply decay rates for photolysis, hydrolysis, leaching, and
c     plant penetration
c
      do i = 1,nrtgas
         effkphot = kphot(i)*fshads*fzenith*fclouds
         dsolrate = kleach(i) + khydro(i) + effkphot
         dvegrate = kpen(i)   + khydro(i) + effkphot
         idx = idxcel + numcol*numrow*(i-1)
         rtsolmas(idx) = rtsolmas(idx)*EXP(-dsolrate*dtime*60.)
         rtvegmas(idx) = rtvegmas(idx)*EXP(-dvegrate*dtime*60.)
         rtsolmas(idx) = MAX(1.e-20,rtsolmas(idx))
         rtvegmas(idx) = MAX(1.e-20,rtvegmas(idx))
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end
