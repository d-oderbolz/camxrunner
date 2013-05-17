c*** SSLOCMC
c
      subroutine sslocmc(y, rr, dt)
      use rtcmcchm
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Advance the slow species in the RTRAC CMC solver
c
c    Copyright 1996 - 2013
c    ENVIRON International Corporation
c
c    Argument descriptions:
c     Inputs:
c      rr    R  rate of each reaction
c      dt    R  time step
c     Outputs:
c      y     R  species concentrations
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    07/06/07   --gyarwood--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      implicit none
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real      dt
      real      rr(MXRX), y(MXTRSP+MXSPEC)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i, j, isp
      real      ydot
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      do i = 1,nslortc
         isp = idslo(i)
         ydot = 0.0
c
c --- accumulate ydot terms for reactant loss
c
         if( nslloss(i) .GT. 0 ) then
            do j = 1,nslloss(i)
               ydot = ydot - rr(islloss(i,j))
            enddo
         endif
c
c --- accumulate ydot terms for product gain
c
         if( nslgain(i) .GT. 0 ) then
            do j = 1,nslgain(i)
               ydot = ydot + rr(islgain(i,j))*spdcoslo(i,j)
            enddo
         endif
c
c --- advance y(i)
c
         y(isp) = y(isp) + dt*ydot
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end
